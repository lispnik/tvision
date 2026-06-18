;;;; concurrency.lisp --- Threads, a thread-safe mailbox, and the worker->UI
;;;; bridge that lets background threads drive the single-threaded UI loop.
;;;;
;;;; Turbo Vision (and this port) runs one event loop on one thread that owns
;;;; the screen and the whole view tree.  To evaluate Lisp on a separate thread
;;;; (so a long/infinite computation doesn't freeze the UI -- the SLIME/Lem
;;;; model), we keep ONE invariant:
;;;;
;;;;     Only the UI thread may touch views or the screen.
;;;;
;;;; Worker threads compute and push *closures* onto a thread-safe queue; the UI
;;;; loop drains that queue each iteration and runs each closure on the UI
;;;; thread.  A self-pipe registered with SERVE-EVENT lets a worker wake the
;;;; input wait immediately, so output appears without polling latency.
;;;;
;;;; Built on SB-THREAD / SERVE-EVENT only -- no external dependencies.

(in-package #:tvision)

;;; ===========================================================================
;;; A simple thread-safe FIFO mailbox (mutex + counting semaphore)
;;; ===========================================================================

(defstruct (mailbox (:constructor %make-mailbox) (:copier nil))
  (lock (sb-thread:make-mutex :name "mailbox-lock"))
  (sem  (sb-thread:make-semaphore :name "mailbox-sem"))
  (head '())     ; FIFO: list of items, oldest first
  (tail '()))    ; last cons of HEAD, for O(1) enqueue

(defun make-mailbox () (%make-mailbox))

(defun mailbox-send (mb item)
  "Enqueue ITEM and wake one waiter."
  (sb-thread:with-mutex ((mailbox-lock mb))
    (let ((cell (cons item nil)))
      (if (mailbox-tail mb)
          (setf (cdr (mailbox-tail mb)) cell)
          (setf (mailbox-head mb) cell))
      (setf (mailbox-tail mb) cell)))
  (sb-thread:signal-semaphore (mailbox-sem mb)))

(defun %mailbox-pop (mb)
  (sb-thread:with-mutex ((mailbox-lock mb))
    (let ((cell (mailbox-head mb)))
      (setf (mailbox-head mb) (cdr cell))
      (unless (mailbox-head mb) (setf (mailbox-tail mb) nil))
      (car cell))))

(defun mailbox-receive (mb)
  "Block until an item is available; return it."
  (sb-thread:wait-on-semaphore (mailbox-sem mb))
  (%mailbox-pop mb))

(defun mailbox-try-receive (mb)
  "Return (values item t) if one is ready, else (values nil nil).  Non-blocking."
  (if (sb-thread:try-semaphore (mailbox-sem mb))
      (values (%mailbox-pop mb) t)
      (values nil nil)))

;;; ===========================================================================
;;; Worker -> UI callback queue + self-pipe wakeup
;;; ===========================================================================

(defvar *ui-callbacks* nil
  "Mailbox of thunks queued by worker threads to run on the UI thread.  NIL when
no UI loop is running (then RUN-ON-UI falls back to running inline).")

(defvar *ui-thread* nil
  "The thread that owns the screen and view tree (set while the UI loop runs).")

(defvar *wakeup-read-fd* nil)
(defvar *wakeup-write-fd* nil)
(defvar *ui-fd-handlers* '())

(defun ui-thread-p ()
  (or (null *ui-thread*) (eq sb-thread:*current-thread* *ui-thread*)))

(defun %drain-wakeup-pipe ()
  "Consume the wakeup byte(s); leftover bytes just re-fire the handler later."
  (when *wakeup-read-fd*
    (let ((buf (make-array 256 :element-type '(unsigned-byte 8))))
      (sb-sys:with-pinned-objects (buf)
        (ignore-errors (sb-unix:unix-read *wakeup-read-fd* (sb-sys:vector-sap buf) 256))))))

(defun %signal-wakeup ()
  "Write one byte to the self-pipe so the UI loop's SERVE-EVENT wait returns."
  (when *wakeup-write-fd*
    (let ((buf (make-array 1 :element-type '(unsigned-byte 8) :initial-element 1)))
      (sb-sys:with-pinned-objects (buf)
        (ignore-errors (sb-unix:unix-write *wakeup-write-fd* (sb-sys:vector-sap buf) 0 1))))))

(defun %fd0-readable-p ()
  (handler-case (and (sb-sys:wait-until-fd-usable 0 :input 0 nil) t)
    (error () nil)))

(defun %ui-input-multiplexer (timeout)
  "Installed as *INPUT-MULTIPLEXER*: wait up to TIMEOUT for terminal input OR a
worker-thread wakeup.  fd 0 is polled directly (the proven path); only the
wakeup pipe is watched by SERVE-EVENT, so a worker posting a UI callback breaks
the wait instantly.  Returns :FD0 when terminal input is ready, else NIL."
  (cond
    ((%fd0-readable-p) :fd0)            ; input already waiting
    (t (handler-case (sb-sys:serve-all-events timeout) (error () nil))
       ;; Woken by a worker, or timed out: re-check fd 0 for input that arrived
       ;; during the wait (so terminal latency is at most one TIMEOUT tick).
       (if (%fd0-readable-p) :fd0 nil))))

(defun install-ui-wakeup ()
  "Create the worker->UI callback queue and self-pipe, register a SERVE-EVENT
handler for the wakeup pipe, and route PUMP-INPUT through the multiplexer so a
worker thread can wake the UI loop instantly.  Safe to call once at start."
  (setf *ui-callbacks* (make-mailbox)
        *ui-thread* sb-thread:*current-thread*
        *ui-fd-handlers* '())
  (handler-case
      (multiple-value-bind (r w) (sb-unix:unix-pipe)
        (setf *wakeup-read-fd* r *wakeup-write-fd* w)
        (push (sb-sys:add-fd-handler r :input
                                     (lambda (fd) (declare (ignore fd)) (%drain-wakeup-pipe)))
              *ui-fd-handlers*)
        (setf *input-multiplexer* #'%ui-input-multiplexer))
    (error ()
      ;; No self-pipe available: fall back to the plain polling path.  Output
      ;; still appears, just within one poll tick rather than instantly.
      (setf *wakeup-read-fd* nil *wakeup-write-fd* nil *input-multiplexer* nil))))

(defun remove-ui-wakeup ()
  "Undo INSTALL-UI-WAKEUP (call on the UI thread during teardown)."
  (setf *input-multiplexer* nil)
  (dolist (h *ui-fd-handlers*) (ignore-errors (sb-sys:remove-fd-handler h)))
  (setf *ui-fd-handlers* '())
  (when *wakeup-read-fd* (ignore-errors (sb-unix:unix-close *wakeup-read-fd*)))
  (when *wakeup-write-fd* (ignore-errors (sb-unix:unix-close *wakeup-write-fd*)))
  (setf *wakeup-read-fd* nil *wakeup-write-fd* nil
        *ui-callbacks* nil *ui-thread* nil))

(defun run-on-ui (thunk)
  "Arrange for THUNK to run on the UI thread.  From a worker: queue it and wake
the loop.  When already on the UI thread (or no loop is running): run inline."
  (cond
    ((and *ui-callbacks* (not (ui-thread-p)))
     (mailbox-send *ui-callbacks* thunk)
     (%signal-wakeup))
    (t (funcall thunk))))

(defun drain-ui-callbacks ()
  "Run every queued UI thunk.  Called by the event loop on the UI thread."
  (when *ui-callbacks*
    (loop
      (multiple-value-bind (thunk present) (mailbox-try-receive *ui-callbacks*)
        (unless present (return))
        (handler-case (funcall thunk) (error () nil))))))

;;; ===========================================================================
;;; Background-thread lifecycle (workers register here so the app can stop them)
;;; ===========================================================================

(defvar *background-shutdown-hook* nil
  "A thunk (set by the REPL layer) that stops all background worker threads;
invoked during application teardown.")

(defun shutdown-background-threads ()
  (when *background-shutdown-hook*
    (ignore-errors (funcall *background-shutdown-hook*))))
