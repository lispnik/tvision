;;;; threadmon.lisp --- a real tvlisp window (the thread monitor) ported onto tv2.
;;;;
;;;; tvlisp's TTHREAD-WINDOW lists the live SBCL threads with Refresh/Kill and
;;;; auto-refreshes on idle.  Here it is rebuilt from tv2 parts: a LIST-BOX of
;;;; threads, BUTTONs bound to commands, a background refresher that posts updates
;;;; through the worker->UI bridge (real changing data, not a demo clock), and
;;;; reactive repaint -- no integer commands, no dispatch COND, no manual geometry.

(in-package #:tv2)

(defvar *tm-threads* '() "Thread objects shown in the list, captured at refresh.")
(defvar *worker-n* 0)

(defun %thread-label (th)
  (format nil "~:[dead~;live~]  ~a"
          (sb-thread:thread-alive-p th) (or (sb-thread:thread-name th) "?")))

(defun tm-refresh (lb)
  "Recapture the live threads into the list-box, keeping the selection in range."
  (setf *tm-threads* (sb-thread:list-all-threads)
        (list-items lb) (mapcar #'%thread-label *tm-threads*))
  (when (>= (list-selected lb) (length *tm-threads*))
    (setf (list-selected lb) (max 0 (1- (length *tm-threads*)))))
  (list-scroll-fix lb)
  (invalidate lb))

(defun %tm-list (root) (find-view root 'threads))
(defun %tm-echo (root v) (let ((e (find-view root 'echo))) (when e (setf (static-text-text e) v) (invalidate e))))

(define-command tm-spawn (v e)
  (push (sb-thread:make-thread (lambda () (sleep 600))
                               :name (format nil "worker-~d" (incf *worker-n*)))
        *tm-threads*)
  (let ((lb (%tm-list (view-root v)))) (when lb (tm-refresh lb)))
  (%tm-echo (view-root v) (format nil " spawned worker-~d " *worker-n*)))

(define-command tm-kill (v e)
  (let* ((root (view-root v)) (lb (%tm-list root))
         (th (and lb (< (list-selected lb) (length *tm-threads*))
                  (nth (list-selected lb) *tm-threads*)))
         (name (and th (or (sb-thread:thread-name th) "?"))))
    (cond
      ((null th))
      ((or (eq th sb-thread:*current-thread*) (search "main" name) (eql 0 (search "tv2" name)))
       (%tm-echo root (format nil " refused to kill ~a " name)))    ; protect UI / system threads
      (t (ignore-errors (sb-thread:terminate-thread th))
         (%tm-echo root (format nil " killed ~a " name))
         (when lb (tm-refresh lb))))))

(define-command tm-refresh-cmd (v e)
  (let ((lb (%tm-list (view-root v)))) (when lb (tm-refresh lb)))
  (%tm-echo (view-root v) " refreshed "))

(defun %tm-selected (root)
  (let ((lb (%tm-list root)))
    (and lb (< (list-selected lb) (length *tm-threads*)) (nth (list-selected lb) *tm-threads*))))

(defun %thread-backtrace (th &optional (max 60))
  "Best-effort backtrace of TH as a string.  For another thread we interrupt it
to snapshot its stack (2s timeout so a wedged thread can't hang the UI) --
SBCL's sb-thread:interrupt-thread + sb-debug:print-backtrace make this possible."
  (flet ((self-bt () (with-output-to-string (s)
                       (ignore-errors (sb-debug:print-backtrace :count max :stream s)))))
    (cond
      ((not (sb-thread:thread-alive-p th)) "(thread is dead)")
      ((eq th sb-thread:*current-thread*) (self-bt))
      (t (let ((out nil) (done (sb-thread:make-semaphore)))
           (handler-case
               (progn
                 (sb-thread:interrupt-thread th
                   (lambda ()
                     (setf out (with-output-to-string (s)
                                 (ignore-errors (sb-debug:print-backtrace :count max :stream s))))
                     (sb-thread:signal-semaphore done)))
                 (if (sb-thread:wait-on-semaphore done :timeout 2)
                     (or out "(empty backtrace)")
                     "(timed out capturing the thread's backtrace)"))
             (error (e) (format nil "(could not capture backtrace: ~a)" e))))))))

(define-command tm-backtrace (v e)
  "Snapshot the selected thread's stack and show it."
  (let* ((root (view-root v)) (th (%tm-selected root)))
    (if th
        (%open-output (format nil " Backtrace: ~a " (or (sb-thread:thread-name th) "(anonymous)"))
                      (%thread-backtrace th))
        (%tm-echo root " select a thread first "))))

(define-command tm-interrupt (v e)
  "Soft-interrupt the selected thread: unwind to its ABORT restart (like Ctrl-C),
not terminate it.  Refuses the UI thread."
  (let* ((root (view-root v)) (th (%tm-selected root)) (name (and th (or (sb-thread:thread-name th) "?"))))
    (cond
      ((null th) (%tm-echo root " select a thread first "))
      ((not (sb-thread:thread-alive-p th)) (%tm-echo root " that thread is already dead "))
      ((eq th sb-thread:*current-thread*) (%tm-echo root " refusing to interrupt the UI thread "))
      (t (ignore-errors
          (sb-thread:interrupt-thread th (lambda () (let ((r (find-restart 'abort))) (when r (invoke-restart r))))))
         (%tm-echo root (format nil " interrupt sent to ~a " name))
         (let ((lb (%tm-list root))) (when lb (tm-refresh lb)))))))

(defun make-threadmon ()
  "Build a thread-monitor window.  Return (values WINDOW FOCUS OPEN); OPEN starts
the background refresher (keyed off the window, not *root*, so it works hosted)
and returns a cleanup thunk that stops it when the window closes."
  (let ((win (ui (window (:title " tv2 — Thread monitor (a real tvlisp window, ported) "
                          :keymap *global-keys*)
                   (stack
                     (1 (static-text :role :label :text " Live SBCL threads (auto-refreshing every 1.5s): "))
                     (:fill (list-box :name 'threads :items (mapcar #'%thread-label (sb-thread:list-all-threads))))
                     (1 (row (9  (button :label "Spawn"     :command 'tm-spawn))
                             (13 (button :label "Backtrace" :command 'tm-backtrace))
                             (13 (button :label "Interrupt" :command 'tm-interrupt))
                             (8  (button :label "Kill"      :command 'tm-kill))
                             (11 (button :label "Refresh"   :command 'tm-refresh-cmd))
                             (:fill (static-text :name 'echo :role :status :text ""))))
                     (1 (static-text :role :status
                          :text " Select a thread · Backtrace snapshots its stack · Interrupt = soft Ctrl-C · Esc: close ")))))))
    (setf *tm-threads* (sb-thread:list-all-threads) (window-help win) :threads)
    (values win (find-view win 'threads)
            (lambda (s) (declare (ignore s))
              (let ((alive t))
                (sb-thread:make-thread
                 (lambda () (loop while alive do
                              (sleep 1.5)
                              (run-on-ui (lambda () (let ((lb (%tm-list win))) (when lb (tm-refresh lb)))))))
                 :name "tv2-thread-refresher")
                (lambda () (setf alive nil)))))))   ; cleanup: stop the refresher

(defun run-threadmon ()
  "Run the ported thread monitor full-screen until q/Esc."
  (multiple-value-bind (w f o) (make-threadmon) (run-view w :focus f :open o)))
