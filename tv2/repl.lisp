;;;; repl.lisp --- a real tvlisp window (the Lisp REPL) ported onto tv2.
;;;;
;;;; tvlisp's listener is the SLIME/Lem model: a worker thread evaluates Lisp
;;;; while the UI keeps running; output, results and errors are shipped back to
;;;; the UI thread via the worker->UI bridge -- "only the UI thread touches the
;;;; screen".  This port keeps that architecture, rebuilt from tv2 parts:
;;;;
;;;;   SCROLLBACK (transcript)  +  INPUT-LINE (prompt)  +  a per-listener worker
;;;;
;;;; A Gray output stream streams the worker's *standard-output* into the
;;;; transcript live (flushing on newline); results, the CL history vars
;;;; (-, +/++/+++, */**/***, /// ) and sticky IN-PACKAGE are maintained on the
;;;; worker and posted back through RUN-ON-UI.  The SLDB-style restart dialog is
;;;; the one piece deferred -- errors print and abort.

(in-package #:tv2)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :sb-concurrency))

(defclass repl-window (window)
  ((package  :initform (find-package :cl-user) :initarg :package :accessor repl-package)
   (history  :initform '()  :accessor repl-history)     ; most-recent-first
   (hist-pos :initform nil  :accessor repl-hist-pos)    ; index into history while recalling
   (busy     :initform nil  :accessor repl-busy)        ; evaluating? (input ignored while t)
   (worker   :initform nil  :accessor repl-worker)
   (mailbox  :initform nil  :accessor repl-mailbox))
  (:metaclass reactive-class))

(defun repl-prompt-string (win)
  (let ((p (repl-package win)))
    (format nil "~a>" (or (first (package-nicknames p)) (package-name p)))))

(defun %repl-update-prompt (win)
  (let ((v (find-view win 'prompt)))
    (when v
      (setf (static-text-text v) (if (repl-busy win) " …eval " (format nil " ~a " (repl-prompt-string win))))
      (invalidate v))))

(defun string-blank-p (s)
  (every (lambda (c) (member c '(#\Space #\Tab #\Newline #\Return))) s))

(defun input-complete-p (string)
  "True when STRING reads as whole forms (no dangling open form)."
  (handler-case
      (with-input-from-string (in string)
        (loop for form = (read in nil :eof) until (eq form :eof))
        t)
    (end-of-file () nil)
    (error () t)))

;;; --- a Gray stream that streams worker output into the transcript live ------

(defclass repl-stream (sb-gray:fundamental-character-output-stream)
  ((win :initarg :win :reader rs-win)
   (buf :initform (make-string-output-stream) :reader rs-buf)))

(defun rs-flush (s)
  (let ((chunk (get-output-stream-string (rs-buf s))))
    (when (plusp (length chunk))
      (let ((win (rs-win s)))
        (run-on-ui (lambda () (let ((sb (find-view win 'transcript)))
                                (when sb (scrollback-append sb chunk)))))))))

(defmethod sb-gray:stream-write-char ((s repl-stream) ch)
  (write-char ch (rs-buf s)) (when (char= ch #\Newline) (rs-flush s)) ch)
(defmethod sb-gray:stream-write-string ((s repl-stream) string &optional (start 0) end)
  (let ((end (or end (length string))))
    (write-string string (rs-buf s) :start start :end end)
    (when (find #\Newline string :start start :end end) (rs-flush s)))
  string)
(defmethod sb-gray:stream-line-column ((s repl-stream)) nil)
(defmethod sb-gray:stream-finish-output ((s repl-stream)) (rs-flush s))
(defmethod sb-gray:stream-force-output ((s repl-stream)) (rs-flush s))

;;; --- the worker thread ------------------------------------------------------

(defun %repl-debug (win condition)
  "Worker thread, inside HANDLER-BIND: capture the live stack, ask the UI thread
to show the SLDB picker (blocking here so the stack -- and the LIVE frames --
stay intact), then carry out the chosen action: invoke a restart, or return a
form's values from a chosen frame.  Transfers control, so it never returns."
  (declare (ignore win))
  (multiple-value-bind (frames lives) (%capture-backtrace)
    (let ((restarts (compute-restarts condition))
          (sem (sb-thread:make-semaphore)) (choice (list nil)))
      (run-on-ui (lambda ()
                   (unwind-protect                      ; ALWAYS release the worker
                        (setf (first choice) (%debugger condition restarts frames))
                     (sb-thread:signal-semaphore sem))))
      (sb-thread:wait-on-semaphore sem)
      (let ((c (first choice)))
        (cond
          ((null c) (invoke-restart (find-restart 'repl-abort)))
          ((eq (first c) :frame-return) (%frame-return lives (second c) (third c) *package*))
          ((eq (first c) :restart)
           (let ((rs (nth (second c) restarts)) (val (third c)))
             (if (and rs val (plusp (length val)) (member (restart-name rs) '(use-value store-value)))
                 (invoke-restart rs (eval (read-from-string val)))
                 (if rs (invoke-restart rs) (invoke-restart (find-restart 'repl-abort)))))))))))

(defun %repl-eval (win input)
  "Worker thread: read+eval every form in INPUT under the listener's package,
streaming output live, maintaining the CL history vars, and posting the result
lines + new package + cleared busy flag back to the UI thread.  Errors route to
the cross-thread debugger (HANDLER-BIND keeps the stack live for the restart)."
  (let ((out (make-instance 'repl-stream :win win)) (msgs '()) (new-pkg nil))
    (let ((*standard-output* out) (*error-output* out) (*trace-output* out)
          (*package* (repl-package win)) (*read-eval* t))
      (restart-case
          (handler-bind ((error (lambda (e) (%repl-debug win e))))
            (with-input-from-string (in input)
              (loop for form = (read in nil :eof) until (eq form :eof)
                    do (setf - form)
                       (let ((vals (multiple-value-list (eval form))))
                         (setf +++ ++  ++ +  + form
                               /// //  // /  / vals
                               *** **  ** *  * (first vals))
                         (if vals
                             (dolist (v vals) (push (format nil "=> ~a" (prin1-to-string v)) msgs))
                             (push "; No values" msgs))))))
        (repl-abort () (push ";; — aborted to top level —" msgs)))
      (setf new-pkg *package*))                         ; sticky IN-PACKAGE (captured before unbind)
    (finish-output out)
    (let ((lines (nreverse msgs)) (pkg new-pkg))
      (run-on-ui (lambda ()
                   (let ((sb (find-view win 'transcript)))
                     (when sb (dolist (l lines) (scrollback-append sb (concatenate 'string l (string #\Newline))))))
                   (setf (repl-package win) pkg (repl-busy win) nil)
                   (%repl-update-prompt win))))))

(defun repl-ensure-worker (win)
  (unless (repl-mailbox win) (setf (repl-mailbox win) (sb-concurrency:make-mailbox)))
  (unless (and (repl-worker win) (sb-thread:thread-alive-p (repl-worker win)))
    (setf (repl-worker win)
          (sb-thread:make-thread
           (lambda ()
             (loop for job = (sb-concurrency:receive-message (repl-mailbox win))
                   do (case (car job)
                        (:quit (return))
                        (:eval (%repl-eval win (cdr job))))))
           :name "tv2-repl-worker"))))

;;; --- commands (bound on the input-line's keymap) ----------------------------

(define-command repl-enter (v e)
  (let* ((win (view-root v)) (sb (find-view win 'transcript)) (input (input-text v)))
    (cond
      ((repl-busy win))                                 ; ignore Enter while evaluating
      ((string-blank-p input)
       (when sb (scrollback-append sb (format nil "~a~%" (repl-prompt-string win))))
       (setf (input-text v) "" (input-caret v) 0) (input-notify v))
      ((not (input-complete-p input))
       (when sb (scrollback-append sb ";; incomplete form — finish it and press Enter again
")))                                                    ; keep the text in the field
      (t (when sb (scrollback-append sb (format nil "~a ~a~%" (repl-prompt-string win) input)))
         (push input (repl-history win)) (setf (repl-hist-pos win) nil)
         (setf (input-text v) "" (input-caret v) 0) (input-notify v)
         (setf (repl-busy win) t) (%repl-update-prompt win)
         (repl-ensure-worker win)
         (sb-concurrency:send-message (repl-mailbox win) (cons :eval input))))))

(defun %repl-recall (v text pos)
  (let ((win (view-root v)))
    (setf (repl-hist-pos win) pos
          (input-text v) (or text "") (input-caret v) (length (or text "")))
    (input-scroll-fix v) (input-notify v)))

(define-command repl-hist-prev (v e)
  (let* ((win (view-root v)) (h (repl-history win)) (n (length h)))
    (when (plusp n)
      (let ((pos (if (repl-hist-pos win) (min (1- n) (1+ (repl-hist-pos win))) 0)))
        (%repl-recall v (nth pos h) pos)))))

(define-command repl-hist-next (v e)
  (let* ((win (view-root v)) (h (repl-history win)) (cur (repl-hist-pos win)))
    (when cur
      (let ((pos (1- cur)))
        (if (minusp pos) (%repl-recall v "" nil) (%repl-recall v (nth pos h) pos))))))

(defkeymap *repl-input-keys* (*global-keys*)            ; Esc/q (when blank) still quit via the parent
  (:enter repl-enter)
  (:up    repl-hist-prev)
  (:down  repl-hist-next))

;;; --- entry point ------------------------------------------------------------

(defun run-repl (&optional (package :cl-user))
  "Run the ported Lisp REPL on the terminal until Esc."
  (tvision:with-screen (s)
    (let* ((win  (make-instance 'repl-window
                                :title " tv2 — Lisp REPL (a real tvlisp window, ported) "
                                :keymap *global-keys*
                                :package (or (find-package package) (find-package :cl-user))))
           (body (ui (stack
                       (:fill (scrollback :name 'transcript))
                       (1 (row (12 (static-text :name 'prompt :role :label :text " CL-USER> "))
                               (:fill (input-line :name 'input :keymap *repl-input-keys*))))
                       (1 (static-text :role :status
                            :text " Enter: eval (on a worker thread) · ↑/↓: history · Tab: scroll transcript · Esc: quit "))))))
      (add-subview win body)
      (layout win (rect 0 0 (tvision:screen-width s) (tvision:screen-height s)))
      (let ((sb (find-view win 'transcript)))
        (scrollback-append sb (format nil "tv2 REPL — ~a ~a~%evaluation runs on a background worker; the UI stays live (output streams in).~%~%"
                                      (lisp-implementation-type) (lisp-implementation-version))))
      (%repl-update-prompt win)
      (setf *root* win
            (container-focus win) (find-view win 'input)
            *ui-thread* sb-thread:*current-thread* *running* t *dirty* t)
      (loop while *running* do
        (drain-ui-callbacks)                            ; run thunks the worker posted
        (when *dirty*
          (draw win) (tvision:flush-screen s) (setf *dirty* nil))   ; input-line owns the cursor; don't hide it
        (tvision::pump-input s 0.05)
        (let ((tev (tvision::screen-next-event s)))
          (when tev (let ((ev (translate tev))) (when ev (handle-event win ev))))))
      ;; stop the worker on the way out
      (when (and (repl-worker win) (sb-thread:thread-alive-p (repl-worker win)))
        (sb-concurrency:send-message (repl-mailbox win) (cons :quit nil))))))
