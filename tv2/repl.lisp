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
   (mailbox  :initform nil  :accessor repl-mailbox)
   (hist-vars :initform nil :accessor repl-hist-vars)    ; per-listener CL history-var state (for a backend)
   (last-value   :initform nil :accessor repl-last-value)    ; most recent primary result (object clipboard)
   (last-value-p :initform nil :accessor repl-last-value-p))
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
          (*package* (repl-package win)) (*read-eval* t)
          ;; route break / single-step (any non-error debugger entry) to tv2's
          ;; cross-thread debugger too, so TRACE :break and (step ...) work in-UI
          (*debugger-hook* (lambda (c hook) (declare (ignore hook)) (%repl-debug win c))))
      (restart-case
          (handler-bind ((sb-ext:step-condition (lambda (c) (%repl-debug win c)))   ; single-stepper -> tv2 debugger
                         (error (lambda (e) (%repl-debug win e))))
            (with-input-from-string (in input)
              (loop for form = (read in nil :eof) until (eq form :eof)
                    do (setf - form)
                       (let ((vals (multiple-value-list (eval form))))
                         (when vals (setf (repl-last-value win) (first vals) (repl-last-value-p win) t))
                         (setf +++ ++  ++ +  + form
                               /// //  // /  / vals
                               *** **  ** *  * (first vals))
                         (if vals
                             ;; keep the live object so the printed result is a
                             ;; clickable presentation (SLY-style)
                             (dolist (v vals) (push (list :present v (format nil "=> ~a~%" (prin1-to-string v))) msgs))
                             (push (list :text (format nil "; No values~%")) msgs))))))
        (repl-abort () (push (list :text (format nil ";; — aborted to top level —~%")) msgs)))
      (setf new-pkg *package*))                         ; sticky IN-PACKAGE (captured before unbind)
    (finish-output out)
    (let ((entries (nreverse msgs)) (pkg new-pkg))
      (run-on-ui (lambda ()
                   (let ((sb (find-view win 'transcript)))
                     (when sb (dolist (e entries)
                                (ecase (first e)
                                  (:present (scrollback-present sb (third e) (second e)))
                                  (:text    (scrollback-append sb (second e)))))))
                   (setf (repl-package win) pkg (repl-busy win) nil)
                   (%repl-update-prompt win))))))

;;; The worker calls this to evaluate INPUT for WIN.  Defaults to tv2's own
;;; streaming evaluator; an embedding app (tvlisp-tv2) can rebind it to reuse a
;;; different eval backend (e.g. tvlisp's repl-backend-eval).
(defvar *repl-eval-fn* '%repl-eval)

(defvar *repl-time* nil "When true, print each submission's run time after it evaluates.")

(defun %repl-eval-timed (win input)
  "Evaluate INPUT via *REPL-EVAL-FN*, appending a run-time line when *REPL-TIME*.
Backend-agnostic (times around the whole synchronous eval), so it works for both
tv2's evaluator and an embedded backend."
  (if (not *repl-time*)
      (funcall *repl-eval-fn* win input)
      (let ((t0 (get-internal-run-time)) (r0 (get-internal-real-time)))
        (funcall *repl-eval-fn* win input)
        (let ((run  (/ (float (- (get-internal-run-time) t0)) internal-time-units-per-second))
              (real (/ (float (- (get-internal-real-time) r0)) internal-time-units-per-second)))
          (run-on-ui (lambda ()
                       (let ((sb (find-view win 'transcript)))
                         (when sb (scrollback-append sb (format nil "; ~,3fs run, ~,3fs real~%" run real))))))))))

(defun repl-ensure-worker (win)
  (unless (repl-mailbox win) (setf (repl-mailbox win) (sb-concurrency:make-mailbox)))
  (unless (and (repl-worker win) (sb-thread:thread-alive-p (repl-worker win)))
    (setf (repl-worker win)
          (sb-thread:make-thread
           (lambda ()
             (loop for job = (sb-concurrency:receive-message (repl-mailbox win))
                   do (case (car job)
                        (:quit (return))
                        (:eval (%repl-eval-timed win (cdr job))))))
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
      (t (setf (input-text v) "" (input-caret v) 0) (input-notify v)
         (repl-submit-string win input)))))

(defun repl-submit-string (win input)
  "Echo INPUT at WIN's prompt and evaluate it on the worker.  Used by Enter and
by the editor's eval-defun / eval-region (programmatic submit)."
  (unless (or (repl-busy win) (string-blank-p input))
    (let ((sb (find-view win 'transcript)))
      (when sb (scrollback-append sb (format nil "~a ~a~%" (repl-prompt-string win) input))))
    (push input (repl-history win)) (setf (repl-hist-pos win) nil (repl-busy win) t)
    (%repl-update-prompt win)
    (repl-ensure-worker win)
    (sb-concurrency:send-message (repl-mailbox win) (cons :eval input))))

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

(defun %repl-history-search (v)
  "Reverse-i-search: a modal, live-filtered picker over the REPL history; Enter
recalls the chosen line into the input."
  (let* ((win (view-root v)) (hist (repl-history win)))
    (if (null hist)
        (input-notify v)                                ; nothing to search yet
        (let ((d (ui (dialog (:title " History search "
                              :keymap *dialog-keys*
                              :value-fn (lambda (d) (let ((lb (find-view d 'lb)))
                                                      (nth (list-selected lb) (list-items lb)))))
                       (stack
                         (1 (row (9 (static-text :role :label :text " Search: "))
                                 (:fill (input-line :name 'q
                                          :on-change (lambda (il)
                                                       (let ((lb (find-view (view-root il) 'lb)))
                                                         (setf (list-items lb) (fuzzy-filter (input-text il) hist)
                                                               (list-selected lb) 0 (list-top lb) 0)
                                                         (invalidate lb)))))))
                         (:fill (list-box :name 'lb :items hist
                                  :on-activate (lambda (lb item) (declare (ignore item)) (perform 'accept lb nil))))
                         (1 (static-text :role :status
                              :text " type to filter · ↑/↓ select · Enter: recall · Esc: cancel ")))))))
          (let ((r (exec-view d :width 66 :height 16)))
            (when (and (not (eq r :cancel)) (stringp r)) (%repl-recall v r nil)))))))

(define-command repl-hist-search (v e) (%repl-history-search v))

(defkeymap *repl-input-keys* (*global-keys*)            ; Esc/q (when blank) still quit via the parent
  (:enter repl-enter)
  (:up    repl-hist-prev)
  (:down  repl-hist-next)
  ((code-char 18) repl-hist-search))                    ; Ctrl-R: reverse-i-search over history

;;; --- entry point ------------------------------------------------------------

(defun %repl-present-inspect (object)
  "Open an inspector on a clicked REPL result -- its live object (SLY-style)."
  (let ((label (let ((s (handler-case (prin1-to-string object) (error () "object"))))
                 (if (> (length s) 40) (concatenate 'string (subseq s 0 37) "…") s))))
    (if *desktop*
        (dt-open *desktop* (lambda () (make-inspector object label)))
        (multiple-value-bind (w f) (make-inspector object label) (run-view w :focus f)))))

(defun make-repl (&optional (package :cl-user))
  "Build a Lisp REPL window.  Return (values WINDOW FOCUS OPEN); OPEN's cleanup
stops the per-listener worker thread when the window closes."
  (let* ((win  (make-instance 'repl-window
                              :title " tv2 — Lisp REPL (a real tvlisp window, ported) "
                              :keymap *global-keys*
                              :package (or (find-package package) (find-package :cl-user))))
         (body (ui (stack
                     (:fill (scrollback :name 'transcript :on-present #'%repl-present-inspect))
                     (1 (row (12 (static-text :name 'prompt :role :label :text " CL-USER> "))
                             (:fill (input-line :name 'input :keymap *repl-input-keys*))))
                     (1 (static-text :role :status
                          :text " Enter: eval (on a worker thread) · ↑/↓: history · Tab: scroll transcript · Esc: close "))))))
    (add-subview win body)
    (scrollback-append (find-view win 'transcript)
                       (format nil "tv2 REPL — ~a ~a~%evaluation runs on a background worker; the UI stays live (output streams in).~%~%"
                               (lisp-implementation-type) (lisp-implementation-version)))
    (%repl-update-prompt win)
    (setf (window-scroll-target win) (find-view win 'transcript) (window-help win) :repl)
    (values win (find-view win 'input)
            (lambda (s) (declare (ignore s))
              (lambda () (when (and (repl-worker win) (sb-thread:thread-alive-p (repl-worker win)))
                           (sb-concurrency:send-message (repl-mailbox win) (cons :quit nil))))))))

(defun run-repl (&optional (package :cl-user))
  "Run the ported Lisp REPL full-screen until Esc."
  (multiple-value-bind (w f o) (make-repl package) (run-view w :focus f :open o)))
