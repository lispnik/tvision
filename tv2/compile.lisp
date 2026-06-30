;;;; compile.lisp --- compile / load a buffer, and interrupt a running eval.
;;;;
;;;; All built on the ported REPL: eval-defun reuses the editor eval hook; load
;;;; and compile submit (load …)/(compile-file …) on the worker so warnings and
;;;; notes stream into the transcript; interrupt aborts a runaway evaluation by
;;;; signalling the worker thread into its repl-abort restart.

(in-package #:tv2)

(defun %code-repl ()
  "Raise (opening if needed) the desktop REPL and return it."
  (let ((r (ensure-repl)))
    (when (and r *desktop*) (dt-raise *desktop* r) (invalidate *desktop*))
    r))

(defun do-eval-defun ()
  "Evaluate (SBCL compiles as it evaluates) the top-level form at the cursor."
  (let ((te (%focused-editor)))
    (when (and te *editor-eval-fn*) (funcall *editor-eval-fn* te))))

(defun %code-submit (r input)
  "Submit INPUT to the REPL, telling the user when it can't run (REPL busy)."
  (if (repl-busy r)
      (%tool-note "REPL is busy — interrupt or wait for the current evaluation")
      (repl-submit-string r input)))

(defun do-load-buffer ()
  "Evaluate every form in the focused editor's buffer in the REPL."
  (let ((te (%focused-editor)) (r (%code-repl)))
    (when (and te r) (%code-submit r (te-text te)))))

(defun do-compile-buffer ()
  "COMPILE-FILE the focused editor's file (saving first), streaming compiler
warnings/notes into the REPL."
  (let ((te (%focused-editor)) (r (%code-repl)))
    (when (and te r)
      (cond
        ((null (te-filename te))
         (%open-output " Compile buffer " "Save the buffer to a file first (compile-file needs a path)."))
        (t (handler-case (when (te-modified te) (te-save te))
             (error (e) (return-from do-compile-buffer
                          (%open-output " Compile buffer " (format nil "Could not save the buffer:~%~a" e)))))
           (%code-submit r (format nil "(compile-file ~s)" (namestring (te-filename te)))))))))

(defun do-interrupt-eval ()
  "Abort a running REPL evaluation by signalling the worker into repl-abort."
  (let ((r (and *desktop* (find :repl (dt-windows *desktop*) :key #'window-kind))))
    (if (and r (repl-busy r) (repl-worker r) (sb-thread:thread-alive-p (repl-worker r)))
        (sb-thread:interrupt-thread
         (repl-worker r)
         (lambda () (let ((res (find-restart 'repl-abort))) (when res (invoke-restart res)))))
        (%tool-note "nothing is evaluating"))))

(push (lambda (dt)
        (declare (ignore dt))
        (list "Eval"
              (list "Eval / compile defun" (lambda () (do-eval-defun)))
              (list "Load buffer"          (lambda () (do-load-buffer)))
              (list "Compile buffer"       (lambda () (do-compile-buffer)))
              (list "Interrupt eval"       (lambda () (do-interrupt-eval)))))
      *extra-menus*)
