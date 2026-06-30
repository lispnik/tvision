;;;; tools.lisp --- tracing, profiling and stepping, ported onto tv2.
;;;;
;;;; The last of tvlisp's debugging tools.  Trace toggling is standard CL
;;;; (TRACE / UNTRACE), so it lives here directly; statistical profiling is a
;;;; hook (*PROFILE-FN*) an app fills with sb-sprof logic, rendered in tv2's
;;;; TABLE-VIEW; single-stepping and break-on-entry route through the REPL and
;;;; tv2's cross-thread debugger (the REPL worker binds *DEBUGGER-HOOK* so a
;;;; step/break surfaces in the same restart picker as an error).

(in-package #:tv2)

;;; --- a table-view window (reused by the profiler) ---------------------------

(defun make-table-window (title columns rows &key (help :browser))
  "A window over a TABLE-VIEW.  COLUMNS = list of (TITLE WIDTH ACCESSOR); ROWS =
row objects.  Return (values WINDOW FOCUS)."
  (let* ((win  (make-instance 'window :title title :keymap *global-keys*))
         (body (ui (stack
                     (:fill (table-view :name 'tbl :columns columns :rows rows))
                     (1 (static-text :role :status :text " ↑/↓ · PgUp/PgDn select · Esc: close "))))))
    (add-subview win body)
    (setf (window-scroll-target win) (find-view win 'tbl) (window-help win) help)
    (values win (find-view win 'tbl))))

;;; --- tracing (standard TRACE / UNTRACE) -------------------------------------

(defun %traced-symbols ()
  "Currently traced function names."
  (remove-if-not #'symbolp (ignore-errors (eval '(trace)))))

(defun %focus-repl (r)
  "Raise R and focus its input so the user can type the next call immediately."
  (when (and *desktop* r)
    (dt-raise *desktop* r)
    (setf (container-focus r) (or (find-view r 'input) (container-focus r)))
    (dt-refocus *desktop*) (invalidate *desktop*)))

(defun %tool-note (msg)
  "Echo MSG into the REPL transcript (opening/raising/focusing the REPL)."
  (let ((r (ensure-repl)))
    (when r
      (let ((sb (find-view r 'transcript)))
        (when sb (scrollback-append sb (format nil "; ~a~%" msg))))
      (%focus-repl r))))

(defun do-trace ()
  "TRACE a function, or UNTRACE it when already traced (output appears in REPL)."
  (let ((s (prompt-string " Trace (toggle) " "Function:")))
    (when (and s (plusp (length (string-trim " " s))))
      (let ((sym (%read-in-active s)))
        (cond
          ((not (and sym (symbolp sym))) (%tool-note (format nil "~a is not a function name." s)))
          ((member sym (%traced-symbols))
           (ignore-errors (eval (list 'untrace sym))) (%tool-note (format nil "untraced ~s" sym)))
          (t (ignore-errors (eval (list 'trace sym)))
             (%tool-note (format nil "tracing ~s — call it; trace output appears here" sym))))))))

(defun do-break-on-entry ()
  "TRACE a function with :break, so its next call stops in tv2's debugger."
  (let ((s (prompt-string " Break on entry " "Function (next call breaks):")))
    (when (and s (plusp (length (string-trim " " s))))
      (let ((sym (%read-in-active s)))
        (if (and sym (symbolp sym))
            (progn (ignore-errors (eval (list 'trace sym :break t)))
                   (%tool-note (format nil "break-on-entry armed on ~s (untrace to clear)" sym)))
            (%tool-note (format nil "~a is not a function name." s)))))))

(defun do-untrace-all ()
  (ignore-errors (eval '(untrace)))
  (%tool-note "untraced everything"))

(defun do-traced-list ()
  (let ((syms (%traced-symbols)))
    (%open-output " Traced functions "
                  (if syms (format nil "~{~s~%~}" syms) "No functions are currently traced."))))

;;; --- statistical profiling (sb-sprof, supplied by the app) ------------------

;;; (funcall fn FORM PACKAGE) -> a plist (:total :secs :mode :rows ...), each row
;;; a plist (:name :self :cumul :self% :cumul% ...).  NIL when no profiler.
(defvar *profile-fn* nil)

(defun %fn-short (name)
  (let ((s (princ-to-string name)))
    (if (> (length s) 46) (concatenate 'string (subseq s 0 43) "...") s)))

(defun do-profile ()
  "Prompt for a form, profile its evaluation, and show a flat report table."
  (if (null *profile-fn*)
      (%open-output " Profile " "No profiler backend is installed.")
      (let ((s (prompt-string " Profile " "Form to profile:")))
        (when (and s (plusp (length (string-trim " " s))))
          (let ((form (%read-in-active s)))
            (handler-case
                (let* ((res  (funcall *profile-fn* form (%active-package)))
                       (rows (sort (copy-list (getf res :rows)) #'>
                                   :key (lambda (r) (or (getf r :self%) 0))))
                       (rows (subseq rows 0 (min 100 (length rows)))))
                  (%open-table
                   (format nil " Profile: ~a  (~d samples, ~,2fs) " (string-trim " " s)
                           (getf res :total) (getf res :secs))
                   (list (list "Function" 46 (lambda (r) (%fn-short (getf r :name))))
                         (list "Self%"  7 (lambda (r) (format nil "~,1f" (getf r :self%))))
                         (list "Cumul%" 7 (lambda (r) (format nil "~,1f" (getf r :cumul%))))
                         (list "Self"   7 (lambda (r) (getf r :self)))
                         (list "Cumul"  7 (lambda (r) (getf r :cumul))))
                   rows))
              (error (e) (%open-output " Profile — error " (princ-to-string e)))))))))

;;; --- stepping / break (through the REPL + tv2's debugger) -------------------

(defun do-step ()
  "Evaluate a form under the single-stepper in the REPL; each step surfaces in
tv2's debugger (Step-next / Step-into / Step-out / Continue restarts)."
  (let ((s (prompt-string " Step " "Form to single-step:")))
    (when (and s (plusp (length (string-trim " " s))))
      (let ((r (ensure-repl)))
        (when r
          (%focus-repl r)
          (repl-submit-string r (format nil "(step ~a)" (string-trim " " s))))))))

;;; --- a couple of desktop helpers --------------------------------------------

(defun %open-table (title columns rows)
  (if *desktop*
      (dt-open *desktop* (lambda () (make-table-window title columns rows)))
      (multiple-value-bind (w f) (make-table-window title columns rows) (run-view w :focus f))))

;;; --- register a Tools menu --------------------------------------------------

(push (lambda (dt)
        (declare (ignore dt))
        (list "Tools"
              (list "Trace (toggle)…"  (lambda () (do-trace)))
              (list "Break on entry…"  (lambda () (do-break-on-entry)))
              (list "Untrace all"      (lambda () (do-untrace-all)))
              (list "Traced functions" (lambda () (do-traced-list)))
              (list "Profile…"         (lambda () (do-profile)))
              (list "Step…"            (lambda () (do-step)))))
      *extra-menus*)
