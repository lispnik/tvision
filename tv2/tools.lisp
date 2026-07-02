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

(defun make-table-window (title columns rows &key (help :browser) on-activate)
  "A window over a TABLE-VIEW.  COLUMNS = list of (TITLE WIDTH ACCESSOR); ROWS =
row objects.  ON-ACTIVATE (tv row) fires on Enter/double-click.  Return (values
WINDOW FOCUS)."
  (let* ((win  (make-instance 'window :title title :keymap *global-keys*))
         (body (ui (stack
                     (:fill (table-view :name 'tbl :columns columns :rows rows :on-activate on-activate))
                     (1 (static-text :role :status :text " ↑/↓ · PgUp/PgDn select · Enter: open · Esc: close "))))))
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
    (setf (container-focus r) (or (find-view r 'transcript) (container-focus r)))
    (dt-refocus *desktop*) (invalidate *desktop*)))

(defun %tool-note (msg)
  "Show MSG as a transient status-bar note and log it to the REPL transcript,
WITHOUT raising or refocusing any window (so a tool action never yanks the REPL
over the editor you're working in)."
  (setf *tool-message* msg *tool-message-time* (get-internal-real-time))
  (when *desktop*
    (ignore-errors (invalidate (dt-statusbar *desktop*)))
    (let ((r (%dt-repl *desktop*)))                          ; log to an existing REPL, in place
      (when r
        (let ((sb (find-view r 'transcript)))
          (when sb (scrollback-append sb (format nil "; ~a~%" msg))))))))

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

(defun do-conditional-break ()
  "TRACE a function with :break gated by a predicate FORM (SBCL binds the args as
a list to SB-DEBUG:ARG, e.g. (> (car sb-debug:arg) 100)); the next matching call
stops in tv2's debugger."
  (let ((s (prompt-string " Conditional break " "Function:")))
    (when (and s (plusp (length (string-trim " " s))))
      (let ((sym (%read-in-active s)))
        (if (and sym (symbolp sym))
            (let ((c (prompt-string " Conditional break "
                                    "Break when (form; args as e.g. (sb-debug:arg 0)):")))
              (when (and c (plusp (length (string-trim " " c))))
                (let ((form (%read-in-active c)))
                  (ignore-errors (eval (list 'trace sym :break form)))
                  (%tool-note (format nil "conditional break armed on ~s when ~a (untrace to clear)"
                                      sym (string-trim " " c))))))
            (%tool-note (format nil "~a is not a function name." s)))))))

(defun do-trace-package ()
  "TRACE every external function of a package (macros / special operators skipped)."
  (let ((p (prompt-string " Trace package " "Package (traces its exported functions):")))
    (when (and p (plusp (length (string-trim " " p))))
      (let* ((pkg (find-package (string-upcase (string-trim " " p))))
             (pn  (and pkg (package-name pkg))))
        (cond
          ((null pkg) (%tool-note (format nil "no such package: ~a" p)))
          ((and pn (or (string= pn "COMMON-LISP") (string= pn "KEYWORD")
                       (and (>= (length pn) 3) (string= (subseq pn 0 3) "SB-"))))
           (%tool-note (format nil "refusing to trace all of ~a — tracing CL/SBCL internals wholesale (format, length…) recurses through the trace output and hangs the IDE.  Trace individual functions instead." pn)))
          (t
            (let ((syms '()))
              (do-external-symbols (s pkg)
                (when (and (fboundp s) (not (macro-function s)) (not (special-operator-p s)))
                  (push s syms)))
              (dolist (s syms) (ignore-errors (eval (list 'trace s))))
              (%tool-note (format nil "tracing ~d function~:p in ~a — call them; output appears here"
                                  (length syms) (package-name pkg))))))))))

;;; --- trace snapshots (named saved sets of traced functions) -----------------

(defvar *trace-snapshots* '() "Named saved sets of traced functions: (name . (symbol ...)).")

(defun do-trace-snapshots ()
  "Save the current traced-function set under a name, or restore a saved one
 (untraces everything, then traces the snapshot's functions)."
  (let* ((choices (cons "Save current set…"
                        (mapcar (lambda (s) (format nil "Restore: ~a (~d)" (car s) (length (cdr s))))
                                *trace-snapshots*)))
         (pick (popup-choose choices :title " Trace snapshots ")))
    (when pick
      (cond
        ((string= pick "Save current set…")
         (let ((name (prompt-string " Save trace snapshot " "Name:")))
           (when (and name (plusp (length (string-trim " " name))))
             (setf name (string-trim " " name))
             (setf *trace-snapshots*
                   (cons (cons name (%traced-symbols))
                         (remove name *trace-snapshots* :key #'car :test #'string=)))
             (%tool-note (format nil "saved trace snapshot ~a (~d function~:p)" name (length (%traced-symbols)))))))
        (t                                          ; restore
         (let* ((idx (position pick choices :test #'string=))
                (snap (nth (1- idx) *trace-snapshots*)))
           (when snap
             (ignore-errors (eval '(untrace)))
             (dolist (s (cdr snap)) (ignore-errors (eval (list 'trace s))))
             (%tool-note (format nil "restored snapshot ~a: tracing ~d function~:p"
                                 (car snap) (length (cdr snap)))))))))))

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

;;; --- deterministic profiling (sb-profile) -----------------------------------

(defun do-profile-deterministic ()
  "Instrument every function in a package (sb-profile), run a form, and show the
exact call-count / time report."
  (let ((pkgname (prompt-string " Deterministic profile " "Profile functions in package:")))
    (when (and pkgname (plusp (length (string-trim " " pkgname))))
      (let ((form-s (prompt-string " Deterministic profile " "Form to run:")))
        (when (and form-s (plusp (length (string-trim " " form-s))))
          (let ((pkg  (or (find-package (string-upcase (string-trim " " pkgname))) (%active-package)))
                (form (ignore-errors (let ((*package* (%active-package))) (read-from-string form-s)))))
            ;; run on a worker thread so a long form doesn't freeze the IDE, and
            ;; only reset when nothing else is profiled (don't wipe the user's
            ;; separately-instrumented counters)
            (sb-thread:make-thread
             (lambda ()
               (handler-case
                   (let ((others (sb-profile:profile)) (txt nil))
                     (when (null others) (sb-profile:reset))
                     (eval (list 'sb-profile:profile (package-name pkg)))
                     (unwind-protect
                          (progn (let ((*standard-output* (make-string-output-stream)) (*package* pkg)) (eval form))
                                 (setf txt (with-output-to-string (s)
                                             (let ((*standard-output* s) (*trace-output* s)) (sb-profile:report)))))
                       (eval (list 'sb-profile:unprofile (package-name pkg)))
                       (when (null others) (sb-profile:reset)))
                     (run-on-ui (lambda () (%open-output (format nil " Deterministic profile: ~a " pkgname)
                                                         (if (and txt (plusp (length (string-trim '(#\Space #\Newline) txt)))) txt
                                                             "No calls were recorded.")))))
                 (error (e) (run-on-ui (lambda () (%open-output " Deterministic profile " (princ-to-string e)))))))
             :name "tv2-det-profile")))))))

;;; --- call-tree tracing (encapsulation-based watch, a navigable tree) --------
;;; Distinct from cl:trace (which dumps indented text): watched functions are
;;; encapsulated so every call/return/error is recorded with the live argument
;;; and result objects, shown as an indented tree whose rows are inspectable.

(defvar *ct-log* '())                              ; rows, most-recent first
(defvar *ct-count* 0)
(defvar *ct-depth* 0)                              ; dynamic call depth (per thread)
(defvar *ct-watched* '())                          ; watched symbols
(defparameter *ct-limit* 4000)
(defvar *ct-lock* (sb-thread:make-mutex :name "tv2-calltree"))

(defun %ct-record (row)
  (sb-thread:with-mutex (*ct-lock*)
    (push row *ct-log*) (incf *ct-count*)
    (when (> *ct-count* (* 2 *ct-limit*))
      (setf *ct-log* (subseq *ct-log* 0 *ct-limit*) *ct-count* *ct-limit*))))

(defun %ct-snapshot () (sb-thread:with-mutex (*ct-lock*) (reverse *ct-log*)))
(defun %ct-clear () (sb-thread:with-mutex (*ct-lock*) (setf *ct-log* '() *ct-count* 0)))

(defun %ct-system-symbol-p (sym)
  "True for functions the recording machinery itself calls (CL / SBCL internals).
Encapsulating one of these re-enters the recorder and corrupts the image, so the
call tree refuses them; watch your own functions instead."
  (let ((p (symbol-package sym)))
    (and p (let ((n (package-name p)))
             (or (string= n "COMMON-LISP") (string= n "KEYWORD")
                 (and (>= (length n) 3) (string= (subseq n 0 3) "SB-")))))))

(defun %ct-watch (sym)
  "Encapsulate SYM so its calls/returns are recorded into the call-tree log.
Returns :SYSTEM (refused) for a CL/SBCL-internal function, T when newly watched,
or NIL when it was already watched."
  (when (%ct-system-symbol-p sym) (return-from %ct-watch :system))
  (unless (member sym *ct-watched*)
    (sb-int:encapsulate
     sym 'tv2-calltree
     (lambda (fn &rest args)
       (let ((d *ct-depth*))
         (%ct-record (list :call d sym (copy-list args)))
         (let ((*ct-depth* (1+ d)))
           (handler-case
               (let ((vals (multiple-value-list (apply fn args))))
                 (%ct-record (list :return d sym vals)) (values-list vals))
             (serious-condition (c) (%ct-record (list :error d sym c)) (error c)))))))
    (push sym *ct-watched*)))

(defun %ct-unwatch (sym)
  (when (member sym *ct-watched*)
    (ignore-errors (sb-int:unencapsulate sym 'tv2-calltree))
    (setf *ct-watched* (remove sym *ct-watched*))))

(defun %ct-row-label (row)
  (destructuring-bind (kind depth name payload) row
    (let ((ind (make-string (* 2 (min depth 24)) :initial-element #\Space))
          (*print-length* 4) (*print-level* 2) (*print-pretty* nil) (*print-readably* nil))
      (flet ((pr (x) (handler-case (prin1-to-string x) (error () "#<?>"))))
        (case kind
          (:call   (format nil "~a› (~(~a~)~{ ~a~})" ind name (mapcar #'pr payload)))
          (:return (format nil "~a‹ ~(~a~) ⇒ ~{~a~^, ~}" ind name
                           (or (mapcar #'pr payload) '("; no values"))))
          (:error  (format nil "~a✗ ~(~a~) signalled ~a" ind name (pr payload))))))))

(defclass call-tree-window (window) ((rows :initform nil :accessor ct-rows))
  (:metaclass reactive-class))

(defun %ct-refresh (win)
  (let ((lb (find-view win 'ct)))
    (setf (ct-rows win) (%ct-snapshot))
    (when lb
      (setf (list-items lb) (or (mapcar #'%ct-row-label (ct-rows win))
                                (list "(no calls yet — press `a' to watch a function)"))
            (list-selected lb) (min (list-selected lb) (max 0 (1- (length (list-items lb))))))
      (setf (window-title win)
            (format nil " Call tree — ~d watched · ~d call~:p   a:watch u:unwatch c:clear r:refresh "
                    (length *ct-watched*) (length (ct-rows win))))
      (invalidate win))))

(defun %ct-inspect-row (win row)
  (when row
    (destructuring-bind (kind depth name payload) row
      (declare (ignore depth))
      (let* ((obj (case kind
                    (:return (if (= 1 (length payload)) (first payload) payload))
                    (t payload)))
             (label (format nil "~(~a~) of ~(~a~)" kind name)))
        (when *desktop* (dt-open *desktop* (lambda () (make-inspector obj label))))))))

(define-command ct-watch (v e)
  (let ((win (view-root v))
        (s (prompt-string " Watch function " "Function to add to the call tree:")))
    (when (and s (plusp (length (string-trim " " s))))
      (let ((sym (%read-in-active s)))
        (cond
          ((not (and sym (symbolp sym) (fboundp sym)))
           (%open-output " Call tree " (format nil "~a is not a function." (string-trim " " s))))
          ((eq (%ct-watch sym) :system)
           (%open-output " Call tree "
                         (format nil "Refusing to watch ~s: CL/SBCL internals are called by the~%recorder itself — watching one corrupts the image.  Watch your own functions." sym)))))
      (%ct-refresh win))))

(define-command ct-unwatch (v e)
  (let ((win (view-root v)))
    (when *ct-watched*
      (let ((pick (popup-choose (mapcar (lambda (s) (format nil "~s" s)) *ct-watched*)
                                :title " Unwatch function ")))
        (when pick
          (let ((sym (find pick *ct-watched* :key (lambda (s) (format nil "~s" s)) :test #'string=)))
            (when sym (%ct-unwatch sym))))))
    (%ct-refresh win)))

(define-command ct-clear (v e) (%ct-clear) (%ct-refresh (view-root v)))
(define-command ct-refresh (v e) (%ct-refresh (view-root v)))

(defkeymap *call-tree-keys* (*global-keys*)
  (#\a ct-watch) (#\u ct-unwatch) (#\c ct-clear) (#\r ct-refresh))

(defun make-call-tree ()
  "Build the call-tree window.  Return (values WINDOW FOCUS)."
  (let* ((win (make-instance 'call-tree-window :title " Call tree " :keymap *global-keys*))
         (body (ui (stack
                     (:fill (list-box :name 'ct :keymap *call-tree-keys*
                              :on-activate (lambda (lb item) (declare (ignore item))
                                             (let ((win (view-root lb)))
                                               (%ct-inspect-row win (nth (list-selected lb) (ct-rows win)))))))
                     (1 (static-text :role :status
                          :text " Enter: inspect row · a:watch u:unwatch c:clear r:refresh · Esc: close "))))))
    (add-subview win body)
    (setf (window-scroll-target win) (find-view win 'ct) (window-help win) :browser)
    (%ct-refresh win)
    (values win (find-view win 'ct))))

(defun do-call-tree ()
  (if *desktop*
      (dt-open *desktop* #'make-call-tree)
      (multiple-value-bind (w f) (make-call-tree) (run-view w :focus f))))

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

;; The trace / break / call-tree / step / profile commands are surfaced through
;; the consolidated "Lisp" menu's Debug submenu (docs.lisp).
