;;;; sbcl.lisp --- SBCL-specific IDE tools that go beyond classic-tvlisp parity.
;;;;
;;;; Type expansion (sb-ext:typexpand), a value's heap allocation (generation),
;;;; the deterministic allocation profiler (sb-aprof, x86-64 only -- gated), a
;;;; GC / heap panel (room + get-bytes-consed + *gc-run-time*), the evaluator
;;;; mode toggle (*evaluator-mode*), package locks, and compile-time environment
;;;; introspection (sb-cltl2).  Each opens a tv2 output window; they hang off the
;;;; consolidated Lisp menu's "SBCL" submenu (see docs.lisp).

(in-package #:tv2)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (ignore-errors (require :sb-cltl2)))                 ; declaration/variable/function-information

;;; --- type expansion ---------------------------------------------------------

(defun do-typexpand ()
  "Expand a type specifier the way MACROEXPAND expands a form (one step + fully)."
  (let ((s (prompt-string " Type expand " "Type specifier:")))
    (when (and s (plusp (length (string-trim " " s))))
      (handler-case
          (let ((spec (%read-in-active s)))
            (multiple-value-bind (one onep) (sb-ext:typexpand-1 spec)
              (multiple-value-bind (full fullp) (sb-ext:typexpand spec)
                (%open-output (format nil " Type expand: ~a " (string-trim " " s))
                              (format nil "one step (typexpand-1):~%  ~s~:[   (not a derived type)~;~]~%~%~
                                           fully (typexpand):~%  ~s~:[   (already primitive)~;~]~%"
                                      one onep full fullp)))))
        (error (e) (%open-output " Type expand — error " (princ-to-string e)))))))

;;; --- a value's heap allocation ----------------------------------------------

(defun do-allocation-info ()
  "Evaluate a form and report where its value lives: which GC generation, or that
it is an immediate (SB-EXT:ALLOCATION-INFORMATION is gone; SB-KERNEL:GENERATION-OF
answers the useful part)."
  (let ((s (prompt-string " Allocation info " "Form (its value is examined):")))
    (when (and s (plusp (length (string-trim " " s))))
      (handler-case
          (let* ((val (let ((*package* (%active-package))) (eval (%read-in-active s))))
                 (gen (ignore-errors (sb-kernel:generation-of val))))
            (%open-output " Allocation info "
                          (format nil "value: ~a~%type:  ~s~%~a~%~%total bytes consed: ~:d~%"
                                  (%insp-repr val) (type-of val)
                                  (if gen (format nil "heap generation: ~d  (0 = nursery … higher = tenured)" gen)
                                      "immediate / not heap-allocated (no generation)")
                                  (sb-ext:get-bytes-consed))))
        (error (e) (%open-output " Allocation info — error " (princ-to-string e)))))))

;;; --- deterministic allocation profiler (sb-aprof; x86-64 only) --------------

(defun %aprof-available-p ()
  (and (find-package "SB-APROF")
       (let ((r (find-symbol "APROF-RUN" "SB-APROF"))) (and r (fboundp r) t))))   ; strict boolean

(defun do-aprof ()
  "Profile exactly what a form allocates, per call site (sb-aprof)."
  (if (not (%aprof-available-p))
      (%open-output " Allocation profiler (sb-aprof) "
                    (format nil "sb-aprof — the deterministic allocation profiler — is only built on~%x86-64.  This SBCL does not provide it (statistical alloc profiling is~%available via the Profile… command's :alloc mode)."))
      (let ((s (prompt-string " Allocation profile " "Form to profile:")))
        (when (and s (plusp (length (string-trim " " s))))
          (handler-case
              (let* ((run (find-symbol "APROF-RUN" "SB-APROF")) (form (%read-in-active s))
                     (txt (with-output-to-string (out)
                            (let ((*standard-output* out) (*package* (%active-package)))
                              (funcall run (lambda () (eval form)))))))
                (%open-output (format nil " Allocation profile: ~a " (string-trim " " s))
                              (if (plusp (length (string-trim '(#\Space #\Newline) txt))) txt
                                  "No allocation was recorded.")))
            (error (e) (%open-output " Allocation profile — error " (princ-to-string e))))))))

;;; --- GC / heap --------------------------------------------------------------

(defun %gc-stats-text ()
  (format nil "total bytes consed:  ~:d~%GC run time:         ~,3f s~%~%~a"
          (sb-ext:get-bytes-consed)
          (/ (float sb-ext:*gc-run-time*) internal-time-units-per-second)
          (with-output-to-string (s) (let ((*standard-output* s)) (room nil)))))

(defun do-gc-stats () (%open-output " GC / heap " (%gc-stats-text)))

(defun do-gc-now ()
  (let ((before (sb-ext:get-bytes-consed)))
    (sb-ext:gc :full t)
    (%tool-note (format nil "full GC done (~:d bytes consed up to now)" before))))

;;; --- evaluator mode ---------------------------------------------------------

(defun do-toggle-evaluator-mode ()
  "Switch SB-EXT:*EVALUATOR-MODE* between :COMPILE and :INTERPRET (how the REPL
and LOAD evaluate top-level forms)."
  (setf sb-ext:*evaluator-mode* (if (eq sb-ext:*evaluator-mode* :compile) :interpret :compile))
  (%tool-note (format nil "evaluator mode is now ~(~a~)" sb-ext:*evaluator-mode*)))

;;; --- package locks ----------------------------------------------------------

(defun do-package-locks ()
  (let ((locked '()))
    (dolist (p (list-all-packages))
      (when (ignore-errors (sb-ext:package-locked-p p)) (push (package-name p) locked)))
    (%open-output " Locked packages "
                  (if locked
                      (format nil "~d locked package~:p:~%~%~{  ~a~%~}~%(Lock / Unlock a package by name from the SBCL menu.)"
                              (length locked) (sort locked #'string<))
                      "No packages are locked."))))

(defun %package-lock-op (title verb fn)
  (let ((s (prompt-string title "Package:")))
    (when (and s (plusp (length (string-trim " " s))))
      (let ((p (find-package (string-upcase (string-trim " " s)))))
        (if p (progn (ignore-errors (funcall fn p))
                     (%tool-note (format nil "~a ~a" verb (package-name p))))
            (%tool-note (format nil "no such package: ~a" s)))))))

(defun do-lock-package ()   (%package-lock-op " Lock package "   "locked"   #'sb-ext:lock-package))
(defun do-unlock-package () (%package-lock-op " Unlock package " "unlocked" #'sb-ext:unlock-package))

;;; --- compile-time environment introspection (sb-cltl2) ----------------------

(defun do-env-info ()
  "Query the compile-time environment for a symbol: its variable- and function-
information (kind, locality, declared type/ftype) plus the global OPTIMIZE policy."
  (let ((s (prompt-string " Environment info " "Symbol:")))
    (when (and s (plusp (length (string-trim " " s))))
      (let ((sym (%read-in-active s)))
        (if (not (symbolp sym))
            (%open-output " Environment info " (format nil "~a is not a symbol." s))
            (%open-output (format nil " Environment info: ~a " sym)
                          (with-output-to-string (o)
                            (multiple-value-bind (k local decls)
                                (ignore-errors (sb-cltl2:variable-information sym nil))
                              (format o "variable-information~%  kind: ~s   local: ~s~%  declarations: ~s~%~%" k local decls))
                            (multiple-value-bind (k local decls)
                                (ignore-errors (sb-cltl2:function-information sym nil))
                              (format o "function-information~%  kind: ~s   local: ~s~%  declarations: ~s~%~%" k local decls))
                            (format o "global OPTIMIZE policy~%  ~s~%"
                                    (ignore-errors (sb-cltl2:declaration-information 'optimize nil))))))))))
