;;;; inspect.lisp --- the introspection-browser family, ported onto tv2.
;;;;
;;;; tvlisp ships a set of "look something up in the live image" windows: a Class
;;;; browser, a Function/GF browser, Apropos, Describe, and a Macroexpander.
;;;; They share two shapes already built on tv2 -- the filterable RUN-BROWSER
;;;; (filter a list, act on the choice) and a scrolling text pane -- so here each
;;;; is just a list of symbols + a text-producing function, reading symbols in
;;;; the live REPL's package (the same introspection the classic windows do:
;;;; DESCRIBE, SB-INTROSPECT arglists, the MOP, MACROEXPAND).

(in-package #:tv2)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :sb-introspect))

;;; --- a scrolling read-only text window --------------------------------------

(defun make-output-window (title text &key (help :browser))
  "A window showing multi-line TEXT in a scrollback (↑/↓/PgUp/PgDn/Home/End to
scroll).  Return (values WINDOW FOCUS)."
  (let* ((win (make-instance 'window :title title :keymap *global-keys*))
         (body (ui (stack
                     (:fill (scrollback :name 'out))
                     (1 (static-text :role :status :text " ↑/↓ · PgUp/PgDn · Home/End scroll · Esc: close "))))))
    (add-subview win body)
    (scrollback-append (find-view win 'out) text)
    (setf (sb-top (find-view win 'out)) 0 (sb-follow (find-view win 'out)) nil)
    (setf (window-scroll-target win) (find-view win 'out) (window-help win) :browser)
    (values win (find-view win 'out))))

(defun %open-output (title text)
  "Show TEXT in an output window, on the desktop when hosted, else full-screen."
  (if *desktop*
      (dt-open *desktop* (lambda () (make-output-window title text)))
      (multiple-value-bind (w f) (make-output-window title text) (run-view w :focus f))))

;;; --- package context + symbol gathering -------------------------------------

(defun %active-package ()
  "The package symbols should be read/printed in: the live REPL's, or *PACKAGE*."
  (let ((r (and *desktop* (find :repl (dt-windows *desktop*) :key #'window-kind))))
    (if r (repl-package r) *package*)))

(defun %read-in-active (string)
  "READ-FROM-STRING STRING in the active package (NIL on error)."
  (ignore-errors (let ((*package* (%active-package))) (read-from-string string))))

(defun %all-symbols (predicate)
  "Every interned symbol satisfying PREDICATE (deduplicated)."
  (let ((seen (make-hash-table :test 'eq)) (out '()))
    (do-all-symbols (s)
      (when (and (symbol-package s) (not (gethash s seen)) (ignore-errors (funcall predicate s)))
        (setf (gethash s seen) t) (push s out)))
    out))

;;; --- text producers (standard CL / SB-INTROSPECT / the MOP) -----------------

(defun %describe-text (sym)
  (with-output-to-string (s) (ignore-errors (describe sym s))))

(defun %function-text (sym)
  (with-output-to-string (s)
    (let ((kind (cond ((macro-function sym) "macro")
                      ((typep (and (fboundp sym) (fdefinition sym)) 'generic-function) "generic function")
                      ((fboundp sym) "function")
                      (t "(not a function)"))))
      (format s "~a~%~a~%~%Kind: ~a~%" sym (make-string (length (princ-to-string sym)) :initial-element #\=) kind))
    (when (or (fboundp sym) (macro-function sym))
      (format s "Arglist: ~a~%" (ignore-errors (sb-introspect:function-lambda-list sym))))
    (let ((d (ignore-errors (documentation sym 'function))))
      (when d (format s "~%Documentation:~%~a~%" d)))
    (let ((fn (ignore-errors (fdefinition sym))))
      (when (typep fn 'generic-function)
        (format s "~%Methods:~%")
        (dolist (m (ignore-errors (sb-mop:generic-function-methods fn)))
          (format s "  ~a~%" m))))))

(defun %class-text (sym)
  (let ((c (find-class sym nil)))
    (with-output-to-string (s)
      (if (null c)
          (format s "~a is not a class." sym)
          (progn
            (ignore-errors (sb-mop:finalize-inheritance c))
            (format s "Class ~a~%~a~%~%" (class-name c)
                    (make-string (+ 6 (length (princ-to-string (class-name c)))) :initial-element #\=))
            (let ((d (ignore-errors (documentation c 'type)))) (when d (format s "~a~%~%" d)))
            (format s "Precedence list:~%")
            (dolist (super (ignore-errors (sb-mop:class-precedence-list c)))
              (format s "  ~a~%" (class-name super)))
            (format s "~%Direct slots:~%")
            (dolist (slot (ignore-errors (sb-mop:class-direct-slots c)))
              (format s "  ~a~%" (sb-mop:slot-definition-name slot)))
            (format s "~%Direct subclasses:~%")
            (dolist (sub (ignore-errors (sb-mop:class-direct-subclasses c)))
              (format s "  ~a~%" (class-name sub))))))))

(defun %pprint-to-string (form) (with-output-to-string (o) (let ((*print-pretty* t)) (pprint form o))))

(defun %macroexpand-text (form)
  (with-output-to-string (s)
    (format s "Form:~a~%~%" (%pprint-to-string form))
    (format s "macroexpand-1:~a~%~%" (%pprint-to-string (ignore-errors (macroexpand-1 form))))
    (format s "macroexpand (full):~a~%" (%pprint-to-string (ignore-errors (macroexpand form))))))

;;; --- the symbol-list browser ------------------------------------------------

(defun make-symbol-browser (title symbols detail-fn)
  "A filterable browser over SYMBOLS; Enter opens DETAIL-FN's text in an output
window.  Return (values WINDOW FOCUS)."
  (let* ((tab (make-hash-table :test 'equal)) (acc '()))
    (dolist (s symbols)
      (let ((label (format nil "~a:~a"
                           (if (symbol-package s) (package-name (symbol-package s)) "#")
                           (symbol-name s))))
        (unless (gethash label tab) (setf (gethash label tab) s) (push label acc))))
    (make-browser title (sort acc #'string<)
                  (lambda (item set)
                    (declare (ignore set))
                    (let ((sym (gethash item tab)))
                      (when sym (%open-output (format nil " ~a " item) (funcall detail-fn sym))))))))

(defun make-class-browser ()
  "Browse every class in the image; Enter shows precedence list / slots / subs."
  (make-symbol-browser " tv2 — Class browser (introspection) "
                       (%all-symbols (lambda (s) (find-class s nil))) #'%class-text))

(defun make-function-browser ()
  "Browse every function / macro / GF; Enter shows kind, arglist, doc, methods."
  (make-symbol-browser " tv2 — Function browser (introspection) "
                       (%all-symbols (lambda (s) (or (fboundp s) (macro-function s)))) #'%function-text))

;;; --- prompted actions: Apropos / Describe / Macroexpand ---------------------

(defun do-apropos ()
  (let ((term (prompt-string " Apropos " "Substring:")))
    (when (and term (plusp (length (string-trim " " term))))
      (let ((syms (ignore-errors (apropos-list (string-trim " " term)))))
        (if (null syms)
            (%open-output (format nil " Apropos ~s " term) (format nil "No symbols match ~s." term))
            (let ((title (format nil " Apropos ~s (~d) " (string-trim " " term) (length syms))))
              (if *desktop*
                  (dt-open *desktop* (lambda () (make-symbol-browser title syms #'%describe-text)))
                  (multiple-value-bind (w f) (make-symbol-browser title syms #'%describe-text)
                    (run-view w :focus f)))))))))

(defun do-describe ()
  (let ((name (prompt-string " Describe " "Symbol:")))
    (when (and name (plusp (length (string-trim " " name))))
      (%open-output (format nil " Describe ~a " name) (%describe-text (%read-in-active name))))))

(defun do-macroexpand ()
  (let ((str (prompt-string " Macroexpand " "Form:")))
    (when (and str (plusp (length (string-trim " " str))))
      (let ((form (%read-in-active str)))
        (if form (%open-output " Macroexpand " (%macroexpand-text form))
            (%open-output " Macroexpand " (format nil "Could not read a form from ~s." str)))))))

;;; --- register with the desktop: builders + an Inspect menu ------------------

(setf *window-builders*
      (append *window-builders*
              (list (cons :classes   #'make-class-browser)
                    (cons :functions #'make-function-browser))))

(push (lambda (dt)
        (list "Inspect"
              (list "Class browser"    (lambda () (dt-open dt :classes)))
              (list "Function browser" (lambda () (dt-open dt :functions)))
              (list "Apropos…"         (lambda () (do-apropos)))
              (list "Describe…"        (lambda () (do-describe)))
              (list "Macroexpand…"     (lambda () (do-macroexpand)))))
      *extra-menus*)
