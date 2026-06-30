;;;; nav.lisp --- source navigation: go-to-definition + xref (who-calls/...).
;;;;
;;;; Standard SB-INTROSPECT, like the other introspection tools: a symbol's
;;;; definition sources and its callers/references/binders/setters become rows
;;;; in a clickable table window that opens the source file at the right line.

(in-package #:tv2)

(defun %offset-to-line (path char-offset)
  "1-based line number for CHAR-OFFSET in PATH (or 1)."
  (or (ignore-errors
        (with-open-file (s path :direction :input :external-format :utf-8)
          (let ((n 1))
            (dotimes (i (or char-offset 0) n)
              (let ((c (read-char s nil nil)))
                (unless c (return n))
                (when (char= c #\Newline) (incf n)))))))
      1))

(defun open-source-at (path line)
  "Open PATH in an editor at LINE, reusing an already-open editor for that file."
  (%open-file-at path line))

(defun %show-locations (title rows)
  "ROWS = list of (LABEL PATH LINE); a table window whose Enter opens the source."
  (if (null rows)
      (%open-output title "No locations found.")
      (let ((cols (list (list "What" 36 #'first)
                        (list "File" 24 (lambda (r) (if (second r) (file-namestring (second r)) "?")))
                        (list "Line" 6 #'third))))
        (flet ((open-row (tv row) (declare (ignore tv)) (open-source-at (second row) (third row))))
          (if *desktop*
              (dt-open *desktop* (lambda () (make-table-window title cols rows :on-activate #'open-row)))
              (multiple-value-bind (w f) (make-table-window title cols rows :on-activate #'open-row)
                (run-view w :focus f)))))))

;;; --- go to definition -------------------------------------------------------

(defun %symbol-definitions (sym)
  "List of (LABEL PATH LINE) source locations for SYM (via SB-INTROSPECT)."
  (let ((out '()))
    (dolist (type '(:function :generic-function :macro :variable :class :structure
                    :condition :method :compiler-macro :setf-expander :type))
      (ignore-errors
        (dolist (src (sb-introspect:find-definition-sources-by-name sym type))
          (let ((path (sb-introspect:definition-source-pathname src)))
            (when path
              (push (list (format nil "~(~a~)  ~a" type sym) (namestring path)
                          (%offset-to-line (namestring path)
                                           (sb-introspect:definition-source-character-offset src)))
                    out))))))
    (remove-duplicates (nreverse out) :test #'equal)))

(defun do-goto-definition ()
  (let ((name (prompt-string " Go to definition " "Symbol:")))
    (when (and name (plusp (length (string-trim " " name))))
      (let* ((sym (%read-in-active name)) (defs (and sym (%symbol-definitions sym))))
        (cond ((null defs) (%open-output " Go to definition "
                                         (format nil "No source location for ~a." name)))
              ((null (cdr defs)) (open-source-at (second (first defs)) (third (first defs))))
              (t (%show-locations (format nil " Definitions of ~a " name) defs)))))))

;;; --- xref: who-calls / -references / -binds / -sets / -macroexpands ---------

(defun %xref (kind sym)
  "List of (LABEL PATH LINE) for the WHO-KIND of SYM."
  (let ((fn (ecase kind
              (:calls #'sb-introspect:who-calls)   (:references #'sb-introspect:who-references)
              (:binds #'sb-introspect:who-binds)   (:sets #'sb-introspect:who-sets)
              (:macroexpands #'sb-introspect:who-macroexpands)))
        (out '()))
    (dolist (entry (ignore-errors (funcall fn sym)))
      (let* ((nm (car entry)) (src (cdr entry))
             (path (ignore-errors (sb-introspect:definition-source-pathname src)))
             (off  (ignore-errors (sb-introspect:definition-source-character-offset src))))
        (push (list (princ-to-string nm) (and path (namestring path))
                    (if path (%offset-to-line (namestring path) off) 0))
              out)))
    (nreverse out)))

(defun do-xref (kind label)
  (let ((name (prompt-string (format nil " Who ~a " label) "Symbol:")))
    (when (and name (plusp (length (string-trim " " name))))
      (let ((sym (%read-in-active name)))
        (%show-locations (format nil " Who ~a ~a " label name)
                         (and sym (symbolp sym) (%xref kind sym)))))))

;;; --- method browser ---------------------------------------------------------

(defun %method-label (m)
  "A printed signature for method M (qualifiers + specializers)."
  (let ((quals (sb-mop:method-qualifiers m))
        (specs (mapcar (lambda (s)
                         (if (typep s 'class) (class-name s)
                             (ignore-errors (list 'eql (sb-mop:eql-specializer-object s)))))
                       (sb-mop:method-specializers m))))
    (string-trim " " (format nil "~{~(~a~)~^ ~} (~{~a~^ ~})" quals specs))))

(defun %gf-methods (gf)
  "List of (LABEL PATH LINE) for the methods of generic function GF."
  (let ((out '()))
    (dolist (m (ignore-errors (sb-mop:generic-function-methods gf)))
      (let* ((src (ignore-errors (sb-introspect:find-definition-source (sb-mop:method-function m))))
             (path (and src (sb-introspect:definition-source-pathname src)))
             (off  (and src (sb-introspect:definition-source-character-offset src))))
        (push (list (%method-label m) (and path (namestring path))
                    (if path (%offset-to-line (namestring path) off) 0))
              out)))
    (nreverse out)))

(defun do-method-browser ()
  (let ((name (prompt-string " Method browser " "Generic function:")))
    (when (and name (plusp (length (string-trim " " name))))
      (let* ((sym (%read-in-active name)) (fn (and sym (symbolp sym) (fboundp sym) (fdefinition sym))))
        (if (typep fn 'generic-function)
            (%show-locations (format nil " Methods of ~a " name) (%gf-methods fn))
            (%open-output " Method browser " (format nil "~a is not a generic function." name)))))))

;;; --- a Navigate menu --------------------------------------------------------

(push (lambda (dt)
        (declare (ignore dt))
        (list "Navigate"
              (list "Go to definition…" (lambda () (do-goto-definition)))
              (list "Method browser…"   (lambda () (do-method-browser)))
              (list "Who calls…"        (lambda () (do-xref :calls "calls")))
              (list "Who references…"   (lambda () (do-xref :references "references")))
              (list "Who binds…"        (lambda () (do-xref :binds "binds")))
              (list "Who sets…"         (lambda () (do-xref :sets "sets")))
              (list "Who macroexpands…" (lambda () (do-xref :macroexpands "macroexpands")))))
      *extra-menus*)
