;;;; docs.lisp --- documentation, disassembly, HyperSpec, and manual browsing.
;;;;
;;;; Documentation and disassembly are standard CL into a text window.  The
;;;; HyperSpec lookup and manual pages fetch real HTML (via *URL-FETCH-FN*) and
;;;; render it in the doc browser; the symbol -> CLHS URL map comes from
;;;; *HYPERSPEC-URL-FN* (tvlisp's hyperspec-url).

(in-package #:tv2)

;;; (funcall fn NAME) -> the symbol's HyperSpec page URL, or NIL.
(defvar *hyperspec-url-fn* nil)

;;; --- documentation (docstrings only) ----------------------------------------

(defun %documentation-text (sym)
  (with-output-to-string (o)
    (let ((any nil))
      (dolist (ty '(function variable type structure setf compiler-macro method-combination))
        (let ((d (ignore-errors (documentation sym ty))))
          (when d (setf any t) (format o "~(~a~):~%~a~%~%" ty d))))
      (unless any (format o "~a has no documentation." sym)))))

(defun do-documentation ()
  (let ((name (prompt-string " Documentation " "Symbol:")))
    (when (and name (plusp (length (string-trim " " name))))
      (let ((sym (%read-in-active name)))
        (%open-output (format nil " Documentation ~a " name)
                      (if sym (%documentation-text sym) (format nil "Could not read ~a." name)))))))

;;; --- disassemble ------------------------------------------------------------

(defun do-disassemble ()
  (let ((name (prompt-string " Disassemble " "Function:")))
    (when (and name (plusp (length (string-trim " " name))))
      (let ((sym (%read-in-active name)))
        (%open-output (format nil " Disassemble ~a " name)
                      (if (and sym (fboundp sym))
                          (with-output-to-string (o)
                            (let ((*standard-output* o)) (ignore-errors (disassemble sym))))
                          (format nil "~a is not a function." name)))))))

;;; --- HyperSpec + manuals (fetched HTML) -------------------------------------

(defun %open-doc-browser (title html &optional base)
  (if *desktop*
      (dt-open *desktop* (lambda () (make-doc-browser title html base)))
      (multiple-value-bind (w f o) (make-doc-browser title html base) (run-view w :focus f :open o))))

(defun %browse-url (title url)
  "Fetch URL and render it, or report why not."
  (cond
    ((null *url-fetch-fn*) (%open-output title "No URL fetcher installed (need curl)."))
    (t (let ((html (funcall *url-fetch-fn* url)))
         (if html (%open-doc-browser title html url)
             (%open-output title (format nil "Could not fetch~%~a" url)))))))

(defun do-hyperspec ()
  (let ((name (prompt-string " HyperSpec lookup " "Symbol:")))
    (when (and name (plusp (length (string-trim " " name))))
      (let ((url (and *hyperspec-url-fn*
                      (ignore-errors (funcall *hyperspec-url-fn* (string-trim " " name))))))
        (if url (%browse-url (format nil " CLHS: ~a " name) url)
            (%open-output " HyperSpec lookup "
                          (format nil "No HyperSpec entry for ~a.~%(Standard CL symbols only.)" name)))))))

(defparameter *manuals*
  '(("SBCL manual" . "http://www.sbcl.org/manual/index.html")
    ("CCL manual"  . "https://ccl.clozure.com/docs/ccl.html")
    ("ECL manual"  . "https://ecl.common-lisp.dev/static/files/manual/current-manual/")))

(defun do-manual (name url) (%browse-url (format nil " ~a " name) url))

;;; --- a Docs menu ------------------------------------------------------------

(push (lambda (dt)
        (declare (ignore dt))
        (list "Docs"
              (list "Documentation…"   (lambda () (do-documentation)))
              (list "Disassemble…"      (lambda () (do-disassemble)))
              (list "HyperSpec lookup…" (lambda () (do-hyperspec)))
              (list "Manuals" :submenu
                    (list "SBCL manual" (lambda () (do-manual "SBCL manual" (cdr (assoc "SBCL manual" *manuals* :test #'string=)))))
                    (list "CCL manual"  (lambda () (do-manual "CCL manual"  (cdr (assoc "CCL manual"  *manuals* :test #'string=)))))
                    (list "ECL manual"  (lambda () (do-manual "ECL manual"  (cdr (assoc "ECL manual"  *manuals* :test #'string=))))))))
      *extra-menus*)
