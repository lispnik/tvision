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

;;; --- the consolidated Browse menu (introspection + inspector + docs) --------
;;; Loaded last of the tool modules, so it can reference inspect.lisp's and
;;; nav.lisp's commands as well as its own.

;;; The single, consolidated "Lisp" menu (Turbo-Vision style, like classic
;;; tvlisp): evaluate/compile at the top, then Navigate / Document / Debug /
;;; Browse submenus.  docs.lisp loads last, so every do-* command it references
;;; (from compile, nav, tools, inspect and here) is already defined.  This
;;; replaces the former separate Run / Search / Debug / Browse top-level menus.
(push (lambda (dt)
        (list "Lisp"
              (list "Eval / compile defun" (lambda () (do-eval-defun)))         ; compile.lisp
              (list "Load buffer"          (lambda () (do-load-buffer)))
              (list "Compile buffer"       (lambda () (do-compile-buffer)))
              (list "Interrupt eval"       (lambda () (do-interrupt-eval)))
              :--
              (list "Inspect…"          (lambda () (do-inspect)))               ; inspect.lisp
              (list "Describe…"         (lambda () (do-describe)))
              (list "Macroexpand…"      (lambda () (do-macroexpand)))
              (list "Apropos…"          (lambda () (do-apropos)))
              :--
              (list "Navigate" :submenu                                          ; nav.lisp
                    (list "Go to definition…" (lambda () (do-goto-definition)))
                    (list "Pop back"          (lambda () (do-pop-back)))
                    :--
                    (list "Who calls…"        (lambda () (do-xref :calls "calls")))
                    (list "Who references…"   (lambda () (do-xref :references "references")))
                    (list "Who binds…"        (lambda () (do-xref :binds "binds")))
                    (list "Who sets…"         (lambda () (do-xref :sets "sets")))
                    (list "Who macroexpands…" (lambda () (do-xref :macroexpands "macroexpands"))))
              (list "Document" :submenu                                          ; the renderer allows only
                    (list "Documentation…"    (lambda () (do-documentation)))     ; two dropdown levels, so the
                    (list "Disassemble…"      (lambda () (do-disassemble)))       ; manuals are flat here rather
                    (list "Compiler notes…"   (lambda () (do-compile-notes)))     ; than a nested submenu
                    (list "Clear notes"       (lambda () (do-clear-notes)))       ; compile.lisp
                    :--
                    (list "HyperSpec lookup…" (lambda () (do-hyperspec)))
                    (list "SBCL manual" (lambda () (do-manual "SBCL manual" (cdr (assoc "SBCL manual" *manuals* :test #'string=)))))
                    (list "CCL manual"  (lambda () (do-manual "CCL manual"  (cdr (assoc "CCL manual"  *manuals* :test #'string=)))))
                    (list "ECL manual"  (lambda () (do-manual "ECL manual"  (cdr (assoc "ECL manual"  *manuals* :test #'string=))))))
              (list "Debug / trace" :submenu                                     ; tools.lisp
                    (list "Trace (toggle)…"  (lambda () (do-trace)))
                    (list "Trace package…"   (lambda () (do-trace-package)))
                    (list "Trace snapshots…" (lambda () (do-trace-snapshots)))
                    (list "Untrace all"      (lambda () (do-untrace-all)))
                    (list "Traced functions" (lambda () (do-traced-list)))
                    :--
                    (list "Break on entry…"     (lambda () (do-break-on-entry)))
                    (list "Conditional break…"  (lambda () (do-conditional-break)))
                    (list "Call tree…"          (lambda () (do-call-tree)))
                    (list "Step…"               (lambda () (do-step)))
                    :--
                    (list "Profile…"               (lambda () (do-profile)))
                    (list "Deterministic profile…" (lambda () (do-profile-deterministic))))
              (list "Browse" :submenu
                    (list "Class browser"    (lambda () (dt-open dt :classes)))
                    (list "Function browser" (lambda () (dt-open dt :functions)))
                    (list "Method browser…"  (lambda () (do-method-browser))))   ; nav.lisp
              (list "SBCL" :submenu                                              ; sbcl.lisp
                    (list "Type expand…"          (lambda () (do-typexpand)))
                    (list "Environment info…"     (lambda () (do-env-info)))
                    :--
                    (list "Allocation of value…"  (lambda () (do-allocation-info)))
                    (list "Allocation profile…"   (lambda () (do-aprof)))
                    :--
                    (list "GC / heap stats"       (lambda () (do-gc-stats)))
                    (list "GC now (full)"         (lambda () (do-gc-now)))
                    (list "Evaluator mode toggle" (lambda () (do-toggle-evaluator-mode)))
                    :--
                    (list "Locked packages"       (lambda () (do-package-locks)))
                    (list "Lock package…"         (lambda () (do-lock-package)))
                    (list "Unlock package…"       (lambda () (do-unlock-package))))
              :--
              (list "Object *" :submenu                                          ; inspect.lisp
                    (list "Clip last value"  (lambda () (do-clip-last-value)))
                    (list "Inspect *"        (lambda () (do-inspect-clipped)))
                    (list "Insert * as text" (lambda () (do-insert-clipped))))))
      *extra-menus*)
