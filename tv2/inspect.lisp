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

;;; --- object inspector -------------------------------------------------------
;;; A drillable object tree.  The object -> outline-node builder is a hook: tv2
;;; ships a compact built-in (works standalone); tvlisp-tv2 swaps in tvlisp's
;;; richer OBJECT->OUTLINE (cycle detection, paging, slot setters, package/symbol
;;; specialisation).  Either way the result is a tvision outline-node tree, which
;;; tv2's OUTLINE renders directly.

(defun %insp-repr (obj)
  (let ((*print-length* 6) (*print-level* 2) (*print-readably* nil))
    (let ((s (handler-case (prin1-to-string obj) (error () "#<unprintable>"))))
      (if (> (length s) 56) (concatenate 'string (subseq s 0 53) "...") s))))

(defun %tv2-object->outline (obj label &optional (depth 2) path)
  "tv2's built-in object -> outline-node tree (depth-limited, cycle-guarded,
error-robust).  Overridable via *OBJECT->OUTLINE-FN*."
  (if (member obj path :test #'eq)
      (tvision:make-outline-node (format nil "~a = ~a  [circular]" label (%insp-repr obj)) nil obj)
      (let ((children '()) (path* (cons obj path)))
        (when (plusp depth)
          (flet ((kid (v lbl)
                   (push (handler-case (%tv2-object->outline v lbl (1- depth) path*)
                           (serious-condition (e) (tvision:make-outline-node (format nil "~a = <~a>" lbl (type-of e)))))
                         children)))
            (handler-case
                (typecase obj
                  (string nil)
                  (cons (let ((i 0)) (dolist (x obj) (when (< i 200) (kid x (format nil "[~d]" i))) (incf i))))
                  (vector (dotimes (i (min (length obj) 200)) (kid (aref obj i) (format nil "[~d]" i))))
                  (hash-table (let ((i 0)) (maphash (lambda (k v) (when (< i 200) (kid v (format nil "~a =>" (%insp-repr k)))) (incf i)) obj)))
                  ((or structure-object standard-object)
                   (dolist (slot (handler-case (sb-mop:class-slots (class-of obj)) (error () nil)))
                     (let ((name (sb-mop:slot-definition-name slot)))
                       (when (handler-case (slot-boundp obj name) (error () nil))
                         (kid (handler-case (slot-value obj name) (serious-condition (e) e)) (format nil "~a" name)))))))
              (serious-condition () nil))))
        (let ((node (tvision:make-outline-node (format nil "~a = ~a" label (%insp-repr obj)) (nreverse children) obj)))
          (setf (tvision:outline-node-expanded node) t)
          node))))

;;; Set by an embedding app (tvlisp-tv2) to tvlisp's richer OBJECT->OUTLINE.
(defvar *object->outline-fn* #'%tv2-object->outline)

(defclass inspector-window (window)
  ((cur  :initform nil :accessor insp-current)   ; (obj . label) shown now
   (back :initform nil :accessor insp-back)       ; back-stack of (obj . label)
   (fwd  :initform nil :accessor insp-fwd))       ; forward-stack (after Back)
  (:metaclass reactive-class))

(defun %node-label (node)
  "The label portion (before \" = \") of NODE's text."
  (let* ((txt (tvision:outline-node-text node)) (sep (search " = " txt)))
    (if sep (subseq txt 0 sep) "value")))

(defun %insp-retitle (w)
  (let* ((crumbs (append (reverse (mapcar #'cdr (insp-back w)))
                         (and (insp-current w) (list (cdr (insp-current w))))))
         (path (format nil "~{~a~^ > ~}" crumbs))
         (path (if (> (length path) 44) (concatenate 'string "…" (subseq path (- (length path) 43))) path)))
    (setf (window-title w)
          (format nil " Inspector: ~a  (Enter: drill~:[~; · Bksp: back~]) " path (insp-back w)))
    (invalidate w)))

(defun %insp-show (w obj label)
  "Re-root W's tree on (OBJ . LABEL) in place (no history change)."
  (let ((ol (find-view w 'tree)))
    (setf (insp-current w) (cons obj label)
          (outline-roots ol) (list (funcall *object->outline-fn* obj label))
          (outline-focused ol) 0 (outline-top ol) 0)
    (%insp-retitle w) (invalidate ol)))

(define-command insp-drill (v e)
  "Re-root the inspector on the focused node's value (remembering the current
view for Back)."
  (let* ((w (view-root v)) (n (ov-current v)))
    (when n
      (let ((val (tvision:outline-node-data n)))
        (push (insp-current w) (insp-back w)) (setf (insp-fwd w) nil)
        (%insp-show w val (%node-label n))))))

(define-command insp-back (v e)
  (let ((w (view-root v)))
    (when (insp-back w)
      (push (insp-current w) (insp-fwd w))
      (let ((prev (pop (insp-back w)))) (%insp-show w (car prev) (cdr prev))))))

(defkeymap *inspector-keys* (*outline-keys*)
  (:enter insp-drill)                    ; Enter drills in; →/← still expand/collapse
  (:back  insp-back))

(defun make-inspector (obj label)
  "Build an inspector window rooted on OBJ.  Return (values WINDOW FOCUS)."
  (let* ((win  (make-instance 'inspector-window :keymap *global-keys*))
         (body (ui (stack
                     (:fill (outline :name 'tree :keymap *inspector-keys*
                              :roots (list (funcall *object->outline-fn* obj label))))
                     (1 (static-text :role :status
                          :text " Enter: drill in · Bksp: back · →/←: expand/collapse · Esc: close "))))))
    (add-subview win body)
    (setf (insp-current win) (cons obj label))
    (%insp-retitle win)
    (setf (window-scroll-target win) (find-view win 'tree) (window-help win) :browser)
    (values win (find-view win 'tree))))

(defun do-inspect ()
  "Prompt for a form, evaluate it in the active package, and inspect the result."
  (let ((str (prompt-string " Inspect " "Form:")))
    (when (and str (plusp (length (string-trim " " str))))
      (let* ((form (%read-in-active str))
             (val (handler-case (let ((*package* (%active-package))) (eval form))
                    (error (e) e))))
        (if *desktop*
            (dt-open *desktop* (lambda () (make-inspector val (string-trim " " str))))
            (multiple-value-bind (w f) (make-inspector val (string-trim " " str)) (run-view w :focus f)))))))

;;; --- object clipboard (the `*' register) ------------------------------------

(defvar *clip-object* nil)
(defvar *clip-present* nil)

(defun do-clip-last-value ()
  "Stash the REPL's most recent result into the object clipboard."
  (let ((r (and *desktop* (find :repl (dt-windows *desktop*) :key #'window-kind))))
    (if (and r (repl-last-value-p r))
        (progn (setf *clip-object* (repl-last-value r) *clip-present* t)
               (%tool-note (format nil "clipped *: ~a" (%insp-repr (repl-last-value r)))))
        (%tool-note "no REPL value to clip yet — evaluate something first"))))

(defun do-inspect-clipped ()
  (if *clip-present*
      (if *desktop*
          (dt-open *desktop* (lambda () (make-inspector *clip-object* "*")))
          (multiple-value-bind (w f) (make-inspector *clip-object* "*") (run-view w :focus f)))
      (%open-output " Object * " "Nothing clipped yet (Inspect → Clip last value).")))

(defun do-insert-clipped ()
  "Insert the clipped object's printed form into the focused editor."
  (let ((te (%focused-editor)))
    (cond ((not *clip-present*) (%tool-note "nothing clipped yet"))
          ((null te) (%tool-note "focus an editor to insert the clipped object"))
          (t (te-insert te (prin1-to-string *clip-object*)) (te-ensure-visible te) (invalidate te)))))

;;; --- register with the desktop: builders + an Inspect menu ------------------

(setf *window-builders*
      (append *window-builders*
              (list (cons :classes   #'make-class-browser)
                    (cons :functions #'make-function-browser))))

;;; The introspection commands are surfaced in the consolidated Browse menu
;;; (built in docs.lisp, which loads after this file and nav.lisp).
