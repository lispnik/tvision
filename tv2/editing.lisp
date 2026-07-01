;;;; editing.lisp --- extra editor commands: comment region, snippets,
;;;; pretty-print, and the auto-close toggle.  All operate on the focused editor.

(in-package #:tv2)

;;; --- comment / uncomment a region -------------------------------------------

(defun %uncomment-line (line)
  "Strip a leading `;'..`; ' comment marker (after indentation) from LINE."
  (let ((k 0))
    (loop while (and (< k (length line)) (member (char line k) '(#\Space #\Tab))) do (incf k))
    (let ((j k))
      (loop while (and (< j (length line)) (char= (char line j) #\;)) do (incf j))
      (when (and (< j (length line)) (char= (char line j) #\Space)) (incf j))
      (if (> j k) (concatenate 'string (subseq line 0 k) (subseq line j)) line))))

(defun %comment-region (te)
  "Toggle `;; ' line comments over the selected lines (or the current line)."
  (when te
    (multiple-value-bind (a b) (te-sel-ordered te)
      (let* ((l0 (if a (car a) (te-cy te)))
             (l1 (if b (if (and (zerop (cdr b)) (> (car b) l0)) (1- (car b)) (car b)) l0))
             (l1 (max l0 (min l1 (1- (te-nlines te)))))
             (all-commented t))
        (loop for li from l0 to l1
              for tr = (string-left-trim '(#\Space #\Tab) (te-line te li))
              when (and (plusp (length tr)) (char/= (char tr 0) #\;)) do (setf all-commented nil))
        (te-save-undo te)
        (loop for li from l0 to l1 for line = (te-line te li)
              do (setf (te-line te li)
                       (if all-commented (%uncomment-line line)
                           (concatenate 'string ";; " line))))
        (te-ensure-visible te) (invalidate te)))))

;;; --- snippets ---------------------------------------------------------------

(defvar *snippets*
  '(("defun"        . "(defun name (args)~%  )")
    ("defmacro"     . "(defmacro name (args)~%  )")
    ("defmethod"    . "(defmethod name ((arg type))~%  )")
    ("defgeneric"   . "(defgeneric name (args))")
    ("defclass"     . "(defclass name ()~%  ((slot :initarg :slot :accessor name-slot)))")
    ("defvar"       . "(defvar *name* value)")
    ("let"          . "(let ((var value))~%  )")
    ("loop collect" . "(loop for x in list~%      collect x)")
    ("handler-case" . "(handler-case~%    (progn )~%  (error (e) ))")
    ("dotimes"      . "(dotimes (i n)~%  )"))
  "Code templates for Insert snippet.")

(defun %insert-snippet (te)
  (when te
    (let ((name (popup-choose (mapcar #'car *snippets*) :title " Insert snippet ")))
      (when name
        (let ((tmpl (cdr (assoc name *snippets* :test #'string=))))
          (when tmpl (te-insert te (format nil tmpl)) (te-ensure-visible te) (invalidate te)))))))

;;; --- pretty-print the selection ---------------------------------------------

(defun %pretty-print-selection (te)
  "Replace the selected form(s) with their pretty-printed form."
  (when te
    (let ((sel (te-selected-string te)))
      (if (or (null sel) (zerop (length (string-trim '(#\Space #\Tab #\Newline) sel))))
          (%open-output " Pretty-print " "Select a form to pretty-print first.")
          (let ((forms (ignore-errors
                         (let ((*package* (%active-package)))
                           (with-input-from-string (in sel)
                             (loop for f = (read in nil in) until (eq f in) collect f))))))
            (when forms
              (let ((pp (with-output-to-string (o)
                          (let ((*print-pretty* t) (*print-right-margin* 78) (*package* (%active-package)))
                            (loop for (f . more) on forms
                                  do (write f :stream o :pretty t :case :downcase)
                                     (when more (terpri o) (terpri o)))))))
                (te-insert te pp) (te-ensure-visible te) (invalidate te))))))))

;;; --- go to line -------------------------------------------------------------

(defun %editor-goto-line (te)
  "Prompt for a line number and move the cursor there (selecting the line)."
  (when te
    (let ((s (prompt-string " Go to line " (format nil "Line (1-~d):" (te-nlines te)))))
      (when (and s (plusp (length (string-trim " " s))))
        (let ((n (ignore-errors (parse-integer (string-trim " " s)))))
          (when n
            (let ((li (max 0 (min (1- (te-nlines te)) (1- n)))))
              (setf (te-cy te) li (te-cx te) 0
                    (te-anchor te) (cons li 0))              ; select the line, for a visible flash
              (te-clamp te) (te-ensure-visible te) (invalidate te))))))))

;;; --- reorder a function's required arguments at its call sites --------------
;;; Heavy sexp rewriting (finding operator-position calls and permuting their
;;; args) is tvlisp's; tv2 does the arglist introspection + buffer orchestration
;;; and calls the hook per open editor buffer.
;;; (funcall *REORDER-FN* NAME-STRING TEXT PERM R) -> new TEXT, or NIL if unchanged.

(defvar *reorder-fn* nil)

(defun %required-names (sym)
  "Names of SYM's leading required parameters (stops at the first &-marker)."
  (let ((ll (ignore-errors (sb-introspect:function-lambda-list sym))) (out '()))
    (dolist (p ll (nreverse out))
      (when (and (symbolp p) (plusp (length (symbol-name p))) (char= (char (symbol-name p) 0) #\&))
        (return (nreverse out)))
      (push p out))))

(defun %parse-perm (input names)
  "Parse INPUT (space/comma separated 1-based indices or param names) into a
0-based permutation of 0..N-1, or NIL when it is not a full permutation."
  (let* ((n (length names))
         (toks (remove "" (loop with acc and start = 0
                                for i to (length input)
                                when (or (= i (length input)) (member (char input i) '(#\Space #\Tab #\,)))
                                  do (push (subseq input start i) acc) (setf start (1+ i))
                                finally (return (nreverse acc)))
                       :test #'string=))
         (perm (cond
                 ((/= (length toks) n) nil)
                 ((every (lambda (tk) (every #'digit-char-p tk)) toks)
                  (mapcar (lambda (tk) (1- (parse-integer tk))) toks))
                 (t (mapcar (lambda (tk) (position (string-upcase tk) names
                                                   :key (lambda (s) (string-upcase (string s))) :test #'string=))
                            toks)))))
    (when (and perm (notany #'null perm)
               (equal (sort (copy-list perm) #'<) (loop for i below n collect i)))
      perm)))

(defun do-reorder-args ()
  "Reorder a function's required arguments at its direct call sites across all
open editor buffers (calls via apply/funcall/#' and the definition are untouched)."
  (unless *reorder-fn* (return-from do-reorder-args (%open-output " Reorder args " "No reorder backend is installed.")))
  (let ((s (prompt-string " Reorder args " "Function:")))
    (when (and s (plusp (length (string-trim " " s))))
      (setf s (string-trim " " s))
      (let* ((sym (%read-in-active s))
             (names (and sym (symbolp sym) (fboundp sym) (%required-names sym)))
             (r (length names)))
        (cond
          ((not (and sym (symbolp sym) (fboundp sym))) (%open-output " Reorder args " (format nil "~a is not a function." s)))
          ((< r 2) (%open-output " Reorder args " (format nil "~a has fewer than 2 required arguments." s)))
          (t (let* ((cur (format nil "~{~(~a~)~^ ~}" names))
                    (input (prompt-string " Reorder args " (format nil "Params (~a) — new order:" cur)))
                    (perm (and input (%parse-perm (string-trim " " input) names))))
               (cond
                 ((null input) nil)
                 ((null perm) (%open-output " Reorder args " (format nil "Not a permutation of the ~d params (~a)." r cur)))
                 ((equal perm (loop for i below r collect i)) (%open-output " Reorder args " "Order unchanged."))
                 (t (let ((name (string-downcase s)) (total 0))
                      (dolist (w (and *desktop* (dt-windows *desktop*)))
                        (when (typep w 'editor-window)
                          (let ((te (find-view w 'edit)))
                            (when te
                              (let* ((old (te-text te))
                                     (new (ignore-errors (funcall *reorder-fn* name old perm r))))
                                (when (and new (stringp new) (string/= new old))
                                  (let ((off (te-offset te (te-cy te) (te-cx te))))
                                    (te-save-undo te) (te-set-text te new)
                                    (multiple-value-bind (l c) (te-pos-at-offset te (min off (length new)))
                                      (setf (te-cy te) l (te-cx te) c))
                                    (te-clamp te) (te-ensure-visible te) (invalidate te))
                                  (incf total)))))))
                      (%open-output " Reorder args "
                                    (if (plusp total)
                                        (format nil "Reordered ~a to (~{~(~a~)~^ ~}) in ~d buffer~:p."
                                                name (mapcar (lambda (k) (nth k names)) perm) total)
                                        (format nil "No direct calls to ~a found in open buffers." name)))))))))))))

;;; --- an Edit menu -----------------------------------------------------------

(push (lambda (dt)
        (declare (ignore dt))
        (flet ((cur () (%focused-editor))
               (pe (op) (lambda () (%editor-paredit (%focused-editor) op))))
          (list "Edit"
                (list "Comment region"      (lambda () (%comment-region (cur))))
                (list "Pretty-print region" (lambda () (%pretty-print-selection (cur))))
                (list "Insert snippet…"     (lambda () (%insert-snippet (cur))))
                :--
                (list "Incremental search"  (lambda () (let ((te (cur))) (when te (te-isearch-start te)))))
                (list "Go to line…"         (lambda () (%editor-goto-line (cur))))
                (list "Reorder args…"       (lambda () (do-reorder-args)))
                :--
                (list "Structure" :submenu                 ; paredit (from paredit.lisp)
                      (list "Slurp forward →"   (pe :slurp))
                      (list "Barf forward ←"    (pe :barf))
                      (list "Slurp backward ←"  (pe :slurp-back))
                      (list "Barf backward →"   (pe :barf-back))
                      (list "Splice"            (pe :splice))
                      (list "Wrap in ( )"       (pe :wrap))
                      (list "Raise"             (pe :raise))
                      (list "Transpose"         (pe :transpose))
                      (list "Kill sexp"         (pe :kill)))
                :--
                (list "Auto-close parens"   (lambda () (let ((te (cur)))
                                                         (when te (setf (te-auto-close te) (not (te-auto-close te)))
                                                               (invalidate te)))))
                (list "Line numbers"        (lambda () (let ((te (cur)))
                                                         (when te (setf (te-line-numbers te) (not (te-line-numbers te)))
                                                               (te-ensure-visible te) (invalidate te))))))))
      *extra-menus*)
