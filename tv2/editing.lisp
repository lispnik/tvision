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
          (let ((form (ignore-errors (let ((*package* (%active-package))) (read-from-string sel)))))
            (when form
              (let ((pp (with-output-to-string (o)
                          (let ((*print-pretty* t) (*print-right-margin* 78) (*package* (%active-package)))
                            (write form :stream o :pretty t :case :downcase)))))
                (te-insert te pp) (te-ensure-visible te) (invalidate te))))))))

;;; --- an Edit menu -----------------------------------------------------------

(push (lambda (dt)
        (declare (ignore dt))
        (flet ((cur () (%focused-editor)))
          (list "Code"
                (list "Comment region"      (lambda () (%comment-region (cur))))
                (list "Insert snippet…"     (lambda () (%insert-snippet (cur))))
                (list "Pretty-print region" (lambda () (%pretty-print-selection (cur))))
                (list "Auto-close parens"   (lambda () (let ((te (cur)))
                                                         (when te (setf (te-auto-close te) (not (te-auto-close te)))
                                                               (invalidate te))))))))
      *extra-menus*)
