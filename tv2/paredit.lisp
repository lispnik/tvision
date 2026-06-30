;;;; paredit.lisp --- structural (paredit) editing for the tv2 editor.
;;;;
;;;; The transforms themselves are pure string surgery over a sexp parser; an
;;;; embedding app supplies them through *PAREDIT-FN* (op text offset) ->
;;;; (values new-text new-offset).  tvlisp-tv2 wires this to tvlisp's real sexp
;;;; layer (%SEXP-BOUNDS / %SEXP-SPAN-AT / %SEXP-SPANS / %INNER-LIST).  Exposed
;;;; here as a "Lisp" menu that operates on the focused editor.

(in-package #:tv2)

;;; (funcall fn OP TEXT OFFSET) -> (values NEW-TEXT NEW-OFFSET), or NIL to no-op.
;;; OP is one of :slurp :barf :slurp-back :barf-back :splice :wrap :raise
;;; :transpose :kill.
(defvar *paredit-fn* nil)

(defun %editor-paredit (te op)
  "Apply structural edit OP to TE's buffer around the cursor (via *PAREDIT-FN*)."
  (when (and te *paredit-fn*)
    (let ((text (te-text te)) (off (te-offset te (te-cy te) (te-cx te))))
      (multiple-value-bind (new new-off) (ignore-errors (funcall *paredit-fn* op text off))
        (when (and new (stringp new))
          (te-save-undo te)
          (te-set-text te new)
          (multiple-value-bind (l c) (te-pos-at-offset te (or new-off off))
            (setf (te-cy te) l (te-cx te) c))
          (te-clamp te) (te-ensure-visible te) (invalidate te))))))

(defun %focused-editor ()
  "The text-edit of the focused desktop window, when it is an editor."
  (let ((w (and *desktop* (dt-top *desktop*))))
    (when (typep w 'editor-window) (find-view w 'edit))))

(push (lambda (dt)
        (declare (ignore dt))
        (flet ((pe (op) (lambda () (%editor-paredit (%focused-editor) op))))
          (list "Lisp"
                (list "Slurp forward →"   (pe :slurp))
                (list "Barf forward ←"    (pe :barf))
                (list "Slurp backward ←"  (pe :slurp-back))
                (list "Barf backward →"   (pe :barf-back))
                (list "Splice"            (pe :splice))
                (list "Wrap in ( )"       (pe :wrap))
                (list "Raise"             (pe :raise))
                (list "Transpose"         (pe :transpose))
                (list "Kill sexp"         (pe :kill)))))
      *extra-menus*)
