;;;; layout.lisp --- a box-model layout protocol (stack / row) and a declarative
;;;; construction DSL (the UI macro), replacing hand-computed bounds.

(in-package #:tv2)

;;; --- size-aware layout containers -------------------------------------------
;;; A child's spec is :FILL (split the remainder) or an integer (fixed rows in a
;;; STACK, fixed columns in a ROW).  Layout containers carry no focus or events
;;; of their own -- they only place their children.

(defclass stack (container) ((specs :initform '() :accessor layout-specs)) (:metaclass reactive-class))
(defclass row   (container) ((specs :initform '() :accessor layout-specs)) (:metaclass reactive-class))

(defun add-laid (c v spec)
  (add-subview c v)
  (setf (layout-specs c) (append (layout-specs c) (list spec)))
  v)

(defun %distribute (total specs)
  "Sizes for SPECS over TOTAL extent; :FILL entries share the remainder evenly."
  (let* ((fixed (loop for s in specs unless (eq s :fill) sum s))
         (fills (count :fill specs))
         (slack (max 0 (- total fixed)))
         (each  (if (plusp fills) (floor slack fills) 0))
         (extra (if (plusp fills) (- slack (* each fills)) 0)))
    (loop for s in specs
          collect (if (eq s :fill)
                      (prog1 (+ each (if (plusp extra) 1 0)) (when (plusp extra) (decf extra)))
                      s))))

(defmethod layout ((c stack) rect)
  (setf (view-bounds c) rect)
  (let ((x0 (r-x0 rect)) (x1 (r-x1 rect)) (y (r-y0 rect))
        (sizes (%distribute (r-h rect) (layout-specs c))))
    (loop for sv in (subviews c) for hh in sizes
          do (layout sv (rect x0 y x1 (+ y hh))) (incf y hh))))

(defmethod layout ((c row) rect)
  (setf (view-bounds c) rect)
  (let ((y0 (r-y0 rect)) (y1 (r-y1 rect)) (x (r-x0 rect))
        (sizes (%distribute (r-w rect) (layout-specs c))))
    (loop for sv in (subviews c) for ww in sizes
          do (layout sv (rect x y0 (+ x ww) y1)) (incf x ww))))

(defmethod layout ((w window) rect)
  (setf (view-bounds w) rect)
  (let ((interior (rect (1+ (r-x0 rect)) (1+ (r-y0 rect)) (1- (r-x1 rect)) (1- (r-y1 rect)))))
    (dolist (sv (subviews w)) (layout sv interior))))   ; the single child fills the interior

;;; --- the construction DSL (compile-time-checked) ----------------------------
;;;   (ui (window (:title ... :keymap ...)
;;;         (stack
;;;           (:fill (outline :name 'tree ...))
;;;           (1 (row (16 (button :label "X" :command 'cmd)) (:fill (static-text ...))))
;;;           (1 (static-text ...)))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun expand-ui (form)
    (unless (consp form) (error "tv2 ui: expected a form, got ~s" form))
    (case (car form)
      (window
       (destructuring-bind (opts child) (cdr form)
         `(let ((w (make-instance 'window ,@opts)))
            (add-subview w ,(expand-ui child))
            w)))
      ((stack row)
       `(let ((c (make-instance ',(car form))))
          ,@(loop for entry in (cdr form)
                  do (unless (and (consp entry) (= (length entry) 2))
                       (error "tv2 ui: ~(~a~) child must be (SIZE FORM), got ~s" (car form) entry))
                  collect `(add-laid c ,(expand-ui (second entry)) ,(first entry)))
          c))
      ((outline button static-text input-line)
       `(make-instance ',(car form) ,@(cdr form)))
      (t (error "tv2 ui: unknown widget/form ~s" (car form))))))

(defmacro ui (form)
  "Build a view tree declaratively; the structure is checked at macroexpansion."
  (expand-ui form))

;;; --- demo built entirely with the DSL ---------------------------------------

(defun run ()
  "Phase-4 demo: the same window/outline/buttons/status, built declaratively with
the UI macro + box layout (no hand-computed bounds).  Tab cycles focus, Enter/
Space fire a button's command, arrows drive the focused outline, q quits."
  (tvision:with-screen (s)
    (let ((win (ui (window (:title " tv2 — input-line · data entry · reactive on-change handler "
                            :keymap *global-keys*)
                     (stack
                       (1 (row
                            (9     (static-text :role :label :text " Filter: "))
                            (:fill (input-line :name 'find
                                     :on-change (lambda (il)
                                                  (let ((echo (find-view (view-root il) 'echo)))
                                                    (when echo
                                                      (setf (static-text-text echo)
                                                            (format nil " typed ~s  (live via on-change -> reactive repaint) "
                                                                    (input-text il))))))))))
                       (:fill (outline :name 'tree :roots (demo-roots) :keymap *outline-keys*))
                       (1 (row
                            (16    (button :label "Collapse all" :command 'collapse-all))
                            (8     (button :label "Quit"         :command 'quit))
                            (:fill (static-text :name 'echo :role :status :text " (type in the Filter field) "))))
                       (1 (static-text :role :status
                            :text " Tab: focus · type in Filter · arrows/Enter: outline · Esc or Quit: exit ")))))))
      (layout win (rect 0 0 (tvision:screen-width s) (tvision:screen-height s)))
      (setf (container-focus win) (first (all-focusables win))
            *running* t *dirty* t)
      (loop while *running* do
        (when *dirty* (draw win) (tvision:flush-screen s) (setf *dirty* nil))
        (tvision::pump-input s 0.05)
        (let ((tev (tvision::screen-next-event s)))
          (when tev
            (let ((e (translate tev)))
              (when e (handle-event win e)))))))))
