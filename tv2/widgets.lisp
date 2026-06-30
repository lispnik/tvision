;;;; widgets.lisp --- window (framed container), button, static-text, and a demo
;;;; that hosts the outline + buttons with Tab focus cycling and command actions.

(in-package #:tv2)

;;; --- window: a framed container with a title --------------------------------

(defclass window (container)
  ((title :initarg :title :initform "" :accessor window-title))
  (:metaclass reactive-class))

(defmethod draw ((w window))
  (let* ((b (view-bounds w))
         (x0 (tvision::rect-ax b)) (y0 (tvision::rect-ay b))
         (x1 (1- (tvision::rect-bx b))) (y1 (1- (tvision::rect-by b)))
         (frame (role :frame)))
    (loop for y from y0 to y1 do                       ; clear interior
      (loop for x from x0 to x1 do (%put-cell x y #\Space (role :normal))))
    (%box x0 y0 x1 y1 frame)
    (%text-at (+ x0 (max 1 (floor (- (tvision::rect-width b) (length (window-title w))) 2)))
              y0 (window-title w) frame)
    (dolist (sv (subviews w)) (draw sv))))             ; children paint over the interior

;;; --- button: focusable, fires a command on Enter/Space ----------------------

(defclass button (view)
  ((label   :initarg :label   :accessor button-label)
   (command :initarg :command :accessor button-command))
  (:metaclass reactive-class))

(defmethod focusable-p ((b button)) t)

(defmethod draw ((b button))
  (let* ((bb (view-bounds b))
         (attr (if (view-focused-p b) (role :button-focused) (role :button))))
    (fill-row b 0 0 (tvision::rect-width bb) attr)
    (draw-text b 0 0 (format nil "[ ~a ]" (button-label b)) attr)))

(defmethod handle-event ((b button) (e key-event))
  (if (member (event-keysym e) (list :enter #\Space) :test #'equal)
      (progn (perform (button-command b) b e) (setf (handled-p e) t))
      (call-next-method)))

;;; --- static-text: a non-focusable label -------------------------------------

(defclass static-text (view)
  ((text :initarg :text :initform "" :accessor static-text-text)
   (role :initarg :role :initform :normal :reader static-text-role))
  (:metaclass reactive-class))

(defmethod draw ((v static-text))
  (let ((attr (role (static-text-role v))))
    (fill-row v 0 0 (tvision::rect-width (view-bounds v)) attr)
    (draw-text v 0 0 (static-text-text v) attr)))

;;; --- a command that reaches across the window to the outline ----------------

(define-command collapse-all (v e)
  (let ((ol (find-view (view-root v) 'tree)))     ; locate the named outline anywhere in the tree
    (when (typep ol 'outline)
      (dolist (root (outline-roots ol))           ; collapse everything below each root
        (labels ((collapse (n)
                   (mapc #'collapse (tvision:outline-node-children n))
                   (setf (tvision:outline-node-expanded n) nil)))
          (mapc #'collapse (tvision:outline-node-children root))))
      (setf (outline-focused ol) 0)
      (invalidate ol))))
