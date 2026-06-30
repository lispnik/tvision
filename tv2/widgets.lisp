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
  (let ((ol (find-if (lambda (sv) (typep sv 'outline)) (subviews (view-owner v)))))
    (when ol
      (dolist (root (outline-roots ol))                ; collapse everything below each root
        (labels ((collapse (n)
                   (mapc #'collapse (tvision:outline-node-children n))
                   (setf (tvision:outline-node-expanded n) nil)))
          (mapc #'collapse (tvision:outline-node-children root))))
      (setf (outline-focused ol) 0)
      (invalidate ol))))

;;; --- demo -------------------------------------------------------------------

(defun run ()
  "Phase-3 demo: a window hosting the outline + buttons; Tab cycles focus,
Enter/Space fire a button's command, arrows drive the focused outline, q quits."
  (tvision:with-screen (s)
    (let* ((w (tvision:screen-width s)) (h (tvision:screen-height s))
           (win (make-instance 'window :keymap *global-keys*
                  :title " tv2 — windows · focus (Tab) · commands (no integer cmds, no dispatch cond) "
                  :bounds (tvision::make-trect 0 0 w h)))
           (ol  (make-instance 'outline :roots (demo-roots) :keymap *outline-keys*
                  :bounds (tvision::make-trect 1 1 (1- w) (- h 3))))
           (b1  (make-instance 'button :label "Collapse all" :command 'collapse-all
                  :bounds (tvision::make-trect 2 (- h 3) 18 (- h 2))))
           (b2  (make-instance 'button :label "Quit" :command 'quit
                  :bounds (tvision::make-trect 20 (- h 3) 30 (- h 2))))
           (st  (make-instance 'static-text :role :status
                  :text " Tab/Shift-Tab: move focus  ·  arrows/Enter: outline  ·  Enter/Space: button  ·  q: quit "
                  :bounds (tvision::make-trect 1 (- h 2) (1- w) (1- h)))))
      (dolist (v (list ol b1 b2 st)) (add-subview win v))
      (setf *running* t *dirty* t)
      (loop while *running* do
        (when *dirty* (draw win) (tvision:flush-screen s) (setf *dirty* nil))
        (tvision::pump-input s 0.05)
        (let ((tev (tvision::screen-next-event s)))
          (when tev
            (let ((e (translate tev)))
              (when e (handle-event win e)))))))))
