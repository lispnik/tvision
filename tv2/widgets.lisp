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

;;; --- input-line: an editable single-line text field -------------------------
;;; Text/caret/scroll are reactive (edits repaint), and an ON-CHANGE closure
;;; (a first-class handler) fires whenever the text changes -- data binding
;;; without GetData/SetData.

(defclass input-line (view)
  ((text      :initarg :text :initform "" :accessor input-text)
   (caret     :initform 0 :accessor input-caret)
   (scroll    :initform 0 :accessor input-scroll)          ; first visible column
   (on-change :initarg :on-change :initform nil :accessor input-on-change))
  (:metaclass reactive-class))

(defmethod focusable-p ((il input-line)) t)

(defun input-scroll-fix (il)
  (let ((b (view-bounds il)))
    (when b
      (let ((w (tvision::rect-width b)) (c (input-caret il)) (sc (input-scroll il)))
        (cond ((< c sc) (setf (input-scroll il) c))
              ((>= c (+ sc w)) (setf (input-scroll il) (1+ (- c w)))))))))

(defun input-notify (il)
  (when (input-on-change il) (funcall (input-on-change il) il)))

(defun input-insert (il ch)
  (let ((txt (input-text il)) (c (input-caret il)))
    (setf (input-text il)  (concatenate 'string (subseq txt 0 c) (string ch) (subseq txt c))
          (input-caret il) (1+ c))
    (input-scroll-fix il) (input-notify il)))

(defun input-backspace (il)
  (let ((txt (input-text il)) (c (input-caret il)))
    (when (plusp c)
      (setf (input-text il)  (concatenate 'string (subseq txt 0 (1- c)) (subseq txt c))
            (input-caret il) (1- c))
      (input-scroll-fix il) (input-notify il))))

(defun input-delete (il)
  (let ((txt (input-text il)) (c (input-caret il)))
    (when (< c (length txt))
      (setf (input-text il) (concatenate 'string (subseq txt 0 c) (subseq txt (1+ c))))
      (input-notify il))))

(defun input-move (il delta)
  (setf (input-caret il) (min (length (input-text il)) (max 0 (+ (input-caret il) delta))))
  (input-scroll-fix il))

(defmethod draw ((il input-line))
  (let* ((b (view-bounds il)) (w (tvision::rect-width b))
         (focused (view-focused-p il))
         (attr (if focused (role :input-focused) (role :input)))
         (txt (input-text il)) (sc (input-scroll il))
         (vis (subseq txt (min sc (length txt)) (min (length txt) (+ sc w)))))
    (fill-row il 0 0 w attr)
    (draw-text il 0 0 vis attr)
    (when (and focused tvision:*screen*)              ; own the hardware cursor while focused
      (tvision:set-cursor-pos tvision:*screen*
                              (+ (tvision::rect-ax b) (- (input-caret il) sc))
                              (tvision::rect-ay b))
      (tvision:show-cursor tvision:*screen*))))

(defmethod handle-event ((il input-line) (e key-event))
  (let ((ks (event-keysym e)))
    (cond
      ((and (characterp ks) (graphic-char-p ks) (zerop (event-modifiers e)))
       (input-insert il ks) (setf (handled-p e) t))
      ((eql ks :back)  (input-backspace il) (setf (handled-p e) t))
      ((eql ks :del)   (input-delete il)    (setf (handled-p e) t))
      ((eql ks :left)  (input-move il -1)   (setf (handled-p e) t))
      ((eql ks :right) (input-move il 1)    (setf (handled-p e) t))
      ((eql ks :home)  (setf (input-caret il) 0) (input-scroll-fix il) (setf (handled-p e) t))
      ((eql ks :end)   (setf (input-caret il) (length (input-text il))) (input-scroll-fix il)
                       (setf (handled-p e) t))
      (t (call-next-method)))))               ; Enter/Tab/Esc bubble (submit, focus, quit)

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
