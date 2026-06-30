;;;; cluster.lisp --- checkbox / radio-button groups.
;;;;
;;;; A CLUSTER is a focusable column of labeled items.  In :CHECK mode each item
;;;; toggles independently ([X]/[ ]) and VALUE is the list of checked indices; in
;;;; :RADIO mode exactly one is chosen ((•)/( )) and VALUE is its index.  Space
;;;; (or a click) toggles the item under the cursor.  Dialogs read CLUSTER-VALUE.

(in-package #:tv2)

(defclass cluster (view)
  ((items  :initarg :items :initform '() :accessor cluster-items)
   (mode   :initarg :mode  :initform :check :accessor cluster-mode)   ; :check | :radio
   (value  :initarg :value :initform nil :accessor cluster-value)     ; :check -> list of idx; :radio -> idx
   (cursor :initform 0 :accessor cluster-cursor))
  (:metaclass reactive-class))

(defmethod focusable-p ((c cluster)) t)

(defun cluster-on-p (c i)
  (if (eq (cluster-mode c) :radio) (eql i (cluster-value c)) (member i (cluster-value c))))

(defun cluster-toggle (c)
  (let ((i (cluster-cursor c)))
    (if (eq (cluster-mode c) :radio)
        (setf (cluster-value c) i)
        (setf (cluster-value c) (if (member i (cluster-value c))
                                    (remove i (cluster-value c))
                                    (sort (cons i (cluster-value c)) #'<))))))

(defmethod draw ((c cluster))
  (let ((w (r-w (view-bounds c))) (focused (view-focused-p c)))
    (dotimes (i (length (cluster-items c)))
      (let* ((on (cluster-on-p c i))
             (mark (if (eq (cluster-mode c) :radio) (if on "(•)" "( )") (if on "[X]" "[ ]")))
             (attr (if (and focused (= i (cluster-cursor c))) (role :focused) (role :normal))))
        (fill-row c 0 i w attr)
        (draw-text c 1 i (format nil "~a ~a" mark (nth i (cluster-items c))) attr)))))

(defmethod handle-event ((c cluster) (e key-event))
  (let ((ks (event-keysym e)) (n (length (cluster-items c))))
    (cond
      ((and (plusp n) (eql ks :up))    (setf (cluster-cursor c) (mod (1- (cluster-cursor c)) n)) (setf (handled-p e) t))
      ((and (plusp n) (eql ks :down))  (setf (cluster-cursor c) (mod (1+ (cluster-cursor c)) n)) (setf (handled-p e) t))
      ((member ks '(#\Space #\x #\X) :test #'eql) (cluster-toggle c) (setf (handled-p e) t))
      (t (call-next-method)))))

(defmethod handle-event ((c cluster) (e mouse-down))
  (let ((i (mouse-row c e)))
    (when (and (>= i 0) (< i (length (cluster-items c))))
      (setf (cluster-cursor c) i) (cluster-toggle c)))
  (setf (handled-p e) t))
