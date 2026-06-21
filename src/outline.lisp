;;;; outline.lisp --- TOutline: a collapsible tree view.

(in-package #:tvision)

(defconstant +cm-outline-item-selected+ 63)

(defstruct (outline-node (:constructor make-outline-node (text &optional children data setter)))
  (text "" )
  (children '())
  (expanded nil)
  (data nil)
  (setter nil))   ; optional (lambda (new-value)) writing DATA back to its place

(defun outline-node (text &rest children)
  "Convenience: a node with TEXT and the given child nodes (expanded)."
  (let ((n (make-outline-node text children)))
    (setf (outline-node-expanded n) t)
    n))

(defclass toutline (tscroller)
  ((roots   :initarg :roots :initform '() :accessor outline-roots)  ; list of top nodes
   (focused :initform 0 :accessor outline-focused)
   (command :initarg :command :initform 0 :accessor outline-command)))

(defmethod get-palette ((ol toutline)) (make-palette 13 14))  ; normal / focused

(defun outline-visible (ol)
  "Return a list of (node . depth) for every currently-visible node."
  (let ((out '()))
    (labels ((walk (nodes depth)
               (dolist (n nodes)
                 (push (cons n depth) out)
                 (when (and (outline-node-children n) (outline-node-expanded n))
                   (walk (outline-node-children n) (1+ depth))))))
      (walk (outline-roots ol) 0))
    (nreverse out)))

(defun outline-update-limit (ol)
  (let* ((vis (outline-visible ol))
         (maxw (reduce #'max vis :initial-value 4
                       :key (lambda (nd) (+ (* 2 (cdr nd)) 4
                                            (length (outline-node-text (car nd))))))))
    (set-scroller-limit ol maxw (length vis))))

(defmethod initialize-instance :after ((ol toutline) &key)
  (outline-update-limit ol))

(defun outline-current (ol)
  "The currently-focused node, or NIL."
  (let ((vis (outline-visible ol)))
    (when (and vis (< (outline-focused ol) (length vis)))
      (car (nth (outline-focused ol) vis)))))

(defun outline-focus (ol i)
  (let ((n (length (outline-visible ol))))
    (when (plusp n)
      (setf (outline-focused ol) (min (max 0 i) (1- n)))
      (let ((h (point-y (view-size ol))) (top (point-y (scroller-delta ol))))
        (cond ((< (outline-focused ol) top)
               (scroll-to ol (point-x (scroller-delta ol)) (outline-focused ol)))
              ((>= (outline-focused ol) (+ top h))
               (scroll-to ol (point-x (scroller-delta ol)) (1+ (- (outline-focused ol) h))))))
      (draw-view ol))))

(defun outline-toggle (ol)
  (let ((n (outline-current ol)))
    (when (and n (outline-node-children n))
      (setf (outline-node-expanded n) (not (outline-node-expanded n)))
      (outline-update-limit ol)
      (draw-view ol)
      t)))

(defun outline-select (ol)
  (let ((n (outline-current ol)))
    (when n
      (when (plusp (outline-command ol))
        (put-event ol (make-event :type +ev-command+ :command (outline-command ol) :info n)))
      (message (view-owner ol) +ev-broadcast+ +cm-outline-item-selected+ ol))))

(defmethod draw ((ol toutline))
  (let* ((w (point-x (view-size ol))) (h (point-y (view-size ol)))
         (normal (get-color ol 1)) (focused (get-color ol 2))
         (active (logtest (view-state ol) +sf-focused+))
         (dx (point-x (scroller-delta ol))) (dy (point-y (scroller-delta ol)))
         (vis (outline-visible ol))
         (db (make-draw-buffer w)))
    (dotimes (row h)
      (let* ((i (+ dy row))
             (sel (and (= i (outline-focused ol)) active))
             (attr (if sel focused normal)))
        (db-fill db #\Space attr)
        (when (< i (length vis))
          (destructuring-bind (node . depth) (nth i vis)
            (let* ((marker (cond ((null (outline-node-children node)) "  ")
                                 ((outline-node-expanded node) "- ")
                                 (t "+ ")))
                   (text (concatenate 'string
                                      (make-string (* 2 depth) :initial-element #\Space)
                                      marker (outline-node-text node)))
                   (vist (subseq text (min dx (length text))
                                 (min (length text) (+ dx w)))))
              (db-move-str db 0 vist attr))))
        (write-line* ol 0 row w 1 db)))))

(defmethod handle-event ((ol toutline) event)
  (cond
    ((scrollbar-event-p ol event) (scroll-from-scrollbars ol))
    ((and (= (event-type event) +ev-mouse-wheel+) (mouse-in-view-p ol event))
     (outline-focus ol (+ (outline-focused ol) (* 3 (event-wheel event))))
     (clear-event event))
    ((and (= (event-type event) +ev-mouse-down+) (mouse-in-view-p ol event))
     (let ((row (+ (point-y (scroller-delta ol))
                   (point-y (make-local ol (event-mouse-where event))))))
       (when (< row (length (outline-visible ol)))
         (outline-focus ol row)
         (if (event-double event) (outline-select ol) (outline-toggle ol))))
     (clear-event event))
    ((and (= (event-type event) +ev-key-down+) (logtest (view-state ol) +sf-focused+))
     (let ((k (event-key-code event)) (ch (event-char-code event)) (handled t)
           (n (outline-current ol)))
       (cond
         ((= k +kb-up+)    (outline-focus ol (1- (outline-focused ol))))
         ((= k +kb-down+)  (outline-focus ol (1+ (outline-focused ol))))
         ((= k +kb-home+)  (outline-focus ol 0))
         ((= k +kb-end+)   (outline-focus ol (1- (length (outline-visible ol)))))
         ((= k +kb-right+)
          (cond ((and n (outline-node-children n) (not (outline-node-expanded n)))
                 (outline-toggle ol))
                ((and n (outline-node-children n))
                 (outline-focus ol (1+ (outline-focused ol))))))
         ((= k +kb-left+)
          (cond ((and n (outline-node-children n) (outline-node-expanded n))
                 (outline-toggle ol))
                (t (outline-focus ol (1- (outline-focused ol))))))
         ((or (= k +kb-enter+) (= ch +kb-space+))
          (if (and n (outline-node-children n)) (outline-toggle ol) (outline-select ol)))
         (t (setf handled nil)))
       (when handled (clear-event event))))))

(defmethod data-size ((ol toutline)) 1)
(defmethod get-data ((ol toutline)) (outline-focused ol))
(defmethod set-data ((ol toutline) data) (outline-focus ol (or data 0)))
