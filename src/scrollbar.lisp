;;;; scrollbar.lisp --- TScrollBar, a basic proportional scroll bar.

(in-package #:tvision)

(defconstant +cm-scrollbar-changed+ 60)

(defclass tscrollbar (tview)
  ((value    :initform 0 :accessor sb-value)
   (minval   :initform 0 :accessor sb-min)
   (maxval   :initform 100 :accessor sb-max)
   (pgstep   :initform 1 :accessor sb-pgstep)
   (arstep   :initform 1 :accessor sb-arstep)
   (vertical :initarg :vertical :initform t :accessor sb-vertical)))

(defmethod initialize-instance :after ((sb tscrollbar) &key)
  (setf (view-options sb) (logior (view-options sb) +of-selectable+)
        (view-grow-mode sb)
        (if (sb-vertical sb)
            (logior +gf-grow-lox+ +gf-grow-hix+ +gf-grow-hiy+)
            (logior +gf-grow-loy+ +gf-grow-hix+ +gf-grow-hiy+))))

(defmethod get-palette ((sb tscrollbar)) (make-palette 1 2 3))

(defun sb-set-params (sb value minval maxval pgstep arstep)
  (setf (sb-min sb) minval
        (sb-max sb) (max minval maxval)
        (sb-pgstep sb) pgstep
        (sb-arstep sb) arstep)
  (sb-set-value sb value))

(defun sb-set-value (sb value)
  (let ((v (min (max value (sb-min sb)) (sb-max sb))))
    (unless (= v (sb-value sb))
      (setf (sb-value sb) v)
      (draw-view sb)
      (message (view-owner sb) +ev-broadcast+ +cm-scrollbar-changed+ sb))))

(defmethod draw ((sb tscrollbar))
  (let* ((vert (sb-vertical sb))
         (len (if vert (point-y (view-size sb)) (point-x (view-size sb))))
         (c (get-color sb 1))
         (cthumb (get-color sb 3))
         (db (make-draw-buffer (max 1 (if vert 1 len)))))
    (when (< len 2) (return-from draw))
    (let* ((range (max 1 (- (sb-max sb) (sb-min sb))))
           (inner (max 1 (- len 2)))
           (pos (+ 1 (floor (* (- (sb-value sb) (sb-min sb)) (1- inner)) range))))
      (if vert
          (progn
            (%set-cell db 0 #x25B2 c) (write-line* sb 0 0 1 1 db)              ; up triangle
            (db-fill db (code-char #x2592) c)
            (loop for y from 1 below (1- len) do (write-line* sb 0 y 1 1 db))
            (%set-cell db 0 #x2588 cthumb) (write-line* sb 0 pos 1 1 db)       ; thumb
            (%set-cell db 0 #x25BC c) (write-line* sb 0 (1- len) 1 1 db))      ; down triangle
          (progn
            (db-fill db (code-char #x2592) c)
            (%set-cell db 0 #x25C4 c)
            (%set-cell db (1- len) #x25BA c)
            (%set-cell db pos #x2588 cthumb)
            (write-line* sb 0 0 len 1 db))))))

(defmethod handle-event ((sb tscrollbar) event)
  (when (and (= (event-type event) +ev-mouse-down+) (mouse-in-view-p sb event))
    (let* ((lp (make-local sb (event-mouse-where event)))
           (pos (if (sb-vertical sb) (point-y lp) (point-x lp)))
           (len (if (sb-vertical sb) (point-y (view-size sb)) (point-x (view-size sb))))
           (inner (max 1 (- len 2)))
           (range (max 1 (- (sb-max sb) (sb-min sb))))
           (thumb (+ 1 (floor (* (- (sb-value sb) (sb-min sb)) (1- inner)) range))))
      (cond
        ((<= pos 0)        (sb-set-value sb (- (sb-value sb) (sb-arstep sb))))
        ((>= pos (1- len)) (sb-set-value sb (+ (sb-value sb) (sb-arstep sb))))
        ((< pos thumb)     (sb-set-value sb (- (sb-value sb) (sb-pgstep sb))))
        ((> pos thumb)     (sb-set-value sb (+ (sb-value sb) (sb-pgstep sb)))))
      (clear-event event)))
  (when (and (= (event-type event) +ev-key-down+))
    (let ((k (event-key-code event)))
      (cond
        ((or (= k +kb-up+) (= k +kb-left+))
         (sb-set-value sb (- (sb-value sb) (sb-arstep sb))) (clear-event event))
        ((or (= k +kb-down+) (= k +kb-right+))
         (sb-set-value sb (+ (sb-value sb) (sb-arstep sb))) (clear-event event))
        ((= k +kb-pgup+)
         (sb-set-value sb (- (sb-value sb) (sb-pgstep sb))) (clear-event event))
        ((= k +kb-pgdn+)
         (sb-set-value sb (+ (sb-value sb) (sb-pgstep sb))) (clear-event event))))))
