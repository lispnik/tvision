;;;; desktop.lisp --- TDesktop and its TBackground fill.

(in-package #:tvision)

(defclass tbackground (tview)
  ((pattern :initarg :pattern :initform #x2592 :accessor background-pattern)))

(defmethod initialize-instance :after ((b tbackground) &key)
  (setf (view-grow-mode b) (logior +gf-grow-hix+ +gf-grow-hiy+)))

(defmethod get-palette ((b tbackground)) (make-palette 38))  ; desktop attribute

(defmethod draw ((b tbackground))
  (let* ((w (point-x (view-size b)))
         (h (point-y (view-size b)))
         (attr (get-color b 1))               ; desktop attribute (palette-driven)
         (db (make-draw-buffer w)))
    (db-fill db (code-char (background-pattern b)) attr)
    (write-line* b 0 0 w h db)))

(defclass tdesktop (tgroup)
  ((background :initform nil :accessor desktop-background)))

(defmethod initialize-instance :after ((d tdesktop) &key)
  (setf (view-grow-mode d) (logior +gf-grow-hix+ +gf-grow-hiy+))
  (let ((bg (make-instance 'tbackground)))
    (set-bounds bg (get-extent d))
    (setf (desktop-background d) bg)
    (insert d bg)))

(defun desktop-windows (d)
  "All managed windows (everything except the background), front-to-back."
  (remove (desktop-background d) (group-subviews d)))

(defun select-window-by-number (d n)
  "Focus and raise the desktop window numbered N (used by Alt+1..9)."
  (let ((win (find n (desktop-windows d)
                   :key (lambda (w) (and (typep w 'twindow) (window-number w))))))
    (when win (set-current d win :normal-select))))

(defmethod handle-event ((d tdesktop) event)
  (call-next-method)
  (when (= (event-type event) +ev-command+)
    (cond
      ((= (event-command event) +cm-next+) (select-next d t)   (clear-event event))
      ((= (event-command event) +cm-prev+) (select-next d nil) (clear-event event)))))

(defun cascade (d)
  "Cascade the desktop's windows from the top-left corner."
  (let* ((ext (get-extent d))
         (wins (reverse (desktop-windows d)))
         (i 0))
    (dolist (win wins)
      (let ((x (* i 2)) (y (* i 1)))
        (when (< (+ x 20) (rect-bx ext))
          (locate win (make-trect x y (- (rect-bx ext) 4) (- (rect-by ext) 2)))))
      (incf i))
    (redraw d)))

(defun tile (d)
  "Tile the desktop's windows into a roughly square grid."
  (let* ((ext (get-extent d))
         (wins (reverse (desktop-windows d)))
         (n (length wins)))
    (when (plusp n)
      (let* ((cols (ceiling (sqrt n)))
             (rows (ceiling n cols))
             (cw (floor (rect-width ext) cols))
             (ch (floor (rect-height ext) rows)))
        (loop for win in wins for k from 0
              for cx = (mod k cols) for cy = (floor k cols)
              do (locate win (make-trect (* cx cw) (* cy ch)
                                         (if (= cx (1- cols)) (rect-bx ext) (* (1+ cx) cw))
                                         (if (= cy (1- rows)) (rect-by ext) (* (1+ cy) ch))))))
      (redraw d))))
