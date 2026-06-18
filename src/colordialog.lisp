;;;; colordialog.lisp --- TColorDialog: pick a foreground/background colour.

(in-package #:tvision)

(defparameter +color-names+
  #("Black" "Blue" "Green" "Cyan" "Red" "Magenta" "Brown" "Light gray"
    "Dark gray" "Light blue" "Light green" "Light cyan" "Light red"
    "Light magenta" "Yellow" "White"))

;;; A live preview swatch that reflects two colour clusters.
(defclass color-preview (tview)
  ((fg :initarg :fg :accessor cp-fg)
   (bg :initarg :bg :accessor cp-bg)))

(defmethod draw ((p color-preview))
  (let* ((w (point-x (view-size p))) (h (point-y (view-size p)))
         (attr (make-attr (cluster-value (cp-fg p)) (cluster-value (cp-bg p))))
         (db (make-draw-buffer w)))
    (db-fill db #\Space attr)
    (db-move-str db 1 "Sample text" attr)
    (dotimes (y h) (write-line* p 0 y w 1 db))))

(defun color-dialog (&key (title "Colors") (fg 7) (bg 0))
  "Open a modal colour picker.  Return (values ok-p fg bg)."
  (when *application*
    (let* ((w 54) (h 21)
           (d (make-instance 'tdialog :title title :bounds (make-trect 0 0 w h)))
           (fgc (make-instance 'tradio-buttons :value fg
                               :labels (coerce +color-names+ 'list)
                               :bounds (make-trect 3 3 22 19)))
           (bgc (make-instance 'tradio-buttons :value bg
                               :labels (coerce (subseq +color-names+ 0 8) 'list)
                               :bounds (make-trect 26 3 45 11)))
           (pv (make-instance 'color-preview :fg fgc :bg bgc
                              :bounds (make-trect 26 13 (- w 3) 16))))
      (flet ((lbl (text x y)
               (let ((l (make-instance 'tlabel :text text)))
                 (set-bounds l (make-trect x y (+ x (length text)) (1+ y)))
                 (insert d l))))
        (lbl "Foreground" 3 2) (insert d fgc)
        (lbl "Background" 26 2) (insert d bgc)
        (lbl "Preview" 26 12) (insert d pv))
      (insert d (make-button (make-trect (- w 26) (- h 3) (- w 16) (- h 1)) "~O~K" +cm-ok+ t))
      (insert d (make-button (make-trect (- w 13) (- h 3) (- w 3) (- h 1)) "Cancel" +cm-cancel+))
      (let* ((desk (program-desktop *application*)))
        (move-to d (max 0 (floor (- (point-x (view-size desk)) w) 2))
                 (max 0 (floor (- (point-y (view-size desk)) h) 2))))
      (focus fgc)
      (if (= (exec-view (program-desktop *application*) d) +cm-ok+)
          (values t (cluster-value fgc) (cluster-value bgc))
          (values nil fg bg)))))
