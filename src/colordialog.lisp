;;;; colordialog.lisp --- TColorDialog and its colour-picker controls.
;;;;
;;;; The granular Turbo Vision colour controls: TColorSelector (a grid of the 16
;;;; colours), TMonoSelector (normal/highlight/underline/inverse), and a
;;;; TColorDisplay sample.  TColorDialog wires a foreground and background
;;;; selector to a live sample.

(in-package #:tvision)

(defparameter +color-names+
  #("Black" "Blue" "Green" "Cyan" "Red" "Magenta" "Brown" "Light gray"
    "Dark gray" "Light blue" "Light green" "Light cyan" "Light red"
    "Light magenta" "Yellow" "White"))

;;; ---------------------------------------------------------------------------
;;; TColorSelector --- a grid of selectable colours
;;; ---------------------------------------------------------------------------

(defparameter +color-sel-cols+ 4)
(defparameter +color-sel-cellw+ 4)

(defclass tcolor-selector (tview)
  ((color :initarg :color :initform 0  :accessor cs-color)   ; selected index
   (range :initarg :range :initform 16 :accessor cs-range))  ; 16 (fg) or 8 (bg)
  (:documentation "A grid of colour swatches; the selected index is its data."))

(defmethod initialize-instance :after ((cs tcolor-selector) &key)
  (setf (view-options cs) (logior (view-options cs) +of-selectable+ +of-first-click+)
        (view-state cs) (logior (view-state cs) +sf-cursor-vis+)))

(defmethod get-palette ((cs tcolor-selector)) (make-palette 8 7))

(defmethod draw ((cs tcolor-selector))
  (let* ((w (point-x (view-size cs)))
         (frame (get-color cs 1))
         (block (code-char #x2588))
         (rows (ceiling (cs-range cs) +color-sel-cols+))
         (db (make-draw-buffer w)))
    (dotimes (row rows)
      (db-fill db #\Space frame)
      (dotimes (col +color-sel-cols+)
        (let ((idx (+ (* row +color-sel-cols+) col)) (x (* col +color-sel-cellw+)))
          (when (< idx (cs-range cs))
            (db-fill db block (make-attr idx 0) (+ x 1) 2)
            (when (= idx (cs-color cs))
              (db-fill db #\[ (make-attr 15 0) x 1)
              (db-fill db #\] (make-attr 15 0) (+ x 3) 1)))))
      (write-line* cs 0 row w 1 db))
    (when (logtest (view-state cs) +sf-focused+)
      (set-cursor cs (1+ (* (mod (cs-color cs) +color-sel-cols+) +color-sel-cellw+))
                  (floor (cs-color cs) +color-sel-cols+)))))

(defun %cs-set (cs idx)
  (setf (cs-color cs) (mod idx (cs-range cs)))
  (draw-view (or (view-owner cs) cs)))   ; redraw owner so the sample updates

(defmethod handle-event ((cs tcolor-selector) event)
  (cond
    ((and (= (event-type event) +ev-key-down+) (logtest (view-state cs) +sf-focused+))
     (let ((k (event-key-code event)) (h t))
       (cond
         ((= k +kb-right+) (%cs-set cs (1+ (cs-color cs))))
         ((= k +kb-left+)  (%cs-set cs (1- (cs-color cs))))
         ((= k +kb-down+)  (%cs-set cs (+ (cs-color cs) +color-sel-cols+)))
         ((= k +kb-up+)    (%cs-set cs (- (cs-color cs) +color-sel-cols+)))
         (t (setf h nil)))
       (when h (clear-event event))))
    ((and (= (event-type event) +ev-mouse-down+) (mouse-in-view-p cs event))
     (let* ((lp (make-local cs (event-mouse-where event)))
            (idx (+ (* (point-y lp) +color-sel-cols+)
                    (floor (point-x lp) +color-sel-cellw+))))
       (when (< idx (cs-range cs)) (%cs-set cs idx)))
     (clear-event event))
    (t (call-next-method))))

(defmethod data-size ((cs tcolor-selector)) 1)
(defmethod get-data ((cs tcolor-selector)) (cs-color cs))
(defmethod set-data ((cs tcolor-selector) data) (setf (cs-color cs) (or data 0)) (draw-view cs))

;;; ---------------------------------------------------------------------------
;;; TMonoSelector --- normal / highlight / underline / inverse
;;; ---------------------------------------------------------------------------

(defparameter +mono-attrs+ '(#x07 #x0f #x01 #x70)
  "DOS attribute bytes for Normal / Highlight / Underline / Inverse.")

(defclass tmono-selector (tradio-buttons) ()
  (:documentation "Pick a monochrome attribute (Normal/Highlight/Underline/Inverse)."))

(defun make-mono-selector (bounds &optional (value 0))
  (make-instance 'tmono-selector :value value
                 :labels '("~N~ormal" "~H~ighlight" "~U~nderline" "~I~nverse")
                 :bounds bounds))

(defun mono-selector-attr (ms) (nth (cluster-value ms) +mono-attrs+))

;;; ---------------------------------------------------------------------------
;;; TColorDisplay --- a live sample reflecting two colour selectors
;;; ---------------------------------------------------------------------------

(defclass tcolor-display (tview)
  ((fg   :initarg :fg   :initform nil :accessor cd-fg)    ; a tcolor-selector
   (bg   :initarg :bg   :initform nil :accessor cd-bg)    ; a tcolor-selector
   (text :initarg :text :initform " Sample text " :accessor cd-text)))

(defmethod draw ((p tcolor-display))
  (let* ((w (point-x (view-size p))) (h (point-y (view-size p)))
         (attr (make-attr (if (cd-fg p) (cs-color (cd-fg p)) 7)
                          (if (cd-bg p) (cs-color (cd-bg p)) 0)))
         (db (make-draw-buffer w)))
    (db-fill db #\Space attr)
    (db-move-str db 1 (cd-text p) attr)
    (dotimes (y h) (write-line* p 0 y w 1 db))))

;; Backward-compatible alias for the previous preview class name.
(defclass color-preview (tcolor-display) ())

;;; ---------------------------------------------------------------------------
;;; TColorDialog
;;; ---------------------------------------------------------------------------

(defun color-dialog (&key (title "Colors") (fg 7) (bg 0))
  "Open a modal colour picker built from TColorSelector grids and a live sample.
Return (values ok-p fg bg)."
  (when *application*
    (let* ((w 44) (h 16)
           (d (make-instance 'tdialog :title title :bounds (make-trect 0 0 w h)))
           (fgc (make-instance 'tcolor-selector :color fg :range 16
                               :bounds (make-trect 3 3 19 7)))
           (bgc (make-instance 'tcolor-selector :color bg :range 8
                               :bounds (make-trect 3 10 19 12)))
           (disp (make-instance 'tcolor-display :fg fgc :bg bgc
                                :bounds (make-trect 24 3 (- w 3) 7))))
      (flet ((lbl (text x y)
               (let ((l (make-instance 'tlabel :text text)))
                 (set-bounds l (make-trect x y (+ x (length text)) (1+ y)))
                 (insert d l))))
        (lbl "Foreground" 3 2) (insert d fgc)
        (lbl "Background" 3 9) (insert d bgc)
        (lbl "Sample" 24 2) (insert d disp))
      (insert d (make-button (make-trect (- w 26) (- h 3) (- w 16) (- h 1)) "~O~K" +cm-ok+ t))
      (insert d (make-button (make-trect (- w 13) (- h 3) (- w 3) (- h 1)) "Cancel" +cm-cancel+))
      (let ((desk (program-desktop *application*)))
        (move-to d (max 0 (floor (- (point-x (view-size desk)) w) 2))
                 (max 0 (floor (- (point-y (view-size desk)) h) 2))))
      (focus fgc)
      (if (= (exec-view (program-desktop *application*) d) +cm-ok+)
          (values t (cs-color fgc) (cs-color bgc))
          (values nil fg bg)))))
