;;;; frame.lisp --- TFrame, the border drawn around a window.

(in-package #:tvision)

;;; Box-drawing glyphs, given by Unicode code point to stay independent of the
;;; source file's external format.  Layout: (top-left horiz top-right vert
;;; bottom-left bottom-right).
(defparameter +single-box+ (vector #x250C #x2500 #x2510 #x2502 #x2514 #x2518))
(defparameter +double-box+ (vector #x2554 #x2550 #x2557 #x2551 #x255A #x255D))

;;; Generics resolved by TWindow (defined later) so TFrame need not know the
;;; concrete window class at compile time.
(defgeneric frame-owner-title (v) (:method ((v tview)) nil))
(defgeneric frame-owner-flags (v) (:method ((v tview)) 0))

(defclass tframe (tview)
  ())

(defmethod initialize-instance :after ((f tframe) &key)
  (setf (view-grow-mode f) (logior +gf-grow-hix+ +gf-grow-hiy+)))

(defmethod get-palette ((f tframe))
  ;; 1 = passive frame, 2 = active frame, 3 = icons
  (make-palette 1 2 3))

(defun %set-cell (db i code attr)
  (let ((data (draw-buffer-data db)))
    (when (and (>= i 0) (< i (length data)))
      (setf (aref data i) (cell-make-code code attr)))))

(defmethod draw ((f tframe))
  (let* ((w (point-x (view-size f)))
         (h (point-y (view-size f)))
         (win (view-owner f))
         (active (and win (logtest (view-state win) +sf-selected+)))
         (box (if active +double-box+ +single-box+))
         (cf (get-color f (if active 2 1)))
         (ci (get-color f 3))
         (db (make-draw-buffer w)))
    (when (or (< w 2) (< h 2)) (return-from draw))
    ;; --- top border -------------------------------------------------------
    (db-fill db (code-char (aref box 1)) cf)
    (%set-cell db 0 (aref box 0) cf)
    (%set-cell db (1- w) (aref box 2) cf)
    (write-line* f 0 0 w 1 db)
    ;; --- middle rows ------------------------------------------------------
    (db-fill db #\Space cf)
    (%set-cell db 0 (aref box 3) cf)
    (%set-cell db (1- w) (aref box 3) cf)
    (loop for y from 1 below (1- h) do (write-line* f 0 y w 1 db))
    ;; --- bottom border ----------------------------------------------------
    (db-fill db (code-char (aref box 1)) cf)
    (%set-cell db 0 (aref box 4) cf)
    (%set-cell db (1- w) (aref box 5) cf)
    (write-line* f 0 (1- h) w 1 db)
    ;; --- title ------------------------------------------------------------
    (let ((title (frame-owner-title win)))
      (when (and title (> (length title) 0) (> w 6))
        (let* ((maxw (- w 4))
               (text (if (> (length title) maxw) (subseq title 0 maxw) title))
               (tx (max 2 (floor (- w (+ 2 (length text))) 2))))
          (write-str f tx 0 (format nil " ~a " text) (if active 2 1)))))
    ;; --- close / zoom icons ----------------------------------------------
    (let ((flags (frame-owner-flags win)))
      (when (and (logtest flags +wf-close+) (> w 5))
        (write-str f 2 0 "[ ]" 3)
        (%set-cell-screen f 3 0 #x00D7 ci))   ; multiplication sign as "close"
      (when (and (logtest flags +wf-zoom+) (> w 7))
        (write-str f (- w 5) 0 "[ ]" 3)
        (%set-cell-screen f (- w 4) 0 #x2191 ci))))) ; up-arrow as "zoom"

(defun %set-cell-screen (v x y code attr)
  "Write a single glyph (by code point) at local (X,Y) of view V."
  (let ((db (make-draw-buffer 1)))
    (%set-cell db 0 code attr)
    (write-line* v x y 1 1 db)))
