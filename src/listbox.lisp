;;;; listbox.lisp --- TListViewer / TListBox: a scrollable, selectable list.

(in-package #:tvision)

(defconstant +cm-list-item-selected+ 61)
(defconstant +cm-list-focus-changed+ 62)

(defclass tlist-box (tscroller)
  ((items   :initarg :items   :initform #() :accessor %list-items)
   (focused :initform 0       :accessor list-focused)
   (command :initarg :command :initform 0 :accessor list-command)))

(defmethod get-palette ((lb tlist-box)) (make-palette 13 14))  ; normal / focused

;;; Items may be a list, a vector, or a TCOLLECTION; normalise access.

(defun list-count (lb)
  (let ((it (%list-items lb)))
    (etypecase it
      (tcollection (collection-count it))
      (sequence (length it)))))

(defun list-item (lb i)
  (let ((it (%list-items lb)))
    (princ-to-string
     (etypecase it
       (tcollection (at it i))
       (list (nth i it))
       (vector (aref it i))))))

(defun list-set-items (lb items)
  (setf (%list-items lb) items)
  (setf (list-focused lb) (min (list-focused lb) (max 0 (1- (list-count lb)))))
  (list-update-limit lb)
  (draw-view lb)
  lb)

(defun list-update-limit (lb)
  (let ((maxw 1))
    (dotimes (i (list-count lb)) (setf maxw (max maxw (length (list-item lb i)))))
    (set-scroller-limit lb maxw (list-count lb))))

(defmethod initialize-instance :after ((lb tlist-box) &key)
  (list-update-limit lb))

(defun list-focus-item (lb i)
  "Move the focus to item I, scrolling it into view."
  (let ((n (list-count lb)))
    (when (plusp n)
      (setf (list-focused lb) (min (max 0 i) (1- n)))
      ;; keep focused row visible
      (let ((h (point-y (view-size lb))) (top (point-y (scroller-delta lb))))
        (cond ((< (list-focused lb) top)
               (scroll-to lb (point-x (scroller-delta lb)) (list-focused lb)))
              ((>= (list-focused lb) (+ top h))
               (scroll-to lb (point-x (scroller-delta lb)) (1+ (- (list-focused lb) h))))))
      (draw-view lb)
      (message (view-owner lb) +ev-broadcast+ +cm-list-focus-changed+ lb))))

(defun list-select (lb)
  "Fire the list's command for the focused item."
  (when (plusp (list-count lb))
    (when (plusp (list-command lb))
      (put-event lb (make-event :type +ev-command+ :command (list-command lb) :info lb)))
    (message (view-owner lb) +ev-broadcast+ +cm-list-item-selected+ lb)))

(defmethod draw ((lb tlist-box))
  (let* ((w (point-x (view-size lb))) (h (point-y (view-size lb)))
         (normal (get-color lb 1)) (focused (get-color lb 2))
         (dx (point-x (scroller-delta lb))) (dy (point-y (scroller-delta lb)))
         (active (logtest (view-state lb) +sf-focused+))
         (db (make-draw-buffer w)))
    (dotimes (row h)
      (let* ((i (+ dy row))
             (sel (and (= i (list-focused lb)) active))
             (attr (if sel focused normal)))
        (db-fill db #\Space attr)
        (when (< i (list-count lb))
          (let* ((s (list-item lb i))
                 (vis (subseq s (min dx (length s)) (min (length s) (+ dx w)))))
            (db-move-str db 0 vis attr)))
        (write-line* lb 0 row w 1 db)))))

(defmethod handle-event ((lb tlist-box) event)
  (cond
    ((scrollbar-event-p lb event) (scroll-from-scrollbars lb))
    ((and (= (event-type event) +ev-mouse-down+) (mouse-in-view-p lb event))
     (let ((row (+ (point-y (scroller-delta lb))
                   (point-y (make-local lb (event-mouse-where event))))))
       (list-focus-item lb row)
       (when (event-double event) (list-select lb)))
     (clear-event event))
    ((= (event-type event) +ev-mouse-wheel+)
     (list-focus-item lb (+ (list-focused lb) (* 3 (event-wheel event))))
     (clear-event event))
    ((and (= (event-type event) +ev-key-down+) (logtest (view-state lb) +sf-focused+))
     (let ((k (event-key-code event)) (h (max 1 (point-y (view-size lb)))) (handled t))
       (cond
         ((= k +kb-up+)    (list-focus-item lb (1- (list-focused lb))))
         ((= k +kb-down+)  (list-focus-item lb (1+ (list-focused lb))))
         ((= k +kb-pgup+)  (list-focus-item lb (- (list-focused lb) h)))
         ((= k +kb-pgdn+)  (list-focus-item lb (+ (list-focused lb) h)))
         ((= k +kb-home+)  (list-focus-item lb 0))
         ((= k +kb-end+)   (list-focus-item lb (1- (list-count lb))))
         ((= k +kb-enter+) (list-select lb))
         (t (setf handled nil)))
       (when handled (clear-event event))))))

(defmethod data-size ((lb tlist-box)) 1)
(defmethod get-data ((lb tlist-box)) (list-focused lb))
(defmethod set-data ((lb tlist-box) data) (list-focus-item lb (or data 0)))
