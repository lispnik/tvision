;;;; listbox.lisp --- TListViewer / TListBox: a scrollable, selectable list.

(in-package #:tvision)

(defconstant +cm-list-item-selected+ 61)
(defconstant +cm-list-focus-changed+ 62)

;;; TListViewer is Turbo Vision's abstract base for scrollable, focusable lists.
;;; TListBox below is the concrete list backed by an item collection.
(defclass tlist-viewer (tscroller) ()
  (:documentation "Abstract base for list viewers (scroll + focus + selection)."))

(defclass tlist-box (tlist-viewer)
  ((items   :initarg :items   :initform #() :accessor %list-items)
   (focused :initform 0       :accessor list-focused)
   (command :initarg :command :initform 0 :accessor list-command)
   (columns :initarg :columns :initform 1 :accessor list-columns)))

(defmethod get-palette ((lb tlist-box)) (make-palette 13 14))  ; normal / focused

(defun list-cols (lb) (max 1 (list-columns lb)))
(defun list-col-width (lb) (floor (max 1 (point-x (view-size lb))) (list-cols lb)))
(defun item-row (lb i) (floor i (list-cols lb)))

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
  (let ((maxw 1) (rows (ceiling (max 1 (list-count lb)) (list-cols lb))))
    (dotimes (i (list-count lb)) (setf maxw (max maxw (length (list-item lb i)))))
    ;; multi-column lists don't scroll horizontally
    (set-scroller-limit lb (if (> (list-cols lb) 1) (point-x (view-size lb)) maxw)
                        rows)))

(defmethod initialize-instance :after ((lb tlist-box) &key)
  (list-update-limit lb))

(defun list-focus-item (lb i)
  "Move the focus to item I, scrolling its row into view."
  (let ((n (list-count lb)))
    (when (plusp n)
      (setf (list-focused lb) (min (max 0 i) (1- n)))
      (let ((h (point-y (view-size lb))) (top (point-y (scroller-delta lb)))
            (row (item-row lb (list-focused lb))))
        (cond ((< row top) (scroll-to lb (point-x (scroller-delta lb)) row))
              ((>= row (+ top h)) (scroll-to lb (point-x (scroller-delta lb)) (1+ (- row h))))))
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
         (normal (get-color lb 1))
         (dx (point-x (scroller-delta lb))) (dy (point-y (scroller-delta lb)))
         (active (logtest (view-state lb) +sf-focused+))
         (cols (list-cols lb)) (cw (list-col-width lb))
         (db (make-draw-buffer w)))
    (dotimes (dr h)
      (db-fill db #\Space normal)
      (let ((row (+ dy dr)))
        (dotimes (c cols)
          (let ((i (+ (* row cols) c)))
            (when (< i (list-count lb))
              (let* ((sel (= i (list-focused lb)))
                     ;; show the selection even when the list isn't focused
                     (attr (if sel (selection-highlight normal active) normal))
                     (x (* c cw))
                     (cwidth (if (> cols 1) cw w))
                     (s (list-item lb i))
                     (start (if (> cols 1) 0 (min dx (length s))))
                     (vis (subseq s (min start (length s))
                                  (min (length s) (+ start (max 0 (1- cwidth)))))))
                (db-fill db #\Space attr x cwidth)
                (db-move-str db x vis attr))))))
      (write-line* lb 0 dr w 1 db))))

(defmethod handle-event ((lb tlist-box) event)
  (cond
    ((scrollbar-event-p lb event) (scroll-from-scrollbars lb))
    ((and (= (event-type event) +ev-mouse-down+) (mouse-in-view-p lb event))
     (let* ((lp (make-local lb (event-mouse-where event)))
            (cols (list-cols lb))
            (row (+ (point-y (scroller-delta lb)) (point-y lp)))
            (col (if (> cols 1) (min (1- cols) (floor (point-x lp) (max 1 (list-col-width lb)))) 0)))
       (list-focus-item lb (+ (* row cols) col))
       (when (event-double event) (list-select lb)))
     (clear-event event))
    ((= (event-type event) +ev-mouse-wheel+)
     (list-focus-item lb (+ (list-focused lb) (* 3 (list-cols lb) (event-wheel event))))
     (clear-event event))
    ((and (= (event-type event) +ev-key-down+) (logtest (view-state lb) +sf-focused+))
     (let ((k (event-key-code event)) (cols (list-cols lb))
           (page (* (max 1 (point-y (view-size lb))) (list-cols lb))) (handled t))
       (cond
         ((= k +kb-up+)    (list-focus-item lb (- (list-focused lb) cols)))
         ((= k +kb-down+)  (list-focus-item lb (+ (list-focused lb) cols)))
         ((and (> cols 1) (= k +kb-left+))  (list-focus-item lb (1- (list-focused lb))))
         ((and (> cols 1) (= k +kb-right+)) (list-focus-item lb (1+ (list-focused lb))))
         ((= k +kb-pgup+)  (list-focus-item lb (- (list-focused lb) page)))
         ((= k +kb-pgdn+)  (list-focus-item lb (+ (list-focused lb) page)))
         ((= k +kb-home+)  (list-focus-item lb 0))
         ((= k +kb-end+)   (list-focus-item lb (1- (list-count lb))))
         ((= k +kb-enter+) (list-select lb))
         (t (setf handled nil)))
       (when handled (clear-event event))))))

(defmethod data-size ((lb tlist-box)) 1)
(defmethod get-data ((lb tlist-box)) (list-focused lb))
(defmethod set-data ((lb tlist-box) data) (list-focus-item lb (or data 0)))

;;; --- TSortedListBox: incremental type-ahead search -------------------------
;;; Typing letters jumps to the first item beginning with what you've typed
;;; (the items are expected to be sorted); Backspace shortens the search.

(defclass tsorted-list-box (tlist-box)
  ((search :initform "" :accessor slb-search)))

(defun %slb-prefixp (prefix string)
  (and (<= (length prefix) (length string))
       (string= prefix string :end2 (length prefix))))

(defun slb-find (lb prefix)
  "Index of the first item starting with PREFIX (case-insensitive), or NIL."
  (let ((p (string-downcase prefix)))
    (dotimes (i (list-count lb))
      (when (%slb-prefixp p (string-downcase (list-item lb i))) (return i)))))

(defun slb-typeahead (lb event)
  (let ((k (event-key-code event)))
    (if (= k +kb-back+)
        (when (plusp (length (slb-search lb)))
          (setf (slb-search lb) (subseq (slb-search lb) 0 (1- (length (slb-search lb))))))
        (setf (slb-search lb)
              (concatenate 'string (slb-search lb)
                           (string (code-char (event-char-code event))))))
    (let ((pos (slb-find lb (slb-search lb))))
      (if pos
          (list-focus-item lb pos)
          ;; no match: drop the just-typed char so the buffer keeps the matched prefix
          (when (and (/= k +kb-back+) (plusp (length (slb-search lb))))
            (setf (slb-search lb) (subseq (slb-search lb) 0 (1- (length (slb-search lb))))))))))

(defmethod handle-event ((lb tsorted-list-box) event)
  (cond
    ((and (= (event-type event) +ev-key-down+)
          (logtest (view-state lb) +sf-focused+)
          (zerop (event-modifiers event))
          (or (= (event-key-code event) +kb-back+)
              (and (plusp (event-char-code event))
                   (let ((c (code-char (event-char-code event))))
                     (and c (graphic-char-p c) (char/= c #\Space))))))
     (slb-typeahead lb event)
     (clear-event event))
    (t
     ;; any navigation/other key restarts the type-ahead buffer
     (when (= (event-type event) +ev-key-down+) (setf (slb-search lb) ""))
     (call-next-method))))
