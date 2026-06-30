;;;; table.lisp --- a column/grid list viewer (tvision's TListViewer/TTableView).
;;;;
;;;; COLUMNS is a list of (TITLE WIDTH ACCESSOR); ACCESSOR maps a row object to
;;;; its cell value.  Row 0 is a header; the rest are the scrollable, selectable
;;;; data rows.  Implements the scroller protocol so a hosting window draws a
;;;; frame scrollbar, and supports keyboard + mouse selection and the wheel.

(in-package #:tv2)

(defclass table-view (view)
  ((columns     :initarg :columns :initform '() :accessor table-columns)   ; (TITLE WIDTH ACCESSOR)
   (rows        :initarg :rows :initform '() :accessor table-rows)
   (selected    :initform 0 :accessor table-selected)
   (top         :initform 0 :accessor table-top)                           ; first visible data row
   (on-activate :initarg :on-activate :initform nil :accessor table-on-activate))
  (:metaclass reactive-class))

(defmethod focusable-p ((tv table-view)) t)

(defun %pad (val w)
  "VAL as a string padded/truncated to W columns (with a trailing gap)."
  (let* ((s (princ-to-string val)) (n (length s)))
    (cond ((>= n w) (subseq s 0 (max 0 (1- w))))
          (t (concatenate 'string s (make-string (- w n) :initial-element #\Space))))))

(defun table-page (tv) (max 1 (1- (r-h (view-bounds tv)))))    ; visible data rows (header takes one)

(defun table-scroll-fix (tv)
  (let ((h (table-page tv)) (sel (table-selected tv)) (top (table-top tv)))
    (cond ((< sel top) (setf (table-top tv) sel))
          ((>= sel (+ top h)) (setf (table-top tv) (1+ (- sel h)))))))

(defun table-move (tv delta)
  (let ((n (length (table-rows tv))))
    (when (plusp n)
      (setf (table-selected tv) (max 0 (min (1- n) (+ (table-selected tv) delta))))
      (table-scroll-fix tv))))

(defmethod draw ((tv table-view))
  (let* ((b (view-bounds tv)) (h (r-h b)) (w (r-w b)) (active (view-focused-p tv))
         (cols (table-columns tv)) (rows (table-rows tv)) (top (table-top tv)))
    (let ((x 0) (hattr (role :label)))                          ; header
      (fill-row tv 0 0 w hattr)
      (dolist (c cols) (draw-text tv x 0 (%pad (first c) (second c)) hattr) (incf x (second c))))
    (dotimes (row (1- h))                                       ; data rows
      (let* ((i (+ top row)) (y (1+ row))
             (attr (if (and (= i (table-selected tv)) active) (role :focused) (role :normal))))
        (fill-row tv 0 y w attr)
        (when (< i (length rows))
          (let ((x 0) (rowdata (nth i rows)))
            (dolist (c cols)
              (draw-text tv x y (%pad (funcall (third c) rowdata) (second c)) attr)
              (incf x (second c)))))))))

(defun table-activate (tv)
  (when (and (table-on-activate tv) (< (table-selected tv) (length (table-rows tv))))
    (funcall (table-on-activate tv) tv (nth (table-selected tv) (table-rows tv)))))

(defmethod handle-event ((tv table-view) (e key-event))
  (let ((ks (event-keysym e)) (n (length (table-rows tv))))
    (cond
      ((eql ks :up)    (table-move tv -1) (setf (handled-p e) t))
      ((eql ks :down)  (table-move tv 1)  (setf (handled-p e) t))
      ((eql ks :pgup)  (table-move tv (- (table-page tv))) (setf (handled-p e) t))
      ((eql ks :pgdn)  (table-move tv (table-page tv)) (setf (handled-p e) t))
      ((eql ks :home)  (setf (table-selected tv) 0) (table-scroll-fix tv) (setf (handled-p e) t))
      ((eql ks :end)   (setf (table-selected tv) (max 0 (1- n))) (table-scroll-fix tv) (setf (handled-p e) t))
      ((eql ks :enter) (table-activate tv) (setf (handled-p e) t))
      (t (call-next-method)))))

(defmethod handle-event ((tv table-view) (e mouse-down))
  (let ((row (+ (table-top tv) (1- (mouse-row tv e)))))         ; row 0 is the header
    (when (and (>= (mouse-row tv e) 1) (< row (length (table-rows tv))))
      (setf (table-selected tv) row) (table-scroll-fix tv)))
  (setf (handled-p e) t))

(defmethod handle-event ((tv table-view) (e wheel-event))
  (table-move tv (* 3 (event-delta e))) (setf (handled-p e) t))

;;; scroller protocol (data rows scroll under the fixed header)
(defmethod scroll-page ((tv table-view)) (table-page tv))
(defmethod scroll-pos  ((tv table-view)) (table-top tv))
(defmethod scroll-max  ((tv table-view)) (max 0 (- (length (table-rows tv)) (table-page tv))))
(defmethod scroll-to   ((tv table-view) pos) (setf (table-top tv) (max 0 (min pos (scroll-max tv)))) (invalidate tv))
