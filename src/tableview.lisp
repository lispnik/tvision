;;;; tableview.lisp --- TTableView: a scrollable grid with a fixed, sortable
;;;; header row.
;;;;
;;;; Beyond Turbo Vision's flowed multi-column TListViewer: a real table with
;;;; named columns, a header row, per-column widths/alignment, and interactive
;;;; sorting (click a header, or press s to cycle / r to reverse).  Rows are
;;;; arbitrary objects; each column carries a KEY function (row -> value).

(in-package #:tvision)

(defstruct (table-column (:constructor make-table-column
                             (title width key &key numeric format)))
  (title "" )
  (width 8 :type fixnum)
  key                 ; (row -> value), used for display and sorting
  (numeric nil)       ; right-align + numeric sort
  (format nil))       ; optional (value -> string)

(defun %fit (s width &optional right)
  "Pad/truncate S to exactly WIDTH chars (right-justified when RIGHT)."
  (let ((len (length s)))
    (cond ((= len width) s)
          ((> len width) (subseq s 0 width))
          (right (concatenate 'string (make-string (- width len) :initial-element #\Space) s))
          (t (concatenate 'string s (make-string (- width len) :initial-element #\Space))))))

(defun %cell-string (col val)
  (let ((f (table-column-format col)))
    (cond (f (funcall f val))
          ((and (table-column-numeric col) (integerp val)) (format nil "~:d" val))
          ((and (table-column-numeric col) (realp val))    (format nil "~,1f" val))
          (t (princ-to-string val)))))

(defclass ttable-view (tscroller)
  ((columns  :initarg :columns :initform #() :accessor table-columns)  ; vector of table-column
   (rows     :initarg :rows    :initform #() :accessor table-rows)     ; vector of row objects
   (focused  :initform 0   :accessor table-focused)
   (sort-col :initarg :sort-col :initform 0 :accessor table-sort-col)
   (sort-asc :initarg :sort-asc :initform nil :accessor table-sort-asc)  ; default descending
   (command  :initarg :command :initform 0 :accessor table-command)))

(defmethod get-palette ((tv ttable-view)) (make-palette 14 13 13))  ; header / normal / sel

(defun table-data-height (tv) (max 1 (1- (point-y (view-size tv)))))
(defun table-top (tv) (point-y (scroller-delta tv)))

(defun table-selected-row (tv)
  (when (and (plusp (length (table-rows tv)))
             (< (table-focused tv) (length (table-rows tv))))
    (aref (table-rows tv) (table-focused tv))))

(defun table-update-limit (tv)
  (let ((w (reduce #'+ (table-columns tv) :key (lambda (c) (1+ (table-column-width c))) :initial-value 0)))
    (set-scroller-limit tv (max 1 w) (max 1 (length (table-rows tv))))))

(defun table-sort (tv)
  "(Re)sort ROWS by the current column/direction."
  (when (plusp (length (table-columns tv)))
    (let* ((col (aref (table-columns tv) (table-sort-col tv)))
           (key (table-column-key col))
           (num (table-column-numeric col))
           (asc (table-sort-asc tv))
           (less (if num
                     (lambda (a b) (< (or (funcall key a) 0) (or (funcall key b) 0)))
                     (lambda (a b) (string-lessp (princ-to-string (funcall key a))
                                                 (princ-to-string (funcall key b))))))
           (cmp (if asc less (lambda (a b) (funcall less b a)))))
      (setf (table-rows tv) (stable-sort (copy-seq (table-rows tv)) cmp))
      (setf (table-focused tv) (min (table-focused tv) (max 0 (1- (length (table-rows tv)))))))))

(defun table-set-rows (tv rows)
  "Replace the rows (a sequence) and re-sort/redraw."
  (setf (table-rows tv) (coerce rows 'vector))
  (table-sort tv)
  (table-update-limit tv)
  (draw-view tv)
  tv)

(defmethod initialize-instance :after ((tv ttable-view) &key)
  (when (plusp (length (table-rows tv)))
    (setf (table-rows tv) (coerce (table-rows tv) 'vector))
    (table-sort tv))
  (table-update-limit tv))

;;; --- drawing ---------------------------------------------------------------

(defmethod draw ((tv ttable-view))
  (let* ((w (point-x (view-size tv))) (h (point-y (view-size tv)))
         (cols (table-columns tv))
         (hdr (get-color tv 1)) (normal (get-color tv 2))
         ;; a reverse-video bar marks the selected row, visible even when the
         ;; table isn't the focused view (the palette's "sel" equals NORMAL)
         (sel (selection-highlight normal (logtest (view-state tv) +sf-focused+)))
         (top (table-top tv))
         (dx (point-x (scroller-delta tv)))      ; horizontal scroll offset
         (db (make-draw-buffer w)))
    ;; header row (fixed)
    (db-fill db #\Space hdr)
    (let ((x (- dx)))
      (dotimes (ci (length cols))
        (let* ((c (aref cols ci))
               (mark (cond ((/= ci (table-sort-col tv)) "")
                           ((table-sort-asc tv) (string (code-char #x2191)))   ; up arrow
                           (t (string (code-char #x2193)))))                   ; down arrow
               (title (concatenate 'string (table-column-title c) mark)))
          (db-move-str db x (%fit title (table-column-width c)) hdr)
          (incf x (1+ (table-column-width c))))))
    (write-line* tv 0 0 w 1 db)
    ;; data rows
    (loop for row from 1 below h
          for ri = (+ top (1- row)) do
      (let ((a (if (and (< ri (length (table-rows tv))) (= ri (table-focused tv))) sel normal)))
        (db-fill db #\Space a)
        (when (< ri (length (table-rows tv)))
          (let ((obj (aref (table-rows tv) ri)) (x (- dx)))
            (dotimes (ci (length cols))
              (let* ((c (aref cols ci))
                     (str (%cell-string c (funcall (table-column-key c) obj))))
                (db-move-str db x (%fit str (table-column-width c) (table-column-numeric c)) a)
                (incf x (1+ (table-column-width c)))))))
        (write-line* tv 0 row w 1 db)))))

;;; --- navigation / sorting --------------------------------------------------

(defun table-ensure-visible (tv)
  (let ((datah (table-data-height tv)) (top (table-top tv)) (f (table-focused tv)))
    (when (< f top) (setf top f))
    (when (>= f (+ top datah)) (setf top (1+ (- f datah))))
    (table-update-limit tv)
    (scroll-to tv 0 (max 0 top))))

(defun table-focus (tv i)
  (let ((n (length (table-rows tv))))
    (when (plusp n)
      (setf (table-focused tv) (min (max 0 i) (1- n)))
      (table-ensure-visible tv)
      (draw-view tv)
      (message (view-owner tv) +ev-broadcast+ +cm-list-focus-changed+ tv))))

(defun table-select (tv)
  (when (plusp (length (table-rows tv)))
    (message (view-owner tv) +ev-broadcast+ +cm-list-item-selected+ tv)))

(defun table-sort-by (tv ci)
  "Sort by column CI; toggle direction if it is already the sort column,
otherwise pick a sensible default (descending for numeric columns)."
  (when (and (>= ci 0) (< ci (length (table-columns tv))))
    (if (= ci (table-sort-col tv))
        (setf (table-sort-asc tv) (not (table-sort-asc tv)))
        (setf (table-sort-col tv) ci
              (table-sort-asc tv) (not (table-column-numeric (aref (table-columns tv) ci)))))
    (table-sort tv)
    (table-ensure-visible tv)
    (draw-view tv)))

(defun %table-col-at (tv lx)
  (let ((x 0))
    (dotimes (ci (length (table-columns tv)) nil)
      (let ((wd (1+ (table-column-width (aref (table-columns tv) ci)))))
        (when (< lx (+ x wd)) (return-from %table-col-at ci))
        (incf x wd)))))

(defmethod handle-event ((tv ttable-view) event)
  (cond
    ((scrollbar-event-p tv event) (scroll-from-scrollbars tv))
    ((and (= (event-type event) +ev-mouse-wheel+) (mouse-in-view-p tv event))
     (table-focus tv (+ (table-focused tv) (* 3 (event-wheel event))))
     (clear-event event))
    ((and (= (event-type event) +ev-mouse-down+) (mouse-in-view-p tv event))
     (let* ((lp (make-local tv (event-mouse-where event)))
            (ly (point-y lp)) (lx (point-x lp)))
       (if (zerop ly)
           (let ((ci (%table-col-at tv lx)))            ; header -> sort
             (when ci (table-sort-by tv ci)))
           (let ((ri (+ (table-top tv) (1- ly))))       ; data row -> focus / activate
             (when (< ri (length (table-rows tv)))
               (table-focus tv ri)
               (when (event-double event) (table-select tv))))))
     (clear-event event))
    ((and (= (event-type event) +ev-key-down+) (logtest (view-state tv) +sf-focused+))
     (let ((k (event-key-code event)) (ch (event-char-code event))
           (page (table-data-height tv)) (handled t))
       (cond
         ((= k +kb-up+)    (table-focus tv (1- (table-focused tv))))
         ((= k +kb-down+)  (table-focus tv (1+ (table-focused tv))))
         ((= k +kb-pgup+)  (table-focus tv (- (table-focused tv) page)))
         ((= k +kb-pgdn+)  (table-focus tv (+ (table-focused tv) page)))
         ((= k +kb-home+)  (table-focus tv 0))
         ((= k +kb-end+)   (table-focus tv (1- (length (table-rows tv)))))
         ((= k +kb-enter+) (table-select tv))
         ((member ch '(#.(char-code #\s) #.(char-code #\S)))  ; cycle sort column
          (table-sort-by tv (mod (1+ (table-sort-col tv)) (max 1 (length (table-columns tv))))))
         ((member ch '(#.(char-code #\r) #.(char-code #\R)))  ; reverse direction
          (table-sort-by tv (table-sort-col tv)))
         (t (setf handled nil)))
       (when handled (clear-event event))))))

(defmethod data-size ((tv ttable-view)) 1)
(defmethod get-data ((tv ttable-view)) (table-focused tv))
(defmethod set-data ((tv ttable-view) data) (table-focus tv (or data 0)))
