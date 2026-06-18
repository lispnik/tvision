;;;; help.lisp --- A help system with hypertext links and a navigable viewer.
;;;;
;;;; Topics are registered by help-context (integer) and/or by name (string).
;;;; Help text may embed links written {Target} or {Display|Target}; following a
;;;; link looks the target up as a named topic.  THELP-VIEWER renders the links,
;;;; lets you Tab between them, Enter/click to follow, and Backspace to go back.

(in-package #:tvision)

(defvar *help-topics* (make-hash-table :test 'eql)
  "Map of help-context -> help text string.")
(defvar *help-by-name* (make-hash-table :test 'equal)
  "Map of topic name -> help text string (link targets).")

(defun register-help (ctx text)
  "Associate help TEXT with help-context CTX."
  (setf (gethash ctx *help-topics*) text)
  ctx)

(defun register-help-topic (name text)
  "Associate help TEXT with a named topic (a link target)."
  (setf (gethash name *help-by-name*) text)
  name)

(defun help-text (ctx) (gethash ctx *help-topics*))
(defun help-topic (name) (gethash name *help-by-name*))

;;; --- link parsing ----------------------------------------------------------

(defun parse-help-links (text)
  "Return (values display-text links), where LINKS is a list of
(line col len target) for each {Target} / {Display|Target} in TEXT."
  (let ((out (make-string-output-stream)) (links '())
        (line 0) (col 0) (i 0) (n (length text)))
    (loop while (< i n) do
      (let ((ch (char text i)))
        (cond
          ((char= ch #\{)
           (let ((close (position #\} text :start i)))
             (if close
                 (let* ((inner (subseq text (1+ i) close))
                        (bar (position #\| inner))
                        (disp (if bar (subseq inner 0 bar) inner))
                        (target (if bar (subseq inner (1+ bar)) inner)))
                   (push (list line col (length disp) target) links)
                   (write-string disp out) (incf col (length disp))
                   (setf i (1+ close)))
                 (progn (write-char ch out) (incf col) (incf i)))))
          ((char= ch #\Newline) (write-char ch out) (incf line) (setf col 0) (incf i))
          (t (write-char ch out) (incf col) (incf i)))))
    (values (get-output-stream-string out) (nreverse links))))

;;; --- the viewer ------------------------------------------------------------

(defclass thelp-viewer (ttext-view)
  ((links :initform '() :accessor help-links)
   (raw   :initform "" :accessor help-raw)
   (stack :initform '() :accessor help-stack)))   ; previous raw texts

(defmethod get-palette ((v thelp-viewer)) (make-palette 6 14))  ; text / link

(defun help-show (v raw &key push)
  "Display RAW help text in viewer V (parsing links)."
  (when push (push (help-raw v) (help-stack v)))
  (multiple-value-bind (disp links) (parse-help-links raw)
    (set-text v disp)
    (setf (help-raw v) raw (help-links v) links (text-read-only v) t)
    (when links
      (setf (text-cur-line v) (first (first links))
            (text-cur-col v) (second (first links))))
    (ensure-visible v) (draw-view v)))

(defun link-at (v line col)
  "Return the link (line col len target) containing (LINE,COL), or NIL."
  (find-if (lambda (lk)
             (destructuring-bind (ln c len tgt) lk
               (declare (ignore tgt))
               (and (= ln line) (>= col c) (< col (+ c len)))))
           (help-links v)))

(defmethod draw ((v thelp-viewer))
  (call-next-method)
  ;; recolor link spans on top of the rendered text
  (when *screen*
    (multiple-value-bind (gx gy) (view-global-origin v)
      (let ((linkc (get-color v 2)) (selc (make-attr 15 0))
            (dx (text-left-col v)) (dy (text-top-line v))
            (w (point-x (view-size v))) (h (point-y (view-size v)))
            (cur (link-at v (text-cur-line v) (text-cur-col v))))
        (dolist (lk (help-links v))
          (destructuring-bind (ln col len target) lk
            (declare (ignore target))
            (let ((row (- ln dy)) (attr (if (eq lk cur) selc linkc)))
              (when (and (>= row 0) (< row h))
                (loop for x from (max 0 (- col dx)) below (min w (- (+ col len) dx))
                      for sx = (+ gx x) for sy = (+ gy row)
                      when (and (< sx (screen-width *screen*)) (< sy (screen-height *screen*)))
                      do (let ((c (aref (screen-back *screen*)
                                        (+ sx (* sy (screen-width *screen*))))))
                           (screen-cell-set *screen* sx sy
                                            (cell-make-code (cell-char-code c) attr))))))))))))

(defun help-next-link (v)
  (let ((next (find-if (lambda (lk)
                         (or (> (first lk) (text-cur-line v))
                             (and (= (first lk) (text-cur-line v))
                                  (> (second lk) (text-cur-col v)))))
                       (help-links v))))
    (let ((lk (or next (first (help-links v)))))
      (when lk
        (setf (text-cur-line v) (first lk) (text-cur-col v) (second lk))
        (ensure-visible v) (draw-view v)))))

(defun help-follow (v)
  (let ((lk (link-at v (text-cur-line v) (text-cur-col v))))
    (when lk
      (let ((txt (help-topic (fourth lk))))
        (help-show v (or txt (format nil "No help topic: ~a" (fourth lk))) :push t)))))

(defun help-back (v)
  (when (help-stack v)
    (help-show v (pop (help-stack v)))))

(defmethod handle-event ((v thelp-viewer) event)
  (cond
    ((= (event-type event) +ev-key-down+)
     (let ((k (event-key-code event)))
       (cond
         ((= k +kb-tab+)   (help-next-link v) (clear-event event))
         ((= k +kb-enter+) (help-follow v) (clear-event event))
         ((= k +kb-back+)  (help-back v) (clear-event event))
         (t (call-next-method)))))
    ((and (= (event-type event) +ev-mouse-down+) (mouse-in-view-p v event))
     (call-next-method)                         ; positions the cursor
     (when (link-at v (text-cur-line v) (text-cur-col v)) (help-follow v)))
    (t (call-next-method))))

;;; --- opening help ----------------------------------------------------------

(defun open-help (ctx &optional (title "Help"))
  "Display the help topic for CTX modally, with link navigation."
  (when *application*
    (let* ((text (or (help-text ctx)
                     (format nil "No help is available for this topic.~%~%(context ~a)" ctx)))
           (desk (program-desktop *application*))
           (w 64) (h (min (- (point-y (view-size desk)) 2) 18))
           (d (make-instance 'tdialog :title title :bounds (make-trect 0 0 w h)))
           (v (make-instance 'thelp-viewer :read-only t
                             :bounds (make-trect 2 1 (1- w) (- h 4)))))
      (insert d v)
      (insert d (make-button (make-trect (floor (- w 10) 2) (- h 3)
                                         (+ (floor (- w 10) 2) 10) (- h 1))
                             "O~K~" +cm-ok+ t))
      (move-to d (floor (- (point-x (view-size desk)) w) 2)
               (max 0 (floor (- (point-y (view-size desk)) h) 2)))
      (help-show v text)
      (focus v)
      (exec-view desk d))))
