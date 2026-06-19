;;;; textview.lisp --- TTextView: an editable, scrollable multi-line text area.
;;;;
;;;; This is the foundation for an editor and, shortly, a Lisp REPL.  Text is
;;;; held as an adjustable vector of line strings.  Editing, navigation and
;;;; scrolling are all driven through small primitives so that subclasses (e.g.
;;;; a REPL) can override behaviour -- in particular TEXT-RETURN, which decides
;;;; what the Enter key does.

(in-package #:tvision)

;;; A text view is a scroller whose virtual area is the text: DELTA's x/y are the
;;; left column and top line.  We expose them under the old names for clarity.

(defvar *clipboard* "" "Shared text clipboard for cut/copy/paste.")

(defclass ttext-view (tscroller)
  ((lines     :accessor text-lines)
   (cur-line  :initform 0 :accessor text-cur-line)
   (cur-col   :initform 0 :accessor text-cur-col)
   (read-only :initarg :read-only :initform nil :accessor text-read-only)
   ;; selection anchor (cons LINE . COL) or NIL when there is no selection
   (anchor    :initform nil :accessor text-anchor)
   ;; read-only boundary (cons LINE . COL): edits before it are blocked (REPL)
   (protect   :initform nil :accessor text-protect)
   (undo      :initform '() :accessor text-undo)
   (redo      :initform '() :accessor text-redo)
   (modified  :initform nil :accessor text-modified)
   (overwrite :initform nil :accessor text-overwrite)
   (wrap      :initarg :wrap :initform nil :accessor text-wrap) ; word-wrap mode
   (goal-col  :initform nil :accessor text-goal-col)))  ; desired visual col for Up/Down

(declaim (inline text-top-line text-left-col))
(defun text-top-line (tv) (point-y (scroller-delta tv)))
(defun text-left-col (tv) (point-x (scroller-delta tv)))
(defun (setf text-top-line) (v tv) (setf (point-y (scroller-delta tv)) v))
(defun (setf text-left-col) (v tv) (setf (point-x (scroller-delta tv)) v))

(defmethod initialize-instance :after ((tv ttext-view) &key (text ""))
  (setf (view-state tv) (logior (view-state tv) +sf-cursor-vis+))
  (set-text tv text))

(defmethod get-palette ((tv ttext-view)) (make-palette 13 14))  ; input colours

;;; --- line storage ----------------------------------------------------------

(defun %make-line-vector (&optional (initial '("")))
  (let ((v (make-array (max 1 (length initial))
                       :adjustable t :fill-pointer 0)))
    (dolist (s initial) (vector-push-extend s v))
    (when (zerop (fill-pointer v)) (vector-push-extend "" v))
    v))

(defun text-update-limit (tv)
  "Set the scroller's virtual size from the current line widths and count.  In
word-wrap mode the virtual width is just the view width (no horizontal scroll)."
  (let ((maxw 1))
    (if (text-wrap tv)
        (setf maxw (max 1 (point-x (view-size tv))))
        (dotimes (i (fill-pointer (text-lines tv)))
          (setf maxw (max maxw (1+ (length (aref (text-lines tv) i)))))))
    (set-scroller-limit tv maxw (fill-pointer (text-lines tv)))))

;;; word-wrap geometry: a logical line of LEN chars occupies this many visual
;;; rows of width W (room for the cursor one past the end on a full row).
(defun %line-rows (len w) (1+ (floor (max 0 len) (max 1 w))))
(defun %vrows-between (tv top line w)
  "Number of visual rows occupied by logical lines [TOP, LINE)."
  (loop for i from top below line sum (%line-rows (length (nth-line tv i)) w)))

(defun set-text-wrap (tv on)
  "Turn word-wrap on/off and reflow."
  (setf (text-wrap tv) on)
  (when on (setf (text-left-col tv) 0))
  (text-update-limit tv)
  (ensure-visible tv)
  (draw-view tv))

(defun set-text (tv string)
  "Replace the contents of TV with STRING (split on newlines)."
  (set-lines tv (%split-lines string)))

(defun set-lines (tv list-of-strings)
  "Replace the contents of TV with the given lines."
  (setf (text-lines tv) (%make-line-vector (or list-of-strings '("")))
        (text-cur-line tv) 0 (text-cur-col tv) 0
        (text-top-line tv) 0 (text-left-col tv) 0
        (text-anchor tv) nil (text-protect tv) nil
        (text-undo tv) '() (text-redo tv) '()
        (text-modified tv) nil)
  (text-update-limit tv)
  tv)

(defun text-attach-scrollbars (tv &key vscroll hscroll)
  "Bind scroll bars so the editor scrolls with them (and updates them)."
  (attach-scrollbars tv :vscroll vscroll :hscroll hscroll)
  (text-update-limit tv)
  tv)

(defun text-string (tv)
  "Return the full contents of TV as one newline-joined string."
  (with-output-to-string (s)
    (loop for i below (fill-pointer (text-lines tv))
          do (write-string (aref (text-lines tv) i) s)
             (when (< i (1- (fill-pointer (text-lines tv)))) (terpri s)))))

(defun line-count (tv) (fill-pointer (text-lines tv)))
(defun nth-line (tv i) (aref (text-lines tv) i))
(defun current-line-string (tv) (nth-line tv (text-cur-line tv)))

(defun append-text (tv string &key (move-cursor t))
  "Append STRING (which may contain newlines) to the end of the buffer.
Used to stream output, e.g. REPL results."
  (let* ((lines (text-lines tv))
         (parts (%split-lines string))
         (last (1- (fill-pointer lines))))
    ;; first part extends the final existing line
    (setf (aref lines last) (concatenate 'string (aref lines last) (first parts)))
    (dolist (p (rest parts)) (vector-push-extend p lines))
    (when move-cursor
      (setf (text-cur-line tv) (1- (fill-pointer lines))
            (text-cur-col tv) (length (aref lines (1- (fill-pointer lines))))))
    (text-update-limit tv)
    (ensure-visible tv)
    tv))

;;; --- scrolling -------------------------------------------------------------

(defun ensure-visible (tv)
  "Scroll (via the scroller machinery, so bound scroll bars stay in sync) so
that the cursor is on screen."
  (text-update-limit tv)
  (let ((h (point-y (view-size tv)))
        (w (max 1 (point-x (view-size tv)))))
    (if (text-wrap tv)
        ;; vertical scroll is by logical line; keep the cursor's visual row in view
        (let ((top (min (text-top-line tv) (text-cur-line tv))))
          (loop for crow = (+ (%vrows-between tv top (text-cur-line tv) w)
                              (floor (text-cur-col tv) w))
                while (and (>= crow h) (< top (text-cur-line tv)))
                do (incf top))
          (scroll-to tv 0 (max 0 top)))
        (let ((dy (text-top-line tv)) (dx (text-left-col tv)))
          (when (< (text-cur-line tv) dy) (setf dy (text-cur-line tv)))
          (when (>= (text-cur-line tv) (+ dy h)) (setf dy (1+ (- (text-cur-line tv) h))))
          (when (< (text-cur-col tv) dx) (setf dx (text-cur-col tv)))
          (when (>= (text-cur-col tv) (+ dx w)) (setf dx (1+ (- (text-cur-col tv) w))))
          (scroll-to tv (max 0 dx) (max 0 dy))))))

;;; --- positions, selection, protected region --------------------------------

(defun text-pos (tv) (cons (text-cur-line tv) (text-cur-col tv)))
(defun pos< (a b) (or (< (car a) (car b)) (and (= (car a) (car b)) (< (cdr a) (cdr b)))))
(defun pos= (a b) (and (= (car a) (car b)) (= (cdr a) (cdr b))))

(defun selection-range (tv)
  "Return (values start-pos end-pos) for the current selection, normalised so
START precedes END; (values nil nil) when there is no selection."
  (let ((a (text-anchor tv)))
    (if (and a (not (pos= a (text-pos tv))))
        (let ((c (text-pos tv))) (if (pos< a c) (values a c) (values c a)))
        (values nil nil))))

(defun text-substring (tv start end)
  (if (= (car start) (car end))
      (subseq (nth-line tv (car start)) (cdr start) (cdr end))
      (with-output-to-string (o)
        (write-string (subseq (nth-line tv (car start)) (cdr start)) o) (terpri o)
        (loop for li from (1+ (car start)) below (car end)
              do (write-string (nth-line tv li) o) (terpri o))
        (write-string (subseq (nth-line tv (car end)) 0 (cdr end)) o))))

(defun selected-string (tv)
  (multiple-value-bind (s e) (selection-range tv)
    (when s (text-substring tv s e))))

(defun set-protect-boundary (tv &optional (line (text-cur-line tv)) (col (text-cur-col tv)))
  "Mark everything before (LINE,COL) read-only (used to protect REPL output)."
  (setf (text-protect tv) (cons line col)))

(defun pos-protected-p (tv pos)
  (and (text-protect tv) (pos< pos (text-protect tv))))

;;; --- drawing ---------------------------------------------------------------

(defmethod draw ((tv ttext-view))
  (if (text-wrap tv) (%draw-wrapped tv) (%draw-flat tv)))

(defun %draw-flat (tv)
  (let* ((w (point-x (view-size tv)))
         (h (point-y (view-size tv)))
         (c (get-color tv 1))
         (hi (get-color tv 2))
         (dx (text-left-col tv))
         (db (make-draw-buffer w)))
    (multiple-value-bind (sels sele) (selection-range tv)
      (dotimes (row h)
        (db-fill db #\Space c)
        (let ((li (+ (text-top-line tv) row)))
          (when (< li (line-count tv))
            (let* ((line (nth-line tv li))
                   (start (min dx (length line)))
                   (vis (subseq line start (min (length line) (+ start w)))))
              (db-move-str db 0 vis c)
              ;; highlight the selected span on this line
              (when (and sels (<= (car sels) li (car sele)))
                (let* ((hs (if (= li (car sels)) (cdr sels) 0))
                       (he (if (= li (car sele)) (cdr sele) (length line)))
                       (vs (max 0 (- hs dx)))
                       (ve (min w (- he dx))))
                  (when (< vs ve) (db-put-attribute db vs hi (- ve vs)))))))
          (write-line* tv 0 row w 1 db))))
    (when (logtest (view-state tv) +sf-focused+)
      (set-cursor tv (- (text-cur-col tv) (text-left-col tv))
                  (- (text-cur-line tv) (text-top-line tv))))))

(defun %draw-wrapped (tv)
  (let* ((w (max 1 (point-x (view-size tv))))
         (h (point-y (view-size tv)))
         (c (get-color tv 1)) (hi (get-color tv 2))
         (db (make-draw-buffer w))
         (row 0) (li (text-top-line tv)))
    (multiple-value-bind (sels sele) (selection-range tv)
      (loop while (< row h) do
        (cond
          ((< li (line-count tv))
           (let* ((line (nth-line tv li)) (len (length line))
                  (nseg (%line-rows len w)))
             (dotimes (seg nseg)
               (when (>= row h) (return))
               (let* ((start (* seg w)) (end (min len (+ start w))))
                 (db-fill db #\Space c)
                 (when (< start end) (db-move-str db 0 (subseq line start end) c))
                 (when (and sels (<= (car sels) li (car sele)))
                   (let* ((hs (if (= li (car sels)) (cdr sels) 0))
                          (he (if (= li (car sele)) (cdr sele) len))
                          (vs (max 0 (- hs start))) (ve (min w (- he start))))
                     (when (< vs ve) (db-put-attribute db vs hi (- ve vs)))))
                 (write-line* tv 0 row w 1 db))
               (incf row))
             (incf li)))
          (t (db-fill db #\Space c) (write-line* tv 0 row w 1 db) (incf row)))))
    (when (logtest (view-state tv) +sf-focused+)
      (set-cursor tv (mod (text-cur-col tv) w)
                  (+ (%vrows-between tv (text-top-line tv) (text-cur-line tv) w)
                     (floor (text-cur-col tv) w))))))

;;; --- editing primitives ----------------------------------------------------

(defun set-line (tv i s)
  (setf (text-modified tv) t)
  (setf (aref (text-lines tv) i) s))

(defun insert-char-at-cursor (tv ch)
  (let* ((l (current-line-string tv)) (col (text-cur-col tv)))
    (set-line tv (text-cur-line tv)
              (concatenate 'string (subseq l 0 col) (string ch) (subseq l col)))
    (incf (text-cur-col tv))))

(defun delete-char-before-cursor (tv)
  (let ((col (text-cur-col tv)) (li (text-cur-line tv)))
    (cond
      ((> col 0)
       (let ((l (current-line-string tv)))
         (set-line tv li (concatenate 'string (subseq l 0 (1- col)) (subseq l col)))
         (decf (text-cur-col tv))))
      ((> li 0)
       ;; join with previous line
       (let* ((prev (nth-line tv (1- li))) (cur (current-line-string tv)))
         (setf (text-cur-col tv) (length prev))
         (set-line tv (1- li) (concatenate 'string prev cur))
         (remove-line tv li)
         (decf (text-cur-line tv)))))))

(defun delete-char-at-cursor (tv)
  (let* ((l (current-line-string tv)) (col (text-cur-col tv)) (li (text-cur-line tv)))
    (cond
      ((< col (length l))
       (set-line tv li (concatenate 'string (subseq l 0 col) (subseq l (1+ col)))))
      ((< li (1- (line-count tv)))
       (set-line tv li (concatenate 'string l (nth-line tv (1+ li))))
       (remove-line tv (1+ li))))))

(defun remove-line (tv i)
  (setf (text-modified tv) t)
  (let ((lines (text-lines tv)))
    (loop for k from i below (1- (fill-pointer lines))
          do (setf (aref lines k) (aref lines (1+ k))))
    (decf (fill-pointer lines))
    (when (zerop (fill-pointer lines)) (vector-push-extend "" lines))))

(defun split-line-at-cursor (tv)
  "Break the current line at the cursor, creating a new line (Enter default)."
  (let* ((lines (text-lines tv)) (li (text-cur-line tv))
         (l (current-line-string tv)) (col (text-cur-col tv)))
    (set-line tv li (subseq l 0 col))
    ;; make room and insert the tail as a new line
    (vector-push-extend "" lines)
    (loop for k from (1- (fill-pointer lines)) above (1+ li)
          do (setf (aref lines k) (aref lines (1- k))))
    (setf (aref lines (1+ li)) (subseq l col))
    (incf (text-cur-line tv))
    (setf (text-cur-col tv) 0)))

(defun insert-string (tv str)
  "Insert STR (which may contain newlines) at the cursor."
  (loop for ch across str
        do (if (char= ch #\Newline)
               (split-line-at-cursor tv)
               (insert-char-at-cursor tv ch))))

;;; --- undo / clipboard / selection edits ------------------------------------

(defun %capture-state (tv)
  (list (subseq (text-lines tv) 0 (line-count tv))
        (text-cur-line tv) (text-cur-col tv) (text-protect tv) (text-modified tv)))

(defun %restore-state (tv state)
  (destructuring-bind (lines cl cc prot mod) state
    (let ((v (make-array (length lines) :adjustable t :fill-pointer (length lines))))
      (dotimes (i (length lines)) (setf (aref v i) (aref lines i)))
      (setf (text-lines tv) v
            (text-cur-line tv) cl (text-cur-col tv) cc
            (text-protect tv) prot (text-anchor tv) nil (text-modified tv) mod)))
  (text-update-limit tv))

(defun text-snapshot (tv)
  "Record the current state for undo; a fresh edit invalidates the redo stack."
  (push (%capture-state tv) (text-undo tv))
  (setf (text-redo tv) '())
  (when (> (length (text-undo tv)) 500)
    (setf (text-undo tv) (subseq (text-undo tv) 0 500))))

(defun text-undo! (tv)
  (when (text-undo tv)
    (push (%capture-state tv) (text-redo tv))
    (%restore-state tv (pop (text-undo tv)))))

(defun text-redo! (tv)
  (when (text-redo tv)
    (push (%capture-state tv) (text-undo tv))
    (%restore-state tv (pop (text-redo tv)))))

(defun delete-selection (tv)
  "Delete the selected text (respecting the protected region)."
  (multiple-value-bind (s e) (selection-range tv)
    (when (and s (not (pos-protected-p tv s)))
      (let* ((sl (car s)) (sc (cdr s)) (el (car e)) (ec (cdr e))
             (head (subseq (nth-line tv sl) 0 sc))
             (tail (subseq (nth-line tv el) ec)))
        (set-line tv sl (concatenate 'string head tail))
        (loop repeat (- el sl) do (remove-line tv (1+ sl)))
        (setf (text-cur-line tv) sl (text-cur-col tv) sc)))
    (setf (text-anchor tv) nil)))

(defun copy-selection (tv)
  (let ((s (selected-string tv))) (when s (setf *clipboard* s) t)))

(defun cut-selection (tv)
  (when (selected-string tv)
    (text-snapshot tv) (copy-selection tv) (delete-selection tv) t))

(defun paste-clipboard (tv)
  (when (and (plusp (length *clipboard*)) (not (pos-protected-p tv (text-pos tv))))
    (text-snapshot tv)
    (when (text-anchor tv) (delete-selection tv))
    (insert-string tv *clipboard*) t))

;;; --- the overridable Enter hook --------------------------------------------

(defgeneric text-return (tv)
  (:documentation "Handle the Enter key.  The default inserts a newline; a REPL
subclass overrides this to evaluate the current input instead.")
  (:method ((tv ttext-view)) (split-line-at-cursor tv)))

;;; --- events ----------------------------------------------------------------

(defun clamp-cursor (tv)
  (setf (text-cur-line tv) (min (max 0 (text-cur-line tv)) (1- (line-count tv)))
        (text-cur-col tv)  (min (max 0 (text-cur-col tv))
                                (length (current-line-string tv)))))

(defun %mouse-to-cursor (tv event)
  "Move the cursor to the click/drag position of EVENT."
  (let* ((lp (make-local tv (event-mouse-where event)))
         (mx (max 0 (point-x lp))) (my (max 0 (point-y lp))))
    (if (text-wrap tv)
        (let ((w (max 1 (point-x (view-size tv)))) (li (text-top-line tv)) (acc 0) (done nil))
          (loop while (and (not done) (< li (line-count tv))) do
            (let ((nseg (%line-rows (length (nth-line tv li)) w)))
              (if (< my (+ acc nseg))
                  (progn
                    (setf (text-cur-line tv) li
                          (text-cur-col tv) (+ (* (- my acc) w) mx)
                          done t))
                  (progn (incf acc nseg) (incf li)))))
          (unless done
            (setf (text-cur-line tv) (1- (line-count tv))
                  (text-cur-col tv) (length (nth-line tv (1- (line-count tv)))))))
        (setf (text-cur-line tv) (min (1- (line-count tv)) (+ (text-top-line tv) my))
              (text-cur-col tv) (+ (text-left-col tv) mx)))
    (clamp-cursor tv)))

(defun %wrap-vmove (tv dir)
  "Move the cursor one VISUAL row (DIR -1 up / +1 down) in word-wrap mode,
keeping the goal visual column across the move."
  (let* ((w (max 1 (point-x (view-size tv))))
         (len (length (current-line-string tv)))
         (vseg (floor (text-cur-col tv) w))
         (goal (or (text-goal-col tv) (mod (text-cur-col tv) w))))
    (setf (text-goal-col tv) goal)
    (if (plusp dir)
        (if (< vseg (1- (%line-rows len w)))                 ; another segment below
            (setf (text-cur-col tv) (min len (+ (* (1+ vseg) w) goal)))
            (when (< (text-cur-line tv) (1- (line-count tv))) ; -> next logical line
              (incf (text-cur-line tv))
              (setf (text-cur-col tv) (min (length (current-line-string tv)) goal))))
        (if (> vseg 0)                                        ; another segment above
            (setf (text-cur-col tv) (min len (+ (* (1- vseg) w) goal)))
            (when (> (text-cur-line tv) 0)                    ; -> prev line's last segment
              (decf (text-cur-line tv))
              (let* ((plen (length (current-line-string tv)))
                     (last-seg (1- (%line-rows plen w))))
                (setf (text-cur-col tv) (min plen (+ (* last-seg w) goal)))))))))

(defun %move-cursor (tv k)
  "Apply a navigation key K to the cursor (no selection / redraw side effects)."
  (cond
    ((= k +kb-up+)    (if (text-wrap tv) (%wrap-vmove tv -1) (decf (text-cur-line tv))))
    ((= k +kb-down+)  (if (text-wrap tv) (%wrap-vmove tv +1) (incf (text-cur-line tv))))
    ((= k +kb-left+)  (if (> (text-cur-col tv) 0) (decf (text-cur-col tv))
                          (when (> (text-cur-line tv) 0)
                            (decf (text-cur-line tv))
                            (setf (text-cur-col tv) (length (current-line-string tv))))))
    ((= k +kb-right+) (if (< (text-cur-col tv) (length (current-line-string tv)))
                          (incf (text-cur-col tv))
                          (when (< (text-cur-line tv) (1- (line-count tv)))
                            (incf (text-cur-line tv)) (setf (text-cur-col tv) 0))))
    ((= k +kb-home+)  (setf (text-cur-col tv) 0))
    ((= k +kb-end+)   (setf (text-cur-col tv) (length (current-line-string tv))))
    ((= k +kb-pgup+)  (decf (text-cur-line tv) (max 1 (1- (point-y (view-size tv))))))
    ((= k +kb-pgdn+)  (incf (text-cur-line tv) (max 1 (1- (point-y (view-size tv))))))))

(defun navigation-key-p (k)
  (member k (list +kb-up+ +kb-down+ +kb-left+ +kb-right+
                  +kb-home+ +kb-end+ +kb-pgup+ +kb-pgdn+)))

(defun %word-char-p (ch) (or (alphanumericp ch) (char= ch #\_)))

(defun word-left (tv)
  "Move the cursor to the start of the previous word."
  (let ((line (current-line-string tv)) (col (text-cur-col tv)))
    (cond
      ((zerop col) (%move-cursor tv +kb-left+))   ; wrap to end of previous line
      (t (decf col)
         (loop while (and (> col 0) (not (%word-char-p (char line col)))) do (decf col))
         (loop while (and (> col 0) (%word-char-p (char line (1- col)))) do (decf col))
         (setf (text-cur-col tv) col)))))

(defun word-right (tv)
  "Move the cursor to the start of the next word."
  (let* ((line (current-line-string tv)) (len (length line)) (col (text-cur-col tv)))
    (cond
      ((>= col len) (%move-cursor tv +kb-right+))  ; wrap to next line
      (t (loop while (and (< col len) (%word-char-p (char line col))) do (incf col))
         (loop while (and (< col len) (not (%word-char-p (char line col)))) do (incf col))
         (setf (text-cur-col tv) col)))))

(defun text-goto (tv line &optional (col 0))
  "Move the cursor to (LINE, COL) (1-based LINE for the public API), redraw."
  (setf (text-anchor tv) nil
        (text-cur-line tv) (max 0 (min (1- line) (1- (line-count tv))))
        (text-cur-col tv) col)
  (clamp-cursor tv) (ensure-visible tv) (draw-view tv))

(defun can-edit-here-p (tv &optional (delete-before nil))
  "Whether an edit at the cursor is allowed given the protected region."
  (if (text-protect tv)
      (if delete-before
          (pos< (text-protect tv) (text-pos tv))   ; need cursor strictly past boundary
          (not (pos< (text-pos tv) (text-protect tv))))
      t))

(defmethod handle-event ((tv ttext-view) event)
  (cond
    ;; a bound scroll bar moved: follow it (without moving the text cursor)
    ((scrollbar-event-p tv event)
     (scroll-from-scrollbars tv))
    ((and (= (event-type event) +ev-mouse-wheel+) (mouse-in-view-p tv event))
     (scroll-to tv (point-x (scroller-delta tv))
                (+ (point-y (scroller-delta tv)) (* 3 (event-wheel event))))
     (clear-event event))
    ;; mouse down: position the cursor and begin a (possible) selection
    ((and (= (event-type event) +ev-mouse-down+) (mouse-in-view-p tv event))
     (setf (text-goal-col tv) nil)
     (%mouse-to-cursor tv event)
     (setf (text-anchor tv) (text-pos tv))   ; drag from here; click w/o drag = no sel
     (ensure-visible tv) (draw-view tv)
     (clear-event event))
    ;; mouse drag: extend the selection to the pointer
    ((= (event-type event) +ev-mouse-move+)
     (when (text-anchor tv)
       (%mouse-to-cursor tv event)
       (ensure-visible tv) (draw-view tv))
     (clear-event event))
    ;; mouse up: a click with no drag leaves no selection
    ((= (event-type event) +ev-mouse-up+)
     (when (and (text-anchor tv) (pos= (text-anchor tv) (text-pos tv)))
       (setf (text-anchor tv) nil) (draw-view tv))
     (clear-event event))
    ((and (= (event-type event) +ev-key-down+)
          (logtest (view-state tv) +sf-focused+))
     (let* ((k (event-key-code event)) (ch (event-char-code event))
            (mods (event-modifiers event))
            (ctrl (logtest mods +md-ctrl+))
            (ctrl-letter (and ctrl (<= 1 ch 26) (code-char (+ ch 96))))
            (handled t))
       ;; Up/Down keep the goal visual column; everything else resets it.
       (unless (or (= k +kb-up+) (= k +kb-down+)) (setf (text-goal-col tv) nil))
       (cond
         ;; clipboard / undo
         ((eql ctrl-letter #\c) (copy-selection tv))
         ((eql ctrl-letter #\x) (if (text-read-only tv) (setf handled nil) (cut-selection tv)))
         ((eql ctrl-letter #\v) (if (text-read-only tv) (setf handled nil) (paste-clipboard tv)))
         ((eql ctrl-letter #\z) (if (text-read-only tv) (setf handled nil) (text-undo! tv)))
         ((eql ctrl-letter #\y) (if (text-read-only tv) (setf handled nil) (text-redo! tv)))
         ((eql ctrl-letter #\a)             ; select all
          (setf (text-anchor tv) (cons 0 0)
                (text-cur-line tv) (1- (line-count tv))
                (text-cur-col tv) (length (nth-line tv (1- (line-count tv))))))
         ;; Insert toggles overwrite mode (block cursor)
         ((= k +kb-ins+)
          (setf (text-overwrite tv) (not (text-overwrite tv)))
          (if (text-overwrite tv) (block-cursor tv) (normal-cursor tv)))
         ;; Ctrl+Left/Right move by word; plain arrows move by char
         ((and ctrl (or (= k +kb-left+) (= k +kb-right+)))
          (if (logtest mods +md-shift+)
              (unless (text-anchor tv) (setf (text-anchor tv) (text-pos tv)))
              (setf (text-anchor tv) nil))
          (if (= k +kb-left+) (word-left tv) (word-right tv)))
         ;; navigation (Shift extends the selection)
         ((navigation-key-p k)
          (if (logtest mods +md-shift+)
              (unless (text-anchor tv) (setf (text-anchor tv) (text-pos tv)))
              (setf (text-anchor tv) nil))
          (%move-cursor tv k))
         ;; Tab inserts spaces (to the next multiple of 4)
         ((= k +kb-tab+)
          (if (or (text-read-only tv) (not (can-edit-here-p tv)))
              (setf handled nil)
              (progn (text-snapshot tv)
                     (when (text-anchor tv) (delete-selection tv))
                     (dotimes (_ (- 4 (mod (text-cur-col tv) 4)))
                       (insert-char-at-cursor tv #\Space)))))
         ((= k +kb-enter+)
          (if (text-read-only tv) (setf handled nil)
              (progn (text-snapshot tv)
                     (when (text-anchor tv) (delete-selection tv))
                     (text-return tv))))
         ((= k +kb-back+)
          (cond ((text-read-only tv) (setf handled nil))
                ((text-anchor tv) (text-snapshot tv) (delete-selection tv))
                ((can-edit-here-p tv t) (text-snapshot tv) (delete-char-before-cursor tv))
                (t (setf handled nil))))
         ((= k +kb-del+)
          (cond ((text-read-only tv) (setf handled nil))
                ((text-anchor tv) (text-snapshot tv) (delete-selection tv))
                ((can-edit-here-p tv) (text-snapshot tv) (delete-char-at-cursor tv))
                (t (setf handled nil))))
         ((and (>= ch 32) (< ch 127) (not ctrl))
          (cond ((text-read-only tv) (setf handled nil))
                ((not (can-edit-here-p tv)) (setf handled nil))
                (t (text-snapshot tv)
                   (cond
                     ((text-anchor tv) (delete-selection tv))
                     ;; overwrite mode: replace the char under the cursor
                     ((and (text-overwrite tv)
                           (< (text-cur-col tv) (length (current-line-string tv))))
                      (delete-char-at-cursor tv)))
                   (insert-char-at-cursor tv (code-char ch)))))
         (t (setf handled nil)))
       (when handled
         (clamp-cursor tv) (ensure-visible tv) (draw-view tv)
         (clear-event event))))))

(defmethod data-size ((tv ttext-view)) (length (text-string tv)))
(defmethod get-data ((tv ttext-view)) (text-string tv))
(defmethod set-data ((tv ttext-view) data) (set-text tv (princ-to-string data)))

;;; --- search ----------------------------------------------------------------

(defun %match-at-p (line pattern pos test)
  (and (<= (+ pos (length pattern)) (length line))
       (loop for i below (length pattern)
             always (funcall test (char pattern i) (char line (+ pos i))))))

(defun %word-match-p (line pos len)
  "True when the LEN-char span at POS is bounded by non-word chars (or edges)."
  (and (or (zerop pos) (not (%word-char-p (char line (1- pos)))))
       (or (>= (+ pos len) (length line)) (not (%word-char-p (char line (+ pos len)))))))

(defun %line-find-fwd (line pattern start test whole-word)
  (loop with len = (length pattern)
        for p from (max 0 start) to (- (length line) len)
        when (and (%match-at-p line pattern p test)
                  (or (not whole-word) (%word-match-p line p len)))
        do (return p)))

(defun %line-find-back (line pattern before test whole-word)
  "Largest match position strictly before column BEFORE."
  (loop with len = (length pattern)
        for p from (min (1- before) (- (length line) len)) downto 0
        when (and (%match-at-p line pattern p test)
                  (or (not whole-word) (%word-match-p line p len)))
        do (return p)))

(defun text-find (tv pattern &key from-line from-col case-sensitive backward whole-word)
  "Search for PATTERN from (FROM-LINE,FROM-COL) (default: cursor).  When BACKWARD,
search toward the start.  Return (cons line col) of the match, or NIL."
  (when (plusp (length pattern))
    (let ((test (if case-sensitive #'char= #'char-equal))
          (fl (or from-line (text-cur-line tv)))
          (fc (or from-col (text-cur-col tv))))
      (if backward
          (loop for li from (min fl (1- (line-count tv))) downto 0
                for line = (nth-line tv li)
                for before = (if (= li fl) fc (1+ (length line)))
                for p = (%line-find-back line pattern before test whole-word)
                when p do (return (cons li p)))
          (loop for li from fl below (line-count tv)
                for line = (nth-line tv li)
                for start = (if (= li fl) fc 0)
                for p = (%line-find-fwd line pattern start test whole-word)
                when p do (return (cons li p)))))))

(defun text-select-match (tv pos pattern)
  "Select the PATTERN-length span at POS and put the cursor at its end."
  (setf (text-anchor tv) (copy-tree pos)
        (text-cur-line tv) (car pos)
        (text-cur-col tv) (+ (cdr pos) (length pattern)))
  (ensure-visible tv) (draw-view tv))

(defun text-find-and-select (tv pattern &key case-sensitive whole-word backward wrap)
  "Find the next (or previous) PATTERN and select it.  Return T on success."
  (let* ((from (if (and backward (text-anchor tv)) (text-anchor tv) (text-pos tv)))
         (m (or (text-find tv pattern :from-line (car from) :from-col (cdr from)
                           :case-sensitive case-sensitive :whole-word whole-word
                           :backward backward)
                (and wrap
                     (let ((last (1- (line-count tv))))
                       (text-find tv pattern
                                  :from-line (if backward last 0)
                                  :from-col (if backward (1+ (length (nth-line tv last))) 0)
                                  :case-sensitive case-sensitive :whole-word whole-word
                                  :backward backward))))))
    (when m (text-select-match tv m pattern) t)))

(defun text-replace-selection (tv string)
  "Replace the current selection (if any) with STRING; cursor ends after it."
  (text-snapshot tv)
  (when (text-anchor tv) (delete-selection tv))
  (insert-string tv string)
  (ensure-visible tv) (draw-view tv))

(defun text-replace-all (tv from to &key case-sensitive whole-word)
  "Replace every occurrence of FROM with TO (within lines).  Return the count.
Undoable (one snapshot)."
  (if (zerop (length from))
      0
      (let ((test (if case-sensitive #'char= #'char-equal)) (len (length from)) (count 0))
        (text-snapshot tv)
        (dotimes (li (line-count tv))
          (let ((line (nth-line tv li)) (i 0) (changed nil)
                (out (make-string-output-stream)))
            (loop
              (let ((p (%line-find-fwd line from i test whole-word)))
                (if p
                    (progn (write-string (subseq line i p) out) (write-string to out)
                           (setf i (+ p len) changed t) (incf count))
                    (progn (write-string (subseq line i) out) (return)))))
            (when changed (set-line tv li (get-output-stream-string out)))))
        (when (plusp count) (text-update-limit tv) (clamp-cursor tv) (draw-view tv))
        count)))

;;; --- file I/O ---------------------------------------------------------------

(defun directory-pathname-p (path)
  "True when PATH names an existing directory."
  (let ((tn (ignore-errors (truename path))))
    (and tn (null (pathname-name tn)) (null (pathname-type tn)))))

(defun text-load-file (tv path)
  "Replace the buffer with the contents of PATH.  Return T on success, or NIL
if the file is missing, a directory, or unreadable (never errors)."
  (when (and (probe-file path) (not (directory-pathname-p path)))
    (handler-case
        (with-open-file (s path)
          (let ((lines '()))
            (loop for line = (read-line s nil :eof) until (eq line :eof)
                  do (push line lines))
            (set-lines tv (nreverse lines))
            t))
      (error () nil))))

(defun text-save-file (tv path)
  "Write the buffer to PATH and clear the modified flag.  Return PATH."
  (with-open-file (s path :direction :output :if-exists :supersede
                          :if-does-not-exist :create)
    (dotimes (i (line-count tv))
      (write-line (nth-line tv i) s)))
  (setf (text-modified tv) nil)
  path)

;;; ===========================================================================
;;; TIndicator -- a small status gadget (line:col, modified, INS/OVR).
;;; ===========================================================================

(defclass tindicator (tview)
  ((source :initarg :source :initform nil :accessor indicator-source)))

(defmethod initialize-instance :after ((ind tindicator) &key)
  (setf (view-grow-mode ind) (logior +gf-grow-loy+ +gf-grow-hiy+)))

(defmethod get-palette ((ind tindicator)) (make-palette 2 3))  ; active frame colours

(defmethod draw ((ind tindicator))
  (let* ((src (indicator-source ind)) (w (point-x (view-size ind)))
         (c (get-color ind 1)) (db (make-draw-buffer w)))
    (db-fill db #\Space c)
    (when src
      (db-move-str db 0
                   (format nil " ~d:~d~a ~a "
                           (1+ (text-cur-line src)) (1+ (text-cur-col src))
                           (if (text-modified src) " *" "")
                           (if (text-overwrite src) "OVR" "INS"))
                   c))
    (write-line* ind 0 0 w 1 db)))

;;; --- TMemo: a multi-line edit control for use inside dialogs ----------------
;;; TEditor in a window is TFileEditor; TMemo is the same editor engine used as a
;;; bounded dialog control whose get-data/set-data is the whole text string.

(defclass tmemo (ttext-view) ()
  (:documentation "A multi-line editable text control for dialogs (the in-dialog
counterpart of the windowed editor).  Its data is the whole text string."))

;;; --- TFileEditor / TEditWindow: the windowed editor classes ----------------

(defclass tfile-editor (ttext-view)
  ((filename :initarg :filename :initform nil :accessor editor-filename))
  (:documentation "An editor bound to a file (TEditor + a filename)."))

(defclass teditor-window (twindow)
  ((editor :initform nil :accessor editor-window-editor))
  (:documentation "A window framing a TFileEditor with a scroll bar + indicator."))

(defun make-edit-window (bounds &key (title "Editor") filename)
  "Build a TEditWindow: a window containing a TFileEditor, a vertical scroll bar
and a position indicator.  Loads FILENAME if it exists.  Returns (values window
editor)."
  (let* ((w (make-instance 'teditor-window :title title :bounds bounds))
         (iw (point-x (view-size w))) (ih (point-y (view-size w)))
         (vsb (standard-scrollbar w t))
         (ed (make-instance 'tfile-editor :filename filename
                            :bounds (make-trect 1 1 (1- iw) (- ih 2))))
         (ind (make-instance 'tindicator :source ed
                             :bounds (make-trect 2 (1- ih) 18 ih))))
    (insert w ed)
    (insert w ind)
    (text-attach-scrollbars ed :vscroll vsb)
    (when (and filename (probe-file filename))
      (ignore-errors (text-load-file ed filename)))
    (setf (editor-window-editor w) ed)
    (focus ed)
    (values w ed)))
