;;;; editor.lisp --- a multi-line text-editing widget + window, ported onto tv2.
;;;;
;;;; The "engine" window: tvision's TTEXT-VIEW is ~1100 lines (wrap, regex
;;;; find/replace, mouse, syntax colour, file I/O).  This port rebuilds the core
;;;; editing experience as a tv2 leaf widget -- a vector-of-lines model with a
;;;; cursor, viewport scrolling, Shift-arrow selection, an internal clipboard,
;;;; snapshot undo/redo, and file load/save -- dispatching keys directly the way
;;;; INPUT-LINE/LIST-BOX do.  (Deferred from the full engine: word-wrap, syntax
;;;; highlighting, and regex search/replace.)

(in-package #:tv2)

(defvar *clipboard* "" "Shared cut/copy buffer for TEXT-EDIT widgets.")

(defclass text-edit (view)
  ((lines    :initform (%vec '("")) :accessor te-lines)   ; adjustable vector of line strings
   (cy       :initform 0 :accessor te-cy)                 ; cursor line / column
   (cx       :initform 0 :accessor te-cx)
   (top      :initform 0 :accessor te-top)                ; first visible line / column
   (left     :initform 0 :accessor te-left)
   (wrap     :initform nil :initarg :wrap :accessor te-wrap)   ; soft-wrap mode
   (tsub     :initform 0 :accessor te-tsub)                ; sub-row within TOP when wrapping
   (anchor   :initform nil :accessor te-anchor)           ; selection origin (cons line col) or NIL
   (modified :initform nil :accessor te-modified)
   (filename :initform nil :initarg :filename :accessor te-filename)
   (undo     :initform '() :accessor te-undo)             ; snapshots: (lines-list cy cx)
   (redo     :initform '() :accessor te-redo)
   (colorizer :initform nil :initarg :colorizer :accessor te-colorizer)   ; (line in-string) -> (values attrs end)
   (indenter  :initform nil :initarg :indenter  :accessor te-indenter)    ; (te) -> indent column for a new line
   (last-find :initform "" :accessor te-last-find))                       ; remembered search query
  (:metaclass reactive-class))

(defmethod focusable-p ((te text-edit)) t)

;;; --- model helpers ----------------------------------------------------------

(defun %vec (list) (make-array (length list) :adjustable t :fill-pointer (length list)
                               :initial-contents list))
(defun te-nlines (te) (length (te-lines te)))
(defun te-line (te i) (aref (te-lines te) i))
(defun te-cur (te) (te-line te (te-cy te)))
(defun (setf te-line) (s te i) (setf (aref (te-lines te) i) s))

(defun split-newlines (s)
  "Split S on newlines into a list of segments (always at least one element)."
  (let ((out '()) (start 0))
    (loop for nl = (position #\Newline s :start start)
          while nl do (push (subseq s start nl) out) (setf start (1+ nl)))
    (push (subseq s start) out)
    (nreverse out)))

(defun te-text (te)
  (with-output-to-string (o)
    (dotimes (i (te-nlines te))
      (write-string (te-line te i) o)
      (when (< (1+ i) (te-nlines te)) (write-char #\Newline o)))))

(defun te-set-text (te string)
  (setf (te-lines te) (%vec (split-newlines (or string "")))
        (te-cy te) 0 (te-cx te) 0 (te-top te) 0 (te-left te) 0 (te-anchor te) nil))

(defun shift-p (e) (logtest (event-modifiers e) tvision::+md-shift+))

;;; --- cursor + viewport ------------------------------------------------------

(defun te-clamp (te)
  (setf (te-cy te) (max 0 (min (1- (te-nlines te)) (te-cy te)))
        (te-cx te) (max 0 (min (length (te-cur te)) (te-cx te)))))

;;; visual-row model for soft-wrap: a logical line of length LEN occupies
;;; (1+ floor(LEN/W)) visual rows; column C sits on sub-row floor(C/W), col mod(C/W).
(defun te-vw (te) (max 1 (r-w (view-bounds te))))
(defun %segs (len w) (max 1 (1+ (floor (max 0 len) w))))
(defun te-cum-vrows (te line w)               ; visual rows occupied by lines [0, LINE)
  (loop for i below line sum (%segs (length (te-line te i)) w)))
(defun te-vrow->pos (te v w)                  ; absolute visual row -> (values line sub)
  (let ((line 0))
    (loop for n = (%segs (length (te-line te line)) w)
          while (and (>= v n) (< line (1- (te-nlines te))))
          do (decf v n) (incf line))
    (values line (min v (1- (%segs (length (te-line te line)) w))))))

(defun te-ensure-visible (te)
  (let ((b (view-bounds te)))
    (when b
      (if (te-wrap te)
          (let* ((w (te-vw te)) (h (r-h b))
                 (curv (+ (te-cum-vrows te (te-cy te) w) (floor (te-cx te) w)))
                 (topv (+ (te-cum-vrows te (te-top te) w) (te-tsub te))))
            (cond ((< curv topv) (setf topv curv))
                  ((>= curv (+ topv h)) (setf topv (1+ (- curv h)))))
            (multiple-value-bind (l s) (te-vrow->pos te (max 0 topv) w)
              (setf (te-top te) l (te-tsub te) s (te-left te) 0)))
          (let ((h (r-h b)) (w (r-w b)))
            (cond ((< (te-cy te) (te-top te)) (setf (te-top te) (te-cy te)))
                  ((>= (te-cy te) (+ (te-top te) h)) (setf (te-top te) (1+ (- (te-cy te) h)))))
            (cond ((< (te-cx te) (te-left te)) (setf (te-left te) (te-cx te)))
                  ((>= (te-cx te) (+ (te-left te) w)) (setf (te-left te) (1+ (- (te-cx te) w))))))))))

(defun te-vmove (te dir)
  "Move the cursor one visual row (DIR -1/+1) in wrap mode, crossing sub-rows
within a long line and spilling onto the next/previous logical line at the same
visual column."
  (let* ((w (te-vw te)) (seg (floor (te-cx te) w)) (vcol (mod (te-cx te) w)))
    (if (plusp dir)
        (if (< seg (1- (%segs (length (te-cur te)) w)))
            (setf (te-cx te) (min (length (te-cur te)) (+ (* (1+ seg) w) vcol)))
            (when (< (te-cy te) (1- (te-nlines te)))
              (incf (te-cy te)) (setf (te-cx te) (min (length (te-cur te)) vcol))))
        (if (plusp seg)
            (setf (te-cx te) (+ (* (1- seg) w) vcol))
            (when (plusp (te-cy te))
              (decf (te-cy te))
              (let ((segc (%segs (length (te-cur te)) w)))
                (setf (te-cx te) (min (length (te-cur te)) (+ (* (1- segc) w) vcol)))))))))

;;; --- selection --------------------------------------------------------------

(defun te-sel-ordered (te)
  "Return (values START END) as ordered (line . col) conses, or NIL when no
selection / an empty selection."
  (let ((a (te-anchor te)) (b (cons (te-cy te) (te-cx te))))
    (when (and a (not (equal a b)))
      (if (or (< (car a) (car b)) (and (= (car a) (car b)) (< (cdr a) (cdr b))))
          (values a b) (values b a)))))

(defun te-mark (te e)
  "Begin/extend selection on a Shift-move; collapse it on a plain move."
  (if (shift-p e)
      (unless (te-anchor te) (setf (te-anchor te) (cons (te-cy te) (te-cx te))))
      (setf (te-anchor te) nil)))

(defun te-selected-p (te line col)
  (multiple-value-bind (a b) (te-sel-ordered te)
    (and a
         (cond ((< line (car a)) nil)
               ((> line (car b)) nil)
               ((= (car a) (car b)) (and (= line (car a)) (<= (cdr a) col) (< col (cdr b))))
               ((= line (car a)) (>= col (cdr a)))
               ((= line (car b)) (< col (cdr b)))
               (t t)))))

(defun te-selected-string (te)
  (multiple-value-bind (a b) (te-sel-ordered te)
    (when a
      (if (= (car a) (car b))
          (subseq (te-line te (car a)) (cdr a) (cdr b))
          (with-output-to-string (o)
            (write-string (subseq (te-line te (car a)) (cdr a)) o) (write-char #\Newline o)
            (loop for l from (1+ (car a)) below (car b)
                  do (write-string (te-line te l) o) (write-char #\Newline o))
            (write-string (subseq (te-line te (car b)) 0 (cdr b)) o))))))

;;; --- edits (all snapshot undo first) ----------------------------------------

(defun te-snapshot (te) (list (coerce (te-lines te) 'list) (te-cy te) (te-cx te)))
(defun te-save-undo (te)
  (push (te-snapshot te) (te-undo te))
  (when (> (length (te-undo te)) 200) (setf (te-undo te) (subseq (te-undo te) 0 200)))
  (setf (te-redo te) '() (te-modified te) t))

(defun te-splice (te l0 l1 newlines)
  "Replace lines L0..L1 (inclusive) with the list NEWLINES."
  (let ((result '()))
    (dotimes (i l0) (push (te-line te i) result))
    (dolist (s newlines) (push s result))
    (loop for i from (1+ l1) below (te-nlines te) do (push (te-line te i) result))
    (setf (te-lines te) (%vec (or (nreverse result) '(""))))))

(defun te-replace-region (te l0 c0 l1 c1 newtext)
  "Replace the region [(L0,C0)..(L1,C1)) with NEWTEXT; leave the cursor at its end."
  (let* ((head (subseq (te-line te l0) 0 c0))
         (tail (subseq (te-line te l1) c1))
         (segs (split-newlines newtext))
         (first* (concatenate 'string head (first segs))))
    (if (= (length segs) 1)
        (progn (te-splice te l0 l1 (list (concatenate 'string first* tail)))
               (setf (te-cy te) l0 (te-cx te) (length first*)))
        (let ((last* (concatenate 'string (car (last segs)) tail)))
          (te-splice te l0 l1 (append (list first*) (subseq segs 1 (1- (length segs))) (list last*)))
          (setf (te-cy te) (+ l0 (1- (length segs))) (te-cx te) (length (car (last segs))))))
    (setf (te-anchor te) nil)))

(defun te-delete-selection (te)
  "Delete the active selection (if any); return T when something was deleted."
  (multiple-value-bind (a b) (te-sel-ordered te)
    (when a (te-replace-region te (car a) (cdr a) (car b) (cdr b) "") t)))

(defun te-insert (te string)
  (te-save-undo te)
  (or (te-delete-selection te) (setf (te-anchor te) nil))
  (te-replace-region te (te-cy te) (te-cx te) (te-cy te) (te-cx te) string))

(defun te-insert-char (te ch)
  (te-save-undo te)
  (when (te-sel-ordered te) (te-delete-selection te))
  (setf (te-anchor te) nil)
  (let ((l (te-cur te)) (c (te-cx te)))
    (setf (te-line te (te-cy te)) (concatenate 'string (subseq l 0 c) (string ch) (subseq l c))
          (te-cx te) (1+ c))))

(defun te-newline (te)
  (te-save-undo te)
  (when (te-sel-ordered te) (te-delete-selection te))
  (te-replace-region te (te-cy te) (te-cx te) (te-cy te) (te-cx te) (string #\Newline))
  (when (te-indenter te)                                ; auto-indent the fresh line
    (let ((n (funcall (te-indenter te) te)))
      (when (and n (plusp n))
        (setf (te-line te (te-cy te)) (concatenate 'string (make-string n :initial-element #\Space) (te-cur te))
              (te-cx te) n)))))

(defun lisp-auto-indent (te)
  "Indent a new line to match the line above, +2 when it has unbalanced opens."
  (let ((py (1- (te-cy te))))
    (if (minusp py) 0
        (let* ((prev (te-line te py))
               (lead (or (position #\Space prev :test-not #'eql) (length prev)))
               (net  (- (count #\( prev) (count #\) prev))))
          (max 0 (+ lead (if (plusp net) 2 0)))))))

;;; --- search -----------------------------------------------------------------

(defun te-find (te query &key (from-line (te-cy te)) (from-col 0))
  "Find QUERY (case-insensitive) at/after (FROM-LINE, FROM-COL); select it and
move the cursor to its end.  Return T on a hit."
  (when (plusp (length query))
    (setf (te-last-find te) query)
    (let ((q (string-downcase query)))
      (loop for ln from from-line below (te-nlines te)
            for line = (string-downcase (te-line te ln))
            for start = (if (= ln from-line) (min from-col (length line)) 0)
            for pos = (search q line :start2 start)
            when pos do
              (setf (te-cy te) ln (te-cx te) (+ pos (length q)) (te-anchor te) (cons ln pos))
              (te-ensure-visible te)
              (return-from te-find t)))
    nil))

(defun te-find-next (te)
  (when (plusp (length (te-last-find te)))
    (or (te-find te (te-last-find te) :from-line (te-cy te) :from-col (te-cx te))
        (te-find te (te-last-find te) :from-line 0 :from-col 0))))   ; wrap to top

(defun te-backspace (te)
  (cond ((te-sel-ordered te) (te-save-undo te) (te-delete-selection te))
        ((plusp (te-cx te))
         (te-save-undo te)
         (let ((l (te-cur te)) (c (te-cx te)))
           (setf (te-line te (te-cy te)) (concatenate 'string (subseq l 0 (1- c)) (subseq l c))
                 (te-cx te) (1- c))))
        ((plusp (te-cy te))
         (te-save-undo te)
         ;; join with the previous line; TE-REPLACE-REGION leaves the cursor at the seam
         (te-replace-region te (1- (te-cy te)) (length (te-line te (1- (te-cy te)))) (te-cy te) 0 ""))))

(defun te-delete (te)
  (cond ((te-sel-ordered te) (te-save-undo te) (te-delete-selection te))
        ((< (te-cx te) (length (te-cur te)))
         (te-save-undo te)
         (let ((l (te-cur te)) (c (te-cx te)))
           (setf (te-line te (te-cy te)) (concatenate 'string (subseq l 0 c) (subseq l (1+ c))))))
        ((< (te-cy te) (1- (te-nlines te)))
         (te-save-undo te)
         (te-replace-region te (te-cy te) (te-cx te) (1+ (te-cy te)) 0 ""))))

(defun te-restore (te snap)
  (setf (te-lines te) (%vec (first snap)) (te-cy te) (second snap) (te-cx te) (third snap)
        (te-anchor te) nil)
  (te-clamp te) (te-ensure-visible te))

(defun te-undo! (te)
  (when (te-undo te)
    (push (te-snapshot te) (te-redo te))
    (te-restore te (pop (te-undo te)))))
(defun te-redo! (te)
  (when (te-redo te)
    (push (te-snapshot te) (te-undo te))
    (te-restore te (pop (te-redo te)))))

(defun te-copy (te) (let ((s (te-selected-string te))) (when s (setf *clipboard* s))))
(defun te-cut (te) (when (te-selected-string te) (te-copy te) (te-save-undo te) (te-delete-selection te)))
(defun te-paste (te) (when (plusp (length *clipboard*)) (te-insert te *clipboard*)))
(defun te-select-all (te)
  (setf (te-anchor te) (cons 0 0)
        (te-cy te) (1- (te-nlines te)) (te-cx te) (length (te-line te (1- (te-nlines te))))))

;;; --- file I/O ---------------------------------------------------------------

(defun te-load (te path)
  (with-open-file (s path :direction :input :if-does-not-exist nil :external-format :utf-8)
    (te-set-text te (if s (let ((str (make-string (file-length s))))
                            (subseq str 0 (read-sequence str s)))
                        "")))
  (setf (te-filename te) path (te-modified te) nil (te-undo te) '() (te-redo te) '()))

(defun te-save (te)
  (when (te-filename te)
    (with-open-file (s (te-filename te) :direction :output :if-exists :supersede
                                        :if-does-not-exist :create :external-format :utf-8)
      (write-string (te-text te) s))
    (setf (te-modified te) nil)
    t))

;;; --- drawing ----------------------------------------------------------------

(defmethod draw ((te text-edit))
  (if (te-wrap te) (te-draw-wrap te) (te-draw-flat te)))

(defun te-draw-flat (te)
  (let* ((b (view-bounds te)) (h (r-h b)) (w (r-w b))
         (top (te-top te)) (left (te-left te))
         (norm (role :normal)) (selattr (role :focused))
         (color (te-colorizer te))
         ;; recover the colorizer's carry state for the first visible line
         (carry (when color (let ((in nil)) (dotimes (i top in) (setf in (lisp-string-carry (te-line te i) in)))))))
    (dotimes (row h)
      (let* ((line-i (+ top row)) (valid (< line-i (te-nlines te)))
             (line (if valid (te-line te line-i) "")) (attrs nil))
        (when (and valid color)
          (multiple-value-bind (a end) (funcall color line carry) (setf attrs a carry end)))
        (dotimes (x w)
          (let* ((col (+ left x))
                 (ch (if (< col (length line)) (char line col) #\Space))
                 (attr (cond ((and valid (te-selected-p te line-i col)) selattr)
                             ((and attrs (< col (length attrs))) (aref attrs col))
                             (t norm))))
            (%put-cell (+ (tvision::rect-ax b) x) (+ (tvision::rect-ay b) row) ch attr)))))
    (when (and (view-focused-p te) tvision:*screen*
               (<= top (te-cy te) (1- (+ top h))) (<= left (te-cx te) (1- (+ left w))))
      (tvision:set-cursor-pos tvision:*screen*
                              (+ (tvision::rect-ax b) (- (te-cx te) left))
                              (+ (tvision::rect-ay b) (- (te-cy te) top)))
      (tvision:show-cursor tvision:*screen*))))

(defun te-draw-wrap (te)
  (let* ((b (view-bounds te)) (h (r-h b)) (w (te-vw te))
         (norm (role :normal)) (selattr (role :focused)) (color (te-colorizer te))
         (line-i (te-top te)) (seg (te-tsub te))
         (carry (when color (let ((in nil)) (dotimes (i (te-top te) in) (setf in (lisp-string-carry (te-line te i) in))))))
         (attrs nil) (cur-line -1) (cursor-row nil) (cursor-col nil))
    (dotimes (row h)
      (let ((valid (< line-i (te-nlines te))))
        (when valid
          (let* ((line (te-line te line-i)) (start (* seg w)) (len (length line)))
            (when (and color (/= cur-line line-i))                  ; colorize each logical line once
              (multiple-value-setq (attrs carry) (funcall color line carry)) (setf cur-line line-i))
            (dotimes (x w)
              (let* ((col (+ start x))
                     (ch (if (< col len) (char line col) #\Space))
                     (attr (cond ((te-selected-p te line-i col) selattr)
                                 ((and color attrs (< col (length attrs))) (aref attrs col))
                                 (t norm))))
                (%put-cell (+ (tvision::rect-ax b) x) (+ (tvision::rect-ay b) row) ch attr)))
            ;; is the cursor on this visual row?
            (when (and (= line-i (te-cy te)) (= seg (floor (te-cx te) w)))
              (setf cursor-row row cursor-col (mod (te-cx te) w)))
            ;; advance one visual row
            (incf seg)
            (when (>= seg (%segs len w)) (setf seg 0) (incf line-i))))
        (unless valid
          (dotimes (x w) (%put-cell (+ (tvision::rect-ax b) x) (+ (tvision::rect-ay b) row) #\Space norm)))))
    (when (and (view-focused-p te) tvision:*screen* cursor-row)
      (tvision:set-cursor-pos tvision:*screen*
                              (+ (tvision::rect-ax b) cursor-col) (+ (tvision::rect-ay b) cursor-row))
      (tvision:show-cursor tvision:*screen*))))

;;; --- key handling (dispatched directly) -------------------------------------

(defun te-move (te e fn)
  "Run movement FN, managing selection (Shift) and viewport."
  (te-mark te e) (funcall fn) (te-clamp te) (te-ensure-visible te))

(defmethod handle-event ((te text-edit) (e mouse-down))
  (setf (te-cy te) (max 0 (min (1- (te-nlines te)) (+ (te-top te) (mouse-row te e))))
        (te-cx te) (max 0 (min (length (te-cur te)) (+ (te-left te) (mouse-col te e)))))
  (setf (te-anchor te) (cons (te-cy te) (te-cx te)))    ; anchor at the click; a drag extends from here
  (te-ensure-visible te) (setf (handled-p e) t))

(defmethod handle-event ((te text-edit) (e mouse-move))
  (when (te-anchor te)                                  ; dragging since the mouse-down -> extend selection
    (setf (te-cy te) (max 0 (min (1- (te-nlines te)) (+ (te-top te) (mouse-row te e))))
          (te-cx te) (max 0 (min (length (te-cur te)) (+ (te-left te) (mouse-col te e)))))
    (te-ensure-visible te))
  (setf (handled-p e) t))

(defmethod handle-event ((te text-edit) (e wheel-event))
  (setf (te-anchor te) nil)
  (incf (te-cy te) (* 3 (event-delta e)))
  (te-clamp te) (te-ensure-visible te) (setf (handled-p e) t))

(defmethod handle-event ((te text-edit) (e key-event))
  (let* ((ks (event-keysym e)) (cc (and (characterp ks) (char-code ks))))
    (macrolet ((done () '(setf (handled-p e) t)))
      (cond
        ;; control chords (arrive as characters with code 1..26)
        ((and cc (<= 1 cc 26))
         (case (code-char (+ 96 cc))
           (#\s (te-save te) (done))
           (#\z (te-undo! te) (done))
           ((#\y #\r) (te-redo! te) (done))
           (#\c (te-copy te) (done))
           (#\x (te-cut te) (te-ensure-visible te) (done))
           (#\v (te-paste te) (te-ensure-visible te) (done))
           (#\a (te-select-all te) (te-ensure-visible te) (done))
           (#\w (setf (te-wrap te) (not (te-wrap te)) (te-left te) 0) (te-ensure-visible te) (done))
           (t (call-next-method))))
        ;; printable insert
        ((and (characterp ks) (graphic-char-p ks))
         (te-insert-char te ks) (te-ensure-visible te) (done))
        ;; editing
        ((eql ks :enter) (te-newline te) (te-ensure-visible te) (done))
        ((eql ks :back)  (te-backspace te) (te-ensure-visible te) (done))
        ((eql ks :del)   (te-delete te) (te-ensure-visible te) (done))
        ;; movement
        ((eql ks :left)  (te-move te e (lambda ()
                                         (if (plusp (te-cx te)) (decf (te-cx te))
                                             (when (plusp (te-cy te))
                                               (decf (te-cy te)) (setf (te-cx te) (length (te-cur te))))))) (done))
        ((eql ks :right) (te-move te e (lambda ()
                                         (if (< (te-cx te) (length (te-cur te))) (incf (te-cx te))
                                             (when (< (te-cy te) (1- (te-nlines te)))
                                               (incf (te-cy te)) (setf (te-cx te) 0))))) (done))
        ((eql ks :up)    (te-move te e (lambda () (if (te-wrap te) (te-vmove te -1) (decf (te-cy te))))) (done))
        ((eql ks :down)  (te-move te e (lambda () (if (te-wrap te) (te-vmove te 1)  (incf (te-cy te))))) (done))
        ((eql ks :home)  (te-move te e (lambda () (setf (te-cx te) 0))) (done))
        ((eql ks :end)   (te-move te e (lambda () (setf (te-cx te) (length (te-cur te))))) (done))
        ((eql ks :pgup)  (te-move te e (lambda () (let ((n (max 1 (1- (r-h (view-bounds te))))))
                                                   (if (te-wrap te) (dotimes (i n) (te-vmove te -1)) (decf (te-cy te) n))))) (done))
        ((eql ks :pgdn)  (te-move te e (lambda () (let ((n (max 1 (1- (r-h (view-bounds te))))))
                                                   (if (te-wrap te) (dotimes (i n) (te-vmove te 1)) (incf (te-cy te) n))))) (done))
        (t (call-next-method))))))               ; Esc / q bubble to the window

;;; --- the editor window ------------------------------------------------------

(defun %editor-status (win)
  (let ((te (find-view win 'edit)) (st (find-view win 'status)))
    (when (and te st)
      (setf (static-text-text st)
            (format nil " ~a~:[~;*~]   L~d:C~d~:[~; (sel)~]~:[~; WRAP~]   C-s save · C-z/y undo · C-c/x/v · C-w wrap · Esc quit "
                    (if (te-filename te) (file-namestring (te-filename te)) "scratch")
                    (te-modified te) (1+ (te-cy te)) (1+ (te-cx te))
                    (te-selected-string te) (te-wrap te)))
      (invalidate st))))

(defun %editor-find (te)
  "Modal find prompt; on Enter, search from the cursor and select the match."
  (let ((d (ui (dialog (:title " Find " :keymap *dialog-keys*
                        :value-fn (lambda (d) (input-text (find-view d 'q))))
                 (stack
                   (1 (row (7 (static-text :role :label :text " Find: ")) (:fill (input-line :name 'q :history-id :find))))
                   (1 (static-text :role :status :text " Enter: search (case-insensitive) · Esc: cancel ")))))))
    (let ((r (exec-view d :width 52 :height 6)))
      (unless (eq r :cancel) (te-find te r :from-line (te-cy te) :from-col (te-cx te))))))

(defmethod status-hints ((te text-edit))   ; chips the desktop shows while the editor is focused
  (list (cons "Find" (lambda () (%editor-find te)))
        (cons "Next" (lambda () (te-find-next te)))
        (cons "Undo" (lambda () (te-undo! te)))
        (cons "Redo" (lambda () (te-redo! te)))
        (cons (if (te-wrap te) "Wrap:on" "Wrap:off")
              (lambda () (setf (te-wrap te) (not (te-wrap te)) (te-left te) 0) (te-ensure-visible te) (invalidate te)))))

(defclass editor-window (window) () (:metaclass reactive-class))
(defmethod draw :before ((w editor-window)) (%editor-status w))   ; keep the status line live each repaint

(defun make-editor (&optional path)
  "Build a text-editor window for PATH (or a scratch buffer).  Return (values
WINDOW FOCUS)."
  (let* ((win (make-instance 'editor-window
                             :title " tv2 — Text editor (a real tvlisp window, ported) " :keymap *global-keys*))
         (body (ui (stack
                     (:fill (text-edit :name 'edit))
                     (1 (static-text :name 'status :role :status :text ""))))))
    (add-subview win body)
    (let ((te (find-view win 'edit)))
      (if (and path (probe-file path))
          (te-load te path)
          (te-set-text te (format nil ";; tv2 scratch buffer~%;; type freely — Shift+arrows select, C-c/C-x/C-v copy/cut/paste, C-z/C-y undo/redo.~%~%(defun hello (name)~%  (format t \"hello, ~~a!~~%\" name))~%")))
      (when (or (null path) (member (pathname-type path) '("lisp" "asd" "cl") :test #'equal))
        (setf (te-colorizer te) #'lisp-colorize (te-indenter te) #'lisp-auto-indent)))
    (%editor-status win)
    (setf (window-scroll-target win) (find-view win 'edit) (window-help win) :editor)
    (values win (find-view win 'edit))))

(defun run-editor (&optional path)
  "Run the ported text editor full-screen until Esc."
  (multiple-value-bind (w f) (make-editor path) (run-view w :focus f)))
