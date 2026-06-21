;;;; draw-buffer.lisp --- TDrawBuffer: a one-dimensional run of screen cells.
;;;;
;;;; Each cell packs a character code (low 21 bits -- the full Unicode range) and
;;;; an attribute (bits 21-52) into a single (unsigned-byte 53).  The attribute
;;;; is either a 4-bit DOS byte (legacy palette colours) or a tagged true-colour
;;;; value -- see colors.lisp.  This generalises Turbo Vision's word-per-cell
;;;; video memory while admitting any Unicode code point, not just the BMP.

(in-package #:tvision)

(deftype cell () '(unsigned-byte 53))

(declaim (inline cell-make cell-char cell-attr cell-char-code))
(defun cell-make (char attr)
  (logior (logand (char-code char) #x1fffff) (ash attr 21)))
(defun cell-make-code (code attr)
  (logior (logand code #x1fffff) (ash attr 21)))
(defun cell-char-code (c) (logand c #x1fffff))
(defun cell-char (c) (code-char (logand c #x1fffff)))
(defun cell-attr (c) (logand (ash c -21) #xffffffff))

(defconstant +impossible-cell+ #x1fffffffffffff
  "A 53-bit cell value no real cell ever takes (an out-of-range char code +
an un-interned RGB attr); the front-buffer sentinel so the first flush repaints
everything.")

(defconstant +wide-cont+ #x1ffffe
  "Char code marking the second cell of a double-width glyph: the renderer skips
it (the wide glyph to its left already covers that column).")

(declaim (inline char-width))
(defun char-width (ch)
  "Display width of CH in terminal columns: 2 for East-Asian wide/fullwidth
characters (CJK, most emoji), 1 otherwise.  ASCII and the low BMP fast-path to 1."
  (let ((code (char-code ch)))
    (if (< code #x1100) 1
        (case (sb-unicode:east-asian-width ch) ((:w :f) 2) (t 1)))))

(defun string-width (s &optional (start 0) (end (length s)))
  "Total display width of S[START,END) in terminal columns (per code point;
for grapheme-aware width use GRAPHEME-WIDTH over clusters)."
  (loop for i from start below end sum (char-width (char s i))))

;;; --- grapheme clusters ------------------------------------------------------
;;; A cell holds one code point in its 21-bit char field; a multi-code-point
;;; grapheme cluster (combining marks, ZWJ / skin-tone emoji) is stored by
;;; interning the cluster string and putting its index in the unused code-point
;;; range #x110000..#x1FFFFD (below the +wide-cont+/+impossible-cell+ sentinels).
;;; Single code points stay literal -- the common, allocation-free path.

(defconstant +cluster-base+ #x110000)

(defvar *graphemes* (make-array 64 :adjustable t :fill-pointer 0)
  "Interned grapheme-cluster strings; a cluster cell's code is +cluster-base+ + index.")
(defvar *grapheme-index* (make-hash-table :test 'equal)
  "Maps a cluster string to its interned cell code (for dedup, so the diff
renderer keeps comparing cells with `=').")

(defun intern-grapheme (s)
  "Intern cluster string S; return its char-field code (>= +cluster-base+)."
  (or (gethash s *grapheme-index*)
      (let ((code (+ +cluster-base+ (fill-pointer *graphemes*))))
        (vector-push-extend (copy-seq s) *graphemes*)
        (setf (gethash s *grapheme-index*) code)
        code)))

(declaim (inline cluster-code-p))
(defun cluster-code-p (code) (<= +cluster-base+ code #x1ffffd))
(defun cluster-string (code)
  "The cluster string for a cluster CODE, or NIL."
  (when (cluster-code-p code) (aref *graphemes* (- code +cluster-base+))))

(defun grapheme-width (cluster)
  "Display width of a grapheme CLUSTER (its base character's width)."
  (if (plusp (length cluster)) (char-width (char cluster 0)) 1))

(defun simple-line-p (line)
  "True when LINE can contain no grapheme cluster spanning >1 code point, i.e.
no combining marks, ZWJ, regional indicators, emoji modifiers or variation
selectors (all of which live at or above U+0300).  The fast path for plain text."
  (every (lambda (ch) (< (char-code ch) #x300)) line))

(defun grapheme-offsets (line)
  "Sorted code-point indices where grapheme clusters start in LINE, plus the
final length: (0 ... LEN)."
  (let ((offs (list 0)) (pos 0))
    (dolist (g (sb-unicode:graphemes line))
      (incf pos (length g)) (push pos offs))
    (nreverse offs)))

(defun next-grapheme-col (line col)
  "Code-point index of the grapheme boundary after COL."
  (if (simple-line-p line)
      (min (1+ col) (length line))
      (or (find-if (lambda (o) (> o col)) (grapheme-offsets line)) (length line))))

(defun prev-grapheme-col (line col)
  "Code-point index of the grapheme boundary before COL."
  (if (simple-line-p line)
      (max (1- col) 0)
      (let ((prev 0))
        (dolist (o (grapheme-offsets line) prev)
          (if (< o col) (setf prev o) (return prev))))))

(defun visual-col (line start col)
  "Display column (relative to START) of code-point index COL, counting each
grapheme cluster as one display unit."
  (cond ((<= col start) 0)
        ((simple-line-p line) (string-width line start (min col (length line))))
        (t (let ((offs (grapheme-offsets line)) (w 0))
             (loop for (a b) on offs while (and b (< a col))
                   when (>= a start)
                   do (incf w (grapheme-width (subseq line a (min b (length line))))))
             w))))

(defun col-at-vcol (line start end g)
  "Code-point index in LINE[START,END) whose display column (relative to START)
is the largest not exceeding G -- the inverse of VISUAL-COL, for cursor up/down
and mouse hits in wrapped text."
  (loop with vx = 0 and i = start
        while (< i end)
        for cw = (char-width (char line i))
        while (<= (+ vx cw) g)
        do (incf vx cw) (setf i (next-grapheme-col line i))
        finally (return i)))

(defun wrap-segments (line w)
  "Code-point start index of each visual row when LINE is wrapped to W display
columns.  Breaks at word boundaries (whitespace) when it can, hard-splitting a
single word that is itself wider than W; always on grapheme boundaries and never
splitting a wide glyph.  Trailing whitespace runs past the margin rather than
wrapping.  A trailing empty row is added when the last row fills W exactly (so
the cursor can sit past a full line).  Always returns at least (0)."
  (let* ((len (length line)) (w (max 1 w)))
    (if (zerop len)
        (list 0)
        (let ((segs (list 0)) (col 0) (i 0)
              (simple (simple-line-p line)))
          (flet ((cwidth (j) (if simple 1 (char-width (char line j))))
                 (gnext (j) (if simple (1+ j) (next-grapheme-col line j)))
                 (spacep (j) (let ((c (char line j))) (or (char= c #\Space) (char= c #\Tab)))))
            (labels ((hard-split (start end)
                       ;; lay [START,END) onto fresh rows, returning the final col
                       (let ((j start) (rc 0))
                         (loop while (< j end) do
                           (let ((cw (cwidth j)))
                             (when (and (> (+ rc cw) w) (plusp rc)) (push j segs) (setf rc 0))
                             (incf rc cw) (setf j (gnext j))))
                         rc)))
              (loop while (< i len) do
                ;; measure the next token: a run of whitespace, or one word
                (let ((tok-start i) (tok-w 0) (space (spacep i)))
                  (loop while (and (< i len) (eq (spacep i) space))
                        do (incf tok-w (cwidth i)) (setf i (gnext i)))
                  (cond
                    ((<= (+ col tok-w) w) (incf col tok-w))           ; fits on this row
                    (space (setf col w))                             ; spaces overrun the margin
                    ((zerop col) (setf col (hard-split tok-start i))) ; word alone wider than W
                    (t (push tok-start segs)                         ; wrap the word to a new row
                       (setf col (if (<= tok-w w) tok-w (hard-split tok-start i)))))))))
          (setf segs (nreverse segs))
          (if (>= col w) (append segs (list len)) segs)))))

(defstruct (draw-buffer (:constructor %make-draw-buffer))
  (data (make-array 0 :element-type '(unsigned-byte 53)) :type (simple-array (unsigned-byte 53) (*)))
  (width 0 :type fixnum))

(defun make-draw-buffer (width)
  (%make-draw-buffer
   :data (make-array (max 0 width) :element-type '(unsigned-byte 53)
                                   :initial-element (cell-make-code 32 #x07))
   :width width))

(defun db-width (b) (draw-buffer-width b))

(defun db-fill (b char attr &optional (start 0) (count (draw-buffer-width b)))
  "Fill COUNT cells of B starting at START with CHAR/ATTR."
  (let ((data (draw-buffer-data b))
        (w (draw-buffer-width b))
        (val (cell-make-code (char-code char) attr)))
    (loop for i from start below (min w (+ start count))
          do (setf (aref data i) val))
    b))

(defun db-put-char (b index char &optional (attr nil))
  "Store CHAR at INDEX, keeping the existing attribute unless ATTR is given."
  (let ((data (draw-buffer-data b)))
    (when (and (>= index 0) (< index (draw-buffer-width b)))
      (let ((a (or attr (cell-attr (aref data index)))))
        (setf (aref data index) (cell-make-code (char-code char) a)))))
  b)

(defun db-put-code (b index code &optional attr)
  "Store raw char CODE at INDEX (keeping the existing attribute unless ATTR is
given).  Used to mark a wide glyph's continuation cell with +wide-cont+."
  (let ((data (draw-buffer-data b)))
    (when (and (>= index 0) (< index (draw-buffer-width b)))
      (let ((a (or attr (cell-attr (aref data index)))))
        (setf (aref data index) (cell-make-code code a)))))
  b)

(defun db-put-attribute (b index attr &optional (count 1))
  "Recolour COUNT cells beginning at INDEX, leaving their characters intact."
  (let ((data (draw-buffer-data b))
        (w (draw-buffer-width b)))
    (loop for i from index below (min w (+ index count))
          when (>= i 0)
          do (setf (aref data i)
                   (cell-make-code (cell-char-code (aref data i)) attr))))
  b)

(defun db-move-char (b index char attr count)
  "Write CHAR/ATTR into COUNT consecutive cells starting at INDEX."
  (db-fill b char attr index count))

(defun db-move-str (b index string attr)
  "Write STRING starting at INDEX, all in attribute ATTR."
  (let ((data (draw-buffer-data b))
        (w (draw-buffer-width b)))
    (loop for ch across string
          for i from index below w
          do (setf (aref data i) (cell-make-code (char-code ch) attr))))
  b)

(defun db-move-cstr (b index string palette &optional (start-attr 1))
  "Write STRING starting at INDEX with embedded colour control.

The character #\\~ (tilde) toggles between palette index START-ATTR and
START-ATTR+1, mirroring Turbo Vision's `~text~' highlight convention."
  (let ((data (draw-buffer-data b))
        (w (draw-buffer-width b))
        (cur (if (vectorp palette) (palette-ref palette start-attr) start-attr))
        (alt (if (vectorp palette) (palette-ref palette (1+ start-attr)) start-attr))
        (toggled nil)
        (i index))
    (loop for ch across string
          do (cond
               ((char= ch #\~) (setf toggled (not toggled)))
               (t (when (< i w)
                    (setf (aref data i)
                          (cell-make-code (char-code ch) (if toggled alt cur))))
                  (incf i))))
    b))

(defun db-move-buf (b index source attr count)
  "Copy COUNT cells from SOURCE (a (unsigned-byte 24) array or draw-buffer)
into B at INDEX.  When ATTR is non-nil it overrides every copied attribute."
  (let* ((data (draw-buffer-data b))
         (w (draw-buffer-width b))
         (src (if (draw-buffer-p source) (draw-buffer-data source) source)))
    (loop for k from 0 below count
          for i from index below w
          for c = (aref src k)
          do (setf (aref data i)
                   (if attr (cell-make-code (cell-char-code c) attr) c))))
  b)
