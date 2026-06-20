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
  "Total display width of S[START,END) in terminal columns."
  (loop for i from start below end sum (char-width (char s i))))

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
