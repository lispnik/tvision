;;;; draw-buffer.lisp --- TDrawBuffer: a one-dimensional run of screen cells.
;;;;
;;;; Each cell packs a character code (low 16 bits) and an attribute byte
;;;; (bits 16-23) into a single (unsigned-byte 24), exactly mirroring the
;;;; word-per-cell layout that Turbo Vision uses for video memory.

(in-package #:tvision)

(deftype cell () '(unsigned-byte 24))

(declaim (inline cell-make cell-char cell-attr cell-char-code))
(defun cell-make (char attr)
  (logior (logand (char-code char) #xffff) (ash attr 16)))
(defun cell-make-code (code attr)
  (logior (logand code #xffff) (ash attr 16)))
(defun cell-char-code (c) (logand c #xffff))
(defun cell-char (c) (code-char (logand c #xffff)))
(defun cell-attr (c) (logand (ash c -16) #xff))

(defstruct (draw-buffer (:constructor %make-draw-buffer))
  (data (make-array 0 :element-type '(unsigned-byte 24)) :type (simple-array (unsigned-byte 24) (*)))
  (width 0 :type fixnum))

(defun make-draw-buffer (width)
  (%make-draw-buffer
   :data (make-array (max 0 width) :element-type '(unsigned-byte 24)
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
