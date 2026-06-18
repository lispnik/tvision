;;;; colors.lisp --- DOS-style text attributes, palettes and ANSI translation.
;;;;
;;;; A Turbo Vision colour attribute is a single byte: the low nibble is the
;;;; foreground colour (0-15) and the high nibble is the background colour
;;;; (bits 4-6, 0-7) plus a blink bit (bit 7).

(in-package #:tvision)

(deftype attr () '(unsigned-byte 8))

(declaim (inline make-attr attr-fg attr-bg))
(defun make-attr (fg bg &optional (blink nil))
  "Build an attribute byte from foreground FG (0-15) and background BG (0-7)."
  (logior (logand fg #x0f)
          (ash (logand bg #x07) 4)
          (if blink #x80 0)))

(defun attr-fg (a) (logand a #x0f))
(defun attr-bg (a) (logand (ash a -4) #x07))

;;; Map DOS colour indices (IRGB ordering) onto ANSI SGR colour indices
;;; (the ANSI order is black,red,green,yellow,blue,magenta,cyan,white).
(declaim (type (simple-array (unsigned-byte 8) (8)) +dos->ansi+))
(defparameter +dos->ansi+
  (make-array 8 :element-type '(unsigned-byte 8)
                :initial-contents '(0 4 2 6 1 5 3 7)))

(defun attr->ansi (a)
  "Return an ANSI SGR escape string that selects attribute byte A."
  (let* ((fg (attr-fg a))
         (bg (attr-bg a))
         (afg (aref +dos->ansi+ (logand fg 7)))
         (abg (aref +dos->ansi+ bg))
         (bright (>= fg 8)))
    (format nil "~c[0;~d;~dm"
            #\Escape
            (if bright (+ 90 afg) (+ 30 afg))
            (+ 40 abg))))

;;; ---------------------------------------------------------------------------
;;; Palettes
;;;
;;; A palette is a vector of attribute bytes.  Views map a small "colour index"
;;; through their own palette, then -- as the request bubbles up through the
;;; owner chain -- through each owning group's palette, just like Turbo Vision.
;;; Index 0 in a palette is unused (1-based), matching the original.
;;; ---------------------------------------------------------------------------

(deftype tpalette () '(simple-array (unsigned-byte 8) (*)))

(defun make-palette (&rest bytes)
  "Build a 1-based palette.  A leading 0 cell is prepended automatically."
  (let ((v (make-array (1+ (length bytes)) :element-type '(unsigned-byte 8))))
    (loop for b in bytes for i from 1 do (setf (aref v i) b))
    v))

(defun make-palette-from-list (bytes)
  (apply #'make-palette bytes))

(declaim (inline palette-ref))
(defun palette-ref (palette index)
  "Look up colour INDEX (1-based) in PALETTE, or 0 if out of range."
  (if (and palette (>= index 1) (< index (length palette)))
      (aref palette index)
      0))

;;; A few named attributes that are handy when no palette applies.
(defparameter +color-normal+ (make-attr 7 0))   ; light grey on black
(defparameter +color-error+  (make-attr 15 4))  ; white on red
