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

;;; ---------------------------------------------------------------------------
;;; Colour rendering: a 16-entry RGB theme for the DOS palette plus a terminal
;;; capability ladder.  Views still work in 4-bit DOS attributes; only the
;;; final escape emission resolves them -- so switching themes or colour depth
;;; never touches a view.  *COLOR-MODE* is :truecolor (24-bit), :256, or :16.
;;; ---------------------------------------------------------------------------

(deftype rgb-theme () '(simple-array (unsigned-byte 8) (48)))  ; 16 colours * (r g b)

(defun make-rgb-theme (triples)
  "Build a 16-colour RGB theme from a list of 16 (R G B) lists."
  (let ((v (make-array 48 :element-type '(unsigned-byte 8))))
    (loop for (r g b) in triples for i from 0 below 16
          do (setf (aref v (* 3 i)) r (aref v (+ 1 (* 3 i))) g (aref v (+ 2 (* 3 i))) b))
    v))

;; The classic VGA / Borland 16-colour palette (IRGB order).
(defparameter +theme-vga+
  (make-rgb-theme
   '((  0   0   0) (  0   0 170) (  0 170   0) (  0 170 170)   ; blk blu grn cyn
     (170   0   0) (170   0 170) (170  85   0) (170 170 170)   ; red mag brn lgry
     ( 85  85  85) ( 85  85 255) ( 85 255  85) ( 85 255 255)   ; dgry lblu lgrn lcyn
     (255  85  85) (255  85 255) (255 255  85) (255 255 255))));lred lmag yel wht

;; A softer, modern palette (Tango-ish) -- nicer on high-res displays.
(defparameter +theme-modern+
  (make-rgb-theme
   '((  7  10  15) ( 52 101 164) ( 78 154   6) ( 17 168 205)
     (204   0   0) (117  80 123) (193 125  17) (186 189 182)
     ( 85  87  83) (114 159 207) (138 226  52) ( 52 226 226)
     (239  41  41) (173 127 168) (252 233  79) (255 255 255))))

(defparameter *rgb-theme* +theme-vga+
  "The active 16-colour RGB theme used when emitting 24-bit / 256-colour SGR.")

(defparameter *color-mode* :truecolor
  "Terminal colour depth for SGR emission: :truecolor (24-bit), :256, or :16.
Set by DETECT-COLOR-MODE at startup; override freely.")

(defun detect-color-mode ()
  "Pick a colour mode from the environment: $COLORTERM=truecolor/24bit -> 24-bit;
a *-256color $TERM -> 256; otherwise 16."
  (let ((ct (sb-ext:posix-getenv "COLORTERM"))
        (term (or (sb-ext:posix-getenv "TERM") "")))
    (cond
      ((and ct (or (search "truecolor" ct) (search "24bit" ct))) :truecolor)
      ((search "256color" term) :256)
      (t :16))))

(declaim (inline %theme-rgb))
(defun %theme-rgb (index)
  "Return (values R G B) for DOS colour INDEX (0-15) in the active theme."
  (let ((i (* 3 (logand index 15))) (th *rgb-theme*))
    (values (aref th i) (aref th (+ i 1)) (aref th (+ i 2)))))

(defun %rgb->256 (r g b)
  "Nearest xterm-256 index for an RGB triple (6x6x6 cube + grey ramp)."
  (flet ((q (v) (cond ((< v 48) 0) ((< v 115) 1) (t (min 5 (floor (- v 35) 40))))))
    (if (and (= r g) (= g b))                       ; grey -> the grey ramp
        (if (< r 8) 16 (if (> r 238) 231 (+ 232 (floor (- r 8) 10))))
        (+ 16 (* 36 (q r)) (* 6 (q g)) (q b)))))

(defun attr->ansi (a)
  "Return an ANSI SGR escape string that selects attribute byte A, honouring
*COLOR-MODE* and the active *RGB-THEME*."
  (let* ((fg (attr-fg a)) (bg (attr-bg a)))
    (ecase *color-mode*
      (:truecolor
       (multiple-value-bind (fr fg* fb) (%theme-rgb fg)
         (multiple-value-bind (br bg* bb) (%theme-rgb bg)
           (format nil "~c[0;38;2;~d;~d;~d;48;2;~d;~d;~dm"
                   #\Escape fr fg* fb br bg* bb))))
      (:256
       (multiple-value-bind (fr fg* fb) (%theme-rgb fg)
         (multiple-value-bind (br bg* bb) (%theme-rgb bg)
           (format nil "~c[0;38;5;~d;48;5;~dm" #\Escape
                   (%rgb->256 fr fg* fb) (%rgb->256 br bg* bb)))))
      (:16
       (let ((afg (aref +dos->ansi+ (logand fg 7)))
             (abg (aref +dos->ansi+ bg))
             (bright (>= fg 8)))
         (format nil "~c[0;~d;~dm" #\Escape
                 (if bright (+ 90 afg) (+ 30 afg)) (+ 40 abg)))))))

(defun set-color-theme (theme)
  "Set the active RGB theme (a name :vga / :modern, or an RGB-THEME vector)."
  (setf *rgb-theme*
        (case theme (:vga +theme-vga+) (:modern +theme-modern+) (t theme))))

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
