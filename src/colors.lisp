;;;; colors.lisp --- DOS-style text attributes, palettes and ANSI translation.
;;;;
;;;; A Turbo Vision colour attribute is a single byte: the low nibble is the
;;;; foreground colour (0-15) and the high nibble is the background colour
;;;; (bits 4-6, 0-7) plus a blink bit (bit 7).

(in-package #:tvision)

;;; An attribute is a 32-bit value.  When bit 31 is clear it is a legacy 4-bit
;;; DOS byte (fg 0-15 / bg 0-7 / blink), resolved through a 16-colour theme.
;;; When bit 31 is set it is a true-colour attribute: the low 31 bits index an
;;; interned (fg . bg) RGB pair, so equal colours share an integer and the
;;; diffing renderer keeps comparing cells by `='.
(deftype attr () '(unsigned-byte 32))

(declaim (inline make-attr attr-fg attr-bg))
(defun make-attr (fg bg &optional (blink nil))
  "Build a legacy DOS attribute from foreground FG (0-15) and background BG (0-7)."
  (logior (logand fg #x0f)
          (ash (logand bg #x07) 4)
          (if blink #x80 0)))

(defun attr-fg (a) (logand a #x0f))
(defun attr-bg (a) (logand (ash a -4) #x07))

;;; --- true-colour attributes ------------------------------------------------

(defconstant +attr-rgb-flag+ #x80000000)

(declaim (inline pack-rgb attr-rgb-p))
(defun pack-rgb (r g b)
  "Pack an (R G B) triple (0-255 each) into a 24-bit integer."
  (logior (ash (logand r #xff) 16) (ash (logand g #xff) 8) (logand b #xff)))
(defun attr-rgb-p (a) (logtest a +attr-rgb-flag+))

(defvar *rgb-pairs* (make-array 64 :adjustable t :fill-pointer 0)
  "Interned (fg<<24 | bg) 48-bit colour pairs; the RGB attr's index points here.")
(defvar *rgb-index* (make-hash-table)
  "Maps a packed fg/bg key to its interned RGB attr (for dedup).")

(defun rgb-attr (fg-rgb bg-rgb)
  "Intern a true-colour attribute from 24-bit packed FG-RGB and BG-RGB; return it."
  (let ((key (logior (ash (logand fg-rgb #xffffff) 24) (logand bg-rgb #xffffff))))
    (or (gethash key *rgb-index*)
        (let ((a (logior +attr-rgb-flag+ (fill-pointer *rgb-pairs*))))
          (vector-push-extend key *rgb-pairs*)
          (setf (gethash key *rgb-index*) a)
          a))))

(defun make-rgb (fr fg fb br bg bb)
  "Intern a true-colour attribute from foreground (FR FG FB) and background
(BR BG BB) channel values (0-255)."
  (rgb-attr (pack-rgb fr fg fb) (pack-rgb br bg bb)))

(defun attr-rgb-fg (a)
  "The 24-bit packed foreground of an RGB attribute A."
  (ash (aref *rgb-pairs* (logand a #x7fffffff)) -24))
(defun attr-rgb-bg (a)
  "The 24-bit packed background of an RGB attribute A."
  (logand (aref *rgb-pairs* (logand a #x7fffffff)) #xffffff))

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

(defun %tint-theme (tr tg tb)
  "A monochrome phosphor theme: map each VGA colour to its luminance scaled by
the tint (TR TG TB).  Black stays black; brighter colours glow brighter."
  (make-rgb-theme
   (loop for i below 16 for j = (* 3 i)
         for lum = (/ (+ (* 0.30 (aref +theme-vga+ j))
                         (* 0.59 (aref +theme-vga+ (+ j 1)))
                         (* 0.11 (aref +theme-vga+ (+ j 2)))) 255.0)
         collect (list (round (* tr lum)) (round (* tg lum)) (round (* tb lum))))))

(defparameter +theme-green+ (%tint-theme  80 255  80) "Green-phosphor CRT look.")
(defparameter +theme-amber+ (%tint-theme 255 182  66) "Amber-phosphor CRT look.")

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

(defun %nearest-dos (r g b)
  "DOS colour index 0-15 whose theme RGB is closest to (R G B)."
  (let ((best 0) (bestd most-positive-fixnum) (th *rgb-theme*))
    (dotimes (i 16 best)
      (let* ((j (* 3 i))
             (dr (- r (aref th j))) (dg (- g (aref th (+ j 1)))) (db (- b (aref th (+ j 2))))
             (d (+ (* dr dr) (* dg dg) (* db db))))
        (when (< d bestd) (setf bestd d best i))))))

(defun %sgr-rgb (fr fg fb br bg bb)
  "An SGR string setting fg (FR FG FB) and bg (BR BG BB), honouring *COLOR-MODE*."
  (ecase *color-mode*
    (:truecolor
     (format nil "~c[0;38;2;~d;~d;~d;48;2;~d;~d;~dm" #\Escape fr fg fb br bg bb))
    (:256
     (format nil "~c[0;38;5;~d;48;5;~dm" #\Escape
             (%rgb->256 fr fg fb) (%rgb->256 br bg bb)))
    (:16
     (let* ((dfg (%nearest-dos fr fg fb)) (dbg (%nearest-dos br bg bb))
            (afg (aref +dos->ansi+ (logand dfg 7))) (abg (aref +dos->ansi+ (logand dbg 7))))
       (format nil "~c[0;~d;~dm" #\Escape
               (if (>= dfg 8) (+ 90 afg) (+ 30 afg)) (+ 40 abg))))))

(defun attr->ansi (a)
  "Return an ANSI SGR escape string for attribute A, honouring *COLOR-MODE* and
the active *RGB-THEME*.  Legacy DOS attrs resolve through the theme; true-colour
attrs emit their exact RGB (downgraded to the cube / nearest-16 when needed)."
  (cond
    ((attr-rgb-p a)
     (let ((fg (attr-rgb-fg a)) (bg (attr-rgb-bg a)))
       (%sgr-rgb (ldb (byte 8 16) fg) (ldb (byte 8 8) fg) (ldb (byte 8 0) fg)
                 (ldb (byte 8 16) bg) (ldb (byte 8 8) bg) (ldb (byte 8 0) bg))))
    ;; legacy 4-bit attr: keep the exact 16-colour codes (back-compatible)
    ((eq *color-mode* :16)
     (let ((fg (attr-fg a)) (bg (attr-bg a)))
       (format nil "~c[0;~d;~dm" #\Escape
               (let ((afg (aref +dos->ansi+ (logand fg 7)))) (if (>= fg 8) (+ 90 afg) (+ 30 afg)))
               (+ 40 (aref +dos->ansi+ bg)))))
    (t
     (multiple-value-bind (fr fg fb) (%theme-rgb (attr-fg a))
       (multiple-value-bind (br bg bb) (%theme-rgb (attr-bg a))
         (%sgr-rgb fr fg fb br bg bb))))))

(defun set-color-theme (theme)
  "Set the active RGB theme (a name :vga / :modern, or an RGB-THEME vector)."
  (setf *rgb-theme*
        (case theme
          (:vga +theme-vga+) (:modern +theme-modern+)
          (:green +theme-green+) (:amber +theme-amber+)
          (t theme))))

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
