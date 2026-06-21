;;;; tvision-tests.lisp --- The control test suite, on FiveAM.
;;;;
;;;; The library and example binaries have zero external dependencies; FiveAM is
;;;; used *only here*, for the tests.  Each test constructs a control, feeds
;;;; events through HANDLE-EVENT, and asserts on its state, data or rendered
;;;; cells.  DEFTEST / OK / IS= are thin wrappers over FiveAM's TEST / IS-TRUE /
;;;; IS so the existing assertions read unchanged but run as real FiveAM checks.
;;;;
;;;; Run with:  (tvision-tests:run-tests)   ; returns the failure count
;;;; or:        make test

(defpackage #:tvision-tests
  (:use #:common-lisp #:tvision)
  (:export #:run-tests #:toplevel #:tvision-suite))

(in-package #:tvision-tests)

;;; ---------------------------------------------------------------------------
;;; Harness: FiveAM with a small compatibility vocabulary
;;; ---------------------------------------------------------------------------

(5am:def-suite tvision-suite
  :description "Turbo Vision control + framework tests.")
(5am:in-suite tvision-suite)

(defmacro deftest (name &body body)
  "Define a FiveAM test NAME in the tvision suite."
  `(5am:test ,name ,@body))

(defmacro ok (desc form)
  "Assert FORM is true; DESC is the failure description."
  `(5am:is-true ,form "~a" ,desc))

(defmacro is= (desc actual expected &key (test '#'equal))
  "Assert (TEST ACTUAL EXPECTED); DESC labels the check."
  (let ((a (gensym)) (e (gensym)))
    `(let ((,a ,actual) (,e ,expected))
       (5am:is (funcall ,test ,a ,e) "~a -- got ~s, want ~s" ,desc ,a ,e))))

(defun make-test-screen ()
  (let ((s (tvision::make-screen)))
    (setf (tvision::screen-out s) (make-string-output-stream))
    (screen-resize s 80 25)
    s))

(defun run-tests ()
  "Run the suite under FiveAM; print the report; return the failure count.
The screen and REPL globals tests rely on are bound for the whole run."
  (let ((*screen* (make-test-screen))
        (*repl-async* nil)            ; keep the REPL inline in tests
        (*repl-debugger* nil)
        (5am:*on-error* nil) (5am:*on-failure* nil))   ; record, never enter the debugger
    (let* ((results (5am:run 'tvision-suite))
           (failures (nth-value 1 (5am:results-status results)))
           (nfail (length failures)))
      (5am:explain! results)
      (format t "~&==== ~d checks, ~d failure~:p ====~%" (length results) nfail)
      nfail)))

(defun toplevel ()
  (sb-ext:exit :code (if (zerop (run-tests)) 0 1)))

;;; --- helpers ---------------------------------------------------------------

(defun host (control &optional (bounds (make-trect 0 0 78 23)))
  "Insert CONTROL into a fresh full-size window so it has an owner (for focus,
broadcasts and drawing); return the control."
  (let ((w (make-instance 'twindow :title "host" :bounds bounds)))
    (insert w control)
    control))

(defun focused (v)
  (setf (view-state v) (logior (view-state v) +sf-focused+))
  v)

(defun ev-key (code &optional (char 0) (mods 0))
  (make-event :type +ev-key-down+ :key-code code :char-code char :modifiers mods))

(defun type-char (v ch)
  (handle-event v (ev-key (char-code ch) (char-code ch))))

(defun press-key (v code)
  (handle-event v (ev-key code 0)))

(defun cell-char-at (x y)
  (tvision::cell-char (aref (screen-back-buffer *screen*)
                           (tvision::screen-index *screen* x y))))

(defun text-at (x y len)
  (coerce (loop for i below len collect (cell-char-at (+ x i) y)) 'string))

;; A group that records the events put to it (for command-dispatch tests).
(defclass recorder (tgroup) ((events :initform '() :accessor rec-events)))
(defmethod put-event ((g recorder) event)
  (push event (rec-events g)))

;; A view whose handle-event always errors (to test loop resilience).
(defclass exploding-view (tview) ())
(defmethod handle-event ((v exploding-view) event)
  (declare (ignore event))
  (error "boom"))

;;; ===========================================================================
;;; Geometry
;;; ===========================================================================

(deftest geometry
  (let ((r (make-trect 2 3 12 8)))
    (is= "width" (rect-width r) 10)
    (is= "height" (rect-height r) 5)
    (ok "contains inside" (rect-contains-p r 5 5))
    (ok "excludes outside" (not (rect-contains-p r 12 8)))      ; bx/by exclusive
    (ok "excludes left" (not (rect-contains-p r 1 5))))
  (let ((a (make-trect 0 0 10 10)) (b (make-trect 5 5 20 20)))
    (let ((i (rect-intersect a b)))
      (is= "intersect ax" (rect-ax i) 5)
      (is= "intersect bx" (rect-bx i) 10))
    (let ((u (rect-union a b)))
      (is= "union bx" (rect-bx u) 20)
      (is= "union by" (rect-by u) 20))))

;;; ===========================================================================
;;; Colour rendering: theme + capability ladder
;;; ===========================================================================

(deftest color-modes
  (let ((a (make-attr 14 1)))          ; bright yellow on blue
    ;; 16-colour mode is the classic 4-bit SGR (unchanged, back-compatible)
    (let ((tvision::*color-mode* :16))
      (is= "16-colour SGR" (attr->ansi a) (format nil "~c[0;93;44m" #\Escape)))
    ;; true colour emits 24-bit fg/bg from the active theme (VGA here)
    (let ((tvision::*color-mode* :truecolor)
          (tvision::*rgb-theme* tvision:+theme-vga+))
      (is= "truecolour SGR"
           (attr->ansi a)
           (format nil "~c[0;38;2;255;255;85;48;2;0;0;170m" #\Escape)))
    ;; 256-colour maps the theme RGB onto the xterm cube
    (let ((tvision::*color-mode* :256)
          (tvision::*rgb-theme* tvision:+theme-vga+))
      (is= "256-colour SGR" (attr->ansi a) (format nil "~c[0;38;5;227;48;5;19m" #\Escape)))
    ;; switching the theme changes the emitted RGB
    (let ((tvision::*color-mode* :truecolor)
          (tvision::*rgb-theme* tvision:+theme-modern+))
      (ok "theme swap changes RGB" (not (search "255;255;85" (attr->ansi a))))))
  (ok "detect-color-mode returns a known tier"
      (member (tvision::detect-color-mode) '(:truecolor :256 :16))))

(deftest unicode-input
  ;; the cell holds a full 21-bit code point (astral plane), not just the BMP
  (let* ((cp #x1F600)                       ; U+1F600 grinning face
         (a (make-rgb 1 2 3 4 5 6))
         (c (tvision::cell-make-code cp a)))
    (is= "astral code point survives in the cell" (tvision::cell-char-code c) cp)
    (is= "attr still survives alongside it" (tvision::cell-attr c) a))
  ;; UTF-8 input is assembled byte-by-byte into one code-point key event
  (flet ((decode (&rest bytes)
           (let ((b (make-array (length bytes) :element-type '(unsigned-byte 8)
                                               :initial-contents bytes)))
             (multiple-value-bind (ev n) (tvision::parse-utf8 b 0 (length b))
               (list (and ev (event-char-code ev)) n)))))
    (is= "2-byte UTF-8 (lambda)" (decode #xCE #xBB) (list #x3BB 2))
    (is= "3-byte UTF-8 (CJK)"    (decode #xE4 #xB8 #xAD) (list #x4E2D 3))
    (is= "4-byte UTF-8 (emoji)"  (decode #xF0 #x9F #x98 #x80) (list #x1F600 4))
    (is= "incomplete sequence waits" (decode #xF0 #x9F) (list nil nil))
    (is= "stray continuation byte consumed as-is" (decode #x80) (list #x80 1))))

(deftest wide-chars
  ;; double-width metrics
  (is= "wide CJK is 2 columns" (char-width #\中) 2)
  (is= "ASCII is 1 column" (char-width #\a) 1)
  (is= "string-width sums display widths" (string-width "中a中b") 6)
  ;; a wide glyph occupies two cells: the glyph + a continuation marker, so the
  ;; following character lands two columns over (no overlap)
  (let ((tv (focused (host (make-instance 'tmemo :bounds (make-trect 1 1 22 6))))))
    (set-text tv "中a中b")
    (setf (text-cur-col tv) 2)            ; after 中a
    (draw-view tv)
    (flet ((cc (x) (tvision::cell-char-code
                    (aref (screen-back-buffer *screen*) (tvision::screen-index *screen* x 1)))))
      (is= "中 at column 1" (cc 1) (char-code #\中))
      (ok  "continuation marker at column 2" (= (cc 2) tvision::+wide-cont+))
      (is= "a at column 3 (pushed past the wide glyph)" (cc 3) (char-code #\a))
      (ok  "second 中 + continuation" (and (= (cc 4) (char-code #\中)) (= (cc 5) tvision::+wide-cont+)))
      (is= "b at column 6" (cc 6) (char-code #\b)))
    (is= "cursor visual column after 中a is 3" (point-x (tvision::view-cursor tv)) 3)))

(deftest grapheme-clusters
  (let ((combining (concatenate 'string "e" (string (code-char #x301)) "ab"))  ; é + ab
        (fam (coerce (list (code-char #x1F468) (code-char #x200D) (code-char #x1F469)
                           (code-char #x200D) (code-char #x1F467)) 'string)))    ; 👨‍👩‍👧
    ;; boundary detection: the base+combining pair is one cluster
    (is= "cluster offsets" (tvision::grapheme-offsets combining) '(0 2 3 4))
    (is= "next boundary skips the combining mark" (tvision::next-grapheme-col combining 0) 2)
    (is= "prev boundary skips the combining mark" (tvision::prev-grapheme-col combining 2) 0)
    (is= "ASCII line stays per-character" (tvision::next-grapheme-col "abc" 0) 1)
    ;; a ZWJ emoji sequence is a single, double-width cluster
    (is= "family emoji is one cluster" (length (sb-unicode:graphemes fam)) 1)
    (is= "family emoji is width 2" (tvision::grapheme-width fam) 2)
    ;; render it: one interned cluster cell + a continuation, then the next char
    (let ((tv (focused (host (make-instance 'tmemo :bounds (make-trect 1 1 30 6))))))
      (set-text tv (concatenate 'string fam "X"))
      (draw-view tv)
      (flet ((cc (x) (tvision::cell-char-code
                      (aref (screen-back-buffer *screen*) (tvision::screen-index *screen* x 1)))))
        (ok  "cluster cell at column 1" (tvision::cluster-code-p (cc 1)))
        (is= "cluster cell holds the whole sequence" (tvision::cluster-string (cc 1)) fam)
        (ok  "continuation at column 2" (= (cc 2) tvision::+wide-cont+))
        (is= "X lands at column 3" (cc 3) (char-code #\X))))
    ;; backspace deletes the whole preceding cluster, not one code point
    (let ((tv (focused (host (make-instance 'tmemo :bounds (make-trect 1 1 30 6))))))
      (set-text tv (concatenate 'string "x" "e" (string (code-char #x301))))    ; xé
      (setf (text-cur-col tv) 3)
      (tvision::delete-char-before-cursor tv)
      (is= "backspace removes the é cluster" (nth-line tv 0) "x")
      (is= "cursor at the cluster boundary" (text-cur-col tv) 1))))

(deftest unicode-wrap
  ;; word-wrap geometry honours display width and grapheme boundaries
  (is= "ASCII wraps at the width boundary" (tvision::wrap-segments "abcdefg" 4) '(0 4))
  (is= "an exactly-full line gets a trailing cursor row" (tvision::wrap-segments "abcd" 4) '(0 4))
  (is= "an empty line is one row" (tvision::wrap-segments "" 4) '(0))
  (let ((cjk "中中中"))                          ; three width-2 glyphs, total width 6
    (is= "a wide glyph never straddles the boundary" (tvision::wrap-segments cjk 4) '(0 2))
    (is= "the first row fills exactly four columns" (tvision::visual-col cjk 0 2) 4)
    (is= "vcol 0 maps back to col 0" (tvision::col-at-vcol cjk 0 3 0) 0)
    (is= "vcol 2 maps to the second glyph" (tvision::col-at-vcol cjk 0 3 2) 1)
    (is= "a column mid-glyph snaps back to its start" (tvision::col-at-vcol cjk 0 3 3) 1))
  ;; render wide glyphs in wrap mode: no straddle, continuation markers intact
  (let ((tv (focused (host (make-instance 'tmemo :bounds (make-trect 0 0 4 6))))))
    (setf (tvision::text-wrap tv) t)
    (set-text tv "中中中")
    (draw-view tv)
    (flet ((cc (x y) (tvision::cell-char-code
                      (aref (screen-back-buffer *screen*) (tvision::screen-index *screen* x y)))))
      (ok  "row 0: glyph + continuation, twice"
           (and (= (cc 0 0) (char-code #\中)) (= (cc 1 0) tvision::+wide-cont+)
                (= (cc 2 0) (char-code #\中)) (= (cc 3 0) tvision::+wide-cont+)))
      (ok  "row 1: the wrapped third glyph"
           (and (= (cc 0 1) (char-code #\中)) (= (cc 1 1) tvision::+wide-cont+))))
    ;; cursor Down/Up moves by visual row, keeping the goal display column
    (setf (text-cur-line tv) 0 (text-cur-col tv) 0 (tvision::text-goal-col tv) nil)
    (tvision::%wrap-vmove tv +1)
    (is= "Down stays on the logical line" (text-cur-line tv) 0)
    (is= "Down lands on the wrapped glyph" (text-cur-col tv) 2)
    (tvision::%wrap-vmove tv -1)
    (is= "Up returns to the first glyph" (text-cur-col tv) 0)))

(deftest hello-file
  ;; A self-contained slice of the Emacs HELLO torture file -- wide CJK, an emoji,
  ;; combining marks, a conjunct Indic cluster and astral-plane (>U+FFFF) Gothic --
  ;; driven through the real editor view in both flat and word-wrapped modes.
  ;; (Astral/emoji built with CODE-CHAR so the test never depends on source-file
  ;; encoding; BMP scripts are literal, matching the other Unicode tests.)
  (let* ((wave (string (code-char #x1F44B)))                              ; 👋
         (goth (coerce (list (code-char #x10332) (code-char #x10330)
                             (code-char #x10339)) 'string))               ; 𐌲𐌰𐌹 (Gothic)
         (lines (list "English	Hello"
                      "Greek	Γειά σας"
                      "Hebrew	שָׁלוֹם"
                      "Devanagari	नमस्ते"
                      "Balinese	ᬒᬁᬲ᭄ᬯᬲ᭄ᬢ᭄ᬬᬲ᭄ᬢᬸ"
                      "East Asia	你好 こんにちは 안녕하세요"
                      (concatenate 'string "Emoji	" wave)
                      (concatenate 'string "Gothic	" goth)))
         (text (format nil "~{~a~^~%~}" lines))
         (n (length lines)))
    ;; load every line
    (let ((m (focused (host (make-instance 'tmemo :bounds (make-trect 0 0 60 12))))))
      (set-text m text)
      (is= "loads every line" (line-count m) n)
      ;; emoji is double-width and its astral code point survives the 21-bit cell
      (is= "wave emoji is double-width" (char-width (code-char #x1F44B)) 2)
      (let ((eline (nth-line m 6)))
        (is= "emoji code point round-trips" (char-code (char eline (1- (length eline)))) #x1F44B))
      ;; astral-plane Gothic round-trips
      (let ((gline (nth-line m 7)))
        (is= "astral Gothic round-trips" (char-code (char gline (search goth gline))) #x10332))
      ;; a conjunct cluster spans fewer display columns than code points
      (let* ((dline (nth-line m 3)) (p (search "नमस्ते" dline)))
        (ok "found the Devanagari cluster" p)
        (when p (ok "conjunct cluster folds combining marks into fewer columns"
                    (< (tvision::visual-col dline p (+ p 6)) 6)))))
    ;; render every screen, flat and wrapped, without error
    (dolist (wrap '(nil t))
      (let ((m (focused (host (make-instance 'tmemo :bounds (make-trect 0 0 30 6))))))
        (setf (tvision::text-wrap m) (and wrap t))
        (set-text m text)
        (ok (format nil "renders without error (~:[flat~;wrap~])" wrap)
            (handler-case
                (progn (dotimes (top n)
                         (setf (tvision::text-top-line m) top (text-cur-line m) top (text-cur-col m) 0)
                         (draw-view m))
                       t)
              (error () nil)))
        ;; Down reaches the last line in both modes
        (setf (text-cur-line m) 0 (text-cur-col m) 0)
        (dotimes (i (* 2 n)) (press-key m +kb-down+))
        (is= (format nil "Down reaches the last line (~:[flat~;wrap~])" wrap)
             (text-cur-line m) (1- n))))
    ;; cursor Right is grapheme-atomic: monotonic, reaches end, and the Balinese
    ;; line (many subjoined consonants) takes fewer steps than it has code points
    (let* ((m (focused (host (make-instance 'tmemo :bounds (make-trect 0 0 60 12))))))
      (set-text m text)
      (setf (text-cur-line m) 4 (text-cur-col m) 0)
      (let* ((line (nth-line m 4)) (len (length line)) (steps 0) (last -1) (ok-mono t))
        (loop while (< (text-cur-col m) len) do
          (press-key m +kb-right+)
          (unless (> (text-cur-col m) last) (setf ok-mono nil) (return))
          (setf last (text-cur-col m)) (incf steps))
        (ok "Right advances monotonically by grapheme" ok-mono)
        (is= "Right reaches end of the Balinese line" (text-cur-col m) len)
        (ok "grapheme steps are fewer than code points" (< steps len))))))

(deftest truecolor-attrs
  ;; an RGB attr packs into the cell alongside the char, and reads back
  (let* ((a (make-rgb 255 128 0  10 20 30))
         (c (tvision::cell-make-code 65 a)))
    (ok "attr is tagged RGB" (attr-rgb-p a))
    (is= "char survives in the cell" (tvision::cell-char c) #\A)
    (is= "attr survives in the cell" (tvision::cell-attr c) a)
    (is= "fg unpacks" (attr-rgb-fg a) (pack-rgb 255 128 0))
    (is= "bg unpacks" (attr-rgb-bg a) (pack-rgb 10 20 30)))
  ;; interning: equal colours share an integer (so the diff renderer's `=' holds)
  (is= "equal RGB interns to one attr" (make-rgb 1 2 3 4 5 6) (make-rgb 1 2 3 4 5 6))
  (ok "different RGB -> different attr" (/= (make-rgb 1 2 3 4 5 6) (make-rgb 9 9 9 0 0 0)))
  ;; emission per mode
  (let ((a (make-rgb 255 128 0  10 20 30)))
    (let ((tvision::*color-mode* :truecolor))
      (is= "truecolour RGB SGR" (attr->ansi a)
           (format nil "~c[0;38;2;255;128;0;48;2;10;20;30m" #\Escape)))
    (let ((tvision::*color-mode* :256))
      (is= "256 RGB SGR" (attr->ansi a) (format nil "~c[0;38;5;208;48;5;16m" #\Escape)))
    ;; legacy attrs and RGB attrs coexist; legacy 16-colour output is unchanged
    (let ((tvision::*color-mode* :16))
      (is= "legacy attr still exact 16-colour"
           (attr->ansi (make-attr 14 1)) (format nil "~c[0;93;44m" #\Escape)))))

;;; ===========================================================================
;;; Draw buffer + a render round-trip
;;; ===========================================================================

(deftest draw-buffer
  (let ((db (make-draw-buffer 10)) (a (make-attr 7 0)))
    (db-fill db #\. a)
    (db-move-str db 2 "Hi" a)
    (is= "fill width" (db-width db) 10)))

(deftest render-static-text
  (let ((st (make-instance 'tstatic-text :text "Hello"
                           :bounds (make-trect 1 1 20 2))))
    (host st)
    (draw-view st)
    (is= "static text renders" (text-at 1 1 5) "Hello")))

(deftest label-mnemonic
  ;; TLabel: ~marker~ derives an Alt-hotkey, the ~ is stripped on screen, and
  ;; Alt-<letter> (or a click) hands focus to the linked control.
  (let* ((d  (make-instance 'tdialog :bounds (make-trect 0 0 40 10)))
         (i1 (make-instance 'tinputline :bounds (make-trect 10 2 30 3) :data ""))
         (i2 (make-instance 'tinputline :bounds (make-trect 10 4 30 5) :data ""))
         (lbl (make-instance 'tlabel :text "~N~ame" :link i2
                                     :bounds (make-trect 2 4 9 5))))
    (insert d i1) (insert d i2) (insert d lbl)
    (is= "hotkey is the marked letter" (tvision::label-hotkey lbl) #\n)
    ;; render strips the ~ markers
    (draw-view d)
    (is= "the ~ markers are not drawn" (text-at 2 4 4) "Name")
    ;; focus starts on the first field (focus only cascades from a focused group)
    (focused d)
    (tvision::set-current d i1 :normal-select)
    (ok "field 1 starts focused" (logtest (view-state i1) +sf-focused+))
    ;; a non-matching Alt key leaves focus alone
    (handle-event lbl (ev-key 0 (char-code #\z) tvision::+md-alt+))
    (ok "Alt-Z (no match) leaves focus on field 1" (logtest (view-state i1) +sf-focused+))
    ;; Alt-N jumps focus to the linked field
    (handle-event lbl (ev-key 0 (char-code #\n) tvision::+md-alt+))
    (ok "Alt-N moves focus to the linked field" (logtest (view-state i2) +sf-focused+))
    (ok "field 1 is no longer focused" (not (logtest (view-state i1) +sf-focused+)))))

;;; ===========================================================================
;;; Input line
;;; ===========================================================================

(deftest input-line
  (let ((il (focused (host (make-instance 'tinputline
                                          :bounds (make-trect 1 1 20 2) :maxlen 30)))))
    (type-char il #\a) (type-char il #\b) (type-char il #\c)
    (is= "typed text" (input-data il) "abc")
    (press-key il +kb-back+)
    (is= "backspace" (input-data il) "ab")
    (set-data il "xyz")
    (is= "set-data/get-data" (get-data il) "xyz")))

;;; ===========================================================================
;;; Clusters
;;; ===========================================================================

(deftest check-boxes
  (let ((c (focused (host (make-instance 'tcheck-boxes
                                         :labels '("~A~lpha" "~B~eta" "~G~amma")
                                         :bounds (make-trect 1 1 14 4))))))
    (ok "multi-state" (multi-state-p c))
    (cluster-press c 0)
    (cluster-press c 2)
    (ok "bit0 set" (logbitp 0 (cluster-value c)))
    (ok "bit2 set" (logbitp 2 (cluster-value c)))
    (ok "bit1 clear" (not (logbitp 1 (cluster-value c))))
    (cluster-press c 0)
    (ok "bit0 toggled off" (not (logbitp 0 (cluster-value c))))
    ;; space presses the focused row
    (setf (tvision::cluster-sel c) 1)
    (handle-event c (ev-key +kb-space+ +kb-space+))
    (ok "space toggled row1" (logbitp 1 (cluster-value c)))))

(deftest radio-buttons
  (let ((c (focused (host (make-instance 'tradio-buttons
                                         :labels '("One" "Two" "Three")
                                         :bounds (make-trect 1 1 14 4))))))
    (cluster-press c 2)
    (is= "radio value" (cluster-value c) 2)
    (cluster-press c 0)
    (is= "radio reselect" (cluster-value c) 0)
    (is= "single-state get-data" (get-data c) 0)))

(deftest multi-check-boxes
  (let ((c (focused (host (make-instance 'tmulti-check-boxes :states " ?X"
                                         :labels '("Read" "Write" "Exec")
                                         :bounds (make-trect 1 1 14 4))))))
    (is= "bits-per-item" (tvision::mcb-bits c) 2)
    (is= "initial state" (mcb-state c 0) 0)
    (cluster-press c 0)
    (is= "state after 1 press" (mcb-state c 0) 1)
    (cluster-press c 0)
    (is= "state after 2 presses" (mcb-state c 0) 2)
    (cluster-press c 0)
    (is= "wraps to 0" (mcb-state c 0) 0)
    (cluster-press c 1)
    (is= "item1 independent" (mcb-state c 1) 1)
    (is= "item0 still 0" (mcb-state c 0) 0)
    (cluster-press c 0)
    (is= "mark glyph for state1" (cluster-mark c 0) "[?] ")))

;;; ===========================================================================
;;; Buttons (command dispatch through the owner)
;;; ===========================================================================

(deftest button
  (let* ((rec (make-instance 'recorder :bounds (make-trect 0 0 40 10)))
         (b (focused (make-button (make-trect 1 1 11 3) "~O~K" +cm-ok+ t))))
    (insert rec b)
    (is= "title" (button-title b) "~O~K")
    (is= "command" (button-command b) +cm-ok+)
    (handle-event b (ev-key +kb-space+ +kb-space+))
    (ok "space fires a command event"
        (some (lambda (e) (and (= (event-type e) +ev-command+)
                               (= (event-command e) +cm-ok+)))
              (rec-events rec)))))

;;; ===========================================================================
;;; List box + sorted (type-ahead) list box
;;; ===========================================================================

(deftest list-box
  (let ((lb (focused (host (make-instance 'tlist-box :items '("a" "b" "c" "d")
                                          :bounds (make-trect 1 1 12 6))))))
    (is= "count" (list-count lb) 4)
    (is= "item 2" (list-item lb 2) "c")
    (is= "initial focus" (list-focused lb) 0)
    (press-key lb +kb-down+) (press-key lb +kb-down+)
    (is= "down twice" (list-focused lb) 2)
    (press-key lb +kb-up+)
    (is= "up once" (list-focused lb) 1)
    (press-key lb +kb-end+)
    (is= "end" (list-focused lb) 3)
    (press-key lb +kb-home+)
    (is= "home" (list-focused lb) 0)))

(deftest sorted-list-box
  (let ((lb (focused (host (make-instance 'tsorted-list-box
                                          :items '("alpha" "banana" "beta" "gamma")
                                          :bounds (make-trect 1 1 14 6))))))
    (is= "find prefix b" (slb-find lb "b") 1)
    (is= "find prefix be" (slb-find lb "be") 2)
    (is= "find miss" (slb-find lb "z") nil)
    ;; type-ahead through events
    (type-char lb #\b)
    (is= "type b focuses banana" (list-focused lb) 1)
    (type-char lb #\e)
    (is= "type be focuses beta" (list-focused lb) 2)
    (is= "search buffer" (slb-search lb) "be")
    (press-key lb +kb-down+)
    (is= "nav resets search" (slb-search lb) "")))

;;; ===========================================================================
;;; Table view (sortable grid)
;;; ===========================================================================

(deftest table-view
  (let* ((rows (list (list :name "bob" :n 12)
                     (list :name "amy" :n 30)
                     (list :name "cy"  :n 5)))
         (cols (vector (make-table-column "N" 6 (lambda (r) (getf r :n)) :numeric t)
                       (make-table-column "Name" 10 (lambda (r) (getf r :name)))))
         (tv (focused (host (make-instance 'ttable-view :columns cols :rows rows
                                           :sort-col 0 :sort-asc nil
                                           :bounds (make-trect 1 1 22 8))))))
    ;; default: numeric column 0, descending -> 30 12 5
    (is= "sort N desc" (mapcar (lambda (r) (getf r :n)) (coerce (table-rows tv) 'list))
         '(30 12 5))
    (is= "selected top" (getf (table-selected-row tv) :name) "amy")
    ;; toggle direction on the same column -> ascending
    (table-sort-by tv 0)
    (is= "sort N asc" (mapcar (lambda (r) (getf r :n)) (coerce (table-rows tv) 'list))
         '(5 12 30))
    (ok "sort-asc flag" (table-sort-asc tv))
    ;; sort by the string column -> alphabetical ascending by default
    (table-sort-by tv 1)
    (is= "sort by name" (mapcar (lambda (r) (getf r :name)) (coerce (table-rows tv) 'list))
         '("amy" "bob" "cy"))
    (is= "sort-col is 1" (table-sort-col tv) 1)
    ;; keyboard navigation
    (press-key tv +kb-down+) (press-key tv +kb-down+)
    (is= "down twice focuses cy" (getf (table-selected-row tv) :name) "cy")
    (press-key tv +kb-home+)
    (is= "home focuses amy" (getf (table-selected-row tv) :name) "amy")))

;;; ===========================================================================
;;; Scroller
;;; ===========================================================================

(deftest scroller
  (let ((sc (host (make-instance 'tscroller :bounds (make-trect 1 1 11 6)))))
    (set-scroller-limit sc 100 100)
    (scroll-to sc 5 7)
    (is= "scroll x" (point-x (scroller-delta sc)) 5)
    (is= "scroll y" (point-y (scroller-delta sc)) 7)
    (scroll-to sc -10 -10)
    (is= "clamp low" (list (point-x (scroller-delta sc)) (point-y (scroller-delta sc)))
         '(0 0))))

;;; ===========================================================================
;;; Event-loop resilience: a handler error must not escape the loop
;;; ===========================================================================

(deftest loop-error-containment
  (let ((v (make-instance 'exploding-view :bounds (make-trect 0 0 5 5)))
        (ev (ev-key (char-code #\a) (char-code #\a))))
    ;; with no hook, the error is swallowed (written to *error-output*) -- the
    ;; call must return normally rather than signal
    (let ((tvision::*event-error-hook* nil)
          (*error-output* (make-string-output-stream)))
      (ok "no-hook: error contained, returns normally"
          (progn (tvision::%handle-loop-event v ev) t)))
    ;; with a hook, it is invoked with the condition
    (let* ((seen nil)
           (tvision::*event-error-hook* (lambda (c) (setf seen c))))
      (tvision::%handle-loop-event v ev)
      (ok "hook receives the condition" (typep seen 'error))
      (ok "condition is the expected one" (search "boom" (princ-to-string seen))))))

;;; ===========================================================================
;;; Outline (tree)
;;; ===========================================================================

(deftest outline
  (let* ((leaf (make-outline-node "leaf" '()))
         (root (make-outline-node "root" (list leaf)))
         (ol (focused (host (make-instance 'toutline :roots (list root)
                                           :bounds (make-trect 1 1 20 8))))))
    (is= "root text" (outline-node-text root) "root")
    (setf (outline-node-expanded root) t)
    (ok "expanded flag" (outline-node-expanded root))
    (outline-toggle ol)
    (ok "toggle flips expanded" (not (outline-node-expanded root)))))

(deftest inspector-tree
  ;; object->outline stores each value in its node so the inspector can drill in
  (let* ((obj (list 10 20 (list 30 40)))
         (node (tvision::object->outline obj "*")))
    (is= "root holds the object" (outline-node-data node) obj)
    (is= "three children" (length (outline-node-children node)) 3)
    (let ((third (third (outline-node-children node))))
      (is= "child label" (subseq (outline-node-text third) 0 3) "[2]")
      (is= "child holds the sub-list" (outline-node-data third) (third obj))
      (is= "sub-list has two children" (length (outline-node-children third)) 2))
    ;; strings are leaves (not exploded char by char), but still carry their value
    (let ((sn (tvision::object->outline "hi" "s")))
      (is= "string node value" (outline-node-data sn) "hi")
      (ok "string node has no children" (null (outline-node-children sn))))))

(deftest inspector-cycles
  ;; a value that points back to an ancestor object is rendered as a leaf marked
  ;; [circular ref], not expanded again (so the inspector can't loop on cycles)
  (let ((v (vector 1 2 nil)))
    (setf (aref v 2) v)                       ; v[2] = v -> a reference cycle
    (let* ((node (tvision::object->outline v "v"))
           (kids (outline-node-children node)))
      (is= "vector shows its three slots" (length kids) 3)
      (let ((back (third kids)))
        (ok "the self-reference is marked circular"
            (search "[circular ref]" (outline-node-text back)))
        (ok "the circular node is a leaf (not re-expanded)"
            (null (outline-node-children back)))
        (is= "the circular node still carries the value" (outline-node-data back) v))))
  ;; a shared-but-acyclic value is NOT mistaken for a cycle (siblings re-expand)
  (let* ((shared (list 7))
         (node (tvision::object->outline (list shared shared) "pair"))
         (kids (outline-node-children node)))
    (is= "both siblings present" (length kids) 2)
    (ok "neither sibling is flagged circular"
        (notany (lambda (k) (search "[circular ref]" (outline-node-text k))) kids))))

(deftest inspector-paging
  ;; big collections show one page plus a drillable "... N more" node, instead of
  ;; silently truncating; re-inspecting that node's value pages the remainder
  (let* ((cap tvision::+inspect-page+)
         (big (loop for i below (+ cap 50) collect i))
         (node (tvision::object->outline big "big"))
         (kids (outline-node-children node)))
    (is= "one page of elements plus an overflow node" (length kids) (1+ cap))
    (let ((more (car (last kids))))
      (ok "overflow node is labelled '... more'" (search "more" (outline-node-text more)))
      (is= "overflow carries the un-shown tail" (outline-node-data more) (nthcdr cap big))
      (ok "overflow node is itself a leaf" (null (outline-node-children more)))
      ;; drilling the overflow re-inspects the tail -> the remaining 50 elements
      (let ((page2 (tvision::object->outline (outline-node-data more) "rest")))
        (is= "drilling the overflow pages the rest"
             (length (outline-node-children page2)) 50))))
  ;; a hash-table over the cap reports the overflow count too
  (let ((h (make-hash-table)))
    (dotimes (i (+ tvision::+inspect-page+ 5)) (setf (gethash i h) i))
    (let ((kids (outline-node-children (tvision::object->outline h "h"))))
      (is= "hash-table page plus overflow" (length kids) (1+ tvision::+inspect-page+))
      (ok "overflow counts the rest" (search "5 more" (outline-node-text (car (last kids))))))))

(deftest inspector-back
  ;; drilling re-roots in place and records history; Backspace restores the
  ;; previous object (one window, not a pile of them)
  (let* ((host (make-instance 'twindow :bounds (make-trect 0 0 60 20)))
         (w  (make-instance 'tvision::tinspector-window :bounds (make-trect 0 0 50 16)))
         (ol (make-instance 'toutline
                            :roots (list (tvision::object->outline (list 10 20) "root"))
                            :bounds (make-trect 1 1 48 14))))
    (setf (tvision::inspector-outline w) ol
          (tvision::inspector-current w) (cons (list 10 20) "root"))
    (insert w ol) (insert host w)
    (let ((child (first (outline-node-children (first (outline-roots ol))))))  ; the [0] node
      (is= "drill target is the [0] node" (tvision::%node-label child) "[0]")
      (tvision::%inspector-drill w child)
      (is= "drilling records one history entry" (length (tvision::inspector-history w)) 1)
      (is= "current view is the drilled value" (cdr (tvision::inspector-current w)) "[0]")
      (tvision::%inspector-back w)
      (is= "back empties the history" (length (tvision::inspector-history w)) 0)
      (is= "back restores the previous view" (cdr (tvision::inspector-current w)) "root"))))

(deftest restart-labels
  ;; the debugger labels restarts with their NAME plus their report description
  (restart-case
      (let ((named (find-restart 'retry-now))
            (anon  (find-restart 'plain)))
        (let ((l (tvision::%restart-label named)))
          (ok "named restart shows its symbolic name" (search "RETRY-NOW" l))
          (ok "named restart shows its report text" (search "Retry the operation" l)))
        (ok "restart label is non-empty even without a useful report"
            (plusp (length (tvision::%restart-label anon)))))
    (retry-now () :report "Retry the operation" nil)
    (plain () nil)))

;;; ===========================================================================
;;; HTML view (hypertext browser)
;;; ===========================================================================

(defun %html-text (v)
  "Flatten the rendered lines of an HTML view into one newline-joined string."
  (with-output-to-string (s)
    (loop for ln across (tvision::html-lines v) do
      (loop for r in ln do (write-string (tvision::html-run-text r) s))
      (terpri s))))

(deftest html-anchors-find
  (let ((v (focused (host (make-instance 'thtml-view :bounds (make-trect 0 0 40 10))))))
    (set-html v "<p id=top>Top</p><h2 id=sec>Section</h2><p>body 123 end</p>")
    (ok "id anchors recorded" (assoc "sec" (tvision::html-anchors v) :test #'string=))
    (ok "goto a known anchor succeeds" (html-goto-anchor v "sec"))
    (ok "goto a missing anchor returns nil" (not (html-goto-anchor v "nope")))
    (is= "regex find-in-page counts the digit run" (html-find-regex v "[0-9]+") 1)
    (is= "extended entity decodes" (tvision::%html-decode-string "a&le;b") "a<=b")))

(deftest html-view
  (let* ((src "<h1>Title</h1>
<p>Hello <b>bold</b> &amp; <a href=\"a.htm\">one</a> and
<a href=\"b.htm\">two</a>.</p>
<pre>code  line</pre>")
         (v (focused (host (make-instance 'thtml-view :html src
                                          :bounds (make-trect 0 0 40 12))))))
    (is= "two links" (html-link-count v) 2)
    (is= "no focus initially" (html-focused-link v) nil)
    (html-next-link v 1)
    (is= "first link href" (html-current-href v) "a.htm")
    (html-next-link v 1)
    (is= "second link href" (html-current-href v) "b.htm")
    (html-next-link v 1)
    (is= "next wraps to first" (html-current-href v) "a.htm")
    (html-next-link v -1)
    (is= "prev wraps to last" (html-current-href v) "b.htm")
    (let ((text (%html-text v)))
      (ok "entity decoded, inline flow" (search "Hello bold & one and two." text))
      (ok "heading text present" (search "Title" text))
      (ok "pre preserves double space" (search "code  line" text)))
    ;; find-in-page
    (is= "find 'and' -> 1 match" (html-find v "and") 1)
    (ok "match recorded" (tvision::html-matches v))
    (is= "find 'l' counts occurrences" (html-find v "l")
         (let ((n 0)) (loop for ln across (tvision::html-lines v) do
                        (loop for r in ln do
                          (loop for c across (string-downcase (tvision::html-run-text r))
                                when (char= c #\l) do (incf n))))
                      n))
    (is= "miss -> 0" (html-find v "zzqq") 0)
    (is= "miss clears match index" (tvision::html-match-index v) nil)
    ;; the focused link renders in the focus colour (6); other links in the
    ;; link colour (5); normal/heading/code runs in their own colours
    (flet ((link-run (id)
             (loop for ln across (tvision::html-lines v)
                   thereis (find id ln :key #'tvision::html-run-link)))
           (style-run (style)
             (loop for ln across (tvision::html-lines v)
                   thereis (find-if (lambda (r) (and (null (tvision::html-run-link r))
                                                     (eq (tvision::html-run-style r) style)))
                                    ln))))
      (html-focus-link v 0)
      (is= "focused link uses highlight colour 6" (tvision::%html-run-color v (link-run 0)) 6)
      (is= "unfocused link uses link colour 5"    (tvision::%html-run-color v (link-run 1)) 5)
      (html-focus-link v 1)
      (is= "focus moves: link 1 now highlighted"  (tvision::%html-run-color v (link-run 1)) 6)
      (is= "and link 0 back to link colour"       (tvision::%html-run-color v (link-run 0)) 5)
      (is= "heading run uses colour 3"            (tvision::%html-run-color v (style-run :heading)) 3)
      (is= "code run uses colour 4"               (tvision::%html-run-color v (style-run :code)) 4))
    ;; reload replaces content and clears focus
    (set-html v "<p><a href=\"z.htm\">only</a></p>")
    (is= "reloaded link count" (html-link-count v) 1)
    (is= "reload clears focus" (html-focused-link v) nil)))

;;; ===========================================================================
;;; Lisp syntax highlighting (editor)
;;; ===========================================================================

(deftest syntax-highlight
  ;; matching-paren scan over a string (offsets); strings/comments are skipped
  (is= "match forward"  (tvision::%paren-match-offset "(a (b) c)" 0) 8)
  (is= "match backward" (tvision::%paren-match-offset "(a (b) c)" 8) 0)
  (is= "match inner"    (tvision::%paren-match-offset "(a (b) c)" 3) 5)
  (is= "paren inside a string is skipped" (tvision::%paren-match-offset "(foo \")\")" 0) 8)
  ;; colouriser: comment / string / keyword differ from the base attribute
  (let* ((base (tvision::make-attr 0 3))            ; black on cyan = the editor base
         (src "(a :kw \"s\") ; c"))
    (multiple-value-bind (attrs instr) (tvision::%lisp-colorize src base nil)
      (ok "line not left in a string" (not instr))
      (ok "plain symbol char stays base" (= (aref attrs 1) base))   ; the 'a'
      (ok "keyword coloured"  (/= (aref attrs (search ":kw" src)) base))
      (ok "string coloured"   (/= (aref attrs (search "\"s\"" src)) base))
      (ok "comment coloured"  (/= (aref attrs (search "; c" src)) base)))
    ;; an unterminated string carries the state to the next line
    (multiple-value-bind (attrs instr) (tvision::%lisp-colorize "\"open" base nil)
      (declare (ignore attrs))
      (ok "unterminated string carries over" instr)))
  ;; auto-indent (per-symbol specs after cl-indent)
  (flet ((ind (s) (tvision::%lisp-indent-at s (length s))))
    (is= "defun body indents +2"      (ind "(defun f (x)") 2)
    (is= "let body indents +2"        (ind "  (let ((x 1))") 4)
    (is= "args align under first arg" (ind "(foo bar") 5)
    (is= "bare open indents +1"       (ind "(") 1)
    (is= "operator alone -> +1"       (ind "(foo") 1)
    (is= "closed form -> 0"           (ind "(foo)") 0)
    (is= "paren in string ignored"    (ind "(foo \";)\" ") 5)
    (is= "if distinguished arg +4"    (ind "(if test") 4)
    (is= "with-open-file body +2"     (ind "(with-open-file (s p)") 2)
    (is= "cond clauses +2"            (ind "(cond") 2)
    (is= "loop clauses align under first clause" (ind "(loop for x in xs") 6)
    (is= "loop conditional body indents +2"
         (ind (format nil "(loop for x in xs~%      when (evenp x)")) 8)
    (is= "loop returns to clause col after an action"
         (ind (format nil "(loop for x in xs~%      when (evenp x) collect x")) 6)
    (is= "literal list aligns under first element" (ind "(1 2 3") 1)
    (is= "binding list aligns under first binding" (ind "(let ((a 1) (b 2)") 6)
    (is= "quoted list aligns under first element" (ind "'(aa bb") 2)
    (is= "backquoted list aligns under first element" (ind "`(aa bb") 2)
    (is= "nested quoted list is data" (ind "'(foo (bar baz") 7))
  ;; lisp-indent-sexp reflows a whole top-level form
  (let ((ed (host (make-instance 'tfile-editor :bounds (make-trect 0 0 40 12)
                                 :text (format nil "(defun f ()~%(when x~%(foo)))")))))
    (lisp-indent-sexp ed)
    (is= "defun line unchanged" (nth-line ed 0) "(defun f ()")
    (is= "when reindented to +2" (nth-line ed 1) "  (when x")
    (is= "body reindented to +4" (nth-line ed 2) "    (foo)))")))

;;; ===========================================================================
;;; Memo + editor
;;; ===========================================================================

(deftest regex-find
  (let ((tv (focused (host (make-instance 'tmemo :bounds (make-trect 0 0 40 8))))))
    (set-text tv (format nil "(defun foo ())~%  (defvar *x* 42)~%bar123baz"))
    ;; anchored match
    (is= "^.defun matches only the first line" (tvision::text-find-regex tv "^.defun") '(0 0 6))
    ;; no match on lines that don't start with it
    (is= "anchor respected (line 1 indented)" (tvision::text-find-regex tv "^.defvar") nil)
    ;; character class + quantifier
    (is= "[0-9]+ finds the digit run" (tvision::text-find-regex tv "[0-9]+" :from-line 2 :from-col 0)
         '(2 3 6))
    ;; dot-star is greedy within the line (first 'f' is in 'defun')
    (is= "f.*o is greedy within the line" (tvision::text-find-regex tv "f.*o" :from-line 0 :from-col 0)
         '(0 3 10))
    ;; \d escape
    (is= "\\d+ matches digits" (tvision::text-find-regex tv "\\d+" :from-line 1 :from-col 0)
         '(1 14 16))
    ;; replace-all with regex
    (set-text tv "a1b22c333")
    (is= "regex replace count" (text-replace-all-regex tv "[0-9]+" "#") 3)
    (is= "regex replace result" (nth-line tv 0) "a#b#c#")))

(deftest match-paren
  (let ((tv (focused (host (make-instance 'tmemo :bounds (make-trect 0 0 30 5))))))
    (set-text tv "(foo (bar))")
    (setf (text-cur-line tv) 0 (text-cur-col tv) 0)        ; on the outer (
    (ok "jumps from an open paren" (match-paren-jump tv))
    (is= "lands on the matching close paren" (text-cur-col tv) 10)
    (ok "jumps back from the close paren" (match-paren-jump tv))
    (is= "returns to the open paren" (text-cur-col tv) 0)
    (setf (text-cur-col tv) 2)                             ; on 'o' of foo
    (ok "no jump when point isn't on a paren" (not (match-paren-jump tv)))))

(deftest memo
  (let ((m (host (make-instance 'tmemo :bounds (make-trect 1 1 30 6)))))
    (set-data m (format nil "one~%two~%three"))
    (is= "memo line count" (line-count m) 3)
    (is= "memo round-trip" (get-data m) (format nil "one~%two~%three"))))

(deftest find-replace
  (let ((m (focused (host (make-instance 'tmemo :bounds (make-trect 1 1 40 6))))))
    (set-data m (format nil "(foo a)~%(bar)~%(foo c)"))
    ;; find selects the next match (wrapping)
    (ok "find first foo" (text-find-and-select m "foo" :wrap t))
    (is= "match on line 0" (text-cur-line m) 0)
    (ok "find next foo" (text-find-and-select m "foo" :wrap t))
    (is= "match on line 2" (text-cur-line m) 2)
    (ok "find missing returns nil" (not (text-find-and-select m "zzz" :wrap t)))
    ;; replace-all returns the count and rewrites the buffer
    (is= "replaced both foos" (text-replace-all m "foo" "baz") 2)
    (ok "buffer updated" (search "(baz a)" (get-data m)))
    (ok "no foo remains" (not (search "foo" (get-data m))))
    ;; an empty replacement deletes the matches
    (is= "delete via empty replacement" (text-replace-all m "baz" "") 2)
    (ok "baz gone" (not (search "baz" (get-data m))))))

;;; ===========================================================================
;;; Validators
;;; ===========================================================================

(deftest validators
  (let ((f (make-filter-validator "0123456789")))
    (ok "filter accepts digits" (is-valid-input f "123"))
    (ok "filter rejects letters" (not (is-valid-input f "12a"))))
  (let ((r (make-range-validator 1 100)))
    (ok "range accepts in-range" (is-valid r "50"))
    (ok "range rejects over" (not (is-valid r "200"))))
  (let ((v (make-string-lookup-validator '("red" "green" "blue"))))
    (ok "lookup is a TLookupValidator" (typep v 'tlookup-validator))
    (ok "lookup accepts member" (is-valid v "green"))
    (ok "lookup rejects non-member" (not (is-valid v "purple")))
    (ok "lookup input accepts prefix" (is-valid-input v "gr"))
    (ok "validator-lookup direct" (validator-lookup v "red"))))

;;; ===========================================================================
;;; Collections
;;; ===========================================================================

(deftest collections
  (let ((c (make-collection)))
    (insert-item c "a") (insert-item c "b") (insert-item c "c")
    (is= "count" (collection-count c) 3)
    (is= "at 1" (at c 1) "b")
    (is= "index-of" (index-of c "c") 2)
    (delete-item c "b")
    (is= "count after delete" (collection-count c) 2)
    (is= "shifted" (at c 1) "c"))
  (let ((s (make-sorted-collection :compare #'string<)))
    (insert-item s "gamma") (insert-item s "alpha") (insert-item s "beta")
    (is= "sorted order" (list (at s 0) (at s 1) (at s 2)) '("alpha" "beta" "gamma"))))

;;; ===========================================================================
;;; History
;;; ===========================================================================

(deftest history
  (history-clear "test-id")
  (history-add "test-id" "first")
  (history-add "test-id" "second")
  (history-add "test-id" "first")          ; moves to front, de-duped
  (is= "history order" (history-list "test-id") '("first" "second"))
  (history-clear "test-id")
  (is= "history cleared" (history-list "test-id") nil))

;;; ===========================================================================
;;; Menus + TMenuPopup
;;; ===========================================================================

(deftest menus
  (let ((menu (new-menu
               (menu-item "~O~pen" 100 :key-code +kb-f3+)
               (menu-separator)
               (sub-menu "~M~ore" (new-menu (menu-item "~D~eep" 200 :key-code +kb-f4+))))))
    (is= "find top-level shortcut" (find-shortcut menu +kb-f3+) 100)
    (is= "find nested shortcut" (find-shortcut menu +kb-f4+) 200)
    (is= "missing shortcut" (find-shortcut menu +kb-f9+) nil)
    (multiple-value-bind (w h) (tvision::box-dims menu)
      (ok "box width sane" (> w 8))
      (ok "box height = items+2" (= h (+ 3 2))))
    (let ((mp (make-menu-popup menu 5 5)))
      (ok "popup carries menu" (eq (menu-popup-menu mp) menu))
      (multiple-value-bind (pw ph) (menu-popup-size mp)
        (declare (ignore ph))
        (ok "popup size matches box" (> pw 8))))))

;;; ===========================================================================
;;; Color controls
;;; ===========================================================================

(deftest color-selector
  (let ((cs (focused (host (make-instance 'tcolor-selector :color 0 :range 16
                                          :bounds (make-trect 1 1 17 5))))))
    (press-key cs +kb-right+)
    (is= "right moves +1" (cs-color cs) 1)
    (press-key cs +kb-down+)
    (is= "down moves +4" (cs-color cs) 5)
    (press-key cs +kb-left+)
    (is= "left moves -1" (cs-color cs) 4)
    (press-key cs +kb-up+)
    (is= "up moves -4" (cs-color cs) 0)
    (set-data cs 11)
    (is= "set-data" (get-data cs) 11)))

(deftest mono-selector
  (let ((ms (make-mono-selector (make-trect 1 1 16 5) 0)))
    (is= "normal attr" (mono-selector-attr ms) #x07)
    (cluster-press ms 3)
    (is= "inverse attr" (mono-selector-attr ms) #x70)))

;;; ===========================================================================
;;; File-dialog helpers (wildcard match + filtered listing + filter apply)
;;; ===========================================================================

(deftest wild-match
  (ok "*.lisp matches foo.lisp" (tvision::%wild-match "*.lisp" "foo.lisp"))
  (ok "*.lisp rejects foo.txt" (not (tvision::%wild-match "*.lisp" "foo.txt")))
  (ok "? matches one char" (tvision::%wild-match "a?c" "abc"))
  (ok "? needs a char" (not (tvision::%wild-match "a?c" "ac")))
  (ok "* matches all" (tvision::%wild-match "*" "anything"))
  (ok "case-insensitive" (tvision::%wild-match "*.LISP" "foo.lisp")))

(deftest file-dialog
  (let ((d (make-file-dialog "Open" :directory (truename "."))))
    (ok "uses TFileInputLine" (typep (tvision::fd-input d) 'tfile-input-line))
    (ok "has TFileInfoPane" (typep (tvision::fd-info d) 'tfile-info-pane))
    (is= "default filter" (fd-filter d) "*")
    (ok "list populated" (> (list-count (tvision::fd-list d)) 0))
    (ok "list has parent entry"
        (string= (list-item (tvision::fd-list d) 0) ".."))
    ;; apply a wildcard filter: only .asd files remain among non-dir entries
    (tvision::fd-apply-filter d "*.asd")
    (let ((files (remove-if (lambda (x) (or (string= x "..") (find #\/ x)))
                            (loop for i below (list-count (tvision::fd-list d))
                                  collect (list-item (tvision::fd-list d) i)))))
      (ok "filter narrows to .asd files"
          (and files (every (lambda (f) (tvision::%wild-match "*.asd" f)) files))))))

(deftest file-dialog-navigation
  (let ((d (make-file-dialog "Open" :directory (truename "."))))
    (flet ((items () (loop for i below (list-count (tvision::fd-list d))
                           collect (list-item (tvision::fd-list d) i)))
           (ok-cmd () (handle-event d (make-event :type +ev-command+ :command +cm-ok+))))
      (setf (tvision::group-current d) (tvision::fd-input d))   ; Name field focused
      ;; typing a bare subdirectory name and pressing OK enters it and updates
      ;; the listing to that directory (resolved against the current dir)
      (set-data (tvision::fd-input d) "src")
      (ok-cmd)
      (ok "relative dir name navigates into src"
          (search "/src/" (namestring (tvision::fd-dir d))))
      (ok "listing updates to src contents"
          (member "package.lisp" (items) :test #'string=))
      (ok "a directory is never accepted as a file"
          (not (member "src" (items) :test #'string=)))  ; we moved, didn't accept
      ;; a typed parent path navigates back up
      (set-data (tvision::fd-input d) (namestring (tvision::%parent-dir (tvision::fd-dir d))))
      (ok-cmd)
      (ok "navigates back up to a dir containing src/"
          (member "src/" (items) :test #'string=)))))

(deftest file-dialog-list-enter
  ;; Enter / OK while the browser is focused acts on the highlighted entry,
  ;; even though the default OK button consumes the keystroke first.
  (let ((d (make-file-dialog "Open" :directory (truename "."))))
    (flet ((items () (loop for i below (list-count (tvision::fd-list d))
                           collect (list-item (tvision::fd-list d) i)))
           (ok-cmd () (handle-event d (make-event :type +ev-command+ :command +cm-ok+))))
      (setf (tvision::group-current d) (tvision::fd-list d))   ; browser focused
      ;; highlight the "src/" entry and activate it
      (let ((idx (position "src/" (items) :test #'string=)))
        (ok "src/ present in listing" idx)
        (list-focus-item (tvision::fd-list d) idx)
        (ok-cmd)
        (ok "Enter on a directory entry navigates into it"
            (search "/src/" (namestring (tvision::fd-dir d))))
        (ok "listing now shows that directory"
            (member "package.lisp" (items) :test #'string=))
        (ok "highlight reset to the top (..) after navigating"
            (= (list-focused (tvision::fd-list d)) 0)))
      ;; Enter on ".." (row 0) goes back up
      (ok-cmd)
      (ok "Enter on .. navigates to parent"
          (member "src/" (items) :test #'string=)))))

;;; ===========================================================================
;;; Change-directory dialog helpers
;;; ===========================================================================

(deftest chdir-dialog
  (let ((d (tvision::make-chdir-dialog "CD" :directory (truename "."))))
    (ok "dir list box" (typep (tvision::cd-list d) 'tdir-list-box))
    (ok "lists parent" (string= (list-item (tvision::cd-list d) 0) ".."))
    (ok "directories only"
        (every (lambda (x) (or (string= x "..") (find #\/ x)))
               (loop for i below (list-count (tvision::cd-list d))
                     collect (list-item (tvision::cd-list d) i))))))

;;; ===========================================================================
;;; Concurrency: mailbox FIFO
;;; ===========================================================================

(deftest mailbox
  (let ((mb (make-mailbox)))
    (multiple-value-bind (v p) (mailbox-try-receive mb)
      (ok "empty try-receive" (and (null v) (null p))))
    (mailbox-send mb 1) (mailbox-send mb 2) (mailbox-send mb 3)
    (is= "fifo 1" (mailbox-receive mb) 1)
    (multiple-value-bind (v p) (mailbox-try-receive mb)
      (ok "try-receive present" p)
      (is= "fifo 2" v 2))
    (is= "fifo 3" (mailbox-receive mb) 3)))

;;; ===========================================================================
;;; Thread monitor
;;; ===========================================================================

(deftest thread-monitor
  (let* ((w (make-thread-window (make-trect 0 0 40 14)))
         (tl (tw-list w)))
    (ok "list populated" (>= (list-count tl) 1))
    (ok "snapshot matches list"
        (= (list-count tl) (length (thread-list-threads tl))))
    (ok "main thread present"
        (member (sb-thread:main-thread) (thread-list-threads tl)))
    (ok "current thread marked *"
        (let ((i (position sb-thread:*current-thread* (thread-list-threads tl))))
          (and i (char= (char (list-item tl i) 0) #\*))))
    ;; backtrace capture of the current thread (the fast self path)
    (let ((bt (tvision::%thread-backtrace sb-thread:*current-thread* 10)))
      (ok "captures a backtrace string for the current thread"
          (and (stringp bt) (plusp (length bt)))))))

;;; ===========================================================================
;;; REPL backend (inline path)
;;; ===========================================================================

(deftest repl-meta-command
  (ok ":help is a meta-command" (tvision::repl-meta-command-p ":help"))
  (ok ":help SYM is a meta-command" (tvision::repl-meta-command-p ":help car"))
  (ok ":h SYM is a meta-command" (tvision::repl-meta-command-p ":h car"))
  (ok "other keywords are not meta-commands" (not (tvision::repl-meta-command-p ":foo")))
  (ok "ordinary forms are not meta-commands" (not (tvision::repl-meta-command-p "(+ 1 2)"))))

(deftest backtrace-export
  (let* ((frames (list (list :label "0  FOO" :locals '(("x" "10" 10) ("y" "20" 20)))
                       (list :label "1  BAR" :locals nil)))
         (txt (tvision::%backtrace-text frames)))
    (ok "includes both frame labels" (and (search "0  FOO" txt) (search "1  BAR" txt)))
    (ok "includes a local and its value" (search "x = 10" txt))))

(deftest repl-backend
  (let ((cands (repl-backend-completions "list-len" (find-package :cl))))
    (ok "completion finds list-length" (member "list-length" cands :test #'string=)))
  (let ((r (make-instance 'trepl-view :bounds (make-trect 0 0 40 10))))
    (multiple-value-bind (out results errored) (repl-eval r "(+ 2 3)")
      (declare (ignore out))
      (ok "eval ok" (not errored))
      (is= "eval result" (caar results) 5))
    (repl-eval r "(* 6 7)")
    (is= "per-listener * history" (repl-hvar r '*) 42)))
