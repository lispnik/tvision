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

;;; ===========================================================================
;;; HTML view (hypertext browser)
;;; ===========================================================================

(defun %html-text (v)
  "Flatten the rendered lines of an HTML view into one newline-joined string."
  (with-output-to-string (s)
    (loop for ln across (tvision::html-lines v) do
      (loop for r in ln do (write-string (tvision::html-run-text r) s))
      (terpri s))))

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
          (and i (char= (char (list-item tl i) 0) #\*))))))

;;; ===========================================================================
;;; REPL backend (inline path)
;;; ===========================================================================

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
