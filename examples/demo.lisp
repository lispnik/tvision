;;;; demo.lisp --- A small Turbo Vision application showing the port in action.

(defpackage #:tvision-demo
  (:use #:common-lisp #:tvision)
  (:export #:main #:toplevel))

(in-package #:tvision-demo)

(defparameter +cm-new-window+ 100)
(defparameter +cm-about+      101)
(defparameter +cm-greet+      102)
(defparameter +cm-cascade+    103)
(defparameter +cm-tile+       104)
(defparameter +cm-editor+     105)
(defparameter +cm-form+       106)
(defparameter +cm-scroller+   107)
(defparameter +cm-listbox+    108)
(defparameter +cm-save+       109)
(defparameter +cm-load+       110)
(defparameter +cm-pal-color+  111)
(defparameter +cm-pal-bw+     112)
(defparameter +cm-pal-mono+   113)
(defparameter +cm-repl+       114)
(defparameter +cm-tree+       115)
(defparameter +cm-coldef+     116)

;; help contexts (see SETUP for the registered topics)
(defparameter +hc-desktop+ 1000)
(defparameter +hc-editor+  1001)
(defparameter +hc-form+    1002)
(defparameter +hc-repl+    1003)

(defparameter +desktop-file+ "/tmp/tvision-desktop.lisp")

(defclass demo-app (tapplication)
  ((win-count :initform 0 :accessor win-count)))

;;; --- menu bar --------------------------------------------------------------

(defmethod tvision::application-menu ((app demo-app))
  (new-menu
   (sub-menu "~F~ile"
     (new-menu
      (menu-item "~R~EPL"        +cm-repl+       :key-code +kb-f2+ :key-text "F2")
      (menu-item "~N~ew window"  +cm-new-window+)
      (menu-item "~E~ditor"      +cm-editor+     :key-code +kb-f7+ :key-text "F7")
      (menu-item "~S~croller"    +cm-scroller+   :key-code +kb-f9+ :key-text "F9")
      (menu-item "~T~ree view"   +cm-tree+)
      (menu-item "~L~ist box..." +cm-listbox+)
      (menu-separator)
      (menu-item "Sa~v~e desktop" +cm-save+)
      (menu-item "L~o~ad desktop" +cm-load+)
      (menu-separator)
      (menu-item "E~x~it"        +cm-quit+       :key-code +kb-alt-x+ :key-text "Alt-X")))
   (sub-menu "~W~indows"
     (new-menu
      (menu-item "~N~ext"        +cm-next+       :key-code +kb-f6+ :key-text "F6")
      (menu-item "~T~ile"        +cm-tile+       :key-code +kb-f5+ :key-text "F5")
      (menu-item "C~a~scade"     +cm-cascade+)
      (menu-item "~S~ize/Move"   +cm-resize+)
      (menu-separator)
      ;; greys out when there is no window to close (see UPDATE-COMMANDS)
      (menu-item "~C~lose"       +cm-close+      :key-code +kb-ctrl-w+ :key-text "Ctrl-W")))
   (sub-menu "~A~ctions"
     (new-menu
      (menu-item "~F~orm..."     +cm-form+       :key-code +kb-f8+ :key-text "F8")
      (menu-item "~G~reeting..." +cm-greet+      :key-code +kb-f4+ :key-text "F4")
      (menu-item "~A~bout..."    +cm-about+      :key-code +kb-f3+ :key-text "F3")))
   (sub-menu "~P~alette"
     (new-menu
      (menu-item "~C~olor"       +cm-pal-color+)
      (menu-item "~B~lack && white" +cm-pal-bw+)
      (menu-item "~M~onochrome"  +cm-pal-mono+)
      (menu-separator)
      (menu-item "~D~esktop color..." +cm-coldef+)))))

;;; --- status line (context-sensitive via help context) ----------------------

(defmethod tvision::init-status-line ((app demo-app))
  (let* ((h (point-y (view-size app))) (w (point-x (view-size app)))
         (sl (make-instance 'tstatus-line
              :items (list (make-status-item "~Alt-X~ Exit" +kb-alt-x+ +cm-quit+)
                           (make-status-item "~F1~ Help" +kb-f1+ 0)
                           (make-status-item "~F10~ Menu" +kb-f10+ 0)
                           (make-status-item "~F2~ REPL" +kb-f2+ +cm-repl+))
              :defs (list
                     (make-status-def +hc-editor+ +hc-editor+
                       (list (make-status-item "~Alt-X~ Exit" +kb-alt-x+ +cm-quit+)
                             (make-status-item "~F1~ Help" +kb-f1+ 0)
                             (make-status-item "~Ctrl-Z~ Undo" 0 0)
                             (make-status-item "Ctrl-C/X/V Clipboard" 0 0)))
                     (make-status-def +hc-form+ +hc-form+
                       (list (make-status-item "~Alt-X~ Exit" +kb-alt-x+ +cm-quit+)
                             (make-status-item "~F1~ Help" +kb-f1+ 0)
                             (make-status-item "~Tab~ Next field" 0 0)))
                     (make-status-def 0 999
                       (list (make-status-item "~Alt-X~ Exit" +kb-alt-x+ +cm-quit+)
                             (make-status-item "~F1~ Help" +kb-f1+ 0)
                             (make-status-item "~F10~ Menu" +kb-f10+ 0)
                             (make-status-item "~F2~ REPL" +kb-f2+ +cm-repl+)))))))
    (set-bounds sl (make-trect 0 (1- h) w h))
    (setf (view-options sl) (logior (view-options sl) +of-post-process+))
    (setf (program-status-line app) sl)
    (insert app sl)))

;;; --- windows ---------------------------------------------------------------

(defun open-window (app)
  (incf (win-count app))
  (let* ((desk (program-desktop app))
         (n (win-count app))
         (w (make-instance 'twindow :title (format nil "Window ~d" n) :number n
                                    :bounds (make-trect (+ 2 (* n 2)) (+ 1 n)
                                                        (+ 46 (* n 2)) (+ 16 n)))))
    (let ((st (make-instance 'tstatic-text
                             :text (format nil "This is window number ~d.~%~%~
Drag the title bar to move me.~%~
Close me with the [x] icon or Ctrl-W.~%~
Use the buttons below, or the~%status line keys at the bottom." n))))
      (set-bounds st (make-trect 3 2 42 9))
      (insert w st))
    (insert w (make-button (make-trect 4 10 16 12) "~G~reet" +cm-greet+ t))
    (insert w (make-button (make-trect 20 10 32 12) "~A~bout" +cm-about+))
    (insert desk w)))

(defun open-editor (app)
  (let* ((desk (program-desktop app))
         (w (make-instance 'twindow :title "Editor"
                           :bounds (make-trect 6 2 66 21)))
         (vsb (standard-scrollbar w t))     ; vertical scroll bar on the frame
         (text (with-output-to-string (s)
                 (format s "Editable text area with a working scroll bar.~%~%~
Type freely; arrows/Home/End/PgUp/PgDn navigate, Enter splits~%~
the line, Backspace/Del edit.  Drag or click the scroll bar at~%~
the right, or just move the cursor past the bottom.~%~%~
This view is the foundation for the coming Lisp REPL.~%")
                 (dotimes (i 40) (format s "~%Line ~2d of filler text to make scrolling visible." (1+ i)))))
         (tv (make-instance 'ttext-view :text text
                            :bounds (make-trect 1 1
                                                (1- (point-x (view-size w)))
                                                (1- (point-y (view-size w)))))))
    (insert w tv)
    (text-attach-scrollbars tv :vscroll vsb)
    (setf (view-help-ctx w) +hc-editor+)
    (insert desk w)
    (focus tv)))

;;; A scroller showing a large virtual area, driven by both scroll bars.

(defclass demo-scroller (tvision:tscroller) ())

(defun scroller-virtual-line (n)
  "A 100-column ruler line whose pattern shifts with the row, so both axes of
scrolling are visually obvious."
  (let ((s (make-string 100)))
    (dotimes (i 100) (setf (char s i) (code-char (+ 33 (mod (+ i n) 94)))))
    (format nil "~4d: ~a" n s)))

(defmethod draw ((sc demo-scroller))
  (let* ((w (point-x (view-size sc))) (h (point-y (view-size sc)))
         (c (get-color sc 1)) (db (make-draw-buffer w))
         (dx (point-x (scroller-delta sc))) (dy (point-y (scroller-delta sc))))
    (dotimes (row h)
      (db-fill db #\Space c)
      (let ((ly (+ dy row)))
        (when (< ly (point-y (scroller-limit sc)))
          (let* ((full (scroller-virtual-line ly))
                 (start (min dx (length full)))
                 (vis (subseq full start (min (length full) (+ start w)))))
            (db-move-str db 0 vis c))))
      (write-line* sc 0 row w 1 db))))

(defun open-scroller (app)
  (let* ((desk (program-desktop app))
         (w (make-instance 'twindow :title "Scroller (arrows / scroll bars)"
                           :bounds (make-trect 4 2 58 20)))
         (vsb (standard-scrollbar w t))
         (hsb (standard-scrollbar w nil))
         (sc (make-instance 'demo-scroller
                            :bounds (make-trect 1 1
                                                (1- (point-x (view-size w)))
                                                (1- (point-y (view-size w)))))))
    (insert w sc)
    (attach-scrollbars sc :vscroll vsb :hscroll hsb)
    (set-scroller-limit sc 108 300)
    (insert desk w)
    (focus sc)))

;;; --- actions ---------------------------------------------------------------

(defun do-about ()
  (message-box
   (format nil "Turbo Vision for Common Lisp~%~%~
A character-mode UI framework~%running on SBCL.")
   (logior +mf-information+ +mf-ok-button+)))

(defun do-greet ()
  (multiple-value-bind (cmd name) (input-box "Greeting" "Your name:" "")
    (when (= cmd +cm-ok+)
      (message-box (format nil "Hello, ~a!"
                           (if (string= name "") "stranger" name))
                   (logior +mf-information+ +mf-ok-button+)))))

(defun do-form (app)
  "A multi-control dialog: Tab cycling, a history input, a range-validated
field, check boxes, radio buttons, and group-level data exchange."
  (let* ((desk (program-desktop app))
         (w 54) (h 17)
         (d (make-instance 'tdialog :title "Sample Form (Tab moves; Down recalls)"
                           :bounds (make-trect 0 0 w h)))
         (name (make-instance 'thistory-input :history-id "form-name"
                              :bounds (make-trect 16 2 (- w 3) 3) :maxlen 30))
         (age  (make-instance 'tinputline
                              :validator (make-range-validator 0 150)
                              :bounds (make-trect 16 4 28 5) :maxlen 3))
         (opts (make-instance 'tcheck-boxes :labels '("~S~ubscribe" "~N~otify")
                              :bounds (make-trect 16 6 (- w 3) 8)))
         (size (make-instance 'tradio-buttons :labels '("~1~ Small" "~2~ Medium" "~3~ Large")
                              :value 1
                              :bounds (make-trect 16 9 (- w 3) 12))))
    (flet ((label (x y text link)
             (let ((l (make-instance 'tlabel :text text :link link)))
               (set-bounds l (make-trect x y (+ x (length text)) (1+ y)))
               (insert d l))))
      (label 3 2 "Name:"  name) (insert d name)
      (label 3 4 "Age:"   age)  (insert d age)
      (label 3 6 "Mail:"  opts) (insert d opts)
      (label 3 9 "Size:"  size) (insert d size))
    (insert d (make-button (make-trect 14 13 26 15) "~O~K" +cm-ok+ t))
    (insert d (make-button (make-trect 30 13 42 15) "Cancel" +cm-cancel+))
    (setf (view-help-ctx d) +hc-form+)
    (move-to d (floor (- (point-x (view-size desk)) w) 2)
             (max 0 (floor (- (point-y (view-size desk)) h) 2)))
    (focus name)
    (when (= (exec-view desk d) +cm-ok+)
      (history-record name)
      ;; group-level data exchange: collect every control's value at once
      (let ((data (get-data d)))
        (message-box
         (format nil "Form data (layout order):~%~{  ~s~%~}" data)
         (logior +mf-information+ +mf-ok-button+))))))

(defun open-listbox (app)
  "A modal chooser built on TListBox + a scroll bar."
  (let* ((desk (program-desktop app))
         (items (loop for i from 1 to 30 collect (format nil "Item number ~2d" i)))
         (w 34) (h 16)
         (d (make-instance 'tdialog :title "Pick an item" :bounds (make-trect 0 0 w h)))
         (vsb (standard-scrollbar d t))
         (lb (make-instance 'tlist-box :items items :command +cm-ok+
                            :bounds (make-trect 2 1 (1- w) (- h 4)))))
    (insert d lb)
    (attach-scrollbars lb :vscroll vsb)
    (insert d (make-button (make-trect (floor (- w 10) 2) (- h 3)
                                       (+ (floor (- w 10) 2) 10) (- h 1))
                           "O~K~" +cm-ok+ t))
    (move-to d (floor (- (point-x (view-size desk)) w) 2)
             (max 0 (floor (- (point-y (view-size desk)) h) 2)))
    (focus lb)
    (when (= (exec-view desk d) +cm-ok+)
      (message-box (format nil "You chose: ~a" (list-item lb (list-focused lb)))
                   (logior +mf-information+ +mf-ok-button+)))))

(defun open-repl (app)
  (multiple-value-bind (w rv) (make-repl-window (make-trect 3 2 76 23))
    (setf (view-help-ctx w) +hc-repl+)
    (insert (program-desktop app) w)
    (focus rv)))

(defun open-tree (app)
  (let* ((desk (program-desktop app))
         (w (make-instance 'twindow :title "Outline" :bounds (make-trect 6 3 46 19)))
         (vsb (standard-scrollbar w t))
         (ol (make-instance 'toutline
                            :bounds (make-trect 1 1 (1- (point-x (view-size w)))
                                                (1- (point-y (view-size w))))
                            :roots (list (outline-node "Fruits"
                                                       (make-outline-node "Apple")
                                                       (make-outline-node "Banana")
                                                       (make-outline-node "Cherry"))
                                         (outline-node "Vegetables"
                                                       (make-outline-node "Carrot")
                                                       (make-outline-node "Potato"))
                                         (outline-node "Grains"
                                                       (make-outline-node "Rice")
                                                       (make-outline-node "Wheat"))))))
    (insert w ol)
    (attach-scrollbars ol :vscroll vsb)
    (insert desk w)
    (focus ol)))

(defun do-coldef (app)
  "Pick a colour for the desktop background with the colour dialog."
  (multiple-value-bind (ok fg bg) (color-dialog :title "Desktop color" :fg 7 :bg 1)
    (when ok
      (setf (aref tvision::+app-palette-color+ 38) (make-attr fg bg))
      (draw-view app))))

(defun do-save (app)
  (save-desktop +desktop-file+ app)
  (message-box (format nil "Saved ~d window(s) to~%~a"
                       (length (desktop-windows (program-desktop app))) +desktop-file+)
               (logior +mf-information+ +mf-ok-button+)))

(defun do-load (app)
  (if (load-desktop +desktop-file+ app)
      (message-box (format nil "Loaded desktop from~%~a" +desktop-file+)
                   (logior +mf-information+ +mf-ok-button+))
      (message-box "No saved desktop found." (logior +mf-error+ +mf-ok-button+))))

;;; --- command enable/disable --------------------------------------------------

(defun update-commands (app)
  "Grey out window-related commands when no windows are open."
  (let ((has-window (plusp (length (desktop-windows (program-desktop app))))))
    (set-command-enabled +cm-tile+    has-window)
    (set-command-enabled +cm-cascade+ has-window)
    (set-command-enabled +cm-close+   has-window)))

(defmethod idle ((app demo-app))
  (update-commands app))

;;; --- right-click context menu ----------------------------------------------

(defun desktop-context-menu ()
  (new-menu
   (menu-item "~R~EPL"        +cm-repl+)
   (menu-item "~N~ew window"  +cm-new-window+)
   (menu-item "~E~ditor"      +cm-editor+)
   (menu-separator)
   (menu-item "~T~ile"        +cm-tile+)       ; greys out with no windows
   (menu-item "C~a~scade"     +cm-cascade+)
   (menu-separator)
   (menu-item "~A~bout..."    +cm-about+)))

(defmethod handle-event ((app demo-app) event)
  ;; right-click anywhere pops up a context menu (handled before dispatch)
  (when (and (= (event-type event) +ev-mouse-down+)
             (logtest (event-mouse-buttons event) +mb-right+))
    (let ((p (event-mouse-where event)))
      (popup-menu (desktop-context-menu) (point-x p) (point-y p)))
    (clear-event event))
  (call-next-method)
  (when (= (event-type event) +ev-command+)
    (let ((c (event-command event)))
      (cond
        ((= c +cm-repl+)       (open-repl app) (clear-event event))
        ((= c +cm-new-window+) (open-window app) (clear-event event))
        ((= c +cm-editor+)     (open-editor app) (clear-event event))
        ((= c +cm-scroller+)   (open-scroller app) (clear-event event))
        ((= c +cm-tree+)       (open-tree app) (clear-event event))
        ((= c +cm-coldef+)     (do-coldef app) (clear-event event))
        ((= c +cm-listbox+)    (open-listbox app) (clear-event event))
        ((= c +cm-form+)       (do-form app) (clear-event event))
        ((= c +cm-about+)      (do-about) (clear-event event))
        ((= c +cm-greet+)      (do-greet) (clear-event event))
        ((= c +cm-tile+)       (tile (program-desktop app)) (clear-event event))
        ((= c +cm-cascade+)    (cascade (program-desktop app)) (clear-event event))
        ((= c +cm-save+)       (do-save app) (clear-event event))
        ((= c +cm-load+)       (do-load app) (clear-event event))
        ((= c +cm-pal-color+)  (set-palette-mode :color app) (clear-event event))
        ((= c +cm-pal-bw+)     (set-palette-mode :bw app) (clear-event event))
        ((= c +cm-pal-mono+)   (set-palette-mode :mono app) (clear-event event))))))

;;; --- entry point -----------------------------------------------------------

(defun register-demo-help ()
  (let ((desktop-help
          (format nil "Turbo Vision demo~%~%~
F2 new window, F7 editor, F9 scroller.~%~
F10 opens the menu; Alt+letter opens a menu directly.~%~
Alt+1..9 selects a window by number.~%~
Resize the terminal and the UI reflows.~%~%~
Press Tab to a link and Enter to follow: {Keyboard Shortcuts|Keys}.")))
    (register-help +hc-desktop+ desktop-help)
    (register-help-topic "Turbo Vision demo" desktop-help))
  (register-help-topic "Keys"
                 (format nil "Keyboard shortcuts~%~%~
F1 help, F10 menu, Alt-X quit.~%~
F2 REPL, F7 editor, F9 scroller, F8 form.~%~
F5 tile, F6 cycle windows, Ctrl-W close.~%~%~
Backspace here returns to the {previous topic|Turbo Vision demo}."))
  (register-help +hc-editor+
                 (format nil "Editor help~%~%~
Arrows/Home/End/PgUp/PgDn move the cursor.~%~
Shift+arrows select; Ctrl-C/X/V copy/cut/paste; Ctrl-Z undo.~%~
Tab inserts spaces.  The scroll bar tracks the viewport."))
  (register-help +hc-form+
                 (format nil "Form help~%~%~
Tab / Shift-Tab move between fields.~%~
The Name field remembers values (press Down to recall).~%~
The Age field accepts only 0..150.~%~
Pressing OK collects every control's value at once."))
  (register-help +hc-repl+
                 (format nil "Lisp REPL help~%~%~
Type a Lisp form and press Enter to evaluate it.~%~
An incomplete form (open parens) continues on the next line.~%~
Up/Down recall previous input; *, **, *** hold recent values.~%~
Output is read-only; only the text after the prompt is editable.")))

(defmethod tvision::setup ((app demo-app))
  (register-demo-help)
  (setf (view-help-ctx (program-desktop app)) +hc-desktop+)
  (open-repl app)
  (update-commands app))

(defun main ()
  "Launch the demo application."
  (run 'demo-app))

(defun toplevel ()
  "Entry point for the dumped executable (see `asdf:make`)."
  (handler-case (main)
    (error (e)
      ;; the terminal is already restored by WITH-SCREEN's unwind-protect
      (format *error-output* "~&Error: ~a~%" e)
      (sb-ext:exit :code 1)))
  (sb-ext:exit :code 0))
