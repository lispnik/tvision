;;;; menu.lisp --- Pull-down menus: TMenuBar and dropdown menu boxes.
;;;;
;;;; The data model is plain structs (menu / menu-item); the only *view* is the
;;;; horizontal TMenuBar that lives on row 0.  Dropdown boxes are painted as
;;;; transient overlays directly into the screen buffer while a local tracking
;;;; loop runs, so they never disturb the desktop's focus or Z-order.

(in-package #:tvision)

;;; ---------------------------------------------------------------------------
;;; Data model
;;; ---------------------------------------------------------------------------

(defstruct (menu-item (:constructor %make-menu-item))
  (name "" )            ; display string, may contain ~hotkey~ markers
  (command 0 :type fixnum)
  (key-code 0 :type fixnum)
  (key-text "")         ; right-aligned shortcut label, e.g. "F3"
  (help-ctx 0)
  (disabled nil)
  (submenu nil))        ; a MENU struct, or NIL

(defstruct (menu (:constructor %make-menu))
  (items '()))

(defun new-menu (&rest items) (%make-menu :items items))

(defun menu-item (name command &key (key-code 0) (key-text "") (help 0) disabled)
  "A leaf menu entry that issues COMMAND when chosen."
  (%make-menu-item :name name :command command :key-code key-code
                   :key-text key-text :help-ctx help :disabled disabled))

(defun sub-menu (name menu)
  "A menu entry that opens a nested MENU."
  (%make-menu-item :name name :submenu menu))

(defun menu-separator ()
  (%make-menu-item :name :line))

(defun separator-p (it) (eq (menu-item-name it) :line))

(defun item-disabled-p (it)
  "True when a leaf item's command is currently disabled in the command set."
  (and (not (menu-item-submenu it))
       (plusp (menu-item-command it))
       (not (command-enabled-p (menu-item-command it)))))

(defun item-selectable (it)
  (and (not (separator-p it))
       (not (menu-item-disabled it))
       (not (item-disabled-p it))
       (or (menu-item-submenu it) (plusp (menu-item-command it)))))

(defun strip~ (s) (remove #\~ (if (stringp s) s "")))

(defun menu-item-hotkey (it)
  "The character marked with ~ in the item's name, downcased, or NIL."
  (let* ((name (if (stringp (menu-item-name it)) (menu-item-name it) ""))
         (p (position #\~ name)))
    (when (and p (< (1+ p) (length name))) (char-downcase (char name (1+ p))))))

(defun find-shortcut (menu key)
  "Search MENU (recursively) for an item whose shortcut KEY-CODE is KEY,
returning its command or NIL."
  (when (and menu (plusp key))
    (dolist (it (menu-items menu))
      (cond
        ((menu-item-submenu it)
         (let ((c (find-shortcut (menu-item-submenu it) key)))
           (when c (return-from find-shortcut c))))
        ((and (plusp (menu-item-key-code it))
              (= (menu-item-key-code it) key)
              (plusp (menu-item-command it))
              (not (menu-item-disabled it))
              (command-enabled-p (menu-item-command it)))
         (return-from find-shortcut (menu-item-command it)))))
    nil))

;;; ---------------------------------------------------------------------------
;;; Absolute-coordinate drawing helpers (used for transient overlays)
;;; ---------------------------------------------------------------------------

(defun abs-cell (x y code attr)
  (screen-cell-set *screen* x y (cell-make-code code attr)))

(defun abs-hfill (x y w code attr)
  (dotimes (i w) (abs-cell (+ x i) y code attr)))

(defun abs-str (x y string attr &optional (hot-attr attr))
  "Draw STRING at absolute (X,Y); ~ toggles to HOT-ATTR for the next run."
  (let ((cur attr) (i 0))
    (loop for ch across string do
      (if (char= ch #\~)
          (setf cur (if (eql cur attr) hot-attr attr))
          (progn (abs-cell (+ x i) y (char-code ch) cur) (incf i))))))

(defun abs-shadow (x y)
  "Darken the cell at absolute (X,Y), keeping its glyph, for a drop shadow."
  (let ((s *screen*))
    (when (and (>= x 0) (< x (screen-width s)) (>= y 0) (< y (screen-height s)))
      (let* ((idx (+ x (* y (screen-width s))))
             (c (aref (screen-back s) idx)))
        (setf (aref (screen-back s) idx)
              (cell-make-code (cell-char-code c) (make-attr 8 0)))))))

;;; ---------------------------------------------------------------------------
;;; TMenuBar
;;; ---------------------------------------------------------------------------

;;; Menu colours (palette-driven via the bar; bound during dropdown tracking).
(defvar *menu-normal* (make-attr 0 7))
(defvar *menu-selected* (make-attr 15 2))
(defvar *menu-disabled* (make-attr 8 7))
(defvar *menu-hot* (make-attr 4 7))

(defclass tmenu-bar (tview)
  ((menu    :initarg :menu :initform (new-menu) :accessor menu-bar-menu)
   (current :initform 0    :accessor menu-bar-current)
   (active  :initform nil  :accessor menu-bar-active)))

(defmethod initialize-instance :after ((mb tmenu-bar) &key)
  (setf (view-grow-mode mb) +gf-grow-hix+
        (view-options mb) (logior (view-options mb) +of-pre-process+)))

(defmethod get-palette ((mb tmenu-bar)) (make-palette 31 32 33 34))

(defun bar-layout (mb)
  "Return a list of (item start-x width) for the top-level items."
  (let ((x 1) (out '()))
    (dolist (it (menu-items (menu-bar-menu mb)))
      (let* ((label (format nil " ~a " (strip~ (menu-item-name it))))
             (w (length label)))
        (push (list it x w) out)
        (incf x w)))
    (nreverse out)))

(defun bar-index-at (mb lx)
  "Return the index of the top-level item under local column LX, or 0."
  (loop for (it x w) in (bar-layout mb) for i from 0
        when (and (>= lx x) (< lx (+ x w))) do (return i)
        finally (return 0)))

(defmethod draw ((mb tmenu-bar))
  (let* ((w (point-x (view-size mb)))
         (normal (get-color mb 1)) (hot (get-color mb 4))
         (sela (get-color mb 2)) (shot (get-color mb 2))
         (db (make-draw-buffer w)))
    (db-fill db #\Space normal)
    (loop for (it x iw) in (bar-layout mb) for idx from 0
          for raw = (format nil " ~a " (menu-item-name it))
          do (if (and (menu-bar-active mb) (= idx (menu-bar-current mb)))
                 (progn (db-fill db #\Space sela x iw)
                        (db-move-cstr db x raw (make-palette sela shot) 1))
                 (db-move-cstr db x raw (make-palette normal hot) 1)))
    (write-line* mb 0 0 w 1 db)))

(defmethod handle-event ((mb tmenu-bar) event)
  (cond
    ((= (event-type event) +ev-key-down+)
     (let ((k (event-key-code event)))
       (cond
         ((= k +kb-f10+) (track-menu mb 0) (clear-event event))
         ;; Alt+letter opens the matching top-level menu
         ((and (logtest (event-modifiers event) +md-alt+) (plusp (event-char-code event))
               (let ((hk (char-downcase (code-char (event-char-code event)))))
                 (loop for it in (menu-items (menu-bar-menu mb)) for idx from 0
                       when (eql hk (menu-item-hotkey it))
                       do (track-menu mb idx) (clear-event event) (return t)))))
         (t (let ((cmd (find-shortcut (menu-bar-menu mb) k)))
              (when cmd
                (put-event mb (make-event :type +ev-command+ :command cmd))
                (clear-event event)))))))
    ((and (= (event-type event) +ev-command+) (= (event-command event) +cm-menu+))
     (track-menu mb 0) (clear-event event))
    ((and (= (event-type event) +ev-mouse-down+) (mouse-in-view-p mb event))
     (track-menu mb (bar-index-at mb (point-x (make-local mb (event-mouse-where event)))))
     (clear-event event))))

;;; ---------------------------------------------------------------------------
;;; Dropdown box rendering + tracking
;;; ---------------------------------------------------------------------------

(defun box-dims (menu)
  "Return (values width height) for a dropdown box rendering MENU."
  (let ((maxw 8) (n 0))
    (dolist (it (menu-items menu))
      (incf n)
      (unless (separator-p it)
        (let ((wd (+ (length (strip~ (menu-item-name it)))
                     (if (plusp (length (menu-item-key-text it)))
                         (+ 3 (length (menu-item-key-text it))) 0)
                     (if (menu-item-submenu it) 2 0))))
          (setf maxw (max maxw wd)))))
    (values (+ maxw 4) (+ n 2))))

(defun next-selectable (items cur dir)
  "Step from index CUR by DIR (+1/-1) to the next selectable item, wrapping."
  (let ((n (length items)) (i cur))
    (dotimes (_ n (max 0 cur))
      (setf i (mod (+ i dir) n))
      (when (item-selectable (nth i items)) (return i)))))

(defun draw-menu-box (menu x y sel w h)
  (let ((normal *menu-normal*) (sela *menu-selected*) (dis *menu-disabled*)
        (hot *menu-hot*) (shot *menu-selected*))
    ;; top border
    (abs-cell x y #x250C normal)
    (abs-hfill (1+ x) y (- w 2) #x2500 normal)
    (abs-cell (+ x w -1) y #x2510 normal)
    ;; items
    (loop for it in (menu-items menu) for i from 0
          for ry = (+ y 1 i)
          do (abs-cell x ry #x2502 normal)
             (abs-cell (+ x w -1) ry #x2502 normal)
             (cond
               ((separator-p it)
                (abs-cell x ry #x251C normal)
                (abs-hfill (1+ x) ry (- w 2) #x2500 normal)
                (abs-cell (+ x w -1) ry #x2524 normal))
               (t
                (let ((a (cond ((= i sel) sela)
                               ((or (menu-item-disabled it) (item-disabled-p it)) dis)
                               (t normal))))
                  (abs-hfill (1+ x) ry (- w 2) 32 a)
                  (abs-str (+ x 2) ry (menu-item-name it) a (if (= i sel) shot hot))
                  (when (plusp (length (menu-item-key-text it)))
                    (abs-str (- (+ x w) 2 (length (menu-item-key-text it))) ry
                             (menu-item-key-text it) a))
                  (when (menu-item-submenu it)
                    (abs-cell (- (+ x w) 2) ry #x25BA a))))))
    ;; bottom border
    (let ((by (+ y h -1)))
      (abs-cell x by #x2514 normal)
      (abs-hfill (1+ x) by (- w 2) #x2500 normal)
      (abs-cell (+ x w -1) by #x2518 normal))
    ;; drop shadow (one column right, one row below)
    (loop for ry from (1+ y) to (+ y h) do (abs-shadow (+ x w) ry) (abs-shadow (+ x w 1) ry))
    (loop for sx from (+ x 2) to (+ x w 1) do (abs-shadow sx (+ y h)))))

(defun open-child (mb it x y w sel under)
  (run-menu-box mb (menu-item-submenu it) (+ x w -1) (+ y 1 sel) under))

(defun run-menu-box (mb menu x y under)
  "Run a dropdown for MENU at (X,Y).  UNDER paints everything beneath this box.
Returns a command integer, or one of :cancel / :left / :right."
  (multiple-value-bind (w h) (box-dims menu)
    (let* ((items (menu-items menu))
           (sel (next-selectable items -1 +1)))
      (flet ((paint ()
               (funcall under)
               (draw-menu-box menu x y sel w h)
               (flush-screen *screen*))
             (activate (i)
               (let ((it (nth i items)))
                 (if (menu-item-submenu it)
                     (let ((r (open-child mb it x y w i
                                          (lambda () (funcall under)
                                            (draw-menu-box menu x y i w h)))))
                       (if (eq r :left) :continue r))
                     (when (plusp (menu-item-command it)) (menu-item-command it))))))
        (loop
          (paint)
          (let ((e (get-event)))
            (when (/= (event-type e) +ev-nothing+)
              (cond
                ((= (event-type e) +ev-key-down+)
                 (let ((k (event-key-code e)))
                   (cond
                     ((= k +kb-up+)    (setf sel (next-selectable items sel -1)))
                     ((= k +kb-down+)  (setf sel (next-selectable items sel +1)))
                     ((= k +kb-left+)  (return-from run-menu-box :left))
                     ((= k +kb-right+)
                      (let ((it (nth sel items)))
                        (if (menu-item-submenu it)
                            (let ((r (activate sel)))
                              (unless (eq r :continue) (return-from run-menu-box r)))
                            (return-from run-menu-box :right))))
                     ((= k +kb-esc+)   (return-from run-menu-box :cancel))
                     ((= k +kb-enter+)
                      (let ((r (activate sel)))
                        (when (and r (not (eq r :continue)))
                          (return-from run-menu-box r))))
                     ;; a plain letter selects + activates the matching item
                     ((and (plusp (event-char-code e)) (zerop (event-modifiers e)))
                      (let ((hk (char-downcase (code-char (event-char-code e)))))
                        (loop for it in items for idx from 0
                              when (and (item-selectable it) (eql hk (menu-item-hotkey it)))
                              do (setf sel idx)
                                 (let ((r (activate idx)))
                                   (unless (eq r :continue) (return-from run-menu-box r)))
                                 (return)))))))
                ((= (event-type e) +ev-mouse-down+)
                 (let* ((mp (event-mouse-where e))
                        (mx (point-x mp)) (my (point-y mp)))
                   (if (and (>= mx x) (< mx (+ x w)) (> my y) (<= my (+ y (length items))))
                       (let ((row (- my y 1)))
                         (when (item-selectable (nth row items))
                           (setf sel row)
                           (let ((r (activate row)))
                             (when (and r (not (eq r :continue)))
                               (return-from run-menu-box r)))))
                       (return-from run-menu-box :cancel))))))))))))

(defun track-menu (mb start)
  "Activate the menu bar starting at top-level item START and run the dropdown
tracking loop until a command is chosen or the menu is cancelled."
  (setf (menu-bar-active mb) t (menu-bar-current mb) start)
  (let ((*menu-normal* (get-color mb 1))
        (*menu-selected* (get-color mb 2))
        (*menu-disabled* (get-color mb 3))
        (*menu-hot* (get-color mb 4)))
   (unwind-protect
       (let ((items (menu-items (menu-bar-menu mb))))
         (loop
           (let* ((idx (menu-bar-current mb))
                  (it (nth idx items))
                  (entry (nth idx (bar-layout mb)))
                  (bx (second entry))
                  (result
                    (if (menu-item-submenu it)
                        (run-menu-box mb (menu-item-submenu it) bx 1
                                      (lambda () (draw-view *application*)))
                        (wait-bar-command mb it))))
             (cond
               ((eq result :cancel) (return))
               ((eq result :switch))   ; current already updated; just re-loop
               ((eq result :left)
                (setf (menu-bar-current mb) (mod (1- idx) (length items))))
               ((eq result :right)
                (setf (menu-bar-current mb) (mod (1+ idx) (length items))))
               ((integerp result)
                (put-event mb (make-event :type +ev-command+ :command result))
                (return))
               (t (return))))))
    (setf (menu-bar-active mb) nil)
    (draw-view *application*)
    (flush-screen *screen*))))

(defun wait-bar-command (mb it)
  "Handle a top-level item that has a direct command (no submenu)."
  (loop
    (draw-view *application*)
    (flush-screen *screen*)
    (let ((e (get-event)))
      (when (/= (event-type e) +ev-nothing+)
        (cond
          ((= (event-type e) +ev-key-down+)
           (let ((k (event-key-code e)))
             (cond ((= k +kb-left+)  (return :left))
                   ((= k +kb-right+) (return :right))
                   ((= k +kb-esc+)   (return :cancel))
                   ((= k +kb-enter+) (return (menu-item-command it))))))
          ((= (event-type e) +ev-mouse-down+)
           (if (mouse-in-view-p mb e)
               (return (let ((i (bar-index-at mb (point-x (make-local mb (event-mouse-where e))))))
                         (setf (menu-bar-current mb) i) :switch))
               (return :cancel))))))))

;;; ---------------------------------------------------------------------------
;;; TMenuPopup --- context (pop-up) menus
;;; ---------------------------------------------------------------------------
;;;
;;; The Turbo Vision class for a menu rooted at a box rather than the bar (used
;;; for right-click context menus).  It is a TMenuView-style view carrying a MENU
;;; and an origin; MENU-POPUP-EXEC runs it modally -- reusing the same dropdown
;;; tracker (submenus, hotkeys, shadow, mouse) as the menu bar -- and returns the
;;; chosen command.

(defclass tmenu-popup (tview)
  ((menu :initarg :menu :initform (new-menu) :accessor menu-popup-menu))
  (:documentation "A pop-up / context menu.  EXEC it with MENU-POPUP-EXEC (or the
POPUP-MENU shorthand); it returns the chosen command integer, or NIL."))

(defmethod get-palette ((mp tmenu-popup)) (make-palette 31 32 33 34))

(defun menu-popup-size (mp)
  "Return (values width height) of the box for pop-up menu MP."
  (box-dims (menu-popup-menu mp)))

(defun make-menu-popup (menu &optional (x 0) (y 0))
  "Build a TMenuPopup for MENU positioned at (X,Y)."
  (multiple-value-bind (w h) (box-dims menu)
    (make-instance 'tmenu-popup :menu menu :bounds (make-trect x y (+ x w) (+ y h)))))

(defun menu-popup-exec (mp &optional x y)
  "Display pop-up menu view MP at absolute (X,Y) -- defaulting to its own origin,
clamped to the screen -- and track it modally.  Deliver the chosen command as an
ev-command to the application and also return it (NIL if cancelled)."
  (when (and *application* *screen*)
    (let* ((menu (menu-popup-menu mp))
           (ox (or x (point-x (view-origin mp))))
           (oy (or y (point-y (view-origin mp))))
           (bar (program-menu-bar *application*))
           (*menu-normal*   (if bar (get-color bar 1) *menu-normal*))
           (*menu-selected* (if bar (get-color bar 2) *menu-selected*))
           (*menu-disabled* (if bar (get-color bar 3) *menu-disabled*))
           (*menu-hot*      (if bar (get-color bar 4) *menu-hot*)))
      (multiple-value-bind (w h) (box-dims menu)
        (let* ((px (max 0 (min ox (- (screen-width *screen*) w))))
               (py (max 0 (min oy (- (screen-height *screen*) h))))
               (result (run-menu-box bar menu px py
                                     (lambda () (draw-view *application*)))))
          (when (integerp result)
            (put-event *application* (make-event :type +ev-command+ :command result))
            result))))))

(defun popup-menu (menu x y)
  "Convenience: build a TMenuPopup for MENU and run it at absolute (X,Y).
Returns the chosen command integer, or NIL if cancelled."
  (menu-popup-exec (make-menu-popup menu x y)))
