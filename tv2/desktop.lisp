;;;; desktop.lisp --- the IDE shell: a menu bar, a status bar, and a desktop that
;;;; hosts the ported windows.  This is the classic Turbo Vision chrome that the
;;;; standalone windows lacked.
;;;;
;;;; One screen, one event loop.  The desktop draws a patterned background, the
;;;; active window (laid out *between* the bars), the bottom status bar, and the
;;;; top menu bar (its dropdown overlays everything).  The menu is "live" whenever
;;;; no window is open; opening a window from it hands the window focus, and Esc
;;;; closes the window back to the menu — so no F10/Alt key decoding is needed.

(in-package #:tv2)

(defvar *app-done* nil "Set by File→Exit to leave the desktop loop.")
(defvar *desktop* nil "The running desktop instance (for cross-window actions like eval-in-REPL).")
(defvar *sizemove-win* nil
  "When set, the desktop routes arrow keys to move (Shift: resize) this window
until Enter/Esc.  Driven from Window ▸ Size/move.")

;;; --- status bar -------------------------------------------------------------

(defvar *tool-message* ""
  "Last transient note (from %TOOL-NOTE); shown right-aligned on the status bar so
tool feedback is visible without raising or refocusing any window.")
(defvar *tool-message-time* 0 "INTERNAL-REAL-TIME when *TOOL-MESSAGE* was last set.")
(defparameter *tool-message-ttl* 4 "Seconds a status-bar note lingers before auto-clearing.")

(defun %expire-tool-message ()
  "Clear the status-bar note once it has been shown for *TOOL-MESSAGE-TTL* seconds.
Returns T when it cleared (so the loop can mark the screen dirty)."
  (when (and (plusp (length *tool-message*))
             (> (- (get-internal-real-time) *tool-message-time*)
                (* *tool-message-ttl* internal-time-units-per-second)))
    (setf *tool-message* "")
    t))

(defclass status-bar (view)
  ((provider :initarg :provider :initform nil :accessor stb-provider)  ; thunk -> ((LABEL . THUNK) ...)
   (ranges   :initform '() :accessor stb-ranges))                      ; ((X0 X1 . THUNK) ...) for hit-testing
  (:metaclass reactive-class))

(defmethod draw ((b status-bar))
  (let* ((attr (role :status)) (w (r-w (view-bounds b)))
         (items (and (stb-provider b) (funcall (stb-provider b))))
         (msg (and (plusp (length *tool-message*)) (format nil " ~a " *tool-message*)))
         (limit (if msg (max 0 (- w (length msg))) w))       ; reserve the right for the note
         (x 0))
    (fill-row b 0 0 w attr)
    (setf (stb-ranges b) '())
    (dolist (it items)
      (let* ((label (format nil " ~a " (car it))) (n (length label)))
        (when (< (+ x n) limit)
          (draw-text b x 0 label attr)
          (push (list x (+ x n) (cdr it)) (stb-ranges b))
          (incf x n)
          (when (< (1+ x) limit) (draw-text b x 0 "│" attr) (incf x 1)))))
    (when msg                                                ; right-aligned transient note, always visible
      (draw-text b (max 0 (- w (length msg))) 0 msg (role :focused)))))

(defmethod handle-event ((b status-bar) (e mouse-down))
  (let ((col (mouse-col b e)))
    (dolist (r (stb-ranges b))
      (when (and (>= col (first r)) (< col (second r))) (funcall (third r)) (return))))
  (setf (handled-p e) t))

;;; --- menu bar (pull-down menus with hotkeys / accelerators) -----------------
;;; A menu is (LABEL . ITEMS); its hotkey (Alt-X) is the label's first letter.
;;; An item is (LABEL THUNK &optional ACCEL ENABLED): ACCEL is a global-shortcut
;;; keysym (e.g. (ctrl #\o)); ENABLED a thunk -> generalized boolean (or NIL).

(defclass menu-bar (view)
  ((menus  :initarg :menus :initform '() :accessor menu-menus)
   (active :initform 0 :accessor menu-active)                    ; open menu index, or NIL
   (sel    :initform 0 :accessor menu-sel)
   (sub    :initform nil :accessor menu-sub))                    ; open submenu index, or NIL
  (:metaclass reactive-class))

(defun ctrl (ch) (code-char (logand (char-code (char-upcase ch)) #x1f)))   ; (ctrl #\o) -> ^O keysym
(defun item-separator-p (it) (eq it :--))                                   ; :-- is a horizontal rule
(defun item-label    (it) (if (item-separator-p it) "" (first it)))
(defun item-submenu-p (it) (and (consp it) (eq (second it) :submenu)))      ; (LABEL :submenu item...)
(defun item-thunk    (it) (and (consp it) (not (item-submenu-p it)) (second it)))
(defun item-accel    (it) (and (consp it) (not (item-submenu-p it)) (third it)))  ; submenu parents have no accel
(defun item-enabled  (it) (and (not (item-separator-p it))
                               (let ((f (and (consp it) (not (item-submenu-p it)) (fourth it))))
                                 (or (null f) (funcall f)))))
(defun item-submenu   (it) (cddr it))

(defun %menu-step (items sel dir)
  "Next selectable (non-separator) index from SEL in direction DIR, wrapping."
  (let ((n (length items)))
    (if (zerop n) 0
        (loop for i from 1 to n
              for k = (mod (+ sel (* dir i)) n)
              unless (item-separator-p (nth k items)) return k
              finally (return sel)))))

(defparameter *menu-order*
  '("≡" "File" "Edit" "Lisp" "Window" "Options" "Help")
  "Left-to-right order of the menu bar; menus not listed fall to the right.")

(defun %order-menus (menus)
  (stable-sort (copy-list menus) #'<
               :key (lambda (m) (or (position (car m) *menu-order* :test #'string=) most-positive-fixnum))))
(defun menu-items   (mb) (cdr (nth (menu-active mb) (menu-menus mb))))
(defun menu-hotkey  (m)  (and (plusp (length (car m))) (char-downcase (char (car m) 0))))

(defun accel-label (ks)
  (cond ((null ks) "")
        ((and (characterp ks) (< (char-code ks) 32)) (format nil "^~a" (code-char (+ 64 (char-code ks)))))
        ((eql ks :f1) "F1")
        ((characterp ks) (string ks))
        (t (string-downcase (string ks)))))

(defun menu-dropdown-width (items)
  (+ 4 (reduce #'max items :initial-value 8
               :key (lambda (it) (+ (length (item-label it))
                                    (if (item-accel it) (+ 2 (length (accel-label (item-accel it)))) 0))))))

(defmethod draw ((mb menu-bar))
  (let* ((b (view-bounds mb)) (w (r-w b)) (ax (tvision::rect-ax b)) (ay (tvision::rect-ay b))
         (bar (role :menu-bar)) (hot (role :menu-hotkey)) (x 1))
    (fill-row mb 0 0 w bar)
    (loop for menu in (menu-menus mb) for i from 0 do
      (let* ((label (car menu)) (open (eql i (menu-active mb))) (attr (if open (role :menu-selected) bar)))
        (%text-at (+ ax x) ay (format nil " ~a " label) attr)
        (%put-cell (+ ax x 1) ay (char label 0) (if open attr hot))   ; highlight the hotkey letter
        (when open
          (let* ((items (cdr menu)) (x0 (+ ax x)) (box-top (1+ ay)) (mw (menu-dropdown-width items))
                 (nb (role :menu)))                                    ; frame colour (same as the menu body)
            (flet ((draw-items (items bx bt sel)
                     ;; a bordered dropdown box: ┌─┐ top, │ … │ items, ├─┤ separators,
                     ;; └─┘ bottom -- like the original Turbo Vision (items inset 1 cell).
                     (let ((mww (menu-dropdown-width items)) (n (length items)))
                       (%drop-shadow bx bt (+ bx mww -1) (+ bt n 1))
                       (%put-cell bx bt #\┌ nb)                        ; top border
                       (loop for k from 1 below (1- mww) do (%put-cell (+ bx k) bt #\─ nb))
                       (%put-cell (+ bx mww -1) bt #\┐ nb)
                       (loop for it in items for r from 0 for ry = (+ bt 1 r) do
                         (%put-cell bx ry #\│ nb) (%put-cell (+ bx mww -1) ry #\│ nb)   ; side borders
                         (if (item-separator-p it)
                             (progn (%put-cell bx ry #\├ nb)           ; tee-connected divider
                                    (loop for k from 1 below (1- mww) do (%put-cell (+ bx k) ry #\─ nb))
                                    (%put-cell (+ bx mww -1) ry #\┤ nb))
                             (let* ((on (eql r sel)) (en (item-enabled it))
                                    (ia (cond ((and on en) (role :menu-selected)) (en (role :menu)) (t (role :menu-disabled)))))
                               (loop for k from 1 below (1- mww) do (%put-cell (+ bx k) ry #\Space ia))
                               (%text-at (+ bx 2) ry (item-label it) ia)
                               (cond ((item-submenu-p it) (%put-cell (+ bx mww -3) ry #\► ia))
                                     ((item-accel it) (let ((a (accel-label (item-accel it))))
                                                        (%text-at (+ bx mww -2 (- (length a))) ry a ia)))))))
                       (let ((by (+ bt n 1)))                          ; bottom border
                         (%put-cell bx by #\└ nb)
                         (loop for k from 1 below (1- mww) do (%put-cell (+ bx k) by #\─ nb))
                         (%put-cell (+ bx mww -1) by #\┘ nb)))))
              (draw-items items x0 box-top (menu-sel mb))
              (when (menu-sub mb)                                      ; second-level dropdown (overlaps the parent's right border)
                (let ((parent (nth (menu-sel mb) items)))
                  (when (item-submenu-p parent)
                    (draw-items (item-submenu parent) (+ x0 mw -1) (+ box-top 1 (menu-sel mb)) (menu-sub mb))))))))
        (incf x (+ 2 (length label)))))))

(defun %menu-run (mb thunk)
  "Close the menu, then run THUNK -- so the menu doesn't linger over the result."
  (setf (menu-active mb) nil (menu-sub mb) nil)
  (invalidate mb)
  (when thunk (funcall thunk)))

(defun menu-invoke-sel (mb)
  "Open a submenu parent, or invoke (and close on) a normal selected item."
  (let ((it (nth (menu-sel mb) (menu-items mb))))
    (cond ((null it) nil)
          ((item-submenu-p it) (setf (menu-sub mb) 0) (invalidate mb))
          ((and (item-enabled it) (item-thunk it)) (%menu-run mb (item-thunk it))))))

(defmethod handle-event ((mb menu-bar) (e key-event))
  (when (menu-active mb)
    (let ((ks (event-keysym e)) (n (length (menu-menus mb))) (items (menu-items mb)))
      (if (menu-sub mb)                                            ; navigating an open submenu
          (let ((subs (item-submenu (nth (menu-sel mb) items))))
            (cond
              ((eql ks :up)   (setf (menu-sub mb) (%menu-step subs (menu-sub mb) -1)) (invalidate mb) (setf (handled-p e) t))
              ((eql ks :down) (setf (menu-sub mb) (%menu-step subs (menu-sub mb) 1)) (invalidate mb) (setf (handled-p e) t))
              ((member ks '(:left :esc)) (setf (menu-sub mb) nil) (invalidate mb) (setf (handled-p e) t))
              ((eql ks :enter) (let ((it (nth (menu-sub mb) subs)))
                                 (%menu-run mb (and it (item-enabled it) (item-thunk it))))
                               (setf (handled-p e) t))))
          (cond
            ((eql ks :left)  (setf (menu-active mb) (mod (1- (menu-active mb)) n) (menu-sel mb) 0 (menu-sub mb) nil) (invalidate mb) (setf (handled-p e) t))
            ((eql ks :right) (let ((it (nth (menu-sel mb) items)))
                               (if (item-submenu-p it) (setf (menu-sub mb) 0)
                                   (setf (menu-active mb) (mod (1+ (menu-active mb)) n) (menu-sel mb) 0)))
                             (invalidate mb) (setf (handled-p e) t))
            ((eql ks :up)    (setf (menu-sel mb) (%menu-step items (menu-sel mb) -1) (menu-sub mb) nil) (invalidate mb) (setf (handled-p e) t))
            ((eql ks :down)  (setf (menu-sel mb) (%menu-step items (menu-sel mb) 1) (menu-sub mb) nil) (invalidate mb) (setf (handled-p e) t))
            ((eql ks :enter) (menu-invoke-sel mb) (setf (handled-p e) t)))))))

(defun menu-hotkey-index (mb ch)
  (position (char-downcase ch) (menu-menus mb) :key #'menu-hotkey))

(defun menu-accel-thunk (mb ks)
  "Thunk for an enabled item whose accelerator is KS, anywhere in the menus."
  (loop for menu in (menu-menus mb) thereis
        (loop for it in (cdr menu)
              when (and (not (item-submenu-p it)) (item-accel it) (eql (item-accel it) ks) (item-enabled it))
                return (item-thunk it))))

(defun menu-title-x (mb i)
  (let ((x 1)) (dotimes (k i x) (incf x (+ 2 (length (car (nth k (menu-menus mb)))))))))

(defun menu-dropdown-cols (mb)
  (when (menu-active mb)
    (values (menu-title-x mb (menu-active mb)) (menu-dropdown-width (menu-items mb)))))

(defun menu-sub-cols (mb)
  "(values SX SY0 COUNT WIDTH) of the open submenu dropdown, or NIL.  SY0 is the
box's top-border row; its items occupy rows SY0+1 .. SY0+COUNT."
  (when (and (menu-active mb) (menu-sub mb))
    (let ((parent (nth (menu-sel mb) (menu-items mb))))
      (when (item-submenu-p parent)
        (multiple-value-bind (x0 mw) (menu-dropdown-cols mb)
          (values (+ x0 mw -1) (+ 2 (menu-sel mb)) (length (item-submenu parent))
                  (menu-dropdown-width (item-submenu parent))))))))

(defun menu-hit-p (mb x y)
  (or (zerop y)
      (and (menu-active mb) (>= y 1) (<= y (+ 2 (length (menu-items mb))))    ; main box incl. borders
           (multiple-value-bind (x0 mw) (menu-dropdown-cols mb)
             (and x0 (>= x x0) (< x (+ x0 mw)))))
      (multiple-value-bind (sx sy0 cnt smw) (menu-sub-cols mb)   ; open submenu box incl. borders
        (and sx (>= x sx) (< x (+ sx smw)) (>= y sy0) (<= y (+ sy0 cnt 1))))))

(defmethod handle-event ((mb menu-bar) (e mouse-down))
  (let ((col (mouse-col mb e)) (row (mouse-row mb e)))
    (multiple-value-bind (sx sy0 cnt smw) (menu-sub-cols mb)
      (cond
        ((and sx (>= col sx) (< col (+ sx smw)) (> row sy0) (<= row (+ sy0 cnt)))   ; submenu item (row sy0 is the border)
         (let* ((idx (- row sy0 1)) (subs (item-submenu (nth (menu-sel mb) (menu-items mb)))) (it (nth idx subs)))
           (setf (menu-sub mb) idx) (invalidate mb)
           (%menu-run mb (and it (item-enabled it) (item-thunk it)))))
        ((zerop row)                                  ; clicked a title -> open that menu
         (let ((x 1))
           (loop for menu in (menu-menus mb) for i from 0 do
             (let ((tw (+ 2 (length (car menu)))))
               (when (and (>= col x) (< col (+ x tw)))
                 (setf (menu-active mb) i (menu-sel mb) 0 (menu-sub mb) nil) (invalidate mb) (return))
               (incf x tw)))))
        ((menu-active mb)                             ; clicked a dropdown item -> invoke / open submenu
         (let ((idx (- row 2)) (items (menu-items mb)))   ; row 1 is the top border; item 0 is at row 2
           (when (and (>= idx 0) (< idx (length items)))
             (setf (menu-sel mb) idx (menu-sub mb) nil) (invalidate mb) (menu-invoke-sel mb))))))
    (setf (handled-p e) t)))

;;; --- desktop ----------------------------------------------------------------

(defclass desktop (view)
  ((menubar   :accessor dt-menubar)
   (statusbar :accessor dt-statusbar)
   (windows   :initform '() :accessor dt-windows)     ; back-to-front Z-order; last = topmost/focused
   (drag      :initform nil :accessor dt-drag))       ; (:move WIN OFFX OFFY) | (:resize WIN) while dragging
  (:metaclass reactive-class))

(defun dt-top (dt) (car (last (dt-windows dt))))       ; the focused window, or NIL
(defun dt-raise (dt w) (setf (dt-windows dt) (append (remove w (dt-windows dt)) (list w))))
(defun dt-content (dt)
  "The rectangle between the menu bar (row 0) and status bar (last row)."
  (let* ((r (view-bounds dt)) (ax (tvision::rect-ax r)) (ay (tvision::rect-ay r)) (w (r-w r)) (h (r-h r)))
    (rect ax (1+ ay) (+ ax w) (+ ay (1- h)))))

(defmethod layout ((dt desktop) r)
  (setf (view-bounds dt) r)
  (let ((ax (tvision::rect-ax r)) (ay (tvision::rect-ay r)) (w (r-w r)) (h (r-h r)))
    (layout (dt-menubar dt)   (rect ax ay (+ ax w) (+ ay 1)))
    (layout (dt-statusbar dt) (rect ax (+ ay (1- h)) (+ ax w) (+ ay h)))))

(defmethod draw ((dt desktop))
  (let* ((b (view-bounds dt)) (w (r-w b)) (h (r-h b))
         (ax (tvision::rect-ax b)) (ay (tvision::rect-ay b)) (bg (role :desktop)) (top (dt-top dt)))
    (loop for y from (1+ ay) below (+ ay (1- h)) do          ; patterned background
      (loop for x from ax below (+ ax w) do (%put-cell x y #\▒ bg)))
    (loop for win in (dt-windows dt) for i from 1 do        ; windows back-to-front, numbered 1..
      (setf (window-active win) (eq win top) (window-number win) i) (draw win))
    (draw (dt-statusbar dt))
    (draw (dt-menubar dt))))                                 ; menu + dropdown overlay everything

(defun dt-refocus (dt)
  "Keep the menu live only when no window is open."
  (setf (menu-active (dt-menubar dt)) (if (dt-windows dt) nil 0)
        (menu-sel (dt-menubar dt)) 0
        (menu-sub (dt-menubar dt)) nil))

(defvar *window-builders* nil "Keyword -> 0-arg make-* builder (populated below); drives layout restore.")

(defun dt-cascade-rect (dt)
  "A cascade-offset window rectangle for the Nth open window."
  (let* ((c (dt-content dt)) (n (length (dt-windows dt)))
         (cw (max 40 (floor (* (r-w c) 4) 5))) (ch (max 8 (floor (* (r-h c) 4) 5)))
         (ox (+ (tvision::rect-ax c) (* (mod n 6) 3))) (oy (+ (tvision::rect-ay c) (* (mod n 6) 2))))
    (rect ox oy (min (+ ox cw) (tvision::rect-bx c)) (min (+ oy ch) (tvision::rect-by c)))))

(defun dt-add (dt win focus open kind bounds)
  "Host WIN at BOUNDS, recording its KIND (a keyword or NIL) for save/restore."
  (layout win bounds)
  (setf (window-managed win) t (window-kind win) kind
        (container-focus win) (or focus (first (all-focusables win)))
        (window-cleanup win) (and open (funcall open tvision:*screen*)))
  (setf (dt-windows dt) (append (dt-windows dt) (list win))))

(defun dt-open (dt kind-or-fn)
  "Open a window: KIND-OR-FN is a builder keyword (looked up in *WINDOW-BUILDERS*
and recorded so the layout can be saved/restored) or a builder function (used
directly, not persisted).  Cascade-positioned, focused on top."
  (let ((bounds (dt-cascade-rect dt)))
    (multiple-value-bind (win focus open)
        (funcall (if (functionp kind-or-fn) kind-or-fn (cdr (assoc kind-or-fn *window-builders*))))
      (when win
        (dt-add dt win focus open (and (keywordp kind-or-fn) kind-or-fn) bounds)
        (dt-refocus dt) (invalidate dt)))))

(defun dt-close-window (dt win)
  (when (window-cleanup win) (ignore-errors (funcall (window-cleanup win))))
  (setf (dt-windows dt) (remove win (dt-windows dt)))
  (dt-refocus dt) (invalidate dt))

(defun dt-next (dt)                                          ; cycle focus: top goes to the bottom
  (when (dt-windows dt)
    (setf (dt-windows dt) (cons (dt-top dt) (butlast (dt-windows dt))))
    (invalidate dt)))

(defun dt-zoom (dt win)
  "Toggle WIN between its size and filling the desktop content area (classic zoom)."
  (when (window-managed win)
    (if (window-zoomed win)
        (progn (when (window-saved-bounds win) (layout win (window-saved-bounds win)))
               (setf (window-zoomed win) nil))
        (progn (setf (window-saved-bounds win) (view-bounds win) (window-zoomed win) t)
               (layout win (dt-content dt))))
    (dt-raise dt win) (dt-refocus dt) (invalidate dt)))

(defun dt-select-number (dt n)
  "Raise + focus the Nth window (1-based z-order), if it exists."
  (let ((win (nth (1- n) (dt-windows dt))))
    (when win (dt-raise dt win) (dt-refocus dt) (invalidate dt))))

(defun dt-cascade (dt)
  (let ((c (dt-content dt)))
    (loop for win in (dt-windows dt) for i from 0
          for ox = (+ (tvision::rect-ax c) (* i 3)) for oy = (+ (tvision::rect-ay c) (* i 2))
          for cw = (max 40 (floor (* (r-w c) 4) 5)) for ch = (max 8 (floor (* (r-h c) 4) 5))
          do (layout win (rect ox oy (min (+ ox cw) (tvision::rect-bx c)) (min (+ oy ch) (tvision::rect-by c)))))
    (invalidate dt)))

(defun dt-tile (dt)
  (let* ((c (dt-content dt)) (n (length (dt-windows dt))))
    (when (plusp n)
      (let* ((cols (ceiling (sqrt n))) (rows (ceiling n cols))
             (cw (floor (r-w c) cols)) (ch (floor (r-h c) rows)))
        (loop for win in (dt-windows dt) for i from 0
              for cx = (mod i cols) for cy = (floor i cols)
              for x0 = (+ (tvision::rect-ax c) (* cx cw)) for y0 = (+ (tvision::rect-ay c) (* cy ch))
              do (layout win (rect x0 y0 (+ x0 cw) (+ y0 ch))))
        (invalidate dt)))))

(defun dt-help (dt)
  "Open help for the focused window's topic (or the contents page)."
  (let ((topic (if (dt-top dt) (window-help (dt-top dt)) :general)))
    (dt-open dt (lambda () (make-help topic)))))

(defun dt-prev (dt)                                         ; cycle focus backwards
  (when (dt-windows dt)
    (setf (dt-windows dt) (append (cdr (dt-windows dt)) (list (car (dt-windows dt)))))
    (invalidate dt)))

(defun %sizemove-step (dt win ks resize)
  "Move (or, when RESIZE, resize) WIN one cell for arrow key KS, clamped to the
desktop content area."
  (let* ((b (view-bounds win)) (c (dt-content dt))
         (ax (tvision::rect-ax b)) (ay (tvision::rect-ay b))
         (bx (tvision::rect-bx b)) (by (tvision::rect-by b))
         (cax (tvision::rect-ax c)) (cay (tvision::rect-ay c))
         (cbx (tvision::rect-bx c)) (cby (tvision::rect-by c)))
    (multiple-value-bind (dx dy)
        (case ks (:left (values -1 0)) (:right (values 1 0)) (:up (values 0 -1)) (:down (values 0 1)) (t (values 0 0)))
      (if resize
          (layout win (rect ax ay (max (+ ax 24) (min (+ bx dx) cbx)) (max (+ ay 5) (min (+ by dy) cby))))
          (let* ((ww (r-w b)) (hh (r-h b))
                 (nax (max cax (min (+ ax dx) (- cbx ww))))
                 (nay (max cay (min (+ ay dy) (- cby hh)))))
            (layout win (rect nax nay (+ nax ww) (+ nay hh)))))
      (invalidate dt))))

(defun %dt-window-list (dt)
  "A modal picker over the open windows; Enter raises + focuses the chosen one."
  (let ((wins (reverse (dt-windows dt))))                   ; top-first
    (if (null wins)
        (%tool-note "no windows open")
        (let* ((titles (loop for w in wins for i from 1
                             collect (format nil "~d. ~a" i (string-trim " " (window-title w)))))
               (d (ui (dialog (:title " Windows " :keymap *dialog-keys*
                               :value-fn (lambda (dd) (list-selected (find-view dd 'lst))))
                        (stack
                          (:fill (list-box :name 'lst :items titles))
                          (1 (static-text :role :status :text " ↑↓ choose · Enter: focus · Esc: cancel ")))))))
          (let ((r (exec-view d :width 54 :height (min 20 (+ 4 (length wins))))))
            (when (and (integerp r) (nth r wins))
              (dt-raise dt (nth r wins)) (dt-refocus dt) (invalidate dt)))))))

;;; --- File-menu actions on editors / the REPL --------------------------------

(defun %focused-te (dt)
  "The text-edit of the focused window, when it is an editor."
  (let ((top (dt-top dt))) (and (typep top 'editor-window) (find-view top 'edit))))

(defun %dt-repl (dt)
  (find-if (lambda (w) (typep w 'repl-window)) (reverse (dt-windows dt))))

(defun %dt-save-as (dt)
  (let ((te (%focused-te dt)))
    (when te
      (let ((p (make-file-dialog :dir *project-dir* :title " Save as ")))
        (when p (setf (te-filename te) p) (te-save te)
              (%tool-note (format nil "saved ~a" (file-namestring p))))))))

(defun %dt-save (dt)
  (let ((te (%focused-te dt)))
    (cond ((null te) (%tool-note "no editor focused"))
          ((te-filename te) (te-save te) (%tool-note (format nil "saved ~a" (file-namestring (te-filename te)))))
          (t (%dt-save-as dt)))))                           ; unnamed buffer -> prompt for a name

(defun %dt-save-all (dt)
  (let ((n 0))
    (dolist (w (dt-windows dt))
      (when (typep w 'editor-window)
        (let ((te (find-view w 'edit)))
          (when (and te (te-filename te) (te-modified te)) (te-save te) (incf n)))))
    (%tool-note (format nil "saved ~d modified buffer~:p" n))))

(defun %dt-reload (dt)
  (let ((te (%focused-te dt)))
    (cond ((or (null te) (null (te-filename te))) (%tool-note "no file to reload"))
          (t (te-load te (te-filename te)) (invalidate te) (%tool-note "reloaded from disk")))))

(defun %dt-save-transcript (dt)
  (let ((r (%dt-repl dt)))
    (if (null r) (%tool-note "no REPL open")
        (let ((p (make-file-dialog :dir *project-dir* :title " Save transcript ")))
          (when p (%repl-save-transcript r p) (%tool-note (format nil "transcript → ~a" (file-namestring p))))))))

(defun %dt-save-script (dt)
  (let ((r (%dt-repl dt)))
    (if (null r) (%tool-note "no REPL open")
        (let ((p (make-file-dialog :dir *project-dir* :title " Save Lisp script ")))
          (when p (%repl-save-script r p) (%tool-note (format nil "script → ~a" (file-namestring p))))))))

(defun %dt-clear-repl (dt)
  (let ((r (%dt-repl dt))) (if r (%repl-clear r) (%tool-note "no REPL open"))))

;;; --- colour themes ----------------------------------------------------------

(defparameter *theme-classic* (copy-list *theme*))
(defparameter *theme-dark*
  (list :normal          (tvision:make-attr 7 0)     :focused         (tvision:make-attr 15 4)
        :frame           (tvision:make-attr 15 0)    :frame-inactive  (tvision:make-attr 8 0)
        :menu-bar        (tvision:make-attr 0 7)     :menu            (tvision:make-attr 0 7)
        :menu-selected   (tvision:make-attr 15 4)    :menu-hotkey     (tvision:make-attr 4 7)
        :menu-disabled   (tvision:make-attr 8 7)     :status          (tvision:make-attr 15 8)
        :button          (tvision:make-attr 0 7)     :button-focused  (tvision:make-attr 15 4)
        :label           (tvision:make-attr 14 0)    :input           (tvision:make-attr 15 8)
        :input-focused   (tvision:make-attr 15 0)    :error           (tvision:make-attr 15 4)
        :desktop         (tvision:make-attr 8 0)     :scrollbar       (tvision:make-attr 7 8)
        :scrollbar-thumb (tvision:make-attr 15 8)))
(defparameter *themes* (list (cons "Classic blue" *theme-classic*) (cons "Dark" *theme-dark*)))
(defvar *theme-index* 0)

(defun cycle-theme (dt)
  (setf *theme-index* (mod (1+ *theme-index*) (length *themes*)))
  (destructuring-bind (name . palette) (nth *theme-index* *themes*)
    (setf *theme* palette)
    (invalidate dt)
    (%tool-note (format nil "colour theme: ~a" name))))

(defmethod handle-event ((dt desktop) (e key-event))
  (let* ((mb (dt-menubar dt)) (top (dt-top dt)) (ks (event-keysym e))
         (alt (logtest (event-modifiers e) tvision::+md-alt+)))
    (cond
      (*sizemove-win*                                        ; interactive keyboard size/move mode
       (cond ((member ks '(:enter :esc)) (setf *sizemove-win* nil) (%tool-note "size/move done"))
             ((member ks '(:up :down :left :right))
              (%sizemove-step dt *sizemove-win* ks (logtest (event-modifiers e) tvision::+md-shift+)))))
      ((and alt (characterp ks) (digit-char-p ks) (char/= ks #\0))   ; Alt-1..9 selects that window
       (dt-select-number dt (digit-char-p ks)))
      ((and alt (characterp ks) (menu-hotkey-index mb ks))   ; Alt-<hotkey> opens that menu
       (setf (menu-active mb) (menu-hotkey-index mb ks) (menu-sel mb) 0) (invalidate mb))
      ((and (eql ks :f5) top) (dt-zoom dt top))             ; F5: zoom/unzoom the top window
      ((eql ks :f1) (dt-help dt))                            ; F1: contextual help
      (top
       (cond
         ((and (eql ks :esc) (menu-active mb)) (setf (menu-active mb) nil) (invalidate mb))  ; close an open menu
         ((eql ks :esc) (dt-close-window dt top))            ; else Esc closes the top window
         ((menu-active mb) (handle-event mb e))              ; a menu is open over the window -> it drives
         (t (setf *running* t) (handle-event top e)          ; otherwise the focused widget gets the key
            (cond ((not *running*) (dt-close-window dt top))
                  ((not (handled-p e))                       ; ignored -> try a global accelerator
                   (let ((th (menu-accel-thunk mb ks))) (when th (funcall th))))))))
      (t (let ((th (menu-accel-thunk mb ks)))                ; no window: accelerators first, then the menu
           (if th (funcall th) (handle-event mb e)))))))

(defun dt-window-at (dt x y)
  (loop for w in (reverse (dt-windows dt)) when (point-in-rect-p x y (view-bounds w)) return w))

(defun dt-drag-update (dt e)
  (let* ((d (dt-drag dt)) (win (second d)) (w (event-where e)) (mx (car w)) (my (cdr w)) (c (dt-content dt)))
    (cond
      ((typep e 'mouse-up) (setf (dt-drag dt) nil))
      ((typep e 'mouse-move)
       (let* ((b (view-bounds win)) (ax (tvision::rect-ax b)) (ay (tvision::rect-ay b)))
         (ecase (first d)
           (:move
            (let* ((ww (r-w b)) (hh (r-h b))
                   (nx (max (tvision::rect-ax c) (min (- mx (third d)) (- (tvision::rect-bx c) ww))))
                   (ny (max (tvision::rect-ay c) (min (- my (fourth d)) (- (tvision::rect-by c) hh)))))
              (layout win (rect nx ny (+ nx ww) (+ ny hh)))))
           (:resize
            (let ((nx2 (max (+ ax 24) (min (1+ mx) (tvision::rect-bx c))))
                  (ny2 (max (+ ay 5)  (min (1+ my) (tvision::rect-by c)))))
              (layout win (rect ax ay nx2 ny2))))
           (:scroll
            (multiple-value-bind (sx sy0 sy1) (window-vscroll-bounds win)
              (declare (ignore sx))
              (when sy0 (%scroll-from-click (window-scroll-target win) my sy0 sy1))))
           (:hscroll
            (multiple-value-bind (sy hx0 hx1) (window-hscroll-bounds win)
              (declare (ignore sy))
              (when hx0 (%hscroll-from-click (window-scroll-target win) mx hx0 hx1)))))
         (invalidate dt))))))

(defun dt-window-click (dt win e)
  (dt-raise dt win)
  (let* ((b (view-bounds win)) (lx (mouse-col win e)) (ly (mouse-row win e)) (w (r-w b)) (h (r-h b)))
    (cond
      ((not (typep e 'mouse-down)) (handle-event win e))            ; wheel etc. -> widgets
      ((and (zerop ly) (<= 1 lx 3)) (dt-close-window dt win))       ; [✕] close box
      ((and (zerop ly) (> w 7) (<= (- w 5) lx (- w 3))) (dt-zoom dt win))  ; [↑] zoom box
      ((and (zerop ly) (event-double e)) (dt-zoom dt win))          ; double-click title -> zoom
      ((and (= lx (1- w)) (= ly (1- h))) (setf (dt-drag dt) (list :resize win)))  ; resize grip
      ((and (= lx (1- w)) (window-scroll-target win) (>= ly 1) (<= ly (- h 2)))   ; right (vertical) scrollbar
       (multiple-value-bind (sx sy0 sy1) (window-vscroll-bounds win)
         (declare (ignore sx))
         (%scroll-from-click (window-scroll-target win) (cdr (event-where e)) sy0 sy1))
       (setf (dt-drag dt) (list :scroll win)))
      ((and (= ly (1- h)) (window-hscroll-bounds win) (<= 1 lx (- w 2)))          ; bottom (horizontal) scrollbar
       (multiple-value-bind (sy hx0 hx1) (window-hscroll-bounds win)
         (declare (ignore sy))
         (%hscroll-from-click (window-scroll-target win) (car (event-where e)) hx0 hx1))
       (setf (dt-drag dt) (list :hscroll win)))
      ((zerop ly) (setf (dt-drag dt) (list :move win lx ly)))       ; title bar -> move
      (t (handle-event win e)))                                     ; interior -> widgets
    (invalidate dt)))

(defmethod handle-event ((dt desktop) (e mouse-event))
  (let* ((w (event-where e)) (x (car w)) (y (cdr w)) (mb (dt-menubar dt)))
    (cond
      ((dt-drag dt) (dt-drag-update dt e))                 ; in a move/resize drag
      ((and (typep e 'mouse-down) (logtest (event-buttons e) tvision::+mb-right+))  ; right-click -> context menu
       (let ((win (dt-window-at dt x y)))
         (when win
           (dt-raise dt win) (dt-refocus dt)
           (let ((v (view-at win x y)))                     ; position the cursor/selection, then pop up
             (when v (handle-event v (make-instance 'mouse-down :where (cons x y)
                                                     :buttons tvision::+mb-left+)))
             (let ((items (and v (context-menu v))))
               (when items (popup-menu items :x x :y (1+ y))))))
         (invalidate dt)))
      ((menu-hit-p mb x y) (handle-event mb e))
      ((and (typep e 'mouse-down) (= y (tvision::rect-ay (view-bounds (dt-statusbar dt)))))
       (handle-event (dt-statusbar dt) e))                 ; status-bar chips
      (t (let ((win (dt-window-at dt x y)))
           (when win (dt-window-click dt win e)))))))

(defun dt-status-items (dt)
  "The status-line chips for the current state: window actions when one is open,
plus the focused widget's own STATUS-HINTS, plus the always-on globals."
  (let* ((mb (dt-menubar dt)) (top (dt-top dt))
         (chips (list (cons "≡ Windows" (lambda () (setf (menu-active mb) 0 (menu-sel mb) 0) (invalidate dt)))
                      (cons "Tile"      (lambda () (dt-tile dt)))
                      (cons "Cascade"   (lambda () (dt-cascade dt)))
                      (cons "Help"      (lambda () (dt-help dt)))
                      (cons "Exit"      (lambda () (setf *app-done* t))))))
    (when top
      (setf chips (append (list (cons "Close" (lambda () (dt-close-window dt top)))) chips
                          (status-hints top)                       ; the window's own chips (any focus)
                          (status-hints (container-focus top)))))   ; plus the focused widget's
    chips))

;;; --- a dialog demonstrating field validators --------------------------------

(defun %validators-dialog ()
  "Modal dialog showing a range-validated and a picture-validated field."
  (let ((d (ui (dialog (:title " Field validators " :keymap *dialog-keys*
                        :value-fn (lambda (d) (declare (ignore d)) t))
                 (stack
                   (1 (row (20 (static-text :role :label :text " Age (1..120): "))
                           (:fill (input-line :name 'age :validator (range-validator 1 120)))))
                   (1 (row (20 (static-text :role :label :text " Date ##/##/####: "))
                           (:fill (input-line :name 'date :validator (picture-validator "##/##/####")))))
                   (1 (static-text :name 'msg :role :error :text ""))
                   (1 (static-text :role :status :text " letters are rejected in Age; OK validates the fields; Esc cancels "))
                   (1 (row (:fill (static-text :text ""))
                           (8  (button :label "OK"     :command 'accept))
                           (12 (button :label "Cancel" :command 'cancel)))))))))
    (exec-view d :width 52 :height 9)))

;;; --- a window demonstrating the table viewer --------------------------------

(defun make-package-table ()
  "All packages as a column table: name · external-symbol count · packages used."
  (let* ((rows (sort (copy-list (list-all-packages)) #'string< :key #'package-name))
         (win (ui (window (:title " Packages (table viewer) " :keymap *global-keys*)
                    (stack
                      (:fill (table-view :name 'tbl :rows rows
                               :columns (list
                                         (list "Package"  30 (lambda (p) (package-name p)))
                                         (list "External" 10 (lambda (p) (let ((n 0))
                                                                           (do-external-symbols (s p) (declare (ignore s)) (incf n)) n)))
                                         (list "Uses"     40 (lambda (p) (format nil "~{~a~^ ~}"
                                                                                (mapcar #'package-name (package-use-list p))))))))
                      (1 (static-text :role :status :text " ↑/↓ select · click a row · wheel scrolls · Esc closes ")))))))
    (setf (window-scroll-target win) (find-view win 'tbl) (window-help win) :browser)
    (values win (find-view win 'tbl))))

;;; --- a small window demonstrating the cluster controls ----------------------

(defun make-options ()
  "A demo window showing checkbox and radio clusters with a live echo."
  (let* ((win (ui (window (:title " Options (cluster controls) " :keymap *global-keys*)
                    (stack
                      (1 (static-text :role :label :text " Features — ↑/↓, Space or click toggles: "))
                      (4 (cluster :name 'features :mode :check
                           :items (list "Syntax highlight" "Word wrap" "Auto-indent" "Line numbers")
                           :value (list 0)))
                      (1 (static-text :role :label :text " Theme — radio: "))
                      (3 (cluster :name 'theme :mode :radio :items (list "Blue" "Dark" "Light") :value 0))
                      (:fill (static-text :name 'echo :role :status :text ""))
                      (1 (static-text :role :status :text " Space/click toggles · Tab switches groups · Esc closes ")))))))
    (values win (find-view win 'features))))

;;; --- desktop layout persistence (whole-desktop save / restore) --------------

(setf *window-builders*                                  ; keyword -> 0-arg builder (now that make-* exist)
      (list (cons :repl    #'make-repl)
            (cons :editor  (lambda () (make-editor)))
            (cons :project (lambda () (make-project)))
            (cons :packages #'make-packages)
            (cons :systems  #'make-systems)
            (cons :threads  #'make-threadmon)
            (cons :html     (lambda () (make-html)))
            (cons :ptable   #'make-package-table)
            (cons :options  #'make-options)))

(defun %desktop-file () (merge-pathnames ".tv2-desktop" (user-homedir-pathname)))

(defun dt-save-layout (dt &optional (path (%desktop-file)))
  "Write the open windows (kind + bounds, Z-order) to PATH.  Editors also save
their filename and -- for scratch or modified buffers -- their text, so a full
session (including unsaved work) is restored."
  (let ((layout (loop for w in (dt-windows dt) for k = (window-kind w) when k
                      collect (let ((b (view-bounds w)))
                                (list k (tvision::rect-ax b) (tvision::rect-ay b)
                                      (tvision::rect-bx b) (tvision::rect-by b)
                                      (and (eq k :editor)
                                           (let ((te (find-view w 'edit)))
                                             (when te
                                               ;; save text only for an unsaved scratch buffer; a named
                                               ;; file is reloaded from disk on restore (never clobbered)
                                               (list (and (te-filename te) (namestring (te-filename te)))
                                                     (when (null (te-filename te)) (te-text te)))))))))))
    (ignore-errors
     (with-open-file (s path :direction :output :if-exists :supersede :if-does-not-exist :create)
       (prin1 layout s)))
    layout))

(defun dt-load-layout (dt &optional (path (%desktop-file)))
  "Reopen the windows recorded in PATH at their saved positions, restoring saved
editor buffer text."
  (dolist (entry (ignore-errors (with-open-file (s path :if-does-not-exist nil) (and s (read s nil nil)))))
    (ignore-errors
     (destructuring-bind (kind x0 y0 x1 y1 &optional extra) entry
       (multiple-value-bind (win focus open)
           (if (eq kind :editor)
               ;; EXTRA is (filename text) -- or, from older sessions, a bare filename string
               (let ((fn (if (consp extra) (first extra) extra))
                     (txt (and (consp extra) (second extra))))
                 (multiple-value-bind (w f) (make-editor fn)
                   ;; restore saved text only for a scratch buffer (no filename); a
                   ;; named file was reloaded fresh by make-editor -- never overwrite it
                   (when (and txt (null fn))
                     (let ((te (find-view w 'edit))) (when te (te-set-text te txt) (setf (te-modified te) t))))
                   (values w f nil)))
               (let ((b (cdr (assoc kind *window-builders*)))) (when b (funcall b))))
         (when win (dt-add dt win focus open kind (rect x0 y0 x1 y1)))))))
  (dt-refocus dt) (invalidate dt))

;;; --- entry point ------------------------------------------------------------

(defvar *extra-menus* nil
  "Functions (DT) -> a menu spec, appended to the menu bar by later modules
(e.g. inspect.lisp's Inspect menu).  Most-recently-pushed appears last.")

(defun %about-dialog ()
  "The classic ≡ system-menu About box."
  (let ((d (ui (dialog (:title " About " :keymap *dialog-keys* :value-fn (constantly t))
                 (stack
                   (1 (static-text :role :label :text "    tvlisp — a Common Lisp IDE"))
                   (1 (static-text :role :label :text "    on the tv2 CLOS kernel"))
                   (1 (static-text :text ""))
                   (1 (static-text :text "    a Turbo Vision-style TUI, ported to SBCL"))
                   (:fill (static-text :text ""))
                   (1 (row (:fill (static-text :text "")) (8 (button :label "OK" :command 'accept))
                           (:fill (static-text :text "")))))))))
    (exec-view d :width 46 :height 10)))

(defun %desktop-menus (dt)
  (flet ((any-win () (lambda () (dt-top dt))))            ; ENABLED predicate: a window is open
    (%order-menus
     (append
      (list
       (list "≡"
             (list "About…"    (lambda () (%about-dialog))))
       (list "File"
             (list "Open file…"     (lambda () (let ((p (make-file-dialog :dir *project-dir* :title " Open file ")))
                                                 (when p (dt-open dt (lambda () (make-editor p)))))) (ctrl #\o))
             (list "Change dir…"    (lambda () (let ((p (make-file-dialog :dir *project-dir* :dirs-only t :title " Change dir ")))
                                                 (when p (setf *project-dir* (uiop:ensure-directory-pathname p))))))
             :--
             (list "Save"           (lambda () (%dt-save dt)) (ctrl #\s) (any-win))
             (list "Save as…"       (lambda () (%dt-save-as dt)) nil (any-win))
             (list "Save all"       (lambda () (%dt-save-all dt)) nil (any-win))
             (list "Reload file"    (lambda () (%dt-reload dt)) nil (any-win))
             :--
             (list "Save transcript…"  (lambda () (%dt-save-transcript dt)))
             (list "Save Lisp script…" (lambda () (%dt-save-script dt)))
             (list "Clear REPL"        (lambda () (%dt-clear-repl dt)))
             :--
             (list "Save layout"    (lambda () (dt-save-layout dt)))
             (list "Restore layout" (lambda () (mapc (lambda (w) (dt-close-window dt w)) (copy-list (dt-windows dt)))
                                      (dt-load-layout dt)) nil (lambda () t))
             :--
             (list "Exit"           (lambda () (setf *app-done* t)) (ctrl #\q)))
       (list "Window"                                     ; open tool windows + window management
             (list "Lisp REPL"       (lambda () (dt-open dt :repl)) (ctrl #\r))
             (list "Text editor"     (lambda () (dt-open dt :editor)))
             (list "Project manager" (lambda () (dt-open dt :project)))
             (list "Package browser" (lambda () (dt-open dt :packages)))
             (list "ASDF systems"    (lambda () (dt-open dt :systems)))
             (list "Thread monitor"  (lambda () (dt-open dt :threads)))
             (list "HTML browser"    (lambda () (dt-open dt :html)))
             (list "Package table"   (lambda () (dt-open dt :ptable)))
             :--
             (list "Size/move"       (lambda () (let ((top (dt-top dt)))
                                                  (when top (setf *sizemove-win* top)
                                                        (%tool-note "Size/move: arrows move · Shift+arrows resize · Enter/Esc done")))) nil (any-win))
             (list "Zoom"            (lambda () (let ((top (dt-top dt))) (when top (dt-zoom dt top)))) :f5 (any-win))
             (list "Next"            (lambda () (dt-next dt) (dt-refocus dt)) nil (any-win))
             (list "Previous"        (lambda () (dt-prev dt) (dt-refocus dt)) nil (any-win))
             (list "Tile"            (lambda () (dt-tile dt) (dt-refocus dt)) nil (any-win))
             (list "Cascade"         (lambda () (dt-cascade dt) (dt-refocus dt)) nil (any-win))
             (list "List…"           (lambda () (%dt-window-list dt)) nil (any-win))
             (list "Close"           (lambda () (let ((top (dt-top dt))) (when top (dt-close-window dt top)))) nil (any-win)))
       (list "Options"
             (list "Settings…"       (lambda () (dt-open dt :options)))
             (list "Colours…"        (lambda () (make-color-dialog)))
             (list "Colour theme"    (lambda () (cycle-theme dt)))
             (list "Validators…"     (lambda () (%validators-dialog)))
             (list "Eval timing"     (lambda () (setf *repl-time* (not *repl-time*))
                                       (%tool-note (if *repl-time* "eval timing ON" "eval timing OFF")))))
       (list "Help"
             (list "Contents"        (lambda () (dt-open dt (lambda () (make-help :general)))))
             (list "This window"     (lambda () (dt-help dt)) :f1)
             (list "Topics" :submenu
                   (list "Lisp REPL"       (lambda () (dt-open dt (lambda () (make-help :repl)))))
                   (list "Text editor"     (lambda () (dt-open dt (lambda () (make-help :editor)))))
                   (list "Project manager" (lambda () (dt-open dt (lambda () (make-help :project)))))
                   (list "Browsers"        (lambda () (dt-open dt (lambda () (make-help :browser)))))
                   (list "HTML browser"    (lambda () (dt-open dt (lambda () (make-help :html)))))
                   (list "Thread monitor"  (lambda () (dt-open dt (lambda () (make-help :threads))))))
             :--
             (list "About…"          (lambda () (%about-dialog)))))
      (mapcar (lambda (f) (funcall f dt)) (reverse *extra-menus*))))))   ; modules' Edit + consolidated Lisp menus

(defun ensure-repl ()
  "The desktop's REPL window, opening one if none is present.  Returns it."
  (when *desktop*
    (or (find :repl (dt-windows *desktop*) :key #'window-kind)
        (progn (dt-open *desktop* :repl) (dt-top *desktop*)))))

(defun run-desktop ()
  "Run the tv2 IDE: a Turbo-Vision-style desktop with a menu bar, a status bar,
and movable / resizable / overlapping windows (drag the title bar, drag the
bottom-right corner to resize, click [✕] to close; Window menu tiles/cascades).
Returns on File→Exit."
  (tvision:with-screen (s)
    (let ((dt (make-instance 'desktop)))
      (setf (dt-menubar dt)   (make-instance 'menu-bar :menus (%desktop-menus dt))
            (dt-statusbar dt)  (make-instance 'status-bar :provider (lambda () (dt-status-items dt))))
      (layout dt (rect 0 0 (tvision:screen-width s) (tvision:screen-height s)))
      (setf *root* dt *desktop* dt *ui-thread* sb-thread:*current-thread* *app-done* nil *dirty* t)
      (dt-load-layout dt)                                    ; restore the previous session's windows
      (loop until *app-done* do
        (drain-ui-callbacks)
        (when (%expire-tool-message) (setf *dirty* t))        ; auto-clear the status-bar note
        (when *dirty*
          (tvision:hide-cursor s)
          (draw dt) (tvision:flush-screen s) (setf *dirty* nil))
        (tvision::pump-input s 0.05)
        (let ((tev (tvision::screen-next-event s)))
          (when tev (let ((ev (translate tev))) (when ev (handle-event dt ev))))))
      (dt-save-layout dt)                                    ; persist the desktop for next launch
      (dolist (win (dt-windows dt))                          ; stop any open windows' threads
        (when (window-cleanup win) (ignore-errors (funcall (window-cleanup win))))))))
