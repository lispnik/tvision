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

;;; --- status bar -------------------------------------------------------------

(defclass status-bar (view)
  ((provider :initarg :provider :initform nil :accessor stb-provider)  ; thunk -> ((LABEL . THUNK) ...)
   (ranges   :initform '() :accessor stb-ranges))                      ; ((X0 X1 . THUNK) ...) for hit-testing
  (:metaclass reactive-class))

(defmethod draw ((b status-bar))
  (let* ((attr (role :status)) (w (r-w (view-bounds b)))
         (items (and (stb-provider b) (funcall (stb-provider b)))) (x 0))
    (fill-row b 0 0 w attr)
    (setf (stb-ranges b) '())
    (dolist (it items)
      (let* ((label (format nil " ~a " (car it))) (n (length label)))
        (when (< (+ x n) w)
          (draw-text b x 0 label attr)
          (push (list x (+ x n) (cdr it)) (stb-ranges b))
          (incf x n)
          (when (< (1+ x) w) (draw-text b x 0 "│" attr) (incf x 1)))))))

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
(defun item-label    (it) (first it))
(defun item-submenu-p (it) (and (consp it) (eq (second it) :submenu)))      ; (LABEL :submenu item...)
(defun item-thunk    (it) (unless (item-submenu-p it) (second it)))
(defun item-accel    (it) (unless (item-submenu-p it) (third it)))          ; submenu parents have no accel
(defun item-enabled  (it) (let ((f (and (not (item-submenu-p it)) (fourth it)))) (or (null f) (funcall f))))
(defun item-submenu   (it) (cddr it))
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
         (bar (role :status)) (hot (role :menu-hotkey)) (x 1))
    (fill-row mb 0 0 w bar)
    (loop for menu in (menu-menus mb) for i from 0 do
      (let* ((label (car menu)) (open (eql i (menu-active mb))) (attr (if open (role :button-focused) bar)))
        (%text-at (+ ax x) ay (format nil " ~a " label) attr)
        (%put-cell (+ ax x 1) ay (char label 0) (if open attr hot))   ; highlight the hotkey letter
        (when open
          (let* ((items (cdr menu)) (dx (+ ax x)) (dy (1+ ay)) (mw (menu-dropdown-width items)))
            (flet ((draw-items (items items-x items-y sel)
                     (loop for it in items for r from 0 do
                       (let* ((on (eql r sel)) (en (item-enabled it))
                              (ia (cond ((and on en) (role :focused)) (en (role :menu)) (t (role :menu-disabled))))
                              (mww (menu-dropdown-width items)))
                         (loop for k below mww do (%put-cell (+ items-x k) (+ items-y r) #\Space ia))
                         (%text-at (+ items-x 1) (+ items-y r) (item-label it) ia)
                         (cond ((item-submenu-p it) (%put-cell (+ items-x mww -2) (+ items-y r) #\▶ ia))
                               ((item-accel it) (let ((a (accel-label (item-accel it))))
                                                  (%text-at (+ items-x mww -1 (- (length a))) (+ items-y r) a ia))))))))
              (draw-items items dx dy (menu-sel mb))
              (when (menu-sub mb)                                      ; second-level dropdown
                (let ((parent (nth (menu-sel mb) items)))
                  (when (item-submenu-p parent)
                    (draw-items (item-submenu parent) (+ dx mw) (+ dy (menu-sel mb)) (menu-sub mb))))))))
        (incf x (+ 2 (length label)))))))

(defun menu-invoke-sel (mb)
  "Open a submenu parent, or invoke a normal selected item."
  (let ((it (nth (menu-sel mb) (menu-items mb))))
    (cond ((null it) nil)
          ((item-submenu-p it) (setf (menu-sub mb) 0) (invalidate mb))
          ((and (item-enabled it) (item-thunk it)) (funcall (item-thunk it))))))

(defmethod handle-event ((mb menu-bar) (e key-event))
  (when (menu-active mb)
    (let ((ks (event-keysym e)) (n (length (menu-menus mb))) (items (menu-items mb)))
      (if (menu-sub mb)                                            ; navigating an open submenu
          (let ((subs (item-submenu (nth (menu-sel mb) items))))
            (cond
              ((eql ks :up)   (setf (menu-sub mb) (mod (1- (menu-sub mb)) (length subs))) (invalidate mb) (setf (handled-p e) t))
              ((eql ks :down) (setf (menu-sub mb) (mod (1+ (menu-sub mb)) (length subs))) (invalidate mb) (setf (handled-p e) t))
              ((member ks '(:left :esc)) (setf (menu-sub mb) nil) (invalidate mb) (setf (handled-p e) t))
              ((eql ks :enter) (let ((it (nth (menu-sub mb) subs)))
                                 (when (and it (item-enabled it) (item-thunk it)) (funcall (item-thunk it))))
                               (setf (handled-p e) t))))
          (cond
            ((eql ks :left)  (setf (menu-active mb) (mod (1- (menu-active mb)) n) (menu-sel mb) 0 (menu-sub mb) nil) (invalidate mb) (setf (handled-p e) t))
            ((eql ks :right) (let ((it (nth (menu-sel mb) items)))
                               (if (item-submenu-p it) (setf (menu-sub mb) 0)
                                   (setf (menu-active mb) (mod (1+ (menu-active mb)) n) (menu-sel mb) 0)))
                             (invalidate mb) (setf (handled-p e) t))
            ((eql ks :up)    (setf (menu-sel mb) (mod (1- (menu-sel mb)) (length items)) (menu-sub mb) nil) (invalidate mb) (setf (handled-p e) t))
            ((eql ks :down)  (setf (menu-sel mb) (mod (1+ (menu-sel mb)) (length items)) (menu-sub mb) nil) (invalidate mb) (setf (handled-p e) t))
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
  "(values SX SY0 COUNT WIDTH) of the open submenu dropdown, or NIL."
  (when (and (menu-active mb) (menu-sub mb))
    (let ((parent (nth (menu-sel mb) (menu-items mb))))
      (when (item-submenu-p parent)
        (multiple-value-bind (x0 mw) (menu-dropdown-cols mb)
          (values (+ x0 mw) (1+ (menu-sel mb)) (length (item-submenu parent))
                  (menu-dropdown-width (item-submenu parent))))))))

(defun menu-hit-p (mb x y)
  (or (zerop y)
      (and (menu-active mb) (plusp y) (<= y (length (menu-items mb)))
           (multiple-value-bind (x0 mw) (menu-dropdown-cols mb)
             (and x0 (>= x x0) (< x (+ x0 mw)))))
      (multiple-value-bind (sx sy0 cnt smw) (menu-sub-cols mb)   ; open submenu region
        (and sx (>= x sx) (< x (+ sx smw)) (>= y sy0) (< y (+ sy0 cnt))))))

(defmethod handle-event ((mb menu-bar) (e mouse-down))
  (let ((col (mouse-col mb e)) (row (mouse-row mb e)))
    (multiple-value-bind (sx sy0 cnt smw) (menu-sub-cols mb)
      (cond
        ((and sx (>= col sx) (< col (+ sx smw)) (>= row sy0) (< row (+ sy0 cnt)))   ; submenu item
         (let* ((idx (- row sy0)) (subs (item-submenu (nth (menu-sel mb) (menu-items mb)))) (it (nth idx subs)))
           (setf (menu-sub mb) idx) (invalidate mb)
           (when (and it (item-enabled it) (item-thunk it)) (funcall (item-thunk it)))))
        ((zerop row)                                  ; clicked a title -> open that menu
         (let ((x 1))
           (loop for menu in (menu-menus mb) for i from 0 do
             (let ((tw (+ 2 (length (car menu)))))
               (when (and (>= col x) (< col (+ x tw)))
                 (setf (menu-active mb) i (menu-sel mb) 0 (menu-sub mb) nil) (invalidate mb) (return))
               (incf x tw)))))
        ((menu-active mb)                             ; clicked a dropdown item -> invoke / open submenu
         (let ((idx (1- row)) (items (menu-items mb)))
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
      (loop for x from ax below (+ ax w) do (%put-cell x y #\░ bg)))
    (dolist (win (dt-windows dt))                            ; windows back-to-front
      (setf (window-active win) (eq win top)) (draw win))
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

(defmethod handle-event ((dt desktop) (e key-event))
  (let* ((mb (dt-menubar dt)) (top (dt-top dt)) (ks (event-keysym e))
         (alt (logtest (event-modifiers e) tvision::+md-alt+)))
    (cond
      ((and alt (characterp ks) (menu-hotkey-index mb ks))   ; Alt-<hotkey> opens that menu
       (setf (menu-active mb) (menu-hotkey-index mb ks) (menu-sel mb) 0) (invalidate mb))
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
              (when sy0 (%scroll-from-click (window-scroll-target win) my sy0 sy1)))))
         (invalidate dt))))))

(defun dt-window-click (dt win e)
  (dt-raise dt win)
  (let* ((b (view-bounds win)) (lx (mouse-col win e)) (ly (mouse-row win e)) (w (r-w b)) (h (r-h b)))
    (cond
      ((not (typep e 'mouse-down)) (handle-event win e))            ; wheel etc. -> widgets
      ((and (zerop ly) (<= 1 lx 3)) (dt-close-window dt win))       ; [✕] close box
      ((and (= lx (1- w)) (= ly (1- h))) (setf (dt-drag dt) (list :resize win)))  ; resize grip
      ((and (= lx (1- w)) (window-scroll-target win) (>= ly 1) (<= ly (- h 2)))   ; frame scrollbar
       (multiple-value-bind (sx sy0 sy1) (window-vscroll-bounds win)
         (declare (ignore sx))
         (%scroll-from-click (window-scroll-target win) (cdr (event-where e)) sy0 sy1))
       (setf (dt-drag dt) (list :scroll win)))
      ((zerop ly) (setf (dt-drag dt) (list :move win lx ly)))       ; title bar -> move
      (t (handle-event win e)))                                     ; interior -> widgets
    (invalidate dt)))

(defmethod handle-event ((dt desktop) (e mouse-event))
  (let* ((w (event-where e)) (x (car w)) (y (cdr w)) (mb (dt-menubar dt)))
    (cond
      ((dt-drag dt) (dt-drag-update dt e))                 ; in a move/resize drag
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
                          (status-hints (container-focus top)))))
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
  "Write the open windows (kind + bounds, Z-order; editor filename) to PATH."
  (let ((layout (loop for w in (dt-windows dt) for k = (window-kind w) when k
                      collect (let ((b (view-bounds w)))
                                (list k (tvision::rect-ax b) (tvision::rect-ay b)
                                      (tvision::rect-bx b) (tvision::rect-by b)
                                      (and (eq k :editor)
                                           (let ((te (find-view w 'edit)))
                                             (and te (te-filename te) (namestring (te-filename te))))))))))
    (ignore-errors
     (with-open-file (s path :direction :output :if-exists :supersede :if-does-not-exist :create)
       (prin1 layout s)))
    layout))

(defun dt-load-layout (dt &optional (path (%desktop-file)))
  "Reopen the windows recorded in PATH at their saved positions."
  (dolist (entry (ignore-errors (with-open-file (s path :if-does-not-exist nil) (and s (read s nil nil)))))
    (ignore-errors
     (destructuring-bind (kind x0 y0 x1 y1 &optional extra) entry
       (multiple-value-bind (win focus open)
           (if (eq kind :editor) (make-editor extra)
               (let ((b (cdr (assoc kind *window-builders*)))) (when b (funcall b))))
         (when win (dt-add dt win focus open kind (rect x0 y0 x1 y1)))))))
  (dt-refocus dt) (invalidate dt))

;;; --- entry point ------------------------------------------------------------

(defun %desktop-menus (dt)
  (flet ((any-win () (lambda () (dt-top dt))))            ; ENABLED predicate: a window is open
    (list (list "Windows"
                (list "Lisp REPL"        (lambda () (dt-open dt :repl)) (ctrl #\r))
                (list "Text editor"      (lambda () (dt-open dt :editor)))
                (list "Project manager"  (lambda () (dt-open dt :project)))
                (list "Package browser"  (lambda () (dt-open dt :packages)))
                (list "ASDF systems"     (lambda () (dt-open dt :systems)))
                (list "Thread monitor"   (lambda () (dt-open dt :threads)))
                (list "HTML browser"     (lambda () (dt-open dt :html)))
                (list "Package table"    (lambda () (dt-open dt :ptable)))
                (list "Options"          (lambda () (dt-open dt :options))))
          (list "Arrange"                                  ; window management (dimmed with no windows)
                (list "Next"             (lambda () (dt-next dt) (dt-refocus dt)) nil (any-win))
                (list "Tile"             (lambda () (dt-tile dt) (dt-refocus dt)) nil (any-win))
                (list "Cascade"          (lambda () (dt-cascade dt) (dt-refocus dt)) nil (any-win))
                (list "Close"            (lambda () (let ((top (dt-top dt))) (when top (dt-close-window dt top)))) nil (any-win)))
          (list "File"
                (list "Open file…"   (lambda () (let ((p (make-file-dialog :dir *project-dir* :title " Open file ")))
                                                  (when p (dt-open dt (lambda () (make-editor p)))))) (ctrl #\o))
                (list "Change dir…"  (lambda () (let ((p (make-file-dialog :dir *project-dir* :dirs-only t :title " Change dir ")))
                                                  (when p (setf *project-dir* (uiop:ensure-directory-pathname p))))))
                (list "Colours…"     (lambda () (make-color-dialog)))
                (list "Validators…" (lambda () (%validators-dialog)))
                (list "Save layout"  (lambda () (dt-save-layout dt)))
                (list "Restore layout" (lambda () (mapc (lambda (w) (dt-close-window dt w)) (copy-list (dt-windows dt)))
                                         (dt-load-layout dt)) nil (lambda () t))
                (list "Exit"         (lambda () (setf *app-done* t)) (ctrl #\q)))
          (list "Help"
                (list "Contents"     (lambda () (dt-open dt (lambda () (make-help :general)))))
                (list "This window"  (lambda () (dt-help dt)) :f1)
                (list "Topics" :submenu                       ; a nested submenu
                      (list "Lisp REPL"       (lambda () (dt-open dt (lambda () (make-help :repl)))))
                      (list "Text editor"     (lambda () (dt-open dt (lambda () (make-help :editor)))))
                      (list "Project manager" (lambda () (dt-open dt (lambda () (make-help :project)))))
                      (list "Browsers"        (lambda () (dt-open dt (lambda () (make-help :browser)))))
                      (list "HTML browser"    (lambda () (dt-open dt (lambda () (make-help :html)))))
                      (list "Thread monitor"  (lambda () (dt-open dt (lambda () (make-help :threads))))))))))

(defun ensure-repl ()
  "The desktop's REPL window, opening one if none is present.  Returns it."
  (when *desktop*
    (or (find :repl (dt-windows *desktop*) :key #'window-kind)
        (progn (dt-open *desktop* :repl) (dt-top *desktop*)))))

(defun run-desktop ()
  "Run the tv2 IDE: a Turbo-Vision-style desktop with a menu bar, a status bar,
and movable / resizable / overlapping windows (drag the title bar, drag the ◢
grip, click [✕] to close; Window menu tiles/cascades).  Returns on File→Exit."
  (tvision:with-screen (s)
    (let ((dt (make-instance 'desktop)))
      (setf (dt-menubar dt)   (make-instance 'menu-bar :menus (%desktop-menus dt))
            (dt-statusbar dt)  (make-instance 'status-bar :provider (lambda () (dt-status-items dt))))
      (layout dt (rect 0 0 (tvision:screen-width s) (tvision:screen-height s)))
      (setf *root* dt *desktop* dt *ui-thread* sb-thread:*current-thread* *app-done* nil *dirty* t)
      (dt-load-layout dt)                                    ; restore the previous session's windows
      (loop until *app-done* do
        (drain-ui-callbacks)
        (when *dirty*
          (tvision:hide-cursor s)
          (draw dt) (tvision:flush-screen s) (setf *dirty* nil))
        (tvision::pump-input s 0.05)
        (let ((tev (tvision::screen-next-event s)))
          (when tev (let ((ev (translate tev))) (when ev (handle-event dt ev))))))
      (dt-save-layout dt)                                    ; persist the desktop for next launch
      (dolist (win (dt-windows dt))                          ; stop any open windows' threads
        (when (window-cleanup win) (ignore-errors (funcall (window-cleanup win))))))))
