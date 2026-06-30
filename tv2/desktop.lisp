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

;;; --- status bar -------------------------------------------------------------

(defclass status-bar (view)
  ((text :initarg :text :initform "" :accessor sb-text))
  (:metaclass reactive-class))

(defmethod draw ((b status-bar))
  (let ((attr (role :status)) (w (r-w (view-bounds b))))
    (fill-row b 0 0 w attr)
    (draw-text b 0 0 (sb-text b) attr)))

;;; --- menu bar (pull-down menus) ---------------------------------------------

(defclass menu-bar (view)
  ((menus  :initarg :menus :initform '() :accessor menu-menus)   ; ((LABEL (ITEM . THUNK) ...) ...)
   (active :initform 0 :accessor menu-active)                    ; open menu index, or NIL (inactive)
   (sel    :initform 0 :accessor menu-sel))                      ; selected item in the open menu
  (:metaclass reactive-class))

(defun menu-items (mb) (cdr (nth (menu-active mb) (menu-menus mb))))

(defmethod draw ((mb menu-bar))
  (let* ((b (view-bounds mb)) (w (r-w b))
         (ax (tvision::rect-ax b)) (ay (tvision::rect-ay b))
         (bar (role :status)) (x 1))
    (fill-row mb 0 0 w bar)
    (loop for menu in (menu-menus mb) for i from 0 do
      (let* ((title (format nil " ~a " (car menu)))
             (open (and (menu-active mb) (= i (menu-active mb)))))
        (%text-at (+ ax x) ay title (if open (role :button-focused) bar))
        (when open
          (let* ((items (cdr menu)) (dx (+ ax x)) (dy (1+ ay))
                 (mw (+ 3 (reduce #'max items :key (lambda (it) (length (car it))) :initial-value 8))))
            (loop for it in items for r from 0 do
              (let ((ia (if (= r (menu-sel mb)) (role :focused) (role :menu))))
                (loop for k below mw do (%put-cell (+ dx k) (+ dy r) #\Space ia))
                (%text-at (+ dx 1) (+ dy r) (car it) ia)))))
        (incf x (length title))))))

(defmethod handle-event ((mb menu-bar) (e key-event))
  (when (menu-active mb)
    (let ((ks (event-keysym e)) (n (length (menu-menus mb))) (items (menu-items mb)))
      (cond
        ((eql ks :left)  (setf (menu-active mb) (mod (1- (menu-active mb)) n) (menu-sel mb) 0) (invalidate mb) (setf (handled-p e) t))
        ((eql ks :right) (setf (menu-active mb) (mod (1+ (menu-active mb)) n) (menu-sel mb) 0) (invalidate mb) (setf (handled-p e) t))
        ((eql ks :up)    (setf (menu-sel mb) (mod (1- (menu-sel mb)) (length items))) (invalidate mb) (setf (handled-p e) t))
        ((eql ks :down)  (setf (menu-sel mb) (mod (1+ (menu-sel mb)) (length items))) (invalidate mb) (setf (handled-p e) t))
        ((eql ks :enter) (let ((thunk (cdr (nth (menu-sel mb) items)))) (when thunk (funcall thunk))) (setf (handled-p e) t))))))

(defun menu-title-x (mb i)
  "Screen column (menu-bar-local) of menu I's title (matches DRAW)."
  (let ((x 1))
    (dotimes (k i x) (incf x (+ 2 (length (car (nth k (menu-menus mb)))))))))

(defun menu-dropdown-cols (mb)
  "(values X0 WIDTH) of the open dropdown, or NIL."
  (when (menu-active mb)
    (let* ((items (menu-items mb)) (x0 (menu-title-x mb (menu-active mb)))
           (mw (+ 3 (reduce #'max items :key (lambda (it) (length (car it))) :initial-value 8))))
      (values x0 mw))))

(defun menu-hit-p (mb x y)
  "True when screen point (X,Y) is on the bar (row 0) or the open dropdown."
  (or (zerop y)
      (and (menu-active mb) (plusp y) (<= y (length (menu-items mb)))
           (multiple-value-bind (x0 mw) (menu-dropdown-cols mb)
             (and x0 (>= x x0) (< x (+ x0 mw)))))))

(defmethod handle-event ((mb menu-bar) (e mouse-down))
  (let ((col (mouse-col mb e)) (row (mouse-row mb e)))
    (if (zerop row)                                   ; clicked a title -> open that menu
        (let ((x 1))
          (loop for menu in (menu-menus mb) for i from 0 do
            (let ((tw (+ 2 (length (car menu)))))
              (when (and (>= col x) (< col (+ x tw)))
                (setf (menu-active mb) i (menu-sel mb) 0) (invalidate mb) (return))
              (incf x tw))))
        (when (menu-active mb)                        ; clicked a dropdown item -> invoke
          (let ((idx (1- row)) (items (menu-items mb)))
            (when (and (>= idx 0) (< idx (length items)))
              (setf (menu-sel mb) idx) (invalidate mb)
              (let ((thunk (cdr (nth idx items)))) (when thunk (funcall thunk)))))))
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
        (menu-sel (dt-menubar dt)) 0))

(defun dt-open (dt make-fn)
  "Open MAKE-FN's window on the desktop at a cascade offset, focused on top."
  (multiple-value-bind (win focus open) (funcall make-fn)
    (let* ((c (dt-content dt)) (n (length (dt-windows dt)))
           (cw (max 40 (floor (* (r-w c) 4) 5))) (ch (max 8 (floor (* (r-h c) 4) 5)))
           (ox (+ (tvision::rect-ax c) (* (mod n 6) 3)))
           (oy (+ (tvision::rect-ay c) (* (mod n 6) 2))))
      (layout win (rect ox oy (min (+ ox cw) (tvision::rect-bx c)) (min (+ oy ch) (tvision::rect-by c))))
      (setf (window-managed win) t
            (container-focus win) (or focus (first (all-focusables win)))
            (window-cleanup win) (and open (funcall open tvision:*screen*)))
      (dt-raise dt win))
    (dt-refocus dt) (invalidate dt)))

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

(defmethod handle-event ((dt desktop) (e key-event))
  (let ((top (dt-top dt)))
    (if top
        (if (eql (event-keysym e) :esc) (dt-close-window dt top)   ; Esc closes the top window
            (progn (setf *running* t) (handle-event top e)
                   (unless *running* (dt-close-window dt top))))
        (handle-event (dt-menubar dt) e))))                  ; no windows: the menu drives

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
              (layout win (rect ax ay nx2 ny2)))))
         (invalidate dt))))))

(defun dt-window-click (dt win e)
  (dt-raise dt win)
  (let* ((b (view-bounds win)) (lx (mouse-col win e)) (ly (mouse-row win e)) (w (r-w b)) (h (r-h b)))
    (cond
      ((not (typep e 'mouse-down)) (handle-event win e))            ; wheel etc. -> widgets
      ((and (zerop ly) (<= 1 lx 3)) (dt-close-window dt win))       ; [✕] close box
      ((and (= lx (1- w)) (= ly (1- h))) (setf (dt-drag dt) (list :resize win)))  ; resize grip
      ((zerop ly) (setf (dt-drag dt) (list :move win lx ly)))       ; title bar -> move
      (t (handle-event win e)))                                     ; interior -> widgets
    (invalidate dt)))

(defmethod handle-event ((dt desktop) (e mouse-event))
  (let* ((w (event-where e)) (x (car w)) (y (cdr w)) (mb (dt-menubar dt)))
    (cond
      ((dt-drag dt) (dt-drag-update dt e))                 ; in a move/resize drag
      ((menu-hit-p mb x y) (handle-event mb e))
      (t (let ((win (dt-window-at dt x y)))
           (when win (dt-window-click dt win e)))))))

;;; --- entry point ------------------------------------------------------------

(defun %desktop-menus (dt)
  (list (list "Windows"
              (cons "Lisp REPL"        (lambda () (dt-open dt #'make-repl)))
              (cons "Text editor"      (lambda () (dt-open dt (lambda () (make-editor)))))
              (cons "Project manager"  (lambda () (dt-open dt (lambda () (make-project)))))
              (cons "Package browser"  (lambda () (dt-open dt #'make-packages)))
              (cons "ASDF systems"     (lambda () (dt-open dt #'make-systems)))
              (cons "Thread monitor"   (lambda () (dt-open dt #'make-threadmon)))
              (cons "HTML browser"     (lambda () (dt-open dt (lambda () (make-html))))))
        (list "Window"
              (cons "Next"             (lambda () (dt-next dt) (dt-refocus dt)))
              (cons "Tile"             (lambda () (dt-tile dt) (dt-refocus dt)))
              (cons "Cascade"          (lambda () (dt-cascade dt) (dt-refocus dt)))
              (cons "Close"            (lambda () (let ((top (dt-top dt))) (when top (dt-close-window dt top))))))
        (list "File"
              (cons "Exit"             (lambda () (setf *app-done* t))))))

(defun run-desktop ()
  "Run the tv2 IDE: a Turbo-Vision-style desktop with a menu bar, a status bar,
and movable / resizable / overlapping windows (drag the title bar, drag the ◢
grip, click [✕] to close; Window menu tiles/cascades).  Returns on File→Exit."
  (tvision:with-screen (s)
    (let ((dt (make-instance 'desktop)))
      (setf (dt-menubar dt)   (make-instance 'menu-bar :menus (%desktop-menus dt))
            (dt-statusbar dt)  (make-instance 'status-bar
                                 :text " click menus/rows/links · drag title to move · drag ◢ to resize · [✕] closes · Window: tile/cascade · File→Exit "))
      (layout dt (rect 0 0 (tvision:screen-width s) (tvision:screen-height s)))
      (setf *root* dt *ui-thread* sb-thread:*current-thread* *app-done* nil *dirty* t)
      (loop until *app-done* do
        (drain-ui-callbacks)
        (when *dirty*
          (tvision:hide-cursor s)
          (draw dt) (tvision:flush-screen s) (setf *dirty* nil))
        (tvision::pump-input s 0.05)
        (let ((tev (tvision::screen-next-event s)))
          (when tev (let ((ev (translate tev))) (when ev (handle-event dt ev))))))
      (dolist (win (dt-windows dt))                          ; stop any open windows' threads
        (when (window-cleanup win) (ignore-errors (funcall (window-cleanup win))))))))
