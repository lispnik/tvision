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

;;; --- desktop ----------------------------------------------------------------

(defclass desktop (view)
  ((menubar   :accessor dt-menubar)
   (statusbar :accessor dt-statusbar)
   (win       :initform nil :accessor dt-win)         ; the hosted window (its OWN root), or NIL
   (cleanup   :initform nil :accessor dt-cleanup))    ; that window's cleanup thunk
  (:metaclass reactive-class))

(defun dt-content (dt)
  "The rectangle between the menu bar (row 0) and status bar (last row)."
  (let* ((r (view-bounds dt)) (ax (tvision::rect-ax r)) (ay (tvision::rect-ay r)) (w (r-w r)) (h (r-h r)))
    (rect ax (1+ ay) (+ ax w) (+ ay (1- h)))))

(defmethod layout ((dt desktop) r)
  (setf (view-bounds dt) r)
  (let ((ax (tvision::rect-ax r)) (ay (tvision::rect-ay r)) (w (r-w r)) (h (r-h r)))
    (layout (dt-menubar dt)   (rect ax ay (+ ax w) (+ ay 1)))
    (layout (dt-statusbar dt) (rect ax (+ ay (1- h)) (+ ax w) (+ ay h)))
    (when (dt-win dt) (layout (dt-win dt) (dt-content dt)))))

(defmethod draw ((dt desktop))
  (let* ((b (view-bounds dt)) (w (r-w b)) (h (r-h b))
         (ax (tvision::rect-ax b)) (ay (tvision::rect-ay b)) (bg (role :desktop)))
    (loop for y from (1+ ay) below (+ ay (1- h)) do          ; patterned background
      (loop for x from ax below (+ ax w) do (%put-cell x y #\░ bg)))
    (when (dt-win dt) (draw (dt-win dt)))                     ; the hosted window
    (draw (dt-statusbar dt))
    (draw (dt-menubar dt))))                                  ; menu + dropdown overlay everything

(defun dt-open (dt make-fn)
  "Open the window built by MAKE-FN inside the desktop and give it focus."
  (multiple-value-bind (win focus open) (funcall make-fn)
    (layout win (dt-content dt))
    (setf (container-focus win) (or focus (first (all-focusables win)))
          (dt-win dt) win
          (dt-cleanup dt) (and open (funcall open tvision:*screen*))
          (menu-active (dt-menubar dt)) nil)                 ; close the menu while the window is up
    (invalidate dt)))

(defun dt-close (dt)
  "Close the active window (running its cleanup) and reactivate the menu."
  (when (dt-cleanup dt) (ignore-errors (funcall (dt-cleanup dt))))
  (setf (dt-win dt) nil (dt-cleanup dt) nil
        (menu-active (dt-menubar dt)) 0 (menu-sel (dt-menubar dt)) 0)
  (invalidate dt))

(defmethod handle-event ((dt desktop) (e key-event))
  (if (dt-win dt)
      (if (eql (event-keysym e) :esc)                        ; Esc closes the window (it never sees Esc)
          (dt-close dt)
          (progn (setf *running* t)                          ; route to the window; its "quit" -> close
                 (handle-event (dt-win dt) e)
                 (unless *running* (dt-close dt))))
      (handle-event (dt-menubar dt) e)))                     ; bare desktop: the menu drives

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
        (list "File"
              (cons "Exit"             (lambda () (setf *app-done* t))))))

(defun run-desktop ()
  "Run the tv2 IDE: a Turbo-Vision-style desktop with a menu bar and status bar
hosting the ported windows.  Returns when the user picks File→Exit."
  (tvision:with-screen (s)
    (let ((dt (make-instance 'desktop)))
      (setf (dt-menubar dt)   (make-instance 'menu-bar :menus (%desktop-menus dt))
            (dt-statusbar dt)  (make-instance 'status-bar
                                 :text " ↑/↓ select · ←/→ menu · Enter open window · (in a window) Esc closes it · File→Exit quits "))
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
      (when (dt-win dt) (dt-close dt)))))                     ; stop any open window's threads
