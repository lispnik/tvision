;;;; kernel.lisp --- the tv2 kernel: reactive metaclass, views, class events,
;;;; keymaps, commands, and the (view x event) dispatch protocol.

(in-package #:tv2)

;;; ---------------------------------------------------------------------------
;;; Reactive metaclass: mutating any slot of a reactive instance invalidates the
;;; screen, so views never call DRAW on themselves -- the loop commits one frame
;;; per iteration.  (A refinement would mark individual slots :reactive and track
;;; damage regions; whole-instance invalidation is enough to prove the model.)
;;; ---------------------------------------------------------------------------

(defclass reactive-class (standard-class) ())
(defmethod sb-mop:validate-superclass ((c reactive-class) (s standard-class)) t)

(defvar *dirty* nil "Set when reactive state changed since the last frame.")
(defvar *root* nil "The current top-level window (the modal background).")
(defun invalidate (object) (declare (ignore object)) (setf *dirty* t))

(defmethod (setf sb-mop:slot-value-using-class) :after
    (new (class reactive-class) object slot)
  (declare (ignore new slot))
  (invalidate object))

;;; ---------------------------------------------------------------------------
;;; Views.  Bounds reuse tvision's TRECT; capabilities would be mixins -- here we
;;; only need the base + OUTLINE.
;;; ---------------------------------------------------------------------------

(defclass view ()
  ((bounds :initarg :bounds :initform nil :accessor view-bounds)
   (owner  :initarg :owner  :initform nil :accessor view-owner)
   (name   :initarg :name   :initform nil :accessor view-name)
   (keymap :initarg :keymap :initform nil :accessor view-keymap))
  (:metaclass reactive-class))

(defgeneric draw (view)
  (:documentation "Render VIEW into the screen back buffer, within its bounds."))

(defgeneric layout (view rect)
  (:documentation "Assign VIEW (and its subtree) bounds within RECT.")
  (:method ((v view) rect) (setf (view-bounds v) rect)))

;;; geometry shorthands over tvision's TRECT
(defun rect (x0 y0 x1 y1) (tvision::make-trect x0 y0 x1 y1))
(defun r-x0 (r) (tvision::rect-ax r))   (defun r-y0 (r) (tvision::rect-ay r))
(defun r-x1 (r) (tvision::rect-bx r))   (defun r-y1 (r) (tvision::rect-by r))
(defun r-w  (r) (tvision::rect-width r)) (defun r-h (r) (tvision::rect-height r))

;;; ---------------------------------------------------------------------------
;;; Events as a class hierarchy -- dispatched on (view x event), no type tags.
;;; ---------------------------------------------------------------------------

(defclass event () ((handled :initform nil :accessor handled-p)))
(defclass key-event (event)
  ((keysym    :initarg :keysym    :reader event-keysym)        ; a character or keyword (:up …)
   (modifiers :initarg :modifiers :initform 0 :reader event-modifiers)))
(defclass mouse-event (event)
  ((where   :initarg :where   :reader event-where)
   (buttons :initarg :buttons :initform 0 :reader event-buttons)))
(defclass mouse-down (mouse-event) ((double :initarg :double :initform nil :reader event-double)))
(defclass mouse-up   (mouse-event) ())
(defclass mouse-move (mouse-event) ())
(defclass wheel-event (mouse-event) ((delta :initarg :delta :reader event-delta)))
(defclass command-event (event) ((command :initarg :command :reader event-command)))
(defclass broadcast-event (event)
  ((id :initarg :id :reader event-id) (info :initarg :info :initform nil :reader event-info)))
(defclass idle-event (event) ())

;;; ---------------------------------------------------------------------------
;;; Keymaps: input bindings are *data* (layered, introspectable, rebindable).
;;; A binding maps a keysym -> a command name (symbol).
;;; ---------------------------------------------------------------------------

(defclass keymap ()
  ((parent   :initarg :parent :initform nil :reader keymap-parent)
   (bindings :initform (make-hash-table :test 'equal) :reader keymap-bindings)))

(defun bind-key (km keysym command) (setf (gethash keysym (keymap-bindings km)) command))

(defun keymap-lookup (km keysym)
  (and km (or (gethash keysym (keymap-bindings km))
              (keymap-lookup (keymap-parent km) keysym))))

(defmacro defkeymap (name (&optional parent) &body bindings)
  "Define a keymap NAME (optionally inheriting PARENT) from (KEYSYM COMMAND) pairs."
  `(defparameter ,name
     (let ((km (make-instance 'keymap :parent ,parent)))
       ,@(loop for (k c) in bindings collect `(bind-key km ,k ',c))
       km)))

;;; ---------------------------------------------------------------------------
;;; Commands: behaviour is an object (with a reactive ENABLED slot, so disabling
;;; one auto-repaints anything that shows it), not an integer + a central COND.
;;; ---------------------------------------------------------------------------

(defclass command ()
  ((name    :initarg :name    :reader command-name)
   (action  :initarg :action  :reader command-action)
   (enabled :initarg :enabled :initform t :accessor command-enabled-p))
  (:metaclass reactive-class))

(defvar *commands* (make-hash-table) "Name -> COMMAND object.")

(defun register-command (name action)
  (setf (gethash name *commands*) (make-instance 'command :name name :action action)))

(defmacro define-command (name (view event) &body body)
  "Define and register command NAME with an action over (VIEW EVENT)."
  `(register-command ',name (lambda (,view ,event)
                              (declare (ignorable ,view ,event)) ,@body)))

(defgeneric perform (command view event)
  (:documentation "Run COMMAND (a command object or its name) for VIEW/EVENT.")
  (:method ((c command) view event)
    (when (command-enabled-p c) (funcall (command-action c) view event)))
  (:method ((c symbol) view event)
    (let ((cmd (gethash c *commands*))) (when cmd (perform cmd view event)))))

;;; ---------------------------------------------------------------------------
;;; Dispatch: handle-event is a multimethod on (view x event).  The base view
;;; turns a key into a command via its keymap chain; everything else is methods.
;;; ---------------------------------------------------------------------------

(defgeneric handle-event (view event)
  (:method ((v view) (e event)) nil))

(defmethod handle-event ((v view) (e key-event))
  (let ((cmd (keymap-lookup (view-keymap v) (event-keysym e))))
    (when cmd (perform cmd v e) (setf (handled-p e) t))))

;;; ---------------------------------------------------------------------------
;;; Drawing helpers (write packed cells straight to tvision's back buffer,
;;; clipped to the view's bounds).
;;; ---------------------------------------------------------------------------

(defun %put-cell (x y char attr)
  (when tvision:*screen*
    (tvision:screen-cell-set tvision:*screen* x y
                             (tvision::cell-make-code (char-code char) attr))))

(defun draw-text (view col row string attr)
  "Write STRING at view-local (COL,ROW), clipped to VIEW's width."
  (let* ((b (view-bounds view))
         (gx (+ (tvision::rect-ax b) col)) (gy (+ (tvision::rect-ay b) row))
         (w (tvision::rect-width b)))
    (loop for i below (min (length string) (max 0 (- w col)))
          do (%put-cell (+ gx i) gy (char string i) attr))))

(defun fill-row (view col row width attr)
  (let* ((b (view-bounds view))
         (gx (+ (tvision::rect-ax b) col)) (gy (+ (tvision::rect-ay b) row)))
    (dotimes (i width) (%put-cell (+ gx i) gy #\Space attr))))

;;; ---------------------------------------------------------------------------
;;; Terminal -> tv2 event translation.  Reuse tvision's escape-sequence decoder;
;;; map its key codes to keysyms (special keys -> keywords, printable -> chars).
;;; ---------------------------------------------------------------------------

(defparameter *special-keys*
  (list (cons tvision:+kb-up+ :up)     (cons tvision:+kb-down+ :down)
        (cons tvision:+kb-left+ :left) (cons tvision:+kb-right+ :right)
        (cons tvision:+kb-enter+ :enter) (cons tvision:+kb-esc+ :esc)
        (cons tvision:+kb-home+ :home) (cons tvision:+kb-end+ :end)
        (cons tvision:+kb-pgup+ :pgup) (cons tvision:+kb-pgdn+ :pgdn)
        (cons tvision:+kb-tab+ :tab)   (cons tvision:+kb-shift-tab+ :shift-tab)
        (cons tvision::+kb-back+ :back) (cons tvision::+kb-del+ :del)))

(defun translate (tev)
  "Translate a tvision event struct into a tv2 event object, or NIL to ignore."
  (let ((ty (tvision::event-type tev)))
    (cond
      ((= ty tvision:+ev-key-down+)
       (let* ((k (tvision::event-key-code tev)) (c (tvision::event-char-code tev))
              (ks (or (cdr (assoc k *special-keys*)) (and (plusp c) (code-char c)))))
         (and ks (make-instance 'key-event :keysym ks
                                :modifiers (tvision::event-modifiers tev)))))
      ((= ty tvision:+ev-mouse-wheel+)
       (make-instance 'wheel-event :delta (tvision::event-wheel tev) :where (%where tev)))
      ((= ty tvision::+ev-mouse-down+)
       (make-instance 'mouse-down :where (%where tev) :buttons (tvision::event-mouse-buttons tev)))
      ((= ty tvision::+ev-mouse-up+)
       (make-instance 'mouse-up :where (%where tev) :buttons (tvision::event-mouse-buttons tev)))
      ((member ty (list tvision::+ev-mouse-move+ tvision::+ev-mouse-auto+))
       (make-instance 'mouse-move :where (%where tev) :buttons (tvision::event-mouse-buttons tev)))
      (t nil))))

(defun %where (tev)
  "Mouse position of TEV as a (X . Y) cons in screen coordinates."
  (let ((p (tvision::event-mouse-where tev)))
    (cons (tvision::point-x p) (tvision::point-y p))))

;;; ---------------------------------------------------------------------------
;;; Theming: colours are named roles resolved through *THEME* (a plist), instead
;;; of byte palettes walked up the owner chain.
;;; ---------------------------------------------------------------------------

(defparameter *theme*
  (list :normal         (tvision:make-attr 7 1)     ; light grey on blue
        :focused        (tvision:make-attr 15 3)    ; white on cyan
        :frame          (tvision:make-attr 15 1)    ; bright white on blue (active window)
        :frame-inactive (tvision:make-attr 7 1)      ; grey on blue (background window)
        :status         (tvision:make-attr 0 6)     ; black on cyan
        :button         (tvision:make-attr 0 7)     ; black on grey
        :button-focused (tvision:make-attr 15 4)     ; white on magenta
        :label          (tvision:make-attr 14 1)     ; yellow on blue
        :input          (tvision:make-attr 7 0)      ; grey on black (a field)
        :input-focused  (tvision:make-attr 15 0)     ; white on black
        :error          (tvision:make-attr 15 4)     ; white on red
        :menu           (tvision:make-attr 0 7)      ; black on grey (menu dropdown)
        :desktop        (tvision:make-attr 8 1))     ; dim ░ pattern on blue (the desktop)
  "Role -> packed attribute.")

(defun role (key) (or (getf *theme* key) (tvision:make-attr 7 0)))

;;; ---------------------------------------------------------------------------
;;; Chrome helpers (box, centred text).
;;; ---------------------------------------------------------------------------

(defun %box (x0 y0 x1 y1 attr)
  (%put-cell x0 y0 #\┌ attr) (%put-cell x1 y0 #\┐ attr)
  (%put-cell x0 y1 #\└ attr) (%put-cell x1 y1 #\┘ attr)
  (loop for x from (1+ x0) below x1 do (%put-cell x y0 #\─ attr) (%put-cell x y1 #\─ attr))
  (loop for y from (1+ y0) below y1 do (%put-cell x0 y #\│ attr) (%put-cell x1 y #\│ attr)))

(defun %text-at (x y string attr)
  (loop for i below (length string) do (%put-cell (+ x i) y (char string i) attr)))

;;; ---------------------------------------------------------------------------
;;; Focus + containers.  FOCUSABLE-P is a protocol GF (default NIL); a container
;;; routes key events to its focused child, handles Tab/Shift-Tab itself, and
;;; bubbles anything unhandled to its own keymap via CALL-NEXT-METHOD.
;;; ---------------------------------------------------------------------------

(defgeneric focusable-p (view) (:method ((v view)) nil))

(defclass container (view)
  ((subviews :initform '() :accessor subviews)
   (focus    :initform nil :accessor container-focus))   ; the focused leaf, anywhere below (root only)
  (:metaclass reactive-class))

(defun add-subview (c v)
  (setf (view-owner v) c
        (subviews c) (append (subviews c) (list v)))
  v)

(defun view-root (v) (if (view-owner v) (view-root (view-owner v)) v))

(defun find-view (root name)
  "Depth-first search for the subview named NAME, or NIL."
  (cond ((and name (eql (view-name root) name)) root)
        ((typep root 'container) (some (lambda (sv) (find-view sv name)) (subviews root)))
        (t nil)))

(defun view-focused-p (v)
  "True when V is the focused widget of its root window."
  (let ((r (view-root v))) (and (typep r 'container) (eq v (container-focus r)))))

;;; Focus is a *window-level* property over every focusable leaf in the subtree,
;;; so nested layout containers don't each need their own focus management.
(defun all-focusables (v)
  (cond ((focusable-p v) (list v))
        ((typep v 'container) (mapcan #'all-focusables (subviews v)))
        (t nil)))

(defun focus-next (root &optional (dir 1))
  (let ((fs (all-focusables root)))
    (when fs
      (let ((cur (or (position (container-focus root) fs) 0)))
        (setf (container-focus root) (nth (mod (+ cur dir) (length fs)) fs))))))

(defmethod draw ((c container))
  (dolist (sv (subviews c)) (draw sv)))

(defmethod handle-event ((c container) (e key-event))
  (cond
    ((eql (event-keysym e) :tab)       (focus-next c 1)  (setf (handled-p e) t))
    ((eql (event-keysym e) :shift-tab) (focus-next c -1) (setf (handled-p e) t))
    (t (let ((f (container-focus c))) (when f (handle-event f e)))   ; -> the focused leaf
       (unless (handled-p e) (call-next-method)))))                  ; -> container's keymap

;;; ---------------------------------------------------------------------------
;;; Mouse: events carry a screen point; dispatch hit-tests the view tree top-down
;;; to the deepest view under the pointer, and a click also focuses it.
;;; ---------------------------------------------------------------------------

(defun point-in-rect-p (x y r)
  (and r (<= (tvision::rect-ax r) x) (< x (tvision::rect-bx r))
       (<= (tvision::rect-ay r) y) (< y (tvision::rect-by r))))

(defun view-at (root x y)
  "The deepest view in ROOT's subtree whose bounds contain (X,Y), or NIL.
Children are tested front-to-back (last added paints on top)."
  (when (point-in-rect-p x y (view-bounds root))
    (or (and (typep root 'container)
             (loop for sv in (reverse (subviews root))
                   for hit = (view-at sv x y) when hit return hit))
        root)))

(defun mouse-col (view e) (- (car (event-where e)) (tvision::rect-ax (view-bounds view))))
(defun mouse-row (view e) (- (cdr (event-where e)) (tvision::rect-ay (view-bounds view))))

(defmethod handle-event ((v view) (e mouse-event)) nil)         ; default: ignore

(defmethod handle-event ((c container) (e mouse-event))
  (let* ((w (event-where e)) (hit (and w (view-at c (car w) (cdr w)))))
    (when (and hit (not (eq hit c)))
      (when (and (typep e 'mouse-down) (focusable-p hit))
        (setf (container-focus (view-root hit)) hit))             ; click focuses
      (handle-event hit e))))
