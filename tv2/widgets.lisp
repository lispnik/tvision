;;;; widgets.lisp --- window (framed container), button, static-text, and a demo
;;;; that hosts the outline + buttons with Tab focus cycling and command actions.

(in-package #:tv2)

;;; --- window: a framed container with a title --------------------------------

(defclass window (container)
  ((title   :initarg :title :initform "" :accessor window-title)
   (managed :initform nil :accessor window-managed)    ; hosted in a desktop (show close/resize affordances)
   (active  :initform t   :accessor window-active)      ; topmost/focused window (brighter frame)
   (cleanup :initform nil :accessor window-cleanup)     ; thunk run when the desktop closes it
   (scroll-target :initform nil :accessor window-scroll-target)   ; scrollable view -> frame scrollbar
   (help    :initform :general :accessor window-help)   ; help topic for F1 / the Help menu
   (kind    :initform nil :accessor window-kind))       ; builder keyword, for desktop layout save/restore
  (:metaclass reactive-class))

(defmethod draw ((w window))
  (let* ((b (view-bounds w))
         (x0 (tvision::rect-ax b)) (y0 (tvision::rect-ay b))
         (x1 (1- (tvision::rect-bx b))) (y1 (1- (tvision::rect-by b)))
         (frame (if (window-active w) (role :frame) (role :frame-inactive))))
    (when (window-managed w) (%drop-shadow x0 y0 x1 y1))   ; TV-style drop shadow (under desktop windows)
    (loop for y from y0 to y1 do                       ; clear interior
      (loop for x from x0 to x1 do (%put-cell x y #\Space (role :normal))))
    (%box x0 y0 x1 y1 frame (window-active w))          ; double-line frame when active, single when not
    (%text-at (+ x0 (max 1 (floor (- (tvision::rect-width b) (length (window-title w))) 2)))
              y0 (window-title w) frame)
    (dolist (sv (subviews w)) (draw sv))               ; children paint over the interior
    (when (window-scroll-target w)                     ; scrollbars on the right + bottom frame edges
      (let ((tgt (window-scroll-target w)))
        (draw-vscroll x1 (1+ y0) (1- y1) (scroll-pos tgt) (scroll-max tgt))
        (when (plusp (scroll-hmax tgt))
          (draw-hscroll y1 (1+ x0) (1- x1) (scroll-hpos tgt) (scroll-hmax tgt)))))
    (when (window-managed w)                            ; desktop affordances: close box + resize grip
      (%text-at (+ x0 1) y0 "[✕]" frame)
      (%put-cell x1 y1 #\◢ frame))))

;;; --- button: focusable, fires a command on Enter/Space ----------------------

(defclass button (view)
  ((label   :initarg :label   :accessor button-label)
   (command :initarg :command :accessor button-command))
  (:metaclass reactive-class))

(defmethod focusable-p ((b button)) t)

(defmethod draw ((b button))
  (let* ((bb (view-bounds b))
         (attr (if (view-focused-p b) (role :button-focused) (role :button))))
    (fill-row b 0 0 (tvision::rect-width bb) attr)
    (draw-text b 0 0 (format nil "[ ~a ]" (button-label b)) attr)))

(defmethod handle-event ((b button) (e key-event))
  (if (member (event-keysym e) (list :enter #\Space) :test #'equal)
      (progn (perform (button-command b) b e) (setf (handled-p e) t))
      (call-next-method)))

;;; --- static-text: a non-focusable label -------------------------------------

(defclass static-text (view)
  ((text :initarg :text :initform "" :accessor static-text-text)
   (role :initarg :role :initform :normal :reader static-text-role))
  (:metaclass reactive-class))

(defmethod draw ((v static-text))
  ;; an empty :error line stays invisible (blends into the background) until set
  (let ((attr (if (and (zerop (length (static-text-text v))) (eq (static-text-role v) :error))
                  (role :normal)
                  (role (static-text-role v)))))
    (fill-row v 0 0 (tvision::rect-width (view-bounds v)) attr)
    (draw-text v 0 0 (static-text-text v) attr)))

;;; --- input-line: an editable single-line text field -------------------------
;;; Text/caret/scroll are reactive (edits repaint), and an ON-CHANGE closure
;;; (a first-class handler) fires whenever the text changes -- data binding
;;; without GetData/SetData.

;;; A field validator: FILTER (char -> keep?) rejects keystrokes as typed; CHECK
;;; (string -> (values ok-p message)) validates the whole field on accept.
(defstruct (field-validator (:constructor %fv (&key filter check))) filter check)

(defvar *input-histories* (make-hash-table) "HISTORY-ID -> list of past entries (most recent first).")

(defclass input-line (view)
  ((text       :initarg :text :initform "" :accessor input-text)
   (caret      :initform 0 :accessor input-caret)
   (scroll     :initform 0 :accessor input-scroll)         ; first visible column
   (on-change  :initarg :on-change :initform nil :accessor input-on-change)
   (validator  :initarg :validator  :initform nil :accessor input-validator)   ; field-validator or NIL
   (history-id :initarg :history-id :initform nil :accessor input-history-id)   ; key into *input-histories*
   (hist-pos   :initform nil :accessor input-hist-pos))
  (:metaclass reactive-class))

(defmethod focusable-p ((il input-line)) t)

(defun input-history (il) (and (input-history-id il) (gethash (input-history-id il) *input-histories*)))
(defun input-remember (il)
  "Push the current text onto this field's history (deduped)."
  (let ((id (input-history-id il)) (s (input-text il)))
    (when (and id (plusp (length s)))
      (setf (gethash id *input-histories*) (cons s (remove s (gethash id *input-histories*) :test #'string=))))))
(defun input-recall (il delta)
  "Replace the field with the previous/next history entry."
  (let* ((h (input-history il)) (n (length h)))
    (when (plusp n)
      (let ((pos (cond ((null (input-hist-pos il)) (if (plusp delta) 0 (1- n)))
                       (t (max -1 (min n (+ (input-hist-pos il) delta)))))))
        (cond ((or (< pos 0) (>= pos n)) (setf (input-hist-pos il) nil (input-text il) ""))
              (t (setf (input-hist-pos il) pos (input-text il) (nth pos h))))
        (setf (input-caret il) (length (input-text il)))
        (input-scroll-fix il) (input-notify il)))))

(defun input-scroll-fix (il)
  (let ((b (view-bounds il)))
    (when b
      (let ((w (tvision::rect-width b)) (c (input-caret il)) (sc (input-scroll il)))
        (cond ((< c sc) (setf (input-scroll il) c))
              ((>= c (+ sc w)) (setf (input-scroll il) (1+ (- c w)))))))))

(defun input-notify (il)
  (when (input-on-change il) (funcall (input-on-change il) il)))

(defun input-insert (il ch)
  (let ((v (input-validator il)))
    (when (or (null v) (null (field-validator-filter v)) (funcall (field-validator-filter v) ch))  ; reject filtered keys
      (let ((txt (input-text il)) (c (input-caret il)))
        (setf (input-text il)  (concatenate 'string (subseq txt 0 c) (string ch) (subseq txt c))
              (input-caret il) (1+ c))
        (input-scroll-fix il) (input-notify il)))))

(defun input-backspace (il)
  (let ((txt (input-text il)) (c (input-caret il)))
    (when (plusp c)
      (setf (input-text il)  (concatenate 'string (subseq txt 0 (1- c)) (subseq txt c))
            (input-caret il) (1- c))
      (input-scroll-fix il) (input-notify il))))

(defun input-delete (il)
  (let ((txt (input-text il)) (c (input-caret il)))
    (when (< c (length txt))
      (setf (input-text il) (concatenate 'string (subseq txt 0 c) (subseq txt (1+ c))))
      (input-notify il))))

(defun input-move (il delta)
  (setf (input-caret il) (min (length (input-text il)) (max 0 (+ (input-caret il) delta))))
  (input-scroll-fix il))

(defmethod draw ((il input-line))
  (let* ((b (view-bounds il)) (w (tvision::rect-width b))
         (focused (view-focused-p il))
         (attr (if focused (role :input-focused) (role :input)))
         (txt (input-text il)) (sc (input-scroll il))
         (vis (subseq txt (min sc (length txt)) (min (length txt) (+ sc w)))))
    (fill-row il 0 0 w attr)
    (draw-text il 0 0 vis attr)
    (when (and focused tvision:*screen*)              ; own the hardware cursor while focused
      (tvision:set-cursor-pos tvision:*screen*
                              (+ (tvision::rect-ax b) (- (input-caret il) sc))
                              (tvision::rect-ay b))
      (tvision:show-cursor tvision:*screen*))))

(defmethod handle-event ((il input-line) (e key-event))
  (let ((ks (event-keysym e)))
    (cond
      ((and (characterp ks) (graphic-char-p ks) (zerop (event-modifiers e)))
       (input-insert il ks) (setf (handled-p e) t))
      ((eql ks :back)  (input-backspace il) (setf (handled-p e) t))
      ((eql ks :del)   (input-delete il)    (setf (handled-p e) t))
      ((eql ks :left)  (input-move il -1)   (setf (handled-p e) t))
      ((eql ks :right) (input-move il 1)    (setf (handled-p e) t))
      ((and (eql ks :up)   (input-history il)) (input-recall il 1)  (setf (handled-p e) t))   ; older
      ((and (eql ks :down) (input-history il)) (input-recall il -1) (setf (handled-p e) t))   ; newer
      ((eql ks :home)  (setf (input-caret il) 0) (input-scroll-fix il) (setf (handled-p e) t))
      ((eql ks :end)   (setf (input-caret il) (length (input-text il))) (input-scroll-fix il)
                       (setf (handled-p e) t))
      (t (call-next-method)))))               ; Enter/Tab/Esc bubble (submit, focus, quit)

;;; --- list-box: a scrollable, selectable flat list --------------------------
;;; Like INPUT-LINE it dispatches keys directly (no keymap); SELECTED/TOP are
;;; reactive, and Enter calls an ON-ACTIVATE closure with the chosen item.

(defclass list-box (view)
  ((items       :initarg :items :initform '() :accessor list-items)
   (selected    :initform 0 :accessor list-selected)
   (top         :initform 0 :accessor list-top)            ; first visible row
   (on-activate :initarg :on-activate :initform nil :accessor list-on-activate)
   (on-select   :initarg :on-select   :initform nil :accessor list-on-select))   ; fired when selection moves
  (:metaclass reactive-class))

(defmethod focusable-p ((lb list-box)) t)

(defun list-scroll-fix (lb)
  (let ((b (view-bounds lb)))
    (when b
      (let ((h (r-h b)) (sel (list-selected lb)) (top (list-top lb)))
        (cond ((< sel top) (setf (list-top lb) sel))
              ((>= sel (+ top h)) (setf (list-top lb) (1+ (- sel h)))))))))

(defun list-notify (lb) (when (list-on-select lb) (funcall (list-on-select lb) lb)))

(defun list-move (lb delta)
  (let ((n (length (list-items lb))))
    (when (plusp n)
      (setf (list-selected lb) (min (1- n) (max 0 (+ (list-selected lb) delta))))
      (list-scroll-fix lb) (list-notify lb))))

(defmethod draw ((lb list-box))
  (let* ((b (view-bounds lb)) (h (tvision::rect-height b)) (w (tvision::rect-width b))
         (active (view-focused-p lb)) (items (list-items lb)) (top (list-top lb)))
    (dotimes (row h)
      (let* ((i (+ top row))
             (sel (and (= i (list-selected lb)) active))
             (attr (if sel (role :focused) (role :normal))))
        (fill-row lb 0 row w attr)
        (when (< i (length items))
          (draw-text lb 1 row (nth i items) attr))))))

(defmethod handle-event ((lb list-box) (e key-event))
  (let ((ks (event-keysym e)) (n (length (list-items lb))))
    (cond
      ((eql ks :up)    (list-move lb -1) (setf (handled-p e) t))
      ((eql ks :down)  (list-move lb 1)  (setf (handled-p e) t))
      ((eql ks :home)  (setf (list-selected lb) 0) (list-scroll-fix lb) (list-notify lb) (setf (handled-p e) t))
      ((eql ks :end)   (setf (list-selected lb) (max 0 (1- n))) (list-scroll-fix lb) (list-notify lb) (setf (handled-p e) t))
      ((eql ks :enter) (when (and (list-on-activate lb) (< (list-selected lb) n))
                         (funcall (list-on-activate lb) lb (nth (list-selected lb) (list-items lb))))
                       (setf (handled-p e) t))
      (t (call-next-method)))))

;;; --- mouse: click to focus/select/press, wheel to scroll -------------------

(defmethod handle-event ((b button) (e mouse-down))
  (perform (button-command b) b e) (setf (handled-p e) t))

(defmethod handle-event ((il input-line) (e mouse-down))
  (setf (input-caret il) (max 0 (min (length (input-text il))
                                     (+ (input-scroll il) (mouse-col il e)))))
  (input-scroll-fix il) (setf (handled-p e) t))

(defmethod handle-event ((lb list-box) (e mouse-down))
  (let ((row (+ (list-top lb) (mouse-row lb e))))
    (when (and (>= row 0) (< row (length (list-items lb))))
      (setf (list-selected lb) row) (list-scroll-fix lb) (list-notify lb)))
  (setf (handled-p e) t))

(defmethod handle-event ((lb list-box) (e wheel-event))
  (list-move lb (* 3 (event-delta e))) (setf (handled-p e) t))

;;; --- a command that reaches across the window to the outline ----------------

(define-command collapse-all (v e)
  (let ((ol (find-view (view-root v) 'tree)))     ; locate the named outline anywhere in the tree
    (when (typep ol 'outline)
      (dolist (root (outline-roots ol))           ; collapse everything below each root
        (labels ((collapse (n)
                   (mapc #'collapse (tvision:outline-node-children n))
                   (setf (tvision:outline-node-expanded n) nil)))
          (mapc #'collapse (tvision:outline-node-children root))))
      (setf (outline-focused ol) 0)
      (invalidate ol))))
