;;;; view.lisp --- TView, the abstract base of every visible object.

(in-package #:tvision)

;;; ---------------------------------------------------------------------------
;;; State / option / grow-mode flag bits
;;; ---------------------------------------------------------------------------

(defconstant +sf-visible+    #x0001)
(defconstant +sf-cursor-vis+ #x0002)
(defconstant +sf-cursor-ins+ #x0004)
(defconstant +sf-shadow+     #x0008)
(defconstant +sf-active+     #x0010)
(defconstant +sf-selected+   #x0020)
(defconstant +sf-focused+    #x0040)
(defconstant +sf-dragging+   #x0080)
(defconstant +sf-disabled+   #x0100)
(defconstant +sf-modal+      #x0200)
(defconstant +sf-exposed+    #x0800)

(defconstant +of-selectable+   #x0001)
(defconstant +of-top-select+   #x0002)
(defconstant +of-first-click+  #x0004)
(defconstant +of-framed+       #x0008)
(defconstant +of-pre-process+  #x0010)
(defconstant +of-post-process+ #x0020)
(defconstant +of-centerx+      #x0100)
(defconstant +of-centery+      #x0200)
(defconstant +of-center+       #x0300)

(defconstant +gf-grow-lox+ #x01)
(defconstant +gf-grow-loy+ #x02)
(defconstant +gf-grow-hix+ #x04)
(defconstant +gf-grow-hiy+ #x08)
(defconstant +gf-grow-all+ #x0f)
(defconstant +gf-grow-rel+ #x10)

(defconstant +hc-no-context+ 0)

;;; ---------------------------------------------------------------------------
;;; TView
;;; ---------------------------------------------------------------------------

(defclass tview ()
  ((origin    :initform (make-tpoint) :accessor view-origin)
   (size      :initform (make-tpoint) :accessor view-size)
   (cursor    :initform (make-tpoint) :accessor view-cursor)
   (state     :initform +sf-visible+  :accessor view-state)
   (options   :initform 0             :accessor view-options)
   (grow-mode :initform 0             :accessor view-grow-mode)
   (drag-mode :initform 0             :accessor view-drag-mode)
   (help-ctx  :initform +hc-no-context+ :accessor view-help-ctx)
   ;; which event classes this view's HANDLE-EVENT is willing to receive
   (event-mask :initform (logior +ev-mouse+ +ev-keyboard+ +ev-message+)
               :accessor view-event-mask)
   (owner     :initform nil           :accessor view-owner)
   (next      :initform nil           :accessor view-next)))

(defmethod initialize-instance :after ((v tview) &key bounds)
  (when bounds (set-bounds v bounds)))

(defun owner-group (v) (view-owner v))

;;; --- geometry --------------------------------------------------------------

(defun get-extent (v &optional (r (make-trect 0 0 0 0)))
  (rect-assign r 0 0 (point-x (view-size v)) (point-y (view-size v))))

(defun get-bounds (v &optional (r (make-trect 0 0 0 0)))
  (let ((o (view-origin v)) (s (view-size v)))
    (rect-assign r (point-x o) (point-y o)
                 (+ (point-x o) (point-x s)) (+ (point-y o) (point-y s)))))

(defun get-rect (v) (get-bounds v))

(defgeneric size-limits (v)
  (:documentation "Return (values min-point max-point) of permitted sizes.")
  (:method ((v tview))
    (values (make-tpoint 0 0)
            (make-tpoint (if (view-owner v) (point-x (view-size (view-owner v))) 999)
                         (if (view-owner v) (point-y (view-size (view-owner v))) 999)))))

(defun set-bounds (v bounds)
  "Assign V's origin and size from rectangle BOUNDS (no redraw)."
  (setf (point-x (view-origin v)) (rect-ax bounds)
        (point-y (view-origin v)) (rect-ay bounds)
        (point-x (view-size v)) (max 0 (rect-width bounds))
        (point-y (view-size v)) (max 0 (rect-height bounds)))
  v)

(defgeneric change-bounds (v bounds)
  (:method ((v tview) bounds)
    (set-bounds v bounds)
    (draw-view v)))

(defun locate (v bounds)
  "Resize/move V to BOUNDS, clamping to its size limits, and redraw."
  (multiple-value-bind (mn mx) (size-limits v)
    (let* ((w (min (max (rect-width bounds) (point-x mn)) (point-x mx)))
           (h (min (max (rect-height bounds) (point-y mn)) (point-y mx)))
           (r (make-trect (rect-ax bounds) (rect-ay bounds)
                          (+ (rect-ax bounds) w) (+ (rect-ay bounds) h))))
      (unless (rect-equal-p r (get-bounds v))
        (change-bounds v r)))))

(defun move-to (v x y)
  (let ((s (view-size v)))
    (change-bounds v (make-trect x y (+ x (point-x s)) (+ y (point-y s))))))

(defun grow-to (v w h)
  (let ((o (view-origin v)))
    (change-bounds v (make-trect (point-x o) (point-y o)
                                 (+ (point-x o) w) (+ (point-y o) h)))))

;;; --- owner-chain coordinate transforms -------------------------------------

(defun view-global-origin (v)
  "Return (values gx gy): V's origin expressed in absolute screen coordinates."
  (let ((gx 0) (gy 0) (cur v))
    (loop while cur do
      (incf gx (point-x (view-origin cur)))
      (incf gy (point-y (view-origin cur)))
      (setf cur (view-owner cur)))
    (values gx gy)))

(defun make-global (v p &optional (dest (make-tpoint)))
  "Convert local point P to global coordinates."
  (multiple-value-bind (gx gy) (view-global-origin v)
    (setf (point-x dest) (+ gx (point-x p))
          (point-y dest) (+ gy (point-y p)))
    dest))

(defun make-local (v p &optional (dest (make-tpoint)))
  "Convert global point P to V-local coordinates."
  (multiple-value-bind (gx gy) (view-global-origin v)
    (setf (point-x dest) (- (point-x p) gx)
          (point-y dest) (- (point-y p) gy))
    dest))

(defun view-global-clip (v)
  "Return the absolute-screen rectangle within which V is allowed to draw:
the intersection of V's bounds with every ancestor's bounds."
  (multiple-value-bind (gx gy) (view-global-origin v)
    (let ((clip (make-trect gx gy
                            (+ gx (point-x (view-size v)))
                            (+ gy (point-y (view-size v)))))
          (owner (view-owner v)))
      (loop while owner do
        (multiple-value-bind (ox oy) (view-global-origin owner)
          (rect-intersect clip (make-trect ox oy
                                           (+ ox (point-x (view-size owner)))
                                           (+ oy (point-y (view-size owner))))))
        (setf owner (view-owner owner)))
      clip)))

;;; --- visibility ------------------------------------------------------------

(defun get-state (v mask) (logtest (view-state v) mask))

(defgeneric set-state (v state enable)
  (:documentation "Set or clear STATE bit(s) on V and react accordingly.")
  (:method ((v tview) state enable)
    (if enable
        (setf (view-state v) (logior (view-state v) state))
        (setf (view-state v) (logandc2 (view-state v) state)))
    (when (logtest state (logior +sf-visible+ +sf-cursor-vis+ +sf-cursor-ins+ +sf-focused+))
      (when (and (logtest state +sf-focused+))
        (reset-cursor v)))))

(defun visible-p (v) (logtest (view-state v) +sf-visible+))

(defun view-disabled-p (v) (logtest (view-state v) +sf-disabled+))

(defun disable-view (v) (set-state v +sf-disabled+ t))
(defun enable-view (v)  (set-state v +sf-disabled+ nil))

(defun wants-event-p (v event)
  "True when V's event mask accepts EVENT's class (TView::eventMask)."
  (logtest (view-event-mask v) (event-type event)))

(defun exposed-p (v)
  "True when V and all of its owners are visible and V has a non-empty clip.
(Sibling occlusion is handled implicitly by back-to-front draw order.)"
  (and (logtest (view-state v) +sf-visible+)
       (let ((o (view-owner v)))
         (or (null o) (exposed-p o)))
       (not (rect-empty-p (view-global-clip v)))))

(defgeneric show (v)
  (:method ((v tview)) (unless (visible-p v) (set-state v +sf-visible+ t))))

(defgeneric hide (v)
  (:method ((v tview)) (when (visible-p v) (set-state v +sf-visible+ nil))))

;;; --- colour ----------------------------------------------------------------

(defgeneric get-palette (v)
  (:documentation "Return this view's palette vector, or NIL.")
  (:method ((v tview)) nil))

(defun default-palette (v) (get-palette v))

(defun get-color (v index)
  "Map colour INDEX through V's palette chain to a concrete attribute byte."
  (let* ((pal (get-palette v))
         (idx (if pal (let ((c (palette-ref pal index))) (if (zerop c) index c)) index)))
    (if (view-owner v)
        (get-color (view-owner v) idx)
        (if (zerop idx) #x07 idx))))

;;; --- writing to the screen -------------------------------------------------

(defun %write-cells (v x y w h fetch)
  "Core blitter.  For each cell in the WxH block at local (X,Y), call
FETCH(col row) -> packed cell and store it on screen, clipped to V's region."
  (when *screen*
    (multiple-value-bind (gx gy) (view-global-origin v)
      (let ((clip (view-global-clip v))
            (s *screen*))
        (dotimes (row h)
          (let ((sy (+ gy y row)))
            (when (and (>= sy (rect-ay clip)) (< sy (rect-by clip)))
              (dotimes (col w)
                (let ((sx (+ gx x col)))
                  (when (and (>= sx (rect-ax clip)) (< sx (rect-bx clip)))
                    (screen-cell-set s sx sy (funcall fetch col row))))))))))))

(defun write-line* (v x y w h db)
  "Write the single buffer row DB (a draw-buffer) to the WxH block at (X,Y),
repeating it on each of the H lines.  This is the workhorse used by `draw'."
  (let ((data (draw-buffer-data db)))
    (%write-cells v x y w h
                  (lambda (col row) (declare (ignore row))
                    (if (< col (length data)) (aref data col) (cell-make-code 32 #x07))))))

(defun write-buf (v x y w h cells)
  "Write a flat W*H array of packed CELLS to the block at (X,Y)."
  (%write-cells v x y w h
                (lambda (col row)
                  (let ((i (+ col (* row w))))
                    (if (< i (length cells)) (aref cells i) (cell-make-code 32 #x07))))))

(defun write-char* (v x y char color count)
  "Write CHAR COUNT times at (X,Y) using mapped colour index COLOR."
  (let ((cell (cell-make-code (char-code char) (get-color v color))))
    (%write-cells v x y count 1 (lambda (col row) (declare (ignore col row)) cell))))

(defun write-str (v x y string color)
  "Write STRING at (X,Y) using mapped colour index COLOR."
  (let* ((attr (get-color v color))
         (vec (map '(simple-array (unsigned-byte 53) (*))
                   (lambda (ch) (cell-make-code (char-code ch) attr))
                   string)))
    (%write-cells v x y (length string) 1
                  (lambda (col row) (declare (ignore row)) (aref vec col)))))

;;; --- cursor ----------------------------------------------------------------

(defun set-cursor (v x y)
  (setf (point-x (view-cursor v)) x (point-y (view-cursor v)) y)
  (reset-cursor v))

(defun show-cursor* (v) (set-state v +sf-cursor-vis+ t))
(defun hide-cursor* (v) (set-state v +sf-cursor-vis+ nil))
(defun normal-cursor (v) (set-state v +sf-cursor-ins+ nil))
(defun block-cursor (v) (set-state v +sf-cursor-ins+ t))

(defun reset-cursor (v)
  "If V owns the focus and wants a visible cursor, position the hardware cursor."
  (when (and *screen*
             (logtest (view-state v) +sf-focused+)
             (logtest (view-state v) +sf-cursor-vis+)
             (exposed-p v))
    (multiple-value-bind (gx gy) (view-global-origin v)
      (set-cursor-pos *screen* (+ gx (point-x (view-cursor v)))
                               (+ gy (point-y (view-cursor v))))
      (set-cursor-shape (if (logtest (view-state v) +sf-cursor-ins+) :block :underline) *screen*)
      (show-cursor *screen*))))

;;; --- drawing ---------------------------------------------------------------

(defgeneric draw (v)
  (:documentation "Render V into the screen back buffer.")
  (:method ((v tview))
    ;; default: clear our area to the normal colour
    (let ((db (make-draw-buffer (point-x (view-size v)))))
      (db-fill db #\Space (get-color v 1))
      (write-line* v 0 0 (point-x (view-size v)) (point-y (view-size v)) db))))

(defgeneric draw-view (v)
  (:method ((v tview))
    (when (exposed-p v)
      (draw v)
      (reset-cursor v))))

;;; --- events ----------------------------------------------------------------

(defgeneric handle-event (v event)
  (:documentation "Process EVENT; call CLEAR-EVENT to mark it handled.")
  (:method ((v tview) event) (declare (ignore event)) nil))

(defgeneric put-event (v event)
  (:documentation "Queue EVENT for delivery on the next loop iteration.")
  (:method ((v tview) event)
    (when (view-owner v) (put-event (view-owner v) event))))

(defun mouse-in-view-p (v event)
  "True when EVENT is a mouse event located inside V."
  (and (mouse-event-p event)
       (let ((clip (view-global-clip v))
             (p (event-mouse-where event)))
         (rect-contains-p clip (point-x p) (point-y p)))))

(defun message (receiver what command info)
  "Send a command/broadcast message to RECEIVER and return the handler result."
  (when receiver
    (let ((e (make-event :type what :command command :info info)))
      (handle-event receiver e)
      (when (= (event-type e) +ev-nothing+) (event-info e)))))

;;; --- selection / focus -----------------------------------------------------

(defgeneric select (v)
  (:method ((v tview))
    (when (logtest (view-options v) +of-selectable+)
      (when (view-owner v) (set-current (view-owner v) v :normal-select)))))

(defun focus (v)
  "Move the focus to V, focusing its owner chain first."
  (let ((owner (view-owner v)))
    (when owner
      (focus owner)
      (set-current owner v :enter-select))
    t))

;;; --- data / validation ------------------------------------------------------

(defgeneric valid-p (v command)
  (:method ((v tview) command) (declare (ignore command)) t))

(defgeneric data-size (v) (:method ((v tview)) 0))
(defgeneric get-data (v) (:method ((v tview)) nil))
(defgeneric set-data (v data) (:method ((v tview) data) (declare (ignore data)) nil))

(defgeneric end-modal (v command)
  (:method ((v tview) command)
    (when (view-owner v) (end-modal (view-owner v) command))))

(defun event-error (v event)
  (declare (ignore v event)) nil)
