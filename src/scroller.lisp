;;;; scroller.lisp --- TScroller: a view onto a larger virtual area, bound to
;;;; one or two TScrollBars.
;;;;
;;;; A scroller keeps a DELTA (the top-left offset into a virtual region of size
;;;; LIMIT) and stays in sync with its horizontal/vertical scroll bars: moving a
;;;; bar scrolls the view (via the cmScrollBarChanged broadcast), and scrolling
;;;; the view updates the bars.  Subclasses override DRAW to render the slice of
;;;; content at DELTA.

(in-package #:tvision)

(defclass tscroller (tview)
  ((delta     :initform (make-tpoint) :accessor scroller-delta)
   (limit     :initform (make-tpoint) :accessor scroller-limit)
   (hscroll   :initform nil :accessor scroller-hscroll)
   (vscroll   :initform nil :accessor scroller-vscroll)
   (in-scroll :initform nil :accessor scroller-in-scroll)))  ; reentrancy guard

(defmethod initialize-instance :after ((sc tscroller) &key)
  (setf (view-options sc) (logior (view-options sc) +of-selectable+ +of-first-click+)
        (view-grow-mode sc) (logior +gf-grow-hix+ +gf-grow-hiy+)))

(defmethod get-palette ((sc tscroller)) (make-palette 6 6))

(defun max-delta-x (sc) (max 0 (- (point-x (scroller-limit sc)) (point-x (view-size sc)))))
(defun max-delta-y (sc) (max 0 (- (point-y (scroller-limit sc)) (point-y (view-size sc)))))

(defun attach-scrollbars (sc &key hscroll vscroll)
  "Bind scroll bars to SC and prime their ranges."
  (setf (scroller-hscroll sc) hscroll
        (scroller-vscroll sc) vscroll)
  (update-scrollbar-params sc)
  sc)

(defun update-scrollbar-params (sc)
  "Push SC's current delta/limit/page size into its scroll bars.  The reentrancy
guard keeps the resulting cmScrollBarChanged broadcasts from looping back in."
  (let ((saved (scroller-in-scroll sc)))
    (setf (scroller-in-scroll sc) t)
    (unwind-protect
         (let ((w (point-x (view-size sc))) (h (point-y (view-size sc))))
           (when (scroller-hscroll sc)
             (sb-set-params (scroller-hscroll sc) (point-x (scroller-delta sc))
                            0 (max-delta-x sc) (max 1 (1- w)) 1))
           (when (scroller-vscroll sc)
             (sb-set-params (scroller-vscroll sc) (point-y (scroller-delta sc))
                            0 (max-delta-y sc) (max 1 (1- h)) 1)))
      (setf (scroller-in-scroll sc) saved))))

(defgeneric scroll-draw (sc)
  (:documentation "Called when DELTA changes; default simply repaints.")
  (:method ((sc tscroller)) (draw-view sc)))

(defun scroll-to (sc x y)
  "Scroll so the virtual point (X,Y) is at the top-left, clamped to LIMIT."
  (unless (scroller-in-scroll sc)
    (let ((nx (min (max 0 x) (max-delta-x sc)))
          (ny (min (max 0 y) (max-delta-y sc))))
      (unless (and (= nx (point-x (scroller-delta sc)))
                   (= ny (point-y (scroller-delta sc))))
        (setf (point-x (scroller-delta sc)) nx
              (point-y (scroller-delta sc)) ny)
        (update-scrollbar-params sc)
        (scroll-draw sc)))))

(defun set-scroller-limit (sc x y)
  "Set the virtual content size to (X,Y), reclamp delta, and refresh the bars."
  (setf (point-x (scroller-limit sc)) (max 0 x)
        (point-y (scroller-limit sc)) (max 0 y)
        (point-x (scroller-delta sc)) (min (point-x (scroller-delta sc)) (max-delta-x sc))
        (point-y (scroller-delta sc)) (min (point-y (scroller-delta sc)) (max-delta-y sc)))
  (update-scrollbar-params sc))

(defun scroll-from-scrollbars (sc)
  (scroll-to sc
             (if (scroller-hscroll sc) (sb-value (scroller-hscroll sc)) (point-x (scroller-delta sc)))
             (if (scroller-vscroll sc) (sb-value (scroller-vscroll sc)) (point-y (scroller-delta sc)))))

(defun scrollbar-event-p (sc event)
  (and (= (event-type event) +ev-broadcast+)
       (= (event-command event) +cm-scrollbar-changed+)
       (or (eq (event-info event) (scroller-hscroll sc))
           (eq (event-info event) (scroller-vscroll sc)))))

(defmethod change-bounds ((sc tscroller) bounds)
  (set-bounds sc bounds)
  (update-scrollbar-params sc)
  (draw-view sc))

(defmethod handle-event ((sc tscroller) event)
  (cond
    ((scrollbar-event-p sc event)
     (scroll-from-scrollbars sc))
    ((and (= (event-type event) +ev-mouse-wheel+) (mouse-in-view-p sc event))
     (scroll-to sc (point-x (scroller-delta sc))
                (+ (point-y (scroller-delta sc)) (* 3 (event-wheel event))))
     (clear-event event))
    ((and (= (event-type event) +ev-key-down+) (logtest (view-state sc) +sf-focused+))
     (let ((k (event-key-code event))
           (dx (point-x (scroller-delta sc)))
           (dy (point-y (scroller-delta sc)))
           (ph (max 1 (1- (point-y (view-size sc)))))
           (handled t))
       (cond
         ((= k +kb-up+)    (scroll-to sc dx (1- dy)))
         ((= k +kb-down+)  (scroll-to sc dx (1+ dy)))
         ((= k +kb-left+)  (scroll-to sc (1- dx) dy))
         ((= k +kb-right+) (scroll-to sc (1+ dx) dy))
         ((= k +kb-pgup+)  (scroll-to sc dx (- dy ph)))
         ((= k +kb-pgdn+)  (scroll-to sc dx (+ dy ph)))
         ((= k +kb-home+)  (scroll-to sc 0 0))
         ((= k +kb-end+)   (scroll-to sc dx (max-delta-y sc)))
         (t (setf handled nil)))
       (when handled (clear-event event))))))
