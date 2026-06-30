;;;; scrollbar.lisp --- the scroller protocol for the scrollable widgets, plus
;;;; the geometry the desktop uses to hit-test a window's frame scrollbar.
;;;;
;;;; A window draws a vertical scrollbar (kernel DRAW-VSCROLL) on its right frame
;;;; edge, bound to its WINDOW-SCROLL-TARGET; each scrollable widget answers
;;;; SCROLL-POS / SCROLL-MAX / SCROLL-PAGE / SCROLL-TO in terms of its own state.

(in-package #:tv2)

(defun %page-h (v) (if (view-bounds v) (r-h (view-bounds v)) 1))

(defmethod scroll-page ((v list-box))  (%page-h v))
(defmethod scroll-pos  ((v list-box))  (list-top v))
(defmethod scroll-max  ((v list-box))  (max 0 (- (length (list-items v)) (%page-h v))))
(defmethod scroll-to   ((v list-box) pos) (setf (list-top v) (max 0 (min pos (scroll-max v)))) (invalidate v))

(defmethod scroll-page ((v outline))  (%page-h v))
(defmethod scroll-pos  ((v outline))  (outline-top v))
(defmethod scroll-max  ((v outline))  (max 0 (- (length (ov-visible (outline-roots v))) (%page-h v))))
(defmethod scroll-to   ((v outline) pos) (setf (outline-top v) (max 0 (min pos (scroll-max v)))) (invalidate v))

(defmethod scroll-page ((v scrollback))  (%page-h v))
(defmethod scroll-pos  ((v scrollback))  (sb-top v))
(defmethod scroll-max  ((v scrollback))  (max 0 (- (sb-total v) (%page-h v))))
(defmethod scroll-to   ((v scrollback) pos)
  (setf (sb-top v) (max 0 (min pos (scroll-max v))) (sb-follow v) nil) (invalidate v))

(defmethod scroll-page ((v text-edit))  (%page-h v))
(defmethod scroll-pos  ((v text-edit))  (te-top v))
(defmethod scroll-max  ((v text-edit))  (max 0 (- (te-nlines v) (%page-h v))))
(defmethod scroll-to   ((v text-edit) pos) (setf (te-top v) (max 0 (min pos (scroll-max v)))) (invalidate v))

(defmethod scroll-page ((v html-view))  (%page-h v))
(defmethod scroll-pos  ((v html-view))  (hv-top v))
(defmethod scroll-max  ((v html-view))  (max 0 (- (hv-nlines v) (%page-h v))))
(defmethod scroll-to   ((v html-view) pos) (setf (hv-top v) (max 0 (min pos (scroll-max v)))) (invalidate v))

;;; --- frame-scrollbar geometry + click mapping (used by the desktop) ---------

(defun window-vscroll-bounds (win)
  "(values COL TOP-ARROW-ROW BOTTOM-ARROW-ROW) for WIN's frame scrollbar, or NIL."
  (when (window-scroll-target win)
    (let ((b (view-bounds win)))
      (values (1- (tvision::rect-bx b)) (1+ (tvision::rect-ay b)) (- (tvision::rect-by b) 2)))))

(defun %scroll-from-click (tgt sy y0 y1)
  "Scroll TGT from a click/drag at screen row SY on a scrollbar with arrows at
rows Y0 (▲) and Y1 (▼)."
  (cond ((<= sy y0) (scroll-to tgt (1- (scroll-pos tgt))))
        ((>= sy y1) (scroll-to tgt (1+ (scroll-pos tgt))))
        (t (let ((track (max 1 (- y1 y0 1))))
             (scroll-to tgt (floor (* (- sy y0 1) (scroll-max tgt)) track))))))
