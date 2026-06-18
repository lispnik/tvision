;;;; geometry.lisp --- TPoint and TRect, the geometric primitives of Turbo Vision.

(in-package #:tvision)

;;; ---------------------------------------------------------------------------
;;; TPoint
;;; ---------------------------------------------------------------------------

(defstruct (tpoint (:constructor make-tpoint (&optional (x 0) (y 0)))
                   (:conc-name point-))
  (x 0 :type fixnum)
  (y 0 :type fixnum))

(declaim (inline point-equal-p copy-point))
(defun point-equal-p (a b)
  (and (= (point-x a) (point-x b))
       (= (point-y a) (point-y b))))

(defun copy-point (p)
  (make-tpoint (point-x p) (point-y p)))

;;; ---------------------------------------------------------------------------
;;; TRect
;;;
;;; A rectangle is defined by two corner points: A (top-left, inclusive) and
;;; B (bottom-right, exclusive), exactly as in Turbo Vision.
;;; ---------------------------------------------------------------------------

(defstruct (trect (:constructor %make-trect)
                  (:conc-name rect-))
  (ax 0 :type fixnum)
  (ay 0 :type fixnum)
  (bx 0 :type fixnum)
  (by 0 :type fixnum))

(defun make-trect (ax ay bx by)
  (%make-trect :ax ax :ay ay :bx bx :by by))

(declaim (inline rect-width rect-height))
(defun rect-width (r) (- (rect-bx r) (rect-ax r)))
(defun rect-height (r) (- (rect-by r) (rect-ay r)))

(defun rect-empty-p (r)
  (or (>= (rect-ax r) (rect-bx r))
      (>= (rect-ay r) (rect-by r))))

(defun rect-equal-p (a b)
  (and (= (rect-ax a) (rect-ax b)) (= (rect-ay a) (rect-ay b))
       (= (rect-bx a) (rect-bx b)) (= (rect-by a) (rect-by b))))

(defun copy-rect (r)
  (make-trect (rect-ax r) (rect-ay r) (rect-bx r) (rect-by r)))

(defun rect-assign (r ax ay bx by)
  (setf (rect-ax r) ax (rect-ay r) ay (rect-bx r) bx (rect-by r) by)
  r)

(defun rect-contains-p (r x y)
  "True when point (X,Y) lies within rectangle R (A inclusive, B exclusive)."
  (and (>= x (rect-ax r)) (< x (rect-bx r))
       (>= y (rect-ay r)) (< y (rect-by r))))

(defun rect-move (r dx dy)
  (incf (rect-ax r) dx) (incf (rect-bx r) dx)
  (incf (rect-ay r) dy) (incf (rect-by r) dy)
  r)

(defun rect-grow (r dx dy)
  (decf (rect-ax r) dx) (incf (rect-bx r) dx)
  (decf (rect-ay r) dy) (incf (rect-by r) dy)
  r)

(defun rect-intersect (r o)
  "Destructively set R to the intersection of R and O."
  (setf (rect-ax r) (max (rect-ax r) (rect-ax o))
        (rect-ay r) (max (rect-ay r) (rect-ay o))
        (rect-bx r) (min (rect-bx r) (rect-bx o))
        (rect-by r) (min (rect-by r) (rect-by o)))
  ;; normalise empty rectangles
  (when (< (rect-bx r) (rect-ax r)) (setf (rect-bx r) (rect-ax r)))
  (when (< (rect-by r) (rect-ay r)) (setf (rect-by r) (rect-ay r)))
  r)

(defun rect-union (r o)
  "Destructively set R to the bounding union of R and O."
  (setf (rect-ax r) (min (rect-ax r) (rect-ax o))
        (rect-ay r) (min (rect-ay r) (rect-ay o))
        (rect-bx r) (max (rect-bx r) (rect-bx o))
        (rect-by r) (max (rect-by r) (rect-by o)))
  r)
