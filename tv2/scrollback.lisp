;;;; scrollback.lisp --- a scrolling transcript view (append-only text log).
;;;;
;;;; The reusable widget the REPL (and any log/output pane) needs: a growing
;;;; buffer of text lines that auto-follows the tail as output arrives, but lets
;;;; the user scroll back through history.  Output streams in as arbitrary chunks
;;;; (not whole lines), so a trailing partial line is held in PENDING until its
;;;; newline shows up.  Appending mutates the line vector in place, so APPEND/
;;;; CLEAR/SCROLL call INVALIDATE explicitly to trigger a repaint.

(in-package #:tv2)

(defclass scrollback (view)
  ((lines   :initform (make-array 0 :adjustable t :fill-pointer 0) :accessor sb-lines)
   (pending :initform "" :accessor sb-pending)        ; incomplete trailing line (no newline yet)
   (top     :initform 0 :accessor sb-top)             ; first visible row
   (follow  :initform t :accessor sb-follow)          ; stick to the tail as new text arrives
   ;; SLY-style presentations: line-index -> live object, so a printed result can
   ;; be clicked to inspect the actual object (the REPL uses this).
   (presentations :initform (make-hash-table) :accessor sb-presentations)
   (on-present :initarg :on-present :initform nil :accessor sb-on-present))  ; (object) -> act on a clicked presentation
  (:metaclass reactive-class))

(defmethod focusable-p ((sb scrollback)) t)

(defun sb-total (sb)
  (+ (length (sb-lines sb)) (if (plusp (length (sb-pending sb))) 1 0)))

(defun sb-row (sb i)
  (let ((n (length (sb-lines sb))))
    (cond ((< i n) (aref (sb-lines sb) i))
          ((= i n) (sb-pending sb))
          (t ""))))

(defun sb-scroll-end (sb)
  "Pin the viewport to the last page (so the newest line is on screen)."
  (let ((b (view-bounds sb)))
    (when b (setf (sb-top sb) (max 0 (- (sb-total sb) (r-h b)))))))

(defun scrollback-append (sb text)
  "Append TEXT (which may contain newlines and need not end in one) to the
transcript, holding any trailing partial line in PENDING for the next chunk."
  (let ((s (concatenate 'string (sb-pending sb) text)) (start 0))
    (loop for nl = (position #\Newline s :start start)
          while nl
          do (vector-push-extend (subseq s start nl) (sb-lines sb))
             (setf start (1+ nl)))
    (setf (sb-pending sb) (subseq s start)))
  (when (sb-follow sb) (sb-scroll-end sb))
  (invalidate sb))

(defun scrollback-present (sb text object)
  "Append TEXT (a full result line, ending in newline) and mark the line(s) it
occupies as a presentation of the live OBJECT, so clicking them fires ON-PRESENT."
  (let ((first (length (sb-lines sb))))
    (scrollback-append sb text)
    (loop for i from first below (length (sb-lines sb))
          do (setf (gethash i (sb-presentations sb)) object))))

(defun scrollback-clear (sb)
  (setf (fill-pointer (sb-lines sb)) 0
        (sb-pending sb) "" (sb-top sb) 0 (sb-follow sb) t)
  (clrhash (sb-presentations sb))
  (invalidate sb))

(defun sb-scroll (sb delta)
  (let* ((b (view-bounds sb)) (maxtop (max 0 (- (sb-total sb) (r-h b)))))
    (setf (sb-top sb)    (max 0 (min maxtop (+ (sb-top sb) delta)))
          (sb-follow sb) (>= (sb-top sb) maxtop))     ; re-arm follow once back at the bottom
    (invalidate sb)))

(defmethod draw ((sb scrollback))
  (let* ((b (view-bounds sb)) (h (r-h b)) (w (r-w b))
         (attr (role :normal)) (pres (role :label)) (top (sb-top sb)) (total (sb-total sb)))
    (dotimes (row h)
      (let* ((i (+ top row))
             (a (if (nth-value 1 (gethash i (sb-presentations sb))) pres attr)))  ; presentation lines stand out
        (fill-row sb 0 row w a)
        (when (< i total) (draw-text sb 0 row (sb-row sb i) a))))))

(defmethod handle-event ((sb scrollback) (e mouse-down))
  (let ((i (+ (sb-top sb) (mouse-row sb e))))          ; click a presented result -> act on the live object
    (multiple-value-bind (obj present) (gethash i (sb-presentations sb))
      (when (and present (sb-on-present sb)) (funcall (sb-on-present sb) obj))))
  (setf (handled-p e) t))

(defmethod handle-event ((sb scrollback) (e wheel-event))
  (sb-scroll sb (* 3 (event-delta e))) (setf (handled-p e) t))

(defmethod handle-event ((sb scrollback) (e key-event))
  (let* ((ks (event-keysym e)) (page (max 1 (1- (r-h (view-bounds sb))))))
    (cond
      ((eql ks :up)   (sb-scroll sb -1)      (setf (handled-p e) t))
      ((eql ks :down) (sb-scroll sb 1)       (setf (handled-p e) t))
      ((eql ks :pgup) (sb-scroll sb (- page))(setf (handled-p e) t))
      ((eql ks :pgdn) (sb-scroll sb page)    (setf (handled-p e) t))
      ((eql ks :home) (setf (sb-top sb) 0 (sb-follow sb) nil) (invalidate sb) (setf (handled-p e) t))
      ((eql ks :end)  (setf (sb-follow sb) t) (sb-scroll-end sb) (invalidate sb) (setf (handled-p e) t))
      (t (call-next-method)))))                       ; q / Esc bubble to the window
