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
   (on-present :initarg :on-present :initform nil :accessor sb-on-present)   ; (object) -> act on a clicked presentation
   ;; optional inline input line (SLIME/SLY-style: the prompt floats after the
   ;; output rather than a fixed input row).  When IACTIVE, the last line is a
   ;; live "IPROMPT INPUT"; new output appends before it, so it drifts down.
   (iactive :initform nil :accessor sb-iactive)
   (input   :initform "" :accessor sb-input)
   (icaret  :initform 0  :accessor sb-icaret)
   (iprompt :initform "" :accessor sb-iprompt)
   (on-submit :initarg :on-submit :initform nil :accessor sb-on-submit))   ; (input-string) -> submit
  (:metaclass reactive-class))

(defmethod focusable-p ((sb scrollback)) t)

(defun sb-input-index (sb)
  "The line index at which the live input line (when IACTIVE) sits: right after
the committed output + any pending partial line."
  (+ (length (sb-lines sb)) (if (plusp (length (sb-pending sb))) 1 0)))

(defun sb-total (sb)
  (+ (sb-input-index sb) (if (sb-iactive sb) 1 0)))

(defun sb-row (sb i)
  (let ((n (length (sb-lines sb))) (pend (plusp (length (sb-pending sb)))))
    (cond ((< i n) (aref (sb-lines sb) i))
          ((and pend (= i n)) (sb-pending sb))
          ((and (sb-iactive sb) (= i (sb-input-index sb)))
           (format nil "~a ~a" (sb-iprompt sb) (sb-input sb)))
          (t ""))))

(defun sb-set-input (sb text)
  "Replace the live input with TEXT (caret at end); used for history recall."
  (setf (sb-input sb) (or text "") (sb-icaret sb) (length (or text "")))
  (when (sb-follow sb) (sb-scroll-end sb))
  (invalidate sb))

(defun sb-scroll-end (sb)
  "Pin the viewport to the last page (so the newest line is on screen)."
  (let ((b (view-bounds sb)))
    (when b (setf (sb-top sb) (max 0 (- (sb-total sb) (r-h b)))))))

(defun scrollback-clear (sb)
  "Empty the transcript (lines, pending text, presentations); keep any inline input."
  (setf (fill-pointer (sb-lines sb)) 0
        (sb-pending sb) ""
        (sb-top sb) 0)
  (clrhash (sb-presentations sb))
  (invalidate sb))

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
         (attr (role :normal)) (pres (role :label)) (top (sb-top sb)) (total (sb-total sb))
         (iidx (and (sb-iactive sb) (sb-input-index sb))))
    (dotimes (row h)
      (let ((i (+ top row)))
        (cond
          ((eql i iidx)                                ; the live input line: prompt stands out
           (fill-row sb 0 row w attr)
           (draw-text sb 0 row (sb-iprompt sb) (role :label))
           (draw-text sb (1+ (length (sb-iprompt sb))) row (sb-input sb) attr))
          (t (let ((a (if (nth-value 1 (gethash i (sb-presentations sb))) pres attr)))  ; presentations stand out
               (fill-row sb 0 row w a)
               (when (< i total) (draw-text sb 0 row (sb-row sb i) a)))))))
    (when (and iidx (view-focused-p sb) tvision:*screen*)   ; the text cursor sits in the input
      (let ((row (- iidx top)))
        (when (<= 0 row (1- h))
          (tvision:set-cursor-pos tvision:*screen*
                                  (+ (tvision::rect-ax b) 1 (length (sb-iprompt sb)) (sb-icaret sb))
                                  (+ (tvision::rect-ay b) row))
          (tvision:set-cursor-shape :underline)
          (tvision:show-cursor tvision:*screen*))))))

(defmethod handle-event ((sb scrollback) (e mouse-down))
  (let ((i (+ (sb-top sb) (mouse-row sb e))))          ; click a presented result -> act on the live object
    (multiple-value-bind (obj present) (gethash i (sb-presentations sb))
      (when (and present (sb-on-present sb)) (funcall (sb-on-present sb) obj))))
  (setf (handled-p e) t))

(defmethod handle-event ((sb scrollback) (e wheel-event))
  (sb-scroll sb (* 3 (event-delta e))) (setf (handled-p e) t))

(defun %sb-edit-input (sb ks)
  "Edit the live input line for keystroke KS; return T when consumed (else the key
bubbles to the window keymap: Up/Down history, Tab completion, Ctrl-R, Esc quit)."
  (let ((in (sb-input sb)) (c (sb-icaret sb)))
    (cond
      ((and (characterp ks) (graphic-char-p ks))
       (setf (sb-input sb) (concatenate 'string (subseq in 0 c) (string ks) (subseq in c))
             (sb-icaret sb) (1+ c))
       (when (sb-follow sb) (sb-scroll-end sb)) (invalidate sb) t)
      ((eql ks :back)  (when (plusp c)
                         (setf (sb-input sb) (concatenate 'string (subseq in 0 (1- c)) (subseq in c))
                               (sb-icaret sb) (1- c)))
                       (invalidate sb) t)
      ((eql ks :del)   (when (< c (length in))
                         (setf (sb-input sb) (concatenate 'string (subseq in 0 c) (subseq in (1+ c)))))
                       (invalidate sb) t)
      ((eql ks :left)  (setf (sb-icaret sb) (max 0 (1- c))) (invalidate sb) t)
      ((eql ks :right) (setf (sb-icaret sb) (min (length in) (1+ c))) (invalidate sb) t)
      ((eql ks :home)  (setf (sb-icaret sb) 0) (invalidate sb) t)
      ((eql ks :end)   (setf (sb-icaret sb) (length in)) (invalidate sb) t)
      ((eql ks :enter) (when (sb-on-submit sb) (funcall (sb-on-submit sb) in)) t)
      (t nil))))

(defmethod handle-event ((sb scrollback) (e key-event))
  (let* ((ks (event-keysym e)) (page (max 1 (1- (r-h (view-bounds sb))))))
    (cond
      ((and (sb-iactive sb) (%sb-edit-input sb ks)) (setf (handled-p e) t))
      ((eql ks :pgup) (sb-scroll sb (- page))(setf (handled-p e) t))
      ((eql ks :pgdn) (sb-scroll sb page)    (setf (handled-p e) t))
      ;; when there is no inline input, plain Up/Down/Home/End scroll the log
      ((and (not (sb-iactive sb)) (eql ks :up))   (sb-scroll sb -1) (setf (handled-p e) t))
      ((and (not (sb-iactive sb)) (eql ks :down)) (sb-scroll sb 1)  (setf (handled-p e) t))
      ((and (not (sb-iactive sb)) (eql ks :home)) (setf (sb-top sb) 0 (sb-follow sb) nil) (invalidate sb) (setf (handled-p e) t))
      ((and (not (sb-iactive sb)) (eql ks :end))  (setf (sb-follow sb) t) (sb-scroll-end sb) (invalidate sb) (setf (handled-p e) t))
      (t (call-next-method)))))                       ; Up/Down (history), Tab, Ctrl-R, q/Esc bubble
