;;;; cluster.lisp --- TCluster base with TCheckBoxes and TRadioButtons.
;;;;
;;;; A cluster shows a vertical column of labelled items.  The shared base
;;;; handles focus row movement, mouse, hot-key letters and rendering; the
;;;; subclass decides the marker glyph and what "pressing" an item does.

(in-package #:tvision)

(defclass tcluster (tview)
  ((labels :initarg :labels :initform '() :accessor cluster-labels)
   (value  :initarg :value  :initform 0   :accessor cluster-value)
   (sel    :initform 0 :accessor cluster-sel)))

(defmethod initialize-instance :after ((c tcluster) &key)
  (setf (view-options c) (logior (view-options c) +of-selectable+
                                 +of-first-click+ +of-pre-process+ +of-post-process+)
        (view-state c) (logior (view-state c) +sf-cursor-vis+)))

(defmethod get-palette ((c tcluster)) (make-palette 6 7 8))  ; normal / sel / disabled

(defgeneric cluster-mark (c i)
  (:documentation "Return the marker string (e.g. \"[X] \") for item I."))
(defgeneric cluster-press (c i)
  (:documentation "Act on item I being pressed."))
(defgeneric multi-state-p (c)
  (:documentation "True if several items may be on at once (check boxes).")
  (:method ((c tcluster)) nil))

(defun cluster-item-on-p (c i)
  (if (multi-state-p c) (logbitp i (cluster-value c)) (= i (cluster-value c))))

(defmethod draw ((c tcluster))
  (let* ((w (point-x (view-size c)))
         (normal (get-color c 1)) (selc (get-color c 2))
         (focused (logtest (view-state c) +sf-focused+))
         (db (make-draw-buffer w)))
    (loop for label in (cluster-labels c)
          for i from 0 below (point-y (view-size c))
          for attr = (if (and focused (= i (cluster-sel c))) selc normal)
          do (db-fill db #\Space attr)
             (db-move-str db 0 (concatenate 'string (cluster-mark c i)
                                            (remove #\~ (princ-to-string label)))
                          attr)
             (write-line* c 0 i w 1 db))
    (when focused
      (set-cursor c 1 (cluster-sel c)))))

(defun cluster-hotkey (label)
  "The character marked with ~ in LABEL, downcased, or NIL."
  (let* ((s (princ-to-string label)) (p (position #\~ s)))
    (when (and p (< (1+ p) (length s))) (char-downcase (char s (1+ p))))))

(defmethod handle-event ((c tcluster) event)
  (cond
    ((and (= (event-type event) +ev-mouse-down+) (mouse-in-view-p c event))
     (let ((row (point-y (make-local c (event-mouse-where event)))))
       (when (< row (length (cluster-labels c)))
         (setf (cluster-sel c) row)
         (cluster-press c row)
         (draw-view c)))
     (clear-event event))
    ((and (= (event-type event) +ev-key-down+) (logtest (view-state c) +sf-focused+))
     (let ((k (event-key-code event)) (ch (event-char-code event)) (handled t))
       (cond
         ((= k +kb-up+)   (setf (cluster-sel c) (max 0 (1- (cluster-sel c)))))
         ((= k +kb-down+) (setf (cluster-sel c) (min (1- (length (cluster-labels c)))
                                                     (1+ (cluster-sel c)))))
         ((= ch +kb-space+) (cluster-press c (cluster-sel c)))
         (t (setf handled nil)))
       (when handled (draw-view c) (clear-event event))))
    ;; hot-key letter selects + presses the matching item (pre/post-process)
    ((and (= (event-type event) +ev-key-down+)
          (plusp (event-char-code event))
          (zerop (event-modifiers event)))
     (let ((ch (char-downcase (code-char (event-char-code event)))))
       (loop for label in (cluster-labels c) for i from 0
             when (eql ch (cluster-hotkey label))
             do (focus c) (setf (cluster-sel c) i) (cluster-press c i)
                (draw-view c) (clear-event event) (return))))))

(defmethod data-size ((c tcluster)) 1)
(defmethod get-data ((c tcluster)) (cluster-value c))
(defmethod set-data ((c tcluster) data) (setf (cluster-value c) (or data 0)) (draw-view c))

;;; --- TCheckBoxes -----------------------------------------------------------

(defclass tcheck-boxes (tcluster) ())

(defmethod multi-state-p ((c tcheck-boxes)) t)
(defmethod cluster-mark ((c tcheck-boxes) i)
  (format nil "[~a] " (if (cluster-item-on-p c i) #\X #\Space)))
(defmethod cluster-press ((c tcheck-boxes) i)
  (setf (cluster-value c) (logxor (cluster-value c) (ash 1 i))))

;; backward-compatible accessors
(defun checkbox-labels (c) (cluster-labels c))
(defun checkbox-value (c) (cluster-value c))
(defun (setf checkbox-value) (v c) (setf (cluster-value c) v))

;;; --- TRadioButtons ---------------------------------------------------------

(defclass tradio-buttons (tcluster) ())

(defmethod cluster-mark ((c tradio-buttons) i)
  (format nil "(~a) " (if (cluster-item-on-p c i) (code-char #x2022) #\Space)))  ; bullet
(defmethod cluster-press ((c tradio-buttons) i)
  (setf (cluster-value c) i))
