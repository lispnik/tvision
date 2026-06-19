;;;; history.lisp --- THistory: an input line with a recallable value history.
;;;;
;;;; A history input line remembers submitted values under a string id.  Press
;;;; the Down arrow to pop up a list of previous values and pick one.

(in-package #:tvision)

(defvar *histories* (make-hash-table :test 'equal)
  "Map of history id -> list of remembered strings (most recent first).")

(defun history-list (id) (gethash id *histories*))

(defun history-add (id string)
  "Remember STRING under ID (most recent first, de-duplicated)."
  (when (and (stringp string) (plusp (length string)))
    (setf (gethash id *histories*)
          (cons string (remove string (gethash id *histories*) :test #'string=))))
  string)

(defun history-clear (id) (remhash id *histories*))

;;; The drop-down list of remembered values, and the window that frames it.
(defclass thistory-viewer (tlist-box) ()
  (:documentation "The list of remembered values shown by a history input line."))
(defclass thistory-window (tdialog) ()
  (:documentation "The pop-up window framing a THistoryViewer."))

(defclass thistory-input (tinputline)
  ((history-id :initarg :history-id :initform "default" :accessor history-id)))

(defmethod draw ((il thistory-input))
  ;; draw the input line, then a down-arrow gadget at the right edge
  (call-next-method)
  (let ((w (point-x (view-size il))))
    (when (>= w 1)
      (let ((db (make-draw-buffer 1)))
        (db-put-char db 0 (code-char #x25BC) (get-color il 3))
        (write-line* il (1- w) 0 1 1 db)))))

(defun history-popup (il)
  "Pop up a list of remembered values; if one is chosen, load it."
  (let ((items (history-list (history-id il))))
    (when (and items *application*)
      (multiple-value-bind (gx gy) (view-global-origin il)
        (let* ((h (min (+ 2 (length items)) 10))
               (w (max (point-x (view-size il)) 20))
               (d (make-instance 'thistory-window :title "History"
                                 :bounds (make-trect 0 0 (+ w 2) h)))
               (lb (make-instance 'thistory-viewer :items items
                                  :command +cm-ok+
                                  :bounds (make-trect 1 1 (+ w 1) (1- h)))))
          (insert d lb)
          ;; place the popup just below the input line, clamped to the desktop
          (let* ((desk (program-desktop *application*))
                 (oy (min (1+ gy) (max 0 (- (point-y (view-size desk)) h)))))
            (move-to d (max 0 (1- gx)) oy))
          (focus lb)
          (when (= (exec-view (program-desktop *application*) d) +cm-ok+)
            (set-data il (list-item lb (list-focused lb)))
            (draw-view il)))))))

(defmethod handle-event ((il thistory-input) event)
  (cond
    ((and (= (event-type event) +ev-key-down+)
          (= (event-key-code event) +kb-down+)
          (logtest (view-state il) +sf-focused+))
     (history-popup il)
     (clear-event event))
    ((and (= (event-type event) +ev-mouse-down+)
          (mouse-in-view-p il event)
          ;; click on the down-arrow gadget
          (= (point-x (make-local il (event-mouse-where event)))
             (1- (point-x (view-size il)))))
     (history-popup il)
     (clear-event event))
    (t (call-next-method))))

(defun history-record (il)
  "Remember the current value of input line IL under its history id."
  (when (typep il 'thistory-input)
    (history-add (history-id il) (input-data il))))
