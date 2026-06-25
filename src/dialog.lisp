;;;; dialog.lisp --- TDialog and the message-box / input-box conveniences.

(in-package #:tvision)

(declaim (special *application*))

(defconstant +mf-warning+       #x0000)
(defconstant +mf-error+         #x0001)
(defconstant +mf-information+   #x0002)
(defconstant +mf-confirmation+  #x0003)
(defconstant +mf-yes-button+    #x0100)
(defconstant +mf-no-button+     #x0200)
(defconstant +mf-ok-button+     #x0400)
(defconstant +mf-cancel-button+ #x0800)

(defclass tdialog (twindow)
  ())

(defmethod initialize-instance :after ((d tdialog) &key)
  (setf (window-flags d) (logior +wf-move+ +wf-close+)
        (view-grow-mode d) 0))

(defmethod get-palette ((d tdialog))
  ;; logical container layout -> application "grey dialog" block (app 16..30)
  (make-palette 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30))

(defmethod valid-p ((d tdialog) command)
  "Cancelling (or closing) a dialog never requires its fields to validate."
  (if (member command (list +cm-cancel+ +cm-close+))
      t
      (call-next-method)))

(defmethod handle-event ((d tdialog) event)
  (call-next-method)
  (cond
    ((and (= (event-type event) +ev-key-down+)
          (= (event-key-code event) +kb-esc+))
     (when (logtest (view-state d) +sf-modal+)
       (end-modal d +cm-cancel+) (clear-event event)))
    ((= (event-type event) +ev-command+)
     (let ((cmd (event-command event)))
       (when (and (member cmd (list +cm-ok+ +cm-cancel+ +cm-yes+ +cm-no+))
                  (logtest (view-state d) +sf-modal+))
         (end-modal d cmd)
         (clear-event event))))))

;;; --- helpers ---------------------------------------------------------------

(defun center-dialog (d)
  "Centre dialog D within the application's desktop."
  (let* ((desk (program-desktop *application*))
         (dw (point-x (view-size d))) (dh (point-y (view-size d)))
         (ox (max 0 (floor (- (point-x (view-size desk)) dw) 2)))
         (oy (max 0 (floor (- (point-y (view-size desk)) dh) 2))))
    (move-to d ox oy)))

(defun %add-buttons (d flags y)
  "Insert the buttons selected by FLAGS along row Y of dialog D, returning their
total width so the caller can size the dialog."
  (let ((specs '()))
    (when (logtest flags +mf-yes-button+)    (push (list "Yes" +cm-yes+ t) specs))
    (when (logtest flags +mf-no-button+)     (push (list "No" +cm-no+ nil) specs))
    (when (logtest flags +mf-ok-button+)     (push (list "OK" +cm-ok+ t) specs))
    (when (logtest flags +mf-cancel-button+) (push (list "Cancel" +cm-cancel+ nil) specs))
    (setf specs (nreverse specs))
    (let* ((bw 10) (gap 2)
           (total (+ (* (length specs) bw) (* (max 0 (1- (length specs))) gap)))
           (x (max 1 (floor (- (point-x (view-size d)) total) 2))))
      (dolist (s specs)
        (insert d (make-button (make-trect x y (+ x bw) (+ y 2))
                               (first s) (second s) (third s)))
        (incf x (+ bw gap)))
      total)))

(defun message-box (text &optional (flags (logior +mf-information+ +mf-ok-button+)))
  "Display TEXT modally with the buttons selected by FLAGS; return the command
that closed the box (e.g. +cm-ok+, +cm-cancel+)."
  (let* ((lines (%split-lines text))
         (textw (reduce #'max lines :key #'length :initial-value 20))
         (w (min 70 (+ 6 (max textw 30))))
         (h (+ 5 (length lines)))
         (d (make-instance 'tdialog
                           :bounds (make-trect 0 0 w h)
                           :title (case (logand flags #x0f)
                                    (#.+mf-error+ "Error")
                                    (#.+mf-information+ "Information")
                                    (#.+mf-confirmation+ "Confirm")
                                    (t "Warning")))))
    (loop for line in lines for y from 2
          do (let ((st (make-instance 'tstatic-text :text line)))
               (set-bounds st (make-trect 3 y (- w 2) (1+ y)))
               (insert d st)))
    (%add-buttons d flags (- h 3))
    (center-dialog d)
    (exec-view (program-desktop *application*) d)))

(defun input-box (title label initial &optional (maxlen 40) history-id)
  "Prompt for a string.  Return (values command string).  When HISTORY-ID is
given, the field remembers submitted values under that id: a down-arrow gadget
(or the Down key) drops down previously-entered values to pick from."
  (let* ((w (max 40 (+ 10 (length label))))
         (d (make-instance 'tdialog :title title :bounds (make-trect 0 0 w 8)))
         (il (if history-id
                 (make-instance 'thistory-input :data initial :maxlen maxlen
                                                :history-id history-id)
                 (make-instance 'tinputline :data initial :maxlen maxlen))))
    (let ((lbl (make-instance 'tlabel :text label :link il)))
      (set-bounds lbl (make-trect 3 2 (- w 3) 3))
      (insert d lbl))
    (set-bounds il (make-trect 3 3 (- w 3) 4))
    (insert d il)
    (%add-buttons d (logior +mf-ok-button+ +mf-cancel-button+) 5)
    (center-dialog d)
    (focus il)
    (let ((cmd (exec-view (program-desktop *application*) d)))
      (when (and history-id (= cmd +cm-ok+)) (history-record il))
      (values cmd (input-data il)))))
