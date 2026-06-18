;;;; statusline.lisp --- TStatusLine, the hint/shortcut bar along the bottom.

(in-package #:tvision)

(defstruct (tstatus-item (:constructor make-status-item (text key-code command)))
  (text "" :type string)
  (key-code 0 :type fixnum)
  (command 0 :type fixnum))

(defstruct (tstatus-def (:constructor make-status-def (min max items)))
  min max items)

(defclass tstatus-line (tview)
  ((items   :initarg :items :initform '() :accessor status-items)
   ;; optional context-sensitive sets: a list of TSTATUS-DEF; the active set is
   ;; chosen by the current help context (see SET-STATUS-CONTEXT)
   (defs    :initarg :defs  :initform nil :accessor status-defs)
   (context :initform -1 :accessor status-context)))

(defun set-status-context (sl ctx)
  "Switch the displayed items to the TSTATUS-DEF whose range contains CTX."
  (when (and (status-defs sl) (/= ctx (status-context sl)))
    (setf (status-context sl) ctx)
    (let ((def (find-if (lambda (d) (<= (tstatus-def-min d) ctx (tstatus-def-max d)))
                        (status-defs sl))))
      (when def
        (setf (status-items sl) (tstatus-def-items def))
        (draw-view sl)))))

(defmethod initialize-instance :after ((sl tstatus-line) &key)
  (setf (view-grow-mode sl) (logior +gf-grow-loy+ +gf-grow-hix+ +gf-grow-hiy+)))

(defmethod get-palette ((sl tstatus-line)) (make-palette 35 36 37))

(defun %status-layout (sl)
  "Return a list of (item start-x text-string) describing the bar layout."
  (let ((x 0) (out '()))
    (dolist (it (status-items sl))
      (let ((display (format nil " ~a " (remove #\~ (tstatus-item-text it)))))
        (push (list it x display) out)
        (incf x (length display))))
    (nreverse out)))

(defmethod draw ((sl tstatus-line))
  (let* ((w (point-x (view-size sl)))
         (normal (get-color sl 1))         ; palette: normal
         (key    (get-color sl 2))         ; palette: highlighted shortcut
         (dim    (get-color sl 3))         ; palette: disabled
         (pal (make-palette normal key))
         (dim-pal (make-palette dim dim))
         (db (make-draw-buffer w)))
    (db-fill db #\Space normal)
    ;; Render each item, honouring ~..~ shortcut highlight markers.  The
    ;; advance uses the de-marked length so positions match %status-layout.
    ;; Items whose command is disabled are drawn dimmed.
    (let ((x 0))
      (dolist (it (status-items sl))
        (let* ((text (tstatus-item-text it))
               (display (format nil " ~a " text))
               (enabled (or (zerop (tstatus-item-command it))
                            (command-enabled-p (tstatus-item-command it)))))
          (db-move-cstr db x display (if enabled pal dim-pal) 1)
          (incf x (length (format nil " ~a " (remove #\~ text)))))))
    (write-line* sl 0 0 w 1 db)))

(defmethod handle-event ((sl tstatus-line) event)
  (cond
    ((= (event-type event) +ev-mouse-down+)
     (let ((lx (point-x (make-local sl (event-mouse-where event)))))
       (dolist (entry (%status-layout sl))
         (destructuring-bind (item x display) entry
           (when (and (>= lx x) (< lx (+ x (length display)))
                      (plusp (tstatus-item-command item))
                      (command-enabled-p (tstatus-item-command item)))
             (put-event sl (make-event :type +ev-command+
                                       :command (tstatus-item-command item)))
             (clear-event event)
             (return))))))
    ((= (event-type event) +ev-key-down+)
     (dolist (it (status-items sl))
       (when (and (plusp (tstatus-item-key-code it))
                  (= (event-key-code event) (tstatus-item-key-code it))
                  (command-enabled-p (tstatus-item-command it)))
         (put-event sl (make-event :type +ev-command+ :command (tstatus-item-command it)))
         (clear-event event)
         (return))))))
