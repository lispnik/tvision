;;;; window.lisp --- TWindow, a framed, movable, focusable group.

(in-package #:tvision)

;;; Window flags (+wf-move+ etc.) are defined in events.lisp so that TFrame,
;;; which is compiled before TWindow, can reference them.

(defclass twindow (tgroup)
  ((title  :initarg :title  :initform nil :accessor window-title)
   (number :initarg :number :initform +wn-no-number+ :accessor window-number)
   (flags  :initarg :flags
           :initform (logior +wf-move+ +wf-grow+ +wf-close+ +wf-zoom+)
           :accessor window-flags)
   (frame  :initform nil :accessor window-frame)
   (zoom-rect :initform nil :accessor window-zoom-rect)))

(defmethod initialize-instance :after ((w twindow) &key)
  (setf (view-state w) (logior (view-state w) +sf-shadow+)
        (view-options w) (logior (view-options w)
                                 +of-selectable+ +of-top-select+ +of-framed+))
  (setf (window-zoom-rect w) (get-bounds w))
  (let ((f (make-instance 'tframe)))
    (set-bounds f (get-extent w))
    (setf (window-frame w) f)
    (insert w f)))

(defmethod get-palette ((w twindow))
  ;; logical container layout -> application "blue window" block (app 1..15)
  (make-palette 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15))

(defmethod frame-owner-title ((w twindow)) (window-title w))
(defmethod frame-owner-flags ((w twindow)) (window-flags w))
(defmethod frame-owner-number ((w twindow)) (window-number w))

(defun standard-scrollbar (w &optional (vertical t))
  "Create and insert a scroll bar along the right (or bottom) edge of W."
  (let* ((ext (get-extent w))
         (sb (make-instance 'tscrollbar :vertical vertical)))
    (if vertical
        (set-bounds sb (make-trect (1- (rect-bx ext)) (1+ (rect-ay ext))
                                   (rect-bx ext) (1- (rect-by ext))))
        (set-bounds sb (make-trect (1+ (rect-ax ext)) (1- (rect-by ext))
                                   (1- (rect-bx ext)) (rect-by ext))))
    (insert w sb)
    sb))

;;; --- close / zoom ----------------------------------------------------------

(defun close-window (w)
  (when (valid-p w +cm-close+)
    (let ((o (view-owner w)))
      (when o (remove-view o w)))))

(defun zoom-window (w)
  (when (logtest (window-flags w) +wf-zoom+)
    (let* ((o (view-owner w))
           (full (get-extent o)))
      (if (rect-equal-p (get-bounds w) full)
          (locate w (window-zoom-rect w))
          (progn (setf (window-zoom-rect w) (get-bounds w))
                 (locate w full))))))

;;; --- events ----------------------------------------------------------------

(defmethod handle-event ((w twindow) event)
  ;; let subviews (controls) handle the event first
  (call-next-method)
  (cond
    ((= (event-type event) +ev-command+)
     (when (logtest (view-state w) +sf-selected+)
       (cond
         ((and (= (event-command event) +cm-close+)
               (logtest (window-flags w) +wf-close+))
          (close-window w) (clear-event event))
         ((and (= (event-command event) +cm-zoom+)
               (logtest (window-flags w) +wf-zoom+))
          (zoom-window w) (clear-event event))
         ((and (= (event-command event) +cm-resize+)
               (logtest (window-flags w) (logior +wf-move+ +wf-grow+)))
          (move-size-window w) (clear-event event)))))
    ((= (event-type event) +ev-mouse-down+)
     (let* ((p (event-mouse-where event))
            (lp (make-local w p))
            (lx (point-x lp)) (ly (point-y lp))
            (w-width (point-x (view-size w)))
            (w-height (point-y (view-size w))))
       (cond
         ;; bottom-right corner is the resize grip
         ((and (logtest (window-flags w) +wf-grow+)
               (= lx (1- w-width)) (= ly (1- w-height)))
          (resize-window w event) (clear-event event))
         ((= ly 0)                          ; click on the title bar
          (cond
            ;; close icon "[x]" sits at local cols 2-4
            ((and (logtest (window-flags w) +wf-close+) (<= 2 lx 4))
             (close-window w) (clear-event event))
            ;; zoom icon sits near the right edge
            ((and (logtest (window-flags w) +wf-zoom+) (<= (- w-width 5) lx (- w-width 3)))
             (zoom-window w) (clear-event event))
            ;; otherwise begin dragging the window
            ((logtest (window-flags w) +wf-move+)
             (drag-window w event) (clear-event event)))))))
    ((= (event-type event) +ev-key-down+)
     ;; Ctrl-W closes the focused window, like Turbo Vision
     (when (and (= (event-key-code event) +kb-ctrl-w+)
                (logtest (window-flags w) +wf-close+))
       (close-window w) (clear-event event)))))

;; Window validity (all subviews must validate) is inherited from TGROUP.

(defun %darken-cell (sx sy)
  "Darken the screen cell at absolute (SX,SY), keeping its glyph (drop shadow)."
  (let ((s *screen*))
    (when (and s (>= sx 0) (< sx (screen-width s)) (>= sy 0) (< sy (screen-height s)))
      (let* ((idx (+ sx (* sy (screen-width s))))
             (c (aref (screen-back s) idx)))
        (setf (aref (screen-back s) idx)
              (cell-make-code (cell-char-code c) (make-attr 8 0)))))))

(defmethod draw :after ((w twindow))
  "Paint a drop shadow below and to the right of a shadowed window."
  (when (and *screen* (logtest (view-state w) +sf-shadow+))
    (multiple-value-bind (gx gy) (view-global-origin w)
      (let ((ww (point-x (view-size w))) (wh (point-y (view-size w))))
        ;; two-column shadow down the right edge
        (loop for y from (1+ gy) to (+ gy wh) do
          (%darken-cell (+ gx ww) y)
          (%darken-cell (+ gx ww 1) y))
        ;; one-row shadow along the bottom
        (loop for x from (+ gx 2) to (+ gx ww 1) do
          (%darken-cell x (+ gy wh)))))))
