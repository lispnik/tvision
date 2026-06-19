;;;; widgets.lisp --- Common controls: static text, labels, buttons,
;;;;                   input lines and check boxes.

(in-package #:tvision)

;;; ===========================================================================
;;; TStaticText
;;; ===========================================================================

(defclass tstatic-text (tview)
  ((text :initarg :text :initform "" :accessor static-text-text)))

(defmethod get-palette ((v tstatic-text)) (make-palette 6))

(defun %split-lines (string)
  (let ((lines '()) (start 0))
    (loop for i from 0 below (length string)
          when (char= (char string i) #\Newline)
          do (push (subseq string start i) lines) (setf start (1+ i)))
    (push (subseq string start) lines)
    (nreverse lines)))

(defmethod draw ((v tstatic-text))
  (let* ((w (point-x (view-size v)))
         (h (point-y (view-size v)))
         (c (get-color v 1))
         (db (make-draw-buffer w)))
    (db-fill db #\Space c)
    (write-line* v 0 0 w h db)
    (loop for line in (%split-lines (static-text-text v))
          for y from 0 below h
          do (write-str v 0 y (if (> (length line) w) (subseq line 0 w) line) 1))))

;;; ===========================================================================
;;; TLabel -- like static text, but highlights and points at a linked control.
;;; ===========================================================================

(defclass tlabel (tstatic-text)
  ((link :initarg :link :initform nil :accessor label-link)))

(defmethod get-palette ((v tlabel)) (make-palette 7 8))

(defmethod draw ((v tlabel))
  (let* ((w (point-x (view-size v)))
         (focused (and (label-link v)
                       (logtest (view-state (label-link v)) +sf-focused+)))
         (c (get-color v (if focused 2 1)))
         (db (make-draw-buffer w)))
    (db-fill db #\Space c)
    (db-move-str db 0 (let ((s (static-text-text v)))
                        (if (> (length s) w) (subseq s 0 w) s))
                 c)
    (write-line* v 0 0 w 1 db)))

;;; ===========================================================================
;;; TButton
;;; ===========================================================================

(defclass tbutton (tview)
  ((title   :initarg :title   :initform "" :accessor button-title)
   (command :initarg :command :initform 0  :accessor button-command)
   (default :initarg :default :initform nil :accessor button-default-p)))

(defmethod initialize-instance :after ((b tbutton) &key)
  (setf (view-options b) (logior (view-options b)
                                 +of-selectable+ +of-first-click+
                                 +of-pre-process+ +of-post-process+)))

(defun make-button (bounds title command &optional default)
  (let ((b (make-instance 'tbutton :title title :command command :default default)))
    (set-bounds b bounds)
    b))

;; logical: 1 normal, 2 default, 3 selected, 4 shortcut
(defmethod get-palette ((b tbutton)) (make-palette 9 10 11 12))

(defmethod draw ((b tbutton))
  (let* ((w (point-x (view-size b)))
         (h (point-y (view-size b)))
         (enabled (and (command-enabled-p (button-command b)) (not (view-disabled-p b))))
         (selected (logtest (view-state b) +sf-selected+))
         (cidx (cond ((not enabled) 1) (selected 3) ((button-default-p b) 2) (t 1)))
         (c (if enabled (get-color b cidx)
                (make-attr 8 (attr-bg (get-color b 1)))))  ; dim disabled
         (title (remove #\~ (or (button-title b) "")))
         (db (make-draw-buffer w)))
    (db-fill db #\Space c)
    (let ((tx (max 0 (floor (- w (length title)) 2))))
      (db-move-str db tx title c))
    (write-line* b 0 0 w 1 db)
    ;; a simple drop shadow for the classic raised-button look
    (when (> h 1)
      (let ((sh (make-draw-buffer w)))
        (db-fill sh #\Space (make-attr 8 0))
        (write-line* b 1 1 (1- w) 1 sh)))))

(defun press-button (b)
  "Fire B's command into the event queue."
  (draw-view b)
  (put-event b (make-event :type +ev-command+ :command (button-command b) :info b)))

(defun button-hotkey (b)
  "The button's Alt-mnemonic character (the one marked with ~ in its title),
downcased, or NIL."
  (let* ((title (or (button-title b) "")) (p (position #\~ title)))
    (when (and p (< (1+ p) (length title)))
      (char-downcase (char title (1+ p))))))

(defmethod handle-event ((b tbutton) event)
  (when (command-enabled-p (button-command b))
    (cond
      ((and (= (event-type event) +ev-mouse-down+) (mouse-in-view-p b event))
       (press-button b) (clear-event event))
      ((= (event-type event) +ev-key-down+)
       (let ((k (event-key-code event)) (ch (event-char-code event)))
         (cond
           ;; Enter presses the focused button, or the default button anywhere
           ((and (= k +kb-enter+)
                 (or (logtest (view-state b) +sf-focused+) (button-default-p b)))
            (press-button b) (clear-event event))
           ;; Space presses the focused button
           ((and (= ch +kb-space+) (logtest (view-state b) +sf-focused+))
            (press-button b) (clear-event event))
           ;; Alt-<mnemonic> presses the button from anywhere (buttons are
           ;; pre/post-process views, so they see this even when unfocused)
           ((and (logtest (event-modifiers event) +md-alt+)
                 (plusp ch)
                 (let ((hk (button-hotkey b)))
                   (and hk (char-equal (code-char ch) hk))))
            (press-button b) (clear-event event))))))))

;;; ===========================================================================
;;; TInputLine
;;; ===========================================================================

(defclass tinputline (tview)
  ((data      :initarg :data   :initform "" :accessor input-data)
   (maxlen    :initarg :maxlen :initform 255 :accessor input-maxlen)
   (cur-pos   :initform 0 :accessor input-cur-pos)
   (first-pos :initform 0 :accessor input-first-pos)
   (validator :initarg :validator :initform nil :accessor input-validator)))

(defmethod initialize-instance :after ((il tinputline) &key)
  (setf (view-options il) (logior (view-options il)
                                  +of-selectable+ +of-first-click+)
        (view-state il) (logior (view-state il) +sf-cursor-vis+)
        (input-cur-pos il) (length (input-data il))))

(defmethod get-palette ((il tinputline)) (make-palette 13 14 15))

(defmethod draw ((il tinputline))
  (let* ((w (point-x (view-size il)))
         (focused (logtest (view-state il) +sf-focused+))
         (c (if (view-disabled-p il)
                (make-attr 8 (attr-bg (get-color il 1)))
                (get-color il (if focused 2 1))))
         (db (make-draw-buffer w))
         (txt (input-data il)))
    (db-fill db #\Space c)
    (let* ((fp (min (input-first-pos il) (length txt)))
           (vis (subseq txt fp))
           (shown (subseq vis 0 (min (length vis) (max 0 (- w 1))))))
      (db-move-str db 1 shown c))
    (write-line* il 0 0 w 1 db)
    (when focused
      (set-cursor il (max 1 (+ 1 (- (input-cur-pos il) (input-first-pos il)))) 0))))

(defun %input-adjust (il)
  "Scroll the visible window so the cursor stays in view."
  (let ((w (- (point-x (view-size il)) 2)))
    (when (< (input-cur-pos il) (input-first-pos il))
      (setf (input-first-pos il) (input-cur-pos il)))
    (when (>= (input-cur-pos il) (+ (input-first-pos il) w))
      (setf (input-first-pos il) (- (input-cur-pos il) w -1)))
    (setf (input-first-pos il) (max 0 (input-first-pos il)))))

(defmethod handle-event ((il tinputline) event)
  (when (and (= (event-type event) +ev-key-down+)
             (logtest (view-state il) +sf-focused+))
    (let ((k (event-key-code event))
          (ch (event-char-code event))
          (txt (input-data il))
          (handled t))
      (cond
        ((= k +kb-left+)  (when (> (input-cur-pos il) 0) (decf (input-cur-pos il))))
        ((= k +kb-right+) (when (< (input-cur-pos il) (length txt)) (incf (input-cur-pos il))))
        ((= k +kb-home+)  (setf (input-cur-pos il) 0))
        ((= k +kb-end+)   (setf (input-cur-pos il) (length txt)))
        ((= k +kb-back+)
         (when (> (input-cur-pos il) 0)
           (setf (input-data il)
                 (concatenate 'string (subseq txt 0 (1- (input-cur-pos il)))
                              (subseq txt (input-cur-pos il))))
           (decf (input-cur-pos il))))
        ((= k +kb-del+)
         (when (< (input-cur-pos il) (length txt))
           (setf (input-data il)
                 (concatenate 'string (subseq txt 0 (input-cur-pos il))
                              (subseq txt (1+ (input-cur-pos il)))))))
        ((and (>= ch 32) (< ch 127) (< (length txt) (input-maxlen il)))
         (let ((candidate (concatenate 'string (subseq txt 0 (input-cur-pos il))
                                       (string (code-char ch))
                                       (subseq txt (input-cur-pos il)))))
           ;; reject the keystroke if a validator forbids the result
           (if (or (null (input-validator il))
                   (is-valid-input (input-validator il) candidate))
               (progn (setf (input-data il) candidate) (incf (input-cur-pos il)))
               (setf handled nil))))
        (t (setf handled nil)))
      (when handled
        (%input-adjust il)
        (draw-view il)
        (clear-event event)))))

(defmethod valid-p ((il tinputline) command)
  "Reject Ok/default commands when the validator says the value is incomplete.
On rejection the cursor is moved to the field and a message box (if available)
shows the validator's error."
  (if (and (input-validator il)
           (member command (list +cm-ok+ +cm-default+ +cm-yes+))
           (not (is-valid (input-validator il) (input-data il))))
      (progn
        (focus il)
        ;; report the error if the dialog layer is loaded (avoids a hard dep)
        (when (fboundp 'message-box)
          (funcall 'message-box (validator-error-message (input-validator il)) 1025))
        nil)
      t))

(defmethod data-size ((il tinputline)) (length (input-data il)))
(defmethod get-data ((il tinputline)) (input-data il))
(defmethod set-data ((il tinputline) data)
  (setf (input-data il) (princ-to-string data)
        (input-cur-pos il) (length (input-data il))
        (input-first-pos il) 0))

;;; ===========================================================================
;;; TParamText -- static text with format arguments.
;;; ===========================================================================

(defclass tparam-text (tstatic-text)
  ((format-args :initarg :args :initform '() :accessor param-text-args)))

(defun set-param-text (pt control-string &rest args)
  (setf (static-text-text pt) (apply #'format nil control-string args))
  (draw-view pt))

;;; TCheckBoxes now lives in cluster.lisp (atop the shared TCluster base).
