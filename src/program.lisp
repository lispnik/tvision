;;;; program.lisp --- TProgram / TApplication and the main event loop.

(in-package #:tvision)

(defvar *application* nil "The running TProgram instance.")

;;; ---------------------------------------------------------------------------
;;; The application (root) palette: these are the only *real* attribute bytes.
;;; Every other palette in the system maps its indices up the owner chain until
;;; it reaches one of these entries.
;;;
;;;   1-15  : "blue window" block
;;;   16-30 : "grey dialog" block
;;; ---------------------------------------------------------------------------

;;; Indices 1-15 = window block, 16-30 = dialog block (each in the role order
;;; frame-passive, frame-active, icon, sb-page, sb-thumb, text, label,
;;; label-sel, button, button-default, button-sel, shortcut, input, input-sel,
;;; input-arrow).  Indices 31-38 = menu/status/desktop (see below).

(defparameter +app-palette-color+
  (make-palette
   ;; --- blue window block (1-15) ---
   (make-attr 7 1) (make-attr 15 1) (make-attr 10 1) (make-attr 1 3)
   (make-attr 7 1) (make-attr 7 1) (make-attr 7 1) (make-attr 15 1)
   (make-attr 15 1) (make-attr 14 1) (make-attr 15 3) (make-attr 13 1)
   (make-attr 0 3) (make-attr 15 3) (make-attr 14 3)
   ;; --- grey dialog block (16-30) ---
   (make-attr 8 7) (make-attr 0 7) (make-attr 15 7) (make-attr 0 7)
   (make-attr 8 7) (make-attr 0 7) (make-attr 0 7) (make-attr 15 7)
   (make-attr 15 2) (make-attr 14 2) (make-attr 15 6) (make-attr 13 2)
   (make-attr 0 3) (make-attr 15 3) (make-attr 0 3)
   ;; --- menu (31-34): normal, selected, disabled, hot ---
   (make-attr 0 7) (make-attr 15 2) (make-attr 8 7) (make-attr 4 7)
   ;; --- status line (35-37): normal, hot, disabled ---
   (make-attr 0 3) (make-attr 13 3) (make-attr 8 3)
   ;; --- desktop background (38) ---
   (make-attr 7 1)))

(defun %role-of (i) (mod (1- i) 15))

(defun %bw-attr (role)
  "Greyscale attribute for a window/dialog role (0-14)."
  (case role
    ((0 3 5 6) (make-attr 7 0))       ; passive frame / sb-page / text / label
    ((1 2 4 7 10 11 13) (make-attr 15 0)) ; active/highlight/bold
    ((8) (make-attr 0 7))             ; button normal (reverse)
    ((9) (make-attr 0 15))            ; default button (bold reverse)
    ((12 14) (make-attr 0 7))         ; input fields (reverse)
    (t (make-attr 7 0))))

(defun %mono-attr (role)
  (case role
    ((1 2 4 7 10 11 13) (make-attr 15 0)) ; bold
    ((8 9 12 14) (make-attr 0 7))         ; reverse
    (t (make-attr 7 0))))                 ; normal

(defun %build-extended (role-fn menu status desk)
  "Build a 38-entry palette from a window/dialog role mapper plus explicit
menu (4), status (3) and desktop (1) attribute lists."
  (apply #'make-palette
         (append (loop for i from 1 to 30 collect (funcall role-fn (%role-of i)))
                 menu status (list desk))))

(defparameter +app-palette-bw+
  (%build-extended #'%bw-attr
                   (list (make-attr 7 0) (make-attr 0 7) (make-attr 8 0) (make-attr 15 0))
                   (list (make-attr 7 0) (make-attr 15 0) (make-attr 8 0))
                   (make-attr 7 0)))

(defparameter +app-palette-mono+
  (%build-extended #'%mono-attr
                   (list (make-attr 7 0) (make-attr 0 7) (make-attr 8 0) (make-attr 15 0))
                   (list (make-attr 7 0) (make-attr 15 0) (make-attr 8 0))
                   (make-attr 0 7)))

(defparameter +app-palette+ +app-palette-color+)  ; backward-compatible alias

;;; ---------------------------------------------------------------------------
;;; TProgram
;;; ---------------------------------------------------------------------------

(defclass tprogram (tgroup)
  ((desktop      :initform nil :accessor program-desktop)
   (status-line  :initform nil :accessor program-status-line)
   (menu-bar     :initform nil :accessor program-menu-bar)
   (screen       :initform nil :accessor program-screen)
   (pending      :initform '() :accessor program-pending)
   (palette-mode :initform :color :accessor program-palette-mode)))

(defmethod get-palette ((app tprogram))
  (ecase (program-palette-mode app)
    (:color +app-palette-color+)
    (:bw    +app-palette-bw+)
    (:mono  +app-palette-mono+)))

(defun set-palette-mode (mode &optional (app *application*))
  "Switch the colour scheme: :color, :bw, or :mono."
  (setf (program-palette-mode app) mode)
  (draw-view app)
  (when *screen* (flush-screen *screen*)))

(defmethod put-event ((app tprogram) event)
  (setf (program-pending app)
        (nconc (program-pending app) (list (copy-event event)))))

;;; --- terminal resize (SIGWINCH) --------------------------------------------

(defvar *resize-pending* nil
  "Set by the SIGWINCH handler; serviced by GET-EVENT on the main thread.")

(defparameter +sigwinch+
  (or (ignore-errors (symbol-value (find-symbol "SIGWINCH" :sb-unix))) 28))

(defun install-resize-handler ()
  (handler-case
      (sb-sys:enable-interrupt
       +sigwinch+
       (lambda (&rest _) (declare (ignore _)) (setf *resize-pending* t)))
    (error () nil)))

(defun apply-resize (app)
  "Re-query the terminal size and, if it changed, resize the screen buffers and
reflow the whole view tree."
  (setf *resize-pending* nil)
  (when *screen*
    (multiple-value-bind (rows cols) (%query-size)
      (when (and (>= cols 2) (>= rows 2)
                 (or (/= cols (screen-width *screen*))
                     (/= rows (screen-height *screen*))))
        (screen-resize *screen* cols rows)
        (change-bounds app (make-trect 0 0 cols rows))
        (draw-view app)
        (flush-screen *screen*)))))

(defun get-event (&optional (app *application*))
  "Return the next event: queued application events first, then terminal input.
A pending terminal resize is serviced first so every loop (main, modal dialog,
menu tracking) reflows on SIGWINCH."
  (when *resize-pending* (apply-resize app))
  (drain-ui-callbacks)
  (cond
    ((program-pending app) (pop (program-pending app)))
    (t (when *screen* (pump-input *screen* 0.02))
       (or (and *screen* (screen-next-event *screen*))
           (make-event :type +ev-nothing+)))))

;;; --- overridable application hooks -----------------------------------------

(defgeneric status-line-items (app)
  (:method ((app tprogram))
    (list (make-status-item "~Alt-X~ Exit" +kb-alt-x+ +cm-quit+))))

(defgeneric application-menu (app)
  (:documentation "Return the MENU for the menu bar, or NIL for no menu bar.")
  (:method ((app tprogram)) nil))

(defgeneric init-menu-bar (app)
  (:method ((app tprogram))
    (let ((menu (application-menu app)))
      (when menu
        (let ((mb (make-instance 'tmenu-bar :menu menu
                                 :bounds (make-trect 0 0 (point-x (view-size app)) 1))))
          (setf (program-menu-bar app) mb)
          (insert app mb))))))

(defgeneric init-desktop (app)
  (:method ((app tprogram))
    (let* ((top (if (program-menu-bar app) 1 0))
           (d (make-instance 'tdesktop
                             :bounds (make-trect 0 top (point-x (view-size app))
                                                 (1- (point-y (view-size app)))))))
      (setf (program-desktop app) d)
      (insert app d))))

(defgeneric init-status-line (app)
  (:method ((app tprogram))
    (let* ((h (point-y (view-size app)))
           (w (point-x (view-size app)))
           (sl (make-instance 'tstatus-line :items (status-line-items app))))
      (set-bounds sl (make-trect 0 (1- h) w h))
      (setf (view-options sl) (logior (view-options sl) +of-post-process+))
      (setf (program-status-line app) sl)
      (insert app sl))))

(defgeneric setup (app)
  (:documentation "Subclass hook: open initial windows etc.")
  (:method ((app tprogram)) nil))

(defgeneric idle (app)
  (:method ((app tprogram)) nil))

(defun suspend (&optional (app *application*)) (declare (ignore app)) nil)
(defun resume (&optional (app *application*)) (declare (ignore app)) nil)
(defun set-screen-mode (mode &optional (app *application*))
  (declare (ignore mode app)) nil)

;;; --- global event handling -------------------------------------------------

(defun key->command (e cmd)
  (setf (event-type e) +ev-command+ (event-command e) cmd (event-info e) nil))

(defun current-help-ctx (app)
  "The nearest non-zero help context along the focused chain (so a window's
context applies to its controls, which usually leave help-ctx at 0)."
  (let ((v app) (ctx +hc-no-context+))
    (loop
      (when (plusp (view-help-ctx v)) (setf ctx (view-help-ctx v)))
      (if (and (typep v 'tgroup) (group-current v))
          (setf v (group-current v))
          (return)))
    ctx))

(defun refresh-status-context (app)
  "Switch the status line to match the focused view's help context."
  (when (program-status-line app)
    (set-status-context (program-status-line app) (current-help-ctx app))))

(defun process-command-set-changes (app)
  "If the command set changed, broadcast +cm-command-set-changed+ so views can
refresh any cached enabled-state (TV's commandSetChanged contract)."
  (when *command-set-changed*
    (setf *command-set-changed* nil)
    (message app +ev-broadcast+ +cm-command-set-changed+ nil)))

(defmethod handle-event ((app tprogram) event)
  ;; translate global shortcuts into commands before dispatch
  (when (= (event-type event) +ev-key-down+)
    (cond
      ((= (event-key-code event) +kb-alt-x+)
       (key->command event +cm-quit+))
      ;; F1 opens help for the focused view's context (help.lisp loads later)
      ((and (= (event-key-code event) +kb-f1+) (fboundp 'open-help))
       (funcall 'open-help (current-help-ctx app)) (clear-event event))
      ;; Alt+1..9 selects the window with that number
      ((and (logtest (event-modifiers event) +md-alt+)
            (<= (char-code #\1) (event-char-code event) (char-code #\9))
            (program-desktop app))
       (select-window-by-number (program-desktop app)
                                (- (event-char-code event) (char-code #\0)))
       (clear-event event))))
  (call-next-method)
  (when (and (= (event-type event) +ev-command+)
             (= (event-command event) +cm-quit+))
    (setf (group-end-state app) +cm-quit+)
    (clear-event event)))

(defmethod end-modal ((app tprogram) command)
  (setf (group-end-state app) command))

;;; --- main loop -------------------------------------------------------------

(defvar *event-error-hook* nil
  "Called with the condition when HANDLE-EVENT signals an error during the modal
loop.  Lets the application report the error (e.g. a message box) instead of
letting it tear down the whole program.  When NIL, the error is written to
*ERROR-OUTPUT* and the loop continues.")

(defun %handle-loop-event (view e)
  "Dispatch one event, containing any error so a single bad command can never
crash the program."
  (handler-case
      (if (/= (event-type e) +ev-nothing+)
          (handle-event view e)
          (idle *application*))
    (serious-condition (c)
      (if *event-error-hook*
          (ignore-errors (funcall *event-error-hook* c))
          (ignore-errors (format *error-output* "~&[event error] ~a~%" c))))))

(defun modal-loop (view)
  "Run VIEW's event loop until its end-state is set AND that command validates.
If VALID-P rejects the end command (e.g. a field's validator fails), the loop
resumes -- mirroring Turbo Vision's execView/valid contract."
  (loop
    (setf (group-end-state view) nil)
    (loop until (group-end-state view) do
      (refresh-status-context *application*)
      (process-command-set-changes *application*)
      (draw-view *application*)
      (when *screen* (flush-screen *screen*))
      (%handle-loop-event view (get-event *application*)))
    (when (valid-p view (group-end-state view))
      (return (group-end-state view)))))

(defun init-application (app s)
  (setf (program-screen app) s)
  (reset-commands)
  (install-resize-handler)
  (set-bounds app (make-trect 0 0 (screen-width s) (screen-height s)))
  (setf (view-state app) (logior (view-state app) +sf-focused+ +sf-selected+))
  (install-ui-wakeup)
  (init-menu-bar app)
  (init-desktop app)
  (init-status-line app)
  (setup app))

(defun run (app-class &rest init-args)
  "Initialise the terminal, build an APP-CLASS instance, run it, then restore."
  (with-screen (s)
    (let ((app (apply #'make-instance app-class init-args)))
      (setf *application* app)
      (unwind-protect
           (progn
             (init-application app s)
             (modal-loop app))
        (shutdown-background-threads)
        (remove-ui-wakeup)
        (setf *application* nil)))))

(defun program-loop (&optional (app *application*)) (modal-loop app))

;;; --- window dragging --------------------------------------------------------

(defun drag-window (w start-event)
  "Drag window W with the mouse until the button is released."
  (let ((owner (view-owner w)))
    (multiple-value-bind (gx gy) (view-global-origin w)
      (let ((dx (- (point-x (event-mouse-where start-event)) gx))
            (dy (- (point-y (event-mouse-where start-event)) gy)))
        (loop
          (draw-view *application*)
          (when *screen* (flush-screen *screen*))
          (let ((e (get-event *application*)))
            (cond
              ((member (event-type e) (list +ev-mouse-move+ +ev-mouse-auto+))
               (multiple-value-bind (ox oy) (view-global-origin owner)
                 (move-to w
                          (max 0 (- (point-x (event-mouse-where e)) dx ox))
                          (max 0 (- (point-y (event-mouse-where e)) dy oy)))))
              ((or (= (event-type e) +ev-mouse-up+)
                   (= (event-type e) +ev-key-down+))
               (return))
              ((= (event-type e) +ev-nothing+) (idle *application*)))))))))

(defun resize-window (w start-event)
  "Resize window W by dragging its bottom-right grip until release."
  (declare (ignore start-event))
  (loop
    (draw-view *application*)
    (when *screen* (flush-screen *screen*))
    (let ((e (get-event *application*)))
      (cond
        ((member (event-type e) (list +ev-mouse-move+ +ev-mouse-auto+))
         (multiple-value-bind (gx gy) (view-global-origin w)
           (grow-to w (max 8 (+ 1 (- (point-x (event-mouse-where e)) gx)))
                    (max 3 (+ 1 (- (point-y (event-mouse-where e)) gy))))))
        ((or (= (event-type e) +ev-mouse-up+) (= (event-type e) +ev-key-down+))
         (return))
        ((= (event-type e) +ev-nothing+) (idle *application*))))))

(defun move-size-window (w)
  "Interactive keyboard move/size: arrows move, Shift+arrows resize, Enter/Esc end."
  (loop
    (draw-view *application*)
    (when *screen* (flush-screen *screen*))
    (let ((e (get-event *application*)))
      (cond
        ((= (event-type e) +ev-key-down+)
         (let* ((k (event-key-code e))
                (resize (logtest (event-modifiers e) +md-shift+))
                (o (view-origin w)) (sz (view-size w))
                (ox (point-x o)) (oy (point-y o))
                (sw (point-x sz)) (sh (point-y sz)))
           (cond
             ((or (= k +kb-esc+) (= k +kb-enter+)) (return))
             ((= k +kb-left+)  (if resize (grow-to w (max 8 (1- sw)) sh) (move-to w (max 0 (1- ox)) oy)))
             ((= k +kb-right+) (if resize (grow-to w (1+ sw) sh) (move-to w (1+ ox) oy)))
             ((= k +kb-up+)    (if resize (grow-to w sw (max 3 (1- sh))) (move-to w ox (max 0 (1- oy)))))
             ((= k +kb-down+)  (if resize (grow-to w sw (1+ sh)) (move-to w ox (1+ oy)))))))
        ((= (event-type e) +ev-nothing+) (idle *application*))))))

;;; ---------------------------------------------------------------------------
;;; TApplication -- a TProgram with the conventional defaults.
;;; ---------------------------------------------------------------------------

(defclass tapplication (tprogram) ())
