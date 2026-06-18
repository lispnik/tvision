;;;; tvlisp.lisp --- A standalone Lisp REPL application built on Turbo Vision.
;;;;
;;;; A focused REPL host: opens a full-window Lisp read-eval-print loop with
;;;; menus for managing REPL windows, clipboard, and window layout.

(defpackage #:tvision-tvlisp
  (:use #:common-lisp #:tvision)
  (:export #:main #:toplevel))

(in-package #:tvision-tvlisp)

(defparameter +cm-new-repl+ 300)
(defparameter +cm-clear+    301)
(defparameter +cm-tile+     302)
(defparameter +cm-cascade+  303)
(defparameter +cm-inspect+  304)
(defparameter +cm-load+     305)
(defparameter +cm-savetx+   306)
(defparameter +cm-interrupt+ 307)

(defparameter +hc-repl+ 1)

(defparameter +history-file+
  (merge-pathnames ".tvlisp_history" (user-homedir-pathname)))

(defclass tvlisp-app (tapplication)
  ((repl-count :initform 0 :accessor repl-count)))

;;; --- menu / status ---------------------------------------------------------

(defmethod tvision::application-menu ((app tvlisp-app))
  (new-menu
   (sub-menu "~F~ile"
     (new-menu
      (menu-item "~N~ew REPL"      +cm-new-repl+ :key-code +kb-f2+ :key-text "F2")
      (menu-item "~C~lear"         +cm-clear+    :key-code +kb-f3+ :key-text "F3")
      (menu-separator)
      (menu-item "~L~oad file..."  +cm-load+     :key-code +kb-f7+ :key-text "F7")
      (menu-item "Save ~t~ranscript..." +cm-savetx+)
      (menu-separator)
      (menu-item "E~x~it"          +cm-quit+     :key-code +kb-alt-x+ :key-text "Alt-X")))
   (sub-menu "~E~dit"
     (new-menu
      (menu-item "Cu~t~"        +cm-cut+   :key-text "Ctrl-X")
      (menu-item "~C~opy"       +cm-copy+  :key-text "Ctrl-C")
      (menu-item "~P~aste"      +cm-paste+ :key-text "Ctrl-V")
      (menu-separator)
      (menu-item "~I~nspect *"     +cm-inspect+   :key-code +kb-f8+ :key-text "F8")
      (menu-item "I~n~terrupt eval" +cm-interrupt+ :key-text "Ctrl-C")))
   (sub-menu "~W~indow"
     (new-menu
      (menu-item "~N~ext"    +cm-next+    :key-code +kb-f6+ :key-text "F6")
      (menu-item "~T~ile"    +cm-tile+    :key-code +kb-f4+ :key-text "F4")
      (menu-item "~C~ascade" +cm-cascade+ :key-code +kb-f5+ :key-text "F5")))))

(defmethod tvision::status-line-items ((app tvlisp-app))
  (list (make-status-item "~Alt-X~ Exit" +kb-alt-x+ +cm-quit+)
        (make-status-item "~F1~ Help"    +kb-f1+ 0)
        (make-status-item "~F2~ New REPL" +kb-f2+ +cm-new-repl+)
        (make-status-item "~F3~ Clear"   +kb-f3+ +cm-clear+)
        (make-status-item "~F10~ Menu"   +kb-f10+ 0)))

;;; --- REPL windows ----------------------------------------------------------

(defun open-repl-window (app &key maximized)
  (let* ((desk (program-desktop app))
         (n (incf (repl-count app)))
         (dw (point-x (view-size desk))) (dh (point-y (view-size desk)))
         (bounds (if maximized
                     (make-trect 0 0 dw dh)
                     (let ((ox (* (mod (1- n) 5) 3)) (oy (mod (1- n) 5)))
                       (make-trect ox oy (min dw (+ ox 72)) (min dh (+ oy 22)))))))
    (multiple-value-bind (w rv)
        (make-repl-window bounds :title (format nil "Lisp REPL ~d" n)
                                 :history-file +history-file+)
      (setf (view-help-ctx w) +hc-repl+)
      (insert desk w)
      (focus rv)
      rv)))

(defun current-repl (app)
  (let ((w (group-current (program-desktop app))))
    (when (typep w 'twindow)
      (find-if (lambda (v) (typep v 'trepl-view)) (group-subviews w)))))

;;; --- command dispatch ------------------------------------------------------

(defparameter +kb-ctrl-c+ 3)

(defmethod handle-event ((app tvlisp-app) event)
  ;; Ctrl-C interrupts the running evaluation -- map it to the command before
  ;; the text view can swallow it.
  (when (and (= (event-type event) +ev-key-down+)
             (= (event-key-code event) +kb-ctrl-c+))
    (let ((rv (current-repl app)))
      (when (and rv (repl-busy rv))
        (repl-interrupt rv)
        (clear-event event))))
  (call-next-method)
  (when (= (event-type event) +ev-command+)
    (let ((c (event-command event))
          (rv (current-repl app)))
      (flet ((with-repl (fn) (when rv (funcall fn rv) (draw-view rv))))
        (cond
          ((= c +cm-new-repl+) (open-repl-window app) (clear-event event))
          ((= c +cm-clear+)    (with-repl #'repl-clear) (clear-event event))
          ((= c +cm-cut+)      (with-repl #'cut-selection) (clear-event event))
          ((= c +cm-copy+)     (with-repl #'copy-selection) (clear-event event))
          ((= c +cm-paste+)    (with-repl #'paste-clipboard) (clear-event event))
          ((= c +cm-inspect+)
           (when rv (repl-inspect (symbol-value (intern "*" (repl-package rv))) "*"))
           (clear-event event))
          ((= c +cm-interrupt+)
           (when rv (repl-interrupt rv)) (clear-event event))
          ((= c +cm-load+)
           (let ((path (file-open-dialog :title "Load Lisp file")))
             (when (and rv path) (repl-load-file rv path) (focus rv)))
           (clear-event event))
          ((= c +cm-savetx+)
           (let ((path (file-save-dialog :title "Save transcript")))
             (when (and rv path) (text-save-file rv path)))
           (clear-event event))
          ((= c +cm-tile+)     (tile (program-desktop app)) (clear-event event))
          ((= c +cm-cascade+)  (cascade (program-desktop app)) (clear-event event)))))))

;;; --- entry points ----------------------------------------------------------

(defun register-tvlisp-help ()
  (register-help +hc-repl+
                 (format nil "tvlisp -- a Turbo Vision Lisp REPL~%~%~
Type a Lisp form and press Enter to evaluate it.~%~
An open form (unbalanced parens) continues on the next line.~%~
Up/Down recall previous input; *, **, *** hold recent values.~%~
Output is read-only; only text after the prompt is editable.~%%~
F2 new REPL, F3 clear, F4 tile, F5 cascade, F6 next window.~%~
Ctrl-X/C/V cut/copy/paste, Ctrl-Z undo, F10 menu, Alt-X exit.")))

(defmethod tvision::setup ((app tvlisp-app))
  (register-tvlisp-help)
  (setf (view-help-ctx (program-desktop app)) +hc-repl+)
  (open-repl-window app :maximized t))

(defun main ()
  "Launch the tvlisp REPL application."
  (run 'tvlisp-app))

(defun toplevel ()
  (handler-case (main)
    (error (e)
      (format *error-output* "~&Error: ~a~%" e)
      (sb-ext:exit :code 1)))
  (sb-ext:exit :code 0))
