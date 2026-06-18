;;;; events.lisp --- Event records, key codes and command identifiers.

(in-package #:tvision)

;;; ---------------------------------------------------------------------------
;;; Event classes (bit masks, so they can be combined for matching).
;;; ---------------------------------------------------------------------------

(defconstant +ev-nothing+     #x0000)
(defconstant +ev-mouse-down+  #x0001)
(defconstant +ev-mouse-up+    #x0002)
(defconstant +ev-mouse-move+  #x0004)
(defconstant +ev-mouse-auto+  #x0008)
(defconstant +ev-key-down+    #x0010)
(defconstant +ev-mouse-wheel+ #x0020)
(defconstant +ev-command+     #x0100)
(defconstant +ev-broadcast+   #x0200)

(defconstant +ev-mouse+    #x002f)   ; all mouse events (down/up/move/auto/wheel)
(defconstant +ev-keyboard+ #x0010)
(defconstant +ev-message+  #xff00)

(defconstant +mb-left+  #x01)
(defconstant +mb-right+ #x02)

;; keyboard / mouse modifier flags (event-modifiers)
(defconstant +md-shift+ #x01)
(defconstant +md-ctrl+  #x02)
(defconstant +md-alt+   #x04)

;; mouse wheel directions (event-wheel)
(defconstant +mw-up+   -1)
(defconstant +mw-down+ +1)

;;; ---------------------------------------------------------------------------
;;; Key codes.  Printable keys carry their character in CHAR-CODE; special keys
;;; use the KEY-CODE constants below.  Values are chosen to be distinct and to
;;; loosely echo the original BIOS scan-code words.
;;; ---------------------------------------------------------------------------

(defconstant +kb-esc+       #x001b)
(defconstant +kb-enter+     #x000d)
(defconstant +kb-tab+       #x0009)
(defconstant +kb-back+      #x0008)
(defconstant +kb-space+     #x0020)
(defconstant +kb-shift-tab+ #x0f00)
(defconstant +kb-up+        #x4800)
(defconstant +kb-down+      #x5000)
(defconstant +kb-left+      #x4b00)
(defconstant +kb-right+     #x4d00)
(defconstant +kb-home+      #x4700)
(defconstant +kb-end+       #x4f00)
(defconstant +kb-pgup+      #x4900)
(defconstant +kb-pgdn+      #x5100)
(defconstant +kb-ins+       #x5200)
(defconstant +kb-del+       #x5300)
(defconstant +kb-f1+        #x3b00)
(defconstant +kb-f2+        #x3c00)
(defconstant +kb-f3+        #x3d00)
(defconstant +kb-f4+        #x3e00)
(defconstant +kb-f5+        #x3f00)
(defconstant +kb-f6+        #x4000)
(defconstant +kb-f7+        #x4100)
(defconstant +kb-f8+        #x4200)
(defconstant +kb-f9+        #x4300)
(defconstant +kb-f10+       #x4400)
(defconstant +kb-ctrl-w+    #x0017)  ; Ctrl-W
(defconstant +kb-alt-x+     #x2d00)

;;; ---------------------------------------------------------------------------
;;; Standard command codes.
;;; ---------------------------------------------------------------------------

(defconstant +cm-quit+   1)
(defconstant +cm-close+  2)
(defconstant +cm-zoom+   3)
(defconstant +cm-next+   4)
(defconstant +cm-prev+   5)
(defconstant +cm-resize+ 6)   ; enter interactive move/size mode
(defconstant +cm-cut+    20)
(defconstant +cm-copy+   21)
(defconstant +cm-paste+  22)

(defconstant +cm-ok+      10)
(defconstant +cm-cancel+  11)
(defconstant +cm-yes+     12)
(defconstant +cm-no+      13)
(defconstant +cm-default+ 14)

;; broadcast / internal commands
(defconstant +cm-valid+              50)
(defconstant +cm-released+           51)
(defconstant +cm-receivedfocus+      52)
(defconstant +cm-menu+               53)
(defconstant +cm-command-set-changed+ 54)

;;; ---------------------------------------------------------------------------
;;; Window flags (defined here so TFrame, which precedes TWindow, can see them).
;;; ---------------------------------------------------------------------------

(defconstant +wf-move+  #x01)
(defconstant +wf-grow+  #x02)
(defconstant +wf-close+ #x04)
(defconstant +wf-zoom+  #x08)
(defconstant +wn-no-number+ 0)

;;; ---------------------------------------------------------------------------
;;; Command set: the globally enabled/disabled commands.
;;;
;;; Views consult COMMAND-ENABLED-P before issuing or acting on a command, and
;;; menus/buttons/status items draw disabled commands greyed out.  Kept as a
;;; simple global set so every layer can reach it without a load-order tangle.
;;; ---------------------------------------------------------------------------

(defvar *disabled-commands* (make-hash-table :test 'eql)
  "Set of command codes that are currently disabled.")

(defvar *command-set-changed* nil
  "Set when the command set changes; the main loop broadcasts
+cm-command-set-changed+ and clears it (TV's commandSetChanged contract).")

(defun command-enabled-p (command)
  (not (gethash command *disabled-commands*)))

(defun disable-command (command)
  (unless (gethash command *disabled-commands*)
    (setf (gethash command *disabled-commands*) t *command-set-changed* t))
  (values))

(defun enable-command (command)
  (when (gethash command *disabled-commands*)
    (remhash command *disabled-commands*)
    (setf *command-set-changed* t))
  (values))

(defun disable-commands (commands) (mapc #'disable-command commands) (values))
(defun enable-commands (commands) (mapc #'enable-command commands) (values))

(defun set-command-enabled (command enabled)
  (if enabled (enable-command command) (disable-command command)))

(defun reset-commands ()
  (when (plusp (hash-table-count *disabled-commands*)) (setf *command-set-changed* t))
  (clrhash *disabled-commands*)
  (values))

;;; ---------------------------------------------------------------------------
;;; The event record.
;;; ---------------------------------------------------------------------------

(defstruct (event (:conc-name event-))
  (type +ev-nothing+ :type fixnum)
  ;; keyboard
  (key-code 0 :type fixnum)
  (char-code 0 :type fixnum)
  (modifiers 0 :type fixnum)        ; +md-shift+ / +md-ctrl+ / +md-alt+
  ;; mouse
  (mouse-where (make-tpoint) :type tpoint)
  (mouse-buttons 0 :type fixnum)
  (double nil)                      ; double-click
  (wheel 0 :type fixnum)            ; -1 up / +1 down for ev-mouse-wheel
  ;; message / command
  (command 0 :type fixnum)
  (info nil))

(defun clear-event (e)
  "Mark event E as fully handled (TV convention: set type to ev-nothing)."
  (setf (event-type e) +ev-nothing+
        (event-info e) nil)
  e)

(defun keyboard-event-p (e) (logtest (event-type e) +ev-keyboard+))
(defun mouse-event-p (e) (logtest (event-type e) +ev-mouse+))
(defun message-event-p (e) (logtest (event-type e) +ev-message+))
