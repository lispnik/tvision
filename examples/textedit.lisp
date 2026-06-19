;;;; textedit.lisp --- A multi-window text editor built on the Turbo Vision port.
;;;;
;;;; Classic features: New/Open/Save/Save-As/Close with "save changes?" prompts,
;;;; Undo/Redo, Cut/Copy/Paste/Select-All, Find/Find-Next/Replace/Goto-Line,
;;;; insert-overwrite toggle, a line:col indicator, multiple windows, tiling.

(defpackage #:tvision-textedit
  (:use #:common-lisp #:tvision)
  (:export #:main #:toplevel #:edit-file))

(in-package #:tvision-textedit)

;; sb-introspect powers go-to-definition; load at compile time so the package
;; exists when this file is read.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :sb-introspect))

;;; --- commands --------------------------------------------------------------

(defparameter +cm-new+      200)
(defparameter +cm-open+     201)
(defparameter +cm-save+     202)
(defparameter +cm-saveas+   203)
(defparameter +cm-undo+     210)
(defparameter +cm-redo+     211)
(defparameter +cm-selall+   212)
(defparameter +cm-find+     220)
(defparameter +cm-findnext+ 221)
(defparameter +cm-replace+  222)
(defparameter +cm-goto+     223)
(defparameter +cm-isearch+  224)
(defparameter +cm-wrap+        225)
(defparameter +cm-macroexpand+ 226)
(defparameter +cm-describe+    227)
(defparameter +cm-gotodef+     228)
(defparameter +cm-tile+     230)
(defparameter +cm-cascade+  231)

(defparameter +hc-edit+ 2000)

;;; --- application + editor window -------------------------------------------

(defclass editor-app (tapplication)
  ((win-count    :initform 0   :accessor win-count)
   (last-search  :initform nil :accessor last-search)
   (last-replace :initform nil :accessor last-replace)
   (opt-case     :initform nil :accessor opt-case)     ; case sensitive
   (opt-word     :initform nil :accessor opt-word)     ; whole word
   (opt-back     :initform nil :accessor opt-back)))   ; search backward

(defclass editor-window (twindow)
  ((editor   :accessor ew-editor)
   (filename :initarg :filename :initform nil :accessor ew-filename)))

(defun ew-title (w)
  (if (ew-filename w)
      (file-namestring (ew-filename w))
      (format nil "Untitled~@[ ~a~]" (and (plusp (window-number w)) (window-number w)))))

(defun update-title (w)
  (setf (window-title w) (ew-title w)))

(defmethod tvision::application-menu ((app editor-app))
  (new-menu
   (sub-menu "~F~ile"
     (new-menu
      (menu-item "~N~ew"        +cm-new+    :key-code +kb-f4+ :key-text "F4")
      (menu-item "~O~pen..."    +cm-open+   :key-code +kb-f3+ :key-text "F3")
      (menu-item "~S~ave"       +cm-save+   :key-code +kb-f2+ :key-text "F2")
      (menu-item "Save ~a~s..." +cm-saveas+)
      (menu-separator)
      (menu-item "~C~lose"      +cm-close+  :key-code +kb-ctrl-w+ :key-text "Ctrl-W")
      (menu-item "E~x~it"       +cm-quit+   :key-code +kb-alt-x+ :key-text "Alt-X")))
   (sub-menu "~E~dit"
     (new-menu
      (menu-item "~U~ndo"       +cm-undo+   :key-text "Ctrl-Z")
      (menu-item "~R~edo"       +cm-redo+   :key-text "Ctrl-Y")
      (menu-separator)
      (menu-item "Cu~t~"        +cm-cut+    :key-text "Ctrl-X")
      (menu-item "~C~opy"       +cm-copy+   :key-text "Ctrl-C")
      (menu-item "~P~aste"      +cm-paste+  :key-text "Ctrl-V")
      (menu-item "Select ~A~ll" +cm-selall+ :key-text "Ctrl-A")))
   (sub-menu "~S~earch"
     (new-menu
      (menu-item "~F~ind..."     +cm-find+     :key-code +kb-f7+ :key-text "F7")
      (menu-item "~I~ncremental" +cm-isearch+  :key-code 6 :key-text "Ctrl-F")
      (menu-item "Find ~N~ext"   +cm-findnext+ :key-code +kb-f5+ :key-text "F5")
      (menu-item "~R~eplace..."  +cm-replace+  :key-code +kb-f8+ :key-text "F8")
      (menu-item "~G~oto line..." +cm-goto+    :key-code +kb-f6+ :key-text "F6")))
   (sub-menu "~L~isp"
     (new-menu
      (menu-item "~M~acroexpand"      +cm-macroexpand+)
      (menu-item "~D~escribe"         +cm-describe+)
      (menu-item "~G~o to definition" +cm-gotodef+ :key-text "Alt-.")))
   (sub-menu "~O~ptions"
     (new-menu
      (menu-item "~W~ord wrap" +cm-wrap+)))
   (sub-menu "~W~indow"
     (new-menu
      (menu-item "~N~ext"     +cm-next+    :key-code +kb-f9+ :key-text "F9")
      (menu-item "~T~ile"     +cm-tile+)
      (menu-item "~C~ascade"  +cm-cascade+)))))

(defmethod tvision::status-line-items ((app editor-app))
  (list (make-status-item "~F2~ Save" +kb-f2+ +cm-save+)
        (make-status-item "~F3~ Open" +kb-f3+ +cm-open+)
        (make-status-item "~F7~ Find" +kb-f7+ +cm-find+)
        (make-status-item "~F10~ Menu" +kb-f10+ 0)
        (make-status-item "~Alt-X~ Exit" +kb-alt-x+ +cm-quit+)))

;;; --- creating editor windows -----------------------------------------------

;;; The editor: a TFileEditor (the framework's windowed-editor class) that also
;;; pops up a context menu on right-click.
(defclass edit-view (tfile-editor) ())

(defun edit-context-menu ()
  (new-menu
   (menu-item "Cu~t~"        +cm-cut+)
   (menu-item "~C~opy"       +cm-copy+)
   (menu-item "~P~aste"      +cm-paste+)
   (menu-separator)
   (menu-item "Select ~A~ll" +cm-selall+)))

(defmethod handle-event ((v edit-view) event)
  (if (and (= (event-type event) +ev-mouse-down+)
           (logtest (event-mouse-buttons event) +mb-right+)
           (mouse-in-view-p v event))
      (let ((p (event-mouse-where event)))
        (popup-menu (edit-context-menu) (point-x p) (point-y p))
        (clear-event event))
      (call-next-method)))

(defun make-editor-window (app &optional path)
  (let* ((desk (program-desktop app))
         (n (incf (win-count app)))
         (dw (point-x (view-size desk))) (dh (point-y (view-size desk)))
         (ox (min (* (mod (1- n) 6) 2) (max 0 (- dw 50))))
         (oy (min (mod (1- n) 6) (max 0 (- dh 18))))
         (w (make-instance 'editor-window :filename path :number n
                           :bounds (make-trect ox oy (min dw (+ ox 70)) (min dh (+ oy 20)))))
         (vsb (standard-scrollbar w t))
         (ed (make-instance 'edit-view
                            :bounds (make-trect 1 1
                                                (1- (point-x (view-size w)))
                                                (1- (point-y (view-size w))))))
         (ind (make-instance 'tindicator :source ed)))
    (insert w ed)
    (text-attach-scrollbars ed :vscroll vsb)
    (setf (ew-editor w) ed)
    (set-bounds ind (make-trect 2 (1- (point-y (view-size w))) 22 (point-y (view-size w))))
    (insert w ind)
    (when (and path (probe-file path))
      (text-load-file ed path))
    (update-title w)
    (setf (view-help-ctx w) +hc-edit+)
    (insert desk w)
    (focus ed)
    w))

(defun current-window (app)
  (let ((w (group-current (program-desktop app))))
    (and (typep w 'editor-window) w)))

(defun current-editor (app)
  (let ((w (current-window app))) (and w (ew-editor w))))

;;; --- file actions ----------------------------------------------------------

(defun ed-do-save (w)
  "Save W; returns T if it ended up saved (not cancelled)."
  (if (ew-filename w)
      (progn (text-save-file (ew-editor w) (ew-filename w)) (update-title w) t)
      (ed-do-saveas w)))

(defun ed-do-saveas (w)
  (let ((path (file-save-dialog)))
    (when path
      (setf (ew-filename w) path)
      (text-save-file (ew-editor w) path)
      (update-title w)
      t)))

;;; Prompt to save before destroying a modified window.
(defmethod valid-p ((w editor-window) command)
  (if (and (= command +cm-close+) (ew-editor w) (text-modified (ew-editor w)))
      (let ((r (message-box (format nil "Save changes to ~a?" (ew-title w))
                            (logior +mf-confirmation+ +mf-yes-button+
                                    +mf-no-button+ +mf-cancel-button+))))
        (cond ((= r +cm-yes+) (ed-do-save w) (not (text-modified (ew-editor w))))
              ((= r +cm-no+) t)
              (t nil)))
      (call-next-method)))

;;; --- search actions --------------------------------------------------------

(defun %center (d)
  (let ((desk (program-desktop *application*)))
    (move-to d (max 0 (floor (- (point-x (view-size desk)) (point-x (view-size d))) 2))
             (max 0 (floor (- (point-y (view-size desk)) (point-y (view-size d))) 2)))))

(defun %search-options-cluster (w x y labels)
  (let ((c (make-instance 'tcheck-boxes :labels labels)))
    (set-bounds c (make-trect x y (- w 3) (+ y (length labels))))
    c))

(defun find-dialog (title initial &key replace)
  "Show a Find (or Replace) dialog.  Return
 (values ok-p find-text replace-text case-p word-p back-or-all-p)."
  (let* ((w 50) (h (if replace 13 12))
         (d (make-instance 'tdialog :title title :bounds (make-trect 0 0 w h)))
         (find-in (make-instance 'tinputline :data initial :maxlen 100
                                 :bounds (make-trect 12 2 (- w 3) 3)))
         (repl-in (when replace
                    (make-instance 'tinputline :data (or (last-replace *application*) "") :maxlen 100
                                   :bounds (make-trect 12 4 (- w 3) 5))))
         (opts (%search-options-cluster
                w 3 (if replace 6 4)
                (if replace
                    '("~C~ase sensitive" "~W~hole word" "Replace ~a~ll (no prompt)")
                    '("~C~ase sensitive" "~W~hole word" "~B~ackward")))))
    (flet ((lbl (text y link)
             (let ((l (make-instance 'tlabel :text text :link link)))
               (set-bounds l (make-trect 3 y (+ 3 (length text)) (1+ y)))
               (insert d l))))
      (lbl "Find:" 2 find-in) (insert d find-in)
      (when replace (lbl "Replace:" 4 repl-in) (insert d repl-in)))
    (insert d opts)
    (insert d (make-button (make-trect (- w 26) (- h 3) (- w 16) (- h 1)) "~O~K" +cm-ok+ t))
    (insert d (make-button (make-trect (- w 13) (- h 3) (- w 3) (- h 1)) "Cancel" +cm-cancel+))
    (%center d)
    (focus find-in)
    (let ((cmd (exec-view (program-desktop *application*) d)))
      (if (= cmd +cm-ok+)
          (let ((v (cluster-value opts)))
            (values t (get-data find-in) (and repl-in (get-data repl-in))
                    (logbitp 0 v) (logbitp 1 v) (logbitp 2 v)))
          (values nil nil nil nil nil nil)))))

(defun do-search (app)
  (let ((ed (current-editor app)) (s (last-search app)))
    (when (and ed s)
      (unless (text-find-and-select ed s :case-sensitive (opt-case app)
                                    :whole-word (opt-word app)
                                    :backward (opt-back app) :wrap t)
        (message-box "Search string not found." (logior +mf-information+ +mf-ok-button+))))))

(defun ed-find (app)
  (when (current-editor app)
    (multiple-value-bind (ok text rep case word back)
        (find-dialog "Find" (or (last-search app) ""))
      (declare (ignore rep))
      (when (and ok (plusp (length text)))
        (setf (last-search app) text (opt-case app) case
              (opt-word app) word (opt-back app) back)
        (do-search app)))))

(defun ed-find-next (app)
  (if (last-search app) (do-search app) (ed-find app)))

(defun interactive-replace (app ed find repl)
  (let ((count 0) (case (opt-case app)) (word (opt-word app)))
    (block done
      (loop
        (let ((m (text-find ed find :case-sensitive case :whole-word word)))
          (unless m (return))
          (text-select-match ed m find)
          (draw-view app) (when tvision:*screen* (flush-screen tvision:*screen*))
          (let ((r (message-box "Replace this occurrence?"
                                (logior +mf-confirmation+ +mf-yes-button+
                                        +mf-no-button+ +mf-cancel-button+))))
            (cond
              ((= r +cm-yes+) (text-replace-selection ed repl) (incf count))
              ((= r +cm-no+)
               (setf (text-anchor ed) nil
                     (text-cur-line ed) (car m)
                     (text-cur-col ed) (+ (cdr m) (length find))))
              (t (return-from done)))))))
    (message-box (format nil "~d replacement~:p made." count)
                 (logior +mf-information+ +mf-ok-button+))))

(defun ed-replace (app)
  (when (current-editor app)
    (multiple-value-bind (ok find repl case word all)
        (find-dialog "Replace" (or (last-search app) "") :replace t)
      (when (and ok (plusp (length find)))
        (setf (last-search app) find (last-replace app) repl
              (opt-case app) case (opt-word app) word)
        (let ((ed (current-editor app)))
          (if all
              (let ((n (text-replace-all ed find repl :case-sensitive case :whole-word word)))
                (message-box (format nil "~d replacement~:p made." n)
                             (logior +mf-information+ +mf-ok-button+)))
              (interactive-replace app ed find repl)))))))

(defun ed-goto (app)
  (let ((ed (current-editor app)))
    (when ed
      (multiple-value-bind (cmd s) (input-box "Goto Line" "Line number:" "")
        (when (= cmd +cm-ok+)
          (let ((n (parse-integer s :junk-allowed t)))
            (when n (text-goto ed n 0))))))))

(defun ed-isearch (app)
  "Emacs-style incremental search: type to jump, Down for next, Esc cancels."
  (let* ((w (current-window app)) (ed (and w (ew-editor w))))
    (when ed
      (let ((query (make-array 0 :element-type 'character :adjustable t :fill-pointer 0))
            (from (cons (text-cur-line ed) (text-cur-col ed)))
            (found t))
        (flet ((prompt ()
                 (setf (window-title w)
                       (format nil "~:[(not found) ~;~]I-search: ~a"
                               found (coerce query 'string))))
               (jump ()
                 (let ((q (coerce query 'string)))
                   (if (zerop (length q))
                       (setf found t)
                       (let ((m (text-find ed q :from-line (car from) :from-col (cdr from))))
                         (if m (progn (text-select-match ed m q) (setf found t))
                             (setf found nil)))))))
          (prompt)
          (block search
            (loop
              (draw-view app) (when tvision:*screen* (flush-screen tvision:*screen*))
              (let ((e (get-event app)))
                (when (= (event-type e) +ev-key-down+)
                  (let ((k (event-key-code e)) (ch (event-char-code e)))
                    (cond
                      ((= k +kb-esc+)
                       (setf (text-cur-line ed) (car from) (text-cur-col ed) (cdr from)
                             (text-anchor ed) nil)
                       (return-from search))
                      ((= k +kb-enter+) (return-from search))
                      ((= k +kb-back+)
                       (when (plusp (fill-pointer query)) (decf (fill-pointer query)))
                       (jump) (prompt))
                      ((= k +kb-down+)            ; find next occurrence
                       (setf from (cons (text-cur-line ed) (text-cur-col ed)))
                       (jump) (prompt))
                      ((and (>= ch 32) (< ch 127))
                       (vector-push-extend (code-char ch) query)
                       (jump) (prompt))))))))
          (update-title w))))))

;;; --- word wrap + Lisp tools ------------------------------------------------

(defun ed-toggle-wrap (ed)
  (set-text-wrap ed (not (text-wrap ed))))

(defun %sym-char-p (ch) (or (alphanumericp ch) (find ch "+-*/<>=!?._%&$~^@:[]{}")))

(defun symbol-at-point (ed)
  "The symbol token surrounding the cursor on the current line, or NIL."
  (let* ((line (current-line-string ed))
         (col (min (text-cur-col ed) (length line)))
         (start col) (end col))
    (loop while (and (> start 0) (%sym-char-p (char line (1- start)))) do (decf start))
    (loop while (and (< end (length line)) (%sym-char-p (char line end))) do (incf end))
    (when (< start end) (subseq line start end))))

(defun form-at-point (ed)
  "The selection, or the current line, as text to read as a form."
  (or (selected-string ed) (current-line-string ed)))

(defun %read-cl (s) (let ((*package* (find-package :cl-user))) (read-from-string s)))

(defun ed-describe (ed)
  (let ((name (symbol-at-point ed)))
    (if (null name)
        (message-box "No symbol at the cursor." (logior +mf-information+ +mf-ok-button+))
        (handler-case
            (show-text-window (format nil "Describe ~a" name)
                              (with-output-to-string (s) (describe (%read-cl name) s)))
          (error (e) (message-box (format nil "~a" e) (logior +mf-error+ +mf-ok-button+)))))))

(defun ed-macroexpand (ed)
  (let ((txt (form-at-point ed)))
    (if (or (null txt) (zerop (length (string-trim '(#\Space #\Tab) txt))))
        (message-box "Nothing to expand (select a form or place the cursor on a line)."
                     (logior +mf-information+ +mf-ok-button+))
        (handler-case
            (let ((*print-pretty* t))
              (show-text-window "Macroexpand-1" (prin1-to-string (macroexpand-1 (%read-cl txt)))))
          (error (e) (message-box (format nil "~a" e) (logior +mf-error+ +mf-ok-button+)))))))

(defun %off-to-line (path off)
  (or (ignore-errors
       (with-open-file (s path)
         (let ((ln 1))
           (dotimes (i (or off 0) ln)
             (let ((c (read-char s nil nil)))
               (unless c (return ln))
               (when (char= c #\Newline) (incf ln)))))))
      1))

(defun ed-gotodef (app ed)
  (let ((name (symbol-at-point ed)))
    (when name
      (handler-case
          (let* ((sym (%read-cl name))
                 (src (loop for ty in '(:function :macro :generic-function
                                        :variable :class :structure :type)
                            thereis (ignore-errors
                                     (first (sb-introspect:find-definition-sources-by-name sym ty)))))
                 (path (and src (sb-introspect:definition-source-pathname src))))
            (if (and path (probe-file path))
                (let ((w (make-editor-window app (namestring path))))
                  (text-goto (ew-editor w)
                             (%off-to-line path (sb-introspect:definition-source-character-offset src))))
                (message-box (format nil "No source location for ~a." name)
                             (logior +mf-information+ +mf-ok-button+))))
        (error (e) (message-box (format nil "~a" e) (logior +mf-error+ +mf-ok-button+)))))))

;;; --- exit handling ---------------------------------------------------------

(defun can-quit-p (app)
  "Try to close every modified window's prompt; abort if the user cancels."
  (every (lambda (w) (valid-p w +cm-close+))
         (remove-if-not (lambda (w) (typep w 'editor-window))
                        (desktop-windows (program-desktop app)))))

;;; --- command dispatch ------------------------------------------------------

(defmethod handle-event ((app editor-app) event)
  ;; intercept Quit so we can prompt for unsaved changes
  (when (and (= (event-type event) +ev-command+)
             (= (event-command event) +cm-quit+))
    (unless (can-quit-p app)
      (clear-event event)
      (return-from handle-event)))
  ;; M-. -> go to definition of the symbol at the cursor
  (when (and (= (event-type event) +ev-key-down+)
             (logtest (event-modifiers event) +md-alt+)
             (= (event-char-code event) 46))
    (let ((ed (current-editor app)))
      (when ed (ed-gotodef app ed) (clear-event event))))
  (call-next-method)
  (when (= (event-type event) +ev-command+)
    (let ((c (event-command event))
          (ed (current-editor app))
          (w (current-window app)))
      (flet ((with-ed (fn) (when ed (funcall fn ed) (draw-view ed))))
        (cond
          ((= c +cm-new+)      (make-editor-window app) (clear-event event))
          ((= c +cm-open+)
           (let ((path (file-open-dialog))) (when path (make-editor-window app path)))
           (clear-event event))
          ((= c +cm-save+)     (when w (ed-do-save w)) (clear-event event))
          ((= c +cm-saveas+)   (when w (ed-do-saveas w)) (clear-event event))
          ((= c +cm-undo+)     (with-ed #'text-undo!) (clear-event event))
          ((= c +cm-redo+)     (with-ed #'text-redo!) (clear-event event))
          ((= c +cm-cut+)      (with-ed #'cut-selection) (clear-event event))
          ((= c +cm-copy+)     (with-ed #'copy-selection) (clear-event event))
          ((= c +cm-paste+)    (with-ed #'paste-clipboard) (clear-event event))
          ((= c +cm-selall+)
           (when ed (setf (text-anchor ed) (cons 0 0)
                          (text-cur-line ed) (1- (line-count ed))
                          (text-cur-col ed) (length (nth-line ed (1- (line-count ed)))))
             (draw-view ed))
           (clear-event event))
          ((= c +cm-find+)     (ed-find app) (clear-event event))
          ((= c +cm-findnext+) (ed-find-next app) (clear-event event))
          ((= c +cm-replace+)  (ed-replace app) (clear-event event))
          ((= c +cm-goto+)     (ed-goto app) (clear-event event))
          ((= c +cm-isearch+)  (ed-isearch app) (clear-event event))
          ((= c +cm-wrap+)        (when ed (ed-toggle-wrap ed)) (clear-event event))
          ((= c +cm-macroexpand+) (when ed (ed-macroexpand ed)) (clear-event event))
          ((= c +cm-describe+)    (when ed (ed-describe ed)) (clear-event event))
          ((= c +cm-gotodef+)     (when ed (ed-gotodef app ed)) (clear-event event))
          ((= c +cm-tile+)     (tile (program-desktop app)) (clear-event event))
          ((= c +cm-cascade+)  (cascade (program-desktop app)) (clear-event event)))))))

;;; --- entry points ----------------------------------------------------------

(defun register-edit-help ()
  (register-help +hc-edit+
                 (format nil "Text editor~%~%~
Type to edit; Insert toggles overwrite (INS/OVR in the corner).~%~
Arrows move; Ctrl+Left/Right by word; Shift+arrows select.~%~
Ctrl-Z undo, Ctrl-Y redo, Ctrl-C/X/V clipboard, Ctrl-A select all.~%~
F7 find (Case sensitive / Whole word / Backward options), F5 find-next,~%~
F8 replace (interactive or replace-all), F6 goto line.~%~
F2 save, F3 open, F9 next window, F10 menu, Alt-X exit.")))

(defvar *startup-files* nil)

(defmethod tvision::setup ((app editor-app))
  (register-edit-help)
  (setf (view-help-ctx (program-desktop app)) +hc-edit+)
  (if *startup-files*
      (dolist (f *startup-files*) (make-editor-window app f))
      (make-editor-window app)))

(defun main ()
  (run 'editor-app))

(defun edit-file (&rest files)
  "Launch the editor opening the given FILES."
  (let ((*startup-files* files))
    (run 'editor-app)))

(defun toplevel ()
  (handler-case
      (let ((*startup-files* (cdr sb-ext:*posix-argv*)))
        (run 'editor-app))
    (error (e)
      (format *error-output* "~&Error: ~a~%" e)
      (sb-ext:exit :code 1)))
  (sb-ext:exit :code 0))
