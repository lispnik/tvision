;;;; repl.lisp --- TReplView: a Lisp read-eval-print loop in a text view.
;;;;
;;;; Built on TTextView.  Output and the current prompt are kept read-only via
;;;; the protected-region boundary; everything the user types after the last
;;;; prompt is the input.
;;;;
;;;; REPL services (completion, evaluation with restarts, object inspection) are
;;;; provided by a small in-process "backend" -- the same operation set Lem gets
;;;; from micros/swank, but called directly since the TUI *is* the Lisp image
;;;; (no socket).  The backend functions (REPL-BACKEND-* below) could be swapped
;;;; for a real micros connection without touching the view.

(in-package #:tvision)

(defvar *repl-debugger* t
  "When true, an error during REPL evaluation opens a restart menu (like the
SLIME/micros debugger); when nil, the error is just reported and aborted.")

(defvar *repl-async* t
  "When true (and a UI loop is running), each REPL evaluates on its own worker
thread so the UI never blocks and output streams in live.  When nil, evaluation
runs inline on the UI thread (used by headless tests).")

(defvar *repl-time* nil
  "When true, the REPL prints the wall-clock time each evaluation took.")

(defparameter +repl-hist-symbols+ '(* ** *** / // /// + ++ +++ -)
  "The CL REPL history variables, in shift order.  Each listener keeps its own
values (in the view) and binds these symbols with PROGV around evaluation, so
concurrent listeners never clobber one another's `*'/`+'/`/'.")

(defun ensure-repl-package ()
  (or (find-package :tv-repl-user)
      (make-package :tv-repl-user :use '(:common-lisp) :nicknames '("REPL"))))

;;; ===========================================================================
;;; Backend: introspection + evaluation (the "micros-equivalent" operations)
;;; ===========================================================================

(defun %symbol-char-p (ch)
  (or (alphanumericp ch) (find ch "+-*/<>=!?._%&$~^@:[]{}")))

(defun %prefixp (prefix string)
  (and (<= (length prefix) (length string))
       (string= prefix string :end2 (length prefix))))

(defun longest-common-prefix (strings)
  (if (null strings) ""
      (let ((p (first strings)))
        (dolist (s (rest strings) p)
          (let ((m (mismatch p s))) (when m (setf p (subseq p 0 m))))))))

(defun repl-backend-completions (token package)
  "Return sorted completion strings for TOKEN in PACKAGE (micros: simple-
completions).  Handles `pkg:name' / `pkg::name' qualified tokens."
  (let ((out '()) (colon (position #\: token)))
    (flet ((collect (sym name &optional prefix)
             (declare (ignore sym))
             (pushnew (if prefix (concatenate 'string prefix name) name)
                      out :test #'string=)))
      (if colon
          (let* ((pkgname (subseq token 0 colon))
                 (double (and (< (1+ colon) (length token))
                              (char= (char token (1+ colon)) #\:)))
                 (rest (string-downcase (subseq token (if double (+ colon 2) (1+ colon)))))
                 (sep (if double "::" ":"))
                 (pkg (find-package (string-upcase pkgname))))
            (when pkg
              (if double
                  (do-symbols (s pkg)
                    (when (and (eq (symbol-package s) pkg)
                               (%prefixp rest (string-downcase (symbol-name s))))
                      (collect s (string-downcase (symbol-name s))
                               (concatenate 'string pkgname sep))))
                  (do-external-symbols (s pkg)
                    (when (%prefixp rest (string-downcase (symbol-name s)))
                      (collect s (string-downcase (symbol-name s))
                               (concatenate 'string pkgname sep)))))))
          (let ((lc (string-downcase token)))
            (do-symbols (s package)
              (let ((n (string-downcase (symbol-name s))))
                (when (%prefixp lc n) (collect s n)))))))
    (sort (remove-duplicates out :test #'string=) #'string<)))

(defmacro with-repl-history ((hist new-hist) &body body)
  "Bind the CL history variables to the values in HIST (a list aligned with
+repl-hist-symbols+, or NIL for a fresh set) for the dynamic extent of BODY,
then capture their resulting values into NEW-HIST.  PROGV makes the binding
thread-local, so concurrent listeners never share `*'/`+'/`/'."
  `(progv +repl-hist-symbols+ (copy-list (or ,hist (make-list 10)))
     (multiple-value-prog1 (progn ,@body)
       (setf ,new-hist (mapcar #'symbol-value +repl-hist-symbols+)))))

(defun repl-backend-eval (input package error-handler &optional hist)
  "Read+eval all forms in INPUT under PACKAGE, capturing output.  Maintains the
standard history vars (-, +/++/+++, */**/***, ///) starting from HIST (the
listener's prior values).  ERROR-HANDLER is invoked with the condition inside
HANDLER-BIND (it must transfer control).  Return (values output-string results
package errored new-hist)."
  (let ((*package* package) (results '()) (errored nil) (last nil) (new-hist hist))
    (let ((output
            (with-output-to-string (out)
              (let ((*standard-output* out) (*error-output* out) (*trace-output* out))
                (with-repl-history (hist new-hist)
                  (restart-case
                      (handler-bind ((error (lambda (e) (setf last e)
                                              (funcall error-handler e))))
                        (with-input-from-string (in input)
                          (loop for form = (read in nil :repl-eof)
                                until (eq form :repl-eof)
                                do (setf - form)
                                   (let ((vals (multiple-value-list (eval form))))
                                     (push vals results)
                                     ;; shift the CL history variables
                                     (setf +++ ++  ++ +  + form
                                           /// //  // /  / vals
                                           *** **  ** *  * (first vals))))))
                    (repl-abort () (setf errored t))))
                (when (and errored last)
                  (format out "~&;; ~(~a~): ~a~%" (type-of last) last))))))
      (values output (nreverse results) *package* errored new-hist))))

;;; ===========================================================================
;;; The REPL view
;;; ===========================================================================

(defclass trepl-view (ttext-view)
  ((package      :initarg :package :initform nil :accessor repl-package)
   (history      :initform '() :accessor repl-history)      ; most-recent first
   (hist-pos     :initform nil :accessor repl-hist-pos)
   (history-file :initarg :history-file :initform nil :accessor repl-history-file)
   ;; per-listener CL history vars (*/+//-, aligned with +repl-hist-symbols+)
   (hist-vars    :initform (make-list 10) :accessor repl-hist-vars)
   ;; --- background evaluation (one worker thread per listener) ---
   (worker       :initform nil :accessor repl-worker)       ; sb-thread:thread
   (to-worker    :initform nil :accessor repl-to-worker)    ; mailbox of jobs
   (busy         :initform nil :accessor repl-busy)))        ; eval in flight?

(defmethod initialize-instance :after ((r trepl-view) &key)
  (unless (repl-package r) (setf (repl-package r) (ensure-repl-package)))
  (when (repl-history-file r) (load-repl-history r))
  (repl-print r (repl-banner r))
  (repl-fresh-prompt r)
  ;; Start the worker eagerly so the listener's thread exists (and shows up in
  ;; the thread monitor) before the first evaluation.  Headless/no-loop use
  ;; stays on the inline path.
  (when (and *repl-async* *ui-callbacks*) (repl-ensure-worker r)))

(defun repl-hvar (r sym)
  "Value of R's per-listener history variable SYM (one of +repl-hist-symbols+,
e.g. '*, '+, '/).  Reads listener-local storage, not the global CL specials."
  (let ((i (position sym +repl-hist-symbols+)))
    (and i (nth i (repl-hist-vars r)))))

(defun repl-banner (r)
  (declare (ignore r))
  (format nil "; Turbo Vision Lisp REPL on SBCL ~a~%~
; Enter evaluates; an open form continues on the next line.  Tab completes.~%~
; Up/Down recall history.  -, +, *, / (and ++/**, etc.) hold recent forms/values.~%~%"
          (lisp-implementation-version)))

(defun repl-clear (r)
  "Clear the transcript and start a fresh banner + prompt."
  (set-text r "")
  (repl-print r (repl-banner r))
  (repl-fresh-prompt r))

(defun repl-prompt-string (r)
  (format nil "~a> " (or (first (package-nicknames (repl-package r)))
                         (package-name (repl-package r)))))

(defun repl-print (r string) (append-text r string))

(defun repl-last-line-empty-p (r)
  (zerop (length (nth-line r (1- (line-count r))))))

(defun repl-ensure-fresh-line (r)
  (unless (repl-last-line-empty-p r) (append-text r (string #\Newline))))

(defun repl-fresh-prompt (r)
  "Start a new prompt line and protect everything above the input."
  (repl-ensure-fresh-line r)
  (append-text r (repl-prompt-string r))
  (set-protect-boundary r (text-cur-line r) (text-cur-col r))
  (setf (text-anchor r) nil)
  (ensure-visible r))

;;; --- reading the current input ---------------------------------------------

(defun repl-current-input (r)
  (let ((p (text-protect r)))
    (if p
        (text-substring r p (cons (1- (line-count r))
                                  (length (nth-line r (1- (line-count r))))))
        "")))

(defun string-blank-p (s)
  (every (lambda (c) (member c '(#\Space #\Tab #\Newline #\Return))) s))

(defun input-complete-p (string)
  "True when STRING reads as zero or more whole forms (no dangling open form)."
  (handler-case
      (with-input-from-string (in string)
        (loop for form = (read in nil :repl-eof) until (eq form :repl-eof))
        t)
    (end-of-file () nil)
    (error () t)))

;;; --- read-only text windows (describe / macroexpand / backtrace / ...) ------

(defun show-text-window (title text &key (width 76) (height 22))
  "Open a modeless, read-only, scrollable window showing TEXT.  Returns the
window and its text view."
  (when *application*
    (let* ((desk (program-desktop *application*))
           (dw (point-x (view-size desk))) (dh (point-y (view-size desk)))
           (w (min width (max 24 (- dw 2)))) (h (min height (max 6 (- dh 2))))
           (win (make-instance 'twindow :title title :bounds (make-trect 0 0 w h)))
           (vsb (standard-scrollbar win t))
           (tv (make-instance 'ttext-view :read-only t
                              :bounds (make-trect 1 1 (1- w) (1- h)))))
      (insert win tv)
      (text-attach-scrollbars tv :vscroll vsb)
      (set-text tv (or text ""))
      (move-to win (max 0 (floor (- dw w) 2)) (max 0 (floor (- dh h) 2)))
      (insert desk win)
      (focus tv)
      (values win tv))))

(defun show-text-dialog (title text &key (width 72) (height 20))
  "Show TEXT in a modal, read-only, scrollable dialog (usable from inside another
modal view, e.g. the restart dialog)."
  (when *application*
    (let* ((desk (program-desktop *application*))
           (dw (point-x (view-size desk))) (dh (point-y (view-size desk)))
           (w (min width (max 24 (- dw 2)))) (h (min height (max 8 (- dh 2))))
           (d (make-instance 'tdialog :title title :bounds (make-trect 0 0 w h)))
           (vsb (standard-scrollbar d t))
           (tv (make-instance 'ttext-view :read-only t
                              :bounds (make-trect 1 1 (1- w) (- h 3)))))
      (insert d tv)
      (text-attach-scrollbars tv :vscroll vsb)
      (set-text tv (or text ""))
      (insert d (make-button (make-trect (floor (- w 10) 2) (- h 3)
                                         (+ (floor (- w 10) 2) 10) (- h 1))
                             "O~K~" +cm-ok+ t))
      (move-to d (max 0 (floor (- dw w) 2)) (max 0 (floor (- dh h) 2)))
      (focus tv)
      (exec-view desk d))))

;;; --- backtrace capture (sb-di) + a frame/locals browser -------------------
;;; Frames are snapshotted eagerly (label + each live local's value as a string)
;;; while the error stack is still live; the snapshot is plain data, so it can be
;;; browsed later on the UI thread (even for the cross-thread worker debugger).

(defun %frame-vars (frame df loc)
  "Each local as (name display-string value); the value object is retained so it
can be inspected (drilled into) later."
  (let ((out '()))
    (handler-case
        (sb-di:do-debug-fun-vars (v df)
          (when (and loc (eq (handler-case (sb-di:debug-var-validity v loc) (error () :invalid))
                             :valid))
            (let ((val (handler-case (sb-di:debug-var-value v frame)
                         (error () '#:|#<unavailable>|))))
              (push (list (string-downcase (symbol-name (sb-di:debug-var-symbol v)))
                          (handler-case
                              (let ((*print-length* 8) (*print-level* 3) (*print-readably* nil))
                                (prin1-to-string val))
                            (error () "#<error printing>"))
                          val)
                    out))))
      (error () nil))
    (nreverse out)))

(defun repl-capture-frames (&key (count 50))
  "Snapshot the live stack as a list of plists
(:label STRING :locals ((name display-string value) ...))."
  (or (ignore-errors
       (let ((frames '()) (i 0))
         (do ((f (sb-di:top-frame) (sb-di:frame-down f)))
             ((or (null f) (>= i count)) (nreverse frames))
           (let* ((df (sb-di:frame-debug-fun f))
                  (name (handler-case (sb-di:debug-fun-name df) (error () "?")))
                  (loc (handler-case (sb-di:frame-code-location f) (error () nil))))
             (push (list :label (format nil "~2d  ~a" i name)
                         :locals (%frame-vars f df loc))
                   frames))
           (incf i))))
      '()))

(defun inspect-modal (obj label)
  "Inspect OBJ in a modal TOutline window (usable from inside another modal view,
e.g. the frame-locals browser).  The tree is drillable: expand nodes to follow
the value's structure."
  (when *application*
    (let* ((desk (program-desktop *application*))
           (dw (point-x (view-size desk))) (dh (point-y (view-size desk)))
           (w (min 70 (max 24 (- dw 2)))) (h (min 20 (max 8 (- dh 2))))
           (d (make-instance 'tdialog :title (format nil "Inspect ~a" label)
                             :bounds (make-trect 0 0 w h)))
           (vsb (standard-scrollbar d t))
           (ol (make-instance 'toutline :roots (list (object->outline obj label))
                              :bounds (make-trect 1 1 (1- w) (- h 3)))))
      (insert d ol) (attach-scrollbars ol :vscroll vsb)
      (insert d (make-button (make-trect (floor (- w 10) 2) (- h 3)
                                         (+ (floor (- w 10) 2) 10) (- h 1)) "O~K~" +cm-ok+))
      (move-to d (max 0 (floor (- dw w) 2)) (max 0 (floor (- dh h) 2)))
      (focus ol)
      (exec-view desk d))))

(defparameter +cm-dbg-eval+ 71)

(defun frame-eval-with-locals (locals package form-string)
  "Evaluate FORM-STRING with the frame's captured LOCALS bound (snapshot
semantics: the locals are the values captured at error time, not live).  Return
a printed result string."
  (handler-case
      (let* ((*package* (or package *package*))
             (form (read-from-string form-string))
             (binds (loop for (name nil val) in locals
                          for sym = (ignore-errors (read-from-string name nil nil))
                          when (and sym (symbolp sym) (not (keywordp sym)))
                          collect (cons sym val))))
        (let ((*print-length* 50) (*print-level* 8) (*print-readably* nil))
          (prin1-to-string
           (eval `(let ,(mapcar (lambda (b) (list (car b) (list 'quote (cdr b)))) binds)
                    (declare (ignorable ,@(mapcar #'car binds)))
                    ,form)))))
    (error (e) (format nil ";; ~a" e))))

(defclass tlocals-dialog (tdialog)
  ((locals  :initarg :locals  :initform nil :accessor locals-dialog-locals) ; (name str value)
   (package :initarg :package :initform nil :accessor locals-dialog-package)
   (lb      :initarg :lb      :initform nil :accessor locals-dialog-lb))
  (:documentation "A frame's local variables; Enter inspects a local's value, and
Eval evaluates a form with the frame's locals bound."))

(defmethod handle-event ((d tlocals-dialog) event)
  (cond
    ((and (message-event-p event)
          (= (event-command event) +cm-list-item-selected+)
          (locals-dialog-lb d))
     (let ((entry (nth (list-focused (locals-dialog-lb d)) (locals-dialog-locals d))))
       (when entry (inspect-modal (third entry) (first entry))))
     (clear-event event))
    ((and (= (event-type event) +ev-command+) (= (event-command event) +cm-dbg-eval+))
     (multiple-value-bind (cmd s)
         (input-box "Eval in frame" "Form (uses the frame's locals):" "" 200)
       (when (and (= cmd +cm-ok+) (plusp (length (string-trim '(#\Space #\Tab) s))))
         (show-text-dialog "Eval in frame"
                           (format nil "~a~%~%=> ~a" s
                                   (frame-eval-with-locals (locals-dialog-locals d)
                                                           (locals-dialog-package d) s)))))
     (clear-event event))
    (t (call-next-method))))

(defun show-locals-dialog (frame &optional package)
  "Modal locals browser for FRAME; Enter inspects a local, Eval evaluates a form
with the frame's locals bound."
  (let ((label (getf frame :label)) (locals (getf frame :locals)))
    (let* ((desk (program-desktop *application*))
           (dw (point-x (view-size desk))) (dh (point-y (view-size desk)))
           (w (min 70 (max 34 (- dw 2)))) (h (min 18 (max 9 (- dh 2))))
           (items (if locals
                      (mapcar (lambda (l) (format nil "~a = ~a" (first l) (second l))) locals)
                      (list "(no locals available)")))
           (lb (make-instance 'tlist-box :items items :command 0
                              :bounds (make-trect 1 1 (1- w) (- h 4))))
           (d (make-instance 'tlocals-dialog :locals locals :lb lb :package package
                             :title (format nil "Locals: ~a" (string-trim " " label))
                             :bounds (make-trect 0 0 w h)))
           (vsb (standard-scrollbar d t)))
      (insert d lb) (attach-scrollbars lb :vscroll vsb)
      (insert d (make-button (make-trect 2 (- h 3) 12 (- h 1)) "~E~val" +cm-dbg-eval+))
      (insert d (make-button (make-trect (- w 12) (- h 3) (- w 2) (- h 1)) "O~K~" +cm-ok+))
      (move-to d (max 0 (floor (- dw w) 2)) (max 0 (floor (- dh h) 2)))
      (focus lb)
      (exec-view desk d))))

(defclass tframe-dialog (tdialog)
  ((frames  :initarg :frames  :initform nil :accessor frame-dialog-frames)
   (package :initarg :package :initform nil :accessor frame-dialog-package)
   (lb      :initarg :lb      :initform nil :accessor frame-dialog-lb))
  (:documentation "The backtrace browser: a list of frames; Enter opens a frame's
locals browser, from which a value can be inspected/drilled into."))

(defun %backtrace-text (frames)
  "A full-backtrace string: each frame's label followed by its locals."
  (with-output-to-string (s)
    (dolist (f frames)
      (format s "~a~%" (getf f :label))
      (dolist (lv (getf f :locals))
        (format s "        ~a = ~a~%" (first lv) (second lv)))
      (terpri s))))

(defmethod handle-event ((d tframe-dialog) event)
  (cond
    ((and (message-event-p event)
          (= (event-command event) +cm-list-item-selected+)
          (frame-dialog-lb d))
     (let ((frame (nth (list-focused (frame-dialog-lb d)) (frame-dialog-frames d))))
       (when frame (show-locals-dialog frame (frame-dialog-package d))))
     (clear-event event))
    ;; `e' exports the whole backtrace (frames + locals) to a scrollable window
    ((and (= (event-type event) +ev-key-down+)
          (plusp (event-char-code event))
          (member (char-downcase (code-char (event-char-code event))) '(#\e)))
     (show-text-dialog "Backtrace (full)" (%backtrace-text (frame-dialog-frames d)) :height 24)
     (clear-event event))
    (t (call-next-method))))

(defun show-frames-dialog (frames &optional package)
  "Modal backtrace browser; pick a frame (Enter) to inspect its locals."
  (when *application*
    (if (null frames)
        (show-text-dialog "Backtrace" "(no backtrace available)")
        (let* ((desk (program-desktop *application*))
               (dw (point-x (view-size desk))) (dh (point-y (view-size desk)))
               (w (min 76 (max 30 (- dw 2)))) (h (min 22 (max 10 (- dh 2))))
               (lb (make-instance 'tlist-box
                                  :items (mapcar (lambda (f) (getf f :label)) frames)
                                  :command 0 :bounds (make-trect 1 1 (1- w) (- h 4))))
               (d (make-instance 'tframe-dialog :frames frames :lb lb :package package
                                 :title "Backtrace — Enter: locals  E: export"
                                 :bounds (make-trect 0 0 w h)))
               (vsb (standard-scrollbar d t)))
          (insert d lb) (attach-scrollbars lb :vscroll vsb)
          ;; OK is NOT the default button: a default button would steal Enter
          ;; from the list (where Enter must open the focused frame's locals).
          (insert d (make-button (make-trect (floor (- w 10) 2) (- h 3)
                                             (+ (floor (- w 10) 2) 10) (- h 1)) "O~K~" +cm-ok+))
          (move-to d (max 0 (floor (- dw w) 2)) (max 0 (floor (- dh h) 2)))
          (focus lb)
          (exec-view desk d)))))

;;; --- restart menu (the micros/SLIME debugger feel) -------------------------

(defparameter +cm-repl-backtrace+ 70)

(defclass trestart-dialog (tdialog)
  ((backtrace :initarg :backtrace :initform nil :accessor restart-dialog-backtrace)
   (package   :initarg :package   :initform nil :accessor restart-dialog-package))
  (:documentation "The debugger dialog; its Backtrace button opens the frame
browser (frames + locals) without dismissing the restart list."))

(defmethod handle-event ((d trestart-dialog) event)
  (when (and (= (event-type event) +ev-command+)
             (= (event-command event) +cm-repl-backtrace+))
    (show-frames-dialog (restart-dialog-backtrace d) (restart-dialog-package d))
    (clear-event event))
  (call-next-method))

(defun %restart-needs-value-p (restart)
  "True for restarts that take a value argument (USE-VALUE / STORE-VALUE), so
the debugger must prompt for one before invoking."
  (let ((n (restart-name restart)))
    (and n (member (symbol-name n) '("USE-VALUE" "STORE-VALUE") :test #'string=))))

(defun %restart-label (rs)
  "A human label for restart RS: its symbolic name plus its report description
 (SLIME-style), e.g. \"RETRY — Retry the request.\".  Either part may be absent."
  (let* ((name (restart-name rs))
         (report (handler-case (string-trim '(#\Space #\Tab #\Newline) (format nil "~a" rs))
                   (error () "")))
         (label (cond ((null name) report)                       ; anonymous restart
                      ((or (zerop (length report))               ; report adds nothing
                           (string-equal report (princ-to-string name)))
                       (princ-to-string name))
                      (t (format nil "~a — ~a" name report)))))
    (if (> (length label) 90) (concatenate 'string (subseq label 0 87) "...") label)))

(defun repl-restart-dialog (condition restarts &optional backtrace package)
  "UI-thread only: show RESTARTS for CONDITION and, when the chosen restart needs
a value (USE-VALUE/STORE-VALUE), prompt for a Lisp form supplying it.  A
Backtrace button opens the frame browser (frames/locals; PACKAGE is used for
eval-in-frame).  Returns (values index value-form-string); INDEX is NIL when the
user aborts.  Safe to call while a worker thread is blocked waiting."
  (when (and *application* restarts)
    (let* ((labels (mapcar #'%restart-label restarts))
           (desk (program-desktop *application*))
           (w 64) (h 17)
           (d (make-instance 'trestart-dialog :title "Error — pick a restart"
                             :backtrace backtrace :package package
                             :bounds (make-trect 0 0 w h)))
           (st (make-instance 'tstatic-text
                              :text (format nil "~(~a~):~%~a" (type-of condition) condition)
                              :bounds (make-trect 2 1 (- w 2) 5)))
           (vsb (standard-scrollbar d t))
           (lb (make-instance 'tlist-box :items labels :command +cm-ok+
                              :bounds (make-trect 2 6 (1- w) (- h 4)))))
      (insert d st) (insert d lb) (attach-scrollbars lb :vscroll vsb)
      (insert d (make-button (make-trect 2 (- h 3) 15 (- h 1)) "~B~acktrace" +cm-repl-backtrace+))
      (insert d (make-button (make-trect (- w 28) (- h 3) (- w 17) (- h 1)) "~I~nvoke" +cm-ok+ t))
      (insert d (make-button (make-trect (- w 14) (- h 3) (- w 3) (- h 1)) "Abort" +cm-cancel+))
      (move-to d (max 0 (floor (- (point-x (view-size desk)) w) 2))
               (max 0 (floor (- (point-y (view-size desk)) h) 2)))
      (focus lb)
      (if (= (exec-view desk d) +cm-ok+)
          (let* ((idx (list-focused lb)) (rs (nth idx restarts)))
            (if (%restart-needs-value-p rs)
                ;; loop until the value form reads cleanly (or the user cancels),
                ;; so a typo re-prompts instead of silently aborting
                (loop
                  (multiple-value-bind (cmd s)
                      (input-box "Restart value"
                                 (format nil "Lisp form to ~(~a~):" (restart-name rs)) "")
                    (cond
                      ((or (/= cmd +cm-ok+)
                           (zerop (length (string-trim '(#\Space #\Tab) s))))
                       (return (values nil nil)))             ; cancelled -> abort
                      ((let ((*package* (or package *package*)))
                         (ignore-errors (read-from-string s) t))
                       (return (values idx s)))               ; readable -> use it
                      (t (message-box "That isn't a readable Lisp form -- try again."
                                      (logior +mf-error+ +mf-ok-button+))))))
                (values idx nil)))
          (values nil nil)))))

(defun repl-invoke-restart (restarts idx value-string)
  "Invoke the chosen restart, reading+evaluating VALUE-STRING for the value of a
USE-VALUE/STORE-VALUE restart; abort when nothing was chosen or the value form
fails to read/evaluate.  Runs on the thread owning the error's dynamic extent."
  (let ((rs (and idx (nth idx restarts))))
    (cond
      ((null rs) (invoke-restart (find-restart 'repl-abort)))
      (value-string
       (let* ((sentinel (cons nil nil))
              (val (handler-case (eval (read-from-string value-string))
                     (error () sentinel))))
         (if (eq val sentinel)
             (invoke-restart (find-restart 'repl-abort))
             (invoke-restart rs val))))
      (t (invoke-restart rs)))))

(defun repl-error-handler (e)
  "HANDLER-BIND handler (inline path): offer restarts, then transfer control."
  (if *repl-debugger*
      (let ((restarts (compute-restarts e))
            (bt (repl-capture-frames)))
        (multiple-value-bind (idx vs) (repl-restart-dialog e restarts bt *package*)
          (repl-invoke-restart restarts idx vs)))
      (invoke-restart (find-restart 'repl-abort))))

;;; --- single-stepper (drives SBCL's stepper via *stepper-hook*) -------------

(defparameter +cm-step-into+ 72)
(defparameter +cm-step-over+ 73)
(defparameter +cm-step-out+  74)
(defparameter +cm-step-run+  75)

(defclass tstep-dialog (tdialog) ()
  (:documentation "The stepper prompt; its buttons end the modal with their own
command so the caller learns which action was chosen."))

(defmethod handle-event ((d tstep-dialog) event)
  (when (and (= (event-type event) +ev-command+)
             (member (event-command event)
                     (list +cm-step-into+ +cm-step-over+ +cm-step-out+ +cm-step-run+)))
    (end-modal d (event-command event))
    (clear-event event))
  (call-next-method))

(defun repl-step-dialog (form args)
  "UI thread: show the form about to be evaluated and return the chosen action
(:into / :next / :out / :continue / :abort)."
  (if (null *application*)
      :continue
      (let* ((desk (program-desktop *application*))
             (dw (point-x (view-size desk))) (dh (point-y (view-size desk)))
             (w (min 72 (max 40 (- dw 2)))) (h 11)
             (d (make-instance 'tstep-dialog :title "Stepper" :bounds (make-trect 0 0 w h)))
             (txt (let ((*print-length* 12) (*print-level* 4))
                    (format nil "~a~@[~%args: ~{~s~^  ~}~]"
                            form (and args (coerce args 'list)))))
             (st (make-instance 'tstatic-text :text txt :bounds (make-trect 2 1 (- w 2) 5)))
             (y (- h 3)))
        (insert d st)
        (insert d (make-button (make-trect 2 y 12 (1+ y)) "~I~nto" +cm-step-into+ t))
        (insert d (make-button (make-trect 12 y 22 (1+ y)) "~O~ver" +cm-step-over+))
        (insert d (make-button (make-trect 22 y 31 (1+ y)) "Ou~t~" +cm-step-out+))
        (insert d (make-button (make-trect 31 y 40 (1+ y)) "~R~un" +cm-step-run+))
        (insert d (make-button (make-trect 40 y 50 (1+ y)) "~A~bort" +cm-cancel+))
        (move-to d (max 0 (floor (- dw w) 2)) (max 0 (floor (- dh h) 2)))
        (let ((c (exec-view desk d)))
          (cond ((= c +cm-step-into+) :into)
                ((= c +cm-step-over+) :next)
                ((= c +cm-step-out+)  :out)
                ((= c +cm-step-run+)  :continue)
                (t :abort))))))

(defun repl-step-ask (form args)
  "Worker thread: ask the UI for a stepping action and block for the answer."
  (if *ui-callbacks*
      (let ((sem (sb-thread:make-semaphore)) (choice (list :continue)))
        (run-on-ui (lambda () (setf (car choice) (repl-step-dialog form args))
                     (sb-thread:signal-semaphore sem)))
        (sb-thread:wait-on-semaphore sem)
        (car choice))
      :continue))

(defun make-step-hook (r)
  "A *stepper-hook* that drives SBCL's stepper from the UI: a dialog per form,
and the value of each stepped form streamed to the transcript."
  (lambda (c)
    (typecase c
      (sb-ext:step-form-condition
       (ecase (repl-step-ask (sb-ext:step-condition-form c)
                             (ignore-errors (sb-ext:step-condition-args c)))
         (:into     (sb-ext:step-into c))
         (:next     (sb-ext:step-next c))
         (:out      (let ((rr (find-restart 'sb-ext:step-out c)))
                      (if rr (invoke-restart rr) (sb-ext:step-next c))))
         (:continue (sb-ext:step-continue c))
         (:abort    (let ((rr (find-restart 'repl-abort)))
                      (if rr (invoke-restart rr) (sb-ext:step-continue c))))))
      (sb-ext:step-values-condition
       (let ((form (ignore-errors (sb-ext:step-condition-form c)))
             (res  (ignore-errors (sb-ext:step-condition-result c))))
         (run-on-ui (lambda ()
                      (repl-print r (format nil "~&; ~s => ~{~s~^, ~}~%" form res))
                      (ensure-visible r) (draw-view r)
                      (when *screen* (flush-screen *screen*)))))
       nil)
      (t nil))))

(defun repl-step-eval (r input)
  "Evaluate INPUT under the single-stepper (worker thread)."
  (when (and *repl-async* *ui-callbacks*)
    (repl-ensure-fresh-line r)
    (repl-print r (format nil "; stepping: ~a~%" (string-trim '(#\Space #\Newline) input)))
    (setf (repl-busy r) t)
    (repl-ensure-worker r)
    (mailbox-send (repl-to-worker r) (cons :step input))))

;;; --- evaluation + printing -------------------------------------------------

(defun repl-eval (r input)
  (multiple-value-bind (output results new-package errored new-hist)
      (repl-backend-eval input (repl-package r) #'repl-error-handler (repl-hist-vars r))
    (setf (repl-package r) new-package          ; sticky in-package
          (repl-hist-vars r) new-hist)          ; per-listener history vars
    (values output results errored)))

(defun repl-print-results (r results)
  (if results
      (dolist (vals results)
        (if vals
            (dolist (v vals) (repl-print r (format nil "~s~%" v)))
            (repl-print r (format nil "; No values~%"))))
      (repl-print r (format nil "; No values~%"))))

(defun repl-submit (r input)
  "Record INPUT in history and start evaluating it -- on the worker thread when
async is enabled and a UI loop is running, otherwise inline."
  (push (string-trim '(#\Space #\Tab #\Newline #\Return) input) (repl-history r))
  (setf (repl-hist-pos r) nil)
  (when (repl-history-file r) (save-repl-history r))
  (append-text r (string #\Newline))
  (cond
    ((and *repl-async* *ui-callbacks*)
     (setf (repl-busy r) t)
     (repl-ensure-worker r)
     (mailbox-send (repl-to-worker r) (cons :eval input)))
    (t                                   ; synchronous fallback
     (multiple-value-bind (output results errored) (repl-eval r input)
       (when (plusp (length output))
         (repl-print r output) (repl-ensure-fresh-line r))
       (unless errored (repl-print-results r results))
       (repl-fresh-prompt r)))))

(defun repl-meta-command-p (input)
  "True when INPUT is the :help / :h meta-command (specifically, so other
self-evaluating keywords typed at the prompt are still evaluated normally)."
  (let ((low (string-downcase (string-trim '(#\Space #\Tab) input))))
    (or (member low '(":help" ":h") :test #'string=)
        (and (>= (length low) 6) (string= ":help " (subseq low 0 6)))
        (and (>= (length low) 3) (string= ":h " (subseq low 0 3))))))

(defun repl-run-meta (r input)
  "Handle the :help [SYMBOL] meta-command: describe SYMBOL (read in the listener's
package), or print a short usage line."
  (let* ((s (string-trim '(#\Space #\Tab) input))
         (sp (position #\Space s))
         (arg (and sp (string-trim '(#\Space #\Tab) (subseq s sp)))))
    (append-text r (string #\Newline))
    (if (and arg (plusp (length arg)))
        (let ((*package* (repl-package r)))
          (handler-case
              (repl-print r (with-output-to-string (o) (describe (read-from-string arg) o)))
            (error (e) (repl-print r (format nil "; ~a~%" e)))))
        (repl-print r (format nil "; :help SYMBOL  -- describe SYMBOL (just type Lisp forms to evaluate)~%")))
    (push (string-trim '(#\Space #\Tab #\Newline) input) (repl-history r))
    (repl-ensure-fresh-line r)
    (repl-fresh-prompt r)))

(defmethod text-return ((r trepl-view))
  (cond
    ((repl-busy r) (call-next-method))   ; evaluating: Enter just inserts a newline
    (t (let ((input (repl-current-input r)))
         (cond
           ((string-blank-p input)
            (append-text r (string #\Newline)) (repl-fresh-prompt r))
           ((repl-meta-command-p input)
            (repl-run-meta r input))
           ((not (input-complete-p input))
            (split-line-at-cursor r))
           (t (repl-submit r input)))))))

;;; ===========================================================================
;;; Background evaluation: one worker thread per listener (the SLIME/Lem model)
;;; ===========================================================================
;;;
;;; The worker evaluates Lisp while the UI thread keeps running.  It NEVER
;;; touches the view directly: output, results, and debugger requests are all
;;; shipped to the UI thread via RUN-ON-UI.

(defvar *repl-views* '() "All live REPL views, so the app can stop their workers.")

;;; --- a Gray stream that streams worker output to the transcript live --------

(defclass repl-output-stream (sb-gray:fundamental-character-output-stream)
  ((view   :initarg :view :reader ros-view)
   (buffer :initform (make-string-output-stream) :reader ros-buffer)))

(defun ros-flush (s)
  (let ((chunk (get-output-stream-string (ros-buffer s))))
    (when (plusp (length chunk))
      (let ((r (ros-view s)))
        (run-on-ui (lambda () (repl-stream-output r chunk)))))))

(defmethod sb-gray:stream-write-char ((s repl-output-stream) ch)
  (write-char ch (ros-buffer s))
  (when (char= ch #\Newline) (ros-flush s))
  ch)

(defmethod sb-gray:stream-write-string ((s repl-output-stream) string &optional (start 0) end)
  (let ((end (or end (length string))))
    (write-string string (ros-buffer s) :start start :end end)
    (when (find #\Newline string :start start :end end) (ros-flush s)))
  string)

(defmethod sb-gray:stream-line-column ((s repl-output-stream)) nil)
(defmethod sb-gray:stream-finish-output ((s repl-output-stream)) (ros-flush s))
(defmethod sb-gray:stream-force-output ((s repl-output-stream)) (ros-flush s))

(defun repl-stream-output (r chunk)
  "UI thread: append a chunk of worker output to the transcript and redraw."
  (append-text r chunk)
  (ensure-visible r)
  (draw-view r)
  (when *screen* (flush-screen *screen*)))

;;; --- the worker thread ------------------------------------------------------

(defun repl-ensure-worker (r)
  "Start R's evaluation thread if it isn't already running."
  (unless (repl-to-worker r) (setf (repl-to-worker r) (make-mailbox)))
  (unless (and (repl-worker r) (sb-thread:thread-alive-p (repl-worker r)))
    (pushnew r *repl-views*)
    (setf *background-shutdown-hook* #'shutdown-repl-workers)
    (setf (repl-worker r)
          (sb-thread:make-thread (lambda () (repl-worker-loop r))
                                 :name "tvision-repl-worker"))))

(defun repl-worker-loop (r)
  (catch 'repl-worker-quit
    (let ((*package* (repl-package r))
          (*read-eval* t))
      (loop
        (let ((job (mailbox-receive (repl-to-worker r))))
          (case (car job)
            (:quit (throw 'repl-worker-quit nil))
            (:eval (repl-worker-eval r (cdr job)))
            (:step (repl-worker-eval r (cdr job) t))
            (:call (repl-worker-call r (cdr job)))))))))

(defun repl-worker-call (r thunk)
  "Run THUNK on the worker with the REPL's output captured/streamed, errors
routed to the cross-thread debugger, and abort (Ctrl-C) honoured.  Clears the
busy flag on the UI thread when done."
  (let ((out (make-instance 'repl-output-stream :view r)))
    (unwind-protect
         (let ((*standard-output* out) (*error-output* out) (*trace-output* out)
               (*package* (repl-package r)))
           (restart-case
               (handler-bind ((error (lambda (e) (repl-worker-debug r e))))
                 (funcall thunk))
             (repl-abort () nil)))
      (finish-output out)
      (run-on-ui (lambda ()
                   (setf (repl-busy r) nil)
                   (draw-view r)
                   (when *screen* (flush-screen *screen*)))))))

(defun repl-call-on-worker (r thunk)
  "Schedule THUNK to run on R's worker thread, so the UI stays responsive (output
streams, errors open the debugger, Ctrl-C interrupts).  Runs inline when there is
no UI loop / async is disabled."
  (cond
    ((and *repl-async* *ui-callbacks*)
     (setf (repl-busy r) t)
     (repl-ensure-worker r)
     (mailbox-send (repl-to-worker r) (cons :call thunk)))
    (t (funcall thunk))))

(defun repl-worker-debug (r condition)
  "Worker thread, inside HANDLER-BIND: ask the UI thread to show the restart
menu, block for the choice, then invoke the chosen restart here (so the live
stack/dynamic extent of the error is intact).  Never returns normally."
  (let ((restarts (compute-restarts condition))
        (bt (repl-capture-frames))
        (pkg (repl-package r)))
    (if (and *repl-debugger* *ui-callbacks*)
        (let ((sem (sb-thread:make-semaphore)) (choice (list nil nil)))
          (run-on-ui (lambda ()
                       (multiple-value-bind (idx vs) (repl-restart-dialog condition restarts bt pkg)
                         (setf (first choice) idx (second choice) vs))
                       (sb-thread:signal-semaphore sem)))
          (sb-thread:wait-on-semaphore sem)
          (repl-invoke-restart restarts (first choice) (second choice)))
        (invoke-restart (find-restart 'repl-abort)))))

(defun repl-worker-eval (r input &optional step)
  "Worker thread: read+eval all forms in INPUT, streaming output to the UI and
routing errors through the cross-thread debugger.  When STEP, evaluate each form
under SBCL's single-stepper.  Posts the final results back to the UI thread."
  (let ((out (make-instance 'repl-output-stream :view r))
        (results '()) (errored nil) (last nil) (new-hist (repl-hist-vars r))
        (start (get-internal-real-time)))
    (let ((*standard-output* out) (*error-output* out) (*trace-output* out)
          (*package* (repl-package r))
          (sb-ext:*stepper-hook* (if step (make-step-hook r) sb-ext:*stepper-hook*)))
      (unwind-protect
           (with-repl-history ((repl-hist-vars r) new-hist)
             (restart-case
                 (handler-bind ((error (lambda (e) (setf last e) (repl-worker-debug r e))))
                   (with-input-from-string (in input)
                     (loop for form = (read in nil :repl-eof)
                           until (eq form :repl-eof)
                           do (setf - form)
                              (let ((vals (multiple-value-list
                                           (if step (eval (list 'step form)) (eval form)))))
                                (push vals results)
                                (setf +++ ++  ++ +  + form
                                      /// //  // /  / vals
                                      *** **  ** *  * (first vals))))))
               (repl-abort () (setf errored t))))
        (finish-output out))
      (let ((results (nreverse results)) (pkg *package*)
            (errored errored) (last last) (new-hist new-hist)
            (ms (round (* 1000 (/ (- (get-internal-real-time) start)
                                  internal-time-units-per-second)))))
        (run-on-ui (lambda () (repl-finish-eval r results pkg errored last new-hist ms)))))))

(defun repl-finish-eval (r results pkg errored last new-hist &optional (ms 0))
  "UI thread: print results/error summary and re-prompt after a worker eval."
  (setf (repl-package r) pkg             ; sticky in-package
        (repl-hist-vars r) new-hist)     ; per-listener history vars
  (repl-ensure-fresh-line r)
  (cond
    ((and errored last)
     (repl-print r (format nil "; ~(~a~): ~a~%" (type-of last) last)))
    ((not errored) (repl-print-results r results)))
  (when (and *repl-time* (not errored))
    (repl-print r (format nil "; ~d ms~%" ms)))
  (setf (repl-busy r) nil)
  (repl-fresh-prompt r)
  (draw-view r)
  (when *screen* (flush-screen *screen*)))

;;; --- interrupt + shutdown ---------------------------------------------------

(defun repl-interrupt (r)
  "Interrupt R's in-flight evaluation (Ctrl-C / menu): unwind it to a fresh
prompt.  No-op when nothing is running."
  (let ((th (repl-worker r)))
    (when (and th (sb-thread:thread-alive-p th) (repl-busy r))
      (ignore-errors
       (sb-thread:interrupt-thread
        th (lambda ()
             (let ((rs (find-restart 'repl-abort)))
               (when rs (invoke-restart rs)))))))))

(defun repl-stop-worker (r)
  (let ((th (repl-worker r)))
    (when (and th (sb-thread:thread-alive-p th))
      (ignore-errors (mailbox-send (repl-to-worker r) (cons :quit nil)))
      (ignore-errors
       (sb-thread:interrupt-thread th (lambda () (throw 'repl-worker-quit nil)))))
    (setf (repl-worker r) nil)))

(defun shutdown-repl-workers ()
  (dolist (r *repl-views*) (repl-stop-worker r))
  (setf *repl-views* '()))

;;; --- tab completion --------------------------------------------------------

(defun repl-token-before-cursor (r)
  "Return (values token start-col) for the symbol token left of the cursor."
  (let* ((line (current-line-string r)) (col (text-cur-col r)) (start col))
    (loop while (and (> start 0) (%symbol-char-p (char line (1- start)))) do (decf start))
    (values (subseq line start col) start)))

(defun repl-insert-completion (r start completion)
  (let ((line (current-line-string r)) (col (text-cur-col r)) (li (text-cur-line r)))
    (text-snapshot r)
    (set-line r li (concatenate 'string (subseq line 0 start) completion (subseq line col)))
    (setf (text-cur-col r) (+ start (length completion)))
    (text-update-limit r) (ensure-visible r) (draw-view r)))

(defun repl-complete (r)
  "Complete the symbol at the cursor: extend to the common prefix, or pop up a
candidate list when several remain."
  (multiple-value-bind (token start) (repl-token-before-cursor r)
    (when (plusp (length token))
      (let ((cands (repl-backend-completions token (repl-package r))))
        (cond
          ((null cands) nil)
          ((= 1 (length cands)) (repl-insert-completion r start (first cands)))
          (t (let ((common (longest-common-prefix cands)))
               (if (> (length common) (length token))
                   (repl-insert-completion r start common)
                   (multiple-value-bind (gx gy) (view-global-origin r)
                     (let ((chosen (popup-list (subseq cands 0 (min 300 (length cands)))
                                               (+ gx (- (text-cur-col r) (text-left-col r)))
                                               (+ gy (1+ (- (text-cur-line r) (text-top-line r))))
                                               :title "Completions")))
                       (when chosen (repl-insert-completion r start chosen))))))))))))

(defun popup-list (items x y &key (title ""))
  "Modal list-box dialog at (X,Y); return the chosen item string, or NIL."
  (when (and *application* items)
    (let* ((maxw (reduce #'max items :key #'length :initial-value 8))
           (w (min 44 (+ 4 maxw))) (h (min 14 (+ 2 (length items))))
           (desk (program-desktop *application*))
           (d (make-instance 'tdialog :title title :bounds (make-trect 0 0 w h)))
           (vsb (standard-scrollbar d t))
           (lb (make-instance 'tlist-box :items items :command +cm-ok+
                              :bounds (make-trect 1 1 (1- w) (1- h)))))
      (insert d lb) (attach-scrollbars lb :vscroll vsb)
      (move-to d (max 0 (min x (- (point-x (view-size desk)) w)))
               (max 0 (min y (- (point-y (view-size desk)) h))))
      (focus lb)
      (when (= (exec-view desk d) +cm-ok+) (list-item lb (list-focused lb))))))

;;; --- object inspector (built on TOutline) ----------------------------------

(defun %short-repr (obj)
  (let ((*print-length* 6) (*print-level* 2) (*print-readably* nil))
    (let ((s (handler-case (prin1-to-string obj) (error () "#<unprintable>"))))
      (if (> (length s) 56) (concatenate 'string (subseq s 0 53) "...") s))))

(defun %inspect-trackable-p (obj)
  "True for aggregate objects that OBJECT->OUTLINE recurses into and that can
therefore form a reference cycle (strings are leaves and never tracked)."
  (typecase obj
    (string nil)
    ((or cons hash-table standard-object structure-object) t)
    (vector t)
    (t nil)))

(defconstant +inspect-page+ 200
  "How many elements OBJECT->OUTLINE shows per collection before a drillable
`... N more' node; drilling that node pages through the remainder.")

(defun object->outline (obj label &optional (depth 3) path)
  "Build a depth-limited TOutline node tree describing OBJ.  Robust against
objects whose slots/elements error when read (e.g. system structures): a
failing branch becomes an `<error>' leaf rather than crashing the inspector.
PATH is the chain of ancestor objects; an OBJ already on it is rendered as a
`[circular ref]' leaf instead of being expanded again."
  (when (and (%inspect-trackable-p obj) (member obj path :test #'eq))
    (return-from object->outline
      (make-outline-node (format nil "~a = ~a  [circular ref]" label (%short-repr obj))
                         nil obj)))
  (let ((children '())
        (path* (if (%inspect-trackable-p obj) (cons obj path) path)))
    (when (plusp depth)
      (flet ((kid (v lbl)
               ;; never let a recursive step escape -- it would kill the UI loop
               (push (handler-case (object->outline v lbl (1- depth) path*)
                       (serious-condition (e)
                         (make-outline-node (format nil "~a = <~a>" lbl (type-of e)))))
                     children))
             (overflow (n rest noun)
               ;; a drillable `... N more' node for the truncated tail of a big
               ;; collection (REST is re-inspected to page through the remainder)
               (push (make-outline-node
                      (if n (format nil "... ~d more ~a" n noun) (format nil "... more ~a" noun))
                      nil rest)
                     children)))
        (handler-case
            (typecase obj
              (string nil)
              ;; packages are structure-objects in SBCL, but their raw slots are
              ;; huge internal symbol tables -- show the useful summary instead.
              (package
               (kid (package-name obj) "name")
               (kid (package-nicknames obj) "nicknames")
               (kid (package-use-list obj) "use-list")
               (kid (package-used-by-list obj) "used-by-list"))
              (cons
               (let ((i 0) (tail obj))
                 (loop while (and (consp tail) (< i +inspect-page+))
                       do (kid (car tail) (format nil "[~d]" i)) (incf i) (setf tail (cdr tail)))
                 (when (consp tail)
                   (let ((m (list-length tail)))   ; NIL for a circular list
                     (overflow m tail (if m "elements" "elements (circular)"))))))
              (vector
               (let ((n (length obj)))
                 (dotimes (i (min n +inspect-page+)) (kid (aref obj i) (format nil "[~d]" i)))
                 (when (> n +inspect-page+)
                   (overflow (- n +inspect-page+) (subseq obj +inspect-page+) "elements"))))
              (hash-table
               (let ((i 0) (total (hash-table-count obj)))
                 (maphash (lambda (k v)
                            (when (< i +inspect-page+) (kid v (format nil "~a =>" (%short-repr k))))
                            (incf i))
                          obj)
                 (when (> total +inspect-page+)
                   (overflow (- total +inspect-page+) nil "entries"))))
              ((or structure-object standard-object)
               (dolist (slot (handler-case (sb-mop:class-slots (class-of obj)) (error () nil)))
                 (let ((name (sb-mop:slot-definition-name slot)))
                   (when (handler-case (slot-boundp obj name) (error () nil))
                     (kid (handler-case (slot-value obj name) (serious-condition (e) e))
                          (format nil "~a" name)))))))
          (serious-condition () nil))))
    ;; store the value in the node so the inspector can drill into it
    (let ((node (make-outline-node (format nil "~a = ~a" label (%short-repr obj))
                                   (nreverse children) obj)))
      (setf (outline-node-expanded node) t)
      node)))

(defvar *inspect-goto-hook* nil
  "Optional (VALUE) -> navigate to VALUE's definition; bound by the application
so the inspector's `g' key can jump to source.")

(defclass tinspector-window (twindow)
  ((outline :initform nil :accessor inspector-outline)
   ;; NB: slot names must NOT be `current'/`history' -- those collide with
   ;; TGROUP's own `current' slot (CLOS merges same-named slots).
   (inspect-current :initform nil :accessor inspector-current)   ; (obj . label) shown now
   (inspect-history :initform nil :accessor inspector-history))  ; back-stack of (obj . label)
  (:documentation "An Inspector window whose tree can be drilled into: Enter /
double-click / `i' re-roots the view on the focused value (in place); Backspace
goes back to the previous object; `g' jumps to its definition.  The window title
shows the breadcrumb path."))

(defun %node-label (node)
  "The label portion (before \" = \") of outline NODE's text."
  (let* ((txt (outline-node-text node)) (sep (search " = " txt)))
    (if sep (subseq txt 0 sep) "value")))

(defun %inspector-retitle (w)
  (let* ((crumbs (append (reverse (mapcar #'cdr (inspector-history w)))
                         (and (inspector-current w) (list (cdr (inspector-current w))))))
         (path (format nil "~{~a~^ > ~}" crumbs))
         (path (if (> (length path) 46)
                   (concatenate 'string "..." (subseq path (- (length path) 43)))
                   path)))
    (setf (window-title w)
          (format nil "Inspector: ~a  (Enter/i:in~:[~; Bksp:back~] g:src)"
                  path (inspector-history w)))))

(defun %inspector-show (w obj label)
  "Re-root the inspector window W on (OBJ . LABEL), in place."
  (let ((ol (inspector-outline w)))
    (setf (inspector-current w) (cons obj label)
          (outline-roots ol) (list (object->outline obj label)))
    (outline-update-limit ol)
    (scroll-to ol 0 0)
    (outline-focus ol 0)
    (%inspector-retitle w)
    (draw-view w)))

(defun %inspector-drill (w node)
  "Drill into NODE's value, remembering the current view so Backspace returns."
  (when node
    (push (inspector-current w) (inspector-history w))
    (%inspector-show w (outline-node-data node) (%node-label node))))

(defun %inspector-back (w)
  "Return to the previous object on the history stack, if any."
  (when (inspector-history w)
    (let ((prev (pop (inspector-history w))))
      (%inspector-show w (car prev) (cdr prev)))))

(defmethod handle-event ((w tinspector-window) event)
  (cond
    ;; Enter on a leaf / double-click broadcasts this -> drill in (in place)
    ((and (= (event-type event) +ev-broadcast+)
          (= (event-command event) +cm-outline-item-selected+)
          (eq (event-info event) (inspector-outline w)))
     (%inspector-drill w (outline-current (inspector-outline w)))
     (clear-event event))
    ((= (event-type event) +ev-key-down+)
     (let ((k (event-key-code event)) (ch (event-char-code event)))
       (cond
         ;; `i' drills into the focused node (works on parents too)
         ((or (= ch (char-code #\i)) (= ch (char-code #\I)))
          (%inspector-drill w (outline-current (inspector-outline w))) (clear-event event))
         ;; Backspace -> back to the previous object
         ((= k +kb-back+)
          (%inspector-back w) (clear-event event))
         ;; `g' jumps to the focused value's definition (if the app supplied a hook)
         ((and *inspect-goto-hook* (or (= ch (char-code #\g)) (= ch (char-code #\G))))
          (let ((n (outline-current (inspector-outline w))))
            (when n (ignore-errors (funcall *inspect-goto-hook* (outline-node-data n)))))
          (clear-event event))
         (t (call-next-method)))))
    (t (call-next-method))))

(defun repl-inspect (obj &optional (label "value"))
  "Open an Inspector window showing OBJ as a collapsible tree.  Enter / a click /
`i' drills into the focused value (re-rooting the same window); Backspace goes
back; `g' jumps to its definition."
  (when *application*
    (let* ((desk (program-desktop *application*))
           (w (make-instance 'tinspector-window
                             :bounds (make-trect 4 2 (min 62 (point-x (view-size desk)))
                                                 (min 20 (point-y (view-size desk))))))
           (vsb (standard-scrollbar w t))
           (ol (make-instance 'toutline :roots (list (object->outline obj label))
                              :bounds (make-trect 1 1 (1- (point-x (view-size w)))
                                                  (1- (point-y (view-size w)))))))
      (setf (inspector-outline w) ol
            (inspector-current w) (cons obj label))
      (%inspector-retitle w)
      (insert w ol) (attach-scrollbars ol :vscroll vsb)
      (insert desk w) (focus ol)
      ol)))

;;; --- input history (persistent) --------------------------------------------

(defun save-repl-history (r)
  (ignore-errors
   (with-open-file (s (repl-history-file r) :direction :output
                                            :if-exists :supersede :if-does-not-exist :create)
     (let ((*print-readably* nil) (*print-length* nil))
       (prin1 (subseq (repl-history r) 0 (min 200 (length (repl-history r)))) s)))))

(defun load-repl-history (r)
  (ignore-errors
   (with-open-file (s (repl-history-file r) :if-does-not-exist nil)
     (when s
       (let ((h (read s nil nil)))
         (when (listp h) (setf (repl-history r) h)))))))

(defun repl-load-file (r path)
  "LOAD PATH on the worker thread (so the UI stays responsive), streaming output
into the transcript and re-prompting when done.  Errors open the debugger; the
sticky package follows any in-package in the file."
  (repl-ensure-fresh-line r)
  (repl-print r (format nil "; loading ~a~%" path))
  (draw-view r)
  (repl-call-on-worker r
    (lambda ()
      (unwind-protect (load path)
        (let ((pkg *package*))
          (run-on-ui (lambda ()
                       (setf (repl-package r) pkg)
                       (repl-ensure-fresh-line r)
                       (repl-print r (format nil "; loaded ~a~%" path))
                       (repl-fresh-prompt r)
                       (draw-view r)
                       (when *screen* (flush-screen *screen*)))))))))

;;; --- history recall (Up/Down at the prompt edges) --------------------------

(defun repl-replace-input (r string)
  (let* ((p (text-protect r)) (pl (car p)) (pc (cdr p)))
    (setf (fill-pointer (text-lines r)) (1+ pl))
    (set-line r pl (subseq (nth-line r pl) 0 pc))
    (setf (text-cur-line r) pl (text-cur-col r) pc)
    (insert-string r string)
    (text-update-limit r) (ensure-visible r) (draw-view r)))

(defun repl-history-recall (r dir)
  (let* ((h (repl-history r)) (n (length h)))
    (when (plusp n)
      (let ((pos (ecase dir
                   (:prev (if (null (repl-hist-pos r)) 0 (min (1- n) (1+ (repl-hist-pos r)))))
                   (:next (if (null (repl-hist-pos r)) -1 (1- (repl-hist-pos r)))))))
        (if (minusp pos)
            (progn (setf (repl-hist-pos r) nil) (repl-replace-input r ""))
            (progn (setf (repl-hist-pos r) pos) (repl-replace-input r (nth pos h))))))))

(defun repl-on-first-input-line-p (r)
  (and (text-protect r) (= (text-cur-line r) (car (text-protect r)))))
(defun repl-on-last-line-p (r)
  (= (text-cur-line r) (1- (line-count r))))

(defmethod handle-event ((r trepl-view) event)
  (let ((k (event-key-code event))
        (focused (logtest (view-state r) +sf-focused+))
        (plain (zerop (event-modifiers event))))
    (cond
      ;; While a worker is evaluating, the buffer is read-only: swallow typing
      ;; (Ctrl-C / the Interrupt command still reach the app to abort the eval).
      ((and (repl-busy r) (= (event-type event) +ev-key-down+) focused)
       (clear-event event))
      ((and (= (event-type event) +ev-key-down+) focused plain (= k +kb-tab+)
            (can-edit-here-p r))
       (repl-complete r) (clear-event event))
      ((and (= (event-type event) +ev-key-down+) focused plain
            (= k +kb-up+) (repl-on-first-input-line-p r))
       (repl-history-recall r :prev) (clear-event event))
      ((and (= (event-type event) +ev-key-down+) focused plain
            (= k +kb-down+) (repl-on-last-line-p r) (repl-hist-pos r))
       (repl-history-recall r :next) (clear-event event))
      (t (call-next-method)))))

;;; --- convenience window ----------------------------------------------------

(defun make-repl-window (bounds &key (title "Lisp REPL") history-file)
  "Create a window containing a REPL view bound to a vertical scroll bar.
Return (values window repl-view)."
  (let* ((w (make-instance 'twindow :title title :bounds bounds))
         (vsb (standard-scrollbar w t))
         (rv (make-instance 'trepl-view :history-file history-file
                            :bounds (make-trect 1 1 (1- (point-x (view-size w)))
                                                (1- (point-y (view-size w)))))))
    (insert w rv)
    (text-attach-scrollbars rv :vscroll vsb)
    (values w rv)))
