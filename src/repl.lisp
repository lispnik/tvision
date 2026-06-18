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

(defun repl-backend-eval (input package error-handler)
  "Read+eval all forms in INPUT under PACKAGE, capturing output.  Maintains the
standard history vars (-, +/++/+++, */**/***, ///).  ERROR-HANDLER is invoked
with the condition inside HANDLER-BIND (it must transfer control).  Return
(values output-string results package errored)."
  (let ((*package* package) (results '()) (errored nil) (last nil))
    (let ((output
            (with-output-to-string (out)
              (let ((*standard-output* out) (*error-output* out) (*trace-output* out))
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
                  (repl-abort () (setf errored t)))
                (when (and errored last)
                  (format out "~&;; ~(~a~): ~a~%" (type-of last) last))))))
      (values output (nreverse results) *package* errored))))

;;; ===========================================================================
;;; The REPL view
;;; ===========================================================================

(defclass trepl-view (ttext-view)
  ((package      :initarg :package :initform nil :accessor repl-package)
   (history      :initform '() :accessor repl-history)      ; most-recent first
   (hist-pos     :initform nil :accessor repl-hist-pos)
   (history-file :initarg :history-file :initform nil :accessor repl-history-file)))

(defmethod initialize-instance :after ((r trepl-view) &key)
  (unless (repl-package r) (setf (repl-package r) (ensure-repl-package)))
  (when (repl-history-file r) (load-repl-history r))
  (repl-print r (repl-banner r))
  (repl-fresh-prompt r))

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

;;; --- restart menu (the micros/SLIME debugger feel) -------------------------

(defun repl-pick-restart (condition)
  "Show the available restarts for CONDITION; return the chosen restart or NIL."
  (when (and *repl-debugger* *application*)
    (let* ((restarts (compute-restarts condition))
           (labels (mapcar (lambda (rs) (format nil "~a" rs)) restarts))
           (desk (program-desktop *application*))
           (w 64) (h 17)
           (d (make-instance 'tdialog :title "Error — pick a restart"
                             :bounds (make-trect 0 0 w h)))
           (st (make-instance 'tstatic-text
                              :text (format nil "~(~a~):~%~a" (type-of condition) condition)
                              :bounds (make-trect 2 1 (- w 2) 5)))
           (vsb (standard-scrollbar d t))
           (lb (make-instance 'tlist-box :items labels :command +cm-ok+
                              :bounds (make-trect 2 6 (1- w) (- h 4)))))
      (insert d st) (insert d lb) (attach-scrollbars lb :vscroll vsb)
      (insert d (make-button (make-trect (- w 28) (- h 3) (- w 17) (- h 1)) "~I~nvoke" +cm-ok+ t))
      (insert d (make-button (make-trect (- w 14) (- h 3) (- w 3) (- h 1)) "Abort" +cm-cancel+))
      (move-to d (max 0 (floor (- (point-x (view-size desk)) w) 2))
               (max 0 (floor (- (point-y (view-size desk)) h) 2)))
      (focus lb)
      (when (and (= (exec-view desk d) +cm-ok+) restarts)
        (nth (list-focused lb) restarts)))))

(defun repl-error-handler (e)
  "HANDLER-BIND handler: offer restarts, then transfer control accordingly."
  (let ((chosen (repl-pick-restart e)))
    (invoke-restart (or chosen (find-restart 'repl-abort)))))

;;; --- evaluation + printing -------------------------------------------------

(defun repl-eval (r input)
  (multiple-value-bind (output results new-package errored)
      (repl-backend-eval input (repl-package r) #'repl-error-handler)
    (setf (repl-package r) new-package)        ; sticky in-package
    (values output results errored)))

(defmethod text-return ((r trepl-view))
  (let ((input (repl-current-input r)))
    (cond
      ((string-blank-p input)
       (append-text r (string #\Newline)) (repl-fresh-prompt r))
      ((not (input-complete-p input))
       (split-line-at-cursor r))
      (t
       (push (string-trim '(#\Space #\Tab #\Newline #\Return) input) (repl-history r))
       (setf (repl-hist-pos r) nil)
       (when (repl-history-file r) (save-repl-history r))
       (append-text r (string #\Newline))
       (multiple-value-bind (output results errored) (repl-eval r input)
         (when (plusp (length output))
           (repl-print r output) (repl-ensure-fresh-line r))
         (unless errored
           (if results
               (dolist (vals results)
                 (if vals
                     (dolist (v vals) (repl-print r (format nil "~s~%" v)))
                     (repl-print r (format nil "; No values~%"))))
               (repl-print r (format nil "; No values~%"))))
         (repl-fresh-prompt r))))))

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

(defun object->outline (obj label &optional (depth 3))
  "Build a depth-limited TOutline node tree describing OBJ."
  (let ((children '()))
    (when (plusp depth)
      (flet ((kid (v lbl) (push (object->outline v lbl (1- depth)) children)))
        (typecase obj
          (string nil)
          (cons (loop for x in obj for i from 0 below 200 do (kid x (format nil "[~d]" i))))
          (vector (loop for x across obj for i from 0 below 200 do (kid x (format nil "[~d]" i))))
          (hash-table
           (let ((i 0))
             (maphash (lambda (k v)
                        (when (< i 200) (kid v (format nil "~a =>" (%short-repr k))) (incf i)))
                      obj)))
          ((or structure-object standard-object)
           (dolist (slot (handler-case (sb-mop:class-slots (class-of obj)) (error () nil)))
             (let ((name (sb-mop:slot-definition-name slot)))
               (when (slot-boundp obj name)
                 (kid (slot-value obj name) (format nil "~a" name)))))))))
    (let ((node (make-outline-node (format nil "~a = ~a" label (%short-repr obj))
                                   (nreverse children))))
      (setf (outline-node-expanded node) t)
      node)))

(defun repl-inspect (obj &optional (label "value"))
  "Open an Inspector window showing OBJ as a collapsible tree."
  (when *application*
    (let* ((desk (program-desktop *application*))
           (w (make-instance 'twindow :title "Inspector"
                             :bounds (make-trect 4 2 (min 62 (point-x (view-size desk)))
                                                 (min 20 (point-y (view-size desk))))))
           (vsb (standard-scrollbar w t))
           (ol (make-instance 'toutline :roots (list (object->outline obj label))
                              :bounds (make-trect 1 1 (1- (point-x (view-size w)))
                                                  (1- (point-y (view-size w)))))))
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
  "LOAD PATH into the REPL package, echoing output into the transcript."
  (let ((*package* (repl-package r)))
    (let ((out (with-output-to-string (s)
                 (let ((*standard-output* s) (*error-output* s))
                   (handler-case (load path)
                     (error (e) (format s ";; ~a~%" e)))))))
      (repl-ensure-fresh-line r)
      (repl-print r (format nil "; loaded ~a~%" path))
      (when (plusp (length out)) (repl-print r out)))
    (setf (repl-package r) *package*))
  (repl-fresh-prompt r))

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
