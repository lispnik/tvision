;;;; repl.lisp --- TReplView: a Lisp read-eval-print loop in a text view.
;;;;
;;;; Built on TTextView.  Output and the current prompt are kept read-only via
;;;; the protected-region boundary; everything the user types after the last
;;;; prompt is the input.  Enter evaluates a complete form (or inserts a newline
;;;; if the form is still open), captures printed output, prints the values, and
;;;; writes a fresh prompt.  Up/Down recall input history at the prompt edges.

(in-package #:tvision)

(defun ensure-repl-package ()
  (or (find-package :tv-repl-user)
      (make-package :tv-repl-user :use '(:common-lisp) :nicknames '("REPL"))))

(defclass trepl-view (ttext-view)
  ((package  :initarg :package :initform nil :accessor repl-package)
   (history  :initform '() :accessor repl-history)      ; most-recent first
   (hist-pos :initform nil :accessor repl-hist-pos)))

(defmethod initialize-instance :after ((r trepl-view) &key)
  (unless (repl-package r) (setf (repl-package r) (ensure-repl-package)))
  (repl-print r (repl-banner r))
  (repl-fresh-prompt r))

(defun repl-banner (r)
  (declare (ignore r))
  (format nil "; Turbo Vision Lisp REPL on SBCL ~a~%~
; Enter evaluates a complete form; an open form continues on the next line.~%~
; Up/Down recall history.  *, **, *** hold recent values.~%~%"
          (lisp-implementation-version)))

(defun repl-prompt-string (r)
  (format nil "~a> " (or (first (package-nicknames (repl-package r)))
                         (package-name (repl-package r)))))

(defun repl-print (r string)
  "Append output text to the transcript."
  (append-text r string))

(defun repl-last-line-empty-p (r)
  (zerop (length (nth-line r (1- (line-count r))))))

(defun repl-ensure-fresh-line (r)
  (unless (repl-last-line-empty-p r)
    (append-text r (string #\Newline))))

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
    (error () t)))   ; other reader errors are "complete" -- eval will report them

;;; --- evaluation ------------------------------------------------------------

(defun repl-eval (r input)
  "Evaluate INPUT in the REPL package.  Return (values output results errored),
where RESULTS is a list of per-form value-lists."
  (let ((*package* (repl-package r))
        (results '())
        (errored nil))
    (let ((output
            (with-output-to-string (out)
              (let ((*standard-output* out) (*error-output* out) (*trace-output* out))
                (handler-case
                    (with-input-from-string (in input)
                      (loop for form = (read in nil :repl-eof)
                            until (eq form :repl-eof)
                            do (let ((vals (multiple-value-list (eval form))))
                                 (push vals results)
                                 ;; expose recent values like a normal REPL
                                 (setf *** ** ** * * (first vals)))))
                  (error (e)
                    (setf errored t)
                    (format out "~&;; ~(~a~): ~a" (type-of e) e)))))))
      (values output (nreverse results) errored))))

(defmethod text-return ((r trepl-view))
  (let ((input (repl-current-input r)))
    (cond
      ((string-blank-p input)
       (append-text r (string #\Newline))
       (repl-fresh-prompt r))
      ((not (input-complete-p input))
       ;; form still open: just break the line and keep editing
       (split-line-at-cursor r))
      (t
       (push (string-trim '(#\Space #\Tab #\Newline #\Return) input) (repl-history r))
       (setf (repl-hist-pos r) nil)
       (append-text r (string #\Newline))          ; end the input line
       (multiple-value-bind (output results errored) (repl-eval r input)
         (when (plusp (length output))
           (repl-print r output)
           (repl-ensure-fresh-line r))
         (unless errored
           (if results
               (dolist (vals results)
                 (if vals
                     (dolist (v vals) (repl-print r (format nil "~s~%" v)))
                     (repl-print r (format nil "; No values~%"))))
               (repl-print r (format nil "; No values~%"))))
         (repl-fresh-prompt r))))))

;;; --- input history (Up/Down at the prompt edges) ---------------------------

(defun repl-replace-input (r string)
  "Replace the current input (after the prompt) with STRING."
  (let* ((p (text-protect r)) (pl (car p)) (pc (cdr p)))
    (setf (fill-pointer (text-lines r)) (1+ pl))      ; drop continuation lines
    (set-line r pl (subseq (nth-line r pl) 0 pc))     ; keep the prompt prefix
    (setf (text-cur-line r) pl (text-cur-col r) pc)
    (insert-string r string)
    (text-update-limit r)
    (ensure-visible r)
    (draw-view r)))

(defun repl-history-recall (r dir)
  (let* ((h (repl-history r)) (n (length h)))
    (when (plusp n)
      (let ((pos (ecase dir
                   (:prev (if (null (repl-hist-pos r)) 0
                              (min (1- n) (1+ (repl-hist-pos r)))))
                   (:next (if (null (repl-hist-pos r)) -1
                              (1- (repl-hist-pos r)))))))
        (if (minusp pos)
            (progn (setf (repl-hist-pos r) nil) (repl-replace-input r ""))
            (progn (setf (repl-hist-pos r) pos) (repl-replace-input r (nth pos h))))))))

(defun repl-on-first-input-line-p (r)
  (and (text-protect r) (= (text-cur-line r) (car (text-protect r)))))
(defun repl-on-last-line-p (r)
  (= (text-cur-line r) (1- (line-count r))))

(defmethod handle-event ((r trepl-view) event)
  (if (and (= (event-type event) +ev-key-down+)
           (logtest (view-state r) +sf-focused+)
           (zerop (event-modifiers event))
           (or (and (= (event-key-code event) +kb-up+) (repl-on-first-input-line-p r))
               (and (= (event-key-code event) +kb-down+) (repl-on-last-line-p r)
                    (repl-hist-pos r))))
      (progn
        (repl-history-recall r (if (= (event-key-code event) +kb-up+) :prev :next))
        (clear-event event))
      (call-next-method)))

;;; --- convenience window ----------------------------------------------------

(defun make-repl-window (bounds &key (title "Lisp REPL"))
  "Create a window containing a REPL view bound to a vertical scroll bar.
Return (values window repl-view)."
  (let* ((w (make-instance 'twindow :title title :bounds bounds))
         (vsb (standard-scrollbar w t))
         (rv (make-instance 'trepl-view
                            :bounds (make-trect 1 1
                                                (1- (point-x (view-size w)))
                                                (1- (point-y (view-size w)))))))
    (insert w rv)
    (text-attach-scrollbars rv :vscroll vsb)
    (values w rv)))
