;;;; compile.lisp --- compile / load a buffer, and interrupt a running eval.
;;;;
;;;; All built on the ported REPL: eval-defun reuses the editor eval hook; load
;;;; and compile submit (load …)/(compile-file …) on the worker so warnings and
;;;; notes stream into the transcript; interrupt aborts a runaway evaluation by
;;;; signalling the worker thread into its repl-abort restart.

(in-package #:tv2)

(defun %code-repl ()
  "Raise (opening if needed) the desktop REPL and return it."
  (let ((r (ensure-repl)))
    (when (and r *desktop*) (dt-raise *desktop* r) (invalidate *desktop*))
    r))

(defun do-eval-defun ()
  "Evaluate (SBCL compiles as it evaluates) the top-level form at the cursor."
  (let ((te (%focused-editor)))
    (when (and te *editor-eval-fn*) (funcall *editor-eval-fn* te))))

(defun %code-submit (r input)
  "Submit INPUT to the REPL, telling the user when it can't run (REPL busy)."
  (if (repl-busy r)
      (%tool-note "REPL is busy — interrupt or wait for the current evaluation")
      (repl-submit-string r input)))

(defun do-load-buffer ()
  "Evaluate every form in the focused editor's buffer in the REPL."
  (let ((te (%focused-editor)) (r (%code-repl)))
    (when (and te r) (%code-submit r (te-text te)))))

(defun do-compile-buffer ()
  "COMPILE-FILE the focused editor's file (saving first), streaming compiler
warnings/notes into the REPL."
  (let ((te (%focused-editor)) (r (%code-repl)))
    (when (and te r)
      (cond
        ((null (te-filename te))
         (%open-output " Compile buffer " "Save the buffer to a file first (compile-file needs a path)."))
        (t (handler-case (when (te-modified te) (te-save te))
             (error (e) (return-from do-compile-buffer
                          (%open-output " Compile buffer " (format nil "Could not save the buffer:~%~a" e)))))
           (%code-submit r (format nil "(compile-file ~s)" (namestring (te-filename te)))))))))

(defun do-interrupt-eval ()
  "Abort a running REPL evaluation by signalling the worker into repl-abort."
  (let ((r (and *desktop* (find :repl (dt-windows *desktop*) :key #'window-kind))))
    (if (and r (repl-busy r) (repl-worker r) (sb-thread:thread-alive-p (repl-worker r)))
        (sb-thread:interrupt-thread
         (repl-worker r)
         (lambda () (let ((res (find-restart 'repl-abort))) (when res (invoke-restart res)))))
        (%tool-note "nothing is evaluating"))))

;;; --- compiler notes: capture, gutter markers, navigable list ----------------
;;; SBCL is unique in signalling sb-ext:compiler-note conditions (boxing, generic
;;; arithmetic, failed inlining...).  We compile the buffer capturing them, mark
;;; the offending lines in the gutter, and list them navigably (Enter jumps).

(defun %note-symchar-p (ch) (or (alphanumericp ch) (find ch "*+/<>=!?%&._-")))

(defun %note-message-symbols (message)
  "Uppercase whole-word tokens in MESSAGE (candidate offending symbol names)."
  (let ((toks '()) (i 0) (n (length message)))
    (loop while (< i n) do
      (if (%note-symchar-p (char message i))
          (let ((j i))
            (loop while (and (< j n) (%note-symchar-p (char message j))) do (incf j))
            (let ((tok (subseq message i j)))
              (when (and (> (length tok) 1) (find-if #'upper-case-p tok) (not (find-if #'lower-case-p tok)))
                (push (let ((c (position #\: tok :from-end t))) (if c (subseq tok (1+ c)) tok)) toks)))
            (setf i j))
          (incf i)))
    (nreverse toks)))

(defun %note-search-token (token text start)
  "Offset of TOKEN in TEXT at/after START as a whole symbol token, or NIL."
  (let* ((tk (string-downcase token)) (low (string-downcase text))
         (n (length text)) (tl (length token)) (i start))
    (loop for p = (search tk low :start2 (min i n)) while p do
      (let ((before (and (> p 0) (char low (1- p)))) (after (and (< (+ p tl) n) (char low (+ p tl)))))
        (if (and (or (null before) (not (%note-symchar-p before))) (or (null after) (not (%note-symchar-p after))))
            (return p) (setf i (1+ p)))))))

(defun %note-refine-offset (text pos message)
  "Refine a top-level-form offset POS to the offending symbol named in MESSAGE."
  (let ((start (max 0 (min pos (length text)))) (best nil))
    (dolist (tok (%note-message-symbols message))
      (let ((p (%note-search-token tok text start)))
        (when (and p (or (null best) (< p best))) (setf best p))))
    (or best start)))

(defun %compile-text-notes (text pkg)
  "Compile TEXT (read in PKG) from a temp file; return (values STATUS NOTES),
STATUS :ok or an error string, each note (:severity KW :pos INT :message STR)."
  (let ((src (format nil "/tmp/tv2-cn-~36r.lisp" (get-internal-real-time)))
        (notes '())
        (fec  (find-symbol "FIND-ERROR-CONTEXT" :sb-c))
        (cefp (find-symbol "COMPILER-ERROR-CONTEXT-FILE-POSITION" :sb-c)))
    (unwind-protect
         (handler-case
             (progn
               (with-open-file (s src :direction :output :if-exists :supersede
                                      :if-does-not-exist :create :external-format :utf-8)
                 (write-string text s))
               (flet ((grab (c sev)
                        (let* ((ctx (and fec (ignore-errors (funcall fec nil))))
                               (pos (or (and ctx cefp (ignore-errors (funcall cefp ctx))) 0)))
                          (push (list :severity sev :pos (or pos 0) :message (princ-to-string c)) notes))
                        (when (find-restart 'muffle-warning c) (muffle-warning c))))
                 (handler-bind ((style-warning        (lambda (c) (grab c :style)))
                                (sb-ext:compiler-note (lambda (c) (grab c :note)))
                                (warning              (lambda (c) (grab c :warning))))
                   (let ((*package* pkg) (*error-output* (make-broadcast-stream))
                         (*standard-output* (make-broadcast-stream)))
                     (with-compilation-unit (:override t)
                       (compile-file src :verbose nil :print nil)))))
               (values :ok (nreverse notes)))
           (error (e) (values (princ-to-string e) (nreverse notes))))
      (ignore-errors (delete-file src))
      (ignore-errors (delete-file (compile-file-pathname src))))))

(defstruct (cnote (:constructor %cnote (severity line message offset)))
  severity line message offset)

(defun %note-jump (te n)
  "Move TE's cursor to note N's offset and focus the editor."
  (multiple-value-bind (line col) (te-pos-at-offset te (cnote-offset n))
    (setf (te-cy te) line (te-cx te) col)
    (te-ensure-visible te) (invalidate te)
    (let ((w (view-root te)))
      (when (and *desktop* (typep w 'window))
        (dt-raise *desktop* w) (dt-refocus *desktop*) (setf (container-focus w) te)))))

(defun %show-compile-notes (te status name notes)
  "Set TE's gutter markers from NOTES and open a navigable notes window."
  (let* ((text (te-text te))
         (rows (mapcar (lambda (nt)
                         (let* ((off  (%note-refine-offset text (getf nt :pos) (getf nt :message)))
                                (line (values (te-pos-at-offset te off))))
                           (%cnote (getf nt :severity) line (getf nt :message) off)))
                       notes)))
    (setf (te-notes te) (mapcar (lambda (n) (cons (cnote-line n) (cnote-severity n))) rows))
    (invalidate te)
    (cond
      ((stringp status) (%open-output " Compile notes " (format nil "compile error:~%~a" status)))
      ((null rows) (%open-output " Compile notes " (format nil "Compiled ~a cleanly — no notes." name)))
      ((null *desktop*) nil)
      (t (dt-open *desktop*
                  (lambda ()
                    (make-table-window
                     (format nil " Compiler notes: ~a (~d) " name (length rows))
                     (list (list "Severity" 9 (lambda (n) (string-downcase (symbol-name (cnote-severity n)))))
                           (list "Line" 5 (lambda (n) (princ-to-string (1+ (cnote-line n)))))
                           (list "Message" 90 #'cnote-message))
                     rows
                     :on-activate (lambda (tv row) (declare (ignore tv)) (%note-jump te row)))))))))

(defun do-compile-notes ()
  "Compile the focused buffer (without loading), capturing SBCL's compiler notes:
mark the offending lines in the gutter and open a navigable notes list."
  (let ((te (%focused-editor)))
    (if (null te)
        (%tool-note "Focus an editor window first.")
        (let ((text (te-text te)) (pkg (%active-package))
              (name (if (te-filename te) (file-namestring (te-filename te)) "buffer")))
          (%tool-note (format nil "compiling ~a for notes …" name))
          (sb-thread:make-thread
           (lambda ()
             (handler-case
                 (multiple-value-bind (status notes) (%compile-text-notes text pkg)
                   (run-on-ui (lambda () (%show-compile-notes te status name notes))))
               (error (e) (run-on-ui (lambda () (%open-output " Compile notes " (princ-to-string e)))))))
           :name "tv2-compile-notes")))))

(defun do-clear-notes ()
  "Clear the compiler-note gutter markers in the focused editor."
  (let ((te (%focused-editor))) (when te (setf (te-notes te) nil) (invalidate te))))

(push (lambda (dt)
        (declare (ignore dt))
        (list "Run"
              (list "Eval / compile defun" (lambda () (do-eval-defun)))
              (list "Load buffer"          (lambda () (do-load-buffer)))
              (list "Compile buffer"       (lambda () (do-compile-buffer)))
              :--
              (list "Compiler notes…"      (lambda () (do-compile-notes)))
              (list "Clear notes"          (lambda () (do-clear-notes)))
              :--
              (list "Interrupt eval"       (lambda () (do-interrupt-eval)))))
      *extra-menus*)
