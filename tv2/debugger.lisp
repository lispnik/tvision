;;;; debugger.lisp --- an SLDB-style restart picker, ported onto tv2.
;;;;
;;;; tvlisp's listener routes a worker-thread error to the UI thread, shows the
;;;; condition + restarts + a live backtrace, and invokes the chosen restart back
;;;; on the worker -- whose stack is still live because it stays blocked waiting
;;;; for the choice (the SLIME cross-thread debugger model).  This port keeps
;;;; that protocol and renders it as a tv2 modal dialog.  (Deferred from the full
;;;; engine: per-frame locals, return-from-frame / restart-frame ops.)

(in-package #:tv2)

(defun %ellipsize (s n)
  (if (> (length s) n) (concatenate 'string (subseq s 0 (1- n)) "…") s))

(defun %frame-locals (frame df loc)
  "Each valid local as (NAME DISPLAY-STRING); plain data, safe to show on the UI
thread.  Guarded so it degrades when debug info is thin."
  (let ((out '()))
    (handler-case
        (sb-di:do-debug-fun-vars (v df)
          (when (and loc (eq (handler-case (sb-di:debug-var-validity v loc) (error () :invalid)) :valid))
            (push (list (string-downcase (symbol-name (sb-di:debug-var-symbol v)))
                        (handler-case (let ((*print-length* 6) (*print-level* 3) (*print-readably* nil))
                                        (prin1-to-string (sb-di:debug-var-value v frame)))
                          (error () "#<unavailable>")))
                  out)))
      (error () nil))
    (nreverse out)))

(defun %frame-source (frame)
  "A short source file name for FRAME, or NIL (resolved via FIND-SYMBOL so no
sb-di internals are hard-referenced at build time)."
  (let ((dsns (find-symbol "DEBUG-SOURCE-NAMESTRING" :sb-di))
        (clds (find-symbol "CODE-LOCATION-DEBUG-SOURCE" :sb-di)))
    (ignore-errors
      (let* ((loc (sb-di:frame-code-location frame))
             (ds  (and loc (fboundp clds) (funcall clds loc)))
             (ns  (and ds (fboundp dsns) (funcall dsns ds))))
        (and ns (file-namestring ns))))))

(defun %capture-backtrace (&key (count 40))
  "Walk the live stack ONCE.  Return (values FRAMES LIVES): FRAMES is a list of
plists (:label :source :locals) -- plain data safe to show on the UI thread --
and LIVES is the index-aligned list of live SB-DI:FRAME objects, valid only while
the erroring thread stays blocked (so frame ops can act on them)."
  (let ((frames '()) (lives '()))
    (ignore-errors
     (let ((i 0))
       (do ((f (sb-di:top-frame) (sb-di:frame-down f)))
           ((or (null f) (>= i count)))
         (let* ((df (sb-di:frame-debug-fun f))
                (name (handler-case (sb-di:debug-fun-name df) (error () "?")))
                (loc (handler-case (sb-di:frame-code-location f) (error () nil))))
           (push (list :label (format nil "~2d  ~a" i (%ellipsize (princ-to-string name) 58))
                       :source (%frame-source f)
                       :locals (%frame-locals f df loc))
                 frames)
           (push f lives))
         (incf i))))
    (values (nreverse frames) (nreverse lives))))

(defun %frame-return (lives index form-string package)
  "Frame op: unwind the live stack to frame INDEX and make it return the values of
FORM-STRING (read+eval'd in PACKAGE).  Uses SBCL's internal unwinder via
FIND-SYMBOL; falls back to REPL-ABORT when the frame has no debug tag or the form
fails.  Does not return normally on success."
  (let ((frame   (and lives (nth index lives)))
        (unwind  (find-symbol "UNWIND-TO-FRAME-AND-CALL" :sb-debug))
        (has-tag (find-symbol "FRAME-HAS-DEBUG-TAG-P" :sb-debug))
        (abort   (lambda () (invoke-restart (find-restart 'repl-abort)))))
    (if (and frame (fboundp unwind) (plusp (length (or form-string "")))
             (or (not (fboundp has-tag)) (ignore-errors (funcall has-tag frame))))
        (let* ((*package* (or package *package*))
               (vals (handler-case (multiple-value-list (eval (read-from-string form-string)))
                       (error () :error))))
          (if (eq vals :error) (funcall abort)
              (funcall unwind frame (lambda () (values-list vals)))))
        (funcall abort))))

(defun %restart-report (r) (handler-case (princ-to-string r) (error () "")))
(defun %cond-line (c)
  (handler-case (format nil " ~(~a~): ~a" (type-of c) c) (error () " (unprintable condition)")))

(defun %dbg-show-locals (d frames idx)
  "Refresh the locals panel for the selected backtrace frame."
  (let ((sb (find-view d 'locals)) (f (and (< idx (length frames)) (nth idx frames))))
    (when sb
      (scrollback-clear sb)
      (when f
        (when (getf f :source) (scrollback-append sb (format nil "source: ~a~%" (getf f :source))))
        (if (getf f :locals)
            (dolist (l (getf f :locals)) (scrollback-append sb (format nil "  ~a = ~a~%" (first l) (second l))))
            (scrollback-append sb "  (no visible locals)~%"))))))

(defun %debugger (condition restarts frames)
  "Modal SLDB.  FRAMES is the captured backtrace (plists).  Return the chosen
action: (:restart INDEX VALUE) | (:frame-return INDEX FORM) | NIL (abort).
Runs on the UI thread."
  (let* ((descs (loop for r in restarts for i from 0
                      collect (format nil " ~d  ~@[[~(~a~)] ~]~a" i (restart-name r) (%restart-report r))))
         (labels* (loop for f in frames collect (concatenate 'string " " (getf f :label))))
         (d (ui (dialog (:title " Debugger "
                          :keymap *dialog-keys*
                          :value-fn (lambda (d)
                                      (let ((bt (find-view d 'bt)) (val (input-text (find-view d 'val))))
                                        (if (eq (container-focus d) bt)         ; focus decides the action
                                            (list :frame-return (list-selected bt) val)
                                            (list :restart (list-selected (find-view d 'restarts)) val)))))
                  (stack
                    (1 (static-text :name 'cond :role :error :text (%cond-line condition)))
                    (1 (static-text :role :label :text " Restarts — ↑/↓, Enter to invoke: "))
                    (4 (list-box :name 'restarts :items descs
                         :on-activate (lambda (lb item) (declare (ignore item)) (perform 'accept lb nil))))
                    (1 (static-text :role :label :text " Backtrace — Tab here; Enter returns the form below FROM that frame: "))
                    (5 (list-box :name 'bt :items labels*
                         :on-select   (lambda (lb) (%dbg-show-locals (view-root lb) frames (list-selected lb)))
                         :on-activate (lambda (lb item) (declare (ignore item)) (perform 'accept lb nil))))
                    (1 (static-text :role :label :text " Frame locals: "))
                    (4 (scrollback :name 'locals))
                    (1 (row (18 (static-text :role :label :text " value / return form: "))
                            (:fill (input-line :name 'val))))
                    (1 (static-text :role :status
                         :text " Enter: invoke restart / return-from-frame (by focus) · Tab: switch · Esc: abort ")))))))
    (%dbg-show-locals d frames 0)
    (let ((result (exec-view d :width 82 :height 22)))
      (if (eq result :cancel) nil result))))
