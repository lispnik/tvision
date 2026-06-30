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

(defun %capture-backtrace (&key (count 40))
  "Walk the live stack into a list of label strings (safe to show on the UI
thread).  Runs on the erroring thread, while its dynamic extent is intact."
  (let ((frames '()))
    (ignore-errors
     (let ((i 0))
       (do ((f (sb-di:top-frame) (sb-di:frame-down f)))
           ((or (null f) (>= i count)))
         (let* ((df (sb-di:frame-debug-fun f))
                (name (handler-case (sb-di:debug-fun-name df) (error () "?"))))
           (push (format nil "~2d  ~a" i (%ellipsize (princ-to-string name) 66)) frames))
         (incf i))))
    (nreverse frames)))

(defun %restart-report (r) (handler-case (princ-to-string r) (error () "")))
(defun %cond-line (c)
  (handler-case (format nil " ~(~a~): ~a" (type-of c) c) (error () " (unprintable condition)")))

(defun %debugger (condition restarts backtrace)
  "Modal restart picker.  Return (values RESTART-INDEX VALUE-STRING), or
(values NIL NIL) when the user aborts.  Runs on the UI thread."
  (let* ((descs (loop for r in restarts for i from 0
                      collect (format nil " ~d  ~@[[~(~a~)] ~]~a"
                                      i (restart-name r) (%restart-report r))))
         (d (ui (dialog (:title " Debugger "
                          :keymap *dialog-keys*
                          :value-fn (lambda (d) (list (list-selected (find-view d 'restarts))
                                                      (input-text (find-view d 'val)))))
                  (stack
                    (1 (static-text :name 'cond :role :error :text (%cond-line condition)))
                    (1 (static-text :role :label :text " Restarts — ↑/↓ then Enter to invoke: "))
                    (6 (list-box :name 'restarts :items descs
                         :on-activate (lambda (lb item) (declare (ignore item))
                                        (perform 'accept lb nil))))
                    (1 (static-text :role :label :text " Backtrace (Tab to focus & scroll): "))
                    (:fill (scrollback :name 'bt))
                    (1 (row (16 (static-text :role :label :text " use-value form: "))
                            (:fill (input-line :name 'val))))
                    (1 (static-text :role :status
                         :text " Enter: invoke selected restart · Tab: focus backtrace/value · Esc: abort ")))))))
    (let ((bt (find-view d 'bt)))
      (dolist (line backtrace) (scrollback-append bt (concatenate 'string line (string #\Newline)))))
    (let ((result (exec-view d :width 78 :height 21)))
      (if (eq result :cancel) (values nil nil) (values (first result) (second result))))))
