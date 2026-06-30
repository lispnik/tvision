;;;; threadmon.lisp --- a real tvlisp window (the thread monitor) ported onto tv2.
;;;;
;;;; tvlisp's TTHREAD-WINDOW lists the live SBCL threads with Refresh/Kill and
;;;; auto-refreshes on idle.  Here it is rebuilt from tv2 parts: a LIST-BOX of
;;;; threads, BUTTONs bound to commands, a background refresher that posts updates
;;;; through the worker->UI bridge (real changing data, not a demo clock), and
;;;; reactive repaint -- no integer commands, no dispatch COND, no manual geometry.

(in-package #:tv2)

(defvar *tm-threads* '() "Thread objects shown in the list, captured at refresh.")
(defvar *worker-n* 0)

(defun %thread-label (th)
  (format nil "~:[dead~;live~]  ~a"
          (sb-thread:thread-alive-p th) (or (sb-thread:thread-name th) "?")))

(defun tm-refresh (lb)
  "Recapture the live threads into the list-box, keeping the selection in range."
  (setf *tm-threads* (sb-thread:list-all-threads)
        (list-items lb) (mapcar #'%thread-label *tm-threads*))
  (when (>= (list-selected lb) (length *tm-threads*))
    (setf (list-selected lb) (max 0 (1- (length *tm-threads*)))))
  (list-scroll-fix lb)
  (invalidate lb))

(defun %tm-list (root) (find-view root 'threads))
(defun %tm-echo (root v) (let ((e (find-view root 'echo))) (when e (setf (static-text-text e) v) (invalidate e))))

(define-command tm-spawn (v e)
  (push (sb-thread:make-thread (lambda () (sleep 600))
                               :name (format nil "worker-~d" (incf *worker-n*)))
        *tm-threads*)
  (let ((lb (%tm-list (view-root v)))) (when lb (tm-refresh lb)))
  (%tm-echo (view-root v) (format nil " spawned worker-~d " *worker-n*)))

(define-command tm-kill (v e)
  (let* ((root (view-root v)) (lb (%tm-list root))
         (th (and lb (< (list-selected lb) (length *tm-threads*))
                  (nth (list-selected lb) *tm-threads*)))
         (name (and th (or (sb-thread:thread-name th) "?"))))
    (cond
      ((null th))
      ((or (eq th sb-thread:*current-thread*) (search "main" name) (eql 0 (search "tv2" name)))
       (%tm-echo root (format nil " refused to kill ~a " name)))    ; protect UI / system threads
      (t (ignore-errors (sb-thread:terminate-thread th))
         (%tm-echo root (format nil " killed ~a " name))
         (when lb (tm-refresh lb))))))

(define-command tm-refresh-cmd (v e)
  (let ((lb (%tm-list (view-root v)))) (when lb (tm-refresh lb)))
  (%tm-echo (view-root v) " refreshed "))

(defun make-threadmon ()
  "Build a thread-monitor window.  Return (values WINDOW FOCUS OPEN); OPEN starts
the background refresher (keyed off the window, not *root*, so it works hosted)
and returns a cleanup thunk that stops it when the window closes."
  (let ((win (ui (window (:title " tv2 — Thread monitor (a real tvlisp window, ported) "
                          :keymap *global-keys*)
                   (stack
                     (1 (static-text :role :label :text " Live SBCL threads (auto-refreshing every 1.5s): "))
                     (:fill (list-box :name 'threads :items (mapcar #'%thread-label (sb-thread:list-all-threads))))
                     (1 (row (16 (button :label "Spawn worker" :command 'tm-spawn))
                             (8  (button :label "Kill"         :command 'tm-kill))
                             (12 (button :label "Refresh"      :command 'tm-refresh-cmd))
                             (:fill (static-text :name 'echo :role :status :text ""))))
                     (1 (static-text :role :status
                          :text " Tab/arrows · Spawn a worker, select it, Kill · live via run-on-ui · Esc: close ")))))))
    (setf *tm-threads* (sb-thread:list-all-threads))
    (values win (find-view win 'threads)
            (lambda (s) (declare (ignore s))
              (let ((alive t))
                (sb-thread:make-thread
                 (lambda () (loop while alive do
                              (sleep 1.5)
                              (run-on-ui (lambda () (let ((lb (%tm-list win))) (when lb (tm-refresh lb)))))))
                 :name "tv2-thread-refresher")
                (lambda () (setf alive nil)))))))   ; cleanup: stop the refresher

(defun run-threadmon ()
  "Run the ported thread monitor full-screen until q/Esc."
  (multiple-value-bind (w f o) (make-threadmon) (run-view w :focus f :open o)))
