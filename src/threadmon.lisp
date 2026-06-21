;;;; threadmon.lisp --- A refreshable thread monitor window.
;;;;
;;;; Lists the live SB-THREAD threads and supports operations on them (kill /
;;;; refresh).  Built on TLIST-BOX; the list owns the snapshot of threads so the
;;;; focused row maps back to a real thread object.  Useful for watching the
;;;; per-listener REPL worker threads (see repl.lisp) and killing a runaway one.

(in-package #:tvision)

(defparameter +cm-thread-refresh+   320)
(defparameter +cm-thread-kill+      321)
(defparameter +cm-thread-backtrace+ 325)
(defparameter +cm-thread-interrupt+ 326)

;;; --- the list --------------------------------------------------------------

(defclass tthread-list (tlist-box)
  ((threads :initform '() :accessor thread-list-threads)
   (status  :initarg :status :initform nil :accessor thread-list-status))
  (:documentation "A list box whose rows are the current threads, kept in sync
with a snapshot in THREADS so the focused row maps to a thread object."))

(defun %thread-marker (th)
  (cond ((eq th sb-thread:*current-thread*) "*")          ; the UI thread
        ((eq th (sb-thread:main-thread)) "M")
        ((sb-thread:thread-alive-p th) " ")
        (t "x")))                                          ; dead

(defun %thread-role (th)
  "A short state/role word for TH."
  (cond ((eq th sb-thread:*current-thread*) "ui")
        ((eq th (sb-thread:main-thread)) "main")
        ((not (sb-thread:thread-alive-p th)) "dead")
        ((let ((n (sb-thread:thread-name th)))
           (and n (search "repl" (string-downcase n)))) "worker")
        (t "live")))

(defun %thread-label (th)
  (format nil "~a ~a~vt[~a]"
          (%thread-marker th)
          (or (sb-thread:thread-name th) "(anonymous)")
          32 (%thread-role th)))

(defun thread-list-selected (tl)
  (nth (list-focused tl) (thread-list-threads tl)))

(defun thread-list-refresh (tl)
  "Re-query the running threads and rebuild the list, keeping the focused row."
  (let ((threads (sb-thread:list-all-threads))
        (keep (thread-list-selected tl)))            ; preserve selection across refresh
    (setf (thread-list-threads tl) threads)
    (list-set-items tl (mapcar #'%thread-label threads))
    (let ((i (and keep (position keep threads))))
      (when i (list-focus-item tl (min i (max 0 (1- (length threads)))))))
    (let ((st (thread-list-status tl)))
      (when st
        (setf (static-text-text st)
              (format nil " ~d thr  *=UI M=main x=dead  (Enter/B backtrace, I interrupt, K/Del kill, R refresh)"
                      (length threads)))
        (draw-view st)))
    (draw-view tl)))

(defun %thread-info-popup (title text)
  "Show TEXT in a read-only scrollable window on the desktop."
  (when *application*
    (let* ((desk (program-desktop *application*))
           (dw (point-x (view-size desk))) (dh (point-y (view-size desk)))
           (w (min 82 (- dw 2))) (h (min 24 (- dh 2)))
           (win (make-instance 'twindow :title title :bounds (make-trect 0 0 w h)))
           (vsb (standard-scrollbar win t))
           (m (make-instance 'tmemo :bounds (make-trect 1 1 (1- w) (1- h)))))
      (insert win m) (attach-scrollbars m :vscroll vsb)
      (set-text m text) (setf (text-read-only m) t)
      (move-to win (max 0 (floor (- dw w) 2)) (max 0 (floor (- dh h) 2)))
      (insert desk win) (focus m))))

(defun %thread-backtrace (th &optional (max 50))
  "Best-effort backtrace of TH as a string.  For another thread we interrupt it
to snapshot its stack, with a timeout so a wedged thread can't hang the UI."
  (flet ((self-bt () (with-output-to-string (s)
                       (ignore-errors (sb-debug:print-backtrace :count max :stream s)))))
    (cond
      ((not (sb-thread:thread-alive-p th)) "(thread is dead)")
      ((eq th sb-thread:*current-thread*) (self-bt))
      (t (let ((out nil) (done (sb-thread:make-semaphore)))
           (handler-case
               (progn
                 (sb-thread:interrupt-thread th
                   (lambda ()
                     (setf out (with-output-to-string (s)
                                 (ignore-errors (sb-debug:print-backtrace :count max :stream s))))
                     (sb-thread:signal-semaphore done)))
                 (if (sb-thread:wait-on-semaphore done :timeout 2)
                     (or out "(empty backtrace)")
                     "(timed out capturing the thread's backtrace)"))
             (error (e) (format nil "(could not capture backtrace: ~a)" e))))))))

(defun thread-list-backtrace (tl)
  "Show the focused thread's backtrace in a popup."
  (let ((th (thread-list-selected tl)))
    (when th
      (%thread-info-popup
       (format nil "Backtrace: ~a" (or (sb-thread:thread-name th) "(anonymous)"))
       (%thread-backtrace th)))))

(defun thread-list-interrupt (tl)
  "Soft-interrupt the focused thread: unwind to its ABORT restart (like Ctrl-C),
rather than terminating it.  Refuses the UI thread."
  (let ((th (thread-list-selected tl)))
    (when th
      (cond
        ((not (sb-thread:thread-alive-p th))
         (message-box "That thread is already dead." (logior +mf-information+ +mf-ok-button+)))
        ((eq th sb-thread:*current-thread*)
         (message-box "Refusing to interrupt the UI thread." (logior +mf-error+ +mf-ok-button+)))
        (t (ignore-errors
            (sb-thread:interrupt-thread th
              (lambda () (let ((r (find-restart 'abort))) (when r (invoke-restart r))))))
           (message-box "Interrupt sent." (logior +mf-information+ +mf-ok-button+))
           (thread-list-refresh tl))))))

(defun thread-list-kill (tl)
  "Terminate the focused thread, after confirmation; refuse the UI/main thread."
  (let ((th (thread-list-selected tl)))
    (when th
      (cond
        ((eq th sb-thread:*current-thread*)
         (message-box "Refusing to kill the UI thread." (logior +mf-error+ +mf-ok-button+)))
        ((eq th (sb-thread:main-thread))
         (message-box "Refusing to kill the main thread." (logior +mf-error+ +mf-ok-button+)))
        ((not (sb-thread:thread-alive-p th))
         (message-box "That thread is already dead." (logior +mf-information+ +mf-ok-button+))
         (thread-list-refresh tl))
        ((= +cm-yes+
            (message-box (format nil "Kill thread ~a?"
                                 (or (sb-thread:thread-name th) "(anonymous)"))
                         (logior +mf-warning+ +mf-yes-button+ +mf-no-button+)))
         (ignore-errors (sb-thread:terminate-thread th))
         (thread-list-refresh tl))))))

(defmethod handle-event ((tl tthread-list) event)
  (cond
    ((and (= (event-type event) +ev-key-down+)
          (logtest (view-state tl) +sf-focused+)
          (zerop (event-modifiers event))
          (= (event-key-code event) +kb-del+))
     (thread-list-kill tl) (clear-event event))
    ((and (= (event-type event) +ev-key-down+)
          (logtest (view-state tl) +sf-focused+)
          (zerop (event-modifiers event))
          (= (event-key-code event) +kb-enter+))
     (thread-list-backtrace tl) (clear-event event))
    ((and (= (event-type event) +ev-key-down+)
          (logtest (view-state tl) +sf-focused+)
          (zerop (event-modifiers event))
          (plusp (event-char-code event))
          (member (char-downcase (code-char (event-char-code event))) '(#\r #\k #\b #\i)))
     (ecase (char-downcase (code-char (event-char-code event)))
       (#\r (thread-list-refresh tl))
       (#\k (thread-list-kill tl))
       (#\b (thread-list-backtrace tl))
       (#\i (thread-list-interrupt tl)))
     (clear-event event))
    (t (call-next-method))))

;;; --- the window ------------------------------------------------------------

(defclass tthread-window (twindow)
  ((list :initform nil :accessor tw-list)))

(defmethod handle-event ((w tthread-window) event)
  (call-next-method)
  (when (and (= (event-type event) +ev-command+) (tw-list w))
    (let ((c (event-command event)))
      (cond
        ((= c +cm-thread-refresh+)   (thread-list-refresh (tw-list w))   (clear-event event))
        ((= c +cm-thread-kill+)      (thread-list-kill (tw-list w))      (clear-event event))
        ((= c +cm-thread-backtrace+) (thread-list-backtrace (tw-list w)) (clear-event event))
        ((= c +cm-thread-interrupt+) (thread-list-interrupt (tw-list w)) (clear-event event))))))

(defun make-thread-window (bounds &key (title "Threads"))
  "Build a refreshable thread-monitor window.  Return (values window list)."
  (let* ((w (make-instance 'tthread-window :title title :bounds bounds))
         (iw (point-x (view-size w))) (ih (point-y (view-size w)))
         (status (make-instance 'tstatic-text :text ""
                                :bounds (make-trect 1 (- ih 4) (1- iw) (- ih 3))))
         (vsb (standard-scrollbar w t))
         (tl (make-instance 'tthread-list :status status
                            :bounds (make-trect 1 1 (1- iw) (- ih 4)))))
    (insert w tl)
    (insert w status)
    (attach-scrollbars tl :vscroll vsb)
    (setf (tw-list w) tl)
    (insert w (make-button (make-trect 2 (- ih 3) 15 (- ih 1)) "~R~efresh" +cm-thread-refresh+ t))
    (insert w (make-button (make-trect 16 (- ih 3) 26 (- ih 1)) "~K~ill" +cm-thread-kill+ nil))
    (thread-list-refresh tl)
    (focus tl)
    (values w tl)))
