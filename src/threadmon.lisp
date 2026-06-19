;;;; threadmon.lisp --- A refreshable thread monitor window.
;;;;
;;;; Lists the live SB-THREAD threads and supports operations on them (kill /
;;;; refresh).  Built on TLIST-BOX; the list owns the snapshot of threads so the
;;;; focused row maps back to a real thread object.  Useful for watching the
;;;; per-listener REPL worker threads (see repl.lisp) and killing a runaway one.

(in-package #:tvision)

(defparameter +cm-thread-refresh+ 320)
(defparameter +cm-thread-kill+    321)

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

(defun %thread-label (th)
  (format nil "~a ~a~@[ ~a~]"
          (%thread-marker th)
          (or (sb-thread:thread-name th) "(anonymous)")
          (unless (sb-thread:thread-alive-p th) "[dead]")))

(defun thread-list-selected (tl)
  (nth (list-focused tl) (thread-list-threads tl)))

(defun thread-list-refresh (tl)
  "Re-query the running threads and rebuild the list."
  (let ((threads (sb-thread:list-all-threads)))
    (setf (thread-list-threads tl) threads)
    (list-set-items tl (mapcar #'%thread-label threads))
    (let ((st (thread-list-status tl)))
      (when st
        (setf (static-text-text st)
              (format nil " ~d thread~:p  *=UI M=main x=dead  (R refresh, K/Del kill)"
                      (length threads)))
        (draw-view st)))
    (draw-view tl)))

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
          (plusp (event-char-code event))
          (member (char-downcase (code-char (event-char-code event))) '(#\r #\k)))
     (ecase (char-downcase (code-char (event-char-code event)))
       (#\r (thread-list-refresh tl))
       (#\k (thread-list-kill tl)))
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
        ((= c +cm-thread-refresh+) (thread-list-refresh (tw-list w)) (clear-event event))
        ((= c +cm-thread-kill+)    (thread-list-kill (tw-list w))    (clear-event event))))))

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
