;;;; host.lisp --- the shared event loop + a full-screen runner.
;;;;
;;;; Every window used to copy the same loop (drain bridge callbacks, repaint on
;;;; *DIRTY*, pump + dispatch input).  EVENT-LOOP is that loop, once; RUN-VIEW is
;;;; the standalone full-screen host.  Windows are now built by make-* builders
;;;; that return (values WINDOW FOCUS OPEN) — OPEN is an optional thunk run after
;;;; layout (to start background threads) that returns a cleanup thunk — so the
;;;; same window can be hosted full-screen (RUN-VIEW) or inside the DESKTOP.

(in-package #:tv2)

(defun event-loop (s root)
  "Drive ROOT until *RUNNING* becomes NIL.  The cursor is hidden every frame and
re-shown by whichever focused widget owns it (input-line / text-edit), so it
never lingers when focus moves to a non-text widget."
  (loop while *running* do
    (drain-ui-callbacks)
    (when *dirty*
      (tvision:hide-cursor s)
      (draw root) (tvision:flush-screen s) (setf *dirty* nil))
    (tvision::pump-input s 0.05)
    (let ((tev (tvision::screen-next-event s)))
      (when tev (let ((ev (translate tev))) (when ev (handle-event root ev)))))))

(defun run-view (win &key focus open)
  "Run WIN full-screen in its own screen session until it quits.  FOCUS is the
initial focused widget; OPEN (a thunk of the screen) may start background work
and return a cleanup thunk."
  (tvision:with-screen (s)
    (layout win (rect 0 0 (tvision:screen-width s) (tvision:screen-height s)))
    (setf *root* win
          (container-focus win) (or focus (first (all-focusables win)))
          *ui-thread* sb-thread:*current-thread* *running* t *dirty* t)
    (let ((cleanup (and open (funcall open s))))
      (unwind-protect (event-loop s win)
        (when cleanup (ignore-errors (funcall cleanup)))))))
