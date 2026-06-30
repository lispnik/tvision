;;;; modal.lisp --- modal dialogs that RETURN VALUES, with conditions/restarts
;;;; for validation (instead of TV's boolean Valid + error flags).

(in-package #:tv2)

;;; A failed field check signals VALIDATION-ERROR; the modal loop catches it,
;;; shows the message, and keeps the dialog open -- the condition drives the flow.
(define-condition validation-error (error)
  ((message :initarg :message :reader validation-message)))

(defun fail-validation (message) (error 'validation-error :message message))

(defclass dialog (window)
  ((result    :initform :cancel :accessor dialog-result)
   (done      :initform nil     :accessor dialog-done)
   (validator :initarg :validator :initform nil :accessor dialog-validator)  ; (dialog) -> t / signals
   (value-fn  :initarg :value-fn  :initform nil :accessor dialog-value-fn))   ; (dialog) -> result value
  (:metaclass reactive-class))

;; Draw dialogs (and their children) in the classic grey palette, with a shadow.
(defmethod draw :around ((d dialog))
  (let ((*theme* *dialog-theme*)) (call-next-method))
  (let ((b (view-bounds d)))
    (when b
      (%drop-shadow (tvision::rect-ax b) (tvision::rect-ay b)
                    (1- (tvision::rect-bx b)) (1- (tvision::rect-by b))))))

(defun %dialog-input-lines (d)
  (let ((out '()))
    (labels ((walk (v) (when (typep v 'input-line) (push v out))
               (when (typep v 'container) (mapc #'walk (subviews v)))))
      (walk d))
    (nreverse out)))

(defun %validate-fields (d)
  "Signal VALIDATION-ERROR for the first field whose validator's CHECK fails."
  (dolist (il (%dialog-input-lines d))
    (let ((vd (input-validator il)))
      (when (and vd (field-validator-check vd))
        (multiple-value-bind (ok msg) (funcall (field-validator-check vd) (input-text il))
          (unless ok (fail-validation (or msg " Invalid field. "))))))))

(define-command accept (v e)
  (let ((d (view-root v)))
    (when (typep d 'dialog)
      (handler-case
          (progn
            (%validate-fields d)                                          ; per-field validators
            (when (dialog-validator d) (funcall (dialog-validator d) d))   ; whole-dialog check
            (mapc #'input-remember (%dialog-input-lines d))                ; record field histories
            (setf (dialog-result d) (if (dialog-value-fn d) (funcall (dialog-value-fn d) d) t)
                  (dialog-done d) t))
        (validation-error (c)
          (let ((msg (find-view d 'msg)))
            (when msg (setf (static-text-text msg) (validation-message c)) (invalidate d))))))))

(define-command cancel (v e)
  (let ((d (view-root v)))
    (when (typep d 'dialog) (setf (dialog-result d) :cancel (dialog-done d) t))))

(defkeymap *dialog-keys* ()
  (:esc   cancel)
  (:enter accept))

(defun exec-view (dialog &key (width 48) (height 9))
  "Run DIALOG modally, centred over the current *ROOT* (drawn behind it), until it
finishes; return its result value, or :CANCEL."
  (let* ((s tvision:*screen*)
         (sw (tvision:screen-width s)) (sh (tvision:screen-height s))
         (x (max 0 (floor (- sw width) 2))) (y (max 0 (floor (- sh height) 2))))
    (layout dialog (rect x y (+ x width) (+ y height)))
    (setf (container-focus dialog) (first (all-focusables dialog))
          (dialog-done dialog) nil)
    (loop until (dialog-done dialog) do
      (drain-ui-callbacks)                 ; keep background threads (the clock) live
      (tvision:hide-cursor s)
      (when *root* (draw *root*))          ; background
      (draw dialog)                        ; modal on top (centred, smaller)
      (tvision:flush-screen s)
      (tvision::pump-input s 0.05)
      (let ((tev (tvision::screen-next-event s)))
        (when tev (let ((ev (translate tev))) (when ev (handle-event dialog ev))))))
    (invalidate *root*)                    ; force the background to repaint cleanly
    (dialog-result dialog)))

;;; --- a command that opens a modal dialog and uses its result ----------------

(define-command go-to-line (v e)
  (let* ((host (view-root v))
         (d (ui (dialog (:title " Go to line "
                          :keymap *dialog-keys*
                          :validator (lambda (d)
                                       (let ((n (parse-integer (input-text (find-view d 'num)) :junk-allowed t)))
                                         (unless (and n (plusp n))
                                           (fail-validation " Please enter a positive integer. "))))
                          :value-fn  (lambda (d)
                                       (parse-integer (input-text (find-view d 'num)) :junk-allowed t)))
                  (stack
                    (1 (row (15    (static-text :role :label :text " Line number: "))
                            (:fill (input-line :name 'num))))
                    (1 (static-text :name 'msg :role :error :text ""))
                    (:fill (static-text :text ""))
                    (1 (row (:fill (static-text :text ""))
                            (8  (button :label "OK"     :command 'accept))
                            (12 (button :label "Cancel" :command 'cancel))))))))
         (result (exec-view d :width 46 :height 9)))
    (unless (eq result :cancel)
      (let ((ol (find-view host 'tree)))
        (when (typep ol 'outline)
          (setf (outline-focused ol) (max 0 (1- result))
                (container-focus host) ol)        ; focus the outline so the jump is visible
          (ov-scroll-to-focus ol)
          (invalidate ol))))))
