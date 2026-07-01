;;;; dialogs.lisp --- the standard dialogs: file picker, change-dir, colours.
;;;;
;;;; Each composes EXEC-VIEW with the existing controls (input-line, list-box,
;;;; cluster, button) and returns a value, mirroring tvision's TFileDialog /
;;;; TChDirDialog / TColorDialog.

(in-package #:tv2)

;;; --- file / directory picker ------------------------------------------------

(defun %dir-entries (dir dirs-only)
  "Listing of DIR: \"../\", then subdirectories (with a trailing /), then files."
  (let ((subs (sort (mapcar (lambda (p) (format nil "~a/" (car (last (pathname-directory p)))))
                            (ignore-errors (uiop:subdirectories dir))) #'string<))
        (files (unless dirs-only
                 (sort (mapcar #'file-namestring (ignore-errors (uiop:directory-files dir))) #'string<))))
    (append (list "../") subs files)))

(defun make-file-dialog (&key (dir (uiop:getcwd)) dirs-only (title " Open file "))
  "Modal file/directory picker.  Navigate by activating directory rows; activate
a file (or click Open) to choose it.  Return a pathname, or NIL on cancel."
  (let ((cur (list (uiop:ensure-directory-pathname dir))))
    (labels ((refill (d)
               (let ((lb (find-view d 'files)) (inp (find-view d 'path)) (ns (namestring (car cur))))
                 (setf (list-items lb) (%dir-entries (car cur) dirs-only)
                       (list-selected lb) 0 (list-top lb) 0
                       (input-text inp) ns (input-caret inp) (length ns))
                 (invalidate d)))
             (activate (lb item)
               (let ((d (view-root lb)))
                 (cond
                   ((string= item "../")
                    (setf (car cur) (uiop:pathname-parent-directory-pathname (car cur))) (refill d))
                   ((and (plusp (length item)) (char= (char item (1- (length item))) #\/))
                    (setf (car cur) (uiop:ensure-directory-pathname
                                     (merge-pathnames (subseq item 0 (1- (length item))) (car cur))))
                    (refill d))
                   (t (setf (input-text (find-view d 'path)) (namestring (merge-pathnames item (car cur))))
                      (perform 'accept lb nil))))))
      (let ((d (ui (dialog (:title title :keymap *dialog-keys*
                            :value-fn (lambda (d) (input-text (find-view d 'path))))
                     (stack
                       (1 (row (7 (static-text :role :label :text " Path: "))
                               (:fill (input-line :name 'path :history-id :file))))
                       (:fill (list-box :name 'files :on-activate #'activate))
                       (1 (row (:fill (static-text :text ""))
                               (8  (button :label "Open"   :command 'accept))
                               (12 (button :label "Cancel" :command 'cancel)))))))))
        (refill d)
        (let ((r (exec-view d :width 66 :height 20)))
          (if (eq r :cancel) nil (ignore-errors (pathname r))))))))

;;; --- colour customiser (visual swatches + live preview of *THEME*) ----------

(defparameter *color-roles*
  '(:normal :focused :frame :menu-bar :menu-selected :status :label :desktop :button))

;;; A row of colour swatches (0..COUNT-1): ←/→ or click selects; a ▲ marks the
;;; choice.  BG-P shows them as background blocks, else foreground blocks.
(defclass color-swatches (view)
  ((count :initarg :count :initform 16 :accessor sw-count)
   (value :initarg :value :initform 0 :accessor sw-value)
   (bg-p  :initarg :bg-p  :initform nil :accessor sw-bg-p)
   (on-change :initarg :on-change :initform nil :accessor sw-on-change))
  (:metaclass reactive-class))

(defmethod focusable-p ((v color-swatches)) t)
(defun sw-notify (v) (when (sw-on-change v) (funcall (sw-on-change v) v)))

(defmethod draw ((v color-swatches))
  (let* ((b (view-bounds v)) (ax (tvision::rect-ax b)) (ay (tvision::rect-ay b)) (w (r-w b))
         (foc (view-focused-p v)))
    (fill-row v 0 0 w (role :label)) (fill-row v 0 1 w (role :label))
    (dotimes (i (sw-count v))
      (let ((cx (* i 3)))
        (when (<= (+ cx 2) w)
          (let ((cattr (if (sw-bg-p v) (tvision:make-attr 0 i) (tvision:make-attr i 0)))
                (ch    (if (sw-bg-p v) #\Space #\█)))
            (%put-cell (+ ax cx) ay ch cattr) (%put-cell (+ ax cx 1) ay ch cattr))
          (when (= i (sw-value v))                    ; marker under the chosen swatch
            (let ((m (if foc #\▲ #\·)))
              (%put-cell (+ ax cx) (1+ ay) m (role :label)) (%put-cell (+ ax cx 1) (1+ ay) m (role :label)))))))))

(defmethod handle-event ((v color-swatches) (e key-event))
  (case (event-keysym e)
    (:left  (setf (sw-value v) (mod (1- (sw-value v)) (sw-count v))) (sw-notify v) (setf (handled-p e) t))
    (:right (setf (sw-value v) (mod (1+ (sw-value v)) (sw-count v))) (sw-notify v) (setf (handled-p e) t))
    (t (call-next-method))))

(defmethod handle-event ((v color-swatches) (e mouse-down))
  (let ((i (floor (mouse-col v e) 3)))
    (when (< i (sw-count v)) (setf (sw-value v) i) (sw-notify v)))
  (setf (handled-p e) t))

;;; A swatch showing sample text in the currently-chosen fg/bg attribute.
(defclass color-preview (view)
  ((attr :initform (tvision:make-attr 7 1) :accessor cp-attr))
  (:metaclass reactive-class))
(defmethod draw ((v color-preview))
  (fill-row v 0 0 (r-w (view-bounds v)) (cp-attr v))
  (draw-text v 1 0 " Sample — the quick brown fox  AaBbCc 0123 " (cp-attr v)))

(defun %color-refresh-preview (d)
  (let ((pv (find-view d 'preview)))
    (when pv (setf (cp-attr pv) (tvision:make-attr (sw-value (find-view d 'fg)) (sw-value (find-view d 'bg))))
          (invalidate pv))))

(define-command color-apply (v e)
  "Set the chosen role's attribute from the fg/bg swatches and repaint live."
  (let* ((d (view-root v))
         (role (nth (cluster-value (find-view d 'role)) *color-roles*))
         (fg (sw-value (find-view d 'fg))) (bg (sw-value (find-view d 'bg))))
    (setf (getf *theme* role) (tvision:make-attr fg bg))
    (when *root* (invalidate *root*))))

(defun make-color-dialog ()
  "Visual colour customiser: pick a role, then a foreground and background from
the swatch strips (with a live sample); Apply previews it on *THEME*."
  (let ((d (ui (dialog (:title " Colours " :keymap *dialog-keys* :value-fn (lambda (d) (declare (ignore d)) t))
                 (stack
                   (1 (label :role :label :link 'role :text " ~R~ole:"))
                   (9 (cluster :name 'role :mode :radio
                        :items (mapcar (lambda (r) (string-downcase (symbol-name r))) *color-roles*) :value 0))
                   (1 (label :role :label :link 'fg :text " ~F~oreground  (←/→):"))
                   (2 (color-swatches :name 'fg :count 16 :value 7
                        :on-change (lambda (v) (%color-refresh-preview (view-root v)))))
                   (1 (label :role :label :link 'bg :text " ~B~ackground  (←/→):"))
                   (2 (color-swatches :name 'bg :count 8 :bg-p t :value 1
                        :on-change (lambda (v) (%color-refresh-preview (view-root v)))))
                   (1 (color-preview :name 'preview))
                   (:fill (static-text :text ""))
                   (1 (row (:fill (static-text :text ""))
                           (9  (button :label "Apply" :command 'color-apply))
                           (10 (button :label "Done"  :command 'accept)))))))))
    (%color-refresh-preview d)
    (exec-view d :width 54 :height 20)))
