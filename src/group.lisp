;;;; group.lisp --- TGroup, a view that owns and arranges other views.

(in-package #:tvision)

(defconstant +phase-pre+      0)
(defconstant +phase-focused+  1)
(defconstant +phase-post+     2)

(defclass tgroup (tview)
  ;; SUBVIEWS is ordered front-to-back: the head is the topmost view.
  ((subviews :initform '()  :accessor group-subviews)
   (current  :initform nil  :accessor group-current)
   (phase    :initform +phase-focused+ :accessor group-phase)
   (end-state :initform nil :accessor group-end-state)
   (buffer   :initform nil  :accessor group-buffer)))

(defmethod initialize-instance :after ((g tgroup) &key)
  (setf (view-options g) (logior (view-options g) +of-selectable+)))

(defun group-last (g) (car (last (group-subviews g))))

;;; --- insertion / removal ---------------------------------------------------

(defun insert (g view)
  "Insert VIEW at the top of G, give it the focus if selectable, and draw it."
  (setf (view-owner view) g)
  (setf (group-subviews g) (cons view (group-subviews g)))
  (set-state view +sf-visible+ t)
  (if (logtest (view-options view) +of-selectable+)
      (set-current g view :normal-select)
      (draw-view view))
  view)

(defun insert-before (g view target)
  "Insert VIEW immediately in front of TARGET (or at top if TARGET is nil)."
  (setf (view-owner view) g)
  (if (and target (member target (group-subviews g)))
      (let ((pos (position target (group-subviews g))))
        (setf (group-subviews g)
              (append (subseq (group-subviews g) 0 pos)
                      (list view)
                      (subseq (group-subviews g) pos))))
      (setf (group-subviews g) (cons view (group-subviews g))))
  (set-state view +sf-visible+ t)
  (draw-view view)
  view)

(defun remove-view (g view)
  "Remove VIEW from G, transfer focus if needed, and repaint."
  (when (member view (group-subviews g))
    (let ((was-current (eq view (group-current g))))
      (setf (group-subviews g) (remove view (group-subviews g)))
      (setf (view-owner view) nil)
      (when was-current
        (setf (group-current g)
              (find-if (lambda (v) (logtest (view-options v) +of-selectable+))
                       (group-subviews g)))
        (when (group-current g)
          (set-state (group-current g) +sf-selected+ t)
          (when (logtest (view-state g) +sf-focused+)
            (set-state (group-current g) +sf-focused+ t))))
      (redraw g))))

;;; --- focus management ------------------------------------------------------

(defun set-current (g view mode)
  "Make VIEW the current (focused) subview of G.  MODE is one of
:normal-select, :enter-select, :leave-select."
  (declare (ignore mode))
  (let ((old (group-current g)))
    (unless (eq old view)
      (when old
        (set-state old +sf-focused+ nil)
        (set-state old +sf-selected+ nil))
      (setf (group-current g) view)
      (when view
        ;; only top-select views (windows) are raised in the Z-order; control
        ;; focus must NOT reorder subviews or Tab cycling order would shift
        (when (logtest (view-options view) +of-top-select+)
          (setf (group-subviews g) (cons view (remove view (group-subviews g)))))
        (set-state view +sf-selected+ t)
        (when (logtest (view-state g) +sf-focused+)
          (set-state view +sf-focused+ t)))
      (redraw g))))

(defun selectable-subviews (g)
  (remove-if-not (lambda (v) (and (logtest (view-options v) +of-selectable+)
                                  (visible-p v)
                                  (not (logtest (view-state v) +sf-disabled+))))
                 (group-subviews g)))

(defun select-next (g &optional (forward t))
  "Move the focus to the next (or previous) selectable subview, cyclically.
Candidates are taken in insertion order (subviews are stored front-to-back, so
we reverse) -- which matches the natural top-to-bottom dialog layout order."
  (let* ((cands (reverse (selectable-subviews g))))
    (when (cdr cands)
      (let* ((cur (group-current g))
             (pos (or (position cur cands) 0))
             (next (if forward
                       (nth (mod (1+ pos) (length cands)) cands)
                       (nth (mod (1- pos) (length cands)) cands))))
        (set-current g next :normal-select)))))

(defun focus-next (g forward) (select-next g forward))

;;; --- iteration -------------------------------------------------------------

(defun for-each (g fn) (dolist (v (group-subviews g)) (funcall fn v)))
(defun foreach-view (g fn) (for-each g fn))
(defun first-that (g pred)
  (find-if pred (group-subviews g)))

;;; --- drawing ---------------------------------------------------------------

(defmethod draw ((g tgroup))
  ;; paint back-to-front so that topmost views land last
  (dolist (v (reverse (group-subviews g)))
    (draw-view v)))

(defun redraw (g) (draw-view g))

(defun calc-bounds (v delta-x delta-y)
  "Return V's new bounds after its container grew by (DELTA-X,DELTA-Y),
honouring V's grow-mode flags."
  (let ((r (get-bounds v)) (gm (view-grow-mode v)))
    (when (logtest gm +gf-grow-lox+) (incf (rect-ax r) delta-x))
    (when (logtest gm +gf-grow-hix+) (incf (rect-bx r) delta-x))
    (when (logtest gm +gf-grow-loy+) (incf (rect-ay r) delta-y))
    (when (logtest gm +gf-grow-hiy+) (incf (rect-by r) delta-y))
    r))

(defun scale-rel-bounds (v ow oh nw nh)
  "Scale V's bounds proportionally when its container grows from (OW,OH) to
(NW,NH) -- used for +gf-grow-rel+ subviews."
  (let ((r (get-bounds v)))
    (when (and (plusp ow) (plusp oh))
      (setf (rect-ax r) (round (* (rect-ax r) nw) ow)
            (rect-bx r) (round (* (rect-bx r) nw) ow)
            (rect-ay r) (round (* (rect-ay r) nh) oh)
            (rect-by r) (round (* (rect-by r) nh) oh)))
    r))

(defmethod change-bounds ((g tgroup) bounds)
  "Resize/move G, repositioning each subview according to its grow-mode."
  (let ((dx (- (rect-width bounds) (point-x (view-size g))))
        (dy (- (rect-height bounds) (point-y (view-size g))))
        (ow (point-x (view-size g))) (oh (point-y (view-size g))))
    (set-bounds g bounds)
    (when (or (/= dx 0) (/= dy 0))
      ;; recurse so nested groups (and e.g. the desktop background) reflow too
      (let ((nw (point-x (view-size g))) (nh (point-y (view-size g))))
        (dolist (v (group-subviews g))
          (change-bounds v (if (logtest (view-grow-mode v) +gf-grow-rel+)
                               (scale-rel-bounds v ow oh nw nh)
                               (calc-bounds v dx dy))))))
    (redraw g)))

(defmethod set-state ((g tgroup) state enable)
  (call-next-method)
  ;; cascade focus / active state to the current subview
  (when (logtest state (logior +sf-active+ +sf-focused+))
    (when (group-current g)
      (set-state (group-current g) state enable))))

;;; --- event dispatch --------------------------------------------------------

(defun mouse-down-p (e) (logtest (event-type e) (logior +ev-mouse-down+)))

(defmethod handle-event ((g tgroup) event)
  (cond
    ;; positional: route to the topmost visible view under the pointer
    ;; (any view, not just selectable -- so the menu bar / status line, which
    ;; are not selectable, still receive clicks, as in Turbo Vision)
    ((mouse-event-p event)
     (let ((target (find-if (lambda (v)
                              (and (visible-p v)
                                   (not (view-disabled-p v))
                                   (wants-event-p v event)
                                   (mouse-in-view-p v event)))
                            (group-subviews g))))
       (when target
         ;; focus the view on a click only if it is selectable
         (when (and (mouse-down-p event) (logtest (view-options target) +of-selectable+))
           (select target))
         (handle-event target event))))
    ;; keyboard: pre-process views (menu bar) first, then the focused chain,
    ;; then post-process views (status line)
    ((keyboard-event-p event)
     (dolist (v (group-subviews g))
       (when (and (not (eq v (group-current g)))
                  (logtest (view-options v) +of-pre-process+)
                  (wants-event-p v event)
                  (/= (event-type event) +ev-nothing+))
         (handle-event v event)))
     (when (and (group-current g) (/= (event-type event) +ev-nothing+)
                (wants-event-p (group-current g) event))
       (handle-event (group-current g) event))
     ;; Tab / Shift-Tab cycle focus among controls.  Consume them at the
     ;; innermost group that directly holds a leaf control (so the desktop
     ;; never cycles windows on Tab); recursion reaches that group first.
     (when (and (= (event-type event) +ev-key-down+)
                (group-current g)
                (not (typep (group-current g) 'tgroup)))
       (let ((k (event-key-code event)))
         (cond
           ((= k +kb-tab+)       (select-next g t)   (clear-event event))
           ((= k +kb-shift-tab+) (select-next g nil) (clear-event event)))))
     (when (/= (event-type event) +ev-nothing+)
       (dolist (v (group-subviews g))
         (when (and (not (eq v (group-current g)))
                    (logtest (view-options v) +of-post-process+)
                    (wants-event-p v event)
                    (/= (event-type event) +ev-nothing+))
           (handle-event v event)))))
    ;; commands / broadcasts: focused view first, then everyone
    ((message-event-p event)
     (when (group-current g)
       (handle-event (group-current g) event))
     (dolist (v (group-subviews g))
       (when (and (not (eq v (group-current g)))
                  (/= (event-type event) +ev-nothing+))
         (handle-event v event))))))

;;; --- modal execution -------------------------------------------------------

(defgeneric exec-view (g modal-view)
  (:documentation "Run MODAL-VIEW modally inside G; return its end command.")
  (:method ((g tgroup) modal-view)
    (let ((save-current (group-current g)))
      (set-state modal-view +sf-modal+ t)
      (insert g modal-view)
      (let ((result (modal-loop modal-view)))   ; defined in program.lisp
        (remove-view g modal-view)
        (when save-current (set-current g save-current :normal-select))
        result))))

(defmethod end-modal ((g tgroup) command)
  (if (logtest (view-state g) +sf-modal+)
      (setf (group-end-state g) command)
      (call-next-method)))

(defun exec (g) (modal-loop g))
(defun end-exec (g command) (end-modal g command))

;;; --- palette ---------------------------------------------------------------

(defmethod size-limits ((g tgroup))
  (values (make-tpoint 0 0) (make-tpoint 999 999)))

;;; --- aggregate data exchange -----------------------------------------------

(defun data-views (g)
  "Subviews that carry data, in insertion (layout) order."
  (reverse (remove-if (lambda (v) (zerop (data-size v))) (group-subviews g))))

(defmethod data-size ((g tgroup))
  (reduce #'+ (group-subviews g) :key #'data-size :initial-value 0))

(defmethod get-data ((g tgroup))
  "Collect each data-bearing subview's value into a list (layout order)."
  (mapcar #'get-data (data-views g)))

(defmethod set-data ((g tgroup) data)
  "Distribute a list of values to the data-bearing subviews (layout order)."
  (loop for v in (data-views g) for d in data do (set-data v d)))

(defmethod valid-p ((g tgroup) command)
  "A group is valid for COMMAND only if every subview is (TGroup::valid)."
  (every (lambda (v) (valid-p v command)) (group-subviews g)))
