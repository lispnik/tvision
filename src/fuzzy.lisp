;;;; fuzzy.lisp --- FUZZY-FILTER-MIXIN: type-to-filter for lists and tables.
;;;;
;;;; A behaviour-only CLOS mixin that adds fzf-style fuzzy filtering to any
;;;; view, layered on top of the view's own navigation via method combination.
;;;; It talks to the underlying view through a tiny protocol (FF-INSTALL /
;;;; FF-FOCUSED), so the *same* mixin composes with both TLIST-BOX and
;;;; TTABLE-VIEW:
;;;;
;;;;   (defclass tfilter-list-box (fuzzy-filter-mixin tlist-box)   ())
;;;;   (defclass tfilter-table    (fuzzy-filter-mixin ttable-view) ())
;;;;
;;;; Put the mixin FIRST in the superclass list so its HANDLE-EVENT/DRAW run
;;;; before the base view's and then CALL-NEXT-METHOD down to it.

(in-package #:tvision)

;;; --- the fuzzy scorer ------------------------------------------------------

(defun flex-score (query candidate)
  "Fuzzy match QUERY against CANDIDATE (case-insensitive).  Returns (values
SCORE MATCHED-INDICES) when QUERY's characters occur in CANDIDATE in order, or
NIL otherwise.  Higher score is better: a match scores more at the string start
or just after a separator (word boundary) and in a contiguous run."
  (let ((qi 0) (qn (length query)) (score 0) (run 0) (prev-sep t) (idx '()))
    (when (zerop qn) (return-from flex-score (values 0 '())))
    (dotimes (i (length candidate))
      (let ((c (char candidate i)))
        (cond
          ((and (< qi qn) (char-equal c (char query qi)))
           (incf score (+ 1 run (if prev-sep 8 0) (if (zerop i) 4 0)))
           (incf run 2)
           (push i idx)
           (incf qi))
          (t (setf run 0)))
        (setf prev-sep (not (alphanumericp c)))))
    (when (= qi qn) (values score (nreverse idx)))))

;;; --- the mixin -------------------------------------------------------------

(defclass fuzzy-filter-mixin ()
  ((all       :initarg :all       :initform #()        :accessor ff-all)
   (key       :initarg :key       :initform #'identity :accessor ff-key)      ; row -> match string
   (display   :initarg :display   :initform #'identity :accessor ff-display)  ; row -> shown string
   (visible   :initform #()       :accessor ff-visible)                       ; current rows (shown order)
   (query     :initform ""        :accessor ff-query)
   (self-edit :initarg :self-edit :initform t          :accessor ff-self-edit)
   ;; When the mixin is driven externally (SELF-EDIT nil) by a host window's
   ;; `/`-to-filter mode, FILTERING records whether that mode is armed.
   (filtering :initform nil       :accessor ff-filtering)
   (on-change :initarg :on-change :initform nil        :accessor ff-on-change))
  (:documentation "Adds fuzzy type-to-filter behaviour to a list/table view."))

(defmethod initialize-instance :after ((v fuzzy-filter-mixin) &key)
  ;; keep the candidate set a vector so REFILTER can iterate it with ACROSS
  ;; regardless of whether :ALL was given a list or a vector
  (setf (ff-all v) (coerce (ff-all v) 'vector)))

;;; protocol the mixin drives the underlying view through
(defgeneric ff-install (view rows)
  (:documentation "Show ROWS (already filtered/ranked) in VIEW's display."))
(defgeneric ff-focused (view)
  (:documentation "The row object currently focused in VIEW, or NIL."))

(defmethod ff-install ((v tlist-box) rows)
  (list-set-items v (map 'vector (ff-display v) rows))
  (when (plusp (length rows)) (list-focus-item v 0)))
(defmethod ff-focused ((v tlist-box))
  (let ((vis (ff-visible v)) (i (list-focused v)))
    (when (and (plusp (length vis)) (< i (length vis))) (aref vis i))))

(defmethod ff-install ((v ttable-view) rows)
  ;; the table keeps its own column sort; we just hand it the surviving rows
  (table-set-rows v rows) (table-focus v 0))
(defmethod ff-focused ((v ttable-view))
  (table-selected-row v))   ; the table tracks its row objects directly

(defun ff-refilter (v)
  "Recompute the visible rows from the query and reinstall them in the view."
  (let* ((q (ff-query v)) (key (ff-key v))
         (rows (if (zerop (length q))
                   (coerce (ff-all v) 'vector)
                   (let ((scored '()))
                     (loop for r across (ff-all v)
                           for sc = (flex-score q (funcall key r))
                           when sc do (push (cons sc r) scored))
                     (map 'vector #'cdr (stable-sort scored #'> :key #'car))))))
    (setf (ff-visible v) rows)
    (ff-install v rows)))

(defun ff-set-query (v q)
  "Set the filter query to Q, refilter, and fire the on-change hook."
  (setf (ff-query v) q)
  (ff-refilter v)
  (when (ff-on-change v) (funcall (ff-on-change v) v)))

(defun ff-set-all (v rows &key (refilter t))
  "Replace the full candidate set (a sequence of row objects)."
  (setf (ff-all v) (coerce rows 'vector))
  (when refilter (ff-refilter v)))

(defun ff-end-filter (v)
  "Leave externally-driven filter mode and restore the full (unfiltered) view."
  (setf (ff-filtering v) nil)
  (ff-set-query v ""))

(defmethod handle-event ((v fuzzy-filter-mixin) event)
  ;; Capture typing into the query (only when SELF-EDIT); everything else —
  ;; arrows, Enter, mouse — falls through to the base list/table view.  When
  ;; SELF-EDIT is nil the mixin is inert and the host (an input field, or a
  ;; window's `/`-filter mode) drives FF-SET-QUERY instead.
  (let ((q (ff-query v)))
    (cond
      ((not (and (ff-self-edit v)
                 (= (event-type event) +ev-key-down+)
                 (logtest (view-state v) +sf-focused+)))
       (call-next-method))
      ((= (event-key-code event) +kb-back+)
       (when (plusp (length q)) (ff-set-query v (subseq q 0 (1- (length q)))))
       (clear-event event))
      ((= (event-key-code event) +kb-esc+)
       (if (plusp (length q))
           (progn (ff-set-query v "") (clear-event event))   ; clear the filter
           (call-next-method)))                              ; empty -> let it cancel
      ((let ((cc (event-char-code event)))
         (and (plusp cc) (zerop (event-modifiers event))
              (let ((c (code-char cc)))
                (and c (graphic-char-p c)
                     (not (and (char= c #\Space) (zerop (length q))))))))
       (ff-set-query v (concatenate 'string q (string (code-char (event-char-code event)))))
       (clear-event event))
      (t (call-next-method)))))

;;; --- composed widgets ------------------------------------------------------

(defclass tfilter-list-box (fuzzy-filter-mixin tlist-box) ()
  (:documentation "A list box you filter by typing a fuzzy query."))
(defclass tfilter-table (fuzzy-filter-mixin ttable-view) ()
  (:documentation "A table you filter by typing a fuzzy query (column sort kept)."))
