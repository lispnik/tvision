;;;; runtime.lisp --- two non-UI services:
;;;;   (1) MOP-based persistence: serialize a model by introspecting its slots,
;;;;       skipping ones marked :transient -- no hand-written streamers;
;;;;   (2) a worker->UI bridge: background threads post closures with RUN-ON-UI,
;;;;       and the event loop runs them on the UI thread via DRAIN-UI-CALLBACKS.

(in-package #:tv2)

;;; ===========================================================================
;;; Persistence via the metaobject protocol
;;; ===========================================================================

(defclass persistent-class (standard-class) ())
(defmethod sb-mop:validate-superclass ((c persistent-class) (s standard-class)) t)

(defclass persistent-slot-mixin ()
  ((transient :initarg :transient :initform nil :reader slot-transient-p)))
(defclass persistent-direct-slot (persistent-slot-mixin sb-mop:standard-direct-slot-definition) ())
(defclass persistent-effective-slot (persistent-slot-mixin sb-mop:standard-effective-slot-definition) ())

(defmethod sb-mop:direct-slot-definition-class ((c persistent-class) &rest initargs)
  (declare (ignore initargs)) (find-class 'persistent-direct-slot))
(defmethod sb-mop:effective-slot-definition-class ((c persistent-class) &rest initargs)
  (declare (ignore initargs)) (find-class 'persistent-effective-slot))
(defmethod sb-mop:compute-effective-slot-definition ((c persistent-class) name dslots)
  (declare (ignore name))
  (let ((eslot (call-next-method)))
    (setf (slot-value eslot 'transient) (some #'slot-transient-p dslots))
    eslot))

(defgeneric serialize (object)
  (:documentation "A readable representation of OBJECT.  Persistent-class objects
become (:object CLASS slot val ...) over their non-transient, bound slots.")
  (:method ((x t)) x)                                   ; numbers, strings, symbols, t, nil
  (:method ((x cons)) (cons (serialize (car x)) (serialize (cdr x))))
  (:method ((obj standard-object))
    (let ((class (class-of obj)))
      (if (typep class 'persistent-class)
          (list* :object (class-name class)
                 (loop for slot in (sb-mop:class-slots class)
                       for name = (sb-mop:slot-definition-name slot)
                       unless (or (slot-transient-p slot) (not (slot-boundp obj name)))
                         append (list name (serialize (slot-value obj name)))))
          obj))))

(defun deserialize (form)
  "Reconstruct what SERIALIZE produced."
  (cond
    ((and (consp form) (eq (car form) :object))
     (let ((obj (make-instance (second form))))
       (loop for (name val) on (cddr form) by #'cddr
             do (setf (slot-value obj name) (deserialize val)))
       obj))
    ((consp form) (cons (deserialize (car form)) (deserialize (cdr form))))
    (t form)))

(defun save-object (object path)
  (ignore-errors
   (with-open-file (s path :direction :output :if-exists :supersede :if-does-not-exist :create)
     (let ((*print-readably* nil)) (prin1 (serialize object) s)))
   t))

(defun load-object (path)
  (when (probe-file path)
    (ignore-errors (with-open-file (s path) (deserialize (read s nil nil))))))

;;; A small persisted model for the demo: the filter text and outline line are
;;; saved; TOUCHED is :transient (recomputed each run, never written).
(defclass session ()
  ((filter  :initarg :filter :initform "" :accessor session-filter)
   (line    :initarg :line   :initform 1  :accessor session-line)
   (touched :initform 0 :accessor session-touched :transient t))
  (:metaclass persistent-class))

(defun session-file () (merge-pathnames ".tv2-session" (user-homedir-pathname)))

;;; ===========================================================================
;;; Worker -> UI bridge
;;; ===========================================================================

(defvar *ui-thread* nil)
(defvar *ui-lock* (sb-thread:make-mutex :name "tv2-ui-queue"))
(defvar *ui-queue* '())   ; FIFO list of thunks awaiting the UI thread

(defun ui-thread-p () (or (null *ui-thread*) (eq sb-thread:*current-thread* *ui-thread*)))

(defun run-on-ui (thunk)
  "Run THUNK on the UI thread: immediately if already there, else enqueue it for
the event loop to drain.  The single rule that keeps views single-threaded."
  (if (ui-thread-p)
      (funcall thunk)
      (sb-thread:with-mutex (*ui-lock*) (setf *ui-queue* (append *ui-queue* (list thunk))))))

(defun drain-ui-callbacks ()
  "Run (on the UI thread) every thunk posted since the last drain."
  (let ((thunks (sb-thread:with-mutex (*ui-lock*) (prog1 *ui-queue* (setf *ui-queue* '())))))
    (dolist (th thunks) (ignore-errors (funcall th)))))
