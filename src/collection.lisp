;;;; collection.lisp --- TCollection / TSortedCollection over adjustable arrays.

(in-package #:tvision)

(defclass tcollection ()
  ((items :initform (make-array 0 :adjustable t :fill-pointer 0)
          :accessor collection-items)))

(defun make-collection (&optional initial-contents)
  (let ((c (make-instance 'tcollection)))
    (when initial-contents
      (map nil (lambda (x) (vector-push-extend x (collection-items c))) initial-contents))
    c))

(defun collection-count (c) (fill-pointer (collection-items c)))
(defun at (c i) (aref (collection-items c) i))
(defun (setf at) (value c i) (setf (aref (collection-items c) i) value))

(defgeneric insert-item (c x)
  (:documentation "Add X to C; return its index.")
  (:method ((c tcollection) x)
    (vector-push-extend x (collection-items c))
    (1- (collection-count c))))

(defun at-insert (c index x)
  "Insert X at INDEX, shifting later items up."
  (let ((items (collection-items c)))
    (vector-push-extend x items)               ; grow by one
    (loop for k from (1- (fill-pointer items)) above index
          do (setf (aref items k) (aref items (1- k))))
    (setf (aref items index) x))
  c)

(defun at-remove (c index)
  "Remove the item at INDEX, shifting later items down."
  (let ((items (collection-items c)))
    (loop for k from index below (1- (fill-pointer items))
          do (setf (aref items k) (aref items (1+ k))))
    (decf (fill-pointer items)))
  c)

(defun delete-item (c x &key (test #'eql))
  (let ((i (index-of c x :test test)))
    (when i (at-remove c i)))
  c)

(defun index-of (c x &key (test #'eql))
  (position x (collection-items c) :test test :end (collection-count c)))

(defun collection-for-each (c fn)
  (dotimes (i (collection-count c)) (funcall fn (at c i))))

(defun collection-list (c)
  (coerce (subseq (collection-items c) 0 (collection-count c)) 'list))

(defun collection-clear (c) (setf (fill-pointer (collection-items c)) 0) c)

;;; --- sorted collection -----------------------------------------------------

(defclass tsorted-collection (tcollection)
  ((compare :initarg :compare :initform #'< :accessor collection-compare)
   (key     :initarg :key     :initform #'identity :accessor collection-key)
   (duplicates :initarg :duplicates :initform t :accessor collection-duplicates)))

(defun make-sorted-collection (&key (compare #'<) (key #'identity) (duplicates t) initial)
  (let ((c (make-instance 'tsorted-collection :compare compare :key key
                          :duplicates duplicates)))
    (when initial (map nil (lambda (x) (insert-item c x)) initial))
    c))

(defun string-collection (&optional initial)
  "A sorted collection ordered case-insensitively by STRING<."
  (make-sorted-collection
   :compare (lambda (a b) (string-lessp a b))
   :initial initial))

(defmethod insert-item ((c tsorted-collection) x)
  "Insert X keeping C ordered; return its index."
  (let* ((cmp (collection-compare c))
         (key (collection-key c))
         (kx (funcall key x))
         (n (collection-count c))
         (pos (loop for i from 0 below n
                    unless (funcall cmp (funcall key (at c i)) kx)
                    do (return i)
                    finally (return n))))
    (when (or (collection-duplicates c)
              (not (and (< pos n)
                        (not (funcall cmp kx (funcall key (at c pos)))))))
      (at-insert c pos x))
    pos))
