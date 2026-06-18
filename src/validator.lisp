;;;; validator.lisp --- TValidator hierarchy for input-line validation.

(in-package #:tvision)

(defclass tvalidator () ())

(defgeneric is-valid-input (v string)
  (:documentation "True if STRING is acceptable so far (char-by-char editing).")
  (:method ((v tvalidator) string) (declare (ignore string)) t))

(defgeneric is-valid (v string)
  (:documentation "True if the completed STRING is acceptable (on Ok/exit).")
  (:method ((v tvalidator) string) (declare (ignore string)) t))

(defgeneric validator-error-message (v)
  (:method ((v tvalidator)) "Invalid input."))

;;; --- TFilterValidator: restrict to a set of characters --------------------

(defclass tfilter-validator (tvalidator)
  ((chars :initarg :chars :initform "" :accessor filter-chars)))

(defun make-filter-validator (chars) (make-instance 'tfilter-validator :chars chars))

(defmethod is-valid-input ((v tfilter-validator) string)
  (every (lambda (c) (find c (filter-chars v))) string))
(defmethod is-valid ((v tfilter-validator) string) (is-valid-input v string))
(defmethod validator-error-message ((v tfilter-validator))
  (format nil "Only these characters are allowed: ~a" (filter-chars v)))

;;; --- TRangeValidator: an integer within [min,max] -------------------------

(defclass trange-validator (tvalidator)
  ((minv :initarg :min :initform most-negative-fixnum :accessor range-min)
   (maxv :initarg :max :initform most-positive-fixnum :accessor range-max)))

(defun make-range-validator (min max) (make-instance 'trange-validator :min min :max max))

(defmethod is-valid-input ((v trange-validator) string)
  (every (lambda (c) (or (digit-char-p c) (char= c #\-))) string))
(defmethod is-valid ((v trange-validator) string)
  (let ((n (ignore-errors (parse-integer string :junk-allowed nil))))
    (and n (<= (range-min v) n (range-max v)))))
(defmethod validator-error-message ((v trange-validator))
  (format nil "Enter an integer from ~d to ~d." (range-min v) (range-max v)))

;;; --- TPictureValidator: a simple mask -------------------------------------
;;; Mask characters: # = digit, ? = letter, & = letter upcased, A = alnum,
;;; * = any.  Other characters are literals that must match exactly.

(defclass tpicture-validator (tvalidator)
  ((picture :initarg :picture :initform "" :accessor picture-mask)))

(defun make-picture-validator (mask) (make-instance 'tpicture-validator :picture mask))

(defun %picture-char-ok (m ch)
  (case m
    (#\# (and (digit-char-p ch) t))
    (#\? (and (alpha-char-p ch) t))
    (#\& (and (alpha-char-p ch) t))
    (#\A (and (alphanumericp ch) t))
    (#\* t)
    (t (char= m ch))))

(defmethod is-valid-input ((v tpicture-validator) string)
  (let ((mask (picture-mask v)))
    (and (<= (length string) (length mask))
         (loop for i below (length string)
               always (%picture-char-ok (char mask i) (char string i))))))
(defmethod is-valid ((v tpicture-validator) string)
  (let ((mask (picture-mask v)))
    (and (= (length string) (length mask))
         (loop for i below (length mask)
               always (%picture-char-ok (char mask i) (char string i))))))
(defmethod validator-error-message ((v tpicture-validator))
  (format nil "Input must match the pattern: ~a" (picture-mask v)))
