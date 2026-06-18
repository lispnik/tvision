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

;;; --- TPictureValidator: a Paradox-style picture mask -----------------------
;;; Class chars: # digit, ? / L letter, & letter, @ / ! any char, A / a
;;; alphanumeric.  `*' makes the following class repeat zero or more times,
;;; `;' escapes the next character as a literal; any other char is a literal.

(defclass tpicture-validator (tvalidator)
  ((picture :initarg :picture :initform "" :accessor picture-mask)))

(defun make-picture-validator (mask) (make-instance 'tpicture-validator :picture mask))

(defun %picture-class-char-p (m) (find m "#?&@!AaLl"))

(defun %picture-class-match (m ch)
  (case m
    (#\# (and (digit-char-p ch) t))
    ((#\? #\L #\l #\& #\?) (and (alpha-char-p ch) t))
    ((#\A #\a) (and (alphanumericp ch) t))
    ((#\@ #\!) t)
    (t nil)))

(defun picture-match (mask string)
  "Match STRING against picture MASK.  Return :complete, :incomplete, or :error."
  (let ((mi 0) (si 0) (mlen (length mask)) (slen (length string)))
    (loop
      (when (>= mi mlen) (return (if (>= si slen) :complete :error)))
      (let ((m (char mask mi)))
        (cond
          ((char= m #\;)                      ; literal escape
           (incf mi)
           (cond ((>= mi mlen) (return :error))
                 ((>= si slen) (return :incomplete))
                 ((char= (char string si) (char mask mi)) (incf mi) (incf si))
                 (t (return :error))))
          ((char= m #\*)                      ; repeat the following class
           (incf mi)
           (when (< mi mlen)
             (let ((cls (char mask mi)))
               (incf mi)
               (loop while (and (< si slen) (%picture-class-match cls (char string si)))
                     do (incf si)))))
          ((%picture-class-char-p m)
           (cond ((>= si slen) (return :incomplete))
                 ((%picture-class-match m (char string si)) (incf mi) (incf si))
                 (t (return :error))))
          (t                                  ; literal
           (cond ((>= si slen) (return :incomplete))
                 ((char= (char string si) m) (incf mi) (incf si))
                 (t (return :error)))))))))

(defmethod is-valid-input ((v tpicture-validator) string)
  (member (picture-match (picture-mask v) string) '(:complete :incomplete)))
(defmethod is-valid ((v tpicture-validator) string)
  (eq (picture-match (picture-mask v) string) :complete))
(defmethod validator-error-message ((v tpicture-validator))
  (format nil "Input must match the pattern: ~a" (picture-mask v)))

;;; --- TStringLookupValidator: value must be one of a known set --------------

(defclass tstring-lookup-validator (tvalidator)
  ((strings :initarg :strings :initform '() :accessor lookup-strings)))

(defun make-string-lookup-validator (strings)
  (make-instance 'tstring-lookup-validator :strings strings))

(defmethod is-valid-input ((v tstring-lookup-validator) string)
  ;; accept while STRING is a (case-insensitive) prefix of some valid entry
  (or (zerop (length string))
      (some (lambda (e) (and (<= (length string) (length e))
                             (string-equal e string :end1 (length string))))
            (lookup-strings v))))
(defmethod is-valid ((v tstring-lookup-validator) string)
  (and (member string (lookup-strings v) :test #'string-equal) t))
(defmethod validator-error-message ((v tstring-lookup-validator))
  (format nil "Must be one of: ~{~a~^, ~}" (lookup-strings v)))
