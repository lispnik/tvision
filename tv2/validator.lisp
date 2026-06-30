;;;; validator.lisp --- field validators for INPUT-LINE (tvision's TValidator).
;;;;
;;;; Each constructor returns a FIELD-VALIDATOR (defined with input-line): FILTER
;;;; rejects keystrokes as typed; CHECK validates the whole field on accept,
;;;; returning (values OK-P MESSAGE).  The modal ACCEPT command runs CHECK across
;;;; a dialog's fields and shows the message via the dialog's 'msg line.

(in-package #:tv2)

(defun filter-validator (allowed)
  "Allow only characters in the string ALLOWED."
  (%fv :filter (lambda (ch) (find ch allowed))))

(defun digits-validator ()
  (%fv :filter (lambda (ch) (digit-char-p ch))))

(defun range-validator (lo hi)
  "Digits only; the value must parse to an integer in [LO, HI]."
  (%fv :filter (lambda (ch) (or (digit-char-p ch) (char= ch #\-)))
       :check  (lambda (s)
                 (let ((n (ignore-errors (parse-integer s :junk-allowed t))))
                   (if (and n (<= lo n hi)) (values t nil)
                       (values nil (format nil " Enter a number ~d..~d " lo hi)))))))

(defun picture-validator (template)
  "Match TEMPLATE where # = a digit, A = a letter, and any other char is a literal
that must appear verbatim (e.g. \"##/##/####\")."
  (%fv :check (lambda (s)
                (if (and (= (length s) (length template))
                         (every (lambda (c p) (case p
                                                (#\# (digit-char-p c))
                                                (#\A (alpha-char-p c))
                                                (t (char= c p))))
                                s template))
                    (values t nil)
                    (values nil (format nil " Format: ~a  (#=digit, A=letter) " template))))))
