;;;; fuzzy.lisp --- fzf-style fuzzy matching for the type-to-filter inputs.
;;;;
;;;; FLEX-SCORE is ported verbatim from the classic framework's src/fuzzy.lisp;
;;;; FUZZY-FILTER ranks a list of items by it (best first), so the browser and
;;;; project filters match out-of-order subsequences instead of plain substrings.

(in-package #:tv2)

(defun flex-score (query candidate)
  "Fuzzy-match QUERY against CANDIDATE (case-insensitive).  Returns (values SCORE
MATCHED-INDICES) when QUERY's characters occur in CANDIDATE in order, else NIL.
Higher is better: a match scores more at the start / after a word boundary and
in a contiguous run."
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

(defun fuzzy-filter (query items &key (key #'identity))
  "ITEMS whose KEY fuzzy-matches QUERY, ranked best score first.  Empty QUERY
returns ITEMS unchanged."
  (if (zerop (length query))
      items
      (let ((scored '()))
        (dolist (it items)
          (let ((s (flex-score query (funcall key it))))
            (when s (push (cons s it) scored))))
        (mapcar #'cdr (stable-sort scored #'> :key #'car)))))
