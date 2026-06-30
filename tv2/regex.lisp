;;;; regex.lisp --- a tiny backtracking regex engine for the editor's find/replace.
;;;;
;;;; Ported verbatim from the classic framework's textview.lisp: enough for the
;;;; common editor patterns (^, $, ., *, +, ?, \d \w \s, [classes]).  Matching is
;;;; per line.  Used by the editor's regex find and replace-all.

(in-package #:tv2)

(defun %rx-parse (pat)
  "Parse PAT into a list of (ATOM . QUANT).  ATOM is (:char c) | (:any) |
 (:class NEG . ITEMS) | (:start) | (:end); QUANT is NIL | :star | :plus | :opt."
  (let ((items '()) (i 0) (n (length pat)))
    (flet ((push-atom (atom)
             (let ((q (when (< i n) (case (char pat i)
                                      (#\* :star) (#\+ :plus) (#\? :opt)))))
               (when q (incf i))
               (push (cons atom q) items))))
      (loop while (< i n) do
        (let ((c (char pat i)))
          (incf i)
          (cond
            ((char= c #\^) (push (cons '(:start) nil) items))
            ((char= c #\$) (push (cons '(:end) nil) items))
            ((char= c #\.) (push-atom '(:any)))
            ((char= c #\\)
             (when (< i n)
               (let ((d (char pat i)))
                 (incf i)
                 (push-atom (case d
                              (#\d '(:class nil (#\0 . #\9)))
                              (#\w '(:class nil (#\a . #\z) (#\A . #\Z) (#\0 . #\9) #\_))
                              (#\s (list :class nil #\Space #\Tab #\Newline #\Return))
                              (t (list :char d)))))))
            ((char= c #\[)
             (let ((neg nil) (set '()))
               (when (and (< i n) (char= (char pat i) #\^)) (setf neg t) (incf i))
               (loop while (and (< i n) (char/= (char pat i) #\])) do
                 (if (and (< (+ i 2) n) (char= (char pat (1+ i)) #\-) (char/= (char pat (+ i 2)) #\]))
                     (progn (push (cons (char pat i) (char pat (+ i 2))) set) (incf i 3))
                     (progn (push (char pat i) set) (incf i))))
               (when (< i n) (incf i))                  ; skip ]
               (push-atom (list* :class neg (nreverse set)))))
            (t (push-atom (list :char c))))))
      (nreverse items))))

(defun %rx-class-match (spec ch)
  (let ((hit (some (lambda (it) (if (consp it) (char<= (car it) ch (cdr it)) (char= it ch)))
                   (cdr spec))))
    (if (car spec) (not hit) hit)))

(defun %rx-atom-match (atom line i len)
  (and (< i len)
       (case (car atom)
         (:char (char= (cadr atom) (char line i)))
         (:any t)
         (:class (%rx-class-match (cdr atom) (char line i)))
         (t nil))))

(defun %rx-match-items (items line i len)
  (if (null items)
      i
      (destructuring-bind (atom . quant) (car items)
        (case (car atom)
          (:start (and (= i 0) (%rx-match-items (cdr items) line i len)))
          (:end   (and (= i len) (%rx-match-items (cdr items) line i len)))
          (t (ecase quant
               ((nil) (and (%rx-atom-match atom line i len)
                           (%rx-match-items (cdr items) line (1+ i) len)))
               (:opt  (or (and (%rx-atom-match atom line i len)
                               (%rx-match-items (cdr items) line (1+ i) len))
                          (%rx-match-items (cdr items) line i len)))
               (:star (%rx-match-greedy atom (cdr items) line i len 0))
               (:plus (%rx-match-greedy atom (cdr items) line i len 1))))))))

(defun %rx-match-greedy (atom rest line i len minrep)
  (let ((j i))
    (loop while (%rx-atom-match atom line j len) do (incf j))
    (loop for k from j downto (+ i minrep)
          for r = (%rx-match-items rest line k len)
          when r do (return r))))

(defun %rx-search-line (items line start)
  "First match of ITEMS in LINE at or after START; (values mstart mend) or NIL."
  (loop with len = (length line)
        for s from start to len
        for e = (%rx-match-items items line s len)
        when e do (return (values s e))))
