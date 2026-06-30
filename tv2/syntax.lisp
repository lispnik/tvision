;;;; syntax.lisp --- pluggable syntax highlighting for the text editor.
;;;;
;;;; A colorizer is a function (LINE IN-STRING) -> (values ATTRS END-IN-STRING):
;;;; ATTRS is a per-character vector of packed attributes, IN-STRING carries the
;;;; "inside a multi-line string" state from one line to the next.  TEXT-EDIT
;;;; calls it per visible line (threading the carry).  LISP-COLORIZE is the one
;;;; bundled colorizer, ported from tvlisp's %lisp-colorize.

(in-package #:tv2)

(defun %lisp-symchar-p (c) (or (alphanumericp c) (find c "+-*/@$%^&_=<>.~!?:")))

(defun %syn (fg) (tvision:make-attr fg (tvision::attr-bg (role :normal))))

(defun lisp-colorize (line in-string)
  "Colour LINE as Lisp: comments, strings, char literals, and :keywords.
Return (values ATTRS END-IN-STRING)."
  (let* ((n (length line)) (base (role :normal))
         (attrs (make-array n :initial-element base))
         (comment (%syn 8)) (string (%syn 10)) (kw (%syn 14))     ; grey / green / yellow
         (i 0) (instr in-string))
    (flet ((paint (a b attr) (loop for k from (max 0 a) below (min b n) do (setf (aref attrs k) attr))))
      (when instr                                                 ; continued string from the line above
        (let ((end 0) (closed nil))
          (loop while (< end n) do
            (cond ((char= (char line end) #\\) (incf end 2))
                  ((char= (char line end) #\") (incf end) (setf closed t) (return))
                  (t (incf end))))
          (paint 0 (min end n) string)
          (setf instr (not closed) i (if closed end n))))
      (loop while (< i n) do
        (let ((c (char line i)))
          (cond
            ((char= c #\;) (paint i n comment) (setf i n))
            ((char= c #\")
             (let ((end (1+ i)) (closed nil))
               (loop while (< end n) do
                 (cond ((char= (char line end) #\\) (incf end 2))
                       ((char= (char line end) #\") (incf end) (setf closed t) (return))
                       (t (incf end))))
               (paint i (min end n) string)
               (setf instr (not closed) i (if closed end n))))
            ((and (char= c #\#) (< (1+ i) n) (char= (char line (1+ i)) #\\))
             (let ((end (min n (+ i 3))))
               (when (and (> end (+ i 2)) (alpha-char-p (char line (1- end))))
                 (loop while (and (< end n) (alphanumericp (char line end))) do (incf end)))
               (paint i end string) (setf i end)))
            ((and (char= c #\:) (or (zerop i) (not (%lisp-symchar-p (char line (1- i))))))
             (let ((end (1+ i)))
               (loop while (and (< end n) (%lisp-symchar-p (char line end))) do (incf end))
               (paint i end kw) (setf i end)))
            (t (incf i))))))
    (values attrs instr)))

(defun lisp-string-carry (line in)
  "Whether LINE ends inside a \"...\" string (cheap; allocates nothing).  Used to
recover the colorizer's carry state for lines above the viewport."
  (let ((n (length line)) (i 0) (instr in))
    (loop while (< i n) do
      (let ((c (char line i)))
        (cond (instr (cond ((char= c #\\) (incf i 2))
                           ((char= c #\") (setf instr nil) (incf i))
                           (t (incf i))))
              ((char= c #\;) (return))                            ; rest of line is a comment
              ((char= c #\") (setf instr t) (incf i))
              ((and (char= c #\#) (< (1+ i) n) (char= (char line (1+ i)) #\\)) (incf i 3))
              (t (incf i)))))
    instr))
