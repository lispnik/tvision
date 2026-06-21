;;;; textview.lisp --- TTextView: an editable, scrollable multi-line text area.
;;;;
;;;; This is the foundation for an editor and, shortly, a Lisp REPL.  Text is
;;;; held as an adjustable vector of line strings.  Editing, navigation and
;;;; scrolling are all driven through small primitives so that subclasses (e.g.
;;;; a REPL) can override behaviour -- in particular TEXT-RETURN, which decides
;;;; what the Enter key does.

(in-package #:tvision)

;;; A text view is a scroller whose virtual area is the text: DELTA's x/y are the
;;; left column and top line.  We expose them under the old names for clarity.

(defvar *clipboard* "" "Shared text clipboard for cut/copy/paste.")

(defclass ttext-view (tscroller)
  ((lines     :accessor text-lines)
   (cur-line  :initform 0 :accessor text-cur-line)
   (cur-col   :initform 0 :accessor text-cur-col)
   (read-only :initarg :read-only :initform nil :accessor text-read-only)
   ;; selection anchor (cons LINE . COL) or NIL when there is no selection
   (anchor    :initform nil :accessor text-anchor)
   ;; read-only boundary (cons LINE . COL): edits before it are blocked (REPL)
   (protect   :initform nil :accessor text-protect)
   (undo      :initform '() :accessor text-undo)
   (redo      :initform '() :accessor text-redo)
   (modified  :initform nil :accessor text-modified)
   (overwrite :initform nil :accessor text-overwrite)
   (wrap      :initarg :wrap :initform nil :accessor text-wrap) ; word-wrap mode
   (highlight :initarg :highlight :initform nil :accessor text-highlight) ; Lisp syntax colouring
   (goal-col  :initform nil :accessor text-goal-col)))  ; desired visual col for Up/Down

(declaim (inline text-top-line text-left-col))
(defun text-top-line (tv) (point-y (scroller-delta tv)))
(defun text-left-col (tv) (point-x (scroller-delta tv)))
(defun (setf text-top-line) (v tv) (setf (point-y (scroller-delta tv)) v))
(defun (setf text-left-col) (v tv) (setf (point-x (scroller-delta tv)) v))

(defmethod initialize-instance :after ((tv ttext-view) &key (text ""))
  (setf (view-state tv) (logior (view-state tv) +sf-cursor-vis+))
  (set-text tv text))

(defmethod get-palette ((tv ttext-view)) (make-palette 13 14))  ; input colours

;;; --- line storage ----------------------------------------------------------

(defun %make-line-vector (&optional (initial '("")))
  (let ((v (make-array (max 1 (length initial))
                       :adjustable t :fill-pointer 0)))
    (dolist (s initial) (vector-push-extend s v))
    (when (zerop (fill-pointer v)) (vector-push-extend "" v))
    v))

(defun text-update-limit (tv)
  "Set the scroller's virtual size from the current line widths and count.  In
word-wrap mode the virtual width is just the view width (no horizontal scroll)."
  (let ((maxw 1))
    (if (text-wrap tv)
        (setf maxw (max 1 (point-x (view-size tv))))
        (dotimes (i (fill-pointer (text-lines tv)))
          (setf maxw (max maxw (1+ (length (aref (text-lines tv) i)))))))
    (set-scroller-limit tv maxw (fill-pointer (text-lines tv)))))

;;; word-wrap geometry: how many visual rows a logical LINE occupies at width W,
;;; honouring display width and grapheme boundaries (WRAP-SEGMENTS).
(defun %line-rows (line w) (length (wrap-segments line w)))
(defun %vrows-between (tv top line w)
  "Number of visual rows occupied by logical lines [TOP, LINE)."
  (loop for i from top below line sum (%line-rows (nth-line tv i) w)))
(defun %seg-index (segs col)
  "Index of the wrap segment (from WRAP-SEGMENTS) containing code-point COL: the
last segment whose start is <= COL."
  (let ((best 0))
    (loop for s in segs for k from 0 when (<= s col) do (setf best k))
    best))

(defun set-text-wrap (tv on)
  "Turn word-wrap on/off and reflow."
  (setf (text-wrap tv) on)
  (when on (setf (text-left-col tv) 0))
  (text-update-limit tv)
  (ensure-visible tv)
  (draw-view tv))

(defun set-text (tv string)
  "Replace the contents of TV with STRING (split on newlines)."
  (set-lines tv (%split-lines string)))

(defun set-lines (tv list-of-strings)
  "Replace the contents of TV with the given lines."
  (setf (text-lines tv) (%make-line-vector (or list-of-strings '("")))
        (text-cur-line tv) 0 (text-cur-col tv) 0
        (text-top-line tv) 0 (text-left-col tv) 0
        (text-anchor tv) nil (text-protect tv) nil
        (text-undo tv) '() (text-redo tv) '()
        (text-modified tv) nil)
  (text-update-limit tv)
  tv)

(defun text-attach-scrollbars (tv &key vscroll hscroll)
  "Bind scroll bars so the editor scrolls with them (and updates them)."
  (attach-scrollbars tv :vscroll vscroll :hscroll hscroll)
  (text-update-limit tv)
  tv)

(defun text-string (tv)
  "Return the full contents of TV as one newline-joined string."
  (with-output-to-string (s)
    (loop for i below (fill-pointer (text-lines tv))
          do (write-string (aref (text-lines tv) i) s)
             (when (< i (1- (fill-pointer (text-lines tv)))) (terpri s)))))

(defun line-count (tv) (fill-pointer (text-lines tv)))
(defun nth-line (tv i) (aref (text-lines tv) i))
(defun current-line-string (tv) (nth-line tv (text-cur-line tv)))

(defun append-text (tv string &key (move-cursor t))
  "Append STRING (which may contain newlines) to the end of the buffer.
Used to stream output, e.g. REPL results."
  (let* ((lines (text-lines tv))
         (parts (%split-lines string))
         (last (1- (fill-pointer lines))))
    ;; first part extends the final existing line
    (setf (aref lines last) (concatenate 'string (aref lines last) (first parts)))
    (dolist (p (rest parts)) (vector-push-extend p lines))
    (when move-cursor
      (setf (text-cur-line tv) (1- (fill-pointer lines))
            (text-cur-col tv) (length (aref lines (1- (fill-pointer lines))))))
    (text-update-limit tv)
    (ensure-visible tv)
    tv))

;;; --- scrolling -------------------------------------------------------------

(defun ensure-visible (tv)
  "Scroll (via the scroller machinery, so bound scroll bars stay in sync) so
that the cursor is on screen."
  (text-update-limit tv)
  (let ((h (point-y (view-size tv)))
        (w (max 1 (point-x (view-size tv)))))
    (if (text-wrap tv)
        ;; vertical scroll is by logical line; keep the cursor's visual row in view
        (let* ((top (min (text-top-line tv) (text-cur-line tv)))
               (sidx (%seg-index (wrap-segments (current-line-string tv) w)
                                 (text-cur-col tv))))
          (loop for crow = (+ (%vrows-between tv top (text-cur-line tv) w) sidx)
                while (and (>= crow h) (< top (text-cur-line tv)))
                do (incf top))
          (scroll-to tv 0 (max 0 top)))
        (let ((dy (text-top-line tv)) (dx (text-left-col tv)))
          (when (< (text-cur-line tv) dy) (setf dy (text-cur-line tv)))
          (when (>= (text-cur-line tv) (+ dy h)) (setf dy (1+ (- (text-cur-line tv) h))))
          (when (< (text-cur-col tv) dx) (setf dx (text-cur-col tv)))
          (when (>= (text-cur-col tv) (+ dx w)) (setf dx (1+ (- (text-cur-col tv) w))))
          (scroll-to tv (max 0 dx) (max 0 dy))))))

;;; --- positions, selection, protected region --------------------------------

(defun text-pos (tv) (cons (text-cur-line tv) (text-cur-col tv)))
(defun pos< (a b) (or (< (car a) (car b)) (and (= (car a) (car b)) (< (cdr a) (cdr b)))))
(defun pos= (a b) (and (= (car a) (car b)) (= (cdr a) (cdr b))))

(defun selection-range (tv)
  "Return (values start-pos end-pos) for the current selection, normalised so
START precedes END; (values nil nil) when there is no selection."
  (let ((a (text-anchor tv)))
    (if (and a (not (pos= a (text-pos tv))))
        (let ((c (text-pos tv))) (if (pos< a c) (values a c) (values c a)))
        (values nil nil))))

(defun text-substring (tv start end)
  (if (= (car start) (car end))
      (subseq (nth-line tv (car start)) (cdr start) (cdr end))
      (with-output-to-string (o)
        (write-string (subseq (nth-line tv (car start)) (cdr start)) o) (terpri o)
        (loop for li from (1+ (car start)) below (car end)
              do (write-string (nth-line tv li) o) (terpri o))
        (write-string (subseq (nth-line tv (car end)) 0 (cdr end)) o))))

(defun selected-string (tv)
  (multiple-value-bind (s e) (selection-range tv)
    (when s (text-substring tv s e))))

(defun set-protect-boundary (tv &optional (line (text-cur-line tv)) (col (text-cur-col tv)))
  "Mark everything before (LINE,COL) read-only (used to protect REPL output)."
  (setf (text-protect tv) (cons line col)))

(defun pos-protected-p (tv pos)
  (and (text-protect tv) (pos< pos (text-protect tv))))

;;; --- drawing ---------------------------------------------------------------

;;; --- Lisp syntax highlighting ----------------------------------------------

(defun %synfg (base fg) (make-attr fg (attr-bg base)))

(defun %lisp-symchar-p (c)
  (or (alphanumericp c) (find c "+-*/@$%^&_=<>.~!?:")))

(defun %lisp-colorize (line base in-string)
  "Return (values ATTRS END-IN-STRING): ATTRS is a per-character attribute-byte
vector colouring LINE as Lisp (comments, strings, char literals, keywords).
IN-STRING means the line begins inside a \"...\" string from the line above."
  (let* ((n (length line))
         (attrs (make-array n :initial-element base :element-type '(unsigned-byte 8)))
         (comment (%synfg base 8)) (string (%synfg base 4)) (kw (%synfg base 14))
         (i 0) (instr in-string))
    (flet ((paint (a b attr) (loop for k from (max 0 a) below (min b n) do (setf (aref attrs k) attr))))
      (when instr
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

(defun %string-start-state (tv top)
  "Whether line TOP begins inside a string (scan the lines above it)."
  (let ((instr nil))
    (dotimes (i (min top (line-count tv)) instr)
      (multiple-value-bind (attrs s) (%lisp-colorize (nth-line tv i) 0 instr)
        (declare (ignore attrs))
        (setf instr s)))))

(defun %paren-match-offset (str target)
  "STR[TARGET] is ( or ).  Return the matching paren's offset, or NIL.  Skips
strings, ; comments and #\\ char literals."
  (let ((n (length str)) (stack '()) (i 0))
    (loop while (< i n) do
      (let ((c (char str i)))
        (cond
          ((char= c #\;) (loop while (and (< i n) (char/= (char str i) #\Newline)) do (incf i)))
          ((char= c #\")
           (incf i)
           (loop while (< i n) do
             (let ((d (char str i))) (incf i)
               (cond ((char= d #\\) (incf i)) ((char= d #\") (return))))))
          ((and (char= c #\#) (< (1+ i) n) (char= (char str (1+ i)) #\\)) (incf i 3))
          ((char= c #\() (push i stack) (incf i))
          ((char= c #\))
           (let ((open (and stack (pop stack))))
             (when (and open (or (= open target) (= i target)))
               (return-from %paren-match-offset (if (= i target) open i))))
           (incf i))
          (t (incf i)))))
    nil))

(defun %matching-parens (tv)
  "Return a list of two (LINE . COL) cells -- the paren at/just-before the
cursor and its match -- or NIL."
  (let* ((cl (text-cur-line tv)) (cc (text-cur-col tv))
         (line (nth-line tv cl)) (llen (length line)))
    (labels ((off (l c)
               (let ((o 0)) (dotimes (i l) (incf o (1+ (length (nth-line tv i))))) (+ o c)))
             (lc (o)
               (let ((l 0))
                 (loop (let ((ll (1+ (length (nth-line tv l)))))
                         (if (< o ll) (return (cons l o)) (progn (decf o ll) (incf l))))))))
      (let ((target (cond
                      ((and (< cc llen) (find (char line cc) "()")) (off cl cc))
                      ((and (> cc 0) (<= cc llen) (find (char line (1- cc)) "()")) (off cl (1- cc))))))
        (when target
          (let ((m (%paren-match-offset (text-string tv) target)))
            (when m (list (lc target) (lc m)))))))))

(defun match-paren-jump (tv)
  "Move the cursor to the paren matching the one at/just-before it.  Returns T
when it moved, NIL when there is no balanced paren at point."
  (let ((pair (%matching-parens tv)))
    (when pair
      (let ((dest (second pair)))                  ; (line . col) of the match
        (setf (text-cur-line tv) (car dest)
              (text-cur-col tv) (cdr dest))
        (ensure-visible tv)
        t))))

;;; --- Lisp auto-indent ------------------------------------------------------

;;; Indentation specs, after Emacs cl-indent: N = number of "distinguished"
;;; arguments (which indent +4); the rest of the body indents +2.  Operators
;;; with no spec are ordinary calls -- their args align under the first one.
(defparameter *lisp-indent-specs* (make-hash-table :test 'equal))

(dolist (spec '(("defun" . 2) ("defmacro" . 2) ("defmethod" . 2) ("defgeneric" . 2)
                ("defvar" . 1) ("defparameter" . 1) ("defconstant" . 1)
                ("define-condition" . 2) ("defclass" . 2) ("defstruct" . 1)
                ("defpackage" . 1) ("define-symbol-macro" . 1) ("define-modify-macro" . 2)
                ("lambda" . 1) ("let" . 1) ("let*" . 1) ("flet" . 1) ("labels" . 1)
                ("macrolet" . 1) ("symbol-macrolet" . 1)
                ("when" . 1) ("unless" . 1) ("case" . 1) ("ccase" . 1) ("ecase" . 1)
                ("typecase" . 1) ("etypecase" . 1) ("ctypecase" . 1) ("if" . 2)
                ("dolist" . 1) ("dotimes" . 1) ("do" . 2) ("do*" . 2)
                ("multiple-value-bind" . 2) ("destructuring-bind" . 2)
                ("with-open-file" . 1) ("with-output-to-string" . 1)
                ("with-input-from-string" . 1) ("with-slots" . 2) ("with-accessors" . 2)
                ("handler-case" . 1) ("handler-bind" . 1) ("restart-case" . 1)
                ("unwind-protect" . 1) ("block" . 1) ("catch" . 1) ("return-from" . 1)
                ("eval-when" . 1) ("prog1" . 1) ("prog2" . 2)
                ("progn" . 0) ("cond" . 0) ("tagbody" . 0) ("locally" . 0)
                ("ignore-errors" . 0)))
  (setf (gethash (car spec) *lisp-indent-specs*) (cdr spec)))

(defvar *lisp-indent-hook* nil
  "Optional (OPERATOR-NAME-STRING) -> N | NIL, consulted for operators not in
*LISP-INDENT-SPECS* -- e.g. to indent a user macro with a &body argument like a
special form.")

(defun %lisp-operator-spec (name)
  "The distinguished-argument count for operator NAME, or NIL for an ordinary
call (whose args align under the first argument)."
  (multiple-value-bind (n found) (gethash name *lisp-indent-specs*)
    (cond (found n)
          (*lisp-indent-hook* (funcall *lisp-indent-hook* name))
          (t nil))))

(defun %lisp-token (str i)
  "Read the symbol token starting at index I in STR, lowercased."
  (let ((n (length str)) (j i))
    (loop while (and (< j n)
                     (not (member (char str j)
                                  '(#\Space #\Tab #\Newline #\Return #\( #\) #\" #\;))))
          do (incf j))
    (string-downcase (subseq str i j))))

(defparameter *lisp-loop-keywords*
  ;; enough clause-introducing keywords that "the last keyword" tracks correctly
  (let ((h (make-hash-table :test 'equal)))
    (dolist (k '("for" "as" "with" "and" "do" "doing" "collect" "collecting"
                 "append" "appending" "nconc" "nconcing" "sum" "summing" "count"
                 "counting" "maximize" "maximizing" "minimize" "minimizing"
                 "when" "unless" "if" "else" "end" "while" "until" "repeat"
                 "always" "never" "thereis" "return" "initially" "finally"
                 "named" "into" "being" "then" "across" "in" "on" "=" "by"))
      (setf (gethash k h) t))
    h))

(defparameter *lisp-loop-conditionals* '("when" "unless" "if" "else"))

(defun %loop-last-keyword (str start off)
  "The last top-level loop keyword (lowercased) before OFF in the loop starting
at index START, or \"\".  Used to detect when a clause body follows a when/if."
  (let ((n (min off (length str))) (i (1+ start)) (depth 0) (last ""))
    (loop while (< i n) do
      (let ((c (char str i)))
        (cond
          ((char= c #\;) (loop while (and (< i n) (char/= (char str i) #\Newline)) do (incf i)))
          ((char= c #\")
           (incf i)
           (loop while (< i n) do
             (let ((d (char str i))) (incf i) (cond ((char= d #\\) (incf i)) ((char= d #\") (return))))))
          ((and (char= c #\#) (< (1+ i) n) (char= (char str (1+ i)) #\\)) (incf i 3))
          ((char= c #\() (incf depth) (incf i))
          ((char= c #\)) (when (plusp depth) (decf depth)) (incf i))
          ((member c '(#\Space #\Tab #\Newline #\Return)) (incf i))
          (t (let ((tok (%lisp-token str i)))
               (when (and (zerop depth) (gethash tok *lisp-loop-keywords*)) (setf last tok))
               (incf i (max 1 (length tok))))))))
    last))

(defun %lisp-indent-at (str off)
  "The indentation column for a fresh line broken at OFF in STR.  Each open form
on the stack tracks (OPEN-COL ELEMENT-COUNT HEAD FIRST-ARG-COL DATAP OPEN-INDEX);
DATAP marks a quoted/binding/literal list (its elements align, no body rule)."
  (let ((n (min off (length str))) (i 0) (col 0) (stack '()) (fresh t)
        (q nil) (comma nil))   ; q: after ' or `  ;  comma: after ,
    (flet ((start-elem (istart is-token)
             (when stack
               (let ((e (car stack)))
                 (incf (second e))
                 (cond ((= (second e) 1) (setf (third e) (if is-token (%lisp-token str istart) "")))
                       ((= (second e) 2) (setf (fourth e) col)))))))
      (loop while (< i n) do
        (let ((c (char str i)))
          (cond
            ((char= c #\Newline) (setf col 0 fresh t) (incf i))
            ((char= c #\;) (loop while (and (< i n) (char/= (char str i) #\Newline)) do (incf i)))
            ((member c '(#\Space #\Tab #\Return)) (incf i) (incf col) (setf fresh t))
            ((or (char= c #\') (char= c #\`)) (setf q t comma nil) (incf i) (incf col))
            ((char= c #\,) (setf comma t q nil) (incf i) (incf col))
            ((char= c #\")
             (start-elem i nil)
             (incf i) (incf col)
             (loop while (< i n) do
               (let ((d (char str i)))
                 (cond ((char= d #\Newline) (setf col 0 i (1+ i)))
                       ((char= d #\\) (incf i 2) (incf col 2))
                       ((char= d #\") (incf i) (incf col) (return))
                       (t (incf i) (incf col)))))
             (setf fresh nil q nil comma nil))
            ((and (char= c #\#) (< (1+ i) n) (char= (char str (1+ i)) #\\))
             (start-elem i nil) (incf i 3) (incf col 3) (setf fresh nil q nil comma nil))
            ((char= c #\()
             (start-elem i nil)
             (push (list col 0 nil nil
                         (or q (and stack (fifth (car stack)) (not comma)))  ; quoted/data?
                         i)
                   stack)
             (setf q nil comma nil fresh t) (incf i) (incf col))
            ((char= c #\)) (when stack (pop stack)) (incf i) (incf col) (setf fresh nil q nil comma nil))
            (t (when fresh (start-elem i t)) (incf i) (incf col) (setf fresh nil q nil comma nil)))))
      (if (null stack)
          0
          (destructuring-bind (open-col nelems head first-arg-col datap open-idx) (car stack)
            (let ((spec (and head (%lisp-operator-spec head))))
              (cond
                ((zerop nelems) (1+ open-col))               ; immediately after "("
                (datap (1+ open-col))                        ; quoted/binding/literal list
                ((and (equal head "loop") first-arg-col)     ; loop: clause / conditional body
                 (if (member (%loop-last-keyword str open-idx off) *lisp-loop-conditionals*
                             :test #'string=)
                     (+ first-arg-col 2)
                     first-arg-col))
                (spec (if (<= nelems spec) (+ open-col 4) (+ open-col 2)))
                ((or (string= head "")                       ; data list by head shape
                     (let ((c0 (char head 0))) (or (digit-char-p c0) (char= c0 #\#))))
                 (1+ open-col))
                (first-arg-col first-arg-col)                ; ordinary call: align under arg 1
                (t (1+ open-col)))))))))                     ; head only, no spec

(defun %cursor-offset (tv)
  (let ((o 0))
    (dotimes (i (text-cur-line tv)) (incf o (1+ (length (nth-line tv i)))))
    (+ o (text-cur-col tv))))

(defun %offset->lc (tv off)
  "Convert character offset OFF (into TEXT-STRING) to a (LINE . COL) cons."
  (let ((l 0) (lc (line-count tv)))
    (loop (let ((len (length (nth-line tv l))))
            (when (or (<= off len) (>= (1+ l) lc)) (return (cons l (min (max 0 off) len))))
            (decf off (1+ len)) (incf l)))))

(defun lisp-indent-line (tv li)
  "Re-indent line LI of TV for Lisp (in place), keeping the cursor sensible."
  (when (< li (line-count tv))
    (let* ((line (nth-line tv li))
           (lead (let ((k 0))
                   (loop while (and (< k (length line)) (member (char line k) '(#\Space #\Tab)))
                         do (incf k))
                   k))
           (start-off (let ((o 0)) (dotimes (i li) (incf o (1+ (length (nth-line tv i))))) o))
           (want (%lisp-indent-at (text-string tv) start-off))
           (new (concatenate 'string (make-string want :initial-element #\Space) (subseq line lead))))
      (unless (string= new line)
        (set-line tv li new)
        (when (= (text-cur-line tv) li)
          (let ((cc (text-cur-col tv)))
            (setf (text-cur-col tv) (if (<= cc lead) want (max 0 (+ cc (- want lead)))))))
        (text-update-limit tv)))))

(defun lisp-indent-region (tv l0 l1)
  "Re-indent lines L0..L1 top-to-bottom (each sees the lines above reindented)."
  (loop for li from (max 0 l0) to (min l1 (1- (line-count tv))) do (lisp-indent-line tv li)))

(defun %toplevel-span (str off)
  "Return (values START END) char-offsets of the top-level form containing OFF."
  (let ((n (length str)) (i 0) (depth 0) (start nil))
    (loop while (< i n) do
      (let ((c (char str i)))
        (cond
          ((char= c #\;) (loop while (and (< i n) (char/= (char str i) #\Newline)) do (incf i)))
          ((char= c #\")
           (incf i)
           (loop while (< i n) do
             (let ((d (char str i))) (incf i) (cond ((char= d #\\) (incf i)) ((char= d #\") (return))))))
          ((and (char= c #\#) (< (1+ i) n) (char= (char str (1+ i)) #\\)) (incf i 3))
          ((char= c #\() (when (zerop depth) (setf start i)) (incf depth) (incf i))
          ((char= c #\))
           (incf i) (when (plusp depth) (decf depth))
           (when (and (zerop depth) start (<= start off) (<= off i))
             (return-from %toplevel-span (values start i)))
           (when (zerop depth) (setf start nil)))
          (t (incf i)))))
    (values nil nil)))

(defun lisp-indent-sexp (tv)
  "Re-indent the whole top-level form containing the cursor."
  (multiple-value-bind (s e) (%toplevel-span (text-string tv) (%cursor-offset tv))
    (when s (lisp-indent-region tv (car (%offset->lc tv s)) (car (%offset->lc tv e))))))

(defgeneric text-tab (tv)
  (:documentation "Handle the Tab key while editing (overridable).")
  (:method ((tv ttext-view))
    (when (text-anchor tv) (delete-selection tv))
    (dotimes (_ (- 4 (mod (text-cur-col tv) 4))) (insert-char-at-cursor tv #\Space))))

(defmethod draw ((tv ttext-view))
  (if (text-wrap tv) (%draw-wrapped tv) (%draw-flat tv)))

(defun %draw-glyphs (db line start end w c &optional attrs)
  "Lay LINE[START,END) into draw-buffer DB across display columns [0,W): one cell
per narrow glyph, two per wide one, interning any multi-code-point grapheme
cluster.  ATTRS, when non-NIL, is a per-code-point attribute array (else C).
Shared by the flat and word-wrapped layouts."
  (let* ((simple (simple-line-p line))
         (offs (unless simple (grapheme-offsets line))))
    (loop with vx = 0 and i = start
          while (and (< i end) (< vx w)) do
            (let* ((gend (if simple (1+ i)
                             (min end (or (find-if (lambda (o) (> o i)) offs) end))))
                   (base (char line i))
                   (cw (char-width base))
                   (attr (if attrs (aref attrs i) c)))
              (if (= (- gend i) 1)
                  (db-fill db base attr vx 1)
                  (db-put-code db vx (intern-grapheme (subseq line i gend)) attr))
              (when (and (= cw 2) (< (1+ vx) w))
                (db-put-code db (1+ vx) +wide-cont+ attr))
              (incf vx cw) (setf i gend)))))

(defun %draw-flat (tv)
  (let* ((w (point-x (view-size tv)))
         (h (point-y (view-size tv)))
         (c (get-color tv 1))
         (hi (get-color tv 2))
         (dx (text-left-col tv))
         (db (make-draw-buffer w))
         (hl (text-highlight tv))
         (instr (and hl (%string-start-state tv (text-top-line tv))))
         (parens (and hl (logtest (view-state tv) +sf-focused+) (%matching-parens tv)))
         (paren-hi (%synfg c 15)))   ; matching paren: bright
    (multiple-value-bind (sels sele) (selection-range tv)
      (dotimes (row h)
        (db-fill db #\Space c)
        (let ((li (+ (text-top-line tv) row)))
          (when (< li (line-count tv))
            (let* ((line (nth-line tv li)) (len (length line))
                   (start (min dx len))
                   (attrs (when hl
                            (multiple-value-bind (a s) (%lisp-colorize line c instr)
                              (setf instr s) a))))
              ;; lay out by grapheme cluster: a multi-code-point cluster is one
              ;; interned glyph; a wide glyph also claims the next cell.
              (%draw-glyphs db line start len w c attrs)
              ;; matching-paren accent
              (when parens
                (dolist (p parens)
                  (when (and (= (car p) li) (>= (cdr p) start) (< (cdr p) len))
                    (let ((sx (visual-col line start (cdr p))))
                      (when (< sx w)
                        (db-put-attribute db sx paren-hi (char-width (char line (cdr p)))))))))
              ;; highlight the selected span on this line
              (when (and sels (<= (car sels) li (car sele)))
                (let* ((hs (if (= li (car sels)) (cdr sels) 0))
                       (he (if (= li (car sele)) (cdr sele) len))
                       (vs (max 0 (visual-col line start hs)))
                       (ve (min w (visual-col line start he))))
                  (when (< vs ve) (db-put-attribute db vs hi (- ve vs)))))))
          (write-line* tv 0 row w 1 db)))
      (when (logtest (view-state tv) +sf-focused+)
        (let ((line (nth-line tv (text-cur-line tv))))
          (set-cursor tv (visual-col line (min dx (length line)) (text-cur-col tv))
                      (- (text-cur-line tv) (text-top-line tv))))))))

(defun %draw-wrapped (tv)
  (let* ((w (max 1 (point-x (view-size tv))))
         (h (point-y (view-size tv)))
         (c (get-color tv 1)) (hi (get-color tv 2))
         (db (make-draw-buffer w))
         (row 0) (li (text-top-line tv)))
    (multiple-value-bind (sels sele) (selection-range tv)
      (loop while (< row h) do
        (cond
          ((< li (line-count tv))
           (let* ((line (nth-line tv li)) (len (length line))
                  (segs (wrap-segments line w)) (nseg (length segs)))
             (dotimes (seg nseg)
               (when (>= row h) (return))
               (let* ((start (nth seg segs))
                      (end (if (< (1+ seg) nseg) (nth (1+ seg) segs) len)))
                 (db-fill db #\Space c)
                 (%draw-glyphs db line start end w c)
                 ;; highlight the selected span lying within this segment
                 (when (and sels (<= (car sels) li (car sele)))
                   (let* ((hs (if (= li (car sels)) (cdr sels) 0))
                          (he (if (= li (car sele)) (cdr sele) len))
                          (vs (visual-col line start (max start (min hs end))))
                          (ve (min w (visual-col line start (max start (min he end))))))
                     (when (< vs ve) (db-put-attribute db vs hi (- ve vs)))))
                 (write-line* tv 0 row w 1 db))
               (incf row))
             (incf li)))
          (t (db-fill db #\Space c) (write-line* tv 0 row w 1 db) (incf row)))))
    (when (logtest (view-state tv) +sf-focused+)
      (let* ((line (nth-line tv (text-cur-line tv)))
             (cc (text-cur-col tv))
             (segs (wrap-segments line w))
             (sidx (%seg-index segs cc))
             (sstart (nth sidx segs)))
        (set-cursor tv (visual-col line sstart cc)
                    (+ (%vrows-between tv (text-top-line tv) (text-cur-line tv) w)
                       sidx))))))

;;; --- editing primitives ----------------------------------------------------

(defun set-line (tv i s)
  (setf (text-modified tv) t)
  (setf (aref (text-lines tv) i) s))

(defun insert-char-at-cursor (tv ch)
  (let* ((l (current-line-string tv)) (col (text-cur-col tv)))
    (set-line tv (text-cur-line tv)
              (concatenate 'string (subseq l 0 col) (string ch) (subseq l col)))
    (incf (text-cur-col tv))))

(defun delete-char-before-cursor (tv)
  (let ((col (text-cur-col tv)) (li (text-cur-line tv)))
    (cond
      ((> col 0)
       ;; delete the whole preceding grapheme cluster, not just one code point
       (let* ((l (current-line-string tv)) (pcol (prev-grapheme-col l col)))
         (set-line tv li (concatenate 'string (subseq l 0 pcol) (subseq l col)))
         (setf (text-cur-col tv) pcol)))
      ((> li 0)
       ;; join with previous line
       (let* ((prev (nth-line tv (1- li))) (cur (current-line-string tv)))
         (setf (text-cur-col tv) (length prev))
         (set-line tv (1- li) (concatenate 'string prev cur))
         (remove-line tv li)
         (decf (text-cur-line tv)))))))

(defun delete-char-at-cursor (tv)
  (let* ((l (current-line-string tv)) (col (text-cur-col tv)) (li (text-cur-line tv)))
    (cond
      ((< col (length l))
       ;; delete the whole grapheme cluster under the cursor
       (let ((nend (next-grapheme-col l col)))
         (set-line tv li (concatenate 'string (subseq l 0 col) (subseq l nend)))))
      ((< li (1- (line-count tv)))
       (set-line tv li (concatenate 'string l (nth-line tv (1+ li))))
       (remove-line tv (1+ li))))))

(defun remove-line (tv i)
  (setf (text-modified tv) t)
  (let ((lines (text-lines tv)))
    (loop for k from i below (1- (fill-pointer lines))
          do (setf (aref lines k) (aref lines (1+ k))))
    (decf (fill-pointer lines))
    (when (zerop (fill-pointer lines)) (vector-push-extend "" lines))))

(defun split-line-at-cursor (tv)
  "Break the current line at the cursor, creating a new line (Enter default)."
  (let* ((lines (text-lines tv)) (li (text-cur-line tv))
         (l (current-line-string tv)) (col (text-cur-col tv)))
    (set-line tv li (subseq l 0 col))
    ;; make room and insert the tail as a new line
    (vector-push-extend "" lines)
    (loop for k from (1- (fill-pointer lines)) above (1+ li)
          do (setf (aref lines k) (aref lines (1- k))))
    (setf (aref lines (1+ li)) (subseq l col))
    (incf (text-cur-line tv))
    (setf (text-cur-col tv) 0)))

(defun insert-string (tv str)
  "Insert STR (which may contain newlines) at the cursor."
  (loop for ch across str
        do (if (char= ch #\Newline)
               (split-line-at-cursor tv)
               (insert-char-at-cursor tv ch))))

;;; --- undo / clipboard / selection edits ------------------------------------

(defun %capture-state (tv)
  (list (subseq (text-lines tv) 0 (line-count tv))
        (text-cur-line tv) (text-cur-col tv) (text-protect tv) (text-modified tv)))

(defun %restore-state (tv state)
  (destructuring-bind (lines cl cc prot mod) state
    (let ((v (make-array (length lines) :adjustable t :fill-pointer (length lines))))
      (dotimes (i (length lines)) (setf (aref v i) (aref lines i)))
      (setf (text-lines tv) v
            (text-cur-line tv) cl (text-cur-col tv) cc
            (text-protect tv) prot (text-anchor tv) nil (text-modified tv) mod)))
  (text-update-limit tv))

(defun text-snapshot (tv)
  "Record the current state for undo; a fresh edit invalidates the redo stack."
  (push (%capture-state tv) (text-undo tv))
  (setf (text-redo tv) '())
  (when (> (length (text-undo tv)) 500)
    (setf (text-undo tv) (subseq (text-undo tv) 0 500))))

(defun text-undo! (tv)
  (when (text-undo tv)
    (push (%capture-state tv) (text-redo tv))
    (%restore-state tv (pop (text-undo tv)))))

(defun text-redo! (tv)
  (when (text-redo tv)
    (push (%capture-state tv) (text-undo tv))
    (%restore-state tv (pop (text-redo tv)))))

(defun delete-selection (tv)
  "Delete the selected text (respecting the protected region)."
  (multiple-value-bind (s e) (selection-range tv)
    (when (and s (not (pos-protected-p tv s)))
      (let* ((sl (car s)) (sc (cdr s)) (el (car e)) (ec (cdr e))
             (head (subseq (nth-line tv sl) 0 sc))
             (tail (subseq (nth-line tv el) ec)))
        (set-line tv sl (concatenate 'string head tail))
        (loop repeat (- el sl) do (remove-line tv (1+ sl)))
        (setf (text-cur-line tv) sl (text-cur-col tv) sc)))
    (setf (text-anchor tv) nil)))

(defun copy-selection (tv)
  (let ((s (selected-string tv))) (when s (setf *clipboard* s) t)))

(defun cut-selection (tv)
  (when (selected-string tv)
    (text-snapshot tv) (copy-selection tv) (delete-selection tv) t))

(defun paste-clipboard (tv)
  (when (and (plusp (length *clipboard*)) (not (pos-protected-p tv (text-pos tv))))
    (text-snapshot tv)
    (when (text-anchor tv) (delete-selection tv))
    (insert-string tv *clipboard*) t))

;;; --- the overridable Enter hook --------------------------------------------

(defgeneric text-return (tv)
  (:documentation "Handle the Enter key.  The default inserts a newline; a REPL
subclass overrides this to evaluate the current input instead.")
  (:method ((tv ttext-view)) (split-line-at-cursor tv)))

;;; --- events ----------------------------------------------------------------

(defun clamp-cursor (tv)
  (setf (text-cur-line tv) (min (max 0 (text-cur-line tv)) (1- (line-count tv)))
        (text-cur-col tv)  (min (max 0 (text-cur-col tv))
                                (length (current-line-string tv)))))

(defun %mouse-to-cursor (tv event)
  "Move the cursor to the click/drag position of EVENT."
  (let* ((lp (make-local tv (event-mouse-where event)))
         (mx (max 0 (point-x lp))) (my (max 0 (point-y lp))))
    (if (text-wrap tv)
        (let ((w (max 1 (point-x (view-size tv)))) (li (text-top-line tv)) (acc 0) (done nil))
          (loop while (and (not done) (< li (line-count tv))) do
            (let* ((line (nth-line tv li)) (segs (wrap-segments line w)) (nseg (length segs)))
              (if (< my (+ acc nseg))
                  (let* ((s (- my acc))
                         (sstart (nth s segs))
                         (send (if (< (1+ s) nseg) (nth (1+ s) segs) (length line))))
                    (setf (text-cur-line tv) li
                          (text-cur-col tv) (col-at-vcol line sstart send mx)
                          done t))
                  (progn (incf acc nseg) (incf li)))))
          (unless done
            (setf (text-cur-line tv) (1- (line-count tv))
                  (text-cur-col tv) (length (nth-line tv (1- (line-count tv)))))))
        (let* ((li (min (1- (line-count tv)) (+ (text-top-line tv) my)))
               (line (nth-line tv li))
               (start (min (text-left-col tv) (length line))))
          (setf (text-cur-line tv) li
                ;; walk grapheme clusters to find the boundary under MX
                (text-cur-col tv)
                (loop with vx = 0 and i = start
                      while (< i (length line))
                      for cw = (char-width (char line i))
                      when (> (+ vx cw) mx) do (return i)
                      do (incf vx cw) (setf i (next-grapheme-col line i))
                      finally (return (length line))))))
    (clamp-cursor tv)))

(defun %wrap-vmove (tv dir)
  "Move the cursor one VISUAL row (DIR -1 up / +1 down) in word-wrap mode,
keeping the goal visual column across the move (width- and grapheme-aware)."
  (let* ((w (max 1 (point-x (view-size tv))))
         (line (current-line-string tv))
         (segs (wrap-segments line w))
         (nseg (length segs))
         (cc (text-cur-col tv))
         (sidx (%seg-index segs cc))
         (sstart (nth sidx segs))
         (goal (or (text-goal-col tv) (visual-col line sstart cc))))
    (setf (text-goal-col tv) goal)
    (flet ((seg-end (ss s) (if (< (1+ s) (length ss)) (nth (1+ s) ss) nil)))
      (if (plusp dir)
          (if (< sidx (1- nseg))                                ; another segment below
              (setf (text-cur-col tv)
                    (col-at-vcol line (nth (1+ sidx) segs)
                                 (or (seg-end segs (1+ sidx)) (length line)) goal))
              (when (< (text-cur-line tv) (1- (line-count tv))) ; -> next logical line
                (incf (text-cur-line tv))
                (let* ((nl (current-line-string tv)) (ns (wrap-segments nl w)))
                  (setf (text-cur-col tv)
                        (col-at-vcol nl 0 (or (seg-end ns 0) (length nl)) goal)))))
          (if (> sidx 0)                                        ; another segment above
              (setf (text-cur-col tv)
                    (col-at-vcol line (nth (1- sidx) segs) (nth sidx segs) goal))
              (when (> (text-cur-line tv) 0)                    ; -> prev line's last segment
                (decf (text-cur-line tv))
                (let* ((pl (current-line-string tv)) (ps (wrap-segments pl w))
                       (last (1- (length ps))))
                  (setf (text-cur-col tv)
                        (col-at-vcol pl (nth last ps) (length pl) goal)))))))))

(defun %move-cursor (tv k)
  "Apply a navigation key K to the cursor (no selection / redraw side effects)."
  (cond
    ((= k +kb-up+)    (if (text-wrap tv) (%wrap-vmove tv -1) (decf (text-cur-line tv))))
    ((= k +kb-down+)  (if (text-wrap tv) (%wrap-vmove tv +1) (incf (text-cur-line tv))))
    ((= k +kb-left+)  (if (> (text-cur-col tv) 0)
                          (setf (text-cur-col tv)
                                (prev-grapheme-col (current-line-string tv) (text-cur-col tv)))
                          (when (> (text-cur-line tv) 0)
                            (decf (text-cur-line tv))
                            (setf (text-cur-col tv) (length (current-line-string tv))))))
    ((= k +kb-right+) (if (< (text-cur-col tv) (length (current-line-string tv)))
                          (setf (text-cur-col tv)
                                (next-grapheme-col (current-line-string tv) (text-cur-col tv)))
                          (when (< (text-cur-line tv) (1- (line-count tv)))
                            (incf (text-cur-line tv)) (setf (text-cur-col tv) 0))))
    ((= k +kb-home+)  (setf (text-cur-col tv) 0))
    ((= k +kb-end+)   (setf (text-cur-col tv) (length (current-line-string tv))))
    ((= k +kb-pgup+)  (decf (text-cur-line tv) (max 1 (1- (point-y (view-size tv))))))
    ((= k +kb-pgdn+)  (incf (text-cur-line tv) (max 1 (1- (point-y (view-size tv))))))))

(defun navigation-key-p (k)
  (member k (list +kb-up+ +kb-down+ +kb-left+ +kb-right+
                  +kb-home+ +kb-end+ +kb-pgup+ +kb-pgdn+)))

(defun %word-char-p (ch) (or (alphanumericp ch) (char= ch #\_)))

(defun word-left (tv)
  "Move the cursor to the start of the previous word."
  (let ((line (current-line-string tv)) (col (text-cur-col tv)))
    (cond
      ((zerop col) (%move-cursor tv +kb-left+))   ; wrap to end of previous line
      (t (decf col)
         (loop while (and (> col 0) (not (%word-char-p (char line col)))) do (decf col))
         (loop while (and (> col 0) (%word-char-p (char line (1- col)))) do (decf col))
         (setf (text-cur-col tv) col)))))

(defun word-right (tv)
  "Move the cursor to the start of the next word."
  (let* ((line (current-line-string tv)) (len (length line)) (col (text-cur-col tv)))
    (cond
      ((>= col len) (%move-cursor tv +kb-right+))  ; wrap to next line
      (t (loop while (and (< col len) (%word-char-p (char line col))) do (incf col))
         (loop while (and (< col len) (not (%word-char-p (char line col)))) do (incf col))
         (setf (text-cur-col tv) col)))))

(defun text-goto (tv line &optional (col 0))
  "Move the cursor to (LINE, COL) (1-based LINE for the public API), redraw."
  (setf (text-anchor tv) nil
        (text-cur-line tv) (max 0 (min (1- line) (1- (line-count tv))))
        (text-cur-col tv) col)
  (clamp-cursor tv) (ensure-visible tv) (draw-view tv))

(defun can-edit-here-p (tv &optional (delete-before nil))
  "Whether an edit at the cursor is allowed given the protected region."
  (if (text-protect tv)
      (if delete-before
          (pos< (text-protect tv) (text-pos tv))   ; need cursor strictly past boundary
          (not (pos< (text-pos tv) (text-protect tv))))
      t))

(defmethod handle-event ((tv ttext-view) event)
  (cond
    ;; a bound scroll bar moved: follow it (without moving the text cursor)
    ((scrollbar-event-p tv event)
     (scroll-from-scrollbars tv))
    ((and (= (event-type event) +ev-mouse-wheel+) (mouse-in-view-p tv event))
     (scroll-to tv (point-x (scroller-delta tv))
                (+ (point-y (scroller-delta tv)) (* 3 (event-wheel event))))
     (clear-event event))
    ;; mouse down: position the cursor and begin a (possible) selection
    ((and (= (event-type event) +ev-mouse-down+) (mouse-in-view-p tv event))
     (setf (text-goal-col tv) nil)
     (%mouse-to-cursor tv event)
     (setf (text-anchor tv) (text-pos tv))   ; drag from here; click w/o drag = no sel
     (ensure-visible tv) (draw-view tv)
     (clear-event event))
    ;; mouse drag: extend the selection to the pointer
    ((= (event-type event) +ev-mouse-move+)
     (when (text-anchor tv)
       (%mouse-to-cursor tv event)
       (ensure-visible tv) (draw-view tv))
     (clear-event event))
    ;; mouse up: a click with no drag leaves no selection
    ((= (event-type event) +ev-mouse-up+)
     (when (and (text-anchor tv) (pos= (text-anchor tv) (text-pos tv)))
       (setf (text-anchor tv) nil) (draw-view tv))
     (clear-event event))
    ((and (= (event-type event) +ev-key-down+)
          (logtest (view-state tv) +sf-focused+))
     (let* ((k (event-key-code event)) (ch (event-char-code event))
            (mods (event-modifiers event))
            (ctrl (logtest mods +md-ctrl+))
            (ctrl-letter (and ctrl (<= 1 ch 26) (code-char (+ ch 96))))
            (handled t))
       ;; Up/Down keep the goal visual column; everything else resets it.
       (unless (or (= k +kb-up+) (= k +kb-down+)) (setf (text-goal-col tv) nil))
       (cond
         ;; clipboard / undo
         ((eql ctrl-letter #\c) (copy-selection tv))
         ((eql ctrl-letter #\x) (if (text-read-only tv) (setf handled nil) (cut-selection tv)))
         ((eql ctrl-letter #\v) (if (text-read-only tv) (setf handled nil) (paste-clipboard tv)))
         ((eql ctrl-letter #\z) (if (text-read-only tv) (setf handled nil) (text-undo! tv)))
         ((eql ctrl-letter #\y) (if (text-read-only tv) (setf handled nil) (text-redo! tv)))
         ((eql ctrl-letter #\a)             ; select all
          (setf (text-anchor tv) (cons 0 0)
                (text-cur-line tv) (1- (line-count tv))
                (text-cur-col tv) (length (nth-line tv (1- (line-count tv))))))
         ;; Insert toggles overwrite mode (block cursor)
         ((= k +kb-ins+)
          (setf (text-overwrite tv) (not (text-overwrite tv)))
          (if (text-overwrite tv) (block-cursor tv) (normal-cursor tv)))
         ;; Ctrl+Left/Right move by word; plain arrows move by char
         ((and ctrl (or (= k +kb-left+) (= k +kb-right+)))
          (if (logtest mods +md-shift+)
              (unless (text-anchor tv) (setf (text-anchor tv) (text-pos tv)))
              (setf (text-anchor tv) nil))
          (if (= k +kb-left+) (word-left tv) (word-right tv)))
         ;; navigation (Shift extends the selection)
         ((navigation-key-p k)
          (if (logtest mods +md-shift+)
              (unless (text-anchor tv) (setf (text-anchor tv) (text-pos tv)))
              (setf (text-anchor tv) nil))
          (%move-cursor tv k))
         ;; Tab inserts spaces (to the next multiple of 4)
         ((= k +kb-tab+)
          (if (or (text-read-only tv) (not (can-edit-here-p tv)))
              (setf handled nil)
              (progn (text-snapshot tv) (text-tab tv))))
         ((= k +kb-enter+)
          (if (text-read-only tv) (setf handled nil)
              (progn (text-snapshot tv)
                     (when (text-anchor tv) (delete-selection tv))
                     (text-return tv))))
         ((= k +kb-back+)
          (cond ((text-read-only tv) (setf handled nil))
                ((text-anchor tv) (text-snapshot tv) (delete-selection tv))
                ((can-edit-here-p tv t) (text-snapshot tv) (delete-char-before-cursor tv))
                (t (setf handled nil))))
         ((= k +kb-del+)
          (cond ((text-read-only tv) (setf handled nil))
                ((text-anchor tv) (text-snapshot tv) (delete-selection tv))
                ((can-edit-here-p tv) (text-snapshot tv) (delete-char-at-cursor tv))
                (t (setf handled nil))))
         ;; a printable character: ASCII or any non-control Unicode code point
         ;; (input.lisp assembles multi-byte UTF-8 into a single code point)
         ((and (>= ch 32) (/= ch 127) (< ch char-code-limit) (not ctrl)
               (let ((c (code-char ch))) (and c (graphic-char-p c))))
          (cond ((text-read-only tv) (setf handled nil))
                ((not (can-edit-here-p tv)) (setf handled nil))
                (t (text-snapshot tv)
                   (cond
                     ((text-anchor tv) (delete-selection tv))
                     ;; overwrite mode: replace the char under the cursor
                     ((and (text-overwrite tv)
                           (< (text-cur-col tv) (length (current-line-string tv))))
                      (delete-char-at-cursor tv)))
                   (insert-char-at-cursor tv (code-char ch)))))
         (t (setf handled nil)))
       (when handled
         (clamp-cursor tv) (ensure-visible tv) (draw-view tv)
         (clear-event event))))))

(defmethod data-size ((tv ttext-view)) (length (text-string tv)))
(defmethod get-data ((tv ttext-view)) (text-string tv))
(defmethod set-data ((tv ttext-view) data) (set-text tv (princ-to-string data)))

;;; --- search ----------------------------------------------------------------

(defun %match-at-p (line pattern pos test)
  (and (<= (+ pos (length pattern)) (length line))
       (loop for i below (length pattern)
             always (funcall test (char pattern i) (char line (+ pos i))))))

(defun %word-match-p (line pos len)
  "True when the LEN-char span at POS is bounded by non-word chars (or edges)."
  (and (or (zerop pos) (not (%word-char-p (char line (1- pos)))))
       (or (>= (+ pos len) (length line)) (not (%word-char-p (char line (+ pos len)))))))

(defun %line-find-fwd (line pattern start test whole-word)
  (loop with len = (length pattern)
        for p from (max 0 start) to (- (length line) len)
        when (and (%match-at-p line pattern p test)
                  (or (not whole-word) (%word-match-p line p len)))
        do (return p)))

(defun %line-find-back (line pattern before test whole-word)
  "Largest match position strictly before column BEFORE."
  (loop with len = (length pattern)
        for p from (min (1- before) (- (length line) len)) downto 0
        when (and (%match-at-p line pattern p test)
                  (or (not whole-word) (%word-match-p line p len)))
        do (return p)))

(defun text-find (tv pattern &key from-line from-col case-sensitive backward whole-word)
  "Search for PATTERN from (FROM-LINE,FROM-COL) (default: cursor).  When BACKWARD,
search toward the start.  Return (cons line col) of the match, or NIL."
  (when (plusp (length pattern))
    (let ((test (if case-sensitive #'char= #'char-equal))
          (fl (or from-line (text-cur-line tv)))
          (fc (or from-col (text-cur-col tv))))
      (if backward
          (loop for li from (min fl (1- (line-count tv))) downto 0
                for line = (nth-line tv li)
                for before = (if (= li fl) fc (1+ (length line)))
                for p = (%line-find-back line pattern before test whole-word)
                when p do (return (cons li p)))
          (loop for li from fl below (line-count tv)
                for line = (nth-line tv li)
                for start = (if (= li fl) fc 0)
                for p = (%line-find-fwd line pattern start test whole-word)
                when p do (return (cons li p)))))))

(defun text-select-match (tv pos pattern)
  "Select the PATTERN-length span at POS and put the cursor at its end."
  (setf (text-anchor tv) (copy-tree pos)
        (text-cur-line tv) (car pos)
        (text-cur-col tv) (+ (cdr pos) (length pattern)))
  (ensure-visible tv) (draw-view tv))

(defun text-find-and-select (tv pattern &key case-sensitive whole-word backward wrap)
  "Find the next (or previous) PATTERN and select it.  Return T on success."
  (let* ((from (if (and backward (text-anchor tv)) (text-anchor tv) (text-pos tv)))
         (m (or (text-find tv pattern :from-line (car from) :from-col (cdr from)
                           :case-sensitive case-sensitive :whole-word whole-word
                           :backward backward)
                (and wrap
                     (let ((last (1- (line-count tv))))
                       (text-find tv pattern
                                  :from-line (if backward last 0)
                                  :from-col (if backward (1+ (length (nth-line tv last))) 0)
                                  :case-sensitive case-sensitive :whole-word whole-word
                                  :backward backward))))))
    (when m (text-select-match tv m pattern) t)))

(defun text-replace-selection (tv string)
  "Replace the current selection (if any) with STRING; cursor ends after it."
  (text-snapshot tv)
  (when (text-anchor tv) (delete-selection tv))
  (insert-string tv string)
  (ensure-visible tv) (draw-view tv))

(defun text-replace-all (tv from to &key case-sensitive whole-word)
  "Replace every occurrence of FROM with TO (within lines).  Return the count.
Undoable (one snapshot)."
  (if (zerop (length from))
      0
      (let ((test (if case-sensitive #'char= #'char-equal)) (len (length from)) (count 0))
        (text-snapshot tv)
        (dotimes (li (line-count tv))
          (let ((line (nth-line tv li)) (i 0) (changed nil)
                (out (make-string-output-stream)))
            (loop
              (let ((p (%line-find-fwd line from i test whole-word)))
                (if p
                    (progn (write-string (subseq line i p) out) (write-string to out)
                           (setf i (+ p len) changed t) (incf count))
                    (progn (write-string (subseq line i) out) (return)))))
            (when changed (set-line tv li (get-output-stream-string out)))))
        (when (plusp count) (text-update-limit tv) (clamp-cursor tv) (draw-view tv))
        count)))

;;; --- a small, dependency-free regular-expression matcher -------------------
;;; Line-scoped (like the rest of search here).  Supports: literals, . ^ $,
;;; quantifiers * + ?, character classes [...] / [^...] with a-z ranges, and the
;;; escapes \d \w \s (plus \<char> as a literal).  No groups / alternation /
;;; backrefs -- enough for the common editor patterns (^(defun, foo.*bar, [0-9]+).

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
  "SPEC is (NEG . ITEMS); each item is a char or (LO . HI) range."
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
  "End index if ITEMS match LINE starting at I, else NIL (backtracking)."
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
    (loop while (%rx-atom-match atom line j len) do (incf j))   ; consume greedily
    (loop for k from j downto (+ i minrep)                      ; then backtrack
          for r = (%rx-match-items rest line k len)
          when r do (return r))))

(defun %rx-search-line (items line start)
  "First match of ITEMS in LINE at or after START; (values mstart mend) or NIL."
  (loop with len = (length line)
        for s from start to len
        for e = (%rx-match-items items line s len)
        when e do (return (values s e))))

(defun text-find-regex (tv pattern &key from-line from-col)
  "Search forward for regex PATTERN (per line); return (list line start end) or NIL."
  (let ((items (ignore-errors (%rx-parse pattern))))
    (when items
      (let ((fl (or from-line (text-cur-line tv))) (fc (or from-col (text-cur-col tv))))
        (loop for li from fl below (line-count tv)
              for line = (nth-line tv li)
              do (multiple-value-bind (ms me) (%rx-search-line items line (if (= li fl) fc 0))
                   (when ms (return (list li ms me)))))))))

(defun text-select-span (tv line start end)
  "Select LINE[START,END) and leave the cursor at END."
  (setf (text-anchor tv) (cons line start)
        (text-cur-line tv) line
        (text-cur-col tv) end)
  (ensure-visible tv) (draw-view tv))

(defun text-find-and-select-regex (tv pattern &key wrap)
  "Find and select the next regex match; return T on success."
  (let* ((from (text-pos tv))
         (m (or (text-find-regex tv pattern :from-line (car from) :from-col (cdr from))
                (and wrap (text-find-regex tv pattern :from-line 0 :from-col 0)))))
    (when m (text-select-span tv (first m) (second m) (third m)) t)))

(defun text-replace-all-regex (tv pattern to)
  "Replace every regex match of PATTERN with literal TO; return the count."
  (let ((items (ignore-errors (%rx-parse pattern))) (count 0))
    (when items
      (text-snapshot tv)
      (dotimes (li (line-count tv))
        (let* ((line (nth-line tv li)) (len (length line))
               (out (make-string-output-stream)) (i 0) (changed nil))
          (loop
            (multiple-value-bind (ms me) (%rx-search-line items line i)
              (cond
                ((null ms) (write-string (subseq line i) out) (return))
                (t (write-string (subseq line i ms) out) (write-string to out)
                   (incf count) (setf changed t)
                   (cond ((= me ms)                       ; zero-width: keep one char
                          (when (< me len) (write-char (char line me) out))
                          (setf i (1+ me)))
                         (t (setf i me)))
                   (when (> i len) (return))))))
          (when changed (set-line tv li (get-output-stream-string out)))))
      (when (plusp count) (text-update-limit tv) (clamp-cursor tv) (draw-view tv)))
    count))

;;; --- file I/O ---------------------------------------------------------------

(defun directory-pathname-p (path)
  "True when PATH names an existing directory."
  (let ((tn (ignore-errors (truename path))))
    (and tn (null (pathname-name tn)) (null (pathname-type tn)))))

(defun text-load-file (tv path)
  "Replace the buffer with the contents of PATH.  Return T on success, or NIL
if the file is missing, a directory, or unreadable (never errors)."
  (when (and (probe-file path) (not (directory-pathname-p path)))
    (handler-case
        (with-open-file (s path)
          (let ((lines '()))
            (loop for line = (read-line s nil :eof) until (eq line :eof)
                  do (push line lines))
            (set-lines tv (nreverse lines))
            t))
      (error () nil))))

(defun text-save-file (tv path)
  "Write the buffer to PATH and clear the modified flag.  Return PATH."
  (with-open-file (s path :direction :output :if-exists :supersede
                          :if-does-not-exist :create)
    (dotimes (i (line-count tv))
      (write-line (nth-line tv i) s)))
  (setf (text-modified tv) nil)
  path)

;;; ===========================================================================
;;; TIndicator -- a small status gadget (line:col, modified, INS/OVR).
;;; ===========================================================================

(defclass tindicator (tview)
  ((source :initarg :source :initform nil :accessor indicator-source)))

(defmethod initialize-instance :after ((ind tindicator) &key)
  (setf (view-grow-mode ind) (logior +gf-grow-loy+ +gf-grow-hiy+)))

(defmethod get-palette ((ind tindicator)) (make-palette 2 3))  ; active frame colours

(defmethod draw ((ind tindicator))
  (let* ((src (indicator-source ind)) (w (point-x (view-size ind)))
         (c (get-color ind 1)) (db (make-draw-buffer w)))
    (db-fill db #\Space c)
    (when src
      (db-move-str db 0
                   (format nil " ~d:~d~a ~a "
                           (1+ (text-cur-line src)) (1+ (text-cur-col src))
                           (if (text-modified src) " *" "")
                           (if (text-overwrite src) "OVR" "INS"))
                   c))
    (write-line* ind 0 0 w 1 db)))

;;; --- TMemo: a multi-line edit control for use inside dialogs ----------------
;;; TEditor in a window is TFileEditor; TMemo is the same editor engine used as a
;;; bounded dialog control whose get-data/set-data is the whole text string.

(defclass tmemo (ttext-view) ()
  (:documentation "A multi-line editable text control for dialogs (the in-dialog
counterpart of the windowed editor).  Its data is the whole text string."))

;;; --- TFileEditor / TEditWindow: the windowed editor classes ----------------

(defclass tfile-editor (ttext-view)
  ((filename :initarg :filename :initform nil :accessor editor-filename))
  (:documentation "An editor bound to a file (TEditor + a filename)."))

(defmethod initialize-instance :after ((ed tfile-editor) &key)
  (setf (text-highlight ed) t))   ; Lisp syntax colouring on for file editors

(defmethod text-return ((ed tfile-editor))
  "Break the line and auto-indent the new one for Lisp."
  (if (text-highlight ed)
      (let ((indent (%lisp-indent-at (text-string ed) (%cursor-offset ed))))
        (split-line-at-cursor ed)
        (dotimes (_ indent) (insert-char-at-cursor ed #\Space)))
      (call-next-method)))

(defmethod text-tab ((ed tfile-editor))
  "Tab re-indents: the selected lines if there is a selection, else the current
line."
  (if (text-highlight ed)
      (if (text-anchor ed)
          (multiple-value-bind (s e) (selection-range ed)
            (lisp-indent-region ed (car s) (car e)))
          (lisp-indent-line ed (text-cur-line ed)))
      (call-next-method)))

(defclass teditor-window (twindow)
  ((editor :initform nil :accessor editor-window-editor))
  (:documentation "A window framing a TFileEditor with a scroll bar + indicator."))

(defun make-edit-window (bounds &key (title "Editor") filename)
  "Build a TEditWindow: a window containing a TFileEditor, a vertical scroll bar
and a position indicator.  Loads FILENAME if it exists.  Returns (values window
editor)."
  (let* ((w (make-instance 'teditor-window :title title :bounds bounds))
         (iw (point-x (view-size w))) (ih (point-y (view-size w)))
         (vsb (standard-scrollbar w t))
         (ed (make-instance 'tfile-editor :filename filename
                            :bounds (make-trect 1 1 (1- iw) (- ih 2))))
         (ind (make-instance 'tindicator :source ed
                             :bounds (make-trect 2 (1- ih) 18 ih))))
    (insert w ed)
    (insert w ind)
    (text-attach-scrollbars ed :vscroll vsb)
    (when (and filename (probe-file filename))
      (ignore-errors (text-load-file ed filename)))
    (setf (editor-window-editor w) ed)
    (focus ed)
    (values w ed)))
