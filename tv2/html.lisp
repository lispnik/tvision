;;;; html.lisp --- a real tvlisp window (the HTML browser) ported onto tv2.
;;;;
;;;; tvlisp's THTML-VIEW renders HTML in the terminal: a tokenizer turns markup
;;;; into a flat token stream, a layout pass wraps it into a vector of styled
;;;; "runs" (heading / emphasis / code / link) plus a links table and named
;;;; anchors, and the view paints + navigates that.  The tokenizer and layout are
;;;; pure functions, ported here essentially verbatim; only the view/dispatch
;;;; layer is rebuilt from tv2 parts.  (Deferred: find-in-page, mouse, regex.)

(in-package #:tv2)

;;; ===========================================================================
;;; Entity decoding + helpers (pure)
;;; ===========================================================================

(defparameter +html-entities+
  '(("lt" . "<") ("gt" . ">") ("amp" . "&") ("quot" . "\"") ("apos" . "'")
    ("nbsp" . " ") ("ensp" . " ") ("emsp" . " ") ("thinsp" . " ") ("shy" . "")
    ("mdash" . "--") ("ndash" . "-") ("minus" . "-") ("dash" . "-")
    ("hellip" . "...") ("copy" . "(c)") ("reg" . "(R)") ("trade" . "(tm)")
    ("rarr" . "->") ("larr" . "<-") ("harr" . "<->") ("uarr" . "^") ("darr" . "v")
    ("rArr" . "=>") ("lArr" . "<=") ("rsquo" . "'") ("lsquo" . "'") ("sbquo" . ",")
    ("rdquo" . "\"") ("ldquo" . "\"") ("bdquo" . "\"") ("times" . "x") ("divide" . "/")
    ("middot" . ".") ("bull" . "*") ("deg" . "deg") ("frac12" . "1/2")
    ("frac14" . "1/4") ("frac34" . "3/4") ("plusmn" . "+/-") ("micro" . "u")
    ("sect" . "S") ("para" . "P") ("dagger" . "+") ("Dagger" . "++") ("permil" . "0/00")
    ("le" . "<=") ("ge" . ">=") ("ne" . "/=") ("equiv" . "==") ("infin" . "inf")
    ("alpha" . "alpha") ("beta" . "beta") ("lambda" . "lambda") ("pi" . "pi")
    ("hearts" . "<3") ("check" . "v") ("cross" . "x") ("prime" . "'") ("Prime" . "\"")
    ("laquo" . "<<") ("raquo" . ">>") ("euro" . "EUR") ("pound" . "GBP") ("cent" . "c")))

(defun %html-decode-entity (s i)
  "S[i] is #\\&.  Return (values replacement next-index)."
  (let* ((n (length s))
         (semi (position #\; s :start (1+ i) :end (min n (+ i 12)))))
    (if (null semi)
        (values "&" (1+ i))
        (let ((name (subseq s (1+ i) semi)))
          (values
           (cond
             ((and (plusp (length name)) (char= (char name 0) #\#))
              (let* ((hex (and (> (length name) 1) (member (char name 1) '(#\x #\X))))
                     (num (ignore-errors
                           (parse-integer name :start (if hex 2 1) :radix (if hex 16 10)))))
                (cond ((null num) " ")
                      ((= num 10) (string #\Newline))
                      ((and (>= num 32) (< num char-code-limit)) (string (code-char num)))
                      (t " "))))
             ((cdr (assoc name +html-entities+ :test #'string-equal)))
             (t (concatenate 'string "&" name ";")))
           (1+ semi))))))

(defun %html-ws-p (c) (member c '(#\Space #\Tab #\Newline #\Return #\Page)))

;;; ===========================================================================
;;; Tokenizer (pure): HTML -> flat token stream + links vector
;;;   :space :break :para :hr  (:text STR STYLE LINK) (:pre STR) (:anchor NAME)
;;; ===========================================================================

(defstruct (html-run (:constructor make-html-run (&key text style link)))
  (text "") (style :normal) (link nil))
(defstruct (html-link (:constructor make-html-link (&key href line)))
  (href "") (line nil))

(defparameter +html-block-tags+
  '("p" "div" "ul" "ol" "dl" "dt" "dd" "blockquote" "table" "tr" "thead"
    "tbody" "center" "form" "fieldset" "caption" "th" "td" "section" "article"
    "nav" "header" "footer" "main" "figure"))

(defun %html-read-tag (s i)
  "S[i] is #\\<.  Return (values name attrs closing next-index)."
  (let ((n (length s)))
    (cond
      ((and (<= (+ i 4) n) (string= s "<!--" :start1 i :end1 (+ i 4)))
       (let ((end (search "-->" s :start2 (+ i 4))))
         (values nil nil nil (if end (+ end 3) n))))
      ((and (< (1+ i) n) (char= (char s (1+ i)) #\!))
       (let ((end (position #\> s :start i)))
         (values nil nil nil (if end (1+ end) n))))
      (t
       (let ((j (1+ i)) (closing nil) (attrs '()))
         (when (and (< j n) (char= (char s j) #\/)) (setf closing t) (incf j))
         (let ((start j))
           (loop while (and (< j n)
                            (let ((c (char s j))) (or (alphanumericp c) (char= c #\-))))
                 do (incf j))
           (let ((name (string-downcase (subseq s start j))))
             (loop
               (loop while (and (< j n) (%html-ws-p (char s j))) do (incf j))
               (cond
                 ((>= j n) (return))
                 ((char= (char s j) #\>) (incf j) (return))
                 ((char= (char s j) #\/) (incf j))
                 (t
                  (let ((as j))
                    (loop while (and (< j n)
                                     (not (member (char s j) '(#\Space #\Tab #\Newline
                                                               #\Return #\= #\> #\/))))
                          do (incf j))
                    (let ((aname (string-downcase (subseq s as j))) (aval ""))
                      (loop while (and (< j n) (%html-ws-p (char s j))) do (incf j))
                      (when (and (< j n) (char= (char s j) #\=))
                        (incf j)
                        (loop while (and (< j n) (%html-ws-p (char s j))) do (incf j))
                        (cond
                          ((and (< j n) (member (char s j) '(#\" #\')))
                           (let ((q (char s j)))
                             (incf j)
                             (let ((vs j))
                               (loop while (and (< j n) (char/= (char s j) q)) do (incf j))
                               (setf aval (subseq s vs (min j n)))
                               (when (< j n) (incf j)))))
                          (t (let ((vs j))
                               (loop while (and (< j n)
                                                (not (member (char s j) '(#\Space #\Tab #\Newline
                                                                          #\Return #\>))))
                                     do (incf j))
                               (setf aval (subseq s vs j))))))
                      (push (cons aname aval) attrs))))))
             (values name (nreverse attrs) closing j))))))))

(defun html->tokens (html)
  "Parse HTML into (values token-list links-vector)."
  (let ((tokens '())
        (links (make-array 0 :adjustable t :fill-pointer 0))
        (i 0) (n (length html))
        (bold 0) (emph 0) (code 0) (heading 0) (cur-link nil)
        (skip 0) (pre 0)
        (text (make-string-output-stream)))
    (labels
        ((style ()
           (cond ((plusp heading) :heading)
                 ((plusp code) :code)
                 ((or (plusp bold) (plusp emph)) :emph)
                 (t :normal)))
         (emit (tok) (push tok tokens))
         (flush-normal ()
           (let ((str (get-output-stream-string text)))
             (when (plusp (length str))
               (let ((j 0) (m (length str)) (st (style)) (lk cur-link))
                 (loop while (< j m) do
                   (if (%html-ws-p (char str j))
                       (progn (emit :space)
                              (loop while (and (< j m) (%html-ws-p (char str j))) do (incf j)))
                       (let ((start j))
                         (loop while (and (< j m) (not (%html-ws-p (char str j)))) do (incf j))
                         (emit (list :text (subseq str start j) st lk)))))))))
         (flush-pre ()
           (let ((str (get-output-stream-string text)) (start 0))
             (dotimes (k (length str))
               (when (char= (char str k) #\Newline)
                 (emit (list :pre (subseq str start k)))
                 (setf start (1+ k))))
             (emit (list :pre (subseq str start)))))
         (handle-tag (name attrs closing)
           (when (and (not closing) (zerop skip))
             (let ((id (or (cdr (assoc "id" attrs :test #'string-equal))
                           (and (string= name "a")
                                (cdr (assoc "name" attrs :test #'string-equal))))))
               (when (and id (plusp (length id)))
                 (emit (list :anchor (string-downcase id))))))
           (cond
             ((member name '("script" "style" "head" "title") :test #'string=)
              (if closing (when (plusp skip) (decf skip)) (incf skip)))
             ((plusp skip) nil)
             ((string= name "pre")
              (if closing (progn (flush-pre) (setf pre 0) (emit :break))
                  (progn (emit :break) (setf pre 1))))
             ((plusp pre) nil)
             ((member name '("b" "strong") :test #'string=)
              (if closing (when (plusp bold) (decf bold)) (incf bold)))
             ((member name '("i" "em" "cite" "dfn" "address" "u") :test #'string=)
              (if closing (when (plusp emph) (decf emph)) (incf emph)))
             ((member name '("tt" "code" "kbd" "samp" "var") :test #'string=)
              (if closing (when (plusp code) (decf code)) (incf code)))
             ((member name '("h1" "h2" "h3" "h4" "h5" "h6") :test #'string=)
              (emit :para)
              (if closing (when (plusp heading) (decf heading)) (incf heading)))
             ((string= name "a")
              (if closing
                  (setf cur-link nil)
                  (let ((href (cdr (assoc "href" attrs :test #'string-equal))))
                    (if (and href (plusp (length href)))
                        (progn (vector-push-extend (make-html-link :href href) links)
                               (setf cur-link (1- (fill-pointer links))))
                        (setf cur-link nil)))))
             ((string= name "br") (emit :break))
             ((string= name "hr") (emit :hr))
             ((string= name "p") (emit :para))
             ((string= name "li")
              (if closing
                  (emit :break)
                  (progn (emit :break)
                         (emit (list :text (string (code-char #x2022)) :normal nil))
                         (emit :space))))
             ((member name +html-block-tags+ :test #'string=) (emit :break))
             (t nil))))
      (loop while (< i n) do
        (let ((c (char html i)))
          (if (char= c #\<)
              (progn
                (when (zerop pre) (flush-normal))
                (multiple-value-bind (name attrs closing next) (%html-read-tag html i)
                  (setf i next)
                  (when name (handle-tag name attrs closing))))
              (let ((out text))
                (loop while (and (< i n) (char/= (char html i) #\<)) do
                  (let ((ch (char html i)))
                    (if (char= ch #\&)
                        (multiple-value-bind (rep next) (%html-decode-entity html i)
                          (when (zerop skip) (write-string rep out))
                          (setf i next))
                        (progn (when (zerop skip) (write-char ch out))
                               (incf i)))))))))
      (if (plusp pre) (flush-pre) (flush-normal))
      (values (nreverse tokens) links))))

;;; ===========================================================================
;;; Layout (pure): tokens + width -> vector of lines (lists of HTML-RUNs)
;;; ===========================================================================

(defun %html-layout (tokens width links)
  "Lay TOKENS out to WIDTH; return (values lines anchors)."
  (let* ((w (max 4 width))
         (lines (make-array 0 :adjustable t :fill-pointer 0))
         (anchors '())
         (cur '()) (col 0) (pending nil))
    (loop for lk across links do (setf (html-link-line lk) nil))
    (labels
        ((cur-empty () (and (null cur) (zerop col)))
         (push-line (runs) (vector-push-extend runs lines))
         (finish () (push-line (nreverse cur)) (setf cur '() col 0 pending nil))
         (do-break () (unless (cur-empty) (finish)))
         (do-para ()
           (unless (cur-empty) (finish))
           (when (and (plusp (fill-pointer lines))
                      (aref lines (1- (fill-pointer lines))))
             (push-line '())))
         (add (str style link)
           (let ((sp (if (and pending (plusp col)) 1 0)))
             (when (and (plusp col) (> (+ col sp (length str)) w))
               (finish) (setf sp 0))
             (when (= sp 1)
               (let ((lr (first cur)))
                 (when lr (setf (html-run-text lr) (concatenate 'string (html-run-text lr) " "))))
               (incf col))
             (setf pending nil)
             (let ((lr (first cur)))
               (if (and lr (eq (html-run-style lr) style) (eql (html-run-link lr) link))
                   (setf (html-run-text lr) (concatenate 'string (html-run-text lr) str))
                   (push (make-html-run :text str :style style :link link) cur)))
             (incf col (length str))
             (when (and link (< link (length links)) (null (html-link-line (aref links link))))
               (setf (html-link-line (aref links link)) (fill-pointer lines))))))
      (dolist (tok tokens)
        (cond
          ((eq tok :space) (setf pending t))
          ((eq tok :break) (do-break))
          ((eq tok :para)  (do-para))
          ((eq tok :hr)
           (do-break)
           (push-line (list (make-html-run :text (make-string (max 1 (1- w))
                                                              :initial-element #\─)
                                            :style :normal))))
          ((and (consp tok) (eq (car tok) :text))
           (destructuring-bind (str style link) (cdr tok) (add str style link)))
          ((and (consp tok) (eq (car tok) :pre))
           (do-break)
           (push-line (list (make-html-run :text (second tok) :style :code))))
          ((and (consp tok) (eq (car tok) :anchor))
           (push (cons (second tok) (fill-pointer lines)) anchors))))
      (unless (cur-empty) (finish))
      (values lines (nreverse anchors)))))

;;; ===========================================================================
;;; The tv2 html-view widget
;;; ===========================================================================

(defclass html-view (view)
  ((tokens  :initform '() :accessor hv-tokens)
   (lines   :initform #() :accessor hv-lines)
   (anchors :initform '() :accessor hv-anchors)        ; (name . line) alist
   (links   :initform #() :accessor hv-links)
   (focus   :initform nil :accessor hv-focus)          ; focused link id, or NIL
   (top     :initform 0   :accessor hv-top)            ; first visible line
   (matches :initform '() :accessor hv-matches)        ; find-in-page hits: (line start end)
   (match-i :initform nil :accessor hv-match-i)        ; current match index, or NIL
   (on-link :initform nil :initarg :on-link :accessor hv-on-link)     ; (lambda (href) ...)
   (on-status :initform nil :accessor hv-on-status))   ; (lambda (string) ...) -> update a status line
  (:metaclass reactive-class))

(defmethod focusable-p ((v html-view)) t)
(defun hv-nlines (v) (length (hv-lines v)))
(defun hv-nlinks (v) (length (hv-links v)))

(defun hv-relayout (v)
  (let ((w (if (view-bounds v) (r-w (view-bounds v)) 76)))
    (multiple-value-bind (lines anchors) (%html-layout (hv-tokens v) w (hv-links v))
      (setf (hv-lines v) lines (hv-anchors v) anchors))))

(defun set-html (v html)
  (multiple-value-bind (toks lks) (html->tokens (or html ""))
    (setf (hv-tokens v) toks (hv-links v) lks (hv-focus v) nil (hv-top v) 0
          (hv-matches v) '() (hv-match-i v) nil))
  (hv-relayout v)
  (invalidate v)
  v)

;;; --- find-in-page -----------------------------------------------------------

(defun hv-line-text (v li)
  (with-output-to-string (s)
    (when (< li (hv-nlines v)) (dolist (r (aref (hv-lines v) li)) (write-string (html-run-text r) s)))))

(defun hv-status (v fmt &rest args)
  (when (hv-on-status v) (funcall (hv-on-status v) (apply #'format nil fmt args))))

(defun hv-find (v query)
  "Find QUERY (case-insensitive) across the rendered lines, recording every match
and jumping to the first.  Match columns are virtual columns = on-screen columns
(the document is wrapped to width, never horizontally scrolled)."
  (let ((q (string-downcase (or query ""))) (ms '()))
    (when (plusp (length q))
      (dotimes (li (hv-nlines v))
        (let ((line (string-downcase (hv-line-text v li))) (start 0))
          (loop for pos = (search q line :start2 start) while pos do
            (push (list li pos (+ pos (length q))) ms)
            (setf start (+ pos (length q)))))))
    (setf (hv-matches v) (nreverse ms) (hv-match-i v) (if (hv-matches v) 0 nil))
    (when (hv-match-i v) (hv-line-into-view v (first (first (hv-matches v)))))
    (invalidate v)
    (if (plusp (length q))
        (if (hv-matches v) (hv-status v " find ~s: 1/~d matches (</> to cycle) " query (length (hv-matches v)))
            (hv-status v " find ~s: no matches " query))
        (hv-status v " find cancelled "))
    (length (hv-matches v))))

(defun hv-find-next (v dir)
  (let ((n (length (hv-matches v))))
    (when (plusp n)
      (setf (hv-match-i v) (mod (+ (or (hv-match-i v) 0) dir) n))
      (hv-line-into-view v (first (nth (hv-match-i v) (hv-matches v))))
      (invalidate v)
      (hv-status v " match ~d/~d " (1+ (hv-match-i v)) n))))

(defun hv-prompt-find (v)
  "Modal find prompt; on Enter, run the search."
  (let ((d (ui (dialog (:title " Find in page " :keymap *dialog-keys*
                         :value-fn (lambda (d) (input-text (find-view d 'q))))
                 (stack
                   (1 (row (8 (static-text :role :label :text " Find: "))
                           (:fill (input-line :name 'q))))
                   (1 (static-text :role :status :text " Enter: search · Esc: cancel ")))))))
    (let ((r (exec-view d :width 52 :height 6)))
      (unless (eq r :cancel) (hv-find v r)))))

(defun hv-scroll (v delta)
  (let* ((h (r-h (view-bounds v))) (maxtop (max 0 (- (hv-nlines v) h))))
    (setf (hv-top v) (max 0 (min maxtop (+ (hv-top v) delta))))
    (invalidate v)))

(defun hv-line-into-view (v ln)
  (when (view-bounds v)                          ; no-op before the view is laid out
    (let ((h (r-h (view-bounds v))) (top (hv-top v)))
      (cond ((< ln top) (setf (hv-top v) ln))
            ((>= ln (+ top h)) (setf (hv-top v) (max 0 (1+ (- ln h)))))))))

(defun hv-focus-link (v id)
  (when (and id (>= id 0) (< id (hv-nlinks v)))
    (setf (hv-focus v) id)
    (let ((ln (html-link-line (aref (hv-links v) id))))
      (when ln (hv-line-into-view v ln)))
    (invalidate v)))

(defun hv-next-link (v dir)
  (let ((n (hv-nlinks v)))
    (when (plusp n)
      (let ((cur (hv-focus v)))
        (hv-focus-link v (if cur (mod (+ cur dir) n) (if (plusp dir) 0 (1- n))))))))

(defun hv-goto-anchor (v name)
  (let ((hit (assoc (string-downcase name) (hv-anchors v) :test #'string=)))
    (when hit (setf (hv-top v) (min (cdr hit) (max 0 (1- (hv-nlines v))))) (invalidate v) t)))

(defun hv-activate (v)
  "Follow the focused link: in-document #anchor scrolls; otherwise call ON-LINK."
  (let ((id (hv-focus v)))
    (when (and id (< id (hv-nlinks v)))
      (let ((href (html-link-href (aref (hv-links v) id))))
        (cond ((and (plusp (length href)) (char= (char href 0) #\#))
               (hv-goto-anchor v (subseq href 1)))
              ((hv-on-link v) (funcall (hv-on-link v) href)))))))

(defun %html-run-attr (v run)
  (if (html-run-link run)
      (if (eql (html-run-link run) (hv-focus v))
          (role :focused)                       ; focused link: highlighted
          (tvision:make-attr 13 1))             ; link: light magenta on blue
      (case (html-run-style run)
        (:heading (tvision:make-attr 14 1))     ; heading: yellow
        (:code    (tvision:make-attr 10 1))     ; code: light green
        (:emph    (tvision:make-attr 11 1))     ; emphasis: light cyan
        (t        (role :normal)))))

(defmethod draw ((v html-view))
  (let* ((b (view-bounds v)) (h (r-h b)) (w (r-w b)) (top (hv-top v))
         (lines (hv-lines v)) (norm (role :normal)))
    (dotimes (row h)
      (fill-row v 0 row w norm)
      (let ((li (+ top row)))
        (when (< li (length lines))
          (let ((col 0))
            (dolist (run (aref lines li))
              (when (< col w) (draw-text v col row (html-run-text run) (%html-run-attr v run)))
              (incf col (length (html-run-text run)))))
          ;; overlay find-in-page matches (current match brighter)
          (when (hv-matches v)
            (let ((lt (hv-line-text v li)))
              (loop for m in (hv-matches v) for mi from 0
                    when (= (first m) li) do
                      (loop for c from (second m) below (min w (third m))
                            when (< c (length lt)) do
                              (%put-cell (+ (tvision::rect-ax b) c) (+ (tvision::rect-ay b) row)
                                         (char lt c)
                                         (if (eql mi (hv-match-i v))
                                             (tvision:make-attr 15 4)    ; current: white on red
                                             (tvision:make-attr 0 6))))))))))))  ; others: black on cyan

(defun hv-link-at (v li col)
  "The link id at visual (LI,COL), or NIL."
  (when (and (>= li 0) (< li (hv-nlines v)))
    (let ((c 0))
      (dolist (run (aref (hv-lines v) li))
        (let ((len (length (html-run-text run))))
          (when (and (html-run-link run) (>= col c) (< col (+ c len)))
            (return-from hv-link-at (html-run-link run)))
          (incf c len))))))

(defmethod handle-event ((v html-view) (e mouse-down))
  (let ((id (hv-link-at v (+ (hv-top v) (mouse-row v e)) (mouse-col v e))))
    (when id (setf (hv-focus v) id) (invalidate v) (hv-activate v)))
  (setf (handled-p e) t))

(defmethod handle-event ((v html-view) (e wheel-event))
  (hv-scroll v (* 3 (event-delta e))) (setf (handled-p e) t))

(defmethod handle-event ((v html-view) (e key-event))
  (let ((ks (event-keysym e)) (page (max 1 (1- (r-h (view-bounds v))))))
    (cond
      ((eql ks :up)    (hv-scroll v -1)     (setf (handled-p e) t))
      ((eql ks :down)  (hv-scroll v 1)      (setf (handled-p e) t))
      ((eql ks :pgup)  (hv-scroll v (- page))(setf (handled-p e) t))
      ((eql ks :pgdn)  (hv-scroll v page)   (setf (handled-p e) t))
      ((eql ks :home)  (setf (hv-top v) 0) (invalidate v) (setf (handled-p e) t))
      ((eql ks :end)   (setf (hv-top v) (max 0 (- (hv-nlines v) (r-h (view-bounds v))))) (invalidate v) (setf (handled-p e) t))
      ((eql ks #\n)    (hv-next-link v 1)   (setf (handled-p e) t))   ; next link
      ((eql ks #\p)    (hv-next-link v -1)  (setf (handled-p e) t))   ; prev link
      ((eql ks :enter) (hv-activate v)      (setf (handled-p e) t))   ; follow link
      ((eql ks #\/)    (hv-prompt-find v)   (setf (handled-p e) t))   ; find in page
      ((eql ks #\>)    (hv-find-next v 1)   (setf (handled-p e) t))   ; next match
      ((eql ks #\<)    (hv-find-next v -1)  (setf (handled-p e) t))   ; prev match
      (t (call-next-method)))))                                       ; q / Esc bubble

;;; --- entry point: a small multi-page demo "site" ----------------------------

(defparameter *html-demo-pages*
  '(("index" .
"<html><head><title>tv2 HTML browser</title></head><body>
<h1>tv2 &mdash; HTML browser</h1>
<p>This is the <b>real tvlisp HTML view</b>, ported onto the tv2 kernel.  The
tokenizer and layout engine are reused <em>verbatim</em>; only the widget and
event dispatch are new.</p>
<h2>What it renders</h2>
<ul>
<li><b>Bold</b>, <em>emphasis</em>, and <code>inline code</code> styles</li>
<li>Headings, paragraphs, lists, and <code>&lt;pre&gt;</code> blocks</li>
<li>HTML entities: &copy; &mdash; &rarr; &le; &amp; &hearts;</li>
<li>Hyperlinks you can focus and follow</li>
</ul>
<pre>(defun fib (n)
  (if (&lt; n 2) n
      (+ (fib (- n 1)) (fib (- n 2)))))</pre>
<h2>Navigation</h2>
<p>Press <code>n</code>/<code>p</code> to move between links, <code>Enter</code>
to follow one, arrows / PgUp / PgDn to scroll.  Try these:</p>
<p>&rarr; <a href=\"about\">About this port</a><br>
&rarr; <a href=\"#bottom\">Jump to the bottom anchor</a><br>
&rarr; <a href=\"https://github.com/lispnik/tvlisp\">An external link (echoed below)</a></p>
<hr>
<p>Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod
tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam,
quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo.</p>
<p id=\"bottom\"><b>You reached the bottom anchor.</b> <a href=\"index\">Back to top link</a>.</p>
</body></html>")
    ("about" .
"<html><body><h1>About</h1>
<p>The HTML browser is the last of tvlisp's &ldquo;engine&rdquo; windows to be
ported onto tv2 &mdash; after the thread monitor, project manager, browser,
REPL and editor.</p>
<p><a href=\"index\">&larr; Back to the index</a></p></body></html>")))

(defun make-html (&optional (page "index"))
  "Build an HTML-browser window over the built-in demo site.  Return (values
WINDOW FOCUS)."
  (let* ((win (ui (window (:title " tv2 — HTML browser (a real tvlisp window, ported) " :keymap *global-keys*)
                    (stack
                      (:fill (html-view :name 'doc))
                      (1 (static-text :name 'status :role :status :text ""))))))
         (doc (find-view win 'doc)) (status (find-view win 'status)))
    (labels ((echo (msg) (setf (static-text-text status) msg) (invalidate status))
             (show (name)
               (let ((html (cdr (assoc name *html-demo-pages* :test #'string=))))
                 (if html
                     (progn (set-html doc html) (hv-next-link doc 1)
                            (echo (format nil " page: ~a   ~d link~:p   n/p: links · Enter: follow · /: find · arrows: scroll · Esc: close "
                                          name (hv-nlinks doc))))
                     (echo (format nil " external link: ~a   (a real browser would navigate here) " name))))))
      (setf (hv-on-link doc) (lambda (href) (show href))   ; SHOW renders a known page or echoes an external href
            (hv-on-status doc) #'echo)
      (setf (window-scroll-target win) doc (window-help win) :html)
      ;; render AFTER the window is laid out (OPEN runs post-layout), so the
      ;; document wraps to the real width and link-scrolling has bounds
      (values win doc (lambda (s) (declare (ignore s)) (show page) nil)))))

(defun run-html (&optional (page "index"))
  "Run the ported HTML browser full-screen until Esc."
  (multiple-value-bind (w f o) (make-html page) (run-view w :focus f :open o)))

;;; --- a fetch-capable document browser (HyperSpec / manuals) -----------------
;;; When *URL-FETCH-FN* is bound (tvlisp-tv2 -> curl), the HTML browser becomes a
;;; real one: link clicks fetch and render the target page.

(defvar *url-fetch-fn* nil "(url) -> HTML string, or NIL.")

(defun %collapse-dots (url)
  "Collapse ./ and ../ segments in URL's path."
  (let ((p (search "://" url)))
    (if (null p) url
        (let* ((rest (+ p 3)) (slash (position #\/ url :start rest))
               (prefix (if slash (subseq url 0 slash) url))
               (path (if slash (subseq url slash) "/")) (segs '()))
          (dolist (s (uiop:split-string path :separator "/"))
            (cond ((string= s "..") (when segs (pop segs)))
                  ((or (string= s ".") (string= s "")))
                  (t (push s segs))))
          (concatenate 'string prefix "/" (format nil "~{~a~^/~}" (reverse segs)))))))

(defun %resolve-url (base href)
  "Resolve HREF (possibly relative) against the BASE page URL."
  (cond ((or (null href) (zerop (length href))) base)
        ((search "://" href) href)
        ((null base) href)
        ((char= (char href 0) #\#) base)                      ; same-page anchor
        (t (let* ((q (or (position #\# href) (length href)))  ; drop any #anchor
                  (href (subseq href 0 q))
                  (cut (position #\/ base :from-end t))
                  (dir (if cut (subseq base 0 (1+ cut)) (concatenate 'string base "/"))))
             (%collapse-dots (concatenate 'string dir href))))))

(defun make-doc-browser (title html &optional base-url)
  "An HTML browser over arbitrary HTML; with *URL-FETCH-FN* set, clicking a link
fetches and renders the target.  Return (values WINDOW FOCUS OPEN)."
  (let* ((cur (list base-url))
         (win (ui (window (:title title :keymap *global-keys*)
                    (stack (:fill (html-view :name 'doc))
                           (1 (static-text :name 'status :role :status :text ""))))))
         (doc (find-view win 'doc)) (status (find-view win 'status)))
    (labels ((echo (m) (setf (static-text-text status) m) (invalidate status))
             (render (h url)
               (setf (first cur) url) (set-html doc h) (hv-next-link doc 1)
               (echo (format nil " ~a   ~d link~:p · n/p · Enter: follow · /: find · Esc: close "
                             (or url "") (hv-nlinks doc)))))
      (setf (hv-on-link doc)
            (lambda (href)
              (let ((target (%resolve-url (first cur) href)))
                (if (and *url-fetch-fn* target)
                    (let ((h (funcall *url-fetch-fn* target)))
                      (if h (render h target) (echo (format nil " could not fetch ~a " target))))
                    (echo (format nil " link: ~a " href)))))
            (hv-on-status doc) #'echo)
      (setf (window-scroll-target win) doc (window-help win) :html)
      (values win doc (lambda (s) (declare (ignore s)) (render html base-url) nil)))))
