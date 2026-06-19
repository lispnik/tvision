;;;; htmlview.lisp --- THtmlView: a minimal HTML renderer / hypertext browser.
;;;;
;;;; Renders the simple, CSS/JS-free HTML used by references like the Common
;;;; Lisp HyperSpec: paragraphs, headings, lists, <pre> code, inline emphasis
;;;; and -- the point -- hyperlinks.  It is a TScroller, so it scrolls and binds
;;;; to scroll bars like any other content view.  Tab / Shift-Tab move between
;;;; links, Enter (or a click) follows the focused link by broadcasting
;;;; +cm-html-link+ to the owner, which can read (HTML-CURRENT-HREF view) and
;;;; load the next page.  No CSS, no JavaScript, no tables-as-layout -- just the
;;;; document flow.

(in-package #:tvision)

(defconstant +cm-html-link+ 64)   ; broadcast (info = view) when a link is followed

;;; ---------------------------------------------------------------------------
;;; Entities
;;; ---------------------------------------------------------------------------

(defparameter +html-entities+
  '(("lt" . "<") ("gt" . ">") ("amp" . "&") ("quot" . "\"") ("apos" . "'")
    ("nbsp" . " ") ("mdash" . "--") ("ndash" . "-") ("minus" . "-")
    ("hellip" . "...") ("copy" . "(c)") ("reg" . "(R)") ("trade" . "(tm)")
    ("rarr" . "->") ("larr" . "<-") ("rsquo" . "'") ("lsquo" . "'")
    ("rdquo" . "\"") ("ldquo" . "\"") ("quot" . "\"") ("times" . "x")
    ("middot" . ".") ("bull" . "*") ("deg" . "deg") ("frac12" . "1/2")))

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
             (t (concatenate 'string "&" name ";")))   ; unknown: leave literal
           (1+ semi))))))

(defun %html-ws-p (c) (member c '(#\Space #\Tab #\Newline #\Return #\Page)))

(defun %html-decode-string (s)
  "Return S with HTML entities decoded."
  (with-output-to-string (out)
    (let ((i 0) (n (length s)))
      (loop while (< i n) do
        (let ((c (char s i)))
          (if (char= c #\&)
              (multiple-value-bind (rep next) (%html-decode-entity s i)
                (write-string rep out) (setf i next))
              (progn (write-char c out) (incf i))))))))

(defun %html-collapse-ws (s)
  "Collapse runs of whitespace in S to single spaces."
  (string-trim
   '(#\Space)
   (with-output-to-string (out)
     (let ((sp nil))
       (loop for c across s do
         (if (%html-ws-p c)
             (setf sp t)
             (progn (when sp (write-char #\Space out) (setf sp nil))
                    (write-char c out))))))))

(defun html-document-title (html)
  "The text of HTML's <title> element (entities decoded, whitespace collapsed),
or NIL if there is none."
  (let* ((lo (string-downcase html))
         (open (search "<title" lo)))
    (when open
      (let ((gt (position #\> html :start open)))
        (when gt
          (let ((close (search "</title" lo :start2 (1+ gt))))
            (when close
              (let ((title (%html-collapse-ws
                            (%html-decode-string (subseq html (1+ gt) close)))))
                (when (plusp (length title)) title)))))))))

;;; ---------------------------------------------------------------------------
;;; Parsing HTML into a flat token stream
;;;
;;; Tokens:  :space  :break  :para  :hr
;;;          (:text  STRING STYLE LINK-ID)   -- one whitespace-free word
;;;          (:pre   STRING)                 -- one preformatted line
;;; STYLE is :normal :emph :code or :heading.  LINK-ID indexes the LINKS vector.
;;; ---------------------------------------------------------------------------

(defstruct (html-run (:constructor make-html-run (&key text style link)))
  (text "") (style :normal) (link nil))

(defstruct (html-link (:constructor make-html-link (&key href line)))
  (href "") (line nil))

(defparameter +html-block-tags+
  '("p" "div" "ul" "ol" "dl" "dt" "dd" "blockquote" "table" "tr" "thead"
    "tbody" "center" "form" "fieldset" "caption" "th" "td" "section" "article"
    "nav" "header" "footer" "main" "figure"))

(defun %html-read-tag (s i)
  "S[i] is #\\<.  Return (values name attrs closing next-index); NAME is NIL for
comments / declarations."
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
              ;; text: read to the next tag, decoding entities
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

;;; ---------------------------------------------------------------------------
;;; Layout: tokens + width -> a vector of lines (each a list of HTML-RUNs)
;;; ---------------------------------------------------------------------------

(defun %html-layout (tokens width links)
  (let* ((w (max 4 width))
         (lines (make-array 0 :adjustable t :fill-pointer 0))
         (cur '()) (col 0) (pending nil))
    (loop for lk across links do (setf (html-link-line lk) nil))
    (labels
        ((cur-empty () (and (null cur) (zerop col)))
         (push-line (runs) (vector-push-extend runs lines))
         (finish () (push-line (nreverse cur)) (setf cur '() col 0 pending nil))
         (do-break () (unless (cur-empty) (finish)))
         (do-para ()
           (unless (cur-empty) (finish))
           ;; a blank separator line, but only after real content (never a
           ;; leading blank, never two blanks in a row)
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
           (push-line (list (make-html-run :text (second tok) :style :code))))))
      (unless (cur-empty) (finish))
      lines)))

(defun %html-lines-width (lines)
  (let ((mw 1))
    (loop for ln across lines do
      (setf mw (max mw (reduce #'+ ln :key (lambda (r) (length (html-run-text r)))
                               :initial-value 0))))
    mw))

;;; ---------------------------------------------------------------------------
;;; The control
;;; ---------------------------------------------------------------------------

(defclass thtml-view (tscroller)
  ((source       :initform "" :accessor html-source)
   (tokens       :initform '() :accessor html-tokens)
   (lines        :initform #() :accessor html-lines)
   (links        :initform #() :accessor html-links)
   (focused-link :initform nil :accessor html-focused-link)))

;; index 1 normal, 2 emphasis, 3 heading, 4 code, 5 link, 6 focused link
(defmethod get-palette ((v thtml-view)) (make-palette 1 2 10 3 12 14))

(defun html-link-count (v) (length (html-links v)))

(defun %html-relayout (v)
  (let ((lines (%html-layout (html-tokens v) (point-x (view-size v)) (html-links v))))
    (setf (html-lines v) lines)
    (set-scroller-limit v (%html-lines-width lines) (max 1 (length lines)))))

(defun set-html (v html)
  "Replace the document shown by V with the HTML string HTML."
  (setf (html-source v) (or html ""))
  (multiple-value-bind (toks lks) (html->tokens (html-source v))
    (setf (html-tokens v) toks
          (html-links v) lks
          (html-focused-link v) nil))
  (%html-relayout v)
  (scroll-to v 0 0)
  (draw-view v)
  v)

(defmethod initialize-instance :after ((v thtml-view) &key html)
  (when html (set-html v html)))

(defmethod change-bounds ((v thtml-view) bounds)
  (set-bounds v bounds)
  (%html-relayout v)
  (draw-view v))

(defun %html-run-color (v run)
  (cond ((html-run-link run) (if (eql (html-run-link run) (html-focused-link v)) 6 5))
        (t (case (html-run-style run)
             (:heading 3) (:code 4) (:emph 2) (t 1)))))

(defmethod draw ((v thtml-view))
  (let* ((w (point-x (view-size v))) (h (point-y (view-size v)))
         (dx (point-x (scroller-delta v))) (dy (point-y (scroller-delta v)))
         (normal (get-color v 1))
         (lines (html-lines v))
         (db (make-draw-buffer w)))
    (dotimes (row h)
      (db-fill db #\Space normal)
      (let ((li (+ dy row)))
        (when (< li (length lines))
          (let ((col 0))
            (dolist (run (aref lines li))
              (let ((attr (get-color v (%html-run-color v run))))
                (loop for ch across (html-run-text run) do
                  (let ((sx (- col dx)))
                    (when (and (>= sx 0) (< sx w)) (db-put-char db sx ch attr)))
                  (incf col)))))))
      (write-line* v 0 row w 1 db))))

;;; --- link navigation -------------------------------------------------------

(defun %html-scroll-line-into-view (v ln)
  (let ((top (point-y (scroller-delta v)))
        (h (point-y (view-size v)))
        (dx (point-x (scroller-delta v))))
    (cond ((< ln top) (scroll-to v dx ln))
          ((>= ln (+ top h)) (scroll-to v dx (max 0 (1+ (- ln h))))))))

(defun html-focus-link (v id)
  "Make link ID the focused link, scrolling it into view."
  (when (and id (>= id 0) (< id (html-link-count v)))
    (setf (html-focused-link v) id)
    (let ((ln (html-link-line (aref (html-links v) id))))
      (when ln (%html-scroll-line-into-view v ln)))
    (draw-view v)
    (message (view-owner v) +ev-broadcast+ +cm-list-focus-changed+ v)))

(defun html-next-link (v &optional (dir 1))
  "Move the focus to the next (DIR 1) or previous (DIR -1) link, wrapping."
  (let ((n (html-link-count v)))
    (when (plusp n)
      (let ((cur (html-focused-link v)))
        (html-focus-link v (if cur
                               (mod (+ cur dir) n)
                               (if (plusp dir) 0 (1- n))))))))

(defun html-current-href (v)
  "The href of the focused link, or NIL."
  (let ((id (html-focused-link v)))
    (when (and id (< id (html-link-count v)))
      (html-link-href (aref (html-links v) id)))))

(defun html-activate-link (v)
  "Follow the focused link: broadcast +cm-html-link+ to the owner."
  (when (html-current-href v)
    (message (view-owner v) +ev-broadcast+ +cm-html-link+ v)))

(defun %html-link-at (v line cx)
  "The link id covering virtual column CX on LINE, or NIL."
  (when (and (>= line 0) (< line (length (html-lines v))))
    (let ((col 0))
      (dolist (run (aref (html-lines v) line) nil)
        (let ((len (length (html-run-text run))))
          (when (and (html-run-link run) (>= cx col) (< cx (+ col len)))
            (return-from %html-link-at (html-run-link run)))
          (incf col len))))))

(defmethod handle-event ((v thtml-view) event)
  (cond
    ((and (= (event-type event) +ev-key-down+) (logtest (view-state v) +sf-focused+))
     (let ((k (event-key-code event)))
       (cond
         ((= k +kb-tab+)       (html-next-link v 1)  (clear-event event))
         ((= k +kb-shift-tab+) (html-next-link v -1) (clear-event event))
         ((= k +kb-enter+)     (html-activate-link v) (clear-event event))
         (t (call-next-method)))))
    ((and (= (event-type event) +ev-mouse-down+) (mouse-in-view-p v event))
     (let* ((lp (make-local v (event-mouse-where event)))
            (li (+ (point-y (scroller-delta v)) (point-y lp)))
            (cx (+ (point-x (scroller-delta v)) (point-x lp)))
            (id (%html-link-at v li cx)))
       (when id
         (html-focus-link v id)
         (html-activate-link v)))
     (clear-event event))
    (t (call-next-method))))
