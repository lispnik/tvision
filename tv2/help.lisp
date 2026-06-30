;;;; help.lisp --- a context-sensitive help system, rendered with the HTML view.
;;;;
;;;; Help topics are HTML strings; MAKE-HELP shows one in an html-view window
;;;; (so headings / lists / cross-topic links all render), and topics link to
;;;; each other via plain hrefs resolved against *HELP-PAGES*.  The desktop opens
;;;; help for the focused window's topic (its WINDOW-HELP) on F1 or the Help menu.

(in-package #:tv2)

(defparameter *help-pages*
  (list
   (cons :general
"<h1>tv2 — Help</h1>
<p>This is the <b>tv2</b> IDE: a Turbo-Vision-style desktop running on a
CLOS-native re-architecture of the framework.</p>
<h2>Getting around</h2>
<ul>
<li>The <b>menu bar</b> (top) opens windows and commands; click a title or use the arrows.</li>
<li>The <b>status bar</b> (bottom) shows clickable actions for the current context.</li>
<li>Windows are <b>movable</b> (drag the title), <b>resizable</b> (drag the <code>&#9698;</code> grip)
and <b>closable</b> (<code>[x]</code>).  The <b>Window</b> menu tiles and cascades them.</li>
<li>The <b>mouse</b> works throughout; the <b>wheel</b> scrolls; <code>Esc</code> closes a window.</li>
</ul>
<h2>The windows</h2>
<p>&#8594; <a href=\"repl\">Lisp REPL</a>, <a href=\"editor\">Text editor</a>,
<a href=\"project\">Project manager</a>, <a href=\"browser\">Browsers</a>,
<a href=\"html\">HTML browser</a>, <a href=\"threads\">Thread monitor</a>.</p>")
   (cons :repl
"<h1>Lisp REPL</h1>
<p>A SLIME-style listener: forms are read and evaluated on a <b>worker thread</b>,
so the UI stays live and output <em>streams</em> into the transcript.</p>
<ul>
<li><code>Enter</code> evaluates; <code>&#8593;</code>/<code>&#8595;</code> recall history.</li>
<li>An error opens the <b>debugger</b>: pick a restart, browse the backtrace and its locals,
or return a value from a frame.</li>
<li>The CL history vars (<code>* ** *** + ++ /</code>) and a sticky <code>in-package</code> are maintained.</li>
</ul>
<p><a href=\"general\">&#8592; Contents</a></p>")
   (cons :editor
"<h1>Text editor</h1>
<p>A multi-line editor with selection, an internal clipboard and undo/redo.</p>
<ul>
<li><b>Shift</b>+arrows select; <code>C-c</code>/<code>C-x</code>/<code>C-v</code> copy/cut/paste.</li>
<li><code>C-z</code>/<code>C-y</code> undo/redo; <code>C-s</code> saves; <code>C-w</code> toggles word-wrap.</li>
<li>Lisp buffers are <b>syntax-highlighted</b>; click to place the caret.</li>
</ul>
<p><a href=\"general\">&#8592; Contents</a></p>")
   (cons :project
"<h1>Project manager</h1>
<p>A <code>git ls-files</code> tree of the project; directories load lazily.</p>
<ul>
<li><code>Right</code>/<code>Enter</code> expand a directory; type in <b>Filter</b> to flatten to matches.</li>
<li>Use <b>File &#8594; Change dir…</b> to point new project windows elsewhere.</li>
</ul>
<p><a href=\"general\">&#8592; Contents</a></p>")
   (cons :browser
"<h1>Browsers</h1>
<p>A filterable list (Packages, ASDF systems, …): type to narrow, <code>Enter</code> to act;
the detail line describes the selection.</p>
<p><a href=\"general\">&#8592; Contents</a></p>")
   (cons :html
"<h1>HTML browser</h1>
<p>Renders HTML in the terminal (headings, lists, code, links).</p>
<ul>
<li><code>n</code>/<code>p</code> move between links, <code>Enter</code> follows one.</li>
<li><code>/</code> finds in the page; <code>&lt;</code>/<code>&gt;</code> cycle matches.</li>
</ul>
<p><a href=\"general\">&#8592; Contents</a></p>")
   (cons :threads
"<h1>Thread monitor</h1>
<p>Lists the live SBCL threads and refreshes in the background.  Spawn a worker,
select it, and Kill it (system/UI threads are protected).</p>
<p><a href=\"general\">&#8592; Contents</a></p>"))
  "Help topic -> HTML string.")

(defun make-help (&optional (topic :general))
  "An html-view window showing help TOPIC; topic links navigate within it."
  (let* ((win (ui (window (:title " Help " :keymap *global-keys*)
                    (stack
                      (:fill (html-view :name 'doc))
                      (1 (static-text :role :status
                           :text " arrows/PgUp/PgDn scroll · n/p links · Enter follows · Esc closes "))))))
         (doc (find-view win 'doc)))
    (setf (window-scroll-target win) doc
          (hv-on-link doc) (lambda (href)
                             (let ((tp (intern (string-upcase href) :keyword)))
                               (when (assoc tp *help-pages*)
                                 (set-html doc (cdr (assoc tp *help-pages*)))
                                 (hv-next-link doc 1)))))
    (values win doc
            (lambda (s) (declare (ignore s))
              (set-html doc (or (cdr (assoc topic *help-pages*)) (cdr (assoc :general *help-pages*))))
              (hv-next-link doc 1)
              nil))))
