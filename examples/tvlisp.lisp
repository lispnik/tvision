;;;; tvlisp.lisp --- A standalone Lisp REPL / mini-IDE built on Turbo Vision.
;;;;
;;;; A focused REPL host with code-intelligence tools: completion, the threaded
;;;; debugger (restarts + backtrace), an object inspector, apropos/describe/
;;;; documentation/macroexpand/disassemble windows, package/system/class
;;;; browsers, transcript search, an editor, theming, and a live status line.

(defpackage #:tvision-tvlisp
  (:use #:common-lisp #:tvision)
  (:export #:main #:toplevel))

(in-package #:tvision-tvlisp)

;; sb-introspect (a contrib) powers the arglist hints; load it at compile time
;; so the SB-INTROSPECT package exists when this file is read.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :sb-introspect)
  (require :sb-sprof)
  (require :sb-cltl2))                   ; macroexpand-all for the macro stepper

;;; --- commands --------------------------------------------------------------

(defparameter +cm-new-repl+    300)
(defparameter +cm-clear+       301)
(defparameter +cm-tile+        302)
(defparameter +cm-cascade+     303)
(defparameter +cm-inspect+     304)
(defparameter +cm-load+        305)
(defparameter +cm-reload+      355)
(defparameter +cm-savetx+      306)
(defparameter +cm-savescript+  360)
(defparameter +cm-interrupt+   307)
(defparameter +cm-threads+     308)
(defparameter +cm-apropos+     310)
(defparameter +cm-describe+    311)
(defparameter +cm-documentation+ 312)
(defparameter +cm-macroexpand+ 313)
(defparameter +cm-disassemble+ 314)
(defparameter +cm-inspect-expr+ 315)
(defparameter +cm-packages+    316)
(defparameter +cm-systems+     317)
(defparameter +cm-classes+     318)
(defparameter +cm-find+        319)
(defparameter +cm-find-next+   320)
(defparameter +cm-replace+     348)
(defparameter +cm-trace+       349)
(defparameter +cm-untrace-all+ 350)
(defparameter +cm-trace-pkg+   357)
(defparameter +cm-trace-snap+  359)
(defparameter +cm-goto-line+   351)
(defparameter +cm-isearch+     352)
(defparameter +cm-wrap+        353)
(defparameter +cm-rgb-theme+   354)
(defparameter +cm-color-demo+  355)
(defparameter +cm-editor+      321)
(defparameter +cm-load-buffer+ 322)
(defparameter +cm-compile-buffer+ 356)
(defparameter +cm-session-save+ 323)
(defparameter +cm-session-load+ 324)
(defparameter +cm-theme+       325)
(defparameter +cm-pprint+      326)
(defparameter +cm-timing+      327)
(defparameter +cm-autoclose+   328)
(defparameter +cm-help+        329)
(defparameter +cm-histsearch+  330)
(defparameter +cm-gotodef+     331)
(defparameter +cm-funcbrowser+ 332)
(defparameter +cm-whocalls+    333)
(defparameter +cm-whorefs+     334)
(defparameter +cm-whobinds+    373)
(defparameter +cm-whosets+     374)
(defparameter +cm-whomacro+    375)
(defparameter +cm-rename+      376)   ; rename a symbol across open buffers
(defparameter +cm-step+        335)
(defparameter +cm-new-file+    336)
(defparameter +cm-save+        337)
(defparameter +cm-saveas+      338)
(defparameter +cm-save-all+    354)
(defparameter +cm-profile+     339)
(defparameter +cm-profile-det+ 340)
(defparameter +cm-browse+      341)
(defparameter +cm-bhistory+    342)
(defparameter +cm-hslookup+    343)
(defparameter +cm-pick-inspect+ 344)   ; "Inspect" button in a list picker
(defparameter +cm-pick-extra+   345)   ; optional extra action button in a picker
(defparameter +cm-pick-extra2+  358)   ; a second optional extra action
(defparameter +cm-winlist+     345)
(defparameter +cm-eval-defun+  346)
(defparameter +cm-eval-region+ 347)
(defparameter +cm-nav-back+    361)   ; pop the go-to-definition stack
(defparameter +cm-complete+    362)   ; complete the symbol at point (editor)
(defparameter +cm-comment+     363)   ; comment / uncomment the region
(defparameter +cm-wrap-paren+  364)   ; structural editing
(defparameter +cm-slurp+       365)
(defparameter +cm-barf+        366)
(defparameter +cm-splice+      367)
(defparameter +cm-raise+       368)
(defparameter +cm-snippet+     369)
(defparameter +cm-compile-defun+ 370) ; compile the form at point, list its notes
(defparameter +cm-calltree+    371)   ; call-tree (watch) window
(defparameter +cm-break-entry+ 372)   ; break on a function's next call
(defparameter +cm-sbclman+     383)   ; open the SBCL manual in the HTML browser

(defparameter +hc-repl+ 1)
;; Computed at runtime (not load/build time) so they follow the running user's
;; $HOME -- a dumped binary must not bake in the build machine's home directory.
(defun history-file () (merge-pathnames ".tvlisp_history" (user-homedir-pathname)))
(defun session-file () (merge-pathnames ".tvlisp_session" (user-homedir-pathname)))

(defparameter +kb-ctrl-c+ 3)
(defparameter +kb-ctrl-f+ 6)
(defparameter +kb-ctrl-l+ 12)
(defparameter +kb-ctrl-r+ 18)
(defparameter +kb-ctrl-rbracket+ 29)   ; Ctrl-] : jump to matching paren

(defclass tvlisp-app (tapplication)
  ((repl-count   :initform 0   :accessor repl-count)
   (find-last    :initform ""  :accessor find-last)
   (replace-last :initform ""  :accessor replace-last)
   (find-case    :initform nil :accessor find-case)
   (find-word    :initform nil :accessor find-word)
   (find-back    :initform nil :accessor find-back)
   (find-regex   :initform nil :accessor find-regex)
   (arglist-hint :initform nil :accessor arglist-hint)
   (auto-close   :initform nil :accessor auto-close)))

;;; --- menu ------------------------------------------------------------------

(defmethod tvision::application-menu ((app tvlisp-app))
  (new-menu
   (sub-menu "~F~ile"
     (new-menu
      (menu-item "~N~ew"             +cm-new-file+)
      (menu-item "New ~R~EPL"        +cm-new-repl+ :key-code +kb-f2+ :key-text "F2")
      (menu-item "~C~lear"           +cm-clear+    :key-code +kb-f3+ :key-text "F3")
      (menu-separator)
      (menu-item "Open in ~e~ditor..." +cm-editor+)
      (menu-item "~S~ave"            +cm-save+     :key-text "Ctrl-S")
      (menu-item "Save ~A~s..."      +cm-saveas+)
      (menu-item "Sa~v~e all"        +cm-save-all+)
      (menu-item "~L~oad file..."    +cm-load+     :key-code +kb-f7+ :key-text "F7")
      (menu-item "Reload ~f~ile"     +cm-reload+)
      (menu-item "Save ~t~ranscript..." +cm-savetx+)
      (menu-item "Save Lis~p~ script..." +cm-savescript+)
      (menu-separator)
      (menu-item "Save sessi~o~n"    +cm-session-save+)   ; ~o~: ~n~ collides with New
      (menu-item "Restore sess~i~on" +cm-session-load+)
      (menu-separator)
      (menu-item "E~x~it"            +cm-quit+     :key-code +kb-alt-x+ :key-text "Alt-X")))
   (sub-menu "~E~dit"
     (new-menu
      (menu-item "Cu~t~"        +cm-cut+   :key-text "Ctrl-X")
      (menu-item "~C~opy"       +cm-copy+  :key-text "Ctrl-C")
      (menu-item "~P~aste"      +cm-paste+ :key-text "Ctrl-V")
      (menu-separator)
      (menu-item "~F~ind..."    +cm-find+      :key-text "Ctrl-F")
      (menu-item "Find ne~x~t"  +cm-find-next+ :key-text "Ctrl-L")
      (menu-item "~R~eplace..." +cm-replace+)
      (menu-item "~I~ncremental search" +cm-isearch+)
      (menu-item "~G~o to line..." +cm-goto-line+)
      (menu-item "~W~ord wrap"  +cm-wrap+)
      (menu-item "~H~istory search" +cm-histsearch+ :key-text "Ctrl-R")
      (menu-separator)
      (menu-item "Comp~l~ete symbol" +cm-complete+ :key-text "Tab")
      (menu-item "Co~m~ment region"  +cm-comment+)
      (menu-item "Insert templ~a~te" +cm-snippet+)
      (menu-item "Rename s~y~mbol..." +cm-rename+)
      (sub-menu "~S~tructural"
        (new-menu
         (menu-item "~W~rap in ()" +cm-wrap-paren+)
         (menu-item "~S~plice"     +cm-splice+)
         (menu-item "~R~aise"      +cm-raise+)
         (menu-item "Sl~u~rp fwd"  +cm-slurp+)
         (menu-item "~B~arf fwd"   +cm-barf+)))
      (menu-separator)
      (menu-item "I~n~terrupt eval" +cm-interrupt+ :key-text "Ctrl-C")))
   ;; grouped into submenus so every entry has a unique, unambiguous mnemonic
   ;; (the flat menu had too many items -- Trace/Class shared Alt-C, etc.)
   (sub-menu "~L~isp"
     (new-menu
      (menu-item "~I~nspect *"        +cm-inspect+ :key-code +kb-f8+ :key-text "F8")
      (menu-item "Inspect ~e~xpr..."  +cm-inspect-expr+)
      (menu-separator)
      (menu-item "E~v~al defun"       +cm-eval-defun+)
      (menu-item "Eval ~r~egion"      +cm-eval-region+)
      (menu-item "~L~oad buffer"      +cm-load-buffer+)
      (menu-item "Compile de~f~un"    +cm-compile-defun+)
      (menu-item "~C~ompile buffer"   +cm-compile-buffer+)
      (menu-separator)
      (sub-menu "~N~avigate"
        (new-menu
         (menu-item "~G~o to definition..." +cm-gotodef+ :key-text "Alt-.")
         (menu-item "Pop ~b~ack"            +cm-nav-back+ :key-text "Alt-,")
         (menu-item "~F~unction browser..." +cm-funcbrowser+)
         (menu-item "~W~ho calls..."        +cm-whocalls+)
         (menu-item "Who ~r~eferences..."   +cm-whorefs+)
         (menu-item "Who b~i~nds..."        +cm-whobinds+)
         (menu-item "Who ~s~ets..."         +cm-whosets+)
         (menu-item "Who ~m~acroexpands..." +cm-whomacro+)))
      (sub-menu "~D~ocument"
        (new-menu
         (menu-item "~D~escribe..."      +cm-describe+)
         (menu-item "Doc~u~mentation..." +cm-documentation+)
         (menu-item "~M~acroexpand..."   +cm-macroexpand+)
         (menu-item "D~i~sassemble..."   +cm-disassemble+)
         (menu-item "~A~propos..."       +cm-apropos+)
         (menu-item "~H~yperSpec lookup..." +cm-hslookup+)))
      (sub-menu "~P~rofile / trace"
        (new-menu
         (menu-item "S~t~ep form..."     +cm-step+)
         (menu-item "~P~rofile..."       +cm-profile+)
         (menu-item "~D~eterministic profile..." +cm-profile-det+)
         (menu-item "Tra~c~e..."          +cm-trace+)
         (menu-item "Trace pac~k~age..."  +cm-trace-pkg+)
         (menu-item "Trace ~s~napshots..." +cm-trace-snap+)
         (menu-item "Ca~l~l tree..."      +cm-calltree+)
         (menu-item "~B~reak on entry..." +cm-break-entry+)
         (menu-item "~U~ntrace all..."    +cm-untrace-all+)))
      (sub-menu "~B~rowse"
        (new-menu
         (menu-item "~C~lasses..."  +cm-classes+)
         (menu-item "~P~ackages..." +cm-packages+)
         (menu-item "~S~ystems..."  +cm-systems+)))))
   (sub-menu "~O~ptions"
     (new-menu
      (menu-item "Desktop c~o~lor..." +cm-theme+)
      (menu-item "Color the~m~e"       +cm-rgb-theme+)
      (menu-item "Color ~d~emo"        +cm-color-demo+)
      (menu-item "~P~retty-print"      +cm-pprint+)
      (menu-item "Eval t~i~ming"       +cm-timing+)
      (menu-item "~A~uto-close parens" +cm-autoclose+)))
   (sub-menu "~W~indow"
     (new-menu
      (menu-item "~L~ist..." +cm-winlist+ :key-text "Alt-0")
      (menu-item "~N~ext"    +cm-next+    :key-code +kb-f6+ :key-text "F6")
      (menu-item "~Z~oom"    +cm-zoom+    :key-code +kb-f5+ :key-text "F5")
      (menu-item "~S~ize/Move" +cm-resize+ :key-text "Ctrl-F5")
      (menu-item "~T~ile"    +cm-tile+    :key-code +kb-f4+ :key-text "F4")
      (menu-item "C~a~scade" +cm-cascade+)
      (menu-item "Cl~o~se"   +cm-close+)
      (menu-separator)
      (menu-item "T~h~reads..." +cm-threads+ :key-code +kb-f9+ :key-text "F9")))
   (sub-menu "~H~elp"
     (new-menu
      (menu-item "Hyper~S~pec / browse..." +cm-browse+)
      (menu-item "SBCL ~m~anual"           +cm-sbclman+)
      (menu-item "~B~rowser history..."    +cm-bhistory+)
      (menu-separator)
      (menu-item "~H~elp" +cm-help+ :key-code +kb-f1+ :key-text "F1")))))

;;; --- live status line (package | threads | busy, or arglist hint) ----------

(defclass tvlisp-status (tstatus-line) ())

(defmethod draw ((sl tvlisp-status))
  (call-next-method)
  (let* ((app *application*)
         (rv (and app (current-repl app)))
         (hint (and app (arglist-hint app)))
         ;; parse state: is the focused REPL mid-way through an incomplete form?
         (waiting (and rv (not (repl-busy rv))
                       (let ((in (ignore-errors (tvision::repl-current-input rv))))
                         (and in (plusp (length (string-trim '(#\Space #\Tab #\Newline) in)))
                              (not (tvision::input-complete-p in))))))
         (info (cond
                 ;; while typing a known call, keep its arglist -- but still flag
                 ;; an incomplete form
                 (hint (if waiting (format nil "~a  (more)" hint) hint))
                 (t (format nil "~a~@[ ~a~] | ~d thr~:[~; | busy~]"
                            (if rv (package-name (repl-package rv)) "-")
                            (and waiting "(more)")
                            (length (sb-thread:list-all-threads))
                            (and rv (repl-busy rv))))))
         (w (point-x (view-size sl)))
         (s (format nil " ~a " info))
         (s (if (> (length s) w) (subseq s 0 w) s))
         (x (max 0 (- w (length s))))
         (c (get-color sl 1))
         (db (make-draw-buffer (length s))))
    (db-fill db #\Space c)
    (db-move-str db 0 s c)
    (write-line* sl x 0 (length s) 1 db)))

(defmethod tvision::init-status-line ((app tvlisp-app))
  (let* ((h (point-y (view-size app))) (w (point-x (view-size app)))
         (sl (make-instance 'tvlisp-status :items (tvision::status-line-items app))))
    (set-bounds sl (make-trect 0 (1- h) w h))
    (setf (view-options sl) (logior (view-options sl) +of-post-process+))
    (setf (program-status-line app) sl)
    (insert app sl)))

(defmethod tvision::status-line-items ((app tvlisp-app))
  (list (make-status-item "~Alt-X~ Exit" +kb-alt-x+ +cm-quit+)
        (make-status-item "~F1~ Help"    +kb-f1+ +cm-help+)
        (make-status-item "~F2~ REPL"    +kb-f2+ +cm-new-repl+)
        (make-status-item "~F8~ Inspect" +kb-f8+ +cm-inspect+)
        (make-status-item "~F10~ Menu"   +kb-f10+ 0)))

;;; --- windows ---------------------------------------------------------------

(defun open-repl-window (app &key maximized (package nil) bounds)
  (let* ((desk (program-desktop app))
         (n (incf (repl-count app)))
         (dw (point-x (view-size desk))) (dh (point-y (view-size desk)))
         (bounds (or bounds
                     (if maximized
                         (make-trect 0 0 dw dh)
                         (let ((ox (* (mod (1- n) 5) 3)) (oy (mod (1- n) 5)))
                           (make-trect ox oy (min dw (+ ox 72)) (min dh (+ oy 22))))))))
    (multiple-value-bind (w rv)
        (make-repl-window bounds :title (format nil "Lisp REPL ~d" n)
                                 :history-file (history-file))
      (setf (view-help-ctx w) +hc-repl+)
      (when package (let ((p (find-package package))) (when p (setf (repl-package rv) p))))
      (insert desk w)
      (focus rv)
      rv)))

(defun current-repl (app)
  (let ((w (group-current (program-desktop app))))
    (when (typep w 'twindow)
      (find-if (lambda (v) (typep v 'trepl-view)) (group-subviews w)))))

(defun some-repl (app)
  "The focused REPL, or any REPL in the desktop."
  (or (current-repl app)
      (dolist (w (group-subviews (program-desktop app)))
        (when (typep w 'twindow)
          (let ((rv (find-if (lambda (v) (typep v 'trepl-view)) (group-subviews w))))
            (when rv (return rv)))))))

(defvar *last-thread-check* 0
  "Throttle for idle-time thread-monitor auto-refresh.")

(defmethod tvision::idle ((app tvlisp-app))
  "While a thread monitor is open, refresh it when the live thread set changes
 (checked a few times a second on idle, so new/dead threads appear on their own)."
  (let ((now (get-internal-real-time)))
    (when (> (- now *last-thread-check*) (floor internal-time-units-per-second 4))   ; ~250ms
      (setf *last-thread-check* now)
      (let ((w (first-that (program-desktop app) (lambda (v) (typep v 'tthread-window)))))
        (when w
          (let ((tl (tw-list w)))
            (unless (equal (sb-thread:list-all-threads) (thread-list-threads tl))
              (thread-list-refresh tl)
              (when tvision:*screen* (flush-screen tvision:*screen*)))))))))

(defun open-thread-window (app)
  (let* ((desk (program-desktop app))
         (existing (first-that desk (lambda (v) (typep v 'tthread-window)))))
    (if existing
        (progn (focus existing)
               (let ((tl (tw-list existing))) (when tl (thread-list-refresh tl))))
        (let* ((dw (point-x (view-size desk))) (dh (point-y (view-size desk)))
               (w (min 56 (- dw 2))) (h (min 18 (- dh 2)))
               (ax (max 0 (- dw w 1))) (ay 1))
          (insert desk (make-thread-window (make-trect ax ay (+ ax w) (+ ay h))))))))

;;; --- prompts / output helpers ----------------------------------------------

(defun prompt-line (title label &optional (default ""))
  (multiple-value-bind (cmd s) (input-box title label (or default "") 200)
    (when (and (= cmd +cm-ok+) (plusp (length (string-trim '(#\Space #\Tab) s)))) s)))

(defun err-box (e)
  (message-box (format nil "~a" e) (logior +mf-error+ +mf-ok-button+)))

(defun read-in (rv string)
  (let ((*package* (repl-package rv))) (read-from-string string)))

(defun choose-from-list (title items &key (w 56) (h 18))
  "Modal sorted (type-ahead) picker; return the chosen string or NIL."
  (when (and *application* items)
    (let* ((desk (program-desktop *application*))
           (d (make-instance 'tdialog :title title :bounds (make-trect 0 0 w h)))
           (vsb (standard-scrollbar d t))
           (lb (make-instance 'tsorted-list-box :items items :command +cm-ok+
                              :bounds (make-trect 1 1 (1- w) (- h 3)))))
      (insert d lb) (attach-scrollbars lb :vscroll vsb)
      (insert d (make-button (make-trect (- w 24) (- h 3) (- w 14) (- h 1)) "~O~K" +cm-ok+ t))
      (insert d (make-button (make-trect (- w 12) (- h 3) (- w 2) (- h 1)) "Cancel" +cm-cancel+))
      (move-to d (max 0 (floor (- (point-x (view-size desk)) w) 2))
               (max 0 (floor (- (point-y (view-size desk)) h) 2)))
      (focus lb)
      (when (and (= (exec-view desk d) +cm-ok+) (plusp (list-count lb)))
        (list-item lb (list-focused lb))))))

(defun choose-index (title labels &key (start 0) (w 64) (h 18))
  "Modal, order-preserving picker over LABELS; return the chosen index (focused
on START) or NIL on cancel.  Enter or OK selects."
  (when (and *application* labels)
    (let* ((desk (program-desktop *application*))
           (d (make-instance 'tdialog :title title :bounds (make-trect 0 0 w h)))
           (vsb (standard-scrollbar d t))
           (lb (make-instance 'tlist-box :items labels :command +cm-ok+
                              :bounds (make-trect 1 1 (1- w) (- h 3)))))
      (insert d lb) (attach-scrollbars lb :vscroll vsb)
      (insert d (make-button (make-trect (- w 24) (- h 3) (- w 14) (- h 1)) "~O~K" +cm-ok+ t))
      (insert d (make-button (make-trect (- w 12) (- h 3) (- w 2) (- h 1)) "Cancel" +cm-cancel+))
      (move-to d (max 0 (floor (- (point-x (view-size desk)) w) 2))
               (max 0 (floor (- (point-y (view-size desk)) h) 2)))
      (list-focus-item lb (min (max 0 start) (1- (list-count lb))))
      (focus lb)
      (when (= (exec-view desk d) +cm-ok+) (list-focused lb)))))

(defun open-outline-window (title roots)
  (let* ((desk (program-desktop *application*))
         (w (make-instance 'twindow :title title :bounds (make-trect 4 2 64 22)))
         (vsb (standard-scrollbar w t))
         (ol (make-instance 'toutline :roots roots
                            :bounds (make-trect 1 1 (1- (point-x (view-size w)))
                                                (1- (point-y (view-size w)))))))
    (insert w ol) (attach-scrollbars ol :vscroll vsb)
    (insert desk w) (focus ol)
    ol))

;;; --- HTML browser (HyperSpec help) -----------------------------------------
;;; A THtmlView in a window, wired so that following a link fetches the next
;;; page.  Remote pages are fetched with curl (no in-image TLS); local files are
;;; read directly.  Backspace goes Back.

(defun %url-p (s) (or (search "://" s) (and (>= (length s) 7) (string-equal "http" s :end2 4))))

(defun %http-get (url)
  "Fetch URL with curl; return the body string, or NIL on failure."
  (handler-case
      (let* ((out (make-string-output-stream))
             (p (sb-ext:run-program "curl" (list "-fsSL" "--max-time" "20" url)
                                    :search t :output out :error nil :wait t)))
        (if (and p (eql 0 (sb-ext:process-exit-code p)))
            (get-output-stream-string out)
            nil))
    (error () nil)))

(defun %read-file-string (path)
  (handler-case
      (with-open-file (s path :external-format :utf-8)
        (let ((buf (make-string (file-length s))))
          (subseq buf 0 (read-sequence buf s))))
    (error () nil)))

(defun %normalize-path (path)
  "Resolve . and .. in a /-separated PATH (string), returning an absolute path."
  (let ((segs '()) (start 0) (n (length path)))
    (loop for i from 0 to n
          when (or (= i n) (char= (char path i) #\/)) do
            (let ((seg (subseq path start i)))
              (cond ((or (string= seg "") (string= seg ".")) nil)
                    ((string= seg "..") (when segs (pop segs)))
                    (t (push seg segs)))
              (setf start (1+ i))))
    (format nil "/~{~a~^/~}" (nreverse segs))))

(defun %resolve-location (base href)
  "Resolve HREF against the current BASE location (URL or file path), preserving
any #fragment so the browser can scroll to an anchor."
  (let* ((hash (position #\# href))
         (h (if hash (subseq href 0 hash) href))
         (frag (and hash (subseq href hash)))         ; includes the leading #
         (resolved
           (cond
             ((zerop (length h)) base)                ; pure fragment -> same page
             ((%url-p h) h)                           ; absolute URL
             ((%url-p base)                           ; relative against a URL
              (let* ((p (search "://" base))
                     (slash (position #\/ base :start (+ p 3)))
                     (origin (if slash (subseq base 0 slash) base))
                     (path (if slash (subseq base slash) "/")))
                (if (and (plusp (length h)) (char= (char h 0) #\/))
                    (concatenate 'string origin (%normalize-path h))
                    (let ((dir (subseq path 0 (1+ (or (position #\/ path :from-end t) 0)))))
                      (concatenate 'string origin (%normalize-path (concatenate 'string dir h)))))))
             (t                                       ; relative against a file
              (namestring (merge-pathnames h (directory-namestring base)))))))
    (if frag (concatenate 'string resolved frag) resolved)))

(defun %location-title (loc)
  (let ((s (if (position #\# loc) (subseq loc 0 (position #\# loc)) loc)))
    (let ((slash (position #\/ s :from-end t)))
      (if (and slash (< (1+ slash) (length s))) (subseq s (1+ slash)) s))))

(defclass thtml-window (twindow)
  ((view   :initform nil :accessor hw-view)
   (base   :initform "" :accessor hw-base)
   (back   :initform '() :accessor hw-back-stack)   ; entries behind the current one
   (fwd    :initform '() :accessor hw-fwd-stack)    ; entries ahead (after going Back)
   (titles :initform '() :accessor hw-titles)        ; (location . <title>) seen so far
   (times  :initform '() :accessor hw-times)))         ; (location . universal-time) last visit
;; Each Back/Forward stack entry is (LOCATION SCROLL FOCUS): the page, the
;; scroll position (DX . DY), and the focused-link index the view had when we
;; navigated away from it, so returning to the entry restores that exact spot
;; and link cursor — even for #anchor jumps within one page.

(defun hw-label (w loc)
  "How LOC should appear in the history: its <title> if we have one, else the URL."
  (or (cdr (assoc loc (hw-titles w) :test #'string=)) loc))

(defvar *page-cache* '() "LRU alist (url . content), most-recent first.")
(defparameter +page-cache-max+ 16)

(defun %cache-get (url) (cdr (assoc url *page-cache* :test #'string=)))
(defun %cache-put (url content)
  (setf *page-cache* (cons (cons url content)
                           (remove url *page-cache* :key #'car :test #'string=)))
  (when (> (length *page-cache*) +page-cache-max+)
    (setf *page-cache* (subseq *page-cache* 0 +page-cache-max+))))

(defun hw-load (loc)
  "Load LOC's content.  Remote URLs are cached (LRU) so Back/Forward are instant;
local files are always read fresh."
  (if (%url-p loc)
      (or (%cache-get loc)
          (let ((c (%http-get loc))) (when c (%cache-put loc c)) c))
      (%read-file-string loc)))

(defun hw-set-title (w)
  ;; prefer the page's <title> (recorded in HW-TITLES); fall back to the filename
  (let ((title (or (cdr (assoc (hw-base w) (hw-titles w) :test #'string=))
                   (%location-title (hw-base w)))))
    (setf (window-title w)
          (format nil "~a  [^B/Bksp back  ^F fwd  ^R reload]" title))))

(defun hw-scroll-pos (w)
  "The view's current scroll position as (DX . DY)."
  (let ((d (scroller-delta (hw-view w))))
    (cons (point-x d) (point-y d))))

(defun hw-here (w)
  "The current page as a history entry: (LOCATION SCROLL FOCUSED-LINK)."
  (list (hw-base w) (hw-scroll-pos w) (html-focused-link (hw-view w))))

(defun hw-go (w loc &key (record t) restore focus)
  "Load LOC (optionally with a #fragment) into the window.  When RECORD, treat it
as fresh navigation: push the current page (with its scroll and link cursor) onto
the Back stack and drop Forward.  RESTORE, when given, is a (DX . DY) scroll
position to return to and FOCUS the focused-link index to re-select — used by
Back / Forward / reload / history; otherwise a #fragment anchor (or the top) is
used.  Return T on a successful load."
  (let* ((hash (position #\# loc))
         (base (if hash (subseq loc 0 hash) loc))
         (frag (and hash (plusp (length (subseq loc (1+ hash)))) (subseq loc (1+ hash))))
         (content (hw-load base)))
    (cond
      (content
       (when (and record (plusp (length (hw-base w))))
         ;; leaving the current page: remember it and where we were on it
         (push (hw-here w) (hw-back-stack w))
         (setf (hw-fwd-stack w) '()))
       (setf (hw-base w) base)
       ;; remember when this page was last visited (for the history list)
       (setf (hw-times w) (cons (cons base (get-universal-time))
                                (remove base (hw-times w) :key #'car :test #'string=)))
       ;; remember the page's <title> for the history list / caption
       (let ((title (html-document-title content)))
         (when title
           (setf (hw-titles w)
                 (cons (cons base title)
                       (remove base (hw-titles w) :key #'car :test #'string=))))
         ;; also log to the persistent cross-session visited-pages history
         (record-browse base title))
       (hw-set-title w)
       (set-html (hw-view w) content)   ; resets scroll to the top and clears the link cursor
       ;; restore the focused-link cursor without scrolling it into view (that is
       ;; what html-focus-link would do); the saved scroll position wins
       (when (and focus (< focus (html-link-count (hw-view w))))
         (setf (html-focused-link (hw-view w)) focus))
       (cond
         ;; Back / Forward / reload / history: return to where we left off
         (restore (scroll-to (hw-view w) (car restore) (cdr restore)))
         ;; a fresh visit to a #fragment jumps to its anchor
         (frag (html-goto-anchor (hw-view w) frag)))
       (focus (hw-view w))
       (draw-view w)
       t)
      (t (message-box (format nil "Could not load:~%~a" base)
                      (logior +mf-error+ +mf-ok-button+))
         nil))))

(defun hw-go-entry (w entry &rest args)
  "Navigate to history ENTRY (LOCATION SCROLL FOCUS), restoring its scroll and
link cursor.  Extra ARGS are passed through to HW-GO."
  (apply #'hw-go w (first entry)
         :record nil :restore (second entry) :focus (third entry) args))

(defun hw-back (w)
  "Go to the previous page, restoring its scroll and link cursor and remembering
the current one for Forward."
  (when (hw-back-stack w)
    (let ((entry (pop (hw-back-stack w))) (here (hw-here w)))
      (if (hw-go-entry w entry)
          (push here (hw-fwd-stack w))
          (push entry (hw-back-stack w))))))

(defun hw-forward (w)
  "Go to the next page (undo a Back), restoring its scroll and link cursor and
remembering the current one for Back."
  (when (hw-fwd-stack w)
    (let ((entry (pop (hw-fwd-stack w))) (here (hw-here w)))
      (if (hw-go-entry w entry)
          (push here (hw-back-stack w))
          (push entry (hw-fwd-stack w))))))

(defun hw-reload (w)
  (when (plusp (length (hw-base w)))
    ;; drop the cached copy so reload really refetches, but keep our place
    (setf *page-cache* (remove (hw-base w) *page-cache* :key #'car :test #'string=))
    (hw-go-entry w (hw-here w))))

(defun hw-history-entries (w)
  "All visited entries (LOCATION . scroll) oldest-first, current page in place."
  (append (reverse (hw-back-stack w)) (list (hw-here w)) (hw-fwd-stack w)))

(defun hw-history-list (w)
  "The full visit history (locations) in chronological order (oldest first)."
  (mapcar #'car (hw-history-entries w)))

(defun hw-history-index (w)
  "Position of the current page within (HW-HISTORY-LIST W)."
  (length (hw-back-stack w)))

(defun hw-goto-index (w i)
  "Jump to chronological history entry I, rebuilding the Back/Forward stacks
around it and restoring that entry's scroll position."
  (let ((entries (hw-history-entries w)))
    (when (and (>= i 0) (< i (length entries)) (/= i (hw-history-index w)))
      (let ((entry (nth i entries)))
        (setf (hw-back-stack w) (reverse (subseq entries 0 i))
              (hw-fwd-stack w)  (subseq entries (1+ i)))
        (hw-go-entry w entry)))))

(defmethod handle-event ((w thtml-window) event)
  (cond
    ((and (= (event-type event) +ev-broadcast+)
          (= (event-command event) +cm-html-link+)
          (eq (event-info event) (hw-view w)))
     (let ((href (html-current-href (hw-view w))))
       (when href (hw-go w (%resolve-location (hw-base w) href))))
     (clear-event event))
    ((= (event-type event) +ev-key-down+)
     (let ((k (event-key-code event))
           (alt (logtest (event-modifiers event) +md-alt+)))
       (cond
         ;; Ctrl-B / Backspace / Alt-Left -> Back
         ((or (= k 2) (= k +kb-back+) (and alt (= k +kb-left+)))  (hw-back w) (clear-event event))
         ;; Ctrl-F / Alt-Right -> Forward
         ((or (= k 6) (and alt (= k +kb-right+)))                 (hw-forward w) (clear-event event))
         ;; Ctrl-R -> Reload
         ((= k 18)                                                (hw-reload w) (clear-event event))
         (t (call-next-method)))))
    (t (call-next-method))))

(defun open-html-window (app loc)
  (let* ((desk (program-desktop app))
         (dw (point-x (view-size desk))) (dh (point-y (view-size desk)))
         (w (make-instance 'thtml-window :title "Browser"
                           :bounds (make-trect 1 0 (min (- dw 1) 88) (min (- dh 1) 30))))
         (vsb (standard-scrollbar w t))
         (hv (make-instance 'thtml-view
                            :bounds (make-trect 1 1 (1- (point-x (view-size w)))
                                                (1- (point-y (view-size w)))))))
    (insert w hv) (attach-scrollbars hv :vscroll vsb)
    (setf (hw-view w) hv)
    (insert desk w)
    (hw-go w loc)
    w))

(defparameter +hyperspec-default+
  "https://www.lispworks.com/documentation/HyperSpec/Front/index_tx.htm")

(defun do-browse (app)
  (let ((loc (prompt-line "HyperSpec / browse" "URL or file:" +hyperspec-default+)))
    (when loc (open-html-window app (string-trim " " loc)))))

(defparameter +sbcl-manual-default+ "http://www.sbcl.org/manual/index.html")

(defun do-sbcl-manual (app)
  (open-html-window app +sbcl-manual-default+))

(defun %hhmm (universal-time)
  "HH:MM for UNIVERSAL-TIME."
  (multiple-value-bind (s m h) (decode-universal-time universal-time)
    (declare (ignore s))
    (format nil "~2,'0d:~2,'0d" h m)))

;;; --- persistent (cross-session) visited-pages history ----------------------
(defvar *browse-history* '()
  "Global visited-pages log: (location title universal-time), newest first.")

(defun browse-history-file () (merge-pathnames ".tvlisp_browse_history" (user-homedir-pathname)))

(defun load-browse-history ()
  (setf *browse-history*
        (or (ignore-errors
             (with-open-file (s (browse-history-file) :if-does-not-exist nil)
               (and s (read s nil nil))))
            '())))

(defun save-browse-history ()
  (ignore-errors
   (with-open-file (s (browse-history-file) :direction :output
                                            :if-exists :supersede :if-does-not-exist :create)
     (let ((*print-readably* nil))
       (prin1 (subseq *browse-history* 0 (min 300 (length *browse-history*))) s)))))

(defun record-browse (loc title)
  "Add LOC (with TITLE) to the persistent visited-pages log and save it."
  (when (and loc (plusp (length loc)))
    (setf *browse-history*
          (cons (list loc (or title loc) (get-universal-time))
                (remove loc *browse-history* :key #'first :test #'string=)))
    (save-browse-history)))

(defun do-visited-pages (app)
  "Browse the persistent visited-pages history (across sessions); open a pick."
  (if (null *browse-history*)
      (message-box "No visited pages recorded yet." (logior +mf-information+ +mf-ok-button+))
      (let* ((entries *browse-history*)
             (labels (mapcar (lambda (e) (format nil "~a  ~a" (%hhmm (third e)) (second e))) entries))
             (sel (choose-index "Visited pages (all sessions)" labels)))
        (when sel (open-html-window app (first (nth sel entries)))))))

(defun do-browser-history (app)
  "Pop up the focused browser window's history; selecting an entry visits it."
  (let ((w (group-current (program-desktop app))))
    (cond
      ((not (typep w 'thtml-window))
       ;; not in a browser -> show the persistent cross-session visited list
       (do-visited-pages app))
      (t (let* ((items (hw-history-list w))
                (cur (hw-history-index w))
                (labels (loop for loc in items for i from 0
                              for ut = (cdr (assoc loc (hw-times w) :test #'string=))
                              collect (format nil "~:[  ~;> ~]~@[~a  ~]~a"
                                              (= i cur) (and ut (%hhmm ut)) (hw-label w loc)))))
           (let ((sel (choose-index "Browser history" labels :start cur)))
             (when sel (hw-goto-index w sel))))))))

;;; --- HyperSpec symbol lookup -----------------------------------------------

(defparameter +hyperspec-base+ "https://www.lispworks.com/documentation/HyperSpec/")
(defvar *hyperspec-map* nil
  "Lazy-loaded hash table mapping an upcased symbol name to its HyperSpec URL.")

(defun %load-hyperspec-map ()
  "Fetch and parse the HyperSpec Data/Map_Sym.txt -- alternating NAME / path
lines -- into a name -> URL hash table, or NIL on failure."
  (let ((txt (%http-get (concatenate 'string +hyperspec-base+ "Data/Map_Sym.txt"))))
    (when txt
      (let ((map (make-hash-table :test 'equal))
            (lines (with-input-from-string (s txt)
                     (loop for l = (read-line s nil nil) while l
                           collect (string-trim '(#\Return #\Space #\Tab) l)))))
        ;; paths are relative to the Data/ directory, e.g. "../Body/f_car_c.htm"
        (loop for (name path) on lines by #'cddr
              when (and name path (plusp (length name)) (plusp (length path))) do
                (let ((rel (if (eql 0 (search "../" path))
                               (subseq path 3)                       ; -> Body/...
                               (concatenate 'string "Data/" path))))
                  (setf (gethash (string-upcase name) map)
                        (concatenate 'string +hyperspec-base+ rel))))
        map))))

(defun hyperspec-url (name)
  "The HyperSpec page URL for symbol NAME, or NIL.  Loads the map on first use;
a failed load is retried next time."
  (when (null *hyperspec-map*)
    (setf *hyperspec-map* (%load-hyperspec-map)))
  (and *hyperspec-map* (gethash (string-upcase name) *hyperspec-map*)))

(defun %hs-symchar-p (ch) (or (alphanumericp ch) (find ch "+-*/@$%^&_=<>.~!?:")))

(defun %symbol-at-point (view)
  "The symbol token surrounding the cursor in text VIEW, or NIL."
  (when (typep view 'ttext-view)
    (let* ((line (current-line-string view))
           (col (min (text-cur-col view) (length line)))
           (start col) (end col))
      (loop while (and (> start 0) (%hs-symchar-p (char line (1- start)))) do (decf start))
      (loop while (and (< end (length line)) (%hs-symchar-p (char line end))) do (incf end))
      (when (< start end) (subseq line start end)))))

(defun %strip-package (name)
  "Drop a leading PACKAGE: / PACKAGE:: qualifier from NAME."
  (let ((p (position #\: name :from-end t)))
    (if p (subseq name (1+ p)) name)))

(defun %current-text-view (app)
  "The focused editor or REPL text view, or NIL."
  (let ((w (group-current (program-desktop app))))
    (if (typep w 'teditor-window) (editor-window-editor w) (current-repl app))))

(defun %point-symbol ()
  "The symbol token at the cursor of the focused editor/REPL view, or \"\" --
used to prefill the Lisp-tool prompts (Describe, Go-to-def, Trace, ...)."
  (let* ((app *application*)
         (view (and app (%current-text-view app))))
    (or (and view (%symbol-at-point view)) "")))

(defun do-hyperspec-lookup (app)
  "Open a browser on the HyperSpec page for the symbol at point.  When there is
no symbol, or it is not a standard symbol, prompt (prefilled with what we found)."
  (let* ((view (%current-text-view app))
         (tok (%strip-package (or (and view (%symbol-at-point view)) "")))
         (url (and (plusp (length tok)) (hyperspec-url tok))))
    (cond
      (url (open-html-window app url))
      (t (let ((name (prompt-line "HyperSpec lookup" "Symbol:" tok)))
           (when name
             (let* ((nm (%strip-package (string-trim " " name)))
                    (u (and (plusp (length nm)) (hyperspec-url nm))))
               (cond
                 (u (open-html-window app u))
                 ((plusp (length nm))
                  (message-box (format nil "~a is not in the Common Lisp HyperSpec."
                                       (string-upcase nm))
                               (logior +mf-information+ +mf-ok-button+)))))))))))

;;; --- Lisp tools ------------------------------------------------------------

(defun %editor-offset (ed)
  "Absolute character offset of the cursor within (text-string ED)."
  (let ((off 0))
    (dotimes (i (text-cur-line ed))
      (incf off (1+ (length (nth-line ed i)))))   ; + the line's newline
    (+ off (min (text-cur-col ed) (length (current-line-string ed))))))

(defun %sexp-at-offset (str off)
  "Substring of STR for the innermost balanced () form containing OFF, or NIL.
Skips strings, #\\char literals and ; comments so their parens don't count."
  (let ((len (length str)) (stack '()) (best nil) (i 0))
    (loop while (< i len) do
      (let ((c (char str i)))
        (cond
          ((char= c #\;)                                  ; line comment
           (loop while (and (< i len) (char/= (char str i) #\Newline)) do (incf i)))
          ((char= c #\")                                  ; string literal
           (incf i)
           (loop while (< i len) do
             (let ((d (char str i)))
               (incf i)
               (cond ((char= d #\\) (incf i))
                     ((char= d #\") (return))))))
          ((and (char= c #\#) (< (1+ i) len) (char= (char str (1+ i)) #\\))
           (incf i 3))                                    ; #\x char literal
          ((char= c #\() (push i stack) (incf i))
          ((char= c #\))
           (when stack
             (let ((start (pop stack)))
               (when (and (<= start off) (<= off i)        ; encloses the cursor,
                          (or (null best) (> start (car best))))  ; keep innermost
                 (setf best (cons start (1+ i))))))
           (incf i))
          (t (incf i)))))
    (when best (subseq str (car best) (cdr best)))))

(defun editor-form-at-point (ed)
  "The selection, the s-expression around the cursor, or the current line."
  (let ((sel (selected-string ed)))
    (if (and sel (plusp (length (string-trim '(#\Space #\Tab #\Newline) sel))))
        sel
        (or (ignore-errors (%sexp-at-offset (text-string ed) (%editor-offset ed)))
            (current-line-string ed)))))

(defun %toplevel-form-at-offset (str off)
  "The outermost ( ) form whose span contains OFF, or NIL.  Skips strings,
#\\char literals and ; comments."
  (let ((len (length str)) (i 0) (depth 0) (start nil))
    (loop while (< i len) do
      (let ((c (char str i)))
        (cond
          ((char= c #\;)
           (loop while (and (< i len) (char/= (char str i) #\Newline)) do (incf i)))
          ((char= c #\")
           (incf i)
           (loop while (< i len) do
             (let ((d (char str i))) (incf i)
               (cond ((char= d #\\) (incf i)) ((char= d #\") (return))))))
          ((and (char= c #\#) (< (1+ i) len) (char= (char str (1+ i)) #\\)) (incf i 3))
          ((char= c #\()
           (when (zerop depth) (setf start i))
           (incf depth) (incf i))
          ((char= c #\))
           (incf i)
           (when (plusp depth) (decf depth))
           (when (zerop depth)
             (when (and start (<= start off) (<= off i))
               (return-from %toplevel-form-at-offset (subseq str start i)))
             (setf start nil)))
          (t (incf i)))))
    nil))

(defun %buffer-in-package (text upto)
  "Package-name STRING of the last (in-package ...) form starting before offset
UPTO in TEXT, or NIL -- so eval-defun / eval-region run in the buffer's declared
package rather than the listener's current one."
  (let ((pos 0) (found nil))
    (loop for i = (search "(in-package" text :start2 pos :test #'char-equal)
          while (and i (< i upto))
          do (let ((form (ignore-errors
                          (with-input-from-string (s text :start i) (read s nil nil)))))
               (when (and (consp form) (symbolp (first form))
                          (string-equal (symbol-name (first form)) "IN-PACKAGE")
                          (cdr form))
                 (setf found (string (second form))))
               (setf pos (+ i 11))))
    found))

(defun %with-buffer-package (app pkg form-text)
  "Prefix FORM-TEXT with (in-package PKG) when the buffer declares a package PKG
that exists and differs from the listener's current one (so the form evaluates
in its file's package, and the listener follows -- like compiling a file)."
  (let* ((rv (some-repl app))
         (cur (and rv (package-name (repl-package rv)))))
    (if (and pkg (find-package pkg) (not (string-equal pkg cur)))
        (format nil "(in-package ~s)~%~a" pkg form-text)
        form-text)))

;;; Interactive macro stepper (macrostep / SLIME C-c C-m style): the rendered
;;; form is navigable, and you expand the macro call *at the cursor* in place,
;;; with undo.  The expansion preserves the surrounding code so you read the
;;; result as ordinary source.
;;;
;;;   e / Enter  expand the macro at the cursor one step, in place
;;;   m          fully expand the macro at the cursor (MACROEXPAND)
;;;   M          expand every macro in the whole form (MACROEXPAND-ALL)
;;;   Tab        jump to the next expandable (macro-call) position
;;;   u          undo the last expansion        0  reset to the original form
;;;   o          open the expansion in an editor c  copy it to the clipboard

(defclass tmacro-window (twindow)
  ((form    :initarg :form :accessor macro-form)     ; current form object
   (orig    :initarg :orig :accessor macro-orig)      ; original, for reset
   (pkg     :initarg :pkg  :accessor macro-pkg)
   (view    :initform nil  :accessor macro-view)
   (spans   :initform '()  :accessor macro-spans)     ; (start end . cons) per cons
   (history :initform '()  :accessor macro-history)   ; previous forms (undo)
   (steps   :initform 0    :accessor macro-steps)))

;;; --- a span-tracking pretty-printer: text + a char-range for every cons -----

(defun %pp-spans (form pkg &key (width 72))
  "Render FORM (read in PKG) to a readable, indented string; return
(values STRING SPANS) where SPANS is a list of (start end . cons) giving the
character range each cons occupies — used to map the cursor to a subform."
  (let ((out (make-string-output-stream)) (pos 0) (col 0) (spans '()))
    (labels
        ((emit (s)
           (write-string s out) (incf pos (length s))
           (let ((nl (position #\Newline s :from-end t)))
             (if nl (setf col (- (length s) nl 1)) (incf col (length s)))))
         (atom-str (x)
           (let ((*package* pkg) (*print-pretty* nil) (*print-readably* nil)
                 (*print-length* 64) (*print-level* 8))
             (handler-case (prin1-to-string x) (error () "#<?>"))))
         (oneline-len (x)
           (length (atom-str x)))
         (sugar (x)                       ; quote/function reader sugar
           (when (and (consp x) (consp (cdr x)) (null (cddr x)))
             (case (car x)
               (quote (values "'" (cadr x)))
               (function (values "#'" (cadr x))))))
         (pr (x indent depth)
           (cond
             ((> depth 100) (emit (atom-str x)))     ; runaway / circular guard
             ((consp x)
              (multiple-value-bind (pfx sub) (sugar x)
                (if pfx
                    (let ((start pos))
                      (emit pfx) (pr sub (+ indent (length pfx)) (1+ depth))
                      (push (list* start pos x) spans))
                    (pr-list x indent depth))))
             (t (emit (atom-str x)))))
         (pr-list (x indent depth)
           (let ((start pos)
                 (inline (<= (+ col (oneline-len x)) width)))
             (emit "(")
             (loop with body = (+ indent 2)
                   for cell on x for first = t then nil do
                     (cond (first (pr (car cell) (1+ indent) (1+ depth)))
                           (inline (emit " ") (pr (car cell) (1+ indent) (1+ depth)))
                           (t (emit (format nil "~%~a" (make-string body :initial-element #\Space)))
                              (pr (car cell) body (1+ depth))))
                     (when (and (cdr cell) (not (consp (cdr cell))))   ; dotted tail
                       (emit " . ") (pr (cdr cell) (1+ indent) (1+ depth)) (return)))
             (emit ")")
             (push (list* start pos x) spans))))
      (pr form 0 0)
      (values (get-output-stream-string out) (nreverse spans)))))

(defun %span-at (spans offset)
  "Innermost span (start end . cons) whose range contains OFFSET, or NIL."
  (let (best)
    (dolist (s spans best)
      (when (and (<= (car s) offset) (< offset (cadr s))
                 (or (null best) (< (- (cadr s) (car s)) (- (cadr best) (car best)))))
        (setf best s)))))

(defun %subst-eq (new old tree)
  "Copy TREE, replacing the sub-tree EQ to OLD with NEW (structure sharing kept
where nothing changed)."
  (cond ((eq tree old) new)
        ((consp tree)
         (let ((a (%subst-eq new old (car tree))) (d (%subst-eq new old (cdr tree))))
           (if (and (eq a (car tree)) (eq d (cdr tree))) tree (cons a d))))
        (t tree)))

(defun %macro-call-p (x)
  "True when X is a macro call (its head names a macro)."
  (and (consp x) (symbolp (car x)) (macro-function (car x)) t))

(defun %macro-set-cursor (tv off)
  "Place TV's cursor at character offset OFF and scroll it into view."
  (let ((line 0) (o off) (n (line-count tv)))
    (loop (let ((len (length (nth-line tv line))))
            (when (or (<= o len) (>= (1+ line) n))
              (setf (text-cur-line tv) line (text-cur-col tv) (max 0 (min o len)))
              (return))
            (decf o (1+ len)) (incf line)))
    (ensure-visible tv)))

(defun %macro-render (w &optional keep-offset)
  "Re-render the current form, refreshing the span table and the title; when
KEEP-OFFSET is given, restore the cursor there."
  (let* ((tv (macro-view w))
         (width (max 20 (- (point-x (view-size tv)) 1))))
    (multiple-value-bind (text spans) (%pp-spans (macro-form w) (macro-pkg w) :width width)
      (setf (macro-spans w) spans)
      (set-text tv text)
      (when keep-offset (%macro-set-cursor tv (min keep-offset (length text))))
      (let ((n (count-if (lambda (s) (%macro-call-p (cddr s))) spans)))
        (setf (window-title w)
              (format nil "Macroexpand — ~d step~:p, ~d expandable  (e:step Tab:next M:all u:undo)"
                      (macro-steps w) n)))
      (draw-view w))))

(defun %macro-expand-at (w &key full)
  "Expand the macro call at the cursor (one step, or FULL via MACROEXPAND) in
place, recording the previous form for undo."
  (let* ((tv (macro-view w))
         (span (%span-at (macro-spans w) (%editor-offset tv)))
         (target (and span (cddr span))))
    (cond
      ((not (consp target))
       (message-box "Put the cursor on a form to expand."
                    (logior +mf-information+ +mf-ok-button+)))
      (t (let ((*package* (macro-pkg w)))
           (multiple-value-bind (exp expanded)
               (handler-case (if full (macroexpand target) (macroexpand-1 target))
                 (error (e) (err-box e) (values target nil)))
             (if (not expanded)
                 (message-box "Not a macro call (nothing to expand here)."
                              (logior +mf-information+ +mf-ok-button+))
                 (progn
                   (push (macro-form w) (macro-history w))
                   (setf (macro-form w) (%subst-eq exp target (macro-form w)))
                   (incf (macro-steps w))
                   (%macro-render w (car span))))))))))

(defun %macro-expand-all (w)
  "Expand every macro in the whole form (sb-cltl2:macroexpand-all)."
  (let ((fn (find-symbol "MACROEXPAND-ALL" :sb-cltl2)))
    (if (and fn (fboundp fn))
        (let ((*package* (macro-pkg w)))
          (handler-case
              (let ((all (funcall fn (macro-form w))))
                (push (macro-form w) (macro-history w))
                (setf (macro-form w) all)
                (incf (macro-steps w))
                (%macro-render w 0))
            (error (e) (err-box e))))
        (message-box "macroexpand-all is unavailable."
                     (logior +mf-information+ +mf-ok-button+)))))

(defun %macro-undo (w)
  (if (macro-history w)
      (progn (setf (macro-form w) (pop (macro-history w)))
             (when (plusp (macro-steps w)) (decf (macro-steps w)))
             (%macro-render w 0))
      (message-box "Nothing to undo." (logior +mf-information+ +mf-ok-button+))))

(defun %macro-reset (w)
  (setf (macro-form w) (macro-orig w) (macro-steps w) 0 (macro-history w) '())
  (%macro-render w 0))

(defun %macro-next-expandable (w)
  "Move the cursor to the next macro-call position (wrapping)."
  (let* ((tv (macro-view w)) (off (%editor-offset tv))
         (cands (sort (loop for s in (macro-spans w)
                            when (%macro-call-p (cddr s)) collect s)
                      #'< :key #'car)))
    (when cands
      (%macro-set-cursor tv (car (or (find-if (lambda (s) (> (car s) off)) cands)
                                     (first cands))))
      (draw-view tv))))

(defun %macro-copy (w)
  (setf *clipboard* (text-string (macro-view w)))
  (message-box "Expansion copied to the clipboard."
               (logior +mf-information+ +mf-ok-button+)))

(defun %macro-to-editor (w)
  "Open the current expansion in a fresh editor window."
  (when *application*
    (let* ((desk (program-desktop *application*))
           (dw (point-x (view-size desk))) (dh (point-y (view-size desk))))
      (multiple-value-bind (win ed)
          (make-edit-window (make-trect 2 1 (min (- dw 2) 78) (min (- dh 1) 22))
                            :title "Expansion")
        (set-text ed (text-string (macro-view w)))
        (insert desk win) (focus win)))))

(defmethod handle-event ((w tmacro-window) event)
  (when (and (macro-view w) (= (event-type event) +ev-key-down+))
    (let ((ch (event-char-code event)) (k (event-key-code event)) (handled t))
      (cond
        ((or (= k +kb-enter+) (= k +kb-tab+))
         (if (= k +kb-tab+) (%macro-next-expandable w) (%macro-expand-at w)))
        ((plusp ch)
         (let ((raw (code-char ch)))
           (case (char-downcase raw)
             (#\e (%macro-expand-at w))
             (#\m (if (char= raw #\M) (%macro-expand-all w) (%macro-expand-at w :full t)))
             (#\u (%macro-undo w))
             (#\0 (%macro-reset w))
             (#\c (%macro-copy w))
             (#\o (%macro-to-editor w))
             (t (setf handled nil)))))
        (t (setf handled nil)))
      (when handled (clear-event event))))
  (call-next-method))

(defun do-macroexpand (app)
  "Open the interactive macro stepper on a form.  When an editor window is
focused the prompt defaults to the form at the cursor; in the stepper, navigate
with the arrows and expand the macro call under the cursor with `e'."
  (let* ((rv (some-repl app))
         (ew (current-editor-window app))
         (default (when ew
                    (string-trim '(#\Space #\Tab #\Newline)
                                 (or (editor-form-at-point (editor-window-editor ew)) ""))))
         (s (prompt-line "Macroexpand" "Form:" (or default ""))))
    (when s
      (handler-case
          (let* ((pkg (if rv (repl-package rv) *package*))
                 (form (let ((*package* pkg)) (read-from-string s)))
                 (desk (program-desktop app))
                 (dw (point-x (view-size desk))) (dh (point-y (view-size desk)))
                 (w (min 78 (- dw 2))) (h (min 20 (- dh 2)))
                 (win (make-instance 'tmacro-window :form form :orig form :pkg pkg
                                     :bounds (make-trect 0 0 w h)))
                 (vsb (standard-scrollbar win t))
                 (tv (make-instance 'ttext-view :read-only t :highlight t
                                    :bounds (make-trect 1 1 (1- w) (1- h)))))
            (insert win tv) (text-attach-scrollbars tv :vscroll vsb)
            (setf (macro-view win) tv)
            (%macro-render win)
            (move-to win (max 0 (floor (- dw w) 2)) (max 0 (floor (- dh h) 2)))
            (insert desk win) (focus tv))
        (error (e) (err-box e))))))

(defclass tdescribe-window (twindow)
  ((sym :initarg :sym :initform nil :accessor describe-sym)))

(defmethod handle-event ((w tdescribe-window) event)
  (when (and (= (event-type event) +ev-key-down+) (describe-sym w))
    (let ((ch (event-char-code event)))
      (cond
        ((member ch (list (char-code #\g) (char-code #\G)))   ; g: jump to source
         (when *application*
           (handler-case (goto-definition-of *application* (describe-sym w)) (error (e) (err-box e))))
         (clear-event event))
        ((member ch (list (char-code #\i) (char-code #\I)))   ; i: inspect the symbol
         (ignore-errors (repl-inspect (describe-sym w) (princ-to-string (describe-sym w))))
         (clear-event event)))))
  (call-next-method))

(defun describe-named (rv name)
  (handler-case
      (let ((sym (read-in rv name)))
        (show-text-window (format nil "Describe ~a  (g: source  i: inspect)" name)
                          (with-output-to-string (s) (describe sym s))
                          :class 'tdescribe-window :initargs (list :sym sym)))
    (error (e) (err-box e))))

(defun do-describe (rv)
  (let ((s (prompt-line "Describe" "Symbol:" (%point-symbol)))) (when (and rv s) (describe-named rv s))))

(defun do-documentation (rv)
  (let ((s (prompt-line "Documentation" "Symbol:" (%point-symbol))))
    (when (and rv s)
      (handler-case
          (let ((sym (read-in rv s)))
            (show-text-window (format nil "Documentation ~a" s)
              (with-output-to-string (o)
                (dolist (ty '(function variable type structure setf compiler-macro))
                  (let ((d (ignore-errors (documentation sym ty))))
                    (when d (format o "~(~a~):~%~a~%~%" ty d))))
                (when (zerop (file-position o)) (format o "(no documentation)")))))
        (error (e) (err-box e))))))

(defun do-disassemble (rv)
  (let ((s (prompt-line "Disassemble" "Function:" (%point-symbol))))
    (when (and rv s)
      (handler-case
          (show-text-window (format nil "Disassemble ~a" s)
            (with-output-to-string (o)
              (let ((*standard-output* o)) (disassemble (read-in rv s)))))
        (error (e) (err-box e))))))

(defun do-apropos (rv)
  (let ((s (prompt-line "Apropos" "Substring:" (%point-symbol))))
    (when (and rv s)
      (let ((names (sort (mapcar #'prin1-to-string (apropos-list s)) #'string<)))
        (if (null names)
            (message-box (format nil "Nothing matches \"~a\"." s)
                         (logior +mf-information+ +mf-ok-button+))
            ;; multi-action picker: Describe (default) or Inspect the symbol
            (multiple-value-bind (chosen cmd)
                (pick-with-inspect (format nil "Apropos \"~a\" (~d)" s (length names)) names
                                   :ok "~D~escribe")
              (when chosen
                (cond ((eql cmd +cm-ok+) (describe-named rv chosen))
                      ((eql cmd +cm-pick-inspect+)
                       (handler-case (repl-inspect (read-in rv chosen) chosen)
                         (error (e) (err-box e))))))))))))

(defun do-inspect-expr (rv)
  (let ((s (prompt-line "Inspect" "Expression:" (%point-symbol))))
    (when (and rv s)
      (handler-case (repl-inspect (eval (read-in rv s)) s)
        (error (e) (err-box e))))))

;;; A list picker with three actions -- OK (default), Inspect, Cancel -- used by
;;; the Packages and Class browsers.  OK / Enter run the primary action on the
;;; selection; Inspect opens it in an Inspector window.
(defclass tlist-pick-dialog (tdialog) ())

(defmethod handle-event ((d tlist-pick-dialog) event)
  (cond
    ((and (= (event-type event) +ev-command+)
          (member (event-command event) (list +cm-pick-inspect+ +cm-pick-extra+ +cm-pick-extra2+))
          (logtest (view-state d) +sf-modal+))
     (end-modal d (event-command event)) (clear-event event))
    (t (call-next-method))))

(defun pick-with-inspect (title items &key (ok "~O~K") extra extra2 select)
  "Modal picker over (sorted) ITEMS with OK / Inspect [/ EXTRA [/ EXTRA2]] /
Cancel buttons.  EXTRA / EXTRA2 are labels for extra actions (returning
+cm-pick-extra+ / +cm-pick-extra2+).  SELECT preselects that item string.
Returns (values selected-item end-command)."
  (when (and *application* items)
    (let* ((desk (program-desktop *application*))
           (nbtn (+ 3 (if extra 1 0) (if extra2 1 0)))
           (w (max 58 (+ 4 (* nbtn 13)))) (h 18)
           (d (make-instance 'tlist-pick-dialog :title title :bounds (make-trect 0 0 w h)))
           (vsb (standard-scrollbar d t))
           (lb (make-instance 'tsorted-list-box :items items :command +cm-ok+
                              :bounds (make-trect 1 1 (1- w) (- h 3)))))
      (insert d lb) (attach-scrollbars lb :vscroll vsb)
      (when select
        (dotimes (i (list-count lb))
          (when (string= (list-item lb i) select) (list-focus-item lb i) (return))))
      (let ((x 2))
        (flet ((btn (label cmd &optional default)
                 (insert d (make-button (make-trect x (- h 3) (+ x 11) (- h 1)) label cmd default))
                 (incf x 13)))
          (btn ok +cm-ok+ t)
          (btn "~I~nspect" +cm-pick-inspect+)
          (when extra  (btn extra  +cm-pick-extra+))
          (when extra2 (btn extra2 +cm-pick-extra2+))
          (btn "~C~ancel" +cm-cancel+)))
      (move-to d (max 0 (floor (- (point-x (view-size desk)) w) 2))
               (max 0 (floor (- (point-y (view-size desk)) h) 2)))
      (focus lb)
      (let ((cmd (exec-view desk d)))
        (values (and (plusp (list-count lb)) (list-item lb (list-focused lb))) cmd)))))

(defun pkg-switch (rv p)
  (when (and rv p)
    (setf (repl-package rv) p)
    (repl-print rv (format nil "~%; switched to package ~a~%" (package-name p)))
    (tvision::repl-fresh-prompt rv)
    (draw-view rv)))

(defun do-package-symbols (rv pkg)
  "Browse PKG's exported symbols: Describe (default), Inspect, or Goto definition."
  (let ((names (sort (let (acc) (do-external-symbols (s pkg acc) (push (prin1-to-string s) acc)))
                     #'string<)))
    (if (null names)
        (message-box (format nil "~a exports no symbols." (package-name pkg))
                     (logior +mf-information+ +mf-ok-button+))
        (multiple-value-bind (chosen cmd)
            (pick-with-inspect (format nil "~a — exported (~d)" (package-name pkg) (length names))
                               names :ok "~D~escribe" :extra "~G~oto")
          (when chosen
            (let ((sym (ignore-errors (let ((*package* pkg)) (read-from-string chosen)))))
              (cond
                ((eql cmd +cm-ok+)
                 (show-text-window (format nil "Describe ~a" chosen)
                   (with-output-to-string (s)
                     (let ((*package* pkg)) (ignore-errors (describe (read-from-string chosen) s))))))
                ((and (eql cmd +cm-pick-inspect+) sym) (repl-inspect sym chosen))
                ((and (eql cmd +cm-pick-extra+) sym)
                 (handler-case (goto-definition-of *application* sym) (error (e) (err-box e)))))))))))

(defun do-packages (rv)
  (let ((cur (and rv (package-name (repl-package rv)))))
    (multiple-value-bind (name cmd)
        (pick-with-inspect "Packages"
                           (sort (mapcar #'package-name (list-all-packages)) #'string<)
                           :extra "S~y~mbols" :select cur)
      (let ((p (and name (find-package name))))
        (when p
          (cond ((eql cmd +cm-ok+) (pkg-switch rv p))
                ((eql cmd +cm-pick-inspect+)
                 (repl-inspect p (format nil "package ~a" (package-name p))))
                ((eql cmd +cm-pick-extra+) (do-package-symbols rv p))))))))

(defun do-window-list (app)
  "Pop up a list of every open window (Alt-0); Enter/OK raises and focuses it."
  (let* ((desk (program-desktop app))
         (wins (remove-if-not (lambda (v) (typep v 'twindow)) (desktop-windows desk)))
         (cur (group-current desk)))
    (if (null wins)
        (message-box "No windows are open." (logior +mf-information+ +mf-ok-button+))
        (let* ((labels (loop for w in wins
                             for n = (window-number w)
                             collect (format nil "~:[  ~;> ~]~@[~d. ~]~a"
                                             (eq w cur) (and (plusp n) n) (window-title w))))
               (sel (choose-index "Window list" labels :start (max 0 (or (position cur wins) 0)))))
          (when sel (set-current desk (nth sel wins) :normal-select))))))

(defun %load-system-async (rv name force)
  "Load (or force-reload) system NAME on the worker, streaming output to a window."
  (repl-print rv (format nil "~%; ~:[loading~;reloading~] system ~a ...~%" force name))
  (repl-call-on-worker rv
    (lambda ()
      (let ((out (with-output-to-string (o)
                   (handler-case
                       (let ((*standard-output* o) (*error-output* o))
                         (asdf:load-system name :force force))
                     (error (e) (format o ";; ~a~%" e))))))
        (run-on-ui
         (lambda ()
           (show-text-window (format nil "~:[Load~;Reload~] system ~a" force name)
                             (if (plusp (length out)) out (format nil "Loaded ~a." name)))))))))

(defun do-systems (rv)
  "Browse ASDF systems (* = already loaded).  Load (default), Inspect the system
object (its dependencies/components), or force-Reload."
  (multiple-value-bind (fcmd filter) (input-box "ASDF Systems" "Filter (blank = all):" "" 60)
    (when (= fcmd +cm-ok+)
      (let* ((loaded (ignore-errors (asdf:already-loaded-systems)))
             (all (sort (copy-list (asdf:registered-systems)) #'string<))
             (names (if (plusp (length (string-trim " " filter)))
                        (remove-if-not (lambda (n) (search (string-downcase (string-trim " " filter))
                                                           (string-downcase n))) all)
                        all))
             (labels (mapcar (lambda (n) (format nil "~:[  ~;* ~]~a" (member n loaded :test #'string=) n))
                             names)))
        (when labels
          (multiple-value-bind (picked cmd)
              (pick-with-inspect "ASDF Systems  (* = loaded)" labels
                                 :ok "~L~oad" :extra "~R~eload" :extra2 "~U~nload")
            (let ((chosen (and picked (string-left-trim '(#\* #\Space) picked))))
              (when chosen
                (cond
                  ((eql cmd +cm-pick-inspect+)
                   (let ((sys (ignore-errors (asdf:find-system chosen nil))))
                     (if sys (repl-inspect sys (format nil "system ~a" chosen))
                         (message-box "Could not find that system object."
                                      (logior +mf-information+ +mf-ok-button+)))))
                  ((eql cmd +cm-pick-extra2+)
                   ;; ASDF has no true unload; clear-system forgets it so the next
                   ;; load fully recompiles (definitions stay until redefined)
                   (handler-case
                       (progn (asdf:clear-system chosen)
                              (when rv (repl-print rv (format nil "~%; cleared ASDF state for ~a (next load recompiles)~%" chosen))
                                    (tvision::repl-fresh-prompt rv) (draw-view rv)))
                     (error (e) (err-box e))))
                  ((null rv)
                   (message-box "No REPL open (needed to load on a worker thread)."
                                (logior +mf-information+ +mf-ok-button+)))
                  (t (%load-system-async rv chosen (eql cmd +cm-pick-extra+))))))))))))

(defun %slot-label (s initformp)
  "Label for slot definition S: name, declared type, and (for direct slots) its
initform."
  (let ((name (sb-mop:slot-definition-name s))
        (type (ignore-errors (sb-mop:slot-definition-type s)))
        (initf (and initformp (ignore-errors (sb-mop:slot-definition-initform s)))))
    (format nil "~a~@[ : ~(~a~)~]~@[ = ~a~]"
            name
            (and type (not (eq type t)) type)
            (and initf (let ((*print-length* 4) (*print-level* 2)) (prin1-to-string initf))))))

(defun %subclass-tree (class depth budget)
  "Outline node for CLASS, recursively expandable to its subclasses, to DEPTH.
BUDGET is a (count) cell capping total nodes so inspecting a class with a vast
subtree (e.g. T) can't explode; children are collapsed until the user drills in."
  (let* ((subs (and (plusp depth) (plusp (car budget))
                    (ignore-errors (sb-mop:class-direct-subclasses class))))
         (kids (loop for c in subs
                     while (plusp (car budget))
                     do (decf (car budget))
                     collect (%subclass-tree c (1- depth) budget))))
    (make-outline-node (princ-to-string (class-name class)) kids)))

(defun class-outline (class)
  "A curated structural view of CLASS: superclasses, a recursive subclass tree,
and direct vs inherited slots (with declared types and direct initforms)."
  (flet ((cls-nodes (cs) (mapcar (lambda (c) (make-outline-node (princ-to-string (class-name c)) nil)) cs))
         (slot-nodes (ss initformp)
           (mapcar (lambda (s) (make-outline-node (%slot-label s initformp) nil)) ss))
         (grp (text kids) (let ((n (make-outline-node text kids))) (setf (outline-node-expanded n) t) n)))
    (let* ((supers (ignore-errors (sb-mop:class-direct-superclasses class)))
           (subs (ignore-errors (sb-mop:class-direct-subclasses class)))
           (directs (ignore-errors (sb-mop:class-direct-slots class)))
           (finalized (ignore-errors (sb-mop:class-finalized-p class)))
           (effective (and finalized (ignore-errors (sb-mop:class-slots class))))
           (dnames (mapcar #'sb-mop:slot-definition-name directs))
           (inherited (remove-if (lambda (s) (member (sb-mop:slot-definition-name s) dnames)) effective))
           (kids (list (grp (format nil "Superclasses (~d)" (length supers)) (cls-nodes supers))
                       (grp (format nil "Subclasses (~d)" (length subs))
                            (let ((budget (list 400)))
                              (mapcar (lambda (c) (%subclass-tree c 6 budget)) subs)))
                       (grp (format nil "Direct slots (~d)" (length directs)) (slot-nodes directs t)))))
      (when inherited
        (setf kids (append kids (list (grp (format nil "Inherited slots (~d)" (length inherited))
                                           (slot-nodes inherited nil))))))
      (let ((node (make-outline-node (format nil "Class ~a" (class-name class)) kids)))
        (setf (outline-node-expanded node) t)
        node))))

(defun class-list ()
  "Sorted ((name-string . class)) for every class reachable from the class T."
  (let ((seen (make-hash-table :test 'eq)) (acc '()))
    (labels ((walk (c)
               (unless (gethash c seen)
                 (setf (gethash c seen) t)
                 (let ((n (class-name c)))
                   (when (symbolp n) (push (cons (prin1-to-string n) c) acc)))
                 (dolist (s (ignore-errors (sb-mop:class-direct-subclasses c))) (walk s)))))
      (walk (find-class t)))
    (sort (delete-duplicates acc :key #'car :test #'string=) #'string< :key #'car)))

(defun do-class-methods (app class)
  "List the methods that specialise on CLASS; Enter jumps to the generic
function's definition."
  (let* ((methods (ignore-errors (sb-mop:specializer-direct-methods class)))
         (labels (mapcar (lambda (m)
                           (format nil "~(~a~) ~a"
                                   (sb-mop:generic-function-name (sb-mop:method-generic-function m))
                                   (method-label m)))
                         methods)))
    (if (null methods)
        (message-box (format nil "No methods specialise on ~a." (class-name class))
                     (logior +mf-information+ +mf-ok-button+))
        (let ((sel (choose-index (format nil "Methods on ~a (~d)" (class-name class) (length methods))
                                 labels)))
          (when sel
            (let ((gf (sb-mop:method-generic-function (nth sel methods))))
              (handler-case (goto-definition-of app (sb-mop:generic-function-name gf))
                (error (e) (err-box e)))))))))

(defun do-classes (rv app)
  "Browse every class.  OK / Enter jumps to the selected class's definition;
Inspect opens it in an Inspector window; Methods lists its methods."
  (let* ((*package* (if rv (repl-package rv) *package*))   ; names as the listener sees them
         (alist (class-list)))
    (multiple-value-bind (name cmd)
        (pick-with-inspect "Classes" (mapcar #'car alist) :ok "~G~oto def" :extra "~M~ethods")
      (let ((class (and name (cdr (assoc name alist :test #'string=)))))
        (when class
          (cond
            ((eql cmd +cm-ok+)
             (handler-case (goto-definition-of app (class-name class)) (error (e) (err-box e))))
            ((eql cmd +cm-pick-inspect+)
             ;; a curated structural view (supers/subs/slots with types), more
             ;; useful for a class than the raw metaobject; no finalization forced
             (open-outline-window (format nil "Class ~a — supers / subs / slots" name)
                                  (list (class-outline class))))
            ((eql cmd +cm-pick-extra+) (do-class-methods app class))))))))

;;; --- navigation: go-to-definition, cross-reference, function browser -------

(defun symbol-definitions (sym)
  "List of (type pathname char-offset) source locations for SYM."
  (loop for type in '(:function :generic-function :macro :variable :class
                      :structure :condition :method :compiler-macro
                      :setf-expander :type)
        append (ignore-errors
                (loop for src in (sb-introspect:find-definition-sources-by-name sym type)
                      for path = (sb-introspect:definition-source-pathname src)
                      when path
                      collect (list type (namestring path)
                                    (sb-introspect:definition-source-character-offset src))))))

(defvar *line-index-cache* (make-hash-table :test 'equal)
  "Caches a file's newline character offsets keyed by (truename . write-date),
so repeated %OFFSET-TO-LINE calls -- goto-def / xref over many hits in one file
-- don't re-scan the file each time.  Stale entries are replaced on write-date
change; the table only grows with the number of distinct files visited.")

(defun %newline-offsets (path)
  "Sorted SIMPLE-VECTOR of the character offsets just past each #\\Newline in
PATH (cached by write-date), or NIL if PATH can't be read."
  (let ((key (ignore-errors (cons (namestring (truename path)) (file-write-date path)))))
    (or (and key (gethash key *line-index-cache*))
        (let ((offs (ignore-errors
                     (with-open-file (s path)
                       (let ((v (make-array 256 :adjustable t :fill-pointer 0)) (i 0))
                         (loop for c = (read-char s nil nil) while c do
                           (incf i)
                           (when (char= c #\Newline) (vector-push-extend i v)))
                         (coerce v 'simple-vector))))))
          (when (and key offs) (setf (gethash key *line-index-cache*) offs))
          offs))))

(defun %offset-to-line (path offset)
  "1-based line number containing character OFFSET in PATH (file scanned once
and cached; subsequent lookups are a binary search over its newline offsets)."
  (let ((offs (%newline-offsets path)) (off (or offset 0)))
    (if offs
        ;; line = 1 + (number of newline-ends at or before OFF)
        (let ((lo 0) (hi (length offs)))
          (loop while (< lo hi)
                for mid = (ash (+ lo hi) -1)
                do (if (<= (svref offs mid) off) (setf lo (1+ mid)) (setf hi mid)))
          (1+ lo))
        1)))

;;; --- go-to-definition pop-back stack (SLIME's M-. / M-,) -------------------

(defvar *nav-stack* '()
  "Stack of (WINDOW LINE COL) locations pushed before each source jump, so
`pop back' (Alt-,) can return to where you came from.")

(defun %nav-push (app)
  "Record the currently focused window (and its cursor, for editors) so a later
pop-back can return there."
  (let ((w (group-current (program-desktop app))))
    (when (typep w 'twindow)
      (push (list w
                  (and (typep w 'teditor-window) (text-cur-line (editor-window-editor w)))
                  (and (typep w 'teditor-window) (text-cur-col (editor-window-editor w))))
            *nav-stack*))))

(defun do-nav-back (app)
  "Pop the navigation stack and return to the most recent still-open location."
  (let ((desk (program-desktop app)))
    (loop
      (let ((e (pop *nav-stack*)))
        (cond
          ((null e)
           (message-box "Nothing to go back to." (logior +mf-information+ +mf-ok-button+))
           (return))
          ((member (first e) (desktop-windows desk))    ; still open?
           (destructuring-bind (w line col) e
             (set-current desk w :normal-select)
             (when (and line (typep w 'teditor-window))
               (let ((ed (editor-window-editor w)))
                 (setf (text-cur-line ed) (min line (1- (line-count ed)))
                       (text-cur-col ed) (min col (length (nth-line ed (min line (1- (line-count ed)))))))
                 (ensure-visible ed) (draw-view ed))))
           (return)))))))      ; window was closed -> skip it, try the next entry

(defvar *source-root* nil
  "Optional directory to search for source files whose recorded (build-time)
path no longer exists -- so a relocated binary can still jump to its sources.")

(defun %source-roots ()
  "Candidate directories to look for relocated sources under (most specific
first): the user's *SOURCE-ROOT*, the executable's directory, the current
directory, and -- when developing from source -- the tvision system directory."
  (remove-duplicates
   (remove nil
           (list (and *source-root* (ignore-errors (uiop:ensure-directory-pathname *source-root*)))
                 (ignore-errors (uiop:pathname-directory-pathname sb-ext:*core-pathname*))
                 (ignore-errors (uiop:getcwd))
                 (ignore-errors (asdf:system-source-directory :tvision))))
   :test #'equal))

(defun %path-suffixes (path)
  "Relative pathnames built from progressively longer trailing directory
components of PATH (shortest first): file.lisp, dir/file.lisp, ..."
  (let* ((p (pathname path)) (dirs (rest (pathname-directory p))))
    (loop for k from 0 to (length dirs)
          for tail = (last dirs k)
          collect (make-pathname :directory (and tail (cons :relative tail))
                                 :name (pathname-name p) :type (pathname-type p)))))

(defun %resolve-source-path (path)
  "A probeable pathname for PATH: itself when it exists, else the first match for
a trailing portion of it under the known source roots (relocation), or NIL."
  (when path
    (let ((p (pathname path)))
      (if (probe-file p) p
          (loop for rel in (%path-suffixes p)
                thereis (loop for root in (%source-roots)
                              for cand = (ignore-errors (merge-pathnames rel root))
                              when (and cand (probe-file cand)) return cand))))))

(defun goto-source (app type path offset)
  (declare (ignore type))
  (let ((resolved (%resolve-source-path path)))
    (if resolved
        (let* ((desk (program-desktop app))
               (dw (point-x (view-size desk))) (dh (point-y (view-size desk))))
          (%nav-push app)                       ; remember where we jumped from
          (multiple-value-bind (w ed)
              (make-edit-window (make-trect 2 1 (min (- dw 2) 84) (min (- dh 1) 26))
                                :title (file-namestring resolved) :filename resolved)
            (insert desk w)
            (when offset (text-goto ed (%offset-to-line resolved offset) 0))
            (focus w)))
        (message-box (format nil "No source file:~%~a~@[~%~%Set *source-root* to locate relocated sources.~]"
                             path (and path (not (probe-file (pathname path)))))
                     (logior +mf-error+ +mf-ok-button+)))))

;;; A shared cross-reference / definitions results window: a sortable table of
;;; (kind, where, file, line) source hits.  Enter on a row jumps to that
;;; location.  Used by Go-to-definition (multiple matches), Who-calls and
;;; Who-references, so they all share one navigable result list instead of a
;;; one-shot picker that exits after a single jump.
(defclass txref-window (twindow)
  ((app   :initarg :app   :initform nil :accessor xref-app)
   (table :initarg :table :initform nil :accessor xref-table)))

(defun %xref-columns ()
  (vector (make-table-column "Kind" 9 (lambda (r) (string-downcase (princ-to-string (getf r :kind)))))
          (make-table-column "Where" 28 (lambda (r) (getf r :label)))
          (make-table-column "File" 22 (lambda (r) (getf r :file)))
          (make-table-column "Line" 6 (lambda (r) (getf r :line)) :numeric t)))

(defun %xref-goto (w)
  (let ((row (and (xref-table w) (table-selected-row (xref-table w)))))
    (when (and row (xref-app w) (getf row :path))
      (goto-source (xref-app w) (getf row :kind) (getf row :path) (getf row :offset)))))

(defmethod handle-event ((w txref-window) event)
  (cond
    ((and (= (event-type event) +ev-broadcast+)
          (= (event-command event) +cm-list-item-selected+)
          (xref-table w))
     (%xref-goto w) (clear-event event))
    (t (call-next-method))))

(defun show-xref-results (app title rows)
  "Open a TXREF-WINDOW over ROWS (each a plist :kind :label :file :path :offset
:line).  Enter on a row jumps to its source.  With no jumpable rows, ROWS is
still shown (entries without a source location simply aren't navigable)."
  (when (and app rows)
    (let* ((desk (program-desktop app))
           (dw (point-x (view-size desk))) (dh (point-y (view-size desk)))
           (w (min 74 (- dw 2))) (h (min 20 (- dh 2)))
           (win (make-instance 'txref-window :app app
                               :title (format nil "~a  (Enter: jump to source)" title)
                               :bounds (make-trect 0 0 w h)))
           (vsb (standard-scrollbar win t))
           (tbl (make-instance 'ttable-view :columns (%xref-columns) :rows rows
                               :sort-col 2 :sort-asc t
                               :bounds (make-trect 1 1 (1- w) (1- h)))))
      (insert win tbl)
      (attach-scrollbars tbl :vscroll vsb)
      (setf (xref-table win) tbl)
      (move-to win (max 0 (floor (- dw w) 2)) (max 0 (floor (- dh h) 2)))
      (insert desk win)
      (focus tbl))))

(defun %def-rows (sym defs)
  "Result rows for SYMBOL-DEFINITIONS triples (type path offset) of SYM."
  (loop for (type path offset) in defs
        collect (list :kind type :label (princ-to-string sym)
                      :file (file-namestring path) :path path :offset offset
                      :line (%offset-to-line path offset))))

(defun %xref-rows (kind entries)
  "Result rows from SB-INTROSPECT:WHO-CALLS / WHO-REFERENCES ENTRIES, each a
 (name . definition-source); the source location is the call/reference site."
  (let ((rows (loop for e in entries
                    for name = (car e)
                    for src = (cdr e)
                    for path = (ignore-errors (sb-introspect:definition-source-pathname src))
                    for offset = (ignore-errors (sb-introspect:definition-source-character-offset src))
                    collect (list :kind kind :label (princ-to-string name)
                                  :file (if path (file-namestring path) "")
                                  :path (and path (namestring path)) :offset offset
                                  :line (if path (%offset-to-line (namestring path) offset) 0)))))
    (remove-duplicates rows :test #'equal
                       :key (lambda (r) (list (getf r :label) (getf r :file) (getf r :line))))))

(defun goto-definition-of (app sym)
  (let ((defs (symbol-definitions sym)))
    (cond
      ((null defs)
       (message-box (format nil "No source location for ~a." sym)
                    (logior +mf-information+ +mf-ok-button+)))
      ((= 1 (length defs)) (apply #'goto-source app (first defs)))
      (t (show-xref-results app (format nil "Definitions of ~a (~d)" sym (length defs))
                            (%def-rows sym defs))))))

(defun do-goto-definition (rv app)
  (let ((s (prompt-line "Go to definition" "Symbol:" (%point-symbol))))
    (when (and rv s)
      (handler-case (goto-definition-of app (read-in rv s)) (error (e) (err-box e))))))

(defun do-xref (rv app kind)
  (let ((s (prompt-line (format nil "Who ~(~a~)" kind) "Symbol:" (%point-symbol))))
    (when (and rv s)
      (handler-case
          (let* ((sym (read-in rv s))
                 (*package* (repl-package rv))   ; print caller names as the listener sees them
                 (entries (ecase kind
                            (:calls (sb-introspect:who-calls sym))
                            (:references (sb-introspect:who-references sym))
                            (:binds (sb-introspect:who-binds sym))
                            (:sets (sb-introspect:who-sets sym))
                            (:macroexpands (sb-introspect:who-macroexpands sym))))
                 (rows (%xref-rows kind entries)))
            (if (null rows)
                (message-box (format nil "Nothing ~(~a~) ~a." kind s)
                             (logior +mf-information+ +mf-ok-button+))
                (show-xref-results app (format nil "Who ~(~a~) ~a (~d)" kind s (length rows)) rows)))
        (error (e) (err-box e))))))

(defun %traced-symbols ()
  "The list of currently traced function names (symbols)."
  (remove-if-not #'symbolp (eval '(trace))))

(defun choose-checklist (title labels &key all)
  "Modal multi-select over LABELS (check boxes).  Return a list of checked
indices, or NIL when cancelled.  When ALL, every box starts checked."
  (when (and *application* labels)
    (let* ((n (length labels))
           (desk (program-desktop *application*))
           (w 60) (h (min (+ n 6) (max 9 (- (point-y (view-size desk)) 2))))
           (d (make-instance 'tdialog :title title :bounds (make-trect 0 0 w h)))
           (cb (make-instance 'tcheck-boxes :labels labels
                              :bounds (make-trect 2 1 (- w 2) (+ 1 n)))))
      (when all (setf (cluster-value cb) (1- (ash 1 n))))
      (insert d cb)
      (insert d (make-button (make-trect (- w 24) (- h 3) (- w 14) (- h 1)) "~O~K" +cm-ok+ t))
      (insert d (make-button (make-trect (- w 12) (- h 3) (- w 2) (- h 1)) "Cancel" +cm-cancel+))
      (move-to d (max 0 (floor (- (point-x (view-size desk)) w) 2))
               (max 0 (floor (- (point-y (view-size desk)) h) 2)))
      (focus cb)
      (when (= (exec-view desk d) +cm-ok+)
        (loop with v = (cluster-value cb) for i below n when (logbitp i v) collect i)))))

(defun do-trace (rv)
  "TRACE a function (with options) or UNTRACE it if already traced.  Trace output
appears in the REPL, indented by call depth (SBCL's default)."
  (when rv
    (let ((s (prompt-line "Trace" "Function (toggles):" (%point-symbol))))
      (when s
        (handler-case
            (let ((sym (read-in rv s)))
              (cond
                ((member sym (%traced-symbols))
                 (eval `(untrace ,sym))
                 (repl-print rv (format nil "~%; untraced ~s~%" sym)))
                (t
                 (let ((mode (choose-index "Trace options"
                                           '("Normal" "Break on entry" "Conditional...")
                                           :start 0))
                       (note nil))
                   (when mode
                     (ecase mode
                       (0 (eval `(trace ,sym)) (setf note ""))
                       (1 (eval `(trace ,sym :break t)) (setf note " (break on entry)"))
                       (2 (let ((c (prompt-line "Trace condition"
                                                "Form (true => trace this call):" "t")))
                            (when c (eval `(trace ,sym :condition ,(read-in rv c)))
                                  (setf note " (conditional)")))))
                     (when note
                       (repl-print rv (format nil "~%; tracing ~s~a~%" sym note)))))))
              (tvision::repl-fresh-prompt rv) (draw-view rv))
          (error (e) (err-box e)))))))

(defun do-break-on-entry (rv)
  "Set a breakpoint: TRACE a function with :break so its next call stops in the
debugger (with the navigable backtrace and frame ops); untrace it to clear."
  (when rv
    (let ((s (prompt-line "Break on entry" "Function (its next call breaks):" (%point-symbol))))
      (when s
        (handler-case
            (let ((sym (read-in rv s)))
              (eval `(trace ,sym :break t))
              (repl-print rv (format nil "~%; break-on-entry armed on ~s (untrace to clear)~%" sym))
              (tvision::repl-fresh-prompt rv) (draw-view rv))
          (error (e) (err-box e)))))))

(defun do-trace-package (rv)
  "TRACE every exported function of a package (macros/special-operators skipped)."
  (when rv
    (let ((p (prompt-line "Trace package" "Package (traces its exported functions):"
                          (package-name (repl-package rv)))))
      (when p
        (handler-case
            (let* ((pkg (or (find-package (string-trim " " p))
                            (error "No such package: ~a" p)))
                   (syms (let (acc)
                           (do-external-symbols (s pkg acc)
                             (when (and (fboundp s) (not (macro-function s))
                                        (not (special-operator-p s)))
                               (push s acc))))))
              (dolist (s syms) (ignore-errors (eval `(trace ,s))))
              (repl-print rv (format nil "~%; tracing ~d function~:p in ~a~%"
                                     (length syms) (package-name pkg)))
              (tvision::repl-fresh-prompt rv) (draw-view rv))
          (error (e) (err-box e)))))))

(defvar *trace-snapshots* '()
  "Named saved sets of traced functions: (name . (symbol ...)).")

(defun do-trace-snapshots (rv)
  "Save the current traced-function set under a name, or restore a saved one
 (untraces everything, then traces the snapshot's functions)."
  (when rv
    (let* ((choices (cons "Save current set..."
                          (mapcar (lambda (s) (format nil "Restore: ~a (~d)" (car s) (length (cdr s))))
                                  *trace-snapshots*)))
           (sel (choose-index "Trace snapshots" choices)))
      (when sel
        (handler-case
            (cond
              ((zerop sel)                           ; save
               (let ((name (prompt-line "Save trace snapshot" "Name:" "")))
                 (when name
                   (setf *trace-snapshots*
                         (cons (cons name (%traced-symbols))
                               (remove name *trace-snapshots* :key #'car :test #'string=)))
                   (repl-print rv (format nil "~%; saved trace snapshot ~a (~d function~:p)~%"
                                          name (length (%traced-symbols))))
                   (tvision::repl-fresh-prompt rv) (draw-view rv))))
              (t                                     ; restore the (1- sel)th snapshot
               (let* ((snap (nth (1- sel) *trace-snapshots*)) (syms (cdr snap)))
                 (eval '(untrace))
                 (dolist (s syms) (ignore-errors (eval `(trace ,s))))
                 (repl-print rv (format nil "~%; restored snapshot ~a: tracing ~d function~:p~%"
                                        (car snap) (length syms)))
                 (tvision::repl-fresh-prompt rv) (draw-view rv))))
          (error (e) (err-box e)))))))

(defun do-untrace-all (rv)
  "Show the traced functions as a checklist (all checked); untrace the ones kept
checked when you confirm."
  (when rv
    (let ((traced (%traced-symbols)))
      (if (null traced)
          (message-box "No functions are traced." (logior +mf-information+ +mf-ok-button+))
          (let ((picked (choose-checklist "Untrace functions"
                                          (mapcar (lambda (s) (format nil "~s" s)) traced)
                                          :all t)))
            (when picked
              (dolist (i picked) (eval `(untrace ,(nth i traced))))
              (repl-print rv (format nil "~%; untraced ~d function~:p~%" (length picked)))
              (tvision::repl-fresh-prompt rv) (draw-view rv)))))))

;;; --- call-tree tracing: watch functions and record a navigable call tree ---
;;; Distinct from cl:trace (which dumps indented text to the REPL): watched
;;; functions are encapsulated so every call/return is recorded with the live
;;; argument and result objects, shown as a tree whose rows are presentations
;;; (Enter inspects the args / result).

(defvar *ct-log* '())                       ; rows, most-recent first
(defvar *ct-count* 0)
(defvar *ct-depth* 0)                        ; dynamic call depth (per thread)
(defvar *ct-watched* '())                    ; watched symbols
(defparameter *ct-limit* 4000)
(defvar *ct-lock* (sb-thread:make-mutex :name "tvlisp-calltree"))

(defun %ct-record (row)
  (sb-thread:with-mutex (*ct-lock*)
    (push row *ct-log*) (incf *ct-count*)
    (when (> *ct-count* (* 2 *ct-limit*))    ; trim rarely (amortized O(1))
      (setf *ct-log* (subseq *ct-log* 0 *ct-limit*) *ct-count* *ct-limit*))))

(defun %ct-snapshot () (sb-thread:with-mutex (*ct-lock*) (reverse *ct-log*)))
(defun %ct-clear () (sb-thread:with-mutex (*ct-lock*) (setf *ct-log* '() *ct-count* 0)))

(defun %ct-watch (sym)
  "Encapsulate SYM so its calls/returns are recorded into the call-tree log."
  (unless (member sym *ct-watched*)
    (sb-int:encapsulate
     sym 'tvlisp-calltree
     (lambda (fn &rest args)
       (let ((d *ct-depth*))
         (%ct-record (list :call d sym (copy-list args)))
         (let ((*ct-depth* (1+ d)))
           (handler-case
               (let ((vals (multiple-value-list (apply fn args))))
                 (%ct-record (list :return d sym vals))
                 (values-list vals))
             (serious-condition (c) (%ct-record (list :error d sym c)) (error c)))))))
    (push sym *ct-watched*)))

(defun %ct-unwatch (sym)
  (when (member sym *ct-watched*)
    (ignore-errors (sb-int:unencapsulate sym 'tvlisp-calltree))
    (setf *ct-watched* (remove sym *ct-watched*))))

(defun %ct-row-label (row)
  (destructuring-bind (kind depth name payload) row
    (let ((ind (make-string (* 2 (min depth 24)) :initial-element #\Space))
          (*print-length* 4) (*print-level* 2) (*print-pretty* nil) (*print-readably* nil))
      (flet ((pr (x) (handler-case (prin1-to-string x) (error () "#<?>"))))
        (case kind
          (:call   (format nil "~a› (~(~a~)~{ ~a~})" ind name (mapcar #'pr payload)))
          (:return (format nil "~a‹ ~(~a~) ⇒ ~{~a~^, ~}" ind name
                           (or (mapcar #'pr payload) '("; no values"))))
          (:error  (format nil "~a✗ ~(~a~) signalled ~a" ind name (pr payload))))))))

(defclass tcalltree-window (twindow)
  ((app  :initarg :app :initform nil :accessor ct-app)
   (rows :initform nil :accessor ct-rows)
   (lb   :initarg :lb  :initform nil :accessor ct-lb))
  (:documentation "A navigable call tree from watched functions; Enter inspects a
row's arguments or result; `a' adds a watch, `u' removes one, `c' clears, `r'
refreshes."))

(defun %ct-refresh (w)
  (setf (ct-rows w) (%ct-snapshot))
  (when (ct-lb w)
    (list-set-items (ct-lb w) (or (mapcar #'%ct-row-label (ct-rows w))
                                  (list "(no calls yet -- `a' to watch a function)"))))
  (setf (window-title w)
        (format nil "Call tree — ~d watched, ~d call~:p  (Enter:inspect a:watch u:unwatch c:clear r:refresh)"
                (length *ct-watched*) (length (ct-rows w))))
  (draw-view w))

(defun %ct-inspect-row (w)
  (let* ((rows (ct-rows w)) (lb (ct-lb w))
         (row (and lb rows (nth (list-focused lb) rows))))
    (when row
      (destructuring-bind (kind depth name payload) row
        (declare (ignore depth))
        (case kind
          (:call   (repl-inspect payload (format nil "args of ~(~a~)" name)))
          (:return (repl-inspect (if (= 1 (length payload)) (first payload) payload)
                                 (format nil "result of ~(~a~)" name)))
          (:error  (repl-inspect payload (format nil "error in ~(~a~)" name))))))))

(defmethod handle-event ((w tcalltree-window) event)
  (cond
    ((and (= (event-type event) +ev-broadcast+)
          (= (event-command event) +cm-list-item-selected+) (ct-lb w))
     (%ct-inspect-row w) (clear-event event))
    ((and (= (event-type event) +ev-key-down+) (plusp (event-char-code event)))
     (case (char-downcase (code-char (event-char-code event)))
       (#\a (let ((s (and (ct-app w)
                          (prompt-line "Watch function" "Function to add to the call tree:"
                                       (%point-symbol)))))
              (when (and s (plusp (length (string-trim " " s))))
                (handler-case (progn (%ct-watch (read-in (some-repl (ct-app w)) s)) (%ct-refresh w))
                  (error (e) (err-box e)))))
            (clear-event event))
       (#\u (let ((picked (and *ct-watched*
                               (choose-checklist "Unwatch functions"
                                                 (mapcar (lambda (s) (format nil "~s" s)) *ct-watched*)))))
              (dolist (i picked) (%ct-unwatch (nth i *ct-watched*)))
              (%ct-refresh w))
            (clear-event event))
       (#\c (%ct-clear) (%ct-refresh w) (clear-event event))
       (#\r (%ct-refresh w) (clear-event event))
       (t (call-next-method))))
    (t (call-next-method))))

(defun do-call-tree (app)
  "Open the call-tree window (encapsulation-based watch tracing)."
  (let* ((desk (program-desktop app))
         (dw (point-x (view-size desk))) (dh (point-y (view-size desk)))
         (w (min 84 (- dw 2))) (h (min 22 (- dh 2)))
         (lb (make-instance 'tlist-box :items #() :command 0
                            :bounds (make-trect 1 1 (1- w) (- h 2))))
         (win (make-instance 'tcalltree-window :app app :lb lb :bounds (make-trect 0 0 w h)))
         (vsb (standard-scrollbar win t)))
    (insert win lb) (attach-scrollbars lb :vscroll vsb)
    (%ct-refresh win)
    (move-to win (max 0 (floor (- dw w) 2)) (max 0 (floor (- dh h) 2)))
    (insert desk win) (focus win)))

(defun method-label (m)
  (string-trim " "
    (format nil "~{~(~a~)~^ ~} (~{~a~^ ~})"
            (sb-mop:method-qualifiers m)
            (mapcar (lambda (s)
                      (cond ((typep s 'class) (class-name s))
                            (t (or (ignore-errors
                                    (list 'eql (sb-mop:eql-specializer-object s)))
                                   s))))
                    (sb-mop:method-specializers m)))))

(defun do-step (rv)
  "Prompt for a form and evaluate it under the single-stepper."
  (when rv
    (let ((s (prompt-line "Step" "Form to step:")))
      (when s (repl-step-eval rv s) (focus rv)))))

;;; --- statistical profiler (sb-sprof) ---------------------------------------

(defun %fn-name (node-name)
  "A short printed name for an sb-sprof node name."
  (let ((s (princ-to-string node-name)))
    (if (> (length s) 48) (concatenate 'string (subseq s 0 45) "...") s)))

(defun run-profile (form package &key (interval 0.001) (mode :time) all-threads)
  "Evaluate FORM under sb-sprof and return a plist (:total :secs :mode :rows ...).
MODE is :time (CPU) or :alloc (allocations); INTERVAL is the sample interval in
seconds; ALL-THREADS samples every thread, not just this one."
  (let ((*package* package) (t0 (get-internal-real-time)))
    (sb-sprof:reset)
    (sb-sprof:start-profiling :max-samples 200000 :sample-interval interval :mode mode
                              :threads (if all-threads :all (list sb-thread:*current-thread*)))
    (unwind-protect (eval form) (sb-sprof:stop-profiling))
    (let* ((secs (/ (- (get-internal-real-time) t0) internal-time-units-per-second))
           (cg (sb-sprof::make-call-graph sb-sprof::*samples* most-positive-fixnum))
           (total (max 1 (sb-sprof::call-graph-nsamples cg)))
           (flat (sb-sprof::call-graph-flat-nodes cg)))
      (list :total total :secs secs :mode mode
            :rows (loop for n in flat
                        for self = (sb-sprof::node-count n)
                        for cumul = (sb-sprof::node-accrued-count n)
                        collect (list :name (sb-sprof::node-name n)
                                      :self self :cumul cumul
                                      :self% (* 100.0 (/ self total))
                                      :cumul% (* 100.0 (/ cumul total))
                                      :callees (loop for e in (sb-sprof::node-edges n)
                                                     collect (%fn-name (sb-sprof::node-name
                                                                        (sb-sprof::edge-vertex e))))))))))

;;; A table window that can export its rows to CSV with the `e' key; the profile
;;; windows build on it.
(defclass tdata-window (twindow)
  ((table :initarg :table :initform nil :accessor data-table)))

(defun %csv-cell (v)
  (let ((s (princ-to-string v)))
    (if (find-if (lambda (c) (member c '(#\, #\" #\Newline))) s)
        (with-output-to-string (o)
          (write-char #\" o)
          (loop for c across s do (when (char= c #\") (write-char #\" o)) (write-char c o))
          (write-char #\" o))
        s)))

(defun %export-table-csv (tv title)
  "Write TV's columns and rows to a CSV file chosen by the user."
  (let ((path (file-save-dialog :title (format nil "Export ~a to CSV" title))))
    (when path
      (handler-case
          (progn
            (with-open-file (s path :direction :output :if-exists :supersede :if-does-not-exist :create)
              (let ((cols (coerce (table-columns tv) 'list)))
                (format s "~{~a~^,~}~%" (mapcar (lambda (c) (%csv-cell (tvision::table-column-title c))) cols))
                (loop for row across (table-rows tv) do
                  (format s "~{~a~^,~}~%"
                          (mapcar (lambda (c) (%csv-cell (funcall (tvision::table-column-key c) row))) cols)))))
            (message-box (format nil "Wrote ~a" path) (logior +mf-information+ +mf-ok-button+)))
        (error (e) (err-box e))))))

(defmethod handle-event ((w tdata-window) event)
  (when (and (= (event-type event) +ev-key-down+)
             (data-table w)
             (member (event-char-code event) (list (char-code #\e) (char-code #\E))))
    (%export-table-csv (data-table w) (window-title w))
    (clear-event event))
  (call-next-method))

(defun %profile-columns ()
  (vector (make-table-column "Self%"  6 (lambda (r) (getf r :self%))  :numeric t
                             :format (lambda (v) (format nil "~,1f" v)))
          (make-table-column "Cumul%" 7 (lambda (r) (getf r :cumul%)) :numeric t
                             :format (lambda (v) (format nil "~,1f" v)))
          (make-table-column "Samples" 8 (lambda (r) (getf r :self)) :numeric t)
          (make-table-column "Function" 48 (lambda (r) (%fn-name (getf r :name))))))

(defclass tprofile-window (tdata-window)
  ((data  :initarg :data  :initform nil :accessor profile-data)
   (app   :initarg :app   :initform nil :accessor profile-app)))

(defun show-profile-tree (w)
  "Open a TOutline of the hottest functions, each expandable to its callees."
  (let ((roots (loop for r in (subseq (getf (profile-data w) :rows) 0
                                      (min 30 (length (getf (profile-data w) :rows))))
                     collect (make-outline-node
                              (format nil "~5,1f%  ~a" (getf r :self%) (%fn-name (getf r :name)))
                              (mapcar (lambda (c) (make-outline-node c nil)) (getf r :callees))))))
    (open-outline-window "Call graph (function -> callees)" roots)))

(defun show-profile-tree-reverse (w)
  "Open a TOutline of the hottest functions, each expandable to its CALLERS
(the inverse of the callee graph)."
  (let ((rows (getf (profile-data w) :rows))
        (callers (make-hash-table :test 'equal)))
    (dolist (r rows)                                   ; invert the callee edges
      (let ((nm (%fn-name (getf r :name))))
        (dolist (c (getf r :callees)) (pushnew nm (gethash c callers) :test #'string=))))
    (let ((roots (loop for r in (subseq rows 0 (min 30 (length rows)))
                       for nm = (%fn-name (getf r :name))
                       collect (make-outline-node
                                (format nil "~5,1f%  ~a" (getf r :self%) nm)
                                (mapcar (lambda (c) (make-outline-node c nil)) (gethash nm callers))))))
      (open-outline-window "Call graph (function <- callers)" roots))))

(defmethod handle-event ((w tprofile-window) event)
  (cond
    ((and (= (event-type event) +ev-broadcast+)
          (= (event-command event) +cm-list-item-selected+)
          (data-table w))
     (let ((row (table-selected-row (data-table w))))
       (when (and row (symbolp (getf row :name)) (profile-app w))
         (goto-definition-of (profile-app w) (getf row :name))))
     (clear-event event))
    ((and (= (event-type event) +ev-key-down+)
          (member (event-char-code event) (list (char-code #\g) (char-code #\G))))
     (show-profile-tree w)
     (clear-event event))
    ((and (= (event-type event) +ev-key-down+)
          (member (event-char-code event) (list (char-code #\r) (char-code #\R))))
     (show-profile-tree-reverse w)
     (clear-event event))
    (t (call-next-method))))

(defun show-profile-results (app data)
  (let* ((desk (program-desktop app))
         (dw (point-x (view-size desk))) (dh (point-y (view-size desk)))
         (w (min 82 (- dw 2))) (h (min 22 (- dh 2)))
         (win (make-instance 'tprofile-window :data data :app app
                             :title (format nil "Profile (~(~a~)) — ~d samples, ~,2fs  (Enter:src g:callees r:callers s:sort e:csv)"
                                            (or (getf data :mode) :time) (getf data :total) (getf data :secs))
                             :bounds (make-trect 0 0 w h)))
         (vsb (standard-scrollbar win t))
         (tbl (make-instance 'ttable-view :columns (%profile-columns) :rows (getf data :rows)
                             :sort-col 0 :sort-asc nil
                             :bounds (make-trect 1 1 (1- w) (1- h)))))
    (insert win tbl)
    (attach-scrollbars tbl :vscroll vsb)
    (setf (data-table win) tbl)
    (move-to win (max 0 (floor (- dw w) 2)) (max 0 (floor (- dh h) 2)))
    (insert desk win)
    (focus tbl)))

(defun do-profile (rv app)
  (let ((s (prompt-line "Profile" "Form to profile:")))
    (when (and rv s)
      (let ((mode (choose-index "Profiler mode"
                                '("CPU time (this thread)" "CPU time (all threads)" "Allocations")
                                :start 0)))
        (when mode
          (let* ((alloc (= mode 2))
                 (all-threads (= mode 1))
                 (ms (unless alloc (prompt-line "Profile" "Sample interval (ms):" "1.0")))
                 (interval (if ms (/ (or (ignore-errors (read-from-string ms)) 1.0) 1000.0) 0.001))
                 (form (read-in rv s)) (pkg (repl-package rv)))
            ;; run on the worker thread so the UI stays responsive (Ctrl-C aborts)
            (repl-call-on-worker rv
              (lambda ()
                (let ((data (run-profile form pkg :interval interval
                                         :mode (if alloc :alloc :time)
                                         :all-threads all-threads)))
                  (run-on-ui (lambda ()
                               (if (getf data :rows)
                                   (show-profile-results app data)
                                   (message-box "No samples collected (the form ran too quickly)."
                                                (logior +mf-information+ +mf-ok-button+))))))))))))))

(defun %split-bar (line)
  "Split LINE on the | column separators of an SB-PROFILE:REPORT row."
  (let ((out '()) (start 0))
    (dotimes (i (length line))
      (when (char= (char line i) #\|)
        (push (subseq line start i) out) (setf start (1+ i))))
    (nreverse (cons (subseq line start) out))))

(defun %parse-sb-profile-report (text)
  "Parse SB-PROFILE:REPORT output (seconds|gc|consed|calls|sec/call|name) into row
plists (:seconds :consed :calls :name).  Header/rule lines are skipped."
  (let ((rows '()))
    (with-input-from-string (s text)
      (loop for line = (read-line s nil nil) while line do
        (let ((cells (%split-bar line)))
          (when (>= (length cells) 6)
            (let ((nums (mapcar (lambda (c) (ignore-errors (read-from-string (string-trim " " c) nil nil)))
                                (butlast cells)))
                  (name (string-trim " " (car (last cells)))))
              (when (and (realp (first nums)) (plusp (length name)))
                (push (list :seconds (or (nth 0 nums) 0) :consed (or (nth 2 nums) 0)
                            :calls (or (nth 3 nums) 0) :name name)
                      rows)))))))
    (nreverse rows)))

(defun show-deterministic-profile (app title rows)
  "Show parsed deterministic-profile ROWS in a sortable, CSV-exportable table."
  (let* ((desk (program-desktop app))
         (dw (point-x (view-size desk))) (dh (point-y (view-size desk)))
         (w (min 76 (- dw 2))) (h (min 22 (- dh 2)))
         (cols (vector (make-table-column "Calls" 9 (lambda (r) (getf r :calls)) :numeric t)
                       (make-table-column "Seconds" 10 (lambda (r) (getf r :seconds)) :numeric t
                                          :format (lambda (v) (format nil "~,4f" v)))
                       (make-table-column "Consed" 13 (lambda (r) (getf r :consed)) :numeric t)
                       (make-table-column "Function" 38 (lambda (r) (getf r :name)))))
         (win (make-instance 'tdata-window :title (format nil "~a  (s:sort  e:csv)" title)
                             :bounds (make-trect 0 0 w h)))
         (vsb (standard-scrollbar win t))
         (tbl (make-instance 'ttable-view :columns cols :rows rows :sort-col 1 :sort-asc nil
                             :bounds (make-trect 1 1 (1- w) (1- h)))))
    (insert win tbl) (attach-scrollbars tbl :vscroll vsb) (setf (data-table win) tbl)
    (move-to win (max 0 (floor (- dw w) 2)) (max 0 (floor (- dh h) 2)))
    (insert desk win) (focus tbl)))

(defun %one-line (s)
  "Flatten S to a single line (newlines/tabs -> spaces) for a table cell."
  (string-trim " " (substitute #\Space #\Tab (substitute #\Space #\Newline s))))

(defun %show-load-notes (path notes)
  "Display compilation NOTES ((kind . message) ...) from loading PATH in a
sortable, CSV-exportable Kind/Message table.  Bound to TVISION:*LOAD-NOTES-HOOK*."
  (when (and *application* notes)
    (let* ((rows (mapcar (lambda (n) (list :kind (car n) :msg (cdr n))) notes))
           (desk (program-desktop *application*))
           (dw (point-x (view-size desk))) (dh (point-y (view-size desk)))
           (w (min 86 (- dw 2))) (h (min 22 (- dh 2)))
           (cols (vector (make-table-column "Kind" 9
                                            (lambda (r) (string-downcase (princ-to-string (getf r :kind)))))
                         (make-table-column "Message" 72 (lambda (r) (%one-line (getf r :msg))))))
           (win (make-instance 'tdata-window
                               :title (format nil "~a — ~d warning~:p  (s:sort  e:csv)"
                                              (file-namestring path) (length notes))
                               :bounds (make-trect 0 0 w h)))
           (vsb (standard-scrollbar win t))
           (tbl (make-instance 'ttable-view :columns cols :rows rows :sort-col 0 :sort-asc t
                               :bounds (make-trect 1 1 (1- w) (1- h)))))
      (insert win tbl) (attach-scrollbars tbl :vscroll vsb) (setf (data-table win) tbl)
      (move-to win (max 0 (floor (- dw w) 2)) (max 0 (floor (- dh h) 2)))
      (insert desk win) (focus tbl))))

(setf tvision:*load-notes-hook* #'%show-load-notes)

(defun do-profile-deterministic (rv)
  "Deterministic profiler (sb-profile): instrument every function in a package,
run a form, and show the call-count/time report."
  (let ((pkg (and rv (prompt-line "Deterministic profile" "Profile functions in package:"
                                  (package-name (repl-package rv))))))
    (when pkg
      (let ((form (prompt-line "Deterministic profile" "Form to run:"))
            (rp (repl-package rv)))
        (when form
          ;; run on the worker thread so the UI stays responsive
          (repl-call-on-worker rv
            (lambda ()
              (handler-case
                  (let ((*package* rp) (txt nil))
                    (sb-profile:reset)
                    (eval (list 'sb-profile:profile pkg))
                    (unwind-protect
                         (progn
                           (eval (read-from-string form))
                           (setf txt (with-output-to-string (s)
                                       (let ((*standard-output* s) (*trace-output* s))
                                         (sb-profile:report)))))
                      (eval (list 'sb-profile:unprofile pkg))
                      (sb-profile:reset))
                    (run-on-ui
                     (lambda ()
                       (let ((rows (and txt (%parse-sb-profile-report txt))))
                         (if rows
                             (show-deterministic-profile
                              *application* (format nil "Deterministic profile: ~a" pkg) rows)
                             (show-text-window (format nil "Deterministic profile: ~a" pkg)
                                               (or txt "")))))))
                (error (e) (run-on-ui (lambda () (err-box e))))))))))))

;;; A modeless, refreshable browser of a generic function's methods: Enter jumps
;;; to a method's source (the window stays open, so you can visit several), `r'
;;; re-fetches (newly-defined methods appear), and the list's type-ahead filters.
(defclass tfun-browser (twindow)
  ((gf    :initarg :gf  :accessor fb-gf)
   (app   :initarg :app :accessor fb-app)
   (lb    :initform nil :accessor fb-lb)
   (alist :initform nil :accessor fb-alist)))   ; (label . method)

(defun %fb-refresh (w)
  (let* ((gf (fb-gf w))
         (alist (mapcar (lambda (m) (cons (method-label m) m))
                        (sb-mop:generic-function-methods gf))))
    (setf (fb-alist w) alist)
    (list-set-items (fb-lb w) (mapcar #'car alist))
    (setf (window-title w)
          (format nil "~(~a~) — ~d method~:p  (Enter: source  r: refresh)"
                  (sb-mop:generic-function-name gf) (length alist)))
    (draw-view w)))

(defun %fb-goto (w)
  (let* ((lb (fb-lb w))
         (label (and (plusp (list-count lb)) (list-item lb (list-focused lb))))
         (m (cdr (assoc label (fb-alist w) :test #'string=)))
         (src (and m (ignore-errors (sb-introspect:find-definition-source (sb-mop:method-function m)))))
         (path (and src (sb-introspect:definition-source-pathname src))))
    (cond (path (goto-source (fb-app w) :method (namestring path)
                             (sb-introspect:definition-source-character-offset src)))
          (m (goto-definition-of (fb-app w) (sb-mop:generic-function-name (fb-gf w)))))))

(defmethod handle-event ((w tfun-browser) event)
  (cond
    ((and (= (event-type event) +ev-broadcast+)
          (= (event-command event) +cm-list-item-selected+) (fb-lb w))
     (%fb-goto w) (clear-event event))
    ((and (= (event-type event) +ev-key-down+)
          (member (event-char-code event) (list (char-code #\r) (char-code #\R))))
     (%fb-refresh w) (clear-event event))
    (t (call-next-method))))

(defun show-method-browser (app gf)
  (let* ((desk (program-desktop app))
         (dw (point-x (view-size desk))) (dh (point-y (view-size desk)))
         (w (min 72 (- dw 2))) (h (min 20 (- dh 2)))
         (win (make-instance 'tfun-browser :gf gf :app app :bounds (make-trect 0 0 w h)))
         (vsb (standard-scrollbar win t))
         (lb (make-instance 'tsorted-list-box :bounds (make-trect 1 1 (1- w) (1- h)))))
    (insert win lb) (attach-scrollbars lb :vscroll vsb)
    (setf (fb-lb win) lb)
    (%fb-refresh win)
    (move-to win (max 0 (floor (- dw w) 2)) (max 0 (floor (- dh h) 2)))
    (insert desk win) (focus lb)))

(defun do-function-browser (rv app)
  (let ((s (prompt-line "Function / GF browser" "Function name:" (%point-symbol))))
    (when (and rv s)
      (handler-case
          (let* ((sym (read-in rv s)) (fn (and (fboundp sym) (fdefinition sym))))
            (cond
              ((typep fn 'generic-function) (show-method-browser app fn))
              (fn (goto-definition-of app sym))
              (t (message-box (format nil "~a is not a function." s)
                              (logior +mf-information+ +mf-ok-button+)))))
        (error (e) (err-box e))))))

;;; --- transcript / editor search -------------------------------------------

(defun %find-dialog (app title initial &key replace)
  "A Find (or Replace) dialog with options.  Returns
(values ok find-text replace-text case-p word-p back-or-all-p)."
  (let* ((w 52) (h (if replace 14 13))
         (d (make-instance 'tdialog :title title :bounds (make-trect 0 0 w h)))
         (find-in (make-instance 'tinputline :data initial :maxlen 100
                                 :bounds (make-trect 12 2 (- w 3) 3)))
         (repl-in (when replace
                    (make-instance 'tinputline :data (replace-last app) :maxlen 100
                                   :bounds (make-trect 12 4 (- w 3) 5))))
         (opts (make-instance 'tcheck-boxes
                              :labels (if replace
                                          '("~C~ase sensitive" "~W~hole word" "Rege~x~p" "Replace ~a~ll (no prompt)")
                                          '("~C~ase sensitive" "~W~hole word" "Rege~x~p" "~B~ackward")))))
    (set-bounds opts (make-trect 3 (if replace 6 4) (- w 3) (if replace 10 8)))
    (setf (cluster-value opts) (logior (if (find-case app) 1 0) (if (find-word app) 2 0)
                                       (if (find-regex app) 4 0)))
    (flet ((lbl (text y link)
             (let ((l (make-instance 'tlabel :text text :link link)))
               (set-bounds l (make-trect 3 y (+ 3 (length text)) (1+ y)))
               (insert d l))))
      (lbl "Find:" 2 find-in) (insert d find-in)
      (when replace (lbl "Replace:" 4 repl-in) (insert d repl-in)))
    (insert d opts)
    (insert d (make-button (make-trect (- w 26) (- h 3) (- w 16) (- h 1)) "~O~K" +cm-ok+ t))
    (insert d (make-button (make-trect (- w 13) (- h 3) (- w 3) (- h 1)) "Cancel" +cm-cancel+))
    (let ((desk (program-desktop app)))
      (move-to d (max 0 (floor (- (point-x (view-size desk)) w) 2))
               (max 0 (floor (- (point-y (view-size desk)) h) 2))))
    (focus find-in)
    (if (= (exec-view (program-desktop app) d) +cm-ok+)
        (let ((v (cluster-value opts)))
          ;; ok find replace case word regex back-or-all
          (values t (get-data find-in) (and repl-in (get-data repl-in))
                  (logbitp 0 v) (logbitp 1 v) (logbitp 2 v) (logbitp 3 v)))
        (values nil nil nil nil nil nil nil))))

(defun %do-search (app)
  (let ((tv (%current-text-view app)) (s (find-last app)))
    (when (and tv (plusp (length s)))
      (unless (if (find-regex app)
                  (text-find-and-select-regex tv s :wrap t)
                  (text-find-and-select tv s :case-sensitive (find-case app)
                                        :whole-word (find-word app)
                                        :backward (find-back app) :wrap t))
        (message-box "Not found." (logior +mf-information+ +mf-ok-button+)))
      (draw-view tv))))

(defun do-find (app)
  (when (%current-text-view app)
    (multiple-value-bind (ok text rep case word regex back) (%find-dialog app "Find" (find-last app))
      (declare (ignore rep))
      (when (and ok (plusp (length text)))
        (setf (find-last app) text (find-case app) case
              (find-word app) word (find-regex app) regex (find-back app) back)
        (%do-search app)))))

(defun do-find-next (app)
  (if (plusp (length (find-last app))) (%do-search app) (do-find app)))

(defun %query-replace (app ed find repl)
  "Step through matches, confirming each (Yes / No / Cancel)."
  (let ((count 0) (case (find-case app)) (word (find-word app)))
    (block done
      (loop
        (let ((m (text-find ed find :case-sensitive case :whole-word word)))
          (unless m (return))
          (text-select-match ed m find)
          (draw-view app) (when tvision:*screen* (flush-screen tvision:*screen*))
          (case (message-box "Replace this occurrence?"
                             (logior +mf-confirmation+ +mf-yes-button+
                                     +mf-no-button+ +mf-cancel-button+))
            (#.+cm-yes+ (text-replace-selection ed repl) (incf count))
            (#.+cm-no+  (setf (text-anchor ed) nil
                              (text-cur-line ed) (car m)
                              (text-cur-col ed) (+ (cdr m) (length find))))
            (t (return-from done))))))
    (message-box (format nil "~d replacement~:p made." count)
                 (logior +mf-information+ +mf-ok-button+))))

(defun %query-replace-regex (app ed find repl)
  "Step through regex matches, confirming each (Yes / No / Cancel)."
  (let ((count 0))
    (block done
      (loop
        (unless (text-find-and-select-regex ed find) (return))
        (draw-view app) (when tvision:*screen* (flush-screen tvision:*screen*))
        (case (message-box "Replace this occurrence?"
                           (logior +mf-confirmation+ +mf-yes-button+ +mf-no-button+ +mf-cancel-button+))
          (#.+cm-yes+ (text-replace-selection ed repl) (incf count))
          (#.+cm-no+  (setf (text-anchor ed) nil))   ; cursor sits at match end -> search moves on
          (t (return-from done)))))
    (message-box (format nil "~d replacement~:p made." count)
                 (logior +mf-information+ +mf-ok-button+))))

(defun do-replace (app)
  "Find/Replace across the focused editor: all-at-once or confirm each match;
literal or regular-expression."
  (let ((ew (current-editor-window app)))
    (if (not ew)
        (message-box "Replace works in an editor window." (logior +mf-information+ +mf-ok-button+))
        (multiple-value-bind (ok find repl case word regex all)
            (%find-dialog app "Replace" (find-last app) :replace t)
          (when (and ok (plusp (length find)))
            (setf (find-last app) find (replace-last app) repl
                  (find-case app) case (find-word app) word (find-regex app) regex)
            (let ((ed (editor-window-editor ew)))
              (cond
                (regex
                 (if all
                     (let ((n (text-replace-all-regex ed find repl)))
                       (draw-view ed)
                       (message-box (format nil "~d replacement~:p made." n)
                                    (logior +mf-information+ +mf-ok-button+)))
                     (%query-replace-regex app ed find repl)))
                (all
                 (let ((n (text-replace-all ed find repl :case-sensitive case :whole-word word)))
                   (draw-view ed)
                   (message-box (format nil "~d replacement~:p made." n)
                                (logior +mf-information+ +mf-ok-button+))))
                (t (%query-replace app ed find repl)))
              (draw-view ed)))))))

(defun do-goto-line (app)
  "Jump to a line number in the focused editor."
  (let ((ew (current-editor-window app)))
    (when ew
      (multiple-value-bind (cmd s) (input-box "Go to line" "Line number:" "" 12)
        (when (= cmd +cm-ok+)
          (let ((n (parse-integer s :junk-allowed t)) (ed (editor-window-editor ew)))
            (when n
              (text-goto ed (max 1 n) 0)
              ;; flash: select the whole target line so the landing spot is obvious
              (let* ((li (text-cur-line ed)) (len (length (nth-line ed li))))
                (setf (text-anchor ed) (cons li 0)
                      (text-cur-line ed) li (text-cur-col ed) len))
              (draw-view ed))))))))

(defun do-isearch (app)
  "Incremental search in the focused editor: type to jump, Down for next,
Backspace shortens, Esc cancels (restores point), Enter keeps the match."
  (let ((ew (current-editor-window app)))
    (when ew
      (let* ((ed (editor-window-editor ew))
             (query (make-array 0 :element-type 'character :adjustable t :fill-pointer 0))
             (from (cons (text-cur-line ed) (text-cur-col ed)))
             (found t)
             (saved-title (window-title ew)))
        (flet ((prompt ()
                 (setf (window-title ew)
                       (format nil "~:[(failing) ~;~]I-search: ~a" found (coerce query 'string)))
                 (draw-view ew))
               (jump ()
                 (let ((q (coerce query 'string)))
                   (if (zerop (length q))
                       (setf found t)
                       ;; search forward from FROM; on miss, wrap around to the top
                       (let ((m (or (text-find ed q :from-line (car from) :from-col (cdr from))
                                    (text-find ed q :from-line 0 :from-col 0))))
                         (if m (progn (text-select-match ed m q) (setf found t))
                             (setf found nil)))))))
          (prompt)
          (block search
            (loop
              (draw-view app) (when tvision:*screen* (flush-screen tvision:*screen*))
              (let ((e (get-event app)))
                (when (= (event-type e) +ev-key-down+)
                  (let ((k (event-key-code e)) (ch (event-char-code e)))
                    (cond
                      ((= k +kb-esc+)
                       (setf (text-cur-line ed) (car from) (text-cur-col ed) (cdr from)
                             (text-anchor ed) nil)
                       (return-from search))
                      ((= k +kb-enter+) (return-from search))
                      ((= k +kb-back+)
                       (when (plusp (fill-pointer query)) (decf (fill-pointer query)))
                       (jump) (prompt))
                      ((= k +kb-down+)
                       (setf from (cons (text-cur-line ed) (text-cur-col ed)))
                       (jump) (prompt))
                      ((and (>= ch 32) (/= ch 127) (< ch char-code-limit)
                            (let ((c (code-char ch))) (and c (graphic-char-p c))))
                       (vector-push-extend (code-char ch) query)
                       (jump) (prompt))))))))
          (setf (window-title ew) saved-title)
          (draw-view ew))))))

(defun do-toggle-wrap (app)
  "Toggle word-wrap in the focused editor."
  (let ((ew (current-editor-window app)))
    (when ew
      (let ((ed (editor-window-editor ew)))
        (set-text-wrap ed (not (text-wrap ed)))
        (draw-view ew)))))

(defun do-history-search (rv)
  (when rv
    (let ((chosen (choose-from-list "History"
                    (remove-duplicates (copy-list (repl-history rv))
                                       :test #'string= :from-end t))))
      (when chosen (repl-replace-input rv chosen) (draw-view rv)))))

;;; --- true-colour demo ------------------------------------------------------

(defun %hsv->rgb (h v)
  "HSV with saturation 1 -> (values R G B), H in 0..360, V in 0..1."
  (let* ((c (float v)) (x (* c (- 1 (abs (- (mod (/ h 60.0) 2) 1))))))
    (multiple-value-bind (r g b)
        (cond ((< h  60) (values c x 0)) ((< h 120) (values x c 0))
              ((< h 180) (values 0 c x)) ((< h 240) (values 0 x c))
              ((< h 300) (values x 0 c)) (t          (values c 0 x)))
      (values (round (* 255 r)) (round (* 255 g)) (round (* 255 b))))))

(defclass tcolor-demo-view (tview) ()
  (:documentation "Paints a hue x brightness true-colour gradient -- a per-cell
24-bit colour field, impossible in the 16-colour model."))

(defmethod draw ((v tcolor-demo-view))
  (let* ((w (point-x (view-size v))) (h (point-y (view-size v)))
         (db (make-draw-buffer w)))
    (dotimes (y h)
      (let ((val (- 1.0 (* 0.85 (/ y (max 1 (1- h)))))))
        (dotimes (x w)
          (multiple-value-bind (r g b) (%hsv->rgb (* 360.0 (/ x (max 1 w))) val)
            (db-fill db #\Space (make-rgb 255 255 255 r g b) x 1))))
      (write-line* v 0 y w 1 db))))

(defun do-color-demo (app)
  "Open a window showing a 24-bit colour gradient (true-colour proof)."
  (let* ((desk (program-desktop app))
         (dw (point-x (view-size desk))) (dh (point-y (view-size desk)))
         (w (make-instance 'twindow :title "True-colour gradient"
                           :bounds (make-trect 3 1 (min (- dw 3) 70) (min (- dh 1) 20))))
         (gv (make-instance 'tcolor-demo-view
                            :bounds (make-trect 1 1 (1- (point-x (view-size w)))
                                                (1- (point-y (view-size w)))))))
    (insert w gv) (insert desk w) (focus w)))

;;; --- editor + load buffer --------------------------------------------------

(defun %backup-file (path)
  "Copy PATH to PATH~ before it is overwritten (best-effort)."
  (when (probe-file path)
    (ignore-errors
     (let ((bak (concatenate 'string (namestring path) "~")))
       (with-open-file (in path :element-type '(unsigned-byte 8))
         (with-open-file (out bak :direction :output :element-type '(unsigned-byte 8)
                                  :if-exists :supersede :if-does-not-exist :create)
           (let ((buf (make-array 65536 :element-type '(unsigned-byte 8))))
             (loop for n = (read-sequence buf in) while (plusp n)
                   do (write-sequence buf out :end n)))))))))

(defun do-new-editor (app)
  "Open a fresh, empty editor window."
  (let* ((desk (program-desktop app))
         (dw (point-x (view-size desk))) (dh (point-y (view-size desk))))
    (multiple-value-bind (w ed)
        (make-edit-window (make-trect 2 1 (min (- dw 2) 78) (min (- dh 1) 22))
                          :title "Untitled")
      (declare (ignore ed))
      (insert desk w)
      (focus w))))

(defun do-open-editor (app)
  (let ((path (file-open-dialog :title "Open in editor")))
    (when path
      (let* ((desk (program-desktop app))
             (dw (point-x (view-size desk))) (dh (point-y (view-size desk))))
        (multiple-value-bind (w ed)
            (make-edit-window (make-trect 2 1 (min (- dw 2) 78) (min (- dh 1) 22))
                              :title (file-namestring path) :filename path)
          (declare (ignore ed))
          (insert desk w)
          (focus w))))))

(defun current-editor-window (app)
  (let ((w (group-current (program-desktop app))))
    (and (typep w 'teditor-window) w)))

(defmethod valid-p ((w teditor-window) command)
  "Confirm before closing or quitting a modified editor window: Yes saves
(Save As when it has no file yet), No discards, Cancel keeps the window."
  (let ((ed (editor-window-editor w)))
    (if (and (member command (list +cm-close+ +cm-quit+)) ed (text-modified ed))
        (let ((ans (message-box
                    (format nil "~a has unsaved changes.  Save before closing?"
                            (window-title w))
                    (logior +mf-warning+ +mf-yes-button+ +mf-no-button+ +mf-cancel-button+))))
          (cond
            ((= ans +cm-yes+)
             (let ((path (or (editor-filename ed) (file-save-dialog :title "Save As"))))
               (when path
                 (%backup-file path)
                 (text-save-file ed path)
                 (setf (editor-filename ed) path
                       (window-title w) (file-namestring path)))
               (and path t)))            ; Save As cancelled -> abort the close
            ((= ans +cm-no+) t)          ; discard changes
            (t nil)))                    ; Cancel -> keep the window
        (call-next-method))))

(defun do-saveas-editor (app)
  "Save the focused editor window under a new path; return T if saved."
  (let ((w (current-editor-window app)))
    (when w
      (let ((path (file-save-dialog :title "Save As")))
        (when path
          (handler-case
              (let ((ed (editor-window-editor w)))
                (%backup-file path)
                (text-save-file ed path)
                (setf (editor-filename ed) path
                      (window-title w) (file-namestring path))
                (draw-view w)
                t)
            (error (e)
              (message-box (format nil "Could not save:~%~a" e)
                           (logior +mf-error+ +mf-ok-button+))
              nil)))))))

;;; --- compile-defun with navigable compiler notes (SLIME C-c C-c) ----------

(defun %toplevel-form-span (str off)
  "(values START END) of the outermost () form whose span contains OFF, or NIL."
  (let ((len (length str)) (i 0) (depth 0) (start nil))
    (loop while (< i len) do
      (let ((c (char str i)))
        (cond
          ((char= c #\;) (loop while (and (< i len) (char/= (char str i) #\Newline)) do (incf i)))
          ((char= c #\")
           (incf i) (loop while (< i len) do
             (let ((d (char str i))) (incf i)
               (cond ((char= d #\\) (incf i)) ((char= d #\") (return))))))
          ((and (char= c #\#) (< (1+ i) len) (char= (char str (1+ i)) #\\)) (incf i 3))
          ((char= c #\() (when (zerop depth) (setf start i)) (incf depth) (incf i))
          ((char= c #\))
           (incf i) (when (plusp depth) (decf depth))
           (when (zerop depth)
             (when (and start (<= start off) (<= off i))
               (return-from %toplevel-form-span (values start i)))
             (setf start nil)))
          (t (incf i)))))
    nil))

(defun %message-symbols (message)
  "Symbol names referenced in a compiler MESSAGE: all-uppercase tokens (CL upcases
symbol names), package qualifier stripped — used to pinpoint the offending form."
  (let ((toks '()) (i 0) (n (length message)))
    (flet ((symchar (ch) (or (alphanumericp ch) (find ch "*+/<>=!?%&._-:"))))
      (loop while (< i n) do
        (if (symchar (char message i))
            (let ((j i))
              (loop while (and (< j n) (symchar (char message j))) do (incf j))
              (let ((tok (subseq message i j)))
                (when (and (> (length tok) 1) (find-if #'upper-case-p tok)
                           (not (find-if #'lower-case-p tok)))
                  (push (%strip-package tok) toks)))
              (setf i j))
            (incf i))))
    (nreverse toks)))

(defun %search-token (token text start)
  "Offset of TOKEN in TEXT at/after START as a *whole* symbol token (not a
substring of a larger symbol, so `nam' won't match inside `name'),
case-insensitively, or NIL."
  (let* ((tk (string-downcase token)) (low (string-downcase text))
         (n (length text)) (tl (length token)) (i start))
    (loop for p = (search tk low :start2 i)
          while p do
            (let ((before (and (> p 0) (char low (1- p))))
                  (after (and (< (+ p tl) n) (char low (+ p tl)))))
              (if (and (or (null before) (not (%hs-symchar-p before)))
                       (or (null after) (not (%hs-symchar-p after))))
                  (return p)
                  (setf i (1+ p)))))))

(defun %note-refine-offset (text pos message)
  "Refine a note at top-level-form offset POS to the offending symbol's position
by searching TEXT (from POS) for a whole-token symbol named in MESSAGE."
  (let ((start (max 0 (min pos (length text)))) (best nil))
    (dolist (tok (%message-symbols message))
      (let ((p (%search-token tok text start)))
        (when (and p (or (null best) (< p best))) (setf best p))))
    (or best start)))

(defun %compile-text-notes (text pkg)
  "Worker-side: compile TEXT (read in PKG) from a temp file, returning
(values STATUS NOTES); STATUS is :ok or an error string and each note is
(:severity KW :pos INT :message STR), POS being the offending top-level form's
character offset in TEXT (via SBCL's compiler-error-context)."
  (let ((src (format nil "/tmp/tvlisp-cd-~36r.lisp" (get-internal-real-time)))
        (notes '())
        (fec  (find-symbol "FIND-ERROR-CONTEXT" :sb-c))
        (cefp (find-symbol "COMPILER-ERROR-CONTEXT-FILE-POSITION" :sb-c)))
    (unwind-protect
         (handler-case
             (progn
               (with-open-file (s src :direction :output :if-exists :supersede
                                      :if-does-not-exist :create :external-format :utf-8)
                 (write-string text s))
               (flet ((grab (c sev)
                        (let* ((ctx (and fec (ignore-errors (funcall fec nil))))
                               (pos (or (and ctx cefp (ignore-errors (funcall cefp ctx))) 0)))
                          (push (list :severity sev :pos (or pos 0) :message (princ-to-string c)) notes))
                        (when (find-restart 'muffle-warning c) (muffle-warning c))))
                 (handler-bind ((style-warning        (lambda (c) (grab c :style)))
                                (sb-ext:compiler-note (lambda (c) (grab c :note)))
                                (warning              (lambda (c) (grab c :warning))))
                   (let ((*package* pkg)
                         (*error-output* (make-broadcast-stream))
                         (*standard-output* (make-broadcast-stream)))
                     (with-compilation-unit (:override t)
                       (compile-file src :verbose nil :print nil)))))
               (values :ok (nreverse notes)))
           (error (e) (values (princ-to-string e) (nreverse notes))))
      (ignore-errors (delete-file src))
      (ignore-errors (delete-file (compile-file-pathname src))))))

(defclass tnotes-window (twindow)
  ((src-win :initarg :src-win :initform nil :accessor notes-src-win)  ; the editor window
   (rows    :initarg :rows    :initform nil :accessor notes-rows)
   (lb      :initarg :lb      :initform nil :accessor notes-lb))
  (:documentation "A navigable list of compiler notes; Enter jumps to the
offending location in the source editor window."))

(defun %notes-jump (w)
  (let* ((rows (notes-rows w)) (lb (notes-lb w))
         (row (and lb rows (nth (list-focused lb) rows)))
         (sw (notes-src-win w)) (desk (and *application* (program-desktop *application*))))
    (when (and row sw desk (member sw (desktop-windows desk)))
      (let ((ed (editor-window-editor sw)))
        (%macro-set-cursor ed (getf row :offset))
        (draw-view ed)
        (set-current desk sw :normal-select)))))

(defmethod handle-event ((w tnotes-window) event)
  (cond
    ((and (= (event-type event) +ev-broadcast+)
          (= (event-command event) +cm-list-item-selected+)
          (notes-lb w))
     (%notes-jump w) (clear-event event))
    (t (call-next-method))))

(defun show-compile-notes (app src-win rows title)
  "Open a navigable compiler-notes window for ROWS (each (:severity :message
:offset)); Enter jumps into SRC-WIN's editor."
  (let* ((desk (program-desktop app))
         (dw (point-x (view-size desk))) (dh (point-y (view-size desk)))
         (w (min 84 (- dw 2))) (h (min 12 (max 6 (+ 3 (length rows)))))
         (items (mapcar (lambda (r)
                          (format nil "~7a ~a"
                                  (case (getf r :severity)
                                    (:style "style") (:warning "warning")
                                    (:note "note") (t "?"))
                                  (getf r :message)))
                        rows))
         (lb (make-instance 'tlist-box :items items :command 0
                            :bounds (make-trect 1 1 (1- w) (- h 2))))
         (win (make-instance 'tnotes-window :src-win src-win :rows rows :lb lb
                             :title title :bounds (make-trect 0 0 w h)))
         (vsb (standard-scrollbar win t)))
    (insert win lb) (attach-scrollbars lb :vscroll vsb)
    (move-to win (max 0 (floor (- dw w) 2)) (max 1 (- dh h 1)))   ; bottom: editor stays visible
    (insert desk win) (focus win)))

(defun do-compile-defun (app)
  "Compile the top-level form at the cursor and list its compiler notes; Enter on
a note jumps to the offending form in the editor (SLIME's C-c C-c)."
  (let* ((win (current-editor-window app))
         (ed (and win (editor-window-editor win)))
         (rv (some-repl app)))
    (cond
      ((not ed) (message-box "Focus an editor window first." (logior +mf-information+ +mf-ok-button+)))
      ((not rv) (message-box "No REPL open." (logior +mf-information+ +mf-ok-button+)))
      (t (let ((text (text-string ed)) (off (%editor-offset ed)))
           (multiple-value-bind (base end) (%toplevel-form-span text off)
             (if (null base)
                 (message-box "No top-level form at the cursor."
                              (logior +mf-information+ +mf-ok-button+))
                 (let ((form-text (subseq text base end))
                       (pkg (or (find-package (%buffer-in-package text off)) (repl-package rv))))
                   (repl-print rv (format nil "~%; compiling top-level form ...~%"))
                   (repl-call-on-worker rv
                     (lambda ()
                       (multiple-value-bind (status notes) (%compile-text-notes form-text pkg)
                         (run-on-ui
                          (lambda ()
                            (tvision::repl-ensure-fresh-line rv)
                            (let ((rows (loop for nt in notes collect
                                              (list :severity (getf nt :severity)
                                                    :message (getf nt :message)
                                                    :offset (+ base (%note-refine-offset
                                                                     form-text (getf nt :pos)
                                                                     (getf nt :message)))))))
                              (cond
                                ((stringp status)
                                 (repl-print rv (format nil "; compile error: ~a~%" status)))
                                ((null rows)
                                 (repl-print rv "; compiled cleanly (no notes)~%"))
                                (t (repl-print rv (format nil "; ~d compiler note~:p~%" (length rows)))
                                   (show-compile-notes app win rows
                                     (format nil "Compiler notes (~d) — Enter: jump to source"
                                             (length rows)))))
                              (tvision::repl-fresh-prompt rv) (draw-view rv)
                              (when tvision:*screen* (flush-screen tvision:*screen*))))))))))))))))

(defun do-compile-buffer (app)
  "Compile the focused editor's whole buffer (without loading it) and show its
compiler notes in a navigable list -- Enter jumps to the offending form.  Runs on
the listener's worker."
  (let* ((win (current-editor-window app))
         (ed (and win (editor-window-editor win)))
         (rv (some-repl app)))
    (cond
      ((not ed) (message-box "Focus an editor window first." (logior +mf-information+ +mf-ok-button+)))
      ((not rv) (message-box "No REPL open." (logior +mf-information+ +mf-ok-button+)))
      (t (let ((text (text-string ed))
               (pkg (or (find-package (%buffer-in-package (text-string ed) 0)) (repl-package rv)))
               (name (if (editor-filename ed) (file-namestring (editor-filename ed)) "buffer")))
           (repl-print rv (format nil "~%; compiling ~a ...~%" name))
           (repl-call-on-worker rv
             (lambda ()
               (multiple-value-bind (status notes) (%compile-text-notes text pkg)
                 (run-on-ui
                  (lambda ()
                    (tvision::repl-ensure-fresh-line rv)
                    (let ((rows (loop for nt in notes collect
                                      (list :severity (getf nt :severity)
                                            :message (getf nt :message)
                                            :offset (%note-refine-offset text (getf nt :pos)
                                                                         (getf nt :message))))))
                      (cond
                        ((stringp status)
                         (repl-print rv (format nil "; compile error: ~a~%" status)))
                        ((null rows)
                         (repl-print rv (format nil "; compiled ~a cleanly~%" name)))
                        (t (repl-print rv (format nil "; compiled ~a  (~d note~:p)~%" name (length rows)))
                           (show-compile-notes app win rows
                             (format nil "~a — ~d note~:p — Enter: jump" name (length rows)))))
                      (tvision::repl-fresh-prompt rv) (draw-view rv)
                      (when tvision:*screen* (flush-screen tvision:*screen*)))))))))))))

(defun do-save-editor (app)
  "Save the focused editor window (Save As if it has no filename yet), keeping a
PATH~ backup of the previous contents."
  (let ((w (current-editor-window app)))
    (when w
      (let* ((ed (editor-window-editor w)) (path (editor-filename ed)))
        (if path
            (handler-case (progn (%backup-file path) (text-save-file ed path) (draw-view w))
              (error (e) (message-box (format nil "Could not save:~%~a" e)
                                      (logior +mf-error+ +mf-ok-button+))))
            (do-saveas-editor app))))))

(defun %write-session-script (rv path)
  "Write RV's input forms (chronological, :help meta-commands dropped) to PATH as
a loadable Lisp script.  Return the number of forms written."
  (let ((forms (remove-if #'tvision::repl-meta-command-p (reverse (repl-history rv)))))
    (with-open-file (s path :direction :output :if-exists :supersede
                            :if-does-not-exist :create :external-format :utf-8)
      (format s ";;; tvlisp session script (~d form~:p)~%~%" (length forms))
      (dolist (form forms) (write-string form s) (terpri s) (terpri s)))
    (length forms)))

(defun do-save-script (rv)
  "Export the session's input forms as a loadable Lisp script."
  (let ((path (and rv (file-save-dialog :title "Save Lisp script"))))
    (when path
      (handler-case
          (message-box (format nil "Wrote ~d form~:p to ~a" (%write-session-script rv path) path)
                       (logior +mf-information+ +mf-ok-button+))
        (error (e) (err-box e))))))

(defun do-save-all (app)
  "Save every modified, file-backed editor window (with PATH~ backups)."
  (let ((n 0))
    (dolist (w (group-subviews (program-desktop app)))
      (when (typep w 'teditor-window)
        (let ((ed (editor-window-editor w)))
          (when (and ed (editor-filename ed) (text-modified ed))
            (handler-case (progn (%backup-file (editor-filename ed))
                                 (text-save-file ed (editor-filename ed)) (draw-view w) (incf n))
              (error () nil))))))
    (message-box (format nil "Saved ~d modified editor~:p." n)
                 (logior +mf-information+ +mf-ok-button+))))

(defun do-load-buffer (app)
  (let* ((win (group-current (program-desktop app)))
         (ed (and (typep win 'teditor-window) (editor-window-editor win)))
         (rv (some-repl app)))
    (cond
      ((not ed) (message-box "Focus an editor window first." (logior +mf-information+ +mf-ok-button+)))
      ((not rv) (message-box "No REPL open." (logior +mf-information+ +mf-ok-button+)))
      (t (let ((text (text-string ed)) (pkg (repl-package rv)))
           ;; evaluate the buffer on the worker so the UI stays responsive;
           ;; in-package forms in the buffer take effect for the forms that follow
           (repl-call-on-worker rv
             (lambda ()
               (let* ((n 0)
                      (out (with-output-to-string (o)
                             (let ((*standard-output* o) (*error-output* o) (*package* pkg))
                               (handler-case
                                   (with-input-from-string (in text)
                                     (loop for f = (read in nil :eof) until (eq f :eof)
                                           do (eval f) (incf n)))
                                 (error (e) (format o ";; ~a~%" e)))))))
                 (run-on-ui (lambda ()
                              (show-text-window "Load buffer"
                                (format nil "~@[~a~%~];; loaded ~d form~:p"
                                        (and (plusp (length out)) out) n))))))))))))

;;; --- evaluate from an editor window ----------------------------------------

(defun %eval-in-repl (app text)
  "Raise a REPL, show TEXT at its prompt, and evaluate it (with full output and
debugger support, exactly as if typed)."
  (let ((rv (some-repl app))
        (form (string-trim '(#\Space #\Tab #\Newline #\Return) (or text ""))))
    (cond
      ((zerop (length form)) nil)
      ((null rv) (message-box "No REPL is open." (logior +mf-information+ +mf-ok-button+)))
      ((repl-busy rv) (message-box "The REPL is busy evaluating."
                                   (logior +mf-information+ +mf-ok-button+)))
      (t (let ((win (view-owner rv)))
           (when (typep win 'twindow) (set-current (program-desktop app) win :normal-select)))
         (repl-replace-input rv form)
         (repl-submit rv form)
         (draw-view rv)))))

(defun do-eval-defun (app)
  "Evaluate the top-level form at the cursor of the focused editor."
  (let ((ew (current-editor-window app)))
    (if (not ew)
        (message-box "Focus an editor window first." (logior +mf-information+ +mf-ok-button+))
        (let* ((ed (editor-window-editor ew))
               (text (text-string ed)) (off (%editor-offset ed))
               (form (%toplevel-form-at-offset text off)))
          (if form
              ;; evaluate, but keep point in the editor (like SLIME's C-c C-c)
              (progn (%eval-in-repl app (%with-buffer-package app (%buffer-in-package text off) form))
                     (focus ew))
              (message-box "No top-level form at the cursor."
                           (logior +mf-information+ +mf-ok-button+)))))))

(defun do-eval-region (app)
  "Evaluate the selected text of the focused editor."
  (let ((ew (current-editor-window app)))
    (if (not ew)
        (message-box "Focus an editor window first." (logior +mf-information+ +mf-ok-button+))
        (let* ((ed (editor-window-editor ew))
               (sel (selected-string ed)))
          (if (and sel (plusp (length (string-trim '(#\Space #\Tab #\Newline) sel))))
              (progn (%eval-in-repl app (%with-buffer-package
                                         app (%buffer-in-package (text-string ed) (%editor-offset ed)) sel))
                     (focus ew))
              (message-box "Select a region first." (logior +mf-information+ +mf-ok-button+)))))))

;;; --- editor productivity: complete, comment, structural edits, snippets ----

(defun %with-editor (app fn)
  "Call FN with the focused editor's text view, or tell the user to focus one."
  (let ((ew (current-editor-window app)))
    (if ew (funcall fn (editor-window-editor ew))
        (message-box "Focus an editor window first."
                     (logior +mf-information+ +mf-ok-button+)))))

(defun %editor-set-text (ed text offset)
  "Replace ED's whole buffer with TEXT and place the cursor at character OFFSET."
  (set-text ed text)
  (%macro-set-cursor ed (max 0 (min offset (length text))))
  (setf (text-modified ed) t)
  (draw-view ed))

(defun do-editor-complete (app)
  "Complete the symbol before the cursor in the focused editor (TAB-style)."
  (%with-editor app
    (lambda (ed)
      (let* ((line (current-line-string ed)) (col (min (text-cur-col ed) (length line)))
             (start col))
        (loop while (and (> start 0) (%hs-symchar-p (char line (1- start)))) do (decf start))
        (let* ((token (subseq line start col))
               (rv (some-repl app))
               (pkg (or (find-package (%buffer-in-package (text-string ed) (%editor-offset ed)))
                        (and rv (repl-package rv)) *package*))
               (cands (and (plusp (length token)) (repl-backend-completions token pkg))))
        (flet ((put (text)
                 (set-line ed (text-cur-line ed)
                           (concatenate 'string (subseq line 0 start) text (subseq line col)))
                 (setf (text-cur-col ed) (+ start (length text)) (text-modified ed) t)
                 (ensure-visible ed) (draw-view ed)))
          (cond
            ((zerop (length token))
             (message-box "Put the cursor after a symbol prefix to complete."
                          (logior +mf-information+ +mf-ok-button+)))
            ((null cands)
             (message-box "No completions." (logior +mf-information+ +mf-ok-button+)))
            ((= 1 (length cands)) (put (first cands)))
            (t (let* ((g (make-global ed (make-tpoint (- col (text-left-col ed))
                                                      (1+ (- (text-cur-line ed) (text-top-line ed))))))
                      (chosen (popup-list cands (point-x g) (point-y g) :title "Complete")))
                 (when chosen (put chosen)))))))))))

(defun %uncomment-line (line)
  "Strip a leading `;'..`; ' comment marker (after indentation) from LINE."
  (let ((k 0))
    (loop while (and (< k (length line)) (member (char line k) '(#\Space #\Tab))) do (incf k))
    (let ((j k))
      (loop while (and (< j (length line)) (char= (char line j) #\;)) do (incf j))
      (when (and (< j (length line)) (char= (char line j) #\Space)) (incf j))
      (if (> j k) (concatenate 'string (subseq line 0 k) (subseq line j)) line))))

(defun do-comment-region (app)
  "Toggle `;; ' line comments over the selected lines (or the current line)."
  (%with-editor app
    (lambda (ed)
      (multiple-value-bind (s e) (selection-range ed)
        (let* ((l0 (if s (car s) (text-cur-line ed)))
               (l1 (if e (if (and (zerop (cdr e)) (> (car e) l0)) (1- (car e)) (car e)) l0))
               (l1 (max l0 (min l1 (1- (line-count ed)))))
               (all-commented t))
          (loop for li from l0 to l1
                for tr = (string-left-trim '(#\Space #\Tab) (nth-line ed li))
                when (and (plusp (length tr)) (char/= (char tr 0) #\;))
                  do (setf all-commented nil))
          (loop for li from l0 to l1 for line = (nth-line ed li) do
            (set-line ed li (if all-commented (%uncomment-line line)
                                (concatenate 'string ";; " line))))
          (setf (text-modified ed) t) (text-update-limit ed) (draw-view ed))))))

(defun %sexp-bounds (str off)
  "(values START END) of the innermost () form containing OFF in STR, or NIL."
  (let ((len (length str)) (stack '()) (best nil) (i 0))
    (loop while (< i len) do
      (let ((c (char str i)))
        (cond
          ((char= c #\;) (loop while (and (< i len) (char/= (char str i) #\Newline)) do (incf i)))
          ((char= c #\")
           (incf i) (loop while (< i len) do
             (let ((d (char str i))) (incf i)
               (cond ((char= d #\\) (incf i)) ((char= d #\") (return))))))
          ((and (char= c #\#) (< (1+ i) len) (char= (char str (1+ i)) #\\)) (incf i 3))
          ((char= c #\() (push i stack) (incf i))
          ((char= c #\))
           (when stack
             (let ((start (pop stack)))
               (when (and (<= start off) (<= off (1+ i))
                          (or (null best) (> start (car best))))
                 (setf best (cons start (1+ i))))))
           (incf i))
          (t (incf i)))))
    (when best (values (car best) (cdr best)))))

(defun %struct-edit (app transform)
  "Apply TRANSFORM (text offset) -> (values new-text new-offset) to the focused
editor's buffer, around the cursor.  TRANSFORM returns NIL to do nothing."
  (%with-editor app
    (lambda (ed)
      (let ((text (text-string ed)) (off (%editor-offset ed)))
        (multiple-value-bind (new new-off) (funcall transform text off)
          (if new (%editor-set-text ed new (or new-off off))
              (message-box "No enclosing form here." (logior +mf-information+ +mf-ok-button+))))))))

(defun do-wrap-paren (app)
  "Wrap the form at the cursor in a new pair of parentheses."
  (%struct-edit app
    (lambda (text off)
      (multiple-value-bind (s e) (%sexp-bounds text off)
        (when s
          (values (concatenate 'string (subseq text 0 s) "(" (subseq text s e) ")"
                               (subseq text e))
                  (1+ s)))))))

(defun do-splice (app)
  "Remove the parentheses of the form enclosing the cursor (paredit splice)."
  (%struct-edit app
    (lambda (text off)
      (multiple-value-bind (s e) (%sexp-bounds text off)
        (when (and s (> e s))
          (values (concatenate 'string (subseq text 0 s) (subseq text (1+ s) (1- e))
                               (subseq text e))
                  (max s (1- off))))))))

(defun do-raise (app)
  "Replace the form enclosing the cursor with the innermost form at the cursor."
  (%struct-edit app
    (lambda (text off)
      (multiple-value-bind (is ie) (%sexp-bounds text off)
        (when is
          ;; the form to keep is the inner one at point; its parent is the form
          ;; just outside it -- find the parent by probing one char before IS
          (multiple-value-bind (ps pe) (%sexp-bounds text (max 0 (1- is)))
            (when (and ps (< ps is) (>= pe ie))
              (values (concatenate 'string (subseq text 0 ps) (subseq text is ie)
                                   (subseq text pe))
                      ps))))))))

(defun %sexp-span-at (str from)
  "From FROM, skip whitespace and line comments, then return (values START END)
of the one sexp beginning there — an atom, string, or balanced () list, with
leading reader prefixes (' ` , ,@) — or NIL when none remains."
  (let ((len (length str)) (i from))
    (loop while (< i len) do
      (let ((c (char str i)))
        (cond ((member c '(#\Space #\Tab #\Newline #\Return #\Page)) (incf i))
              ((char= c #\;) (loop while (and (< i len) (char/= (char str i) #\Newline)) do (incf i)))
              (t (return)))))
    (when (< i len)
      (let ((start i))
        (loop while (and (< i len) (member (char str i) '(#\' #\` #\,))) do
          (incf i) (when (and (< i len) (char= (char str i) #\@)) (incf i)))
        (when (< i len)
          (let ((c (char str i)))
            (cond
              ((char= c #\()
               (let ((depth 0))
                 (loop while (< i len) do
                   (let ((d (char str i)))
                     (cond ((char= d #\;) (loop while (and (< i len) (char/= (char str i) #\Newline)) do (incf i)))
                           ((char= d #\")
                            (incf i) (loop while (< i len) do
                              (let ((e (char str i))) (incf i)
                                (cond ((char= e #\\) (incf i)) ((char= e #\") (return))))))
                           ((and (char= d #\#) (< (1+ i) len) (char= (char str (1+ i)) #\\)) (incf i 3))
                           ((char= d #\() (incf depth) (incf i))
                           ((char= d #\)) (incf i) (decf depth) (when (zerop depth) (return)))
                           (t (incf i)))))))
              ((char= c #\")
               (incf i) (loop while (< i len) do
                 (let ((e (char str i))) (incf i)
                   (cond ((char= e #\\) (incf i)) ((char= e #\") (return))))))
              (t (loop while (and (< i len)
                                  (not (member (char str i)
                                               '(#\Space #\Tab #\Newline #\Return #\Page #\( #\) #\" #\;))))
                       do (incf i))))))
        (values start i)))))

(defun do-slurp (app)
  "Slurp-forward: extend the form at the cursor to absorb the next sexp after it."
  (%struct-edit app
    (lambda (text off)
      (multiple-value-bind (s e) (%sexp-bounds text off)
        (when (and s (> e s))
          (let ((cp (1- e)))                                  ; the close paren
            (multiple-value-bind (n0 n1) (%sexp-span-at text e)
              (declare (ignore n0))
              (when n1
                (values (concatenate 'string (subseq text 0 cp) (subseq text (1+ cp) n1)
                                     ")" (subseq text n1))
                        off)))))))))

(defun do-barf (app)
  "Barf-forward: expel the last sexp of the form at the cursor out past its `)'."
  (%struct-edit app
    (lambda (text off)
      (multiple-value-bind (s e) (%sexp-bounds text off)
        (when (and s (> (- e s) 2))
          (let ((cp (1- e)) (last nil) (i (1+ s)))
            (loop (multiple-value-bind (a b) (%sexp-span-at text i)
                    (if (and a (< a cp)) (progn (setf last (cons a b) i b)) (return))))
            (when last
              (let* ((l0 (car last)) (l1 (min (cdr last) cp))
                     (trimmed (string-right-trim '(#\Space #\Tab #\Newline #\Return)
                                                 (subseq text (1+ s) l0))))
                (values (concatenate 'string (subseq text 0 (1+ s)) trimmed ") "
                                     (subseq text l0 l1) (subseq text (1+ cp)))
                        off)))))))))

(defparameter *snippets*
  '(("defun"        . "(defun name (args)~%  )")
    ("defmacro"     . "(defmacro name (args)~%  )")
    ("defclass"     . "(defclass name ()~%  ((slot :initarg :slot :accessor name-slot)))")
    ("defmethod"    . "(defmethod name ((arg type))~%  )")
    ("defgeneric"   . "(defgeneric name (args))")
    ("defvar"       . "(defvar *name* value)")
    ("let"          . "(let ((var value))~%  )")
    ("loop collect" . "(loop for x in list~%      collect x)")
    ("handler-case" . "(handler-case~%    (progn )~%  (error (e) ))")
    ("dotimes"      . "(dotimes (i n)~%  )"))
  "Code templates for Insert snippet.")

(defun do-insert-snippet (app)
  "Pick a code template and insert it at the cursor (continuation lines indented
to the cursor's column)."
  (%with-editor app
    (lambda (ed)
      (let ((chosen (choose-index "Insert snippet" (mapcar #'car *snippets*))))
        (when chosen
          (let* ((indent (make-string (text-cur-col ed) :initial-element #\Space))
                 (body (format nil (cdr (nth chosen *snippets*))))
                 (snippet (with-output-to-string (o)
                            (loop for ch across body do
                              (write-char ch o)
                              (when (char= ch #\Newline) (write-string indent o)))))
                 (text (text-string ed)) (off (%editor-offset ed)))
            (%editor-set-text ed
                              (concatenate 'string (subseq text 0 off) snippet (subseq text off))
                              (+ off (length snippet)))))))))

;;; --- rename a symbol across open editor buffers (preview + confirm) ---------

(defun %rename-occurrences (text old)
  "Offsets of OLD as a whole symbol token in TEXT (case-insensitive)."
  (let ((offs '()) (i 0))
    (loop for p = (%search-token old text i)
          while p do (push p offs) (setf i (+ p (length old))))
    (nreverse offs)))

(defun %rename-in-text (text old new)
  "Replace every whole-token OLD with NEW in TEXT; (values NEW-TEXT COUNT)."
  (let ((offs (%rename-occurrences text old)) (n (length old)))
    (if (null offs) (values text 0)
        (let ((out (make-string-output-stream)) (i 0))
          (dolist (p offs)
            (write-string (subseq text i p) out) (write-string new out) (setf i (+ p n)))
          (write-string (subseq text i) out)
          (values (get-output-stream-string out) (length offs))))))

(defun %line-around (text off)
  "(values LINE-NUMBER TRIMMED-LINE-TEXT) for character offset OFF in TEXT."
  (let* ((off (min off (length text)))
         (ls (let ((p (position #\Newline text :end off :from-end t))) (if p (1+ p) 0)))
         (le (or (position #\Newline text :start off) (length text))))
    (values (1+ (count #\Newline text :end ls))
            (string-trim '(#\Space #\Tab) (subseq text ls le)))))

(defun do-rename (app)
  "Rename a symbol across all open editor buffers: gather whole-token
occurrences, show a preview, and on confirm replace them all.  (Textual, so
occurrences in strings/comments are included — the preview shows what changes.)"
  (let ((old (prompt-line "Rename" "Symbol to rename:" (%point-symbol))))
    (when (and old (plusp (length (string-trim " " old))))
      (setf old (string-trim " " old))
      (let ((new (prompt-line "Rename" (format nil "Rename ~a to:" old) old)))
        (when (and new (plusp (length (string-trim " " new))))
          (setf new (string-trim " " new))
          (let* ((desk (program-desktop app))
                 (editors (remove-if-not (lambda (w) (typep w 'teditor-window)) (desktop-windows desk)))
                 (hits (loop for w in editors
                             for ed = (editor-window-editor w)
                             for offs = (%rename-occurrences (text-string ed) old)
                             when offs collect (list w ed offs)))
                 (total (reduce #'+ hits :key (lambda (h) (length (third h))) :initial-value 0)))
            (if (zerop total)
                (message-box (format nil "No occurrences of ~a in open editors." old)
                             (logior +mf-information+ +mf-ok-button+))
                (let* ((samples '()) (shown 0))
                  (block gather
                    (dolist (h hits)
                      (let* ((w (first h)) (ed (second h)) (text (text-string ed))
                             (name (or (and (editor-filename ed) (file-namestring (editor-filename ed)))
                                       (window-title w))))
                        (dolist (o (third h))
                          (multiple-value-bind (ln lt) (%line-around text o)
                            (push (format nil "~a:~d: ~a" name ln (tvision::%ellipsize lt 48)) samples))
                          (when (>= (incf shown) 8) (return-from gather))))))
                  (let ((preview (format nil "Rename ~d occurrence~:p of  ~a  ->  ~a  in ~d buffer~:p:~%~%~{  ~a~%~}~a~%Proceed?"
                                         total old new (length hits) (nreverse samples)
                                         (if (> total shown) (format nil "  ...and ~d more~%" (- total shown)) ""))))
                    (when (= +cm-yes+ (message-box preview
                                                   (logior +mf-confirmation+ +mf-yes-button+ +mf-no-button+)))
                      (dolist (h hits)
                        (let* ((w (first h)) (ed (second h)) (off (%editor-offset ed)))
                          (multiple-value-bind (nt cnt) (%rename-in-text (text-string ed) old new)
                            (declare (ignore cnt))
                            (set-text ed nt) (setf (text-modified ed) t)
                            (%macro-set-cursor ed (min off (length nt)))
                            (draw-view w))))
                      (message-box (format nil "Renamed ~d occurrence~:p." total)
                                   (logior +mf-information+ +mf-ok-button+))))))))))))

;;; --- session save/restore --------------------------------------------------

(defun %window-bounds-list (w)
  (let ((b (get-bounds w))) (list (rect-ax b) (rect-ay b) (rect-bx b) (rect-by b))))

(defun do-session-save (app)
  "Save the desktop: each REPL's package and each file-backed editor's path,
cursor line and window geometry (so the layout comes back on restore)."
  (ignore-errors
   (with-open-file (s (session-file) :direction :output :if-exists :supersede
                                     :if-does-not-exist :create)
     (let ((wins '()))
       (dolist (w (reverse (group-subviews (program-desktop app))))   ; back-to-front
         (when (typep w 'twindow)
           (let ((rv (find-if (lambda (v) (typep v 'trepl-view)) (group-subviews w)))
                 (ed (and (typep w 'teditor-window) (editor-window-editor w))))
             (cond
               (rv (push (list :repl :package (package-name (repl-package rv))
                               :bounds (%window-bounds-list w)) wins))
               ((and ed (editor-filename ed))
                (push (list :editor :file (namestring (editor-filename ed))
                            :line (1+ (text-cur-line ed)) :bounds (%window-bounds-list w)) wins))))))
       (prin1 (list :version 2 :windows (nreverse wins)) s))))
  (message-box "Session saved." (logior +mf-information+ +mf-ok-button+)))

(defun %restore-window (app wspec)
  ;; WSPEC is a tagged list (:repl ...props) / (:editor ...props); read props
  ;; from the CDR (the head is the type tag, not a plist key).
  (let ((props (cdr wspec)))
    (ecase (first wspec)
      (:repl (open-repl-window app :package (getf props :package)
                               :bounds (let ((b (getf props :bounds))) (and b (apply #'make-trect b)))))
      (:editor (let ((file (getf props :file)))
                 (when (and file (probe-file file))
                   (let ((bounds (let ((b (getf props :bounds)))
                                   (if b (apply #'make-trect b) (make-trect 2 1 78 22)))))
                     (multiple-value-bind (w ed)
                         (make-edit-window bounds :title (file-namestring file) :filename file)
                       (insert (program-desktop app) w)
                       (when (getf props :line) (text-goto ed (getf props :line) 0))
                       (focus w)))))))))

(defun do-session-load (app)
  (let ((data (ignore-errors
               (with-open-file (s (session-file) :if-does-not-exist nil)
                 (and s (read s nil nil))))))
    (cond
      ((and (consp data) (eql (getf data :version) 2))           ; new format
       (dolist (wspec (getf data :windows))
         (handler-case (%restore-window app wspec)
           (error (e) (err-box e)))))
      ((and (consp data) (eq (car data) :repls))                 ; legacy format
       (dolist (pkg (getf data :repls)) (open-repl-window app :package pkg)))
      (t (message-box "No saved session." (logior +mf-information+ +mf-ok-button+))))))

;;; --- options ---------------------------------------------------------------

(defun do-theme (app)
  (multiple-value-bind (ok fg bg) (color-dialog :title "Desktop color" :fg 7 :bg 0)
    (when ok
      (setf (aref tvision::+app-palette-color+ 38) (make-attr fg bg))
      (draw-view app)
      (when *screen* (flush-screen *screen*)))))

(defvar *rgb-themes* (list (cons "VGA" tvision:+theme-vga+)
                           (cons "Modern" tvision:+theme-modern+)
                           (cons "Green" tvision:+theme-green+)
                           (cons "Amber" tvision:+theme-amber+)))
(defvar *rgb-theme-idx* 0)

(defun do-rgb-theme (app)
  "Cycle the 16-colour RGB theme (VGA <-> Modern) and repaint."
  (setf *rgb-theme-idx* (mod (1+ *rgb-theme-idx*) (length *rgb-themes*)))
  (let ((entry (nth *rgb-theme-idx* *rgb-themes*)))
    (set-color-theme (cdr entry))
    (draw-view app)
    (when *screen* (screen-invalidate *screen*) (flush-screen *screen*))))

(defun toggle-msg (name on)
  (message-box (format nil "~a ~:[off~;on~]." name on) (logior +mf-information+ +mf-ok-button+)))

;;; --- arglist echo + auto-close ---------------------------------------------

(defun %first-token (s pkg)
  (let ((end (or (position-if (lambda (c) (member c '(#\Space #\Tab #\( #\) #\Newline))) s)
                 (length s))))
    (when (plusp end)
      (let ((*package* pkg))
        (ignore-errors
         (let ((sym (read-from-string (subseq s 0 end) nil nil)))
           (and (symbolp sym) sym)))))))

(defun operator-before (col line pkg)
  "The operator symbol of the innermost open form left of COL, or NIL."
  (let ((depth 0) (i (1- (min col (length line)))))
    (loop while (>= i 0) do
      (let ((ch (char line i)))
        (cond ((char= ch #\)) (incf depth))
              ((char= ch #\()
               (if (zerop depth)
                   (return-from operator-before (%first-token (subseq line (1+ i)) pkg))
                   (decf depth)))))
      (decf i))
    nil))

(defun update-arglist-hint (app rv)
  (setf (arglist-hint app)
        (ignore-errors
         (let ((sym (operator-before (text-cur-col rv)
                                     (tvision::current-line-string rv)
                                     (repl-package rv))))
           (when (and sym (fboundp sym))
             (format nil "(~(~a~)~{ ~(~a~)~})" sym
                     (sb-introspect:function-lambda-list sym)))))))

(defun %macro-indent-spec (name)
  "Indentation spec for operator NAME (a lowercased string) when it is a macro
with a &body/&rest argument -- the count of params before it, so the editor
indents user macros like built-in special forms.  NIL otherwise."
  (let ((sym (or (ignore-errors (find-symbol (string-upcase name) *package*))
                 (ignore-errors (find-symbol (string-upcase name) :cl)))))
    (when (and sym (macro-function sym))
      (let ((ll (ignore-errors (sb-introspect:function-lambda-list sym))) (count 0))
        (when (listp ll)
          (dolist (p ll nil)
            (cond ((member p '(&body &rest)) (return count))
                  ((and (symbolp p) (plusp (length (symbol-name p)))
                        (char= (char (symbol-name p) 0) #\&)) nil)  ; skip &optional/&key/...
                  (t (incf count)))))))))

(setf tvision:*lisp-indent-hook* #'%macro-indent-spec)

(defun %inspect-goto (value)
  "Jump to VALUE's definition (for the inspector's `g' key): symbols, classes
and named functions resolve to a source location."
  (let ((sym (typecase value
               (symbol value)
               (class (class-name value))
               (function (nth-value 2 (function-lambda-expression value)))
               (t nil))))
    (if (and sym (symbolp sym))
        (goto-definition-of *application* sym)
        (message-box "No source location for this value."
                     (logior +mf-information+ +mf-ok-button+)))))

(setf tvision:*inspect-goto-hook* #'%inspect-goto)

(defun %point-in-string/comment-p (view)
  "True when the cursor in text VIEW sits inside a string literal or a ; comment
 (so auto-close should stay out of the way)."
  (let ((text (text-string view)) (off (%editor-offset view))
        (i 0) (len 0) (in-str nil) (in-com nil))
    (setf len (length text))
    (loop while (< i off) do
      (let ((c (char text i)))
        (cond
          (in-com (when (char= c #\Newline) (setf in-com nil)) (incf i))
          (in-str (cond ((char= c #\\) (incf i 2))
                        ((char= c #\") (setf in-str nil) (incf i))
                        (t (incf i))))
          ((char= c #\;) (setf in-com t) (incf i))
          ((char= c #\") (setf in-str t) (incf i))
          ((and (char= c #\#) (< (1+ i) len) (char= (char text (1+ i)) #\\)) (incf i 3))
          (t (incf i)))))
    (or in-str in-com)))

(defun maybe-auto-close (app event)
  "When auto-close is on, typing an opening ( [ { or \" inserts the matching pair
with the cursor between -- in the focused editor or REPL, but never inside a
string or comment (so it won't fight existing literals)."
  (when (and (auto-close app) (= (event-type event) +ev-key-down+))
    (let* ((ch (event-char-code event))
           (c (and (plusp ch) (< ch char-code-limit) (code-char ch)))
           (pair (case c (#\( "()") (#\[ "[]") (#\{ "{}") (#\" "\"\""))))
      (when pair
        (let ((view (%current-text-view app)) (rv (current-repl app)))
          (when (and view (typep view 'ttext-view)
                     (not (and (eq view rv) (repl-busy rv)))
                     (tvision::can-edit-here-p view)
                     (not (%point-in-string/comment-p view)))
            (insert-string view pair)
            (setf (text-cur-col view) (1- (text-cur-col view)))
            (draw-view view)
            (clear-event event)
            t))))))

;;; --- context menu ----------------------------------------------------------

(defun repl-context-menu ()
  (new-menu
   (menu-item "Cu~t~"       +cm-cut+      :key-text "Ctrl-X")
   (menu-item "~C~opy"      +cm-copy+     :key-text "Ctrl-C")
   (menu-item "~P~aste"     +cm-paste+    :key-text "Ctrl-V")
   (menu-separator)
   (menu-item "~I~nspect *" +cm-inspect+  :key-text "F8")
   (menu-item "~M~acroexpand..." +cm-macroexpand+)
   (menu-item "~D~escribe..."    +cm-describe+)
   (menu-separator)
   (menu-item "I~n~terrupt" +cm-interrupt+)))

;;; --- event dispatch --------------------------------------------------------

(defmethod handle-event ((app tvlisp-app) event)
  ;; Alt-0 -> Window list, from anywhere
  (when (and (= (event-type event) +ev-key-down+)
             (logtest (event-modifiers event) +md-alt+)
             (= (event-char-code event) (char-code #\0)))
    (do-window-list app) (clear-event event))
  ;; Ctrl-F5 -> Size/Move the active window.  The menu shortcut machinery
  ;; ignores modifiers (F5 = Zoom would also fire for Ctrl-F5), so intercept it
  ;; here -- before the menu bar -- and issue cmResize instead.
  (when (and (= (event-type event) +ev-key-down+)
             (= (event-key-code event) +kb-f5+)
             (logtest (event-modifiers event) +md-ctrl+))
    (put-event app (make-event :type +ev-command+ :command +cm-resize+))
    (clear-event event))
  ;; Alt-Q -> re-indent the whole top-level form in the focused editor
  (when (and (= (event-type event) +ev-key-down+)
             (logtest (event-modifiers event) +md-alt+)
             (= (event-char-code event) (char-code #\q))
             (current-editor-window app))
    (let ((ed (editor-window-editor (current-editor-window app))))
      (tvision::text-snapshot ed) (lisp-indent-sexp ed) (draw-view ed))
    (clear-event event))
  ;; Ctrl-keys handled before the text view swallows them -- but NOT when a
  ;; browser window is focused, so it can claim Ctrl-B/F/R for navigation.
  (when (and (= (event-type event) +ev-key-down+)
             (not (typep (group-current (program-desktop app)) 'thtml-window)))
    (let ((k (event-key-code event)))
      (cond
        ((= k +kb-ctrl-c+)
         (let ((rv (current-repl app)))
           (when (and rv (repl-busy rv)) (repl-interrupt rv) (clear-event event))))
        ((= k +kb-ctrl-f+) (do-find app) (clear-event event))
        ((= k +kb-ctrl-l+) (do-find-next app) (clear-event event))
        ((= k +kb-ctrl-r+) (do-history-search (current-repl app)) (clear-event event))
        ((and (= k 19) (current-editor-window app)) ; Ctrl-S: save focused editor
         (do-save-editor app) (clear-event event))
        ((= k +kb-ctrl-rbracket+)                   ; Ctrl-]: jump to matching paren
         (let ((v (%current-text-view app)))
           (when (and v (match-paren-jump v)) (draw-view v) (clear-event event))))
        ((and (logtest (event-modifiers event) +md-alt+) (= (event-char-code event) 46)) ; M-.
         (do-goto-definition (current-repl app) app) (clear-event event))
        ((and (logtest (event-modifiers event) +md-alt+) (= (event-char-code event) 44)) ; M-,
         (do-nav-back app) (clear-event event))
        ;; Tab in an editor: complete when the cursor follows a symbol, else indent
        ((and (= k +kb-tab+) (current-editor-window app)
              (let ((ed (editor-window-editor (current-editor-window app))))
                (and (plusp (text-cur-col ed))
                     (<= (text-cur-col ed) (length (current-line-string ed)))
                     (%hs-symchar-p (char (current-line-string ed) (1- (text-cur-col ed)))))))
         (do-editor-complete app) (clear-event event)))))
  ;; auto-close parens
  (maybe-auto-close app event)
  ;; right-click context menu
  (when (and (= (event-type event) +ev-mouse-down+)
             (logtest (event-mouse-buttons event) +mb-right+)
             (current-repl app))
    (let ((p (event-mouse-where event)))
      (popup-menu (repl-context-menu) (point-x p) (1+ (point-y p))))
    (clear-event event))
  (call-next-method)
  ;; refresh the arglist hint (call-next-method may have consumed the event, so
  ;; don't gate on its type here)
  (let ((rv (current-repl app)))
    (when rv (update-arglist-hint app rv)))
  (when (= (event-type event) +ev-command+)
    (let ((c (event-command event)) (rv (current-repl app)))
      (flet ((with-repl (fn) (when rv (funcall fn rv) (draw-view rv))))
        (cond
          ((= c +cm-new-repl+) (open-repl-window app) (clear-event event))
          ((= c +cm-clear+)    (with-repl #'repl-clear) (clear-event event))
          ((= c +cm-cut+)      (with-repl #'cut-selection) (clear-event event))
          ((= c +cm-copy+)     (with-repl #'copy-selection) (clear-event event))
          ((= c +cm-paste+)    (with-repl #'paste-clipboard) (clear-event event))
          ((= c +cm-inspect+)  (when rv (repl-inspect (repl-hvar rv '*) "*")) (clear-event event))
          ((= c +cm-inspect-expr+) (do-inspect-expr rv) (clear-event event))
          ((= c +cm-macroexpand+) (do-macroexpand app) (clear-event event))
          ((= c +cm-describe+)    (do-describe rv) (clear-event event))
          ((= c +cm-documentation+) (do-documentation rv) (clear-event event))
          ((= c +cm-disassemble+) (do-disassemble rv) (clear-event event))
          ((= c +cm-apropos+)     (do-apropos rv) (clear-event event))
          ((= c +cm-classes+)     (do-classes rv app) (clear-event event))
          ((= c +cm-winlist+)     (do-window-list app) (clear-event event))
          ((= c +cm-gotodef+)     (do-goto-definition rv app) (clear-event event))
          ((= c +cm-funcbrowser+) (do-function-browser rv app) (clear-event event))
          ((= c +cm-browse+)      (do-browse app) (clear-event event))
          ((= c +cm-sbclman+)     (do-sbcl-manual app) (clear-event event))
          ((= c +cm-bhistory+)    (do-browser-history app) (clear-event event))
          ((= c +cm-hslookup+)    (do-hyperspec-lookup app) (clear-event event))
          ((= c +cm-step+)        (do-step rv) (clear-event event))
          ((= c +cm-profile+)     (do-profile rv app) (clear-event event))
          ((= c +cm-profile-det+) (do-profile-deterministic rv) (clear-event event))
          ((= c +cm-whocalls+)    (do-xref rv app :calls) (clear-event event))
          ((= c +cm-whorefs+)     (do-xref rv app :references) (clear-event event))
          ((= c +cm-whobinds+)    (do-xref rv app :binds) (clear-event event))
          ((= c +cm-whosets+)     (do-xref rv app :sets) (clear-event event))
          ((= c +cm-whomacro+)    (do-xref rv app :macroexpands) (clear-event event))
          ((= c +cm-packages+)    (do-packages rv) (clear-event event))
          ((= c +cm-systems+)     (do-systems rv) (clear-event event))
          ((= c +cm-load-buffer+) (do-load-buffer app) (clear-event event))
          ((= c +cm-compile-buffer+) (do-compile-buffer app) (clear-event event))
          ((= c +cm-compile-defun+)  (do-compile-defun app) (clear-event event))
          ((= c +cm-calltree+)       (do-call-tree app) (clear-event event))
          ((= c +cm-break-entry+)    (do-break-on-entry rv) (clear-event event))
          ((= c +cm-eval-defun+)  (do-eval-defun app) (clear-event event))
          ((= c +cm-eval-region+) (do-eval-region app) (clear-event event))
          ((= c +cm-nav-back+)    (do-nav-back app) (clear-event event))
          ((= c +cm-complete+)    (do-editor-complete app) (clear-event event))
          ((= c +cm-comment+)     (do-comment-region app) (clear-event event))
          ((= c +cm-snippet+)     (do-insert-snippet app) (clear-event event))
          ((= c +cm-rename+)      (do-rename app) (clear-event event))
          ((= c +cm-wrap-paren+)  (do-wrap-paren app) (clear-event event))
          ((= c +cm-splice+)      (do-splice app) (clear-event event))
          ((= c +cm-raise+)       (do-raise app) (clear-event event))
          ((= c +cm-slurp+)       (do-slurp app) (clear-event event))
          ((= c +cm-barf+)        (do-barf app) (clear-event event))
          ((= c +cm-find+)        (do-find app) (clear-event event))
          ((= c +cm-find-next+)   (do-find-next app) (clear-event event))
          ((= c +cm-replace+)     (do-replace app) (clear-event event))
          ((= c +cm-goto-line+)   (do-goto-line app) (clear-event event))
          ((= c +cm-isearch+)     (do-isearch app) (clear-event event))
          ((= c +cm-wrap+)        (do-toggle-wrap app) (clear-event event))
          ((= c +cm-trace+)       (do-trace rv) (clear-event event))
          ((= c +cm-trace-pkg+)   (do-trace-package rv) (clear-event event))
          ((= c +cm-trace-snap+)  (do-trace-snapshots rv) (clear-event event))
          ((= c +cm-untrace-all+) (do-untrace-all rv) (clear-event event))
          ((= c +cm-histsearch+)  (do-history-search rv) (clear-event event))
          ((= c +cm-new-file+)    (do-new-editor app) (clear-event event))
          ((= c +cm-editor+)      (do-open-editor app) (clear-event event))
          ((= c +cm-save+)        (do-save-editor app) (clear-event event))
          ((= c +cm-saveas+)      (do-saveas-editor app) (clear-event event))
          ((= c +cm-save-all+)    (do-save-all app) (clear-event event))
          ((= c +cm-interrupt+)   (when rv (repl-interrupt rv)) (clear-event event))
          ((= c +cm-session-save+) (do-session-save app) (clear-event event))
          ((= c +cm-session-load+) (do-session-load app) (clear-event event))
          ((= c +cm-theme+)       (do-theme app) (clear-event event))
          ((= c +cm-rgb-theme+)   (do-rgb-theme app) (clear-event event))
          ((= c +cm-color-demo+)  (do-color-demo app) (clear-event event))
          ((= c +cm-pprint+)      (setf *print-pretty* (not *print-pretty*))
                                  (toggle-msg "Pretty-print" *print-pretty*) (clear-event event))
          ((= c +cm-timing+)      (setf *repl-time* (not *repl-time*))
                                  (toggle-msg "Eval timing" *repl-time*) (clear-event event))
          ((= c +cm-autoclose+)   (setf (auto-close app) (not (auto-close app)))
                                  (toggle-msg "Auto-close parens" (auto-close app)) (clear-event event))
          ((= c +cm-help+)        (open-help +hc-repl+ "tvlisp Help") (clear-event event))
          ((= c +cm-load+)
           (let ((path (file-open-dialog :title "Load Lisp file")))
             (when (and rv path) (repl-load-file rv path) (focus rv)))
           (clear-event event))
          ((= c +cm-reload+)
           (cond
             ((null rv) (message-box "No REPL open." (logior +mf-information+ +mf-ok-button+)))
             ((null (repl-last-file rv))
              (message-box "No file has been loaded yet." (logior +mf-information+ +mf-ok-button+)))
             (t (repl-load-file rv (repl-last-file rv)) (focus rv)))
           (clear-event event))
          ((= c +cm-savetx+)
           (let ((path (file-save-dialog :title "Save transcript")))
             (when (and rv path) (text-save-file rv path)))
           (clear-event event))
          ((= c +cm-savescript+) (do-save-script rv) (clear-event event))
          ((= c +cm-tile+)     (tile (program-desktop app)) (clear-event event))
          ((= c +cm-cascade+)  (cascade (program-desktop app)) (clear-event event))
          ((= c +cm-threads+)  (open-thread-window app) (clear-event event)))))))

;;; --- entry points ----------------------------------------------------------

(defun register-tvlisp-help ()
  (register-help +hc-repl+
                 (format nil "tvlisp -- a Turbo Vision Lisp REPL / mini-IDE~%~%~
Type a Lisp form and press Enter to evaluate it; an open form continues on the~%~
next line.  Up/Down recall input; *, +, / hold recent values (per window).~%~
Each REPL runs on its own thread; an error opens the restart debugger (with a~%~
Backtrace button).~%~%~
Lisp menu: Inspect, Macroexpand, Describe, Documentation, Disassemble, Apropos,~%~
Class/Package/System browsers, Load buffer.~%~
Edit menu: Ctrl-F find, Ctrl-L find-next, Ctrl-R history search, Ctrl-C interrupt.~%~
Options: theme, pretty-print, eval timing, auto-close parens.~%~
F2 new REPL, F3 clear, F4 tile, F5 cascade, F6 next, F8 inspect, F9 threads.")))

(defmethod tvision::setup ((app tvlisp-app))
  (register-tvlisp-help)
  ;; Compile REPL-defined code with full debug so the stepper can step *into*
  ;; user functions and the debugger shows frame locals.
  (proclaim '(optimize (debug 3) (speed 1)))
  (setf (view-help-ctx (program-desktop app)) +hc-repl+)
  (open-repl-window app :maximized t))

(defun %report-event-error (c)
  "Show an unexpected UI-thread error in a dialog instead of crashing the IDE."
  (ignore-errors
   (message-box (format nil "Unexpected error:~%~a" c)
                (logior +mf-error+ +mf-ok-button+))))

(defun main ()
  (setf tvision:*event-error-hook* #'%report-event-error)
  (load-browse-history)
  (run 'tvlisp-app))

(defun toplevel ()
  (handler-case (main)
    (error (e)
      (format *error-output* "~&Error: ~a~%" e)
      (sb-ext:exit :code 1)))
  (sb-ext:exit :code 0))
