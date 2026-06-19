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
  (require :sb-sprof))

;;; --- commands --------------------------------------------------------------

(defparameter +cm-new-repl+    300)
(defparameter +cm-clear+       301)
(defparameter +cm-tile+        302)
(defparameter +cm-cascade+     303)
(defparameter +cm-inspect+     304)
(defparameter +cm-load+        305)
(defparameter +cm-savetx+      306)
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
(defparameter +cm-editor+      321)
(defparameter +cm-load-buffer+ 322)
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
(defparameter +cm-step+        335)
(defparameter +cm-new-file+    336)
(defparameter +cm-save+        337)
(defparameter +cm-saveas+      338)
(defparameter +cm-profile+     339)
(defparameter +cm-profile-det+ 340)
(defparameter +cm-browse+      341)
(defparameter +cm-bhistory+    342)

(defparameter +hc-repl+ 1)
(defparameter +history-file+ (merge-pathnames ".tvlisp_history" (user-homedir-pathname)))
(defparameter +session-file+ (merge-pathnames ".tvlisp_session" (user-homedir-pathname)))

(defparameter +kb-ctrl-c+ 3)
(defparameter +kb-ctrl-f+ 6)
(defparameter +kb-ctrl-l+ 12)
(defparameter +kb-ctrl-r+ 18)

(defclass tvlisp-app (tapplication)
  ((repl-count   :initform 0   :accessor repl-count)
   (find-last    :initform ""  :accessor find-last)
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
      (menu-item "~L~oad file..."    +cm-load+     :key-code +kb-f7+ :key-text "F7")
      (menu-item "Save ~t~ranscript..." +cm-savetx+)
      (menu-separator)
      (menu-item "Save sessio~n~"    +cm-session-save+)
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
      (menu-item "Find ~n~ext"  +cm-find-next+ :key-text "Ctrl-L")
      (menu-item "~H~istory search" +cm-histsearch+ :key-text "Ctrl-R")
      (menu-separator)
      (menu-item "I~n~terrupt eval" +cm-interrupt+ :key-text "Ctrl-C")))
   (sub-menu "~L~isp"
     (new-menu
      (menu-item "~I~nspect *"        +cm-inspect+ :key-code +kb-f8+ :key-text "F8")
      (menu-item "Inspect ~e~xpr..."  +cm-inspect-expr+)
      (menu-separator)
      (menu-item "~G~o to definition..." +cm-gotodef+ :key-text "Alt-.")
      (menu-item "~F~unction browser..." +cm-funcbrowser+)
      (menu-item "~W~ho calls..."        +cm-whocalls+)
      (menu-item "Who ~r~eferences..."   +cm-whorefs+)
      (menu-separator)
      (menu-item "S~t~ep form..."     +cm-step+)
      (menu-item "Profi~l~e..."       +cm-profile+)
      (menu-item "Determi~n~istic profile..." +cm-profile-det+)
      (menu-item "~M~acroexpand..."   +cm-macroexpand+)
      (menu-item "~D~escribe..."      +cm-describe+)
      (menu-item "Doc~u~mentation..." +cm-documentation+)
      (menu-item "Dis~a~ssemble..."   +cm-disassemble+)
      (menu-item "A~p~ropos..."       +cm-apropos+)
      (menu-separator)
      (menu-item "~C~lass browser..." +cm-classes+)
      (menu-item "Pac~k~ages..."      +cm-packages+)
      (menu-item "~S~ystems..."       +cm-systems+)
      (menu-item "Load ~b~uffer"      +cm-load-buffer+)))
   (sub-menu "~O~ptions"
     (new-menu
      (menu-item "~T~heme..."          +cm-theme+)
      (menu-item "~P~retty-print"      +cm-pprint+)
      (menu-item "Eval t~i~ming"       +cm-timing+)
      (menu-item "~A~uto-close parens" +cm-autoclose+)))
   (sub-menu "~W~indow"
     (new-menu
      (menu-item "~N~ext"    +cm-next+    :key-code +kb-f6+ :key-text "F6")
      (menu-item "~T~ile"    +cm-tile+    :key-code +kb-f4+ :key-text "F4")
      (menu-item "~C~ascade" +cm-cascade+ :key-code +kb-f5+ :key-text "F5")
      (menu-separator)
      (menu-item "T~h~reads..." +cm-threads+ :key-code +kb-f9+ :key-text "F9")))
   (sub-menu "~H~elp"
     (new-menu
      (menu-item "Hyper~S~pec / browse..." +cm-browse+)
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
         (info (or hint
                   (format nil "~a | ~d thr~:[~; | busy~]"
                           (if rv (package-name (repl-package rv)) "-")
                           (length (sb-thread:list-all-threads))
                           (and rv (repl-busy rv)))))
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

(defun open-repl-window (app &key maximized (package nil))
  (let* ((desk (program-desktop app))
         (n (incf (repl-count app)))
         (dw (point-x (view-size desk))) (dh (point-y (view-size desk)))
         (bounds (if maximized
                     (make-trect 0 0 dw dh)
                     (let ((ox (* (mod (1- n) 5) 3)) (oy (mod (1- n) 5)))
                       (make-trect ox oy (min dw (+ ox 72)) (min dh (+ oy 22)))))))
    (multiple-value-bind (w rv)
        (make-repl-window bounds :title (format nil "Lisp REPL ~d" n)
                                 :history-file +history-file+)
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
  "Resolve HREF against the current BASE location (URL or file path)."
  (let* ((hash (position #\# href)) (h (if hash (subseq href 0 hash) href)))
    (cond
      ((zerop (length h)) base)                       ; pure fragment
      ((%url-p h) h)                                  ; absolute URL
      ((%url-p base)                                  ; relative against a URL
       (let* ((p (search "://" base))
              (slash (position #\/ base :start (+ p 3)))
              (origin (if slash (subseq base 0 slash) base))
              (path (if slash (subseq base slash) "/")))
         (if (and (plusp (length h)) (char= (char h 0) #\/))
             (concatenate 'string origin (%normalize-path h))
             (let ((dir (subseq path 0 (1+ (or (position #\/ path :from-end t) 0)))))
               (concatenate 'string origin (%normalize-path (concatenate 'string dir h)))))))
      (t                                              ; relative against a file
       (namestring (merge-pathnames h (directory-namestring base)))))))

(defun %location-title (loc)
  (let ((s (if (position #\# loc) (subseq loc 0 (position #\# loc)) loc)))
    (let ((slash (position #\/ s :from-end t)))
      (if (and slash (< (1+ slash) (length s))) (subseq s (1+ slash)) s))))

(defclass thtml-window (twindow)
  ((view   :initform nil :accessor hw-view)
   (base   :initform "" :accessor hw-base)
   (back   :initform '() :accessor hw-back-stack)   ; pages behind the current one
   (fwd    :initform '() :accessor hw-fwd-stack)    ; pages ahead (after going Back)
   (titles :initform '() :accessor hw-titles)))     ; (location . <title>) seen so far

(defun hw-label (w loc)
  "How LOC should appear in the history: its <title> if we have one, else the URL."
  (or (cdr (assoc loc (hw-titles w) :test #'string=)) loc))

(defun hw-load (loc) (if (%url-p loc) (%http-get loc) (%read-file-string loc)))

(defun hw-set-title (w)
  (setf (window-title w)
        (format nil "~a  [^B/Bksp back  ^F fwd  ^R reload]"
                (%location-title (hw-base w)))))

(defun hw-go (w loc &key (record t))
  "Load LOC into the window.  When RECORD, treat it as fresh navigation: push
the current page onto the Back stack and drop the Forward stack.  Return T on
a successful load."
  (let ((content (hw-load loc)))
    (cond
      (content
       (when (and record (plusp (length (hw-base w))))
         (push (hw-base w) (hw-back-stack w))
         (setf (hw-fwd-stack w) '()))
       (setf (hw-base w) loc)
       ;; remember the page's <title> for the history list
       (let ((title (html-document-title content)))
         (when title
           (setf (hw-titles w)
                 (cons (cons loc title)
                       (remove loc (hw-titles w) :key #'car :test #'string=)))))
       (hw-set-title w)
       (set-html (hw-view w) content)
       (focus (hw-view w))
       (draw-view w)
       t)
      (t (message-box (format nil "Could not load:~%~a" loc)
                      (logior +mf-error+ +mf-ok-button+))
         nil))))

(defun hw-back (w)
  "Go to the previous page, remembering the current one for Forward."
  (when (hw-back-stack w)
    (let ((target (pop (hw-back-stack w))) (cur (hw-base w)))
      (when (hw-go w target :record nil)
        (push cur (hw-fwd-stack w))))))

(defun hw-forward (w)
  "Go to the next page (undo a Back), remembering the current one for Back."
  (when (hw-fwd-stack w)
    (let ((target (pop (hw-fwd-stack w))) (cur (hw-base w)))
      (when (hw-go w target :record nil)
        (push cur (hw-back-stack w))))))

(defun hw-reload (w)
  (when (plusp (length (hw-base w)))
    (hw-go w (hw-base w) :record nil)))

(defun hw-history-list (w)
  "The full visit history in chronological order (oldest first)."
  (append (reverse (hw-back-stack w)) (list (hw-base w)) (hw-fwd-stack w)))

(defun hw-history-index (w)
  "Position of the current page within (HW-HISTORY-LIST W)."
  (length (hw-back-stack w)))

(defun hw-goto-index (w i)
  "Jump to chronological history entry I, rebuilding the Back/Forward stacks
around it."
  (let ((items (hw-history-list w)))
    (when (and (>= i 0) (< i (length items)) (/= i (hw-history-index w)))
      (setf (hw-back-stack w) (reverse (subseq items 0 i))
            (hw-fwd-stack w)  (subseq items (1+ i)))
      (hw-go w (nth i items) :record nil))))

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

(defun do-browser-history (app)
  "Pop up the focused browser window's history; selecting an entry visits it."
  (let ((w (group-current (program-desktop app))))
    (cond
      ((not (typep w 'thtml-window))
       (message-box "Select a browser window first."
                    (logior +mf-information+ +mf-ok-button+)))
      (t (let* ((items (hw-history-list w))
                (cur (hw-history-index w))
                (labels (loop for loc in items for i from 0
                              collect (format nil "~:[  ~;> ~]~a" (= i cur) (hw-label w loc)))))
           (let ((sel (choose-index "Browser history" labels :start cur)))
             (when sel (hw-goto-index w sel))))))))

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

(defun do-macroexpand (app)
  "Macroexpand-1 a form.  When an editor window is focused, the prompt defaults
to the form at the cursor (the selection, the enclosing s-expression, or the
current line)."
  (let* ((rv (some-repl app))
         (ew (current-editor-window app))
         (default (when ew
                    (string-trim '(#\Space #\Tab #\Newline)
                                 (or (editor-form-at-point (editor-window-editor ew)) ""))))
         (s (prompt-line "Macroexpand" "Form:" (or default ""))))
    (when s
      (handler-case
          (let ((*print-pretty* t)
                (*package* (if rv (repl-package rv) *package*)))
            (show-text-window "Macroexpand-1" (prin1-to-string (macroexpand-1 (read-from-string s)))))
        (error (e) (err-box e))))))

(defun describe-named (rv name)
  (handler-case
      (show-text-window (format nil "Describe ~a" name)
                        (with-output-to-string (s) (describe (read-in rv name) s)))
    (error (e) (err-box e))))

(defun do-describe (rv)
  (let ((s (prompt-line "Describe" "Symbol:"))) (when (and rv s) (describe-named rv s))))

(defun do-documentation (rv)
  (let ((s (prompt-line "Documentation" "Symbol:")))
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
  (let ((s (prompt-line "Disassemble" "Function:")))
    (when (and rv s)
      (handler-case
          (show-text-window (format nil "Disassemble ~a" s)
            (with-output-to-string (o)
              (let ((*standard-output* o)) (disassemble (read-in rv s)))))
        (error (e) (err-box e))))))

(defun do-apropos (rv)
  (let ((s (prompt-line "Apropos" "Substring:")))
    (when (and rv s)
      (let* ((names (sort (mapcar #'prin1-to-string (apropos-list s)) #'string<))
             (chosen (choose-from-list (format nil "Apropos \"~a\" (~d)" s (length names)) names)))
        (when chosen (describe-named rv chosen))))))

(defun do-inspect-expr (rv)
  (let ((s (prompt-line "Inspect" "Expression:")))
    (when (and rv s)
      (handler-case (repl-inspect (eval (read-in rv s)) s)
        (error (e) (err-box e))))))

(defun do-packages (rv)
  (let ((chosen (choose-from-list "Packages"
                                  (sort (mapcar #'package-name (list-all-packages)) #'string<))))
    (when (and rv chosen)
      (let ((p (find-package chosen)))
        (when p
          (setf (repl-package rv) p)
          (repl-print rv (format nil "~%; switched to package ~a~%" (package-name p)))
          (tvision::repl-fresh-prompt rv)
          (draw-view rv))))))

(defun do-systems ()
  (let ((chosen (choose-from-list "ASDF Systems"
                                  (sort (copy-list (asdf:registered-systems)) #'string<))))
    (when chosen
      (let ((out (with-output-to-string (o)
                   (handler-case
                       (let ((*standard-output* o) (*error-output* o)) (asdf:load-system chosen))
                     (error (e) (format o ";; ~a~%" e))))))
        (show-text-window (format nil "Load system ~a" chosen)
                          (if (plusp (length out)) out (format nil "Loaded ~a." chosen)))))))

(defun class-outline (class)
  (flet ((cls-nodes (cs) (mapcar (lambda (c) (make-outline-node (format nil "~a" (class-name c)) nil)) cs))
         (slot-nodes (ss) (mapcar (lambda (s) (make-outline-node
                                               (format nil "~a" (sb-mop:slot-definition-name s)) nil)) ss)))
    (let* ((supers (sb-mop:class-direct-superclasses class))
           (subs (sb-mop:class-direct-subclasses class))
           (slots (ignore-errors (sb-mop:class-slots class)))
           (node (make-outline-node
                  (format nil "Class ~a" (class-name class))
                  (list (make-outline-node (format nil "Superclasses (~d)" (length supers)) (cls-nodes supers))
                        (make-outline-node (format nil "Subclasses (~d)" (length subs)) (cls-nodes subs))
                        (make-outline-node (format nil "Slots (~d)" (length slots)) (slot-nodes slots))))))
      (setf (outline-node-expanded node) t)
      node)))

(defun do-classes (rv)
  (let ((s (prompt-line "Class browser" "Class name:")))
    (when (and rv s)
      (handler-case
          (let ((class (find-class (read-in rv s))))
            (ignore-errors (sb-mop:finalize-inheritance class))
            (open-outline-window (format nil "Class ~a" s) (list (class-outline class))))
        (error (e) (err-box e))))))

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

(defun %offset-to-line (path offset)
  "1-based line number containing character OFFSET in PATH."
  (or (ignore-errors
       (with-open-file (s path)
         (let ((line 1))
           (dotimes (i (or offset 0) line)
             (let ((c (read-char s nil nil)))
               (unless c (return line))
               (when (char= c #\Newline) (incf line)))))))
      1))

(defun goto-source (app type path offset)
  (declare (ignore type))
  (if (and path (probe-file path))
      (let* ((desk (program-desktop app))
             (dw (point-x (view-size desk))) (dh (point-y (view-size desk))))
        (multiple-value-bind (w ed)
            (make-edit-window (make-trect 2 1 (min (- dw 2) 84) (min (- dh 1) 26))
                              :title (file-namestring path) :filename path)
          (insert desk w)
          (when offset (text-goto ed (%offset-to-line path offset) 0))
          (focus w)))
      (message-box (format nil "No source file:~%~a" path) (logior +mf-error+ +mf-ok-button+))))

(defun goto-definition-of (app sym)
  (let ((defs (symbol-definitions sym)))
    (cond
      ((null defs)
       (message-box (format nil "No source location for ~a." sym)
                    (logior +mf-information+ +mf-ok-button+)))
      ((= 1 (length defs)) (apply #'goto-source app (first defs)))
      (t (let* ((labels (mapcar (lambda (d) (format nil "~(~a~)  ~a" (first d) (second d))) defs))
                (chosen (choose-from-list "Definitions" labels)))
           (when chosen
             (apply #'goto-source app (nth (position chosen labels :test #'string=) defs))))))))

(defun do-goto-definition (rv app)
  (let ((s (prompt-line "Go to definition" "Symbol:")))
    (when (and rv s)
      (handler-case (goto-definition-of app (read-in rv s)) (error (e) (err-box e))))))

(defun do-xref (rv app kind)
  (let ((s (prompt-line (format nil "Who ~(~a~)" kind) "Symbol:")))
    (when (and rv s)
      (handler-case
          (let* ((sym (read-in rv s))
                 (entries (ecase kind
                            (:calls (sb-introspect:who-calls sym))
                            (:references (sb-introspect:who-references sym))))
                 (names (sort (remove-duplicates
                               (mapcar (lambda (e) (format nil "~s" (car e))) entries)
                               :test #'string=)
                              #'string<)))
            (if (null names)
                (message-box (format nil "Nothing ~(~a~) ~a." kind s)
                             (logior +mf-information+ +mf-ok-button+))
                (let ((chosen (choose-from-list
                               (format nil "Who ~(~a~) ~a (~d)" kind s (length names)) names)))
                  (when chosen (goto-definition-of app (read-in rv chosen))))))
        (error (e) (err-box e))))))

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

(defun run-profile (form package)
  "Evaluate FORM under sb-sprof (sampling only this thread) and return a plist
 (:total samples :secs seconds :rows (row-plist ...)); each row is
 (:name N :self S :cumul C :self% f :cumul% f :callees (labels...))."
  (let ((*package* package) (t0 (get-internal-real-time)))
    (sb-sprof:reset)
    (sb-sprof:start-profiling :max-samples 200000 :sample-interval 0.001 :mode :time
                              :threads (list sb-thread:*current-thread*))
    (unwind-protect (eval form) (sb-sprof:stop-profiling))
    (let* ((secs (/ (- (get-internal-real-time) t0) internal-time-units-per-second))
           (cg (sb-sprof::make-call-graph sb-sprof::*samples* most-positive-fixnum))
           (total (max 1 (sb-sprof::call-graph-nsamples cg)))
           (flat (sb-sprof::call-graph-flat-nodes cg)))
      (list :total total :secs secs
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

(defun %profile-columns ()
  (vector (make-table-column "Self%"  6 (lambda (r) (getf r :self%))  :numeric t
                             :format (lambda (v) (format nil "~,1f" v)))
          (make-table-column "Cumul%" 7 (lambda (r) (getf r :cumul%)) :numeric t
                             :format (lambda (v) (format nil "~,1f" v)))
          (make-table-column "Samples" 8 (lambda (r) (getf r :self)) :numeric t)
          (make-table-column "Function" 48 (lambda (r) (%fn-name (getf r :name))))))

(defclass tprofile-window (twindow)
  ((data  :initarg :data  :initform nil :accessor profile-data)
   (table :initarg :table :initform nil :accessor profile-table)
   (app   :initarg :app   :initform nil :accessor profile-app)))

(defun show-profile-tree (w)
  "Open a TOutline of the hottest functions, each expandable to its callees."
  (let ((roots (loop for r in (subseq (getf (profile-data w) :rows) 0
                                      (min 30 (length (getf (profile-data w) :rows))))
                     collect (make-outline-node
                              (format nil "~5,1f%  ~a" (getf r :self%) (%fn-name (getf r :name)))
                              (mapcar (lambda (c) (make-outline-node c nil)) (getf r :callees))))))
    (open-outline-window "Call graph (function -> callees)" roots)))

(defmethod handle-event ((w tprofile-window) event)
  (cond
    ((and (= (event-type event) +ev-broadcast+)
          (= (event-command event) +cm-list-item-selected+)
          (profile-table w))
     (let ((row (table-selected-row (profile-table w))))
       (when (and row (symbolp (getf row :name)) (profile-app w))
         (goto-definition-of (profile-app w) (getf row :name))))
     (clear-event event))
    ((and (= (event-type event) +ev-key-down+)
          (member (event-char-code event) (list (char-code #\g) (char-code #\G))))
     (show-profile-tree w)
     (clear-event event))
    (t (call-next-method))))

(defun show-profile-results (app data)
  (let* ((desk (program-desktop app))
         (dw (point-x (view-size desk))) (dh (point-y (view-size desk)))
         (w (min 82 (- dw 2))) (h (min 22 (- dh 2)))
         (win (make-instance 'tprofile-window :data data :app app
                             :title (format nil "Profile — ~d samples, ~,2fs  (Enter:src g:graph s:sort)"
                                            (getf data :total) (getf data :secs))
                             :bounds (make-trect 0 0 w h)))
         (vsb (standard-scrollbar win t))
         (tbl (make-instance 'ttable-view :columns (%profile-columns) :rows (getf data :rows)
                             :sort-col 0 :sort-asc nil
                             :bounds (make-trect 1 1 (1- w) (1- h)))))
    (insert win tbl)
    (attach-scrollbars tbl :vscroll vsb)
    (setf (profile-table win) tbl)
    (move-to win (max 0 (floor (- dw w) 2)) (max 0 (floor (- dh h) 2)))
    (insert desk win)
    (focus tbl)))

(defun do-profile (rv app)
  (let ((s (prompt-line "Profile" "Form to profile:")))
    (when (and rv s)
      (let ((form (read-in rv s)) (pkg (repl-package rv)))
        ;; run on the worker thread so the UI stays responsive (Ctrl-C aborts)
        (repl-call-on-worker rv
          (lambda ()
            (let ((data (run-profile form pkg)))
              (run-on-ui (lambda ()
                           (if (getf data :rows)
                               (show-profile-results app data)
                               (message-box "No samples collected (the form ran too quickly)."
                                            (logior +mf-information+ +mf-ok-button+))))))))))))

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
                    (run-on-ui (lambda ()
                                 (show-text-window (format nil "Deterministic profile: ~a" pkg)
                                                   (or txt "")))))
                (error (e) (run-on-ui (lambda () (err-box e))))))))))))

(defun do-function-browser (rv app)
  (let ((s (prompt-line "Function / GF browser" "Function name:")))
    (when (and rv s)
      (handler-case
          (let* ((sym (read-in rv s)) (fn (and (fboundp sym) (fdefinition sym))))
            (cond
              ((typep fn 'generic-function)
               (let* ((methods (sb-mop:generic-function-methods fn))
                      (labels (mapcar #'method-label methods))
                      (chosen (choose-from-list
                               (format nil "~a — ~d method~:p" s (length methods)) labels)))
                 (when chosen
                   (let* ((m (nth (position chosen labels :test #'string=) methods))
                          (src (ignore-errors
                                (sb-introspect:find-definition-source (sb-mop:method-function m))))
                          (path (and src (sb-introspect:definition-source-pathname src))))
                     (if path
                         (goto-source app :method (namestring path)
                                      (sb-introspect:definition-source-character-offset src))
                         (message-box "No source for that method."
                                      (logior +mf-information+ +mf-ok-button+)))))))
              (fn (goto-definition-of app sym))
              (t (message-box (format nil "~a is not a function." s)
                              (logior +mf-information+ +mf-ok-button+)))))
        (error (e) (err-box e))))))

;;; --- transcript search / history -------------------------------------------

(defun do-find (app)
  (let ((rv (current-repl app)))
    (when rv
      (let ((s (prompt-line "Find" "Search for:" (find-last app))))
        (when s
          (setf (find-last app) s)
          (unless (text-find-and-select rv s :wrap t)
            (message-box "Not found." (logior +mf-information+ +mf-ok-button+)))
          (draw-view rv))))))

(defun do-find-next (app)
  (let ((rv (current-repl app)) (s (find-last app)))
    (when (and rv (plusp (length s)))
      (text-find-and-select rv s :wrap t)
      (draw-view rv))))

(defun do-history-search (rv)
  (when rv
    (let ((chosen (choose-from-list "History"
                    (remove-duplicates (copy-list (repl-history rv))
                                       :test #'string= :from-end t))))
      (when chosen (repl-replace-input rv chosen) (draw-view rv)))))

;;; --- editor + load buffer --------------------------------------------------

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

(defun do-saveas-editor (app)
  "Save the focused editor window under a new path; return T if saved."
  (let ((w (current-editor-window app)))
    (when w
      (let ((path (file-save-dialog :title "Save As")))
        (when path
          (let ((ed (editor-window-editor w)))
            (text-save-file ed path)
            (setf (editor-filename ed) path
                  (window-title w) (file-namestring path))
            (draw-view w)
            t))))))

(defun do-save-editor (app)
  "Save the focused editor window (Save As if it has no filename yet)."
  (let ((w (current-editor-window app)))
    (when w
      (let* ((ed (editor-window-editor w)) (path (editor-filename ed)))
        (if path
            (progn (text-save-file ed path) (draw-view w))
            (do-saveas-editor app))))))

(defun do-load-buffer (app)
  (let* ((win (group-current (program-desktop app)))
         (ed (and (typep win 'teditor-window) (editor-window-editor win)))
         (rv (some-repl app)))
    (cond
      ((not ed) (message-box "Focus an editor window first." (logior +mf-information+ +mf-ok-button+)))
      ((not rv) (message-box "No REPL open." (logior +mf-information+ +mf-ok-button+)))
      (t (let ((text (text-string ed)) (pkg (repl-package rv)))
           ;; evaluate the buffer on the worker so the UI stays responsive
           (repl-call-on-worker rv
             (lambda ()
               (let ((out (with-output-to-string (o)
                            (let ((*standard-output* o) (*error-output* o) (*package* pkg))
                              (handler-case
                                  (with-input-from-string (in text)
                                    (loop for f = (read in nil :eof) until (eq f :eof) do (eval f)))
                                (error (e) (format o ";; ~a~%" e)))))))
                 (run-on-ui (lambda ()
                              (show-text-window "Load buffer"
                                                (if (plusp (length out)) out "Loaded (no output)."))))))))))))

;;; --- session save/restore --------------------------------------------------

(defun do-session-save (app)
  (ignore-errors
   (with-open-file (s +session-file+ :direction :output :if-exists :supersede
                                     :if-does-not-exist :create)
     (let ((repls '()))
       (dolist (w (reverse (group-subviews (program-desktop app))))
         (when (typep w 'twindow)
           (let ((rv (find-if (lambda (v) (typep v 'trepl-view)) (group-subviews w))))
             (when rv (push (package-name (repl-package rv)) repls)))))
       (prin1 (list :repls repls) s))))
  (message-box "Session saved." (logior +mf-information+ +mf-ok-button+)))

(defun do-session-load (app)
  (let ((data (ignore-errors
               (with-open-file (s +session-file+ :if-does-not-exist nil)
                 (and s (read s nil nil))))))
    (if (and (consp data) (eq (car data) :repls))
        (dolist (pkg (getf data :repls))
          (open-repl-window app :package pkg))
        (message-box "No saved session." (logior +mf-information+ +mf-ok-button+)))))

;;; --- options ---------------------------------------------------------------

(defun do-theme (app)
  (multiple-value-bind (ok fg bg) (color-dialog :title "Desktop color" :fg 7 :bg 0)
    (when ok
      (setf (aref tvision::+app-palette-color+ 38) (make-attr fg bg))
      (draw-view app)
      (when *screen* (flush-screen *screen*)))))

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

(defun maybe-auto-close (app event)
  "When auto-close is on, typing ( inserts () with the cursor between."
  (when (and (auto-close app)
             (= (event-type event) +ev-key-down+)
             (= (event-char-code event) (char-code #\()))
    (let ((rv (current-repl app)))
      (when (and rv (not (repl-busy rv)) (tvision::can-edit-here-p rv))
        (insert-string rv "()")
        (setf (text-cur-col rv) (1- (text-cur-col rv)))
        (draw-view rv)
        (clear-event event)
        t))))

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
        ((and (logtest (event-modifiers event) +md-alt+) (= (event-char-code event) 46)) ; M-.
         (do-goto-definition (current-repl app) app) (clear-event event)))))
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
          ((= c +cm-classes+)     (do-classes rv) (clear-event event))
          ((= c +cm-gotodef+)     (do-goto-definition rv app) (clear-event event))
          ((= c +cm-funcbrowser+) (do-function-browser rv app) (clear-event event))
          ((= c +cm-browse+)      (do-browse app) (clear-event event))
          ((= c +cm-bhistory+)    (do-browser-history app) (clear-event event))
          ((= c +cm-step+)        (do-step rv) (clear-event event))
          ((= c +cm-profile+)     (do-profile rv app) (clear-event event))
          ((= c +cm-profile-det+) (do-profile-deterministic rv) (clear-event event))
          ((= c +cm-whocalls+)    (do-xref rv app :calls) (clear-event event))
          ((= c +cm-whorefs+)     (do-xref rv app :references) (clear-event event))
          ((= c +cm-packages+)    (do-packages rv) (clear-event event))
          ((= c +cm-systems+)     (do-systems) (clear-event event))
          ((= c +cm-load-buffer+) (do-load-buffer app) (clear-event event))
          ((= c +cm-find+)        (do-find app) (clear-event event))
          ((= c +cm-find-next+)   (do-find-next app) (clear-event event))
          ((= c +cm-histsearch+)  (do-history-search rv) (clear-event event))
          ((= c +cm-new-file+)    (do-new-editor app) (clear-event event))
          ((= c +cm-editor+)      (do-open-editor app) (clear-event event))
          ((= c +cm-save+)        (do-save-editor app) (clear-event event))
          ((= c +cm-saveas+)      (do-saveas-editor app) (clear-event event))
          ((= c +cm-interrupt+)   (when rv (repl-interrupt rv)) (clear-event event))
          ((= c +cm-session-save+) (do-session-save app) (clear-event event))
          ((= c +cm-session-load+) (do-session-load app) (clear-event event))
          ((= c +cm-theme+)       (do-theme app) (clear-event event))
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
          ((= c +cm-savetx+)
           (let ((path (file-save-dialog :title "Save transcript")))
             (when (and rv path) (text-save-file rv path)))
           (clear-event event))
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

(defun main ()
  (run 'tvlisp-app))

(defun toplevel ()
  (handler-case (main)
    (error (e)
      (format *error-output* "~&Error: ~a~%" e)
      (sb-ext:exit :code 1)))
  (sb-ext:exit :code 0))
