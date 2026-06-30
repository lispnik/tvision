;;;; project.lisp --- the project-manager core (a real tvlisp window) on tv2.
;;;;
;;;; tvlisp's project manager is a git-tracked file tree.  Here its navigation
;;;; core is rebuilt on tv2: an OUTLINE whose directories load lazily, fed by
;;;; `git ls-files`, with a filter INPUT-LINE that flattens the tree to matching
;;;; files.  (The shipped PM's git badges / file-ops / find-in-files are extra
;;;; layers on the same shape.)

(in-package #:tv2)

(defun %git-files (dir)
  "Sorted relative paths git tracks under DIR, or NIL when DIR is not a git repo."
  (handler-case
      (let* ((out (make-string-output-stream))
             (p (sb-ext:run-program "git" (list "-C" (namestring dir) "ls-files")
                                    :search t :output out :error nil :wait t)))
        (when (and p (eql 0 (sb-ext:process-exit-code p)))
          (let ((lines (with-input-from-string (s (get-output-stream-string out))
                         (loop for l = (read-line s nil nil) while l
                               when (plusp (length l)) collect l))))
            (and lines (sort lines #'string<)))))
    (error () nil)))

(defun %walk-files (dir)
  (let ((dir (uiop:ensure-directory-pathname dir)) (acc '()))
    (labels ((walk (d)
               (dolist (f (ignore-errors (uiop:directory-files d))) (push (enough-namestring f dir) acc))
               (dolist (sub (ignore-errors (uiop:subdirectories d)))
                 (unless (string= (car (last (pathname-directory sub))) ".git") (walk sub)))))
      (walk dir))
    (sort acc #'string<)))

;;; --- embedding hooks: an app supplies the real git / search logic -----------
;;; (funcall fn DIR)        -> hash table  relpath -> :modified / :added
(defvar *project-status-fn* nil)
;;; (funcall fn DIR QUERY)  -> list of (ABS-PATH LINE-NUMBER TEXT) matches
(defvar *project-grep-fn* nil)

(defun %pm-badge (status relpath)
  "A git status tag appended to a file node's label (empty when clean/unknown)."
  (case (and status (gethash relpath status))
    (:modified " [M]") (:added " [A]") (t "")))

(defun %fs-nodes (entries dir status)
  "Outline nodes for ENTRIES (a list of (SEGMENTS . RELPATH)) under DIR.  Sub-dirs
get a lazy loader; files are leaves whose DATA is their relpath, labelled with a
git status badge from STATUS (a relpath -> keyword hash, or NIL)."
  (let ((order '()) (groups (make-hash-table :test 'equal)))
    (dolist (e entries)
      (let ((head (first (car e))))
        (unless (nth-value 1 (gethash head groups)) (push head order))
        (push e (gethash head groups))))
    (let ((nodes '()))
      (dolist (head (nreverse order))
        (let* ((es (nreverse (gethash head groups)))
               (file (and (= (length es) 1) (null (rest (car (first es)))) (first es))))
          (if file
              (push (tvision:make-outline-node          ; DATA is the absolute path (works across roots)
                     (concatenate 'string head (%pm-badge status (cdr file)))
                     nil (namestring (merge-pathnames head dir)))
                    nodes)
              (let* ((sub (uiop:ensure-directory-pathname (merge-pathnames head dir)))
                     (sub-entries (mapcar (lambda (e) (cons (rest (car e)) (cdr e))) es))
                     (node (tvision:make-outline-node (format nil "~a/" head))))
                (setf (tvision:outline-node-loader node) (lambda () (%fs-nodes sub-entries sub status)))
                (push node nodes)))))
      (setf nodes (nreverse nodes))
      (append (remove-if-not #'tvision:outline-node-loader nodes)     ; dirs first
              (remove-if #'tvision:outline-node-loader nodes)))))

(defun %git-root (dir)
  "Return (values ROOT-NODE RELPATHS) for the git tree at DIR, with git status
badges on changed files (via *PROJECT-STATUS-FN* when bound)."
  (let* ((dir (uiop:ensure-directory-pathname dir))
         (status (and *project-status-fn* (ignore-errors (funcall *project-status-fn* dir))))
         (rels (or (%git-files dir) (%walk-files dir)))
         (entries (mapcar (lambda (r) (cons (uiop:split-string r :separator "/") r)) rels))
         (changed (and status (hash-table-count status)))
         (root (tvision:make-outline-node
                (format nil "~a/  (~d files~@[, ~d changed~])"
                        (car (last (pathname-directory dir))) (length rels)
                        (and changed (plusp changed) changed))
                (%fs-nodes entries dir status))))
    (setf (tvision:outline-node-expanded root) t)
    (values root (mapcar (lambda (r) (namestring (merge-pathnames r dir))) rels))))   ; abspaths

(defclass project-window (window)
  ((dir        :initarg :dir :accessor pw-dir)      ; the primary project root (a directory pathname)
   (extra-dirs :initform nil :accessor pw-extra-dirs)  ; additional roots (each its own top-level tree)
   (tree-node  :initform nil :accessor pw-tree-node)   ; list of per-root root outline-nodes
   (rels       :initform nil :accessor pw-rels))       ; absolute paths of every file (for the filter)
  (:metaclass reactive-class))

(defun pw-dirs (win) (cons (pw-dir win) (reverse (pw-extra-dirs win))))

(defun %pm-short (win abspath)
  "ABSPATH shown relative to whichever root contains it (else the full path)."
  (dolist (d (pw-dirs win) abspath)
    (let ((rel (ignore-errors (enough-namestring abspath d))))
      (when (and rel (string/= rel abspath)) (return rel)))))

(defun %pm-rebuild (win)
  "Rescan WIN's roots from disk (new/removed files + fresh git status) and refresh
the tree, preserving the filter."
  (let ((roots '()) (rels '()))
    (dolist (d (pw-dirs win))
      (multiple-value-bind (tree rs) (%git-root d) (push tree roots) (setf rels (append rels rs))))
    (setf (pw-tree-node win) (nreverse roots) (pw-rels win) rels)
    (let ((ol (find-view win 'tree)) (q (find-view win 'q)))
      (when ol
        (let ((filter (and q (input-text q))))
          (setf (outline-roots ol)
                (if (or (null filter) (zerop (length filter))) (pw-tree-node win)
                    (mapcar (lambda (r) (tvision:make-outline-node (%pm-short win r) nil r))
                            (fuzzy-filter filter rels)))
                (outline-focused ol) 0 (outline-top ol) 0))
        (invalidate ol)))))

;;; --- opening files in an editor on the desktop ------------------------------

(defun %pm-goto (ed-win line)
  "Move EDITOR-WINDOW ED-WIN's cursor to 1-based LINE (NIL = leave it)."
  (let ((te (and ed-win (find-view ed-win 'edit))))
    (when (and te line)
      (setf (te-cy te) (max 0 (1- line)) (te-cx te) 0 (te-anchor te) nil)
      (te-clamp te) (te-ensure-visible te))))

(defun %pm-open-file (win relpath &optional line)
  "Open RELPATH (resolved under WIN's project dir) in a tv2 editor, at LINE if
given.  Reuses an already-open editor for the same file; opens on *DESKTOP* when
hosted, else full-screen."
  (let ((path (merge-pathnames relpath (pw-dir win))))
    (when (probe-file path)
      (let ((existing (and *desktop*
                           (find-if (lambda (w)
                                      (and (typep w 'editor-window)
                                           (let ((te (find-view w 'edit)))
                                             (and te (te-filename te)
                                                  (equal (namestring (te-filename te)) (namestring path))))))
                                    (dt-windows *desktop*)))))
        (cond
          ((null *desktop*) (run-editor path))
          (existing (dt-raise *desktop* existing) (dt-refocus *desktop*)
                    (%pm-goto existing line) (invalidate *desktop*))
          (t (dt-open *desktop* (lambda () (make-editor path)))
             (%pm-goto (dt-top *desktop*) line) (invalidate *desktop*)))))))

(defun %pm-open-current (win)
  "Toggle the current directory node, or open the current file in an editor."
  (let* ((ol (find-view win 'tree)) (n (and ol (ov-current ol))))
    (when n
      (if (tvision:outline-node-expandable-p n)
          (progn (setf (tvision:outline-node-expanded n) (not (tvision:outline-node-expanded n)))
                 (when (tvision:outline-node-expanded n) (tvision:outline-ensure-children n))
                 (invalidate ol))
          (let ((data (tvision:outline-node-data n)))
            (when data (%pm-open-file win (princ-to-string data))))))))

(define-command proj-open (v e)
  "Expand/collapse a directory, or open a file leaf in the editor."
  (%pm-open-current (view-root v)))

;;; --- find-in-files ----------------------------------------------------------

(defun prompt-string (title label)
  "Modal one-line prompt; return the entered string, or NIL on cancel."
  (let ((d (ui (dialog (:title title :keymap *dialog-keys*
                        :value-fn (lambda (d) (input-text (find-view d 'q))))
                 (stack
                   (1 (row ((+ 2 (length label)) (static-text :role :label :text label))
                           (:fill (input-line :name 'q))))
                   (1 (static-text :role :status :text " Enter: search · Esc: cancel ")))))))
    (let ((r (exec-view d :width 60 :height 6))) (if (eq r :cancel) nil r))))

(defun %pm-find-in-files (win)
  "Prompt for a string, grep the project (via *PROJECT-GREP-FN*), and open the
chosen match at its line."
  (when *project-grep-fn*
    (let ((q (prompt-string " Find in files " "Search for:")))
      (when (and q (plusp (length (string-trim " " q))))
        (let ((hits (ignore-errors (funcall *project-grep-fn* (pw-dir win) (string-trim " " q)))))
          (if (null hits)
              (let ((echo (find-view win 'echo)))
                (when echo (setf (static-text-text echo) (format nil " no matches for ~s " q)) (invalidate echo)))
              (let* ((base (pw-dir win))
                     (labels (mapcar (lambda (h)
                                       (format nil "~a:~d: ~a"
                                               (enough-namestring (first h) base) (second h) (third h)))
                                     hits))
                     (chosen (popup-choose labels :title (format nil " ~d match~:p — ~s " (length hits) q)))
                     (idx (and chosen (position chosen labels :test #'string=))))
                (when idx
                  (let ((h (nth idx hits)))
                    (%pm-open-file win (enough-namestring (first h) base) (second h)))))))))))

(defmethod status-hints ((win project-window))   ; chips shown while a PM window is focused
  (append
   (list (cons "Open" (lambda () (%pm-open-current win))))
   (when *project-grep-fn* (list (cons "Find" (lambda () (%pm-find-in-files win)))))
   (list (cons "New"     (lambda () (%pm-new-file win)))
         (cons "Rename"  (lambda () (%pm-rename-file win)))
         (cons "Delete"  (lambda () (%pm-delete-file win)))
         (cons "Reveal"  (lambda () (%pm-reveal win)))
         (cons "+Root"   (lambda () (%pm-add-root win)))
         (cons "Refresh" (lambda () (%pm-rebuild win))))))

;;; --- file operations --------------------------------------------------------

(defun %confirm (message)
  "A modal Yes/No dialog; return T on Yes."
  (let ((d (ui (dialog (:title " Confirm " :keymap *dialog-keys* :value-fn (constantly t))
                 (stack (1 (static-text :role :label :text message))
                        (:fill (static-text :text ""))
                        (1 (row (:fill (static-text :text ""))
                                (9  (button :label "Yes" :command 'accept))
                                (9  (button :label "No"  :command 'cancel)))))))))
    (not (eq (exec-view d :width 54 :height 7) :cancel))))

(defun %pm-echo (win msg)
  (let ((e (find-view win 'echo))) (when e (setf (static-text-text e) msg) (invalidate e))))

(defun %pm-selected-file (win)
  "Absolute pathname of the selected file leaf, or NIL when a dir/none is focused."
  (let* ((ol (find-view win 'tree)) (n (and ol (ov-current ol))))
    (when (and n (not (tvision:outline-node-expandable-p n)) (tvision:outline-node-data n))
      (merge-pathnames (princ-to-string (tvision:outline-node-data n)) (pw-dir win)))))

(defun %pm-new-file (win)
  (let ((name (prompt-string " New file " "Path (relative to project):")))
    (when (and name (plusp (length (string-trim " " name))))
      (let ((path (merge-pathnames (string-trim " " name) (pw-dir win))))
        (handler-case
            (progn (ensure-directories-exist path)
                   (unless (probe-file path)
                     (with-open-file (s path :direction :output :if-does-not-exist :create)))
                   (%pm-rebuild win)
                   (%pm-open-file win (enough-namestring path (pw-dir win))))
          (error (e) (%pm-echo win (format nil " ~a " e))))))))

(defun %pm-rename-file (win)
  (let ((path (%pm-selected-file win)))
    (if (null path)
        (%pm-echo win " select a file to rename ")
        (let ((new (prompt-string " Rename " (format nil "New name for ~a:" (file-namestring path)))))
          (when (and new (plusp (length (string-trim " " new))))
            (handler-case
                (let ((dest (merge-pathnames (string-trim " " new) (uiop:pathname-directory-pathname path))))
                  (rename-file path dest) (%pm-rebuild win) (%pm-echo win (format nil " renamed -> ~a " (file-namestring dest))))
              (error (e) (%pm-echo win (format nil " ~a " e)))))))))

(defun %pm-delete-file (win)
  (let ((path (%pm-selected-file win)))
    (if (null path)
        (%pm-echo win " select a file to delete ")
        (when (%confirm (format nil " Delete ~a? " (file-namestring path)))
          (handler-case (progn (delete-file path) (%pm-rebuild win) (%pm-echo win " deleted "))
            (error (e) (%pm-echo win (format nil " ~a " e))))))))

(defun %pm-add-root (win)
  "Add another project root (its own top-level tree)."
  (let ((d (prompt-string " Add root " "Directory:")))
    (when (and d (plusp (length (string-trim " " d))))
      (let ((dir (ignore-errors (uiop:ensure-directory-pathname (string-trim " " d)))))
        (if (and dir (probe-file dir))
            (progn (pushnew (truename dir) (pw-extra-dirs win) :test #'equal) (%pm-rebuild win))
            (%pm-echo win (format nil " no such directory: ~a " d)))))))

(defun %pm-reveal (win)
  "Reveal the active editor's file: filter the tree down to it."
  (let* ((ew (and *desktop* (find-if (lambda (w) (typep w 'editor-window)) (reverse (dt-windows *desktop*)))))
         (te (and ew (find-view ew 'edit)))
         (path (and te (te-filename te))))
    (if (null path)
        (%pm-echo win " no editor file to reveal ")
        (let ((q (find-view win 'q)) (name (file-namestring path)))
          (when q (setf (input-text q) name (input-caret q) (length name)) (input-notify q))
          (%pm-echo win (format nil " revealing ~a " name))))))

(defkeymap *proj-keys* (*outline-keys*)
  (:enter proj-open))                    ; override Enter; arrows/Right/Left inherit from *outline-keys*

(defvar *project-dir* "/Users/mkennedy/Projects/common-lisp/tvision/"
  "Default root for new project-manager windows; set by the Change-dir dialog.")

(defun make-project (&optional (dir *project-dir*))
  "Build a project-manager window for DIR.  Return (values WINDOW FOCUS)."
  (let* ((win (make-instance 'project-window
                             :dir (uiop:ensure-directory-pathname dir)
                             :title " tv2 — Project manager (a real tvlisp window, ported) "
                             :keymap *global-keys*))
         (body (ui (stack
                     (1 (row (9 (static-text :role :label :text " Filter: "))
                             (:fill (input-line :name 'q
                                      :on-change (lambda (il)
                                                   (let* ((q (input-text il)) (w (view-root il)) (ol (find-view w 'tree)))
                                                     (setf (outline-roots ol)
                                                           (if (zerop (length q)) (pw-tree-node w)
                                                               (mapcar (lambda (r) (tvision:make-outline-node (%pm-short w r) nil r))
                                                                       (fuzzy-filter q (pw-rels w))))
                                                           (outline-focused ol) 0 (outline-top ol) 0)
                                                     (invalidate ol)))))))
                     (:fill (outline :name 'tree :keymap *proj-keys*))
                     (1 (static-text :name 'echo :role :status :text " Enter on a file: open · [M]/[A] = git modified/added "))
                     (1 (static-text :role :status
                          :text " filter · Open / Find / New / Rename / Delete / Refresh chips · Esc: close "))))))
    (add-subview win body)
    (%pm-rebuild win)
    (setf (window-scroll-target win) (find-view win 'tree) (window-help win) :project)
    (values win (find-view win 'q))))

(defun run-project (&optional (dir "/Users/mkennedy/Projects/common-lisp/tvision/"))
  "Browse a git project as a lazy tree with a flat-match filter."
  (multiple-value-bind (w f) (make-project dir) (run-view w :focus f)))
