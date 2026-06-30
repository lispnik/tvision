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
              (push (tvision:make-outline-node
                     (concatenate 'string head (%pm-badge status (cdr file))) nil (cdr file))
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
    (values root rels)))

(defclass project-window (window)
  ((dir :initarg :dir :accessor pw-dir))          ; the project root (a directory pathname)
  (:metaclass reactive-class))

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
   (when *project-grep-fn* (list (cons "Find in files" (lambda () (%pm-find-in-files win)))))))

(defkeymap *proj-keys* (*outline-keys*)
  (:enter proj-open))                    ; override Enter; arrows/Right/Left inherit from *outline-keys*

(defvar *project-dir* "/Users/mkennedy/Projects/common-lisp/tvision/"
  "Default root for new project-manager windows; set by the Change-dir dialog.")

(defun make-project (&optional (dir *project-dir*))
  "Build a project-manager window for DIR.  Return (values WINDOW FOCUS)."
  (multiple-value-bind (tree rels) (%git-root dir)
    (let* ((win (make-instance 'project-window
                               :dir (uiop:ensure-directory-pathname dir)
                               :title " tv2 — Project manager (a real tvlisp window, ported) "
                               :keymap *global-keys*))
           (body (ui (stack
                       (1 (row (9 (static-text :role :label :text " Filter: "))
                               (:fill (input-line :name 'q
                                        :on-change (lambda (il)
                                                     (let* ((q (input-text il)) (ol (find-view (view-root il) 'tree)))
                                                       (setf (outline-roots ol)
                                                             (if (zerop (length q)) (list tree)
                                                                 (mapcar (lambda (r) (tvision:make-outline-node r nil r))
                                                                         (fuzzy-filter q rels)))
                                                             (outline-focused ol) 0 (outline-top ol) 0)
                                                       (invalidate ol)))))))
                       (:fill (outline :name 'tree :roots (list tree) :keymap *proj-keys*))
                       (1 (static-text :name 'echo :role :status :text " Enter on a file: open in editor · [M]/[A] = git modified/added "))
                       (1 (static-text :role :status
                            :text " type to filter · Open / Find-in-files chips · Esc: close "))))))
      (add-subview win body)
      (setf (window-scroll-target win) (find-view win 'tree) (window-help win) :project)
      (values win (find-view win 'q)))))

(defun run-project (&optional (dir "/Users/mkennedy/Projects/common-lisp/tvision/"))
  "Browse a git project as a lazy tree with a flat-match filter."
  (multiple-value-bind (w f) (make-project dir) (run-view w :focus f)))
