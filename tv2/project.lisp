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

(defun %fs-nodes (entries dir)
  "Outline nodes for ENTRIES (a list of (SEGMENTS . RELPATH)) under DIR.  Sub-dirs
get a lazy loader; files are leaves whose DATA is their relpath."
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
              (push (tvision:make-outline-node head nil (cdr file)) nodes)
              (let* ((sub (uiop:ensure-directory-pathname (merge-pathnames head dir)))
                     (sub-entries (mapcar (lambda (e) (cons (rest (car e)) (cdr e))) es))
                     (node (tvision:make-outline-node (format nil "~a/" head))))
                (setf (tvision:outline-node-loader node) (lambda () (%fs-nodes sub-entries sub)))
                (push node nodes)))))
      (setf nodes (nreverse nodes))
      (append (remove-if-not #'tvision:outline-node-loader nodes)     ; dirs first
              (remove-if #'tvision:outline-node-loader nodes)))))

(defun %git-root (dir)
  "Return (values ROOT-NODE RELPATHS) for the git tree at DIR."
  (let* ((dir (uiop:ensure-directory-pathname dir))
         (rels (or (%git-files dir) (%walk-files dir)))
         (entries (mapcar (lambda (r) (cons (uiop:split-string r :separator "/") r)) rels))
         (root (tvision:make-outline-node
                (format nil "~a/  (~d files)" (car (last (pathname-directory dir))) (length rels))
                (%fs-nodes entries dir))))
    (setf (tvision:outline-node-expanded root) t)
    (values root rels)))

(define-command proj-open (v e)
  "Expand/collapse a directory, or 'open' (echo) a file leaf."
  (let ((n (ov-current v)))
    (when n
      (if (tvision:outline-node-expandable-p n)
          (progn (setf (tvision:outline-node-expanded n) (not (tvision:outline-node-expanded n)))
                 (when (tvision:outline-node-expanded n) (tvision:outline-ensure-children n))
                 (invalidate v))
          (let ((echo (find-view (view-root v) 'echo)))
            (when echo
              (setf (static-text-text echo) (format nil " open ~a " (or (tvision:outline-node-data n)
                                                                        (tvision:outline-node-text n))))
              (invalidate echo)))))))

(defkeymap *proj-keys* (*outline-keys*)
  (:enter proj-open))                    ; override Enter; arrows/Right/Left inherit from *outline-keys*

(defvar *project-dir* "/Users/mkennedy/Projects/common-lisp/tvision/"
  "Default root for new project-manager windows; set by the Change-dir dialog.")

(defun make-project (&optional (dir *project-dir*))
  "Build a project-manager window for DIR.  Return (values WINDOW FOCUS)."
  (multiple-value-bind (tree rels) (%git-root dir)
    (let ((win (ui (window (:title " tv2 — Project manager (a real tvlisp window, ported) "
                            :keymap *global-keys*)
                     (stack
                       (1 (row (9 (static-text :role :label :text " Filter: "))
                               (:fill (input-line :name 'q
                                        :on-change (lambda (il)
                                                     (let* ((q (input-text il)) (ol (find-view (view-root il) 'tree)))
                                                       (setf (outline-roots ol)
                                                             (if (zerop (length q)) (list tree)
                                                                 (loop for r in rels
                                                                       when (search q r :test #'char-equal)
                                                                         collect (tvision:make-outline-node r nil r)))
                                                             (outline-focused ol) 0 (outline-top ol) 0)
                                                       (invalidate ol)))))))
                       (:fill (outline :name 'tree :roots (list tree) :keymap *proj-keys*))
                       (1 (static-text :name 'echo :role :status :text " Right/Enter: expand (lazy) · Enter on a file: open "))
                       (1 (static-text :role :status
                            :text " Tab/arrows · type to filter to a flat match list · Esc: close ")))))))
      (setf (window-scroll-target win) (find-view win 'tree))
      (values win (find-view win 'q)))))

(defun run-project (&optional (dir "/Users/mkennedy/Projects/common-lisp/tvision/"))
  "Browse a git project as a lazy tree with a flat-match filter."
  (multiple-value-bind (w f) (make-project dir) (run-view w :focus f)))
