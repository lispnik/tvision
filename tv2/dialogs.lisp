;;;; dialogs.lisp --- the standard dialogs: file picker, change-dir, colours.
;;;;
;;;; Each composes EXEC-VIEW with the existing controls (input-line, list-box,
;;;; cluster, button) and returns a value, mirroring tvision's TFileDialog /
;;;; TChDirDialog / TColorDialog.

(in-package #:tv2)

;;; --- file / directory picker ------------------------------------------------

(defun %dir-entries (dir dirs-only)
  "Listing of DIR: \"../\", then subdirectories (with a trailing /), then files."
  (let ((subs (sort (mapcar (lambda (p) (format nil "~a/" (car (last (pathname-directory p)))))
                            (ignore-errors (uiop:subdirectories dir))) #'string<))
        (files (unless dirs-only
                 (sort (mapcar #'file-namestring (ignore-errors (uiop:directory-files dir))) #'string<))))
    (append (list "../") subs files)))

(defun make-file-dialog (&key (dir (uiop:getcwd)) dirs-only (title " Open file "))
  "Modal file/directory picker.  Navigate by activating directory rows; activate
a file (or click Open) to choose it.  Return a pathname, or NIL on cancel."
  (let ((cur (list (uiop:ensure-directory-pathname dir))))
    (labels ((refill (d)
               (let ((lb (find-view d 'files)) (inp (find-view d 'path)) (ns (namestring (car cur))))
                 (setf (list-items lb) (%dir-entries (car cur) dirs-only)
                       (list-selected lb) 0 (list-top lb) 0
                       (input-text inp) ns (input-caret inp) (length ns))
                 (invalidate d)))
             (activate (lb item)
               (let ((d (view-root lb)))
                 (cond
                   ((string= item "../")
                    (setf (car cur) (uiop:pathname-parent-directory-pathname (car cur))) (refill d))
                   ((and (plusp (length item)) (char= (char item (1- (length item))) #\/))
                    (setf (car cur) (uiop:ensure-directory-pathname
                                     (merge-pathnames (subseq item 0 (1- (length item))) (car cur))))
                    (refill d))
                   (t (setf (input-text (find-view d 'path)) (namestring (merge-pathnames item (car cur))))
                      (perform 'accept lb nil))))))
      (let ((d (ui (dialog (:title title :keymap *dialog-keys*
                            :value-fn (lambda (d) (input-text (find-view d 'path))))
                     (stack
                       (1 (row (7 (static-text :role :label :text " Path: "))
                               (:fill (input-line :name 'path :history-id :file))))
                       (:fill (list-box :name 'files :on-activate #'activate))
                       (1 (row (:fill (static-text :text ""))
                               (8  (button :label "Open"   :command 'accept))
                               (12 (button :label "Cancel" :command 'cancel)))))))))
        (refill d)
        (let ((r (exec-view d :width 66 :height 20)))
          (if (eq r :cancel) nil (ignore-errors (pathname r))))))))

;;; --- colour customiser (live preview of *THEME*) ----------------------------

(defparameter *color-roles* '(:normal :focused :frame :status :label :desktop :menu))

(define-command color-apply (v e)
  "Set the chosen role's attribute from the fg/bg clusters and repaint live."
  (let* ((d (view-root v))
         (role (nth (cluster-value (find-view d 'role)) *color-roles*))
         (fg (cluster-value (find-view d 'fg)))
         (bg (cluster-value (find-view d 'bg))))
    (setf (getf *theme* role) (tvision:make-attr fg bg))
    (when *root* (invalidate *root*))))

(defun make-color-dialog ()
  "Modal colour customiser: pick a role + fg + bg, Apply previews it on the live
*THEME* (the desktop repaints)."
  (let ((d (ui (dialog (:title " Colours (live preview) " :keymap *dialog-keys*
                        :value-fn (lambda (d) (declare (ignore d)) t))
                 (stack
                   (1 (static-text :role :label :text " Role · Foreground · Background — Space selects, Apply previews: "))
                   (:fill (row (18 (cluster :name 'role :mode :radio
                                     :items (mapcar (lambda (r) (string-downcase (symbol-name r))) *color-roles*)
                                     :value 0))
                               (12 (cluster :name 'fg :mode :radio
                                     :items (loop for i below 16 collect (format nil "fg ~2d" i)) :value 7))
                               (12 (cluster :name 'bg :mode :radio
                                     :items (loop for i below 8 collect (format nil "bg ~d" i)) :value 1))))
                   (1 (row (:fill (static-text :text ""))
                           (9  (button :label "Apply" :command 'color-apply))
                           (11 (button :label "Done"  :command 'accept)))))))))
    (exec-view d :width 50 :height 22)))
