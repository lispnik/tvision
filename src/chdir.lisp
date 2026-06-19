;;;; chdir.lisp --- TChDirDialog: a change-directory dialog with a TDirListBox.
;;;;
;;;; A focused directory browser (no file column): the list shows ".." and the
;;;; subdirectories of the current directory; entering one navigates, and Chdir
;;;; accepts the current directory.  Reuses the directory helpers in
;;;; filedialog.lisp (%dir-list / %parent-dir / %subdir).

(in-package #:tvision)

(defclass tchdir-dialog (tdialog)
  ((dir   :initarg :dir :accessor cd-dir)
   (input :accessor cd-input)
   (list  :accessor cd-list)))

;;; TDirListBox: a sorted (type-ahead) list of directories that navigates on
;;; Enter / double-click.
(defclass tdir-list-box (tsorted-list-box)
  ((dialog :initarg :dialog :initform nil :accessor dlb-dialog)))

(defun chdir-refresh (d)
  "Repopulate the directory list and path field from the current directory."
  (multiple-value-bind (dirs files) (%dir-list (cd-dir d))
    (declare (ignore files))
    (list-set-items (cd-list d)
                    (cons ".." (mapcar (lambda (n) (concatenate 'string n "/")) dirs))))
  (set-data (cd-input d) (namestring (cd-dir d))))

(defun chdir-navigate (d dir)
  (let ((tn (ignore-errors (truename dir))))
    (when tn (setf (cd-dir d) tn) (chdir-refresh d))))

(defun chdir-enter (d)
  "Enter the focused directory entry."
  (let* ((lb (cd-list d)) (item (list-item lb (list-focused lb))))
    (cond
      ((string= item "..") (chdir-navigate d (%parent-dir (cd-dir d))))
      ((and (plusp (length item)) (char= (char item (1- (length item))) #\/))
       (chdir-navigate d (%subdir (cd-dir d) (subseq item 0 (1- (length item)))))))))

(defmethod handle-event ((lb tdir-list-box) event)
  (cond
    ((and (= (event-type event) +ev-mouse-down+) (mouse-in-view-p lb event))
     (let ((row (+ (point-y (scroller-delta lb))
                   (point-y (make-local lb (event-mouse-where event))))))
       (when (< row (list-count lb))
         (list-focus-item lb row)
         (when (event-double event) (chdir-enter (dlb-dialog lb)))))
     (clear-event event))
    ((and (= (event-type event) +ev-key-down+)
          (= (event-key-code event) +kb-enter+)
          (logtest (view-state lb) +sf-focused+))
     (chdir-enter (dlb-dialog lb))
     (clear-event event))
    (t (call-next-method))))

(defun make-chdir-dialog (title &key (directory (truename (user-homedir-pathname))))
  (let* ((w 52) (h 17)
         (d (make-instance 'tchdir-dialog :title title :dir directory
                           :bounds (make-trect 0 0 w h)))
         (lbl (make-instance 'tlabel :text "Directory name"))
         (input (make-instance 'tinputline :bounds (make-trect 3 3 (- w 3) 4) :maxlen 255))
         (lb (make-instance 'tdir-list-box :command 0 :dialog d
                            :bounds (make-trect 3 5 (- w 4) (- h 3))))
         (vsb nil))
    (set-bounds lbl (make-trect 3 2 (+ 3 14) 3))
    (insert d lbl)
    (insert d input)
    (insert d lb)
    (setf vsb (standard-scrollbar d t))
    (attach-scrollbars lb :vscroll vsb)
    (insert d (make-button (make-trect (- w 36) (- h 3) (- w 26) (- h 1)) "~C~hdir" +cm-ok+ t))
    (insert d (make-button (make-trect (- w 24) (- h 3) (- w 14) (- h 1)) "Cancel" +cm-cancel+))
    (setf (cd-input d) input (cd-list d) lb)
    (chdir-refresh d)
    (focus lb)
    d))

;;; OK while the Name field holds a directory path navigates into it; otherwise
;;; OK accepts the current directory.
(defmethod handle-event ((d tchdir-dialog) event)
  (when (and (= (event-type event) +ev-command+)
             (= (event-command event) +cm-ok+)
             (let ((p (get-data (cd-input d))))
               (and (plusp (length p))
                    (not (equal (ignore-errors (truename p)) (cd-dir d))))))
    (chdir-navigate d (get-data (cd-input d)))
    (clear-event event))
  (call-next-method))

(defun chdir-dialog (&key (title "Change Directory")
                          (directory (truename (user-homedir-pathname))))
  "Open a modal change-directory dialog.  Return the chosen directory namestring,
or NIL if cancelled."
  (when *application*
    (let* ((d (make-chdir-dialog title :directory directory))
           (desk (program-desktop *application*)))
      (move-to d (max 0 (floor (- (point-x (view-size desk)) (point-x (view-size d))) 2))
               (max 0 (floor (- (point-y (view-size desk)) (point-y (view-size d))) 2)))
      (when (= (exec-view desk d) +cm-ok+)
        (namestring (cd-dir d))))))
