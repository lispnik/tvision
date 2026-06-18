;;;; filedialog.lisp --- A reusable file open / save dialog.

(in-package #:tvision)

(defun %dir-list (dir)
  "Return (values subdir-names file-names) for directory DIR (best effort)."
  (let ((d (ignore-errors (truename dir))) (files '()) (dirs '()))
    (when d
      (handler-case
          (progn
            (dolist (p (directory (make-pathname :name :wild :type :wild :defaults d)))
              (let ((n (file-namestring p)))
                (when (plusp (length n)) (push n files))))
            (dolist (p (directory (make-pathname
                                   :directory (append (pathname-directory d) (list :wild))
                                   :name nil :type nil :defaults d)))
              (let ((n (car (last (pathname-directory p)))))
                (when (stringp n) (push n dirs)))))
        (error () nil)))
    (values (sort dirs #'string<) (sort files #'string<))))

(defun %parent-dir (dir)
  (let ((d (pathname-directory dir)))
    (if (and (listp d) (cdr d))
        (make-pathname :directory (butlast d) :name nil :type nil :defaults dir)
        dir)))

(defun %subdir (dir name)
  (make-pathname :directory (append (pathname-directory dir) (list name))
                 :name nil :type nil :defaults dir))

(defun %dir-item-p (item)
  (or (string= item "..")
      (and (plusp (length item)) (char= (char item (1- (length item))) #\/))))

(defclass tfile-dialog (tdialog)
  ((dir   :initarg :dir :accessor fd-dir)
   (input :accessor fd-input)
   (list  :accessor fd-list)))

;;; The list box knows its owning dialog so that a single click on a directory
;;; can navigate immediately (a click on a file just selects it; double-click /
;;; Enter opens it).
(defclass tfd-list (tlist-box)
  ((dialog :initarg :dialog :initform nil :accessor fdl-dialog)))

(defun fd-navigate (d dir)
  (let ((tn (ignore-errors (truename dir))))
    (when tn (setf (fd-dir d) tn) (fd-refresh d))))

(defun fd-activate (d &key open)
  "Act on the focused list item.  Directories are entered; files are selected
into the Name field, and (when OPEN) the dialog is accepted."
  (let* ((lb (fd-list d)) (item (list-item lb (list-focused lb))))
    (cond
      ((string= item "..")
       (fd-navigate d (%parent-dir (fd-dir d))))
      ((%dir-item-p item)
       (fd-navigate d (%subdir (fd-dir d) (subseq item 0 (1- (length item))))))
      (t
       (set-data (fd-input d) (namestring (merge-pathnames item (fd-dir d))))
       (when open (end-modal d +cm-ok+))))))

(defmethod handle-event ((lb tfd-list) event)
  (cond
    ;; a click focuses the row; directories navigate, files select,
    ;; a double-click on a file opens it
    ((and (= (event-type event) +ev-mouse-down+) (mouse-in-view-p lb event))
     (let ((row (+ (point-y (scroller-delta lb))
                   (point-y (make-local lb (event-mouse-where event))))))
       (when (< row (list-count lb))
         (list-focus-item lb row)
         (fd-activate (fdl-dialog lb) :open (event-double event))))
     (clear-event event))
    ;; Enter opens the focused item (navigates if it is a directory)
    ((and (= (event-type event) +ev-key-down+)
          (= (event-key-code event) +kb-enter+)
          (logtest (view-state lb) +sf-focused+))
     (fd-activate (fdl-dialog lb) :open t)
     (clear-event event))
    (t (call-next-method))))

(defun fd-refresh (d)
  "Repopulate the list and path field from the current directory."
  (multiple-value-bind (dirs files) (%dir-list (fd-dir d))
    (list-set-items (fd-list d)
                    (append (list "..")
                            (mapcar (lambda (n) (concatenate 'string n "/")) dirs)
                            files)))
  (set-data (fd-input d) (namestring (fd-dir d))))

(defmethod handle-event ((d tfile-dialog) event)
  ;; OK on a directory path navigates into it instead of accepting it
  (when (and (= (event-type event) +ev-command+)
             (= (event-command event) +cm-ok+)
             (directory-pathname-p (get-data (fd-input d))))
    (fd-navigate d (get-data (fd-input d)))
    (clear-event event))
  (call-next-method))

(defun make-file-dialog (title &key (directory (truename (user-homedir-pathname))))
  (let* ((w 52) (h 18)
         (d (make-instance 'tfile-dialog :title title :dir directory
                           :bounds (make-trect 0 0 w h)))
         (lbl (make-instance 'tlabel :text "Name:"))
         (input (make-instance 'tinputline :bounds (make-trect 9 2 (- w 3) 3) :maxlen 255))
         (vsb nil)
         (lb (make-instance 'tfd-list :command 0 :dialog d
                            :bounds (make-trect 2 4 (- w 4) (- h 4)))))
    (set-bounds lbl (make-trect 3 2 8 3))
    (insert d lbl)
    (insert d input)
    (insert d lb)
    (setf vsb (standard-scrollbar d t))
    (attach-scrollbars lb :vscroll vsb)
    (insert d (make-button (make-trect (- w 26) (- h 3) (- w 16) (- h 1)) "~O~K" +cm-ok+ t))
    (insert d (make-button (make-trect (- w 13) (- h 3) (- w 3) (- h 1)) "Cancel" +cm-cancel+))
    (setf (fd-input d) input (fd-list d) lb)
    (fd-refresh d)
    (focus lb)              ; start in the browser; Tab to the Name field to type
    d))

(defun %run-file-dialog (title directory)
  (when *application*
    (let* ((d (make-file-dialog title :directory directory))
           (desk (program-desktop *application*)))
      (move-to d (max 0 (floor (- (point-x (view-size desk)) (point-x (view-size d))) 2))
               (max 0 (floor (- (point-y (view-size desk)) (point-y (view-size d))) 2)))
      (when (= (exec-view desk d) +cm-ok+)
        (let ((path (get-data (fd-input d))))
          (and (plusp (length path)) path))))))

(defun file-open-dialog (&key (title "Open File")
                              (directory (truename (user-homedir-pathname))))
  "Prompt for a file to open.  Return the chosen path string, or NIL."
  (%run-file-dialog title directory))

(defun file-save-dialog (&key (title "Save File As")
                              (directory (truename (user-homedir-pathname))))
  "Prompt for a file to save.  Return the chosen path string, or NIL."
  (%run-file-dialog title directory))
