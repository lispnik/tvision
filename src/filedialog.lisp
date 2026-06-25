;;;; filedialog.lisp --- A reusable file open / save dialog.

(in-package #:tvision)

(defun %wild-match (pattern name)
  "Case-insensitive filename glob match supporting * and ?."
  (let ((p (string-downcase pattern)) (n (string-downcase name)))
    (labels ((m (pp nn)
               (cond ((= pp (length p)) (= nn (length n)))
                     ((char= (char p pp) #\*)
                      (or (m (1+ pp) nn)
                          (and (< nn (length n)) (m pp (1+ nn)))))
                     ((= nn (length n)) nil)
                     ((or (char= (char p pp) #\?) (char= (char p pp) (char n nn)))
                      (m (1+ pp) (1+ nn)))
                     (t nil))))
      (m 0 0))))

(defun %dir-list (dir &optional (pattern "*"))
  "Return (values subdir-names file-names) for directory DIR (best effort).
File names are filtered by the glob PATTERN."
  (let ((d (ignore-errors (truename dir))) (files '()) (dirs '()))
    (when d
      (handler-case
          (progn
            (dolist (p (directory (make-pathname :name :wild :type :wild :defaults d)))
              (let ((n (file-namestring p)))
                (when (and (plusp (length n)) (%wild-match pattern n)) (push n files))))
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
  ((dir    :initarg :dir :accessor fd-dir)
   (filter :initarg :filter :initform "*" :accessor fd-filter)  ; current wildcard
   (input  :initform nil :accessor fd-input)
   (info   :initform nil :accessor fd-info)
   (list   :initform nil :accessor fd-list)))

;;; The list box knows its owning dialog so that a single click on a directory
;;; can navigate immediately (a click on a file just selects it; double-click /
;;; Enter opens it).
(defclass tfd-list (tlist-box)
  ((dialog :initarg :dialog :initform nil :accessor fdl-dialog)))

;;; TFileInputLine: the name field; entering a wildcard (e.g. *.lisp) refilters
;;; the list instead of accepting.  Inherits THistory so the Down key / ▼ gadget
;;; recalls previously-entered paths.
(defclass tfile-input-line (thistory-input)
  ((dialog :initarg :dialog :initform nil :accessor fil-dialog)
   (history-id :initform "file")))

;;; TFileInfoPane: shows the focused entry's size and modification date.
(defclass tfile-info-pane (tstatic-text)
  ((dialog :initarg :dialog :initform nil :accessor fip-dialog)))

(defun %wildcardp (s) (find-if (lambda (c) (member c '(#\* #\?))) s))

(defun fd-apply-filter (d value)
  "If VALUE holds a wildcard, adopt it as the file filter and refresh the list.
Return T when a filter was applied."
  (when (%wildcardp value)
    (let ((pat (file-namestring value)))
      (setf (fd-filter d) (if (plusp (length pat)) pat "*")))
    (fd-refresh d)
    t))

(defun fd-navigate (d dir)
  (let ((tn (ignore-errors (truename dir))))
    (when tn
      (setf (fd-dir d) tn)
      (fd-refresh d)
      (list-focus-item (fd-list d) 0))))   ; highlight the top of the new listing

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
  "Repopulate the list and path field from the current directory + filter."
  (multiple-value-bind (dirs files) (%dir-list (fd-dir d) (fd-filter d))
    (list-set-items (fd-list d)
                    (append (list "..")
                            (mapcar (lambda (n) (concatenate 'string n "/")) dirs)
                            files)))
  (set-data (fd-input d) (namestring (fd-dir d)))
  (when (fd-info d) (draw-view (fd-info d))))

;;; --- TFileInputLine / TFileInfoPane behaviour ------------------------------

(defmethod handle-event ((il tfile-input-line) event)
  ;; Enter on a wildcard pattern filters the list instead of accepting.
  (if (and (= (event-type event) +ev-key-down+)
           (= (event-key-code event) +kb-enter+)
           (logtest (view-state il) +sf-focused+)
           (fil-dialog il)
           (%wildcardp (input-data il)))
      (progn (fd-apply-filter (fil-dialog il) (input-data il))
             (focus (fd-list (fil-dialog il)))
             (clear-event event))
      (call-next-method)))

(defun %fmt-date (ut)
  (multiple-value-bind (s m h d mo y) (decode-universal-time ut)
    (declare (ignore s))
    (format nil "~d-~2,'0d-~2,'0d ~2,'0d:~2,'0d" y mo d h m)))

(defmethod draw ((p tfile-info-pane))
  (let* ((w (point-x (view-size p))) (c (get-color p 1)) (db (make-draw-buffer w))
         (d (fip-dialog p)) (lb (and d (fd-list d)))
         (item (and lb (plusp (list-count lb)) (list-item lb (list-focused lb))))
         (text (cond
                 ((null item) "")
                 ((%dir-item-p item) (format nil "~a  <DIR>" item))
                 (t (let* ((path (ignore-errors (merge-pathnames item (fd-dir d))))
                           (size (and path (ignore-errors
                                            (with-open-file (s path) (file-length s)))))
                           (wd (and path (ignore-errors (file-write-date path)))))
                      (format nil "~a~@[  ~:d bytes~]~@[  ~a~]"
                              item size (and wd (%fmt-date wd))))))))
    (db-fill db #\Space c)
    (db-move-str db 0 (subseq text 0 (min (length text) (max 0 (1- w)))) c)
    (write-line* p 0 0 w 1 db)))

(defmethod handle-event ((p tfile-info-pane) event)
  (when (and (message-event-p event) (= (event-command event) +cm-list-focus-changed+))
    (draw-view p))
  (call-next-method))

(defmethod handle-event ((d tfile-dialog) event)
  ;; OK on a directory navigates into it (so the listing updates to that dir);
  ;; OK on a wildcard refilters; otherwise it accepts the typed name.  Names are
  ;; resolved against the current directory, so a bare subdirectory name like
  ;; "src" enters DIR/src and a relative file name is returned as an absolute
  ;; path.
  (when (and (= (event-type event) +ev-command+)
             (= (event-command event) +cm-ok+))
    (cond
      ;; Enter / OK while the browser is focused acts on the highlighted entry:
      ;; a directory is entered (and the listing updates to it), a file accepted.
      ;; The default OK button consumes Enter before the list can, so we route
      ;; it here rather than in the list's own handler.
      ((eq (group-current d) (fd-list d))
       (fd-activate d :open t)
       (clear-event event))
      (t
       (let* ((val (get-data (fd-input d)))
              (resolved (and (plusp (length val))
                             (ignore-errors (merge-pathnames val (fd-dir d))))))
         (cond
           ((and resolved (directory-pathname-p resolved))
            (fd-navigate d resolved) (clear-event event))
           ((fd-apply-filter d val) (clear-event event))
           ;; a file to accept: hand back an absolute path
           (resolved (set-data (fd-input d) (namestring resolved))))))))
  (call-next-method))

(defun make-file-dialog (title &key (directory (truename (user-homedir-pathname))))
  (let* ((w 52) (h 19)
         (d (make-instance 'tfile-dialog :title title :dir directory
                           :bounds (make-trect 0 0 w h)))
         (input (make-instance 'tfile-input-line :dialog d
                               :bounds (make-trect 9 2 (- w 3) 3) :maxlen 255))
         (vsb nil)
         (lb (make-instance 'tfd-list :command 0 :dialog d
                            :bounds (make-trect 2 4 (- w 4) (- h 6))))
         ;; mnemonic labels: Alt-N jumps to the Name field, Alt-F to the browser
         (lbl  (make-instance 'tlabel :text "~N~ame:" :link input
                                      :bounds (make-trect 3 2 8 3)))
         (flbl (make-instance 'tlabel :text "~F~iles" :link lb
                                      :bounds (make-trect 2 3 9 4)))
         (info (make-instance 'tfile-info-pane :dialog d :text ""
                              :bounds (make-trect 2 (- h 5) (- w 2) (- h 4)))))
    (insert d lbl)
    (insert d input)
    (insert d flbl)
    (insert d lb)
    (insert d info)
    (setf vsb (standard-scrollbar d t))
    (attach-scrollbars lb :vscroll vsb)
    (insert d (make-button (make-trect (- w 26) (- h 3) (- w 16) (- h 1)) "~O~K" +cm-ok+ t))
    (insert d (make-button (make-trect (- w 13) (- h 3) (- w 3) (- h 1)) "~C~ancel" +cm-cancel+))
    (setf (fd-input d) input (fd-list d) lb (fd-info d) info)
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
          (when (plusp (length path))
            (history-add "file" path)        ; remember the chosen path
            path))))))

(defun file-open-dialog (&key (title "Open File")
                              (directory (truename (user-homedir-pathname))))
  "Prompt for a file to open.  Return the chosen path string, or NIL."
  (%run-file-dialog title directory))

(defun file-save-dialog (&key (title "Save File As")
                              (directory (truename (user-homedir-pathname))))
  "Prompt for a file to save.  Return the chosen path string, or NIL."
  (%run-file-dialog title directory))
