;;;; help.lisp --- A minimal help system keyed by help context.

(in-package #:tvision)

(defvar *help-topics* (make-hash-table :test 'eql)
  "Map of help-context -> help text string.")

(defun register-help (ctx text)
  "Associate help TEXT with help-context CTX."
  (setf (gethash ctx *help-topics*) text)
  ctx)

(defun help-text (ctx)
  (gethash ctx *help-topics*))

(defun open-help (ctx &optional (title "Help"))
  "Display the help topic for CTX modally."
  (when *application*
    (let* ((text (or (help-text ctx)
                     (format nil "No help is available for this topic.~%~%(help context ~a)" ctx)))
           (lines (%split-lines text))
           (longest (reduce #'max lines :key #'length :initial-value 24))
           (desk (program-desktop *application*))
           (w (min 72 (max 40 (+ 6 longest))))
           (h (min (- (point-y (view-size desk)) 2) (+ 6 (length lines))))
           (d (make-instance 'tdialog :title title :bounds (make-trect 0 0 w h)))
           (tv (make-instance 'ttext-view :text text :read-only t
                              :bounds (make-trect 2 1 (1- w) (- h 4)))))
      (insert d tv)
      (insert d (make-button (make-trect (floor (- w 10) 2) (- h 3)
                                         (+ (floor (- w 10) 2) 10) (- h 1))
                             "O~K~" +cm-ok+ t))
      (move-to d (floor (- (point-x (view-size desk)) w) 2)
               (max 0 (floor (- (point-y (view-size desk)) h) 2)))
      (exec-view desk d))))
