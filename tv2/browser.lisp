;;;; browser.lisp --- a generic filterable list browser, ported onto tv2.
;;;;
;;;; tvlisp has a whole family of "filter a list, act on the choice" windows --
;;;; the Classes / Packages / Systems / Functions / Apropos / HyperSpec pickers.
;;;; They are all the same shape, so one RUN-BROWSER covers them: a filter
;;;; INPUT-LINE over a LIST-BOX, a detail line, Enter to act.  Instantiated below
;;;; for packages and ASDF systems on real data.

(in-package #:tv2)

(defun make-browser (title all-items on-activate)
  "Build a filterable browser over ALL-ITEMS (strings).  Typing in the filter
narrows the list; Enter calls ON-ACTIVATE with the chosen item and a SETTER that
writes the detail line.  Return (values WINDOW FOCUS)."
    (let ((win (ui (window (:title title :keymap *global-keys*)
                     (stack
                       (1 (row (9 (static-text :role :label :text " Filter: "))
                               (:fill (input-line :name 'q
                                        :on-change (lambda (il)
                                                     (let* ((query (input-text il))
                                                            (lb (find-view (view-root il) 'items))
                                                            (items (fuzzy-filter query all-items)))
                                                       (setf (list-items lb) items
                                                             (list-selected lb) 0 (list-top lb) 0)))))))
                       (:fill (list-box :name 'items :items all-items
                                :on-activate (lambda (lb item)
                                               (let ((detail (find-view (view-root lb) 'detail)))
                                                 (funcall on-activate item
                                                          (lambda (text)
                                                            (when detail
                                                              (setf (static-text-text detail) text)
                                                              (invalidate detail))))))))
                       (1 (static-text :name 'detail :role :status :text " (type to filter · Enter to act on a row) "))
                       (1 (static-text :role :status
                            :text " Tab: focus · type to filter · Enter: act · Esc: close ")))))))
      (setf (window-scroll-target win) (find-view win 'items) (window-help win) :browser)
      (values win (find-view win 'q))))

(defun run-browser (title all-items on-activate)
  (multiple-value-bind (w f) (make-browser title all-items on-activate) (run-view w :focus f)))

(defun make-packages ()
  "Browse all packages (a real tvlisp Packages window)."
  (make-browser
   " tv2 — Package browser (a real tvlisp window, ported) "
   (sort (mapcar #'package-name (list-all-packages)) #'string<)
   (lambda (name set)
     (let ((p (find-package name)) (n 0))
       (when p (do-external-symbols (sym p) (declare (ignore sym)) (incf n)))
       (funcall set (if p (format nil "  ~a — ~d external symbol~:p · uses ~{~a~^ ~}"
                                  name n (mapcar #'package-name (package-use-list p)))
                        "  ?"))))))

(defun make-systems ()
  "Browse the registered ASDF systems (a real tvlisp Systems window)."
  (make-browser
   " tv2 — ASDF system browser (a real tvlisp window, ported) "
   (sort (copy-list (ignore-errors (asdf:registered-systems))) #'string<)
   (lambda (name set)
     (let ((sys (ignore-errors (asdf:find-system name nil))))
       (funcall set (format nil "  ~a~@[ — depends on ~{~(~a~)~^ ~}~]"
                            name (and sys (ignore-errors (asdf:system-depends-on sys)))))))))

(defun run-packages () (multiple-value-bind (w f) (make-packages) (run-view w :focus f)))
(defun run-systems  () (multiple-value-bind (w f) (make-systems)  (run-view w :focus f)))
