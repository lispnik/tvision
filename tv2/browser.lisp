;;;; browser.lisp --- a generic filterable list browser, ported onto tv2.
;;;;
;;;; tvlisp has a whole family of "filter a list, act on the choice" windows --
;;;; the Classes / Packages / Systems / Functions / Apropos / HyperSpec pickers.
;;;; They are all the same shape, so one RUN-BROWSER covers them: a filter
;;;; INPUT-LINE over a LIST-BOX, a detail line, Enter to act.  Instantiated below
;;;; for packages and ASDF systems on real data.

(in-package #:tv2)

(defun run-browser (title all-items on-activate)
  "A filterable browser over ALL-ITEMS (strings).  Typing in the filter narrows
the list (substring, case-insensitive); Enter calls ON-ACTIVATE with the chosen
item and a SETTER that writes the detail line."
  (tvision:with-screen (s)
    (let ((win (ui (window (:title title :keymap *global-keys*)
                     (stack
                       (1 (row (9 (static-text :role :label :text " Filter: "))
                               (:fill (input-line :name 'q
                                        :on-change (lambda (il)
                                                     (let* ((query (input-text il))
                                                            (lb (find-view *root* 'items))
                                                            (items (if (zerop (length query)) all-items
                                                                       (remove-if-not
                                                                        (lambda (x) (search query x :test #'char-equal))
                                                                        all-items))))
                                                       (setf (list-items lb) items
                                                             (list-selected lb) 0 (list-top lb) 0)))))))
                       (:fill (list-box :name 'items :items all-items
                                :on-activate (lambda (lb item)
                                               (declare (ignore lb))
                                               (let ((detail (find-view *root* 'detail)))
                                                 (funcall on-activate item
                                                          (lambda (text)
                                                            (when detail
                                                              (setf (static-text-text detail) text)
                                                              (invalidate detail))))))))
                       (1 (static-text :name 'detail :role :status :text " (type to filter · Enter to act on a row) "))
                       (1 (static-text :role :status
                            :text " Tab: focus · type to filter · Enter: act · Esc: quit ")))))))
      (layout win (rect 0 0 (tvision:screen-width s) (tvision:screen-height s)))
      (setf *root* win
            (container-focus win) (first (all-focusables win))
            *ui-thread* sb-thread:*current-thread* *running* t *dirty* t)
      (loop while *running* do
        (drain-ui-callbacks)
        (when *dirty*
          (tvision:hide-cursor s)
          (draw win) (tvision:flush-screen s) (setf *dirty* nil))
        (tvision::pump-input s 0.05)
        (let ((tev (tvision::screen-next-event s)))
          (when tev (let ((ev (translate tev))) (when ev (handle-event win ev)))))))))

(defun run-packages ()
  "Browse all packages (a real tvlisp Packages window)."
  (run-browser
   " tv2 — Package browser (a real tvlisp window, ported) "
   (sort (mapcar #'package-name (list-all-packages)) #'string<)
   (lambda (name set)
     (let ((p (find-package name)) (n 0))
       (when p (do-external-symbols (sym p) (declare (ignore sym)) (incf n)))
       (funcall set (if p (format nil "  ~a — ~d external symbol~:p · uses ~{~a~^ ~}"
                                  name n (mapcar #'package-name (package-use-list p)))
                        "  ?"))))))

(defun run-systems ()
  "Browse the registered ASDF systems (a real tvlisp Systems window)."
  (run-browser
   " tv2 — ASDF system browser (a real tvlisp window, ported) "
   (sort (copy-list (ignore-errors (asdf:registered-systems))) #'string<)
   (lambda (name set)
     (let ((sys (ignore-errors (asdf:find-system name nil))))
       (funcall set (format nil "  ~a~@[ — depends on ~{~(~a~)~^ ~}~]"
                            name (and sys (ignore-errors (asdf:system-depends-on sys)))))))))
