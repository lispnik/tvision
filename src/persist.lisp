;;;; persist.lisp --- S-expression persistence for the desktop.
;;;;
;;;; Rather than Turbo Vision's binary TStreamable format, we lean on the Lisp
;;;; reader/printer: each persistable view EXTERNALIZEs to a plist, and
;;;; INTERNALIZE rebuilds it.  SAVE-DESKTOP / LOAD-DESKTOP round-trip the open
;;;; windows of a desktop to a file.

(in-package #:tvision)

(defun %bounds-list (v)
  (let ((r (get-bounds v)))
    (list (rect-ax r) (rect-ay r) (rect-bx r) (rect-by r))))

(defun %list-bounds (l) (apply #'make-trect l))

(defun %child-forms (g)
  (loop for c in (reverse (group-subviews g))
        for f = (externalize c)
        when f collect f))

;;; --- externalize -----------------------------------------------------------

(defgeneric externalize (v)
  (:documentation "Return a serialisable plist for V, or NIL if not persistable.")
  (:method ((v tview)) nil))

(defmethod externalize ((w twindow))
  (list :window :bounds (%bounds-list w) :title (window-title w)
        :number (window-number w) :flags (window-flags w)
        :children (%child-forms w)))

(defmethod externalize ((d tdialog))
  (list :dialog :bounds (%bounds-list d) :title (window-title d)
        :children (%child-forms d)))

(defmethod externalize ((v tstatic-text))
  (list :static-text :bounds (%bounds-list v) :text (static-text-text v)))

(defmethod externalize ((b tbutton))
  (list :button :bounds (%bounds-list b) :title (button-title b)
        :command (button-command b) :default (button-default-p b)))

(defmethod externalize ((il tinputline))
  (list :input :bounds (%bounds-list il) :data (input-data il)
        :maxlen (input-maxlen il)))

(defmethod externalize ((c tcheck-boxes))
  (list :checkboxes :bounds (%bounds-list c) :labels (cluster-labels c)
        :value (cluster-value c)))

(defmethod externalize ((c tradio-buttons))
  (list :radio :bounds (%bounds-list c) :labels (cluster-labels c)
        :value (cluster-value c)))

(defmethod externalize ((lb tlist-box))
  (list :listbox :bounds (%bounds-list lb)
        :items (loop for i below (list-count lb) collect (list-item lb i))
        :command (list-command lb)))

(defmethod externalize ((tv ttext-view))
  (list :textview :bounds (%bounds-list tv) :text (text-string tv)
        :read-only (text-read-only tv)))

;;; --- internalize -----------------------------------------------------------

(defun internalize (form)
  "Rebuild a view from a plist produced by EXTERNALIZE."
  (when (and (consp form) (keywordp (first form)))
    (let* ((type (first form)) (p (rest form))
           (b (%list-bounds (getf p :bounds))))
      (flet ((mk (class &rest args) (apply #'make-instance class :bounds b args)))
        (case type
          (:window
           (let ((w (mk 'twindow :title (getf p :title)
                        :number (getf p :number)
                        :flags (getf p :flags))))
             (dolist (cf (getf p :children))
               (let ((c (internalize cf))) (when c (insert w c))))
             w))
          (:dialog
           (let ((d (mk 'tdialog :title (getf p :title))))
             (dolist (cf (getf p :children))
               (let ((c (internalize cf))) (when c (insert d c))))
             d))
          (:static-text (mk 'tstatic-text :text (getf p :text)))
          (:button (mk 'tbutton :title (getf p :title)
                       :command (getf p :command) :default (getf p :default)))
          (:input (mk 'tinputline :data (getf p :data) :maxlen (getf p :maxlen)))
          (:checkboxes (mk 'tcheck-boxes :labels (getf p :labels) :value (getf p :value)))
          (:radio (mk 'tradio-buttons :labels (getf p :labels) :value (getf p :value)))
          (:listbox (mk 'tlist-box :items (getf p :items) :command (getf p :command)))
          (:textview (mk 'ttext-view :text (getf p :text) :read-only (getf p :read-only)))
          (t nil))))))

;;; --- save / load ------------------------------------------------------------

(defun save-desktop (path &optional (app *application*))
  "Write the desktop's windows to PATH as a readable S-expression."
  (with-open-file (s path :direction :output :if-exists :supersede
                          :if-does-not-exist :create)
    (let ((*print-readably* nil) (*print-pretty* t))
      (prin1 (list :desktop
                   (loop for w in (reverse (desktop-windows (program-desktop app)))
                         for f = (externalize w) when f collect f))
             s)
      (terpri s)))
  path)

(defun load-desktop (path &optional (app *application*))
  "Recreate windows saved by SAVE-DESKTOP, inserting them on the desktop."
  (with-open-file (s path :if-does-not-exist nil)
    (when s
      (let ((form (read s nil nil)))
        (when (and (consp form) (eq (first form) :desktop))
          (dolist (wf (second form))
            (let ((w (internalize wf)))
              (when w (insert (program-desktop app) w))))
          t)))))
