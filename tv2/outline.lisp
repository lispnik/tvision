;;;; outline.lisp --- the outline widget ported onto the tv2 kernel.
;;;;
;;;; Compare with src/outline.lisp (the classic TOutline): there is no integer
;;;; command, no event-type COND, and no manual DRAW-VIEW calls.  Navigation
;;;; keys are data (a keymap), each maps to a named command, mutating a reactive
;;;; slot (FOCUSED/TOP) repaints automatically, and colours come from theme
;;;; roles.  The lazy outline-node data structure is reused from tvision intact.

(in-package #:tv2)

(defvar *running* nil)

(defclass outline (view)
  ((roots   :initarg :roots :initform '() :accessor outline-roots)
   (focused :initform 0 :accessor outline-focused)
   (top     :initform 0 :accessor outline-top))           ; first visible row
  (:metaclass reactive-class)
  (:documentation "A collapsible tree; FOCUSED/TOP are reactive."))

(defmethod focusable-p ((ol outline)) t)

(defun ov-visible (roots)
  "List of (NODE . DEPTH) for every visible node, loading lazy children en route."
  (let ((out '()))
    (labels ((walk (nodes depth)
               (dolist (n nodes)
                 (push (cons n depth) out)
                 (when (tvision:outline-node-expanded n)
                   (tvision:outline-ensure-children n)               ; lazy load
                   (walk (tvision:outline-node-children n) (1+ depth))))))
      (walk roots 0))
    (nreverse out)))

(defmethod draw ((ol outline))
  (let* ((b (view-bounds ol)) (h (tvision::rect-height b)) (w (tvision::rect-width b))
         (vis (ov-visible (outline-roots ol))) (top (outline-top ol))
         (active (or (null (view-owner ol)) (view-focused-p ol))))
    (dotimes (row h)
      (let* ((i (+ top row))
             (nd (and (< i (length vis)) (nth i vis)))
             (sel (and (= i (outline-focused ol)) active))
             (color (and nd (tvision:outline-node-color (car nd))))
             (attr (cond (sel (role :focused))
                         (color (tvision:make-attr color 1))
                         (t (role :normal)))))
        (fill-row ol 0 row w attr)
        (when nd
          (destructuring-bind (n . depth) nd
            (draw-text ol 0 row
                       (concatenate 'string
                                    (make-string (* 2 depth) :initial-element #\Space)
                                    (cond ((not (tvision:outline-node-expandable-p n)) "  ")
                                          ((tvision:outline-node-expanded n) "- ")
                                          (t "+ "))
                                    (tvision:outline-node-text n))
                       attr)))))))

;;; --- navigation helpers (mutate reactive slots -> auto repaint) -------------

(defun ov-current (ol)
  (let ((vis (ov-visible (outline-roots ol))))
    (and (< (outline-focused ol) (length vis)) (car (nth (outline-focused ol) vis)))))

(defun ov-scroll-to-focus (ol)
  (let ((h (tvision::rect-height (view-bounds ol))) (f (outline-focused ol)) (top (outline-top ol)))
    (cond ((< f top) (setf (outline-top ol) f))
          ((>= f (+ top h)) (setf (outline-top ol) (1+ (- f h)))))))

(defun ov-move (ol delta)
  (let ((n (length (ov-visible (outline-roots ol)))))
    (when (plusp n)
      (setf (outline-focused ol) (min (max 0 (+ (outline-focused ol) delta)) (1- n)))
      (ov-scroll-to-focus ol))))

;;; --- commands + keymap ------------------------------------------------------

(define-command quit (v e) (setf *running* nil))
(define-command cursor-up   (v e) (ov-move v -1))
(define-command cursor-down (v e) (ov-move v 1))

(define-command activate (v e)
  (let ((n (ov-current v)))
    (when (and n (tvision:outline-node-expandable-p n))
      (setf (tvision:outline-node-expanded n) (not (tvision:outline-node-expanded n)))
      (when (tvision:outline-node-expanded n) (tvision:outline-ensure-children n))
      (invalidate v))))                ; the node is a struct (not reactive) -> repaint by hand

(define-command collapse (v e)
  (let ((n (ov-current v)))
    (if (and n (tvision:outline-node-expandable-p n) (tvision:outline-node-expanded n))
        (progn (setf (tvision:outline-node-expanded n) nil) (invalidate v))
        (ov-move v -1))))

(defkeymap *global-keys* ()
  (#\q quit)
  (:esc quit))   ; an escape hatch that works even while a text field is focused

(defkeymap *outline-keys* (*global-keys*)
  (:up    cursor-up)
  (:down  cursor-down)
  (:enter activate)
  (:right activate)
  (:left  collapse))

;;; --- sample data ------------------------------------------------------------

(defun demo-roots ()
  "A small hand-built tree, including a lazily-loaded directory and a tinted node."
  (flet ((file (name &optional color)
           (let ((n (tvision:make-outline-node name nil :file)))
             (when color (setf (tvision:outline-node-color n) color))
             n)))
    (let* ((utils (tvision:make-outline-node "utils/" (list (file "strings.lisp")
                                                            (file "math.lisp"))))
           (lazy  (tvision:make-outline-node "vendor/  (lazy)")))
      (setf (tvision:outline-node-loader lazy)
            (lambda () (list (file "big-lib.lisp") (file "more.lisp"))))
      (let* ((src  (tvision:make-outline-node "src/" (list utils lazy (file "main.lisp" 14))))
             (root (tvision:make-outline-node "my-project"
                                              (list src (file "README.md") (file ".gitignore")))))
        (setf (tvision:outline-node-expanded utils) t
              (tvision:outline-node-expanded src) t
              (tvision:outline-node-expanded root) t)
        (list root)))))
