;;;; hello-world.lisp --- the smallest tv2 program.
;;;;
;;;; Builds one window with a greeting and an OK button, then runs it full-screen
;;;; until the user quits.  Shows the essentials of a standalone tv2 app: the UI
;;;; DSL (WINDOW / STACK / ROW / STATIC-TEXT / BUTTON), the global keymap (Esc / q
;;;; quit), a command on a button, and RUN-VIEW as the entry point.
;;;;
;;;; Run from the repo root:  sbcl --script examples/hello-world.lisp
;;;; Quit with Esc, q, or the OK button.

(require :asdf)
(asdf:load-asd (truename "tv2.asd"))
(handler-bind ((warning #'muffle-warning)) (asdf:load-system :tv2))

(defpackage #:tv2-hello (:use #:common-lisp #:tv2))
(in-package #:tv2-hello)

(defun hello-world ()
  (let ((win (ui (window (:title " tv2 " :keymap *global-keys*)
                   (stack
                     (:fill (static-text :text ""))                 ; top spacer
                     (1 (row (:fill (static-text :text ""))          ; centred greeting
                             (13 (static-text :role :label :text "Hello, World!"))
                             (:fill (static-text :text ""))))
                     (1 (static-text :text ""))
                     (1 (row (:fill (static-text :text ""))          ; centred OK button
                             (8  (button :label "OK" :command 'quit))
                             (:fill (static-text :text ""))))
                     (:fill (static-text :text ""))                 ; bottom spacer
                     (1 (static-text :role :status :text " Press Esc, q, or OK to quit ")))))))
    (run-view win)))                                                ; runs until QUIT sets *running* nil

(hello-world)
