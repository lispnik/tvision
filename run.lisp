;;;; run.lisp --- Load the framework and launch the demo application.
;;;; Usage:  sbcl --script run.lisp        (or:  ./run-demo.sh)

(require :asdf)
(asdf:load-system :tvision/examples)

(handler-case
    (tvision-demo:main)
  (error (e)
    ;; the terminal is already restored by WITH-SCREEN's unwind-protect
    (format *error-output* "~&Error: ~a~%" e)
    (sb-ext:exit :code 1)))

(sb-ext:exit :code 0)
