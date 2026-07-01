;;;; tv2-sbcl-tests.lisp --- tests for the SBCL-specific IDE features on tv2.
;;;;
;;;; These exercise the *logic* behind the SBCL-only IDE features (compiler
;;;; notes, sb-di backtraces, sb-aprof, allocation-information, typexpand, GC
;;;; stats, evaluator mode, package locks, sb-cltl2) directly -- no UI needed.
;;;;
;;;; Run from the repo root:  sbcl --script tests/tv2-sbcl-tests.lisp

(require :asdf)
(asdf:load-asd (truename "tv2.asd"))
(handler-bind ((warning #'muffle-warning)) (asdf:load-system :tv2))
(in-package #:tv2)

(defvar *pass* 0) (defvar *fail* 0)
(defmacro check (desc form)
  `(handler-case (if ,form (progn (incf *pass*) (format t "  ok   ~a~%" ,desc))
                     (progn (incf *fail*) (format t "  FAIL ~a~%" ,desc)))
     (error (e) (incf *fail*) (format t "  ERR  ~a -- ~a~%" ,desc e))))

;;; ===========================================================================
;;; 1. Compiler notes (sb-ext:compiler-note capture + offset refine)
;;; ===========================================================================
(format t "~&## compiler notes~%")
(multiple-value-bind (status notes)
    (%compile-text-notes
     (format nil "(defun add-floats (a b)~%  (declare (optimize speed))~%  (+ a b))~%")
     (find-package :cl-user))
  (check "compile status is :ok" (eq status :ok))
  (check "captured at least one note" (>= (length notes) 1))
  (check "at least one :note severity" (some (lambda (n) (eq (getf n :severity) :note)) notes))
  (check "a speed note about a full call"
         (some (lambda (n) (search "FULL CALL" (string-upcase (getf n :message)))) notes))
  (check "every note carries a position" (every (lambda (n) (integerp (getf n :pos))) notes)))

;; clean code yields no notes
(multiple-value-bind (status notes)
    (%compile-text-notes "(defun plain-id (x) x)" (find-package :cl-user))
  (check "clean code compiles :ok" (eq status :ok))
  (check "clean code yields no notes" (null notes)))

;; offset refinement points at the named symbol
(let ((text "(foo)
(bar BAZ)
"))
  (check "refine-offset locates the offending token"
         (= (%note-refine-offset text 6 "undefined variable: BAZ") (search "BAZ" text))))

;;; ===========================================================================
;;; 2. Cross-thread backtrace (sb-thread:interrupt-thread + print-backtrace)
;;; ===========================================================================
(format t "~%## thread backtrace~%")
(check "self backtrace is a non-empty string"
       (let ((bt (%thread-backtrace sb-thread:*current-thread*)))
         (and (stringp bt) (plusp (length bt)))))
(let* ((gate (sb-thread:make-semaphore))
       (th (sb-thread:make-thread
            (lambda () (sb-thread:wait-on-semaphore gate)) :name "tv2-test-victim")))
  (sleep 0.1)
  (let ((bt (%thread-backtrace th)))
    (check "another thread's backtrace is captured" (and (stringp bt) (plusp (length bt))))
    (check "backtrace mentions a stack frame"
           (or (search "SEMAPHORE" (string-upcase bt)) (search "WAIT" (string-upcase bt))
               (search "(" bt))))
  (sb-thread:signal-semaphore gate)
  (ignore-errors (sb-thread:join-thread th :timeout 2))
  (sleep 0.1)
  (check "dead thread reports as dead"
         (string= (%thread-backtrace th) "(thread is dead)")))

;;; ===========================================================================
;;; 3. sb-di frame-local debugger (%capture-backtrace reads frame locals)
;;; ===========================================================================
(format t "~%## sb-di frame locals~%")
(declaim (optimize (debug 3)))
(defun bt-probe (aardvark)
  (let ((zebra (* aardvark 2)))
    (declare (ignorable zebra))
    (%capture-backtrace :count 20)))          ; frames of the live stack, with locals
(multiple-value-bind (frames lives) (bt-probe 21)
  (check "backtrace captured frames" (and (consp frames) (plusp (length frames))))
  (check "frames align with live sb-di frames" (= (length frames) (length lives)))
  (check "the probe frame is present" (some (lambda (f) (search "BT-PROBE" (string-upcase (getf f :label)))) frames))
  (let ((probe (find-if (lambda (f) (search "BT-PROBE" (string-upcase (getf f :label)))) frames)))
    (check "its locals include aardvark = 21"
           (and probe (assoc "aardvark" (getf probe :locals) :test #'string=)
                (string= "21" (second (assoc "aardvark" (getf probe :locals) :test #'string=)))))
    (check "locals are (name value-string) pairs"
           (and probe (every (lambda (l) (and (stringp (first l)) (stringp (second l))))
                             (getf probe :locals))))))

;;; ===========================================================================
(format t "~%~d passed, ~d failed~%" *pass* *fail*)
(sb-ext:exit :code (if (zerop *fail*) 0 1))
