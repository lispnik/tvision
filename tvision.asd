;;;; tvision.asd --- ASDF system definition for the Common Lisp port of Turbo Vision
;;;;
;;;; Loadable with plain SBCL (the cwd is on the ASDF source registry via the
;;;; ocicl runtime configured in ~/.sbclrc).  No external dependencies.

(asdf:defsystem "tvision"
  :description "A Common Lisp port of Borland's Turbo Vision text-mode UI framework."
  :author "ported with Claude Code"
  :license "MIT"
  :version "0.1.0"
  :depends-on ()
  :serial t
  :components ((:module "src"
                :serial t
                :components ((:file "package")
                             (:file "geometry")
                             (:file "colors")
                             (:file "draw-buffer")
                             (:file "events")
                             (:file "screen")
                             (:file "concurrency")
                             (:file "view")
                             (:file "group")
                             (:file "frame")
                             (:file "scrollbar")
                             (:file "window")
                             (:file "desktop")
                             (:file "validator")
                             (:file "widgets")
                             (:file "cluster")
                             (:file "dialog")
                             (:file "statusline")
                             (:file "program")
                             (:file "menu")
                             (:file "scroller")
                             (:file "textview")
                             (:file "collection")
                             (:file "listbox")
                             (:file "tableview")
                             (:file "fuzzy")
                             (:file "outline")
                             (:file "htmlview")
                             (:file "history")
                             (:file "filedialog")
                             (:file "chdir")
                             (:file "colordialog")
                             (:file "help")
                             (:file "persist")
                             (:file "stream")
                             (:file "threadmon")
                             (:file "repl")))))

(asdf:defsystem "tvision/tests"
  :description "Headless test suite for the Turbo Vision controls (uses FiveAM).
Only the tests depend on FiveAM; the tvision library and the example binaries
have no external dependencies."
  :depends-on ("tvision" "fiveam")
  :serial t
  :components ((:module "tests"
                :components ((:file "tvision-tests"))))
  :perform (asdf:test-op (o c)
             (uiop:symbol-call :tvision-tests :run-tests)))
