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
                             (:file "outline")
                             (:file "history")
                             (:file "filedialog")
                             (:file "chdir")
                             (:file "colordialog")
                             (:file "help")
                             (:file "persist")
                             (:file "stream")
                             (:file "threadmon")
                             (:file "repl")))))

(asdf:defsystem "tvision/examples"
  :description "Demo applications for the Turbo Vision Common Lisp port."
  :depends-on ("tvision")
  :serial t
  ;; `asdf:make :tvision/examples` dumps a standalone executable named
  ;; tvision-demo (in this directory) whose entry point launches the demo.
  :build-operation "program-op"
  :build-pathname "tvision-demo"
  :entry-point "tvision-demo:toplevel"
  :components ((:module "examples"
                :serial t
                :components ((:file "demo")))))

(asdf:defsystem "tvision/examples/textedit"
  :description "A multi-window text editor example for the Turbo Vision port."
  :depends-on ("tvision")
  :serial t
  ;; `asdf:make :tvision/examples/textedit` dumps a standalone `textedit'
  ;; executable; pass file paths on the command line to open them.
  :build-operation "program-op"
  :build-pathname "textedit"
  :entry-point "tvision-textedit:toplevel"
  :components ((:module "examples"
                :components ((:file "textedit")))))

(asdf:defsystem "tvision/examples/tvlisp"
  :description "A standalone Lisp REPL application on the Turbo Vision port."
  :depends-on ("tvision")
  :serial t
  ;; `asdf:make :tvision/examples/tvlisp` dumps a standalone `tvlisp' REPL.
  :build-operation "program-op"
  :build-pathname "tvlisp"
  :entry-point "tvision-tvlisp:toplevel"
  :components ((:module "examples"
                :components ((:file "tvlisp")))))
