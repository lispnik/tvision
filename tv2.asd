;;;; tv2.asd --- An experimental CLOS-native re-architecture kernel for tvision.
;;;;
;;;; tv2 is a clean-break redesign of the framework's *dispatch and construction*
;;;; layers: state is reactive (a metaclass invalidates the screen on mutation),
;;;; events are CLOS classes dispatched by multimethods, input resolves to named
;;;; commands through layered keymaps, and behaviour lives in command objects --
;;;; not 138 integer constants and a central dispatch COND.
;;;;
;;;; It reuses tvision's terminal driver, screen/cell buffer, geometry and the
;;;; outline-node data structure unchanged; only the plumbing is new.

(asdf:defsystem "tv2"
  :description "Experimental CLOS-native kernel for the tvision TUI framework."
  :depends-on ("tvision")
  :serial t
  :components ((:module "tv2"
                :serial t
                :components ((:file "package")
                             (:file "kernel")
                             (:file "runtime")
                             (:file "outline")
                             (:file "widgets")
                             (:file "scrollback")
                             (:file "layout")
                             (:file "modal")
                             (:file "threadmon")
                             (:file "browser")
                             (:file "project")
                             (:file "repl")
                             (:file "editor")
                             (:file "html")))))
