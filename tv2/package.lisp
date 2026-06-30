;;;; package.lisp --- the tv2 kernel package.

(defpackage #:tv2
  (:use #:cl)
  (:documentation "Experimental CLOS-native kernel for the tvision TUI framework.
Reuses tvision's renderer/geometry/outline-node data; the dispatch, command, and
event layers are new.")
  (:export
   ;; reactive core
   #:reactive-class #:invalidate
   ;; views
   #:view #:view-bounds #:view-owner #:view-keymap #:draw
   ;; events + dispatch
   #:event #:key-event #:mouse-event #:mouse-down #:wheel-event
   #:command-event #:broadcast-event #:idle-event
   #:event-keysym #:event-modifiers #:event-where #:event-delta #:handled-p
   #:handle-event
   ;; keymaps
   #:keymap #:keymap-parent #:bind-key #:keymap-lookup #:defkeymap
   ;; commands
   #:command #:command-name #:command-enabled-p #:register-command
   #:define-command #:perform
   ;; theming + focus + containers
   #:*theme* #:role #:focusable-p #:view-focused-p #:view-name #:view-root #:find-view
   #:container #:subviews #:container-focus #:add-subview #:focus-next #:all-focusables
   ;; layout DSL
   #:layout #:stack #:row #:add-laid #:rect #:ui
   ;; widgets
   #:outline #:outline-roots #:outline-focused
   #:window #:window-title #:button #:button-label #:button-command #:static-text
   ;; demo
   #:run))
