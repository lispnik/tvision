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
   ;; persistence + worker->UI bridge
   #:serialize #:deserialize #:persistent-class #:save-object #:load-object
   #:session #:session-filter #:session-line #:session-file
   #:run-on-ui #:drain-ui-callbacks #:*ui-thread*
   ;; modal dialogs + validation
   #:dialog #:dialog-result #:exec-view #:validation-error #:validation-message #:fail-validation
   ;; widgets
   #:outline #:outline-roots #:outline-focused
   #:window #:window-title #:button #:button-label #:button-command #:static-text
   #:input-line #:input-text #:input-caret #:input-on-change
   #:list-box #:list-items #:list-selected #:list-on-activate
   #:scrollback #:scrollback-append #:scrollback-clear #:sb-scroll #:sb-follow
   #:text-edit #:te-load #:te-save #:te-text #:te-set-text #:te-filename #:te-modified
   ;; demo + ported real windows
   #:run #:run-threadmon
   #:run-browser #:run-packages #:run-systems #:run-project
   #:run-repl #:repl-window #:repl-package
   #:run-editor
   #:html-view #:set-html #:run-html))
