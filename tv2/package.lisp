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
   #:cluster #:cluster-value #:cluster-items
   #:table-view #:table-rows #:table-columns #:table-selected
   #:filter-validator #:range-validator #:picture-validator #:digits-validator #:input-validator #:input-history-id
   #:make-file-dialog #:make-color-dialog #:make-help #:*help-pages*
   #:text-edit #:te-load #:te-save #:te-text #:te-set-text #:te-filename #:te-modified #:te-colorizer #:te-find #:te-find-regex #:te-replace-all
   #:lisp-colorize #:*lisp-indenter*
   #:flex-score #:fuzzy-filter
   ;; demo + ported real windows
   #:run #:run-threadmon
   #:run-browser #:run-packages #:run-systems #:run-project #:*project-status-fn* #:*project-grep-fn*
   #:*object->outline-fn* #:make-inspector #:*profile-fn* #:make-table-window #:*paredit-fn*
   #:run-repl #:repl-window #:repl-package #:*repl-eval-fn* #:repl-hist-vars #:repl-busy #:repl-submit-string #:repl-last-value #:repl-last-value-p
   #:run-editor #:*editor-eval-fn* #:*editor-completions-fn* #:*paren-matcher*
   #:html-view #:set-html #:run-html #:make-doc-browser #:*url-fetch-fn* #:*hyperspec-url-fn*
   #:run-app #:run-menu #:*app-windows* #:*desktop* #:ensure-repl
   #:run-desktop #:desktop #:menu-bar #:status-bar))
