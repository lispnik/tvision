;;;; package.lisp --- Package definition for the Turbo Vision port.

(defpackage #:tvision
  (:nicknames #:tv)
  (:use #:common-lisp)
  (:export
   ;; geometry
   #:tpoint #:make-tpoint #:point-x #:point-y #:point-equal-p #:copy-point
   #:trect #:make-trect #:rect-ax #:rect-ay #:rect-bx #:rect-by
   #:rect-width #:rect-height #:rect-empty-p #:rect-contains-p
   #:rect-move #:rect-grow #:rect-intersect #:rect-union #:rect-equal-p
   #:copy-rect #:rect-assign
   ;; colors
   #:attr #:make-attr #:attr-fg #:attr-bg #:attr->ansi
   #:tpalette #:make-palette #:palette-ref
   #:*color-mode* #:detect-color-mode #:*rgb-theme* #:set-color-theme
   #:make-rgb-theme #:+theme-vga+ #:+theme-modern+ #:+theme-green+ #:+theme-amber+
   #:make-rgb #:rgb-attr #:pack-rgb #:attr-rgb-p #:attr-rgb-fg #:attr-rgb-bg
   ;; draw buffer
   #:draw-buffer #:make-draw-buffer #:db-width
   #:db-move-char #:db-move-str #:db-move-cstr #:db-put-attribute
   #:db-put-char #:db-move-buf #:db-fill #:char-width #:string-width
   ;; events
   #:event #:make-event #:event-type #:event-key-code #:event-char-code
   #:event-mouse-where #:event-mouse-buttons #:event-command #:event-info
   #:event-modifiers #:event-double #:event-triple #:event-wheel
   #:clear-event #:set-double-click-time
   #:+ev-nothing+ #:+ev-mouse-down+ #:+ev-mouse-up+ #:+ev-mouse-move+
   #:+ev-mouse-auto+ #:+ev-key-down+ #:+ev-mouse-wheel+ #:+ev-command+ #:+ev-broadcast+
   #:+ev-mouse+ #:+ev-keyboard+ #:+ev-message+
   #:+mb-left+ #:+mb-right+ #:+md-shift+ #:+md-ctrl+ #:+md-alt+ #:+mw-up+ #:+mw-down+
   #:+kb-esc+ #:+kb-enter+ #:+kb-tab+ #:+kb-back+ #:+kb-up+ #:+kb-down+
   #:+kb-left+ #:+kb-right+ #:+kb-home+ #:+kb-end+ #:+kb-pgup+ #:+kb-pgdn+
   #:+kb-ins+ #:+kb-del+ #:+kb-f1+ #:+kb-f2+ #:+kb-f3+ #:+kb-f4+ #:+kb-f5+
   #:+kb-f6+ #:+kb-f7+ #:+kb-f8+ #:+kb-f9+ #:+kb-f10+ #:+kb-shift-tab+
   #:+kb-alt-x+ #:+kb-ctrl-w+ #:+kb-space+
   ;; commands
   #:+cm-quit+ #:+cm-close+ #:+cm-zoom+ #:+cm-ok+ #:+cm-cancel+ #:+cm-yes+
   #:+cm-no+ #:+cm-next+ #:+cm-prev+ #:+cm-default+ #:+cm-cut+ #:+cm-copy+
   #:+cm-paste+ #:+cm-menu+ #:+cm-valid+ #:+cm-released+ #:+cm-receivedfocus+
   #:+cm-command-set-changed+ #:*command-set-changed*
   ;; command set
   #:command-enabled-p #:enable-command #:disable-command
   #:enable-commands #:disable-commands #:set-command-enabled #:reset-commands
   ;; state / option / growmode flags
   #:+sf-visible+ #:+sf-cursor-vis+ #:+sf-cursor-ins+ #:+sf-shadow+
   #:+sf-active+ #:+sf-selected+ #:+sf-focused+ #:+sf-dragging+
   #:+sf-disabled+ #:+sf-modal+ #:+sf-exposed+
   #:+of-selectable+ #:+of-top-select+ #:+of-first-click+ #:+of-framed+
   #:+of-pre-process+ #:+of-post-process+ #:+of-centerx+ #:+of-centery+
   #:+of-center+
   #:+gf-grow-lox+ #:+gf-grow-loy+ #:+gf-grow-hix+ #:+gf-grow-hiy+
   #:+gf-grow-all+ #:+gf-grow-rel+
   #:+hc-no-context+
   ;; screen driver
   #:init-screen #:done-screen #:with-screen #:screen-width #:screen-height
   #:flush-screen #:screen-back-buffer #:screen-cell-set #:screen-resize
   #:screen-invalidate #:set-mouse-cursor
   #:show-cursor #:hide-cursor #:set-cursor-pos #:set-cursor-shape #:*screen*
   ;; view
   #:tview #:view-origin #:view-size #:view-cursor #:view-owner #:view-next
   #:view-state #:view-options #:view-grow-mode #:view-drag-mode #:view-help-ctx
   #:get-rect #:get-bounds #:get-extent #:get-clip-rect #:size-limits
   #:set-bounds #:change-bounds #:calc-bounds #:grow-to #:move-to #:locate
   #:draw #:draw-view #:get-color #:get-palette #:default-palette
   #:set-state #:get-state #:handle-event #:put-event #:clear-event*
   #:write-buf #:write-line* #:write-char* #:write-str #:exposed-p
   #:show #:hide #:set-cursor #:show-cursor* #:hide-cursor* #:normal-cursor
   #:block-cursor #:make-global #:make-local #:mouse-in-view-p
   #:view-event-mask #:wants-event-p #:view-disabled-p #:disable-view #:enable-view
   #:valid-p #:data-size #:get-data #:set-data #:focus #:select
   #:event-error #:end-modal #:owner-group
   ;; group
   #:tgroup #:group-last #:group-current #:group-subviews #:group-buffer #:group-phase
   #:insert #:insert-before #:remove-view #:group-draw-subviews
   #:select-next #:set-current #:exec-view #:redraw #:focus-next
   #:first-that #:for-each #:foreach-view #:exec #:end-exec #:data-views
   #:+phase-pre+ #:+phase-focused+ #:+phase-post+
   ;; frame / scrollbar
   #:tframe #:tscrollbar #:sb-value #:sb-set-params #:sb-set-value
   ;; scroller
   #:tscroller #:scroller-delta #:scroller-limit #:scroller-hscroll
   #:scroller-vscroll #:scroll-to #:scroll-draw #:set-scroller-limit
   #:attach-scrollbars #:scroll-from-scrollbars #:scrollbar-event-p
   ;; collection
   #:tcollection #:tsorted-collection #:make-collection #:make-sorted-collection
   #:string-collection #:collection-count #:at #:insert-item #:at-insert
   #:at-remove #:delete-item #:index-of #:collection-for-each #:collection-list
   #:collection-clear
   ;; listbox
   #:tlist-box #:list-focused #:list-count #:list-item #:list-set-items
   #:list-focus-item #:list-select #:list-command #:list-columns
   #:+cm-list-item-selected+ #:+cm-list-focus-changed+
   #:tlist-viewer #:tsorted-list-box #:slb-search #:slb-find
   ;; table view (sortable grid)
   ;; fuzzy filtering
   #:flex-score #:fuzzy-filter-mixin #:tfilter-list-box #:tfilter-table
   #:ff-all #:ff-key #:ff-display #:ff-query #:ff-self-edit #:ff-on-change
   #:ff-filtering #:ff-end-filter
   #:ff-set-query #:ff-set-all #:ff-refilter #:ff-focused #:ff-visible
   #:ttable-view #:make-table-column #:table-columns #:table-rows #:table-set-rows
   #:table-selected-row #:table-sort #:table-sort-by #:table-focused
   #:table-sort-col #:table-sort-asc #:table-column-title #:table-column-width
   ;; outline (tree view)
   #:toutline #:outline-node #:make-outline-node #:outline-node-text
   #:outline-node-children #:outline-node-expanded #:outline-node-data #:outline-node-setter
   #:outline-roots #:outline-current #:outline-focus #:outline-focused
   #:outline-update-limit #:outline-toggle
   #:outline-select #:outline-command #:+cm-outline-item-selected+
   ;; html view (hypertext browser)
   #:thtml-view #:set-html #:html-source #:html-current-href #:html-focused-link
   #:html-next-link #:html-focus-link #:html-activate-link #:html-link-count
   #:html->tokens #:html-document-title #:html-find #:html-find-next #:+cm-html-link+
   #:html-goto-anchor #:html-find-regex #:html-anchors
   ;; window
   #:twindow #:tcyan-window #:window-title #:window-number #:window-flags #:window-frame
   #:close-window #:zoom-window #:standard-scrollbar
   #:+wf-move+ #:+wf-grow+ #:+wf-close+ #:+wf-zoom+ #:+wn-no-number+
   ;; desktop
   #:tdesktop #:desktop-background #:tbackground #:tile #:cascade #:desktop-windows
   ;; widgets
   #:tstatic-text #:tlabel #:tbutton #:tinputline #:tparam-text #:set-param-text
   #:button-title #:button-command #:input-data #:make-button
   ;; cluster / radio / checkboxes
   #:tcluster #:tcheck-boxes #:tradio-buttons #:cluster-labels #:cluster-value
   #:cluster-mark #:cluster-press #:multi-state-p #:checkbox-value
   #:tmulti-check-boxes #:mcb-states #:mcb-state #:mcb-bits
   ;; validators
   #:tvalidator #:tfilter-validator #:trange-validator #:tpicture-validator
   #:tlookup-validator #:validator-lookup
   #:tstring-lookup-validator #:make-string-lookup-validator
   #:make-filter-validator #:make-range-validator #:make-picture-validator
   #:is-valid #:is-valid-input #:validator-error-message #:input-validator
   #:picture-match
   ;; history
   #:thistory-input #:thistory-viewer #:thistory-window
   #:history-id #:history-add #:history-list #:history-clear
   #:history-record
   ;; dialog
   #:tdialog #:message-box #:input-box
   #:+mf-warning+ #:+mf-error+ #:+mf-information+ #:+mf-confirmation+
   #:+mf-yes-button+ #:+mf-no-button+ #:+mf-ok-button+ #:+mf-cancel-button+
   ;; statusline
   #:tstatus-line #:tstatus-item #:tstatus-def #:make-status-item
   #:make-status-def #:status-defs #:set-status-context
   ;; file dialog
   #:tfile-dialog #:make-file-dialog #:file-open-dialog #:file-save-dialog
   #:tfile-input-line #:tfile-info-pane #:fd-filter #:%wild-match
   ;; change-directory dialog
   #:tchdir-dialog #:tdir-list-box #:chdir-dialog #:make-chdir-dialog
   ;; color dialog
   #:color-dialog #:color-preview #:+color-names+
   #:tcolor-selector #:cs-color #:cs-range #:tcolor-display #:cd-fg #:cd-bg
   #:tmono-selector #:make-mono-selector #:mono-selector-attr #:+mono-attrs+
   ;; help
   #:register-help #:register-help-topic #:help-text #:help-topic #:open-help
   #:current-help-ctx #:refresh-status-context #:thelp-viewer #:parse-help-links
   ;; persistence
   #:externalize #:internalize #:save-desktop #:load-desktop
   ;; binary streams / resource files
   #:stream-write-value #:stream-read-value #:stream-write-view #:stream-read-view
   #:write-u8 #:read-u8 #:write-u32 #:read-u32 #:write-bstring #:read-bstring
   #:tresource-file #:make-resource-file #:resource-put #:resource-get
   #:resource-names #:resource-put-object #:resource-get-object
   #:save-resource-file #:load-resource-file
   ;; concurrency / worker->UI bridge
   #:make-mailbox #:mailbox-send #:mailbox-receive #:mailbox-try-receive
   #:run-on-ui #:drain-ui-callbacks #:install-ui-wakeup #:remove-ui-wakeup
   #:*ui-callbacks* #:*input-multiplexer* #:shutdown-background-threads
   ;; NOTE: the thread-monitor and REPL symbols moved to the tvlisp project
   ;; (src/threadmon.lisp, src/repl.lisp); they export into this package there.
   ;; menu
   #:tmenu-bar #:menu #:menu-item #:new-menu #:sub-menu #:menu-separator
   #:menu-items #:menu-bar-menu #:track-menu #:application-menu #:init-menu-bar
   #:program-menu-bar #:find-shortcut #:popup-menu
   #:tmenu-popup #:make-menu-popup #:menu-popup-exec #:menu-popup-menu
   #:menu-popup-size
   ;; text view
   #:ttext-view #:text-lines #:text-string #:set-text #:append-text
   #:text-cur-line #:text-cur-col #:text-return #:text-read-only
   #:line-count #:nth-line #:current-line-string #:ensure-visible
   #:set-line #:selection-range #:text-left-col #:text-top-line
   #:text-attach-scrollbars #:text-update-limit
   #:set-protect-boundary #:text-protect #:insert-string #:selected-string
   #:copy-selection #:cut-selection #:paste-clipboard #:select-all #:*clipboard*
   #:text-snapshot #:text-undo! #:text-redo! #:text-anchor #:set-lines
   #:text-modified #:text-overwrite #:text-goto #:word-left #:word-right
   #:text-find #:text-find-and-select #:text-select-match #:text-replace-all
   #:match-paren-jump
   #:text-find-regex #:text-find-and-select-regex #:text-replace-all-regex #:text-select-span
   #:text-replace-selection #:text-load-file #:text-save-file
   #:tindicator #:indicator-source #:tmemo #:text-wrap #:set-text-wrap
   #:tfile-editor #:teditor-window #:make-edit-window #:editor-filename
   #:editor-window-editor #:text-highlight
   #:text-gutter-width #:draw-gutter #:text-area-width
   #:lisp-indent-line #:lisp-indent-region #:lisp-indent-sexp #:*lisp-indent-hook*
   ;; resize
   #:apply-resize #:install-resize-handler
   ;; program / application
   #:tprogram #:tapplication #:program-desktop #:program-status-line
   #:*application* #:run #:init-desktop #:init-status-line #:init-screen-program
   #:get-event #:idle #:program-loop #:suspend #:resume #:set-screen-mode
   #:*event-error-hook*
   ;; palette modes / window management
   #:set-palette-mode #:program-palette-mode #:select-window-by-number
   #:move-size-window #:resize-window #:drag-window
   #:+cm-resize+))
