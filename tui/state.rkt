#lang racket

;; tui/state.rkt — UI state and event→state reduction
;;
;; Re-export facade. Implementation split into three modules (v0.22.6 W2):
;;   state-types.rkt  — Structs, constructors, entry/cache helpers
;;   state-events.rkt — Event→state reduction (apply-event-to-state)
;;   state-ui.rkt     — Scroll, selection, overlay, widgets, queries
;;
;; All existing consumers continue to work unchanged.

(require "state-types.rkt"
         "state-events.rkt"
         "state-ui.rkt")

(provide (all-from-out "state-types.rkt")
         apply-event-to-state
         transcript-entries
         add-transcript-entry
         visible-entries
         scroll-up
         scroll-down
         scroll-to-bottom
         scroll-to-top
         ui-busy?
         ui-session-label
         ui-model-label
         ui-status-text
         set-current-branch
         set-visible-branches
         clear-visible-branches
         show-overlay
         update-overlay-input
         dismiss-overlay
         overlay-active?
         ANCHOR-TOP-LEFT
         ANCHOR-CENTER
         ANCHOR-BOTTOM-RIGHT
         anchor?
         (struct-out overlay-config)
         overlay-config?
         make-overlay-config
         show-overlay-with-config
         overlay-compute-bounds
         set-selection-anchor
         set-selection-end
         clear-selection
         has-selection?
         set-extension-widget
         remove-extension-widget
         remove-all-extension-widgets
         get-widget-lines-above
         get-widget-lines-below
         set-custom-header
         set-custom-footer
         clear-custom-header
         clear-custom-footer
         set-focused-component
         clear-focused-component
         set-editor-component
         clear-editor-component
         get-last-turn-tool-summary
         truncate-string
         extract-arg-summary
         current-gsd-mode-query)
