#lang racket/base

;; ui-core/ui-state-protocol.rkt -- Shared UI state protocol (F3 extraction)
;;
;; Re-exports ui-state struct and its protocol functions from tui/state-types
;; so that extensions and other non-TUI modules can depend on the protocol
;; without creating a direct tui/ dependency.
;;
;; STABILITY: evolving
;; CONSUMERS: extensions/dialog-api.rkt, extensions/widget-lifecycle.rkt

(require racket/contract
         (only-in "../tui/state-types.rkt"
                  ;; Struct type and predicate
                  ui-state
                  ui-state?
                  ;; Status bar protocol
                  ui-state-status-message
                  set-status-message))

(provide ;; Struct predicate
         (contract-out [ui-state? (-> any/c boolean?)]
                       ;; Status bar protocol
                       [ui-state-status-message (-> ui-state? (or/c string? #f))]
                       [set-status-message (-> ui-state? (or/c string? #f) ui-state?)]
                       ;; Struct constructor -- re-exported for convenience
                       [ui-state any/c]))
