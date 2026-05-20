#lang racket/base
;; STABILITY: evolving

;; q/runtime/context-assembly.rkt — Unified context assembly pipeline
;;
;; Thin facade: re-exports from context-assembly/budgeting, selection, serialization.
;; Also re-exports from context-summary.rkt (sibling module).
;; All struct definitions live in sub-modules (single source of truth).

(require racket/contract
         "context-assembly/budgeting.rkt"
         "context-assembly/selection.rkt"
         "context-assembly/serialization.rkt"
         (only-in "context-summary.rkt"
                  DEFAULT-CACHE-MAX-ENTRIES
                  make-summary-cache
                  summary-cache?
                  summary-cache-lookup
                  summary-cache-store!
                  summary-cache-count
                  summary-cache-max-entries
                  catalog-entry
                  catalog-entry?
                  catalog-entry-id
                  catalog-entry-role
                  catalog-entry-summary
                  generate-catalog
                  collapse-consecutive-tools
                  message->catalog-entry
                  tool-group->catalog-entry
                  context-summary
                  context-summary?
                  context-summary-from-id
                  context-summary-to-id
                  context-summary-text
                  context-summary-entry-count
                  context-summary-prompt
                  generate-context-summary
                  simple-summary-text
                  simple-summary-count
                  extract-message-text
                  truncate-string))

;; Explicit re-exports from sub-modules (S1-F4)
;; Struct: context-assembly-config
(provide (contract-out [context-assembly-config any/c]
                       [context-assembly-config? (-> any/c boolean?)]
                       [context-assembly-config-recent-tokens any/c]
                       [context-assembly-config-max-catalog-entries any/c]
                       [context-assembly-config-max-catalog-tokens any/c]
                       [context-assembly-config-summary-window any/c]
                       [make-context-assembly-config any/c]
                       ;; Struct: context-result
                       [context-result any/c]
                       [context-result? (-> any/c boolean?)]
                       [context-result-messages any/c]
                       [context-result-total-tokens any/c]
                       [context-result-pinned-count any/c]
                       [context-result-recent-count any/c]
                       [context-result-excluded-count any/c]
                       [context-result-over-budget? (-> any/c boolean?)]
                       [context-result-catalog any/c]
                       [context-result-summary any/c]
                       ;; Context builders
                       [build-assembled-context any/c]
                       [build-assembled-context/raw any/c]
                       [build-assembled-context/v2 any/c]
                       [build-session-context any/c]
                       ;; Struct: context-assembly-call-options
                       [context-assembly-call-options any/c]
                       [context-assembly-call-options? (-> any/c boolean?)]
                       [context-assembly-call-options-cache any/c]
                       [context-assembly-call-options-provider any/c]
                       [context-assembly-call-options-model-name any/c]
                       [context-assembly-call-options-trace-callback any/c]
                       [context-assembly-call-options-working-set any/c]
                       [context-assembly-call-options-generate-summary-proc any/c]
                       [context-assembly-call-options-generate-catalog-proc any/c]
                       [context-assembly-call-options-estimate-text-proc any/c]
                       [make-context-assembly-call-options any/c]
                       ;; Struct: tiered-context
                       [tiered-context any/c]
                       [tiered-context? (-> any/c boolean?)]
                       [tiered-context-tier-a any/c]
                       [tiered-context-tier-b any/c]
                       [tiered-context-tier-c any/c]
                       [build-tiered-context any/c]
                       [tiered-context->message-list any/c]
                       [build-tiered-context-with-hooks any/c]
                       [compute-dynamic-tier-b-count any/c]
                       [summarize-tool-result any/c]
                       ;; Struct: context-assembly-payload
                       [context-assembly-payload any/c]
                       [context-assembly-payload? (-> any/c boolean?)]
                       [context-assembly-payload-tier-a-messages any/c]
                       [context-assembly-payload-tier-b-messages any/c]
                       [context-assembly-payload-tier-c-messages any/c]
                       [context-assembly-payload-max-tokens any/c]
                       [context-assembly-payload-metadata any/c]
                       ;; Payload / context helpers
                       [payload->tiered-context any/c]
                       [tiered-context->payload any/c]
                       [build-session-context/tokens any/c]
                       [entry->context-message any/c]
                       [load-agents-context any/c]
                       [build-system-preamble any/c]
                       [truncate-messages-to-budget any/c]))

;; Re-export from context-summary.rkt
;; Constants
(provide (contract-out [DEFAULT-CACHE-MAX-ENTRIES exact-positive-integer?]
                       ;; Summary cache
                       [make-summary-cache any/c]
                       [summary-cache? (-> any/c boolean?)]
                       [summary-cache-lookup any/c]
                       [summary-cache-store! any/c]
                       [summary-cache-count any/c]
                       [summary-cache-max-entries any/c]
                       ;; Struct: catalog-entry
                       [catalog-entry any/c]
                       [catalog-entry? (-> any/c boolean?)]
                       [catalog-entry-id any/c]
                       [catalog-entry-role any/c]
                       [catalog-entry-summary any/c]
                       [generate-catalog any/c]
                       ;; Struct: context-summary
                       [context-summary any/c]
                       [context-summary? (-> any/c boolean?)]
                       [context-summary-from-id any/c]
                       [context-summary-to-id any/c]
                       [context-summary-text any/c]
                       [context-summary-entry-count any/c]
                       [context-summary-prompt any/c]
                       [generate-context-summary any/c]))
