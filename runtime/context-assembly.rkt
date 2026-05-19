#lang racket/base
;; STABILITY: evolving

;; q/runtime/context-assembly.rkt — Unified context assembly pipeline
;;
;; Thin facade: re-exports from context-assembly/budgeting, selection, serialization.
;; Also re-exports from context-summary.rkt (sibling module).
;; All struct definitions live in sub-modules (single source of truth).

(require "context-assembly/budgeting.rkt"
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
(provide context-assembly-config
         context-assembly-config?
         context-assembly-config-recent-tokens
         context-assembly-config-max-catalog-entries
         context-assembly-config-max-catalog-tokens
         context-assembly-config-summary-window
         make-context-assembly-config
         context-result
         context-result?
         context-result-messages
         context-result-total-tokens
         context-result-pinned-count
         context-result-recent-count
         context-result-excluded-count
         context-result-over-budget?
         context-result-catalog
         context-result-summary
         build-assembled-context
         build-assembled-context/raw
         build-assembled-context/v2
         build-session-context
         context-assembly-call-options
         context-assembly-call-options?
         context-assembly-call-options-cache
         context-assembly-call-options-provider
         context-assembly-call-options-model-name
         context-assembly-call-options-trace-callback
         context-assembly-call-options-working-set
         context-assembly-call-options-generate-summary-proc
         context-assembly-call-options-generate-catalog-proc
         context-assembly-call-options-estimate-text-proc
         make-context-assembly-call-options
         tiered-context
         tiered-context?
         tiered-context-tier-a
         tiered-context-tier-b
         tiered-context-tier-c
         build-tiered-context
         tiered-context->message-list
         build-tiered-context-with-hooks
         compute-dynamic-tier-b-count
         summarize-tool-result
         context-assembly-payload
         context-assembly-payload?
         context-assembly-payload-tier-a-messages
         context-assembly-payload-tier-b-messages
         context-assembly-payload-tier-c-messages
         context-assembly-payload-max-tokens
         context-assembly-payload-metadata
         payload->tiered-context
         tiered-context->payload
         build-session-context/tokens
         entry->context-message
         load-agents-context
         build-system-preamble
         truncate-messages-to-budget)

;; Re-export from context-summary.rkt
(provide DEFAULT-CACHE-MAX-ENTRIES
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
         context-summary
         context-summary?
         context-summary-from-id
         context-summary-to-id
         context-summary-text
         context-summary-entry-count
         context-summary-prompt
         generate-context-summary)
