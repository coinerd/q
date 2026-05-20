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
(provide (contract-out
          ;; Struct: context-assembly-config
          [context-assembly-config? (-> any/c boolean?)]
          [context-assembly-config-recent-tokens
           (-> context-assembly-config? exact-positive-integer?)]
          [context-assembly-config-max-catalog-entries
           (-> context-assembly-config? exact-nonnegative-integer?)]
          [context-assembly-config-max-catalog-tokens
           (-> context-assembly-config? exact-nonnegative-integer?)]
          [context-assembly-config-summary-window
           (-> context-assembly-config? exact-positive-integer?)]
          [make-context-assembly-config
           (->* ()
                (#:recent-tokens exact-positive-integer?
                                 #:max-catalog-entries exact-nonnegative-integer?
                                 #:max-catalog-tokens exact-nonnegative-integer?
                                 #:summary-window exact-positive-integer?)
                context-assembly-config?)]
          ;; Struct: context-result
          [context-result? (-> any/c boolean?)]
          [context-result-messages (-> context-result? list?)]
          [context-result-total-tokens (-> context-result? exact-nonnegative-integer?)]
          [context-result-pinned-count (-> context-result? exact-nonnegative-integer?)]
          [context-result-recent-count (-> context-result? exact-nonnegative-integer?)]
          [context-result-excluded-count (-> context-result? exact-nonnegative-integer?)]
          [context-result-over-budget? (-> context-result? boolean?)]
          [context-result-catalog (-> context-result? list?)]
          [context-result-summary (-> context-result? (or/c any/c #f))]
          ;; Context builders
          [build-assembled-context
           (->* (any/c any/c)
                (#:cache any/c
                         #:provider any/c
                         #:model-name (or/c string? #f)
                         #:trace-callback (or/c procedure? #f)
                         #:working-set any/c
                         #:generate-summary-proc (or/c procedure? #f)
                         #:generate-catalog-proc (or/c procedure? #f)
                         #:estimate-text-proc (or/c procedure? #f))
                any/c)]
          [build-assembled-context/raw
           (->* (list? any/c any/c #:memo any/c)
                (#:cache any/c
                         #:provider any/c
                         #:model-name (or/c string? #f)
                         #:trace (or/c procedure? #f)
                         #:estimate-text-proc (or/c procedure? #f)
                         #:generate-summary-proc (or/c procedure? #f)
                         #:generate-catalog-proc (or/c procedure? #f))
                any/c)]
          [build-assembled-context/v2 (->* (any/c any/c any/c) any/c)]
          [build-session-context (-> any/c any/c)]
          ;; Struct: context-assembly-call-options
          [context-assembly-call-options? (-> any/c boolean?)]
          [context-assembly-call-options-cache (-> context-assembly-call-options? (or/c any/c #f))]
          [context-assembly-call-options-provider (-> context-assembly-call-options? (or/c any/c #f))]
          [context-assembly-call-options-model-name
           (-> context-assembly-call-options? (or/c string? #f))]
          [context-assembly-call-options-trace-callback
           (-> context-assembly-call-options? (or/c procedure? #f))]
          [context-assembly-call-options-working-set
           (-> context-assembly-call-options? (or/c any/c #f))]
          [context-assembly-call-options-generate-summary-proc
           (-> context-assembly-call-options? (or/c procedure? #f))]
          [context-assembly-call-options-generate-catalog-proc
           (-> context-assembly-call-options? (or/c procedure? #f))]
          [context-assembly-call-options-estimate-text-proc
           (-> context-assembly-call-options? (or/c procedure? #f))]
          [make-context-assembly-call-options
           (->* ()
                (#:cache (or/c any/c #f)
                         #:provider (or/c any/c #f)
                         #:model-name (or/c string? #f)
                         #:trace-callback (or/c procedure? #f)
                         #:working-set (or/c any/c #f)
                         #:generate-summary-proc (or/c procedure? #f)
                         #:generate-catalog-proc (or/c procedure? #f)
                         #:estimate-text-proc (or/c procedure? #f))
                context-assembly-call-options?)]
          ;; Struct: tiered-context
          [tiered-context? (-> any/c boolean?)]
          [tiered-context-tier-a (-> tiered-context? list?)]
          [tiered-context-tier-b (-> tiered-context? list?)]
          [tiered-context-tier-c (-> tiered-context? list?)]
          [build-tiered-context
           (->* (list?)
                (#:tier-b-count any/c
                                #:tier-c-count any/c
                                #:trace (or/c procedure? #f)
                                #:working-set-messages (or/c list? #f))
                tiered-context?)]
          [tiered-context->message-list (-> tiered-context? list?)]
          [build-tiered-context-with-hooks
           (->* (list?)
                (#:tier-b-count any/c
                                #:tier-c-count any/c
                                #:hook-dispatcher (or/c procedure? #f)
                                #:max-tokens any/c
                                #:working-set-messages (or/c list? #f))
                (values tiered-context? any/c))]
          [compute-dynamic-tier-b-count (-> exact-nonnegative-integer? exact-nonnegative-integer?)]
          [summarize-tool-result (-> any/c any/c)]
          ;; Struct: context-assembly-payload
          [context-assembly-payload? (-> any/c boolean?)]
          [context-assembly-payload-tier-a-messages (-> context-assembly-payload? list?)]
          [context-assembly-payload-tier-b-messages (-> context-assembly-payload? list?)]
          [context-assembly-payload-tier-c-messages (-> context-assembly-payload? list?)]
          [context-assembly-payload-max-tokens
           (-> context-assembly-payload? exact-nonnegative-integer?)]
          [context-assembly-payload-metadata (-> context-assembly-payload? hash?)]
          ;; Payload / context helpers
          [payload->tiered-context (-> context-assembly-payload? tiered-context?)]
          [tiered-context->payload
           (->* (tiered-context? exact-nonnegative-integer?) (hash?) context-assembly-payload?)]
          [build-session-context/tokens (-> any/c #:max-tokens exact-nonnegative-integer? any/c)]
          [entry->context-message (-> any/c any/c)]
          [load-agents-context (-> (or/c path? string?) list?)]
          [build-system-preamble (-> (or/c path? string?) list?)]
          [truncate-messages-to-budget (-> list? exact-nonnegative-integer? list?)])
         ;; Struct constructors (direct for match compatibility)
         context-assembly-config
         context-result
         context-assembly-call-options
         tiered-context
         context-assembly-payload)

;; Re-export from context-summary.rkt
;; Constants
(provide (contract-out [DEFAULT-CACHE-MAX-ENTRIES exact-positive-integer?]
                       ;; Summary cache
                       [summary-cache? (-> any/c boolean?)]
                       [summary-cache-lookup (-> summary-cache? any/c any/c (or/c string? #f))]
                       [summary-cache-store! (-> summary-cache? any/c any/c string? void?)]
                       [summary-cache-count (-> summary-cache? exact-nonnegative-integer?)]
                       [summary-cache-max-entries (-> summary-cache? exact-positive-integer?)]
                       ;; Catalog entries
                       [catalog-entry? (-> any/c boolean?)]
                       [catalog-entry-id (-> catalog-entry? string?)]
                       [catalog-entry-role (-> catalog-entry? string?)]
                       [catalog-entry-summary (-> catalog-entry? string?)]
                       [generate-catalog
                        (->* (list?)
                             (#:max-entries exact-nonnegative-integer?
                                            #:max-tokens exact-nonnegative-integer?
                                            #:estimate-text (-> string? exact-nonnegative-integer?))
                             (listof catalog-entry?))]
                       ;; Context summary
                       [context-summary? (-> any/c boolean?)]
                       [context-summary-from-id (-> context-summary? string?)]
                       [context-summary-to-id (-> context-summary? string?)]
                       [context-summary-text (-> context-summary? string?)]
                       [context-summary-entry-count (-> context-summary? exact-nonnegative-integer?)]
                       [context-summary-prompt (-> context-summary? string?)]
                       [generate-context-summary
                        (->* (list? (or/c any/c #f) (or/c string? #f))
                             (#:cache (or/c summary-cache? #f))
                             (or/c context-summary? #f))])
         ;; Struct constructors (direct for match compatibility)
         catalog-entry
         context-summary
         ;; Factory functions (complex signatures)
         make-summary-cache)
