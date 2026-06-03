#lang racket/base
;; STABILITY: evolving

;; q/runtime/context-assembly.rkt — Unified context assembly pipeline
;;
;; Thin facade: re-exports from context-assembly/budgeting, selection, serialization.
;; Also re-exports from context-summary.rkt (sibling module).
;; All struct definitions live in sub-modules (single source of truth).
;;
;; I13 (v0.72.6): Documented as transitional facade. Each symbol originates
;; from a specific sub-module. New code should import directly from:
;;   - context-assembly/budgeting.rkt  — token budget calculations
;;   - context-assembly/selection.rkt  — message selection strategies
;;   - context-assembly/serialization.rkt — serialization helpers
;;   - context-summary.rkt            — summary entities

(require racket/contract
         "../context-assembly/budgeting.rkt"
         "../context-assembly/selection.rkt"
         "../context-assembly/serialization.rkt"
         (only-in "../session-index.rkt" session-index?)
         (only-in "../working-set.rkt" working-set?)
         (only-in "../../llm/provider.rkt" provider?)
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
                  truncate-string)
         (only-in "../../util/message/message.rkt" message?)
         (only-in "../../util/hook-types.rkt" hook-result?))

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
          [context-result-messages (-> context-result? (listof message?))]
          [context-result-total-tokens (-> context-result? exact-nonnegative-integer?)]
          [context-result-pinned-count (-> context-result? exact-nonnegative-integer?)]
          [context-result-recent-count (-> context-result? exact-nonnegative-integer?)]
          [context-result-excluded-count (-> context-result? exact-nonnegative-integer?)]
          [context-result-over-budget? (-> context-result? boolean?)]
          [context-result-catalog (-> context-result? list?)]
          [context-result-summary (-> context-result? (or/c context-summary? #f))]
          ;; Context builders
          [build-assembled-context
           (->* (session-index? context-assembly-config?)
                (#:cache (or/c summary-cache? #f)
                         #:provider (or/c provider? #f)
                         #:model-name (or/c string? #f)
                         #:trace-callback (or/c procedure? #f)
                         #:working-set (or/c working-set? #f)
                         #:generate-summary-proc (or/c procedure? #f)
                         #:generate-catalog-proc (or/c procedure? #f)
                         #:estimate-text-proc (or/c procedure? #f))
                context-result?)]
          [build-assembled-context/raw
           (->* ((listof message?) context-assembly-config? (or/c working-set? #f) #:memo hash?)
                (#:cache (or/c summary-cache? #f)
                         #:provider (or/c provider? #f)
                         #:model-name (or/c string? #f)
                         #:trace (or/c procedure? #f)
                         #:estimate-text-proc (or/c procedure? #f)
                         #:generate-summary-proc (or/c procedure? #f)
                         #:generate-catalog-proc (or/c procedure? #f))
                context-result?)]
          [build-assembled-context/v2
           (-> session-index?
               context-assembly-config?
               context-assembly-call-options?
               context-result?)]
          [build-session-context (-> session-index? (listof message?))]
          ;; Struct: context-assembly-call-options
          [context-assembly-call-options? (-> any/c boolean?)]
          [context-assembly-call-options-cache
           (-> context-assembly-call-options? (or/c summary-cache? #f))]
          [context-assembly-call-options-provider
           (-> context-assembly-call-options? (or/c provider? #f))]
          [context-assembly-call-options-model-name
           (-> context-assembly-call-options? (or/c string? #f))]
          [context-assembly-call-options-trace-callback
           (-> context-assembly-call-options? (or/c procedure? #f))]
          [context-assembly-call-options-working-set
           (-> context-assembly-call-options? (or/c working-set? #f))]
          [context-assembly-call-options-generate-summary-proc
           (-> context-assembly-call-options? (or/c procedure? #f))]
          [context-assembly-call-options-generate-catalog-proc
           (-> context-assembly-call-options? (or/c procedure? #f))]
          [context-assembly-call-options-estimate-text-proc
           (-> context-assembly-call-options? (or/c procedure? #f))]
          [make-context-assembly-call-options
           (->* ()
                (#:cache (or/c summary-cache? #f)
                         #:provider (or/c provider? #f)
                         #:model-name (or/c string? #f)
                         #:trace-callback (or/c procedure? #f)
                         #:working-set (or/c working-set? #f)
                         #:generate-summary-proc (or/c procedure? #f)
                         #:generate-catalog-proc (or/c procedure? #f)
                         #:estimate-text-proc (or/c procedure? #f))
                context-assembly-call-options?)]
          ;; Struct: tiered-context
          [tiered-context? (-> any/c boolean?)]
          [tiered-context-tier-a (-> tiered-context? (listof message?))]
          [tiered-context-tier-b (-> tiered-context? (listof message?))]
          [tiered-context-tier-c (-> tiered-context? (listof message?))]
          [build-tiered-context
           (->* ((listof message?))
                (#:tier-b-count (or/c exact-nonnegative-integer? #f)
                                #:tier-c-count (or/c exact-nonnegative-integer? #f)
                                #:trace (or/c procedure? #f)
                                #:working-set-messages (or/c (listof message?) #f))
                tiered-context?)]
          [tiered-context->message-list (-> tiered-context? (listof message?))]
          [build-tiered-context-with-hooks
           (->* ((listof message?))
                (#:tier-b-count (or/c exact-nonnegative-integer? #f)
                                #:tier-c-count (or/c exact-nonnegative-integer? #f)
                                #:hook-dispatcher (or/c procedure? #f)
                                #:max-tokens exact-nonnegative-integer?
                                #:working-set-messages (or/c (listof message?) #f))
                (values tiered-context? (or/c hook-result? #f)))]
          [compute-dynamic-tier-b-count (-> exact-nonnegative-integer? exact-nonnegative-integer?)]
          [summarize-tool-result (-> message? message?)]
          ;; Struct: context-assembly-payload
          [context-assembly-payload? (-> any/c boolean?)]
          [context-assembly-payload-tier-a-messages (-> context-assembly-payload? (listof message?))]
          [context-assembly-payload-tier-b-messages (-> context-assembly-payload? (listof message?))]
          [context-assembly-payload-tier-c-messages (-> context-assembly-payload? (listof message?))]
          [context-assembly-payload-max-tokens
           (-> context-assembly-payload? exact-nonnegative-integer?)]
          [context-assembly-payload-metadata (-> context-assembly-payload? hash?)]
          ;; Payload / context helpers
          [payload->tiered-context (-> context-assembly-payload? tiered-context?)]
          [tiered-context->payload
           (->* (tiered-context? exact-nonnegative-integer?) (hash?) context-assembly-payload?)]
          [build-session-context/tokens
           (-> session-index?
               #:max-tokens exact-nonnegative-integer?
               (values (listof message?) exact-nonnegative-integer?))]
          [entry->context-message (-> message? (or/c message? #f))]
          [load-agents-context (-> (or/c path? string?) string?)]
          [build-system-preamble (-> (or/c path? string?) string?)]
          [truncate-messages-to-budget
           (-> (listof message?) exact-nonnegative-integer? (listof message?))])
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
                       [summary-cache-lookup (-> summary-cache? string? string? (or/c string? #f))]
                       [summary-cache-store! (-> summary-cache? string? string? string? void?)]
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
                       [context-summary-prompt
                        (->* (list?) (#:previous-summary (or/c string? #f)) string?)]
                       [generate-context-summary
                        (->* ((listof message?) (or/c provider? #f) (or/c string? #f))
                             (#:cache (or/c summary-cache? #f))
                             (or/c context-summary? #f))])
         ;; Struct constructors (direct for match compatibility)
         catalog-entry
         context-summary
         ;; Factory functions (complex signatures)
         make-summary-cache)
