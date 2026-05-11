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

;; Re-export everything from sub-modules
;; WARNING: all-from-out leaks submodule additions — keep submodules stable
(provide (all-from-out "context-assembly/budgeting.rkt")
         (all-from-out "context-assembly/selection.rkt")
         (all-from-out "context-assembly/serialization.rkt"))

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
