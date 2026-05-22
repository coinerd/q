#lang racket/base

;; runtime/layer-adapters.rkt — Stable adapter facade for upward layer imports
;; STABILITY: internal
;;
;; ARCH-01 (layer exception): The runtime layer formally imports from tools/ and
;; extensions/ (upper layers). This module provides a SINGLE import point for
;; those upward dependencies, making the boundary explicit and auditable.
;;
;; Consumers in runtime/ should import from THIS module instead of directly
;; from tools/ or extensions/. This prevents layer violations from spreading
;; and allows the underlying implementations to change without breaking callers.
;;
;; Re-exported from tools/:
;;   - list-tools-jsexpr, merge-tool-lists (tool schema assembly)
;;   - run-tool-batch, scheduler-result, scheduler-result-results (tool execution)
;;   - make-exec-context, make-error-result (execution context)
;;   - tool-result?, tool-registry? (predicates)
;;
;; Re-exported from extensions/:
;;   - dispatch-hooks (context assembly hook dispatch)
;;   - make-extension-ctx (extension context factory)

(require racket/contract
         (only-in "../tools/tool.rkt"
                  list-tools-jsexpr
                  merge-tool-lists
                  tool-result?
                  tool-registry?
                  make-exec-context
                  make-error-result)
         (only-in "../tools/scheduler.rkt" run-tool-batch scheduler-result scheduler-result-results)
         (only-in "../extensions/hooks.rkt" dispatch-hooks)
         (only-in "../extensions/context.rkt" make-extension-ctx))

;; Tool schema assembly
(provide list-tools-jsexpr
         merge-tool-lists
         ;; Tool execution
         run-tool-batch
         scheduler-result
         scheduler-result-results
         ;; Tool predicates
         tool-result?
         tool-registry?
         ;; Execution context
         make-exec-context
         make-error-result
         ;; Extension hooks
         dispatch-hooks
         ;; Extension context factory
         make-extension-ctx)
