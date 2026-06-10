#lang racket/base
;; STABILITY: stable

;; interfaces/sdk.rkt — embeddable library surface (facade)
;;
;; v0.22.9 W3: Decomposed into:
;;   sdk-core.rkt   — runtime types, session operations, factories
;;   sdk-compat.rkt — enriched API aliases, tree API, GSD convenience
;; This module re-exports everything from both for backward compatibility.

(require "sdk-core.rkt"
         "sdk-compat.rkt")

;; Explicit re-exports from sdk-core.rkt — do NOT use all-from-out (ADR-0028)
(provide runtime-config?
         runtime-config
         runtime-config-provider
         runtime-config-tool-registry
         runtime-config-extension-registry
         runtime-config-event-bus
         runtime-config-session-dir
         runtime-config-model-name
         runtime-config-max-iterations
         runtime-config-system-instructions
         runtime-config-token-budget-threshold
         runtime-config-resource-loader
         runtime-config-session-manager
         runtime?
         runtime-rt-config
         runtime-rt-session
         runtime-rt-cancellation-token
         cancel-token!
         cancellation-token?
         cancellation-token-cancelled?
         make-runtime
         open-session
         run-prompt!
         make-cancellation-token
         make-in-memory-session-manager
         subscribe-events!
         interrupt!
         fork-session!
         compact-session!
         session-info
         steer!
         follow-up!
         navigate!
         compaction-result?
         navigate-result?
         in-memory-session-manager?
         in-memory-append!
         in-memory-append-entries!
         in-memory-load
         in-memory-list-sessions
         in-memory-fork!
         create-agent-session
         session:thinking-levels
         session:thinking-level?
         session:thinking-level->budget
         session:agent-session-thinking-level
         session:set-thinking-level!
         context-usage?
         context-usage-total-tokens
         context-usage-max-tokens
         context-usage-usage-percent
         context-usage-compaction-threshold
         get-context-usage
         context-usage-near-threshold?

         ;; Explicit re-exports from sdk-compat.rkt — do NOT use all-from-out (ADR-0028)
         q:create-session
         q:session-send
         q:session-subscribe
         q:session-interrupt
         q:session-fork
         q:session-compact
         q:session-info
         q:session-branch
         q:session-navigate
         q:session-tree-info
         gsd-status
         q:plan
         q:go
         q:gsd-status
         q:reset-gsd!
         dispatch-command!)
