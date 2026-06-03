#lang racket/base

;; interfaces/sdk-public.rkt — Stable public SDK API
;; STABILITY: stable
;;
;; Curated subset of q's public API for SDK consumers.
;; Internal modules may export more symbols, but only these
;; are considered stable and documented.
;;
;; SDK consumers should (require "interfaces/sdk-public.rkt") rather than
;; importing from main.rkt or individual internal modules.
;;
;; This module does NOT re-export interface-layer internals (TUI, CLI,
;; JSON-mode, RPC-mode, doctor, sessions) — those are for q's own interfaces.
;;
;; ── CONTRACT ENFORCEMENT ──────────────────────────────────────
;; All callables are contracted via contract-out. Struct predicates
;; and accessors are exported bare (they're already guarded by structs).
;; ───────────────────────────────────────────────────────────────

(require racket/contract)

(require (only-in "sdk.rkt"
                  runtime?
                  runtime
                  runtime-config
                  runtime-config?
                  make-runtime
                  open-session
                  run-prompt!
                  interrupt!
                  fork-session!
                  compact-session!
                  session-info
                  steer!
                  follow-up!
                  navigate!
                  dispatch-command!
                  subscribe-events!
                  create-agent-session
                  ;; Context usage
                  context-usage?
                  context-usage-total-tokens
                  context-usage-max-tokens
                  context-usage-usage-percent
                  get-context-usage
                  ;; In-memory sessions
                  make-in-memory-session-manager
                  in-memory-session-manager?
                  in-memory-append!
                  in-memory-append-entries!
                  in-memory-load
                  in-memory-list-sessions
                  in-memory-fork!
                  ;; Thinking levels
                  session:thinking-levels
                  session:thinking-level?
                  session:agent-session-thinking-level
                  session:set-thinking-level!
                  ;; Enriched SDK aliases
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
                  ;; GSD convenience API
                  q:plan
                  q:go
                  q:gsd-status
                  q:reset-gsd!
                  gsd-status
                  ;; Cancellation tokens
                  make-cancellation-token
                  cancellation-token?
                  cancellation-token-cancelled?
                  cancel-token!
                  ;; Navigation types
                  navigate-result?
                  ;; Compaction types
                  compaction-result?
                  ;; runtime-config accessors
                  runtime-config-provider
                  runtime-config-tool-registry
                  runtime-config-extension-registry
                  runtime-config-event-bus
                  runtime-config-session-dir
                  runtime-config-model-name
                  runtime-config-max-iterations
                  runtime-config-system-instructions)
         (only-in "../llm/provider.rkt" provider? make-mock-provider)
         (only-in "../runtime/provider/provider-factory.rkt" build-provider)
         (only-in "../agent/event-bus.rkt" make-event-bus event-bus? subscribe! publish!)
         (only-in "../util/event/event.rkt" event?)
         (only-in "../util/message/message.rkt" message?)
         (only-in "../extensions/api.rkt"
                  make-extension-registry
                  register-extension!
                  extension-registry?
                  extension?)
         (only-in "../tools/tool.rkt"
                  make-tool-registry
                  register-tool!
                  list-tools
                  tool-registry?
                  tool?))

;; ═══════════════════════════════════════════════════════════
;; Contracted SDK boundary — ALL callables have contracts
;; ═══════════════════════════════════════════════════════════

(provide (contract-out
          ;; Runtime lifecycle
          [make-runtime
           (->* (#:provider provider?)
                (#:session-dir (or/c path-string? path? #f)
                               #:tool-registry (or/c tool-registry? #f)
                               #:extension-registry (or/c extension-registry? #f)
                               #:event-bus (or/c event-bus? #f)
                               #:model-name (or/c string? #f)
                               #:max-iterations exact-positive-integer?
                               #:system-instructions (listof string?)
                               #:token-budget-threshold exact-positive-integer?
                               #:cancellation-token (or/c cancellation-token? #f)
                               #:register-default-tools? boolean?
                               #:auto-load-extensions? boolean?
                               #:project-dir (or/c path-string? path? #f))
                runtime?)]
          [open-session (->* (runtime?) ((or/c string? #f)) runtime?)]
          [run-prompt! (-> runtime? (or/c string? message?) (values runtime? (or/c hash? #f)))]
          [interrupt! (-> runtime? runtime?)]
          [fork-session! (->* (runtime?) ((or/c string? #f)) (or/c runtime? 'no-active-session))]
          [compact-session! (->* (runtime?) (#:persist? boolean?) any)]
          [session-info (-> runtime? (or/c #f hash?))]
          [steer! (-> runtime? string? runtime?)]
          [follow-up! (-> runtime? string? runtime?)]
          [navigate!
           (-> runtime?
               (or/c string? exact-integer?)
               (or/c navigate-result? 'no-active-session 'invalid-target))]
          [dispatch-command! (-> runtime? string? string? (values runtime? (or/c hash? #f)))]
          [subscribe-events!
           (->* (runtime? procedure?) ((or/c procedure? #f)) exact-nonnegative-integer?)]
          [create-agent-session
           (->* (#:provider provider?)
                (#:session-dir (or/c path-string? path? #f)
                               #:model-name (or/c string? #f)
                               #:max-iterations exact-positive-integer?
                               #:system-instructions (listof string?))
                runtime?)]
          ;; GSD convenience API
          [q:plan (-> runtime? string? (values runtime? (or/c hash? #f)))]
          [q:go
           (->* (runtime?) ((or/c exact-nonnegative-integer? #f)) (values runtime? (or/c hash? #f)))]
          [q:gsd-status (-> (or/c 'no-active-session hash?))]
          [q:reset-gsd! (-> void?)]
          ;; Provider construction
          [build-provider (-> (or/c hash? #f) (or/c string? #f) (or/c provider? #f))]
          [make-mock-provider
           (->* ((or/c list? #f)) (#:name string? #:stream-chunks (or/c #f list?)) provider?)]
          ;; Event bus
          [make-event-bus (-> event-bus?)]
          [subscribe!
           (->* (event-bus? procedure?) (#:filter (or/c procedure? #f)) exact-nonnegative-integer?)]
          [publish! (-> event-bus? event? event?)]
          ;; Extension registration
          [make-extension-registry (-> extension-registry?)]
          [register-extension! (-> extension-registry? extension? extension-registry?)]
          ;; Tool registry
          [make-tool-registry (-> tool-registry?)]
          [register-tool! (-> tool-registry? tool? void?)]
          [list-tools (-> tool-registry? (listof tool?))]
          ;; Cancellation tokens
          [make-cancellation-token (-> cancellation-token?)]
          [cancel-token! (-> cancellation-token? void)]
          ;; In-memory session helpers (v0.22.6: moved into contract-out)
          [in-memory-append! (-> in-memory-session-manager? string? (or/c string? hash?) void?)]
          [in-memory-append-entries!
           (-> in-memory-session-manager? string? (listof (or/c hash? string?)) void?)]
          [in-memory-load (-> in-memory-session-manager? string? (listof (or/c hash? string?)))]
          [in-memory-list-sessions (-> in-memory-session-manager? (listof string?))]
          [in-memory-fork!
           (->* (in-memory-session-manager? string? string?) ((or/c #f string?)) string?)]
          ;; Context usage (v0.22.5: moved into contract-out)
          [get-context-usage
           (-> exact-nonnegative-integer? exact-nonnegative-integer? context-usage?)]
          ;; Thinking levels (v0.22.5: moved into contract-out)
          [session:set-thinking-level!
           (-> (or/c runtime? #f) (or/c 'off 'minimal 'low 'medium 'high 'xhigh) void?)]
          ;; GSD status (v0.22.5: moved into contract-out)
          [gsd-status (-> (or/c 'no-active-session hash?))]
          ;; Enriched SDK aliases (v0.22.5: moved into contract-out)
          [q:create-session
           (->* (#:provider provider?)
                (#:session-dir (or/c path-string? path? #f)
                               #:model-name (or/c string? #f)
                               #:max-iterations exact-positive-integer?
                               #:system-instructions (listof string?))
                runtime?)]
          [q:session-send (-> runtime? string? (values runtime? (or/c hash? #f)))]
          [q:session-subscribe
           (->* (runtime? procedure?) ((or/c procedure? #f)) exact-nonnegative-integer?)]
          [q:session-interrupt (-> runtime? runtime?)]
          [q:session-fork (->* (runtime?) ((or/c string? #f)) (or/c runtime? 'no-active-session))]
          [q:session-compact (->* (runtime?) (#:persist? boolean?) any)]
          [q:session-info (-> runtime? (or/c #f hash?))]
          [q:session-branch
           (->* (runtime?) ((or/c string? #f) string?) (or/c 'no-active-session hash?))]
          [q:session-navigate (-> runtime? (or/c string? #f) (or/c 'no-active-session hash?))]
          [q:session-tree-info (-> runtime? (or/c 'no-active-session hash?))])

         ;; ── Non-contracted struct exports (predicates + accessors) ──
         runtime?
         runtime-config
         runtime-config?
         runtime-config-provider
         runtime-config-tool-registry
         runtime-config-extension-registry
         runtime-config-event-bus
         runtime-config-session-dir
         runtime-config-model-name
         runtime-config-max-iterations
         runtime-config-system-instructions
         provider?
         event-bus?
         navigate-result?
         compaction-result?
         context-usage?
         context-usage-total-tokens
         context-usage-max-tokens
         context-usage-usage-percent
         cancellation-token?
         cancellation-token-cancelled?

         ;; Thinking levels (predicates and constants — not callables)
         session:thinking-levels
         session:thinking-level?
         session:agent-session-thinking-level

         ;; In-memory session manager
         make-in-memory-session-manager
         in-memory-session-manager?)
