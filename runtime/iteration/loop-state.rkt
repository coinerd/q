#lang typed/racket

;; runtime/iteration/loop-state.rkt — DI resolvers for iteration loop
;;
;; v0.29.5 W2: Removed parameter indirection and lazy-require.
;; DI resolvers now directly reference concrete implementations.
;; Callers pass these explicitly via keyword arguments.
;; v0.30.2 W0: Migrated to Typed Racket (TR beachhead).
;;
;; TR BOUNDARY:
;; This is a #lang typed/racket module. Untyped consumers receive
;; auto-generated contracts from TR boundary system.
;; Opaque types (EventBus, ToolRegistry, ExtRegistry, CancellationToken)
;; are defined via #:opaque to avoid any-wrap/c issues with TR boundaries.

(provide resolve-compact-proc
         resolve-estimate-tokens
         resolve-inject-topic
         (struct-out loop-infra)
         (struct-out loop-counters)
         make-initial-counters)

;; ── Opaque types for untyped struct values ────────────────────
;; These avoid TR's any-wrap/c failure with opaque structs.

(require/typed "../../agent/event-bus.rkt"
               [#:opaque EventBus event-bus?])

(require/typed "../../tools/tool.rkt"
               [#:opaque ToolRegistry tool-registry?])

(require/typed "../../extensions/api.rkt"
               [#:opaque ExtRegistry extension-registry?])

(require/typed "../../util/cancellation.rkt"
               [#:opaque CancellationToken cancellation-token?])

;; ── Typed imports from untyped modules ──────────────────────────

(require/typed "../../runtime/compactor.rkt"
               [compact-history (->* ((Listof Any))
                                     (#:summarize-fn Any
                                      #:provider Any
                                      #:model-name Any
                                      #:previous-summary Any
                                      #:hook-dispatcher Any
                                      #:token-config Any)
                                     Any)])

(require/typed "../../llm/token-budget.rkt"
               [estimate-context-tokens (-> (Listof Any) Nonnegative-Integer)])

(require/typed "../../extensions/message-inject.rkt"
               [injection-event-topic String])

;; Direct DI resolvers — no parameter fallback, no lazy-require.
(define (resolve-compact-proc)
  : (->* ((Listof Any))
        (#:summarize-fn Any #:provider Any #:model-name Any
         #:previous-summary Any #:hook-dispatcher Any #:token-config Any)
        Any)
  compact-history)

(define (resolve-estimate-tokens) : (-> (Listof Any) Nonnegative-Integer)
  estimate-context-tokens)

(define (resolve-inject-topic) : String
  injection-event-topic)

;; ── Loop state structs (v0.29.16 W0) ──────────────────────────
;;
;; Captures iteration-loop state that was previously threaded as
;; 26 positional parameters through dispatch-loop-action.
;; See AUDIT-v0.29.15 W4 for the 26-param code smell analysis.

;; Infrastructure that doesn't change across iterations.
(struct loop-infra
        ([ctx : (Listof Any)]
         [ext-reg : (U ExtRegistry #f)]
         [reg : (U ToolRegistry #f)]
         [bus : EventBus]
         [session-id : String]
         [log-path : Path-String]
         [token : (U CancellationToken #f)])
  #:transparent)

;; Counters and accumulators that evolve across iterations.
(struct loop-counters
        ([iteration : Nonnegative-Integer]
         [consecutive-tool-count : Nonnegative-Integer]
         [seen-paths : Any] ; racket/set — typed as Any for TR boundary
         [intent-retry-count : Nonnegative-Integer]
         [consecutive-error-count : Nonnegative-Integer]
         [recent-tool-names : (Listof Any)]
         [explore-count : Nonnegative-Integer]
         [implement-count : Nonnegative-Integer]
         [stall-retry-count : Nonnegative-Integer])
  #:transparent)

;; Constructor helper: create initial counters from defaults.
;; Uses cast because racket/set is not available as typed/racket/set.
(define (make-initial-counters) : loop-counters
  (loop-counters 0 0 (cast (set) Any) 0 0 '() 0 0 0))
