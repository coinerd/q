#lang typed/racket

;; runtime/iteration/loop-state.rkt — loop state structs
;;
;; v0.29.5 W2: Removed parameter indirection and lazy-require.
;; v0.29.16 W0: Introduced loop-infra and loop-counters structs.
;; v0.30.2 W0: Migrated to Typed Racket (TR beachhead).
;; v0.33.0 W3: Removed dead DI resolve functions (RA-06).
;;
;; TR BOUNDARY:
;; This is a #lang typed/racket module. Untyped consumers receive
;; auto-generated contracts from TR boundary system.
;; Opaque types (EventBus, ToolRegistry, ExtRegistry, CancellationToken)
;; are defined via #:opaque to avoid any-wrap/c issues with TR boundaries.

(provide (struct-out loop-infra)
         (struct-out loop-counters)
         make-initial-counters)

;; ── Opaque types for untyped struct values ────────────────────
;; These avoid TR's any-wrap/c failure with opaque structs.

(require/typed "../../agent/event-bus.rkt" [#:opaque EventBus event-bus?])

(require/typed "../../tools/tool.rkt" [#:opaque ToolRegistry tool-registry?])

(require/typed "../../util/contracts.rkt" [#:opaque ExtRegistry extension-registry?])

(require/typed "../../util/cancellation.rkt" [#:opaque CancellationToken cancellation-token?])

;; ── Typed imports from untyped modules ──────────────────────────


;; ── Loop state structs (v0.29.16 W0) ──────────────────────────
;;
;; Captures iteration-loop state that was previously threaded as
;; 26 positional parameters through dispatch-loop-action.
;; See AUDIT-v0.29.15 W4 for the 26-param code smell analysis.

;; Infrastructure that doesn't change across iterations.
(struct loop-infra
        ([ctx : (Listof Any)] [ext-reg : (U ExtRegistry #f)]
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
         [seen-paths : (Listof String)] ; plain list — avoids TR boundary issues with racket/set
         [intent-retry-count : Nonnegative-Integer]
         [consecutive-error-count : Nonnegative-Integer]
         [recent-tool-names : (Listof Any)]
         [explore-count : Nonnegative-Integer]
         [implement-count : Nonnegative-Integer]
         [stall-retry-count : Nonnegative-Integer])
  #:transparent)

;; Constructor helper: create initial counters from defaults.
;; Uses empty list for seen-paths (no set required).
(define (make-initial-counters)
  :
  loop-counters
  (loop-counters 0 0 '() 0 0 '() 0 0 0))
