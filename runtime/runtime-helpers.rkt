#lang racket/base

;; runtime/runtime-helpers.rkt — shared runtime helper functions
;; STABILITY: internal
;;
;; QUAL-01 (v0.22.0): Extracted from iteration.rkt and tool-coordinator.rkt
;; to eliminate duplication. Both files had identical copies of
;; emit-session-event! and maybe-dispatch-hooks.
;;
;; v0.45.12 M2: emit-session-event! moved to agent/event-emitter.rkt to fix
;; layer violation (agent core should not import from runtime). Re-exported
;; here for backward compatibility with existing runtime callers.

(require racket/contract
         (only-in "../agent/event-emitter.rkt" emit-session-event!)
         (only-in "../agent/event-bus.rkt" make-event-bus)
         (only-in "layer-adapters.rkt" dispatch-hooks)
         (only-in "../util/hook-types.rkt" hook-result-payload))

(provide emit-session-event!
         make-event-bus
         (contract-out [maybe-dispatch-hooks
                        (->* (any/c any/c any/c) (#:ctx any/c) (values any/c any/c))]))

;; emit-session-event! is now defined in agent/event-emitter.rkt
;; and re-exported here for backward compatibility.
;; Runtime callers can continue importing from runtime-helpers.rkt.

;; Safely dispatch hooks if extension-registry is present.
;; Returns (values amended-payload hook-result) or (values payload #f) if no registry.
(define (maybe-dispatch-hooks ext-reg hook-point payload #:ctx [ctx #f])
  (if ext-reg
      (let ([result (dispatch-hooks hook-point payload ext-reg #:ctx ctx)])
        (values (hook-result-payload result) result))
      (values payload #f)))
