#lang racket/base

;; runtime/runtime-helpers.rkt — shared runtime helper functions
;; STABILITY: internal
;;
;; QUAL-01 (v0.22.0): Extracted from iteration.rkt and tool-coordinator.rkt
;; to eliminate duplication. Both files had identical copies of
;; emit-session-event! and maybe-dispatch-hooks.
;;
;; Dependencies: event-bus, protocol-types, hooks, ids.

(require (only-in "../agent/event-bus.rkt" publish!)
         (only-in "../util/event-contracts.rkt" event-payload-contract)
         (only-in "../util/protocol-types.rkt" make-event)
         (only-in "../util/ids.rkt" now-seconds)
         (only-in "../extensions/hooks.rkt" dispatch-hooks)
         (only-in "../util/hook-types.rkt" hook-result-payload))

(provide emit-session-event!
         maybe-dispatch-hooks)

;; Publish a session-scoped event to the event bus.
(define (emit-session-event! bus sid event-name payload)
  ;; Contract check -- only for events with defined contracts (R2 wiring)
  (define pc (event-payload-contract event-name))
  (when pc
    (unless (pc payload)
      (log-warning "Event payload contract violation: ~a" event-name)))
  (define evt (make-event event-name (now-seconds) sid #f payload))
  (publish! bus evt)
  evt)

;; Safely dispatch hooks if extension-registry is present.
;; Returns (values amended-payload hook-result) or (values payload #f) if no registry.
(define (maybe-dispatch-hooks ext-reg hook-point payload #:ctx [ctx #f])
  (if ext-reg
      (let ([result (dispatch-hooks hook-point payload ext-reg #:ctx ctx)])
        (values (hook-result-payload result) result))
      (values payload #f)))
