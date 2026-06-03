#lang racket/base

;; BOUNDARY: integration

;; tests/test-runtime-helpers.rkt — QUAL-01: runtime helper extraction

(require rackunit
         (only-in "../agent/event-bus.rkt" make-event-bus subscribe!)
         (only-in "../util/message/protocol-types.rkt" event-ev event-payload)
         (only-in "../runtime/runtime-helpers.rkt" emit-session-event! maybe-dispatch-hooks))

(test-case "QUAL-01: emit-session-event! publishes event to bus"
  (define bus (make-event-bus))
  (define received (box #f))
  (subscribe! bus (lambda (evt) (set-box! received evt)))
  (emit-session-event! bus "test-session" "test.event" (hasheq 'key "value"))
  (define r (unbox received))
  (check-not-false r)
  (check-equal? (event-ev r) "test.event")
  (check-equal? (hash-ref (event-payload r) 'key) "value"))

(test-case "QUAL-01: maybe-dispatch-hooks returns payload+false when no registry"
  (define-values (amended hook-res) (maybe-dispatch-hooks #f 'test "payload"))
  (check-equal? amended "payload")
  (check-false hook-res))
