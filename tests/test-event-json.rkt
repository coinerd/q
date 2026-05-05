#lang racket

;; tests/test-event-json.rkt — W2-D1: Test scaffold for agent/event-json.rkt
;; v0.29.13: Round-trip smoke tests for typed event JSON serialization.

(require rackunit
         "../agent/event-json.rkt"
         "../agent/event-structs/base.rkt"
         "../agent/event-structs/session-events.rkt"
         "../agent/event-structs/turn-events.rkt")

(test-case "session-start-event round-trip"
  (define evt (make-session-start-event #:session-id "test-sess"
                                        #:timestamp 1000
                                        #:turn-id #f
                                        #:model (hasheq 'model "gpt-4")))
  (define jx (typed-event->jsexpr evt))
  (check-equal? (hash-ref jx 'type) "session-start")
  (check-equal? (hash-ref jx 'sessionId) "test-sess")
  (define restored (jsexpr->typed-event jx))
  (check-true (session-start-event? restored))
  (check-equal? (typed-event-session-id restored) "test-sess"))

(test-case "session-shutdown-event round-trip"
  (define evt (make-session-shutdown-event #:session-id "test-sess"
                                           #:timestamp 2000
                                           #:turn-id "t1"
                                           #:reason "graceful"))
  (define jx (typed-event->jsexpr evt))
  (check-equal? (hash-ref jx 'type) "session-shutdown")
  (define restored (jsexpr->typed-event jx))
  (check-true (session-shutdown-event? restored)))

(test-case "all-known-event-types is callable and returns a list"
  (define types (all-known-event-types))
  (check-true (list? types))
  (check-true (> (length types) 0)))
