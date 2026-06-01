#lang racket/base

;; tests/test-event-type-uniqueness.rkt — T1-3: Verify event type strings are unique
;; STABILITY: evolving

(require rackunit
         rackunit/text-ui
         "../agent/event-structs.rkt")

;; ── Collect event type string constants ──
;; *-event-type are exported as string constants (not procedures)

(define all-type-strings
  (list stream-completed-event-type
        stream-turn-completed-event-type
        stream-turn-cancelled-event-type
        turn-start-event-type
        turn-end-event-type
        turn-cancelled-event-type
        tool-call-event-type
        tool-result-event-type
        provider-request-event-type
        provider-response-event-type))

;; ── Test Suite ──

(define suite
  (test-suite "Event Type Uniqueness (T1-3)"

    (test-case "all event type constants are strings"
      (for ([t (in-list all-type-strings)])
        (check-true (string? t) (format "event type should be a string, got: ~a" t))))

    (test-case "no duplicate event type strings among sampled events"
      (define seen (make-hash))
      (define duplicates '())
      (for ([t (in-list all-type-strings)])
        (when (hash-has-key? seen t)
          (set! duplicates (cons t duplicates)))
        (hash-set! seen t #t))
      (check-equal? duplicates '() (format "Duplicate event type strings: ~a" duplicates)))

    (test-case "stream-turn-completed uses stream.turn.completed"
      (check-equal? stream-turn-completed-event-type "stream.turn.completed"))

    (test-case "turn-end keeps canonical turn.completed"
      (check-equal? turn-end-event-type "turn.completed"))

    (test-case "stream-turn-completed differs from turn-end"
      (check-not-equal? stream-turn-completed-event-type turn-end-event-type))

    (test-case "stream-turn-cancelled uses stream.turn.cancelled"
      (check-equal? stream-turn-cancelled-event-type "stream.turn.cancelled"))

    (test-case "stream-turn-cancelled differs from turn-cancelled"
      (check-not-equal? stream-turn-cancelled-event-type turn-cancelled-event-type))))

(run-tests suite)
