#lang racket/base

;; @speed fast
;; @suite default

;; BOUNDARY: integration

(require rackunit
         "../util/event/event.rkt"
         "../util/event/event-access.rkt")

;; Test selectors on a sample event
;; make-event: ev time session-id turn-id payload [version]
(define sample-event
  (make-event 'test-event
              1234567890
              "session-1"
              "42" ; turn-id as string per contract
              (hash 'key "value")))

(test-case "event-type-ref returns correct type"
  (check-equal? (event-type-ref sample-event) 'test-event))

(test-case "event-timestamp-ref returns correct timestamp"
  (check-equal? (event-timestamp-ref sample-event) 1234567890))

(test-case "event-session-id-ref returns correct session-id"
  (check-equal? (event-session-id-ref sample-event) "session-1"))

(test-case "event-turn-id-ref returns correct turn-id"
  (check-equal? (event-turn-id-ref sample-event) "42"))

(test-case "event-payload-ref returns correct payload"
  (check-equal? (event-payload-ref sample-event) (hash 'key "value")))

(test-case "event-session-id-ref on event without session-id returns #f"
  (define evt (make-event 'other 0 #f #f (hash)))
  (check-false (event-session-id-ref evt)))

;; Test predicates
(test-case "event? predicate works"
  (check-true (event? sample-event))
  (check-false (event? "not an event")))

(printf "test-event-access.rkt: all tests passed~n")
