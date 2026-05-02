#lang racket/base

;; Test that util/event.rkt (now #lang typed/racket) exports work from untyped
(require rackunit
         "../util/event.rkt")

(test-case "make-event creates event with positional args"
  (define evt (make-event 'test-event 1000 "sess1" #f (hasheq 'key "val")))
  (check-equal? (event-ev evt) 'test-event)
  (check-equal? (event-version evt) 1)
  (check-equal? (event-session-id evt) "sess1"))

(test-case "make-event with explicit version"
  (define evt (make-event 'test-event 1000 "sess1" "turn1" 'payload 2))
  (check-equal? (event-version evt) 2)
  (check-equal? (event-turn-id evt) "turn1"))

(test-case "event->jsexpr and jsexpr->event round-trip"
  (define evt (make-event 'round-trip 42 "s" "t" 'data))
  (define js (event->jsexpr evt))
  (define evt2 (jsexpr->event js))
  (check-equal? (event-ev evt2) 'round-trip)
  (check-equal? (event-time evt2) 42)
  (check-equal? (event-session-id evt2) "s")
  (check-equal? (event-turn-id evt2) "t"))

(test-case "event-event accessor alias works"
  (define evt (make-event 'alias-test 0 "s" #f 'p))
  (check-equal? (event-event evt) 'alias-test))

(test-case "CURRENT-EVENT-VERSION is defined"
  (check-true (integer? CURRENT-EVENT-VERSION))
  (check-equal? CURRENT-EVENT-VERSION 1))

;; Test that util/hook-types.rkt (now #lang typed/racket) exports work from untyped
(require "../util/hook-types.rkt")

(test-case "hook-pass creates pass result"
  (define r (hook-pass "data"))
  (check-equal? (hook-result-action r) 'pass)
  (check-equal? (hook-result-payload r) "data"))

(test-case "hook-amend creates amend result"
  (define r (hook-amend "new-data"))
  (check-equal? (hook-result-action r) 'amend))

(test-case "hook-block creates block result"
  (define r (hook-block "reason"))
  (check-equal? (hook-result-action r) 'block))

(test-case "validate-hook-result TR boundary"
  (check-true (validate-hook-result 'model-request-pre (hook-pass "x")))
  (check-false (validate-hook-result 'turn-end (hook-block "x"))))

(test-case "hook-schema-version from TR"
  (check-equal? (hook-schema-version) 1))
