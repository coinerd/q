#lang racket/base

(require rackunit
         "../util/typed-event-predicates.rkt"
         "../agent/event-structs/turn-events.rkt"
         "../agent/event-structs/hook-events.rkt")

;; Test that predicates work
(define sample-turn-start (turn-start-event "turn.started" 0 "session-1" "1" "gpt-4" "openai"))
(define sample-hook-blocked (model-request-blocked-event "model.request.blocked" 0 "session-1" "1" "some-reason"))

(test-case "turn-start-event? predicate"
  (check-true (turn-start-event? sample-turn-start))
  (check-false (turn-start-event? sample-hook-blocked)))

(test-case "model-request-blocked-event? predicate"
  (check-true (model-request-blocked-event? sample-hook-blocked))
  (check-false (model-request-blocked-event? sample-turn-start)))

(test-case "typed-event? base predicate"
  (check-true (typed-event? sample-turn-start))
  (check-true (typed-event? sample-hook-blocked)))

(printf "test-typed-event-predicates.rkt: all tests passed~n")
