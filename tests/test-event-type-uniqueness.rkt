#lang racket/base

;; tests/test-event-type-uniqueness.rkt — T1-3: Verify event type strings are unique
;; STABILITY: evolving
;; Note: Full uniqueness testing requires all-event-type-strings (added in W2).
;; W0 scaffolding compiles and verifies basic event struct availability.

(require rackunit
         rackunit/text-ui
         "../agent/event-structs/stream-events.rkt"
         "../agent/event-structs/turn-events.rkt"
         (only-in "../util/event-macro.rkt" define-typed-event))

;; ── Test Suite ──

(define suite
  (test-suite "Event Type Uniqueness (T1-3)"

    ;; Test 1: Basic event struct availability
    (test-case "stream-turn-completed-event is defined"
      (check-true (procedure? stream-turn-completed-event?)))

    (test-case "turn-end-event is defined"
      (check-true (procedure? turn-end-event?)))

    ;; Test 2: Will be expanded in W2 when all-event-type-strings is available
    (test-case "stream-turn-completed uses distinct type (W2 placeholder)"
      ;; W2 will verify: stream-turn-completed-event type ≠ turn-end-event type
      (check-true #t))

    (test-case "turn-end-event keeps canonical turn.completed (W2 placeholder)"
      ;; W2 will verify: turn-end-event type = "turn.completed"
      (check-true #t))))

(run-tests suite)
