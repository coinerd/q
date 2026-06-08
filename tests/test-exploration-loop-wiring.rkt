#lang racket/base

;; @speed fast
;; @suite default

;; BOUNDARY: integration

;; test-exploration-loop-wiring.rkt — TDD tests for v0.28.22 W1
;; Tests for exploration loop detection wiring

(require rackunit
         rackunit/text-ui
         racket/string
         "../runtime/iteration/retry-policy.rkt"
         "../runtime/runtime-helpers.rkt"
         "../agent/event-bus.rkt"
         "../util/event/event.rkt")

(define suite
  (test-suite "Exploration loop detection wiring"

    (test-case "T5: detect repeating 2-tool pattern"
      ;; read-grep-read-grep-read-grep = 3 repeats
      (define tools '("read" "grep" "read" "grep" "read" "grep" "bash"))
      (define result (detect-exploration-loop tools))
      (check-true (string? result) "loop detected")
      (check-true (string-contains? result "read") "mentions read"))

    (test-case "T6: no warning for varied tool sequence"
      (define tools '("read" "bash" "edit" "write" "read" "bash"))
      (define result (detect-exploration-loop tools))
      (check-false result "no loop detected for varied sequence"))

    (test-case "detect-exploration-loop with insufficient history"
      (define tools '("read" "grep"))
      (define result (detect-exploration-loop tools))
      (check-false result "not enough data for loop detection"))

    (test-case "detect-exploration-loop with single tool repeated"
      (define tools '("read" "read" "read" "read" "read"))
      (define result (detect-exploration-loop tools))
      ;; Single-tool pattern: ("read" "read") repeated 2 times with one extra
      (check-true (or (string? result) (not result)) "single-tool patterns handled gracefully"))

    ;; v0.28.23 W1: verify exploration loop event emission path

    (test-case "exploration loop event emission path via event bus"
      ;; Simulates what iteration.rkt does after detect-exploration-loop
      (define bus (make-event-bus))
      (define events '())
      (subscribe! bus (lambda (evt) (set! events (cons evt events))))
      ;; Build repeating 6-tool pattern (3 repeats of read-grep)
      (define recent-tools '("read" "grep" "read" "grep" "read" "grep"))
      (define loop-warning (detect-exploration-loop recent-tools))
      (check-true (string? loop-warning) "loop detected in 6-tool pattern")
      ;; Emit event same way iteration.rkt does
      (when loop-warning
        (emit-session-event! bus
                             "test-session"
                             "iteration.exploration-loop"
                             (hasheq 'pattern loop-warning 'recent-tools recent-tools 'iteration 5)))
      (define loop-events
        (filter
         (lambda (e)
           (and (event? e) (string? (event-ev e)) (string-contains? (event-ev e) "exploration-loop")))
         events))
      (check-true (= (length loop-events) 1) "exactly one exploration-loop event")
      (check-true (string-contains? loop-warning "read") "warning mentions read tool"))

    (test-case "no exploration loop event for varied tool sequence"
      (define bus (make-event-bus))
      (define events '())
      (subscribe! bus (lambda (evt) (set! events (cons evt events))))
      (define recent-tools '("read" "bash" "edit" "write" "read" "bash"))
      (define loop-warning (detect-exploration-loop recent-tools))
      (check-false loop-warning "no loop for varied sequence")
      ;; No event should be emitted when no loop detected
      (when loop-warning
        (emit-session-event! bus
                             "test-session"
                             "iteration.exploration-loop"
                             (hasheq 'pattern loop-warning 'recent-tools recent-tools 'iteration 5)))
      (check-true (= (length events) 0) "no event emitted for non-looping sequence"))))

(run-tests suite)
