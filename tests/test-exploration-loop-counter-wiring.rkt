#lang racket/base

;; @speed fast
;; @suite default

;; BOUNDARY: integration

;; test-exploration-loop-counter-wiring.rkt — Integration tests for v0.96.15 W2 (MF3)
;; Tests increment-loop-warning-count! helper, escalation wiring,
;; and non-overlapping trigger contexts.

(require rackunit
         rackunit/text-ui
         (only-in "../runtime/context-assembly/rollback-actions.rkt"
                  current-loop-warning-count
                  increment-loop-warning-count!
                  escalation-threshold
                  warnings->actions
                  rollback-action-type
                  rollback-action?))

(define suite
  (test-suite "Exploration loop counter wiring (MF3)"

    (test-case "W2.1: increment-loop-warning-count! increments by 1"
      (current-loop-warning-count 0)
      (increment-loop-warning-count!)
      (check-equal? (current-loop-warning-count) 1))

    (test-case "W2.2: increment-loop-warning-count! accepts custom amount"
      (current-loop-warning-count 0)
      (increment-loop-warning-count! 3)
      (check-equal? (current-loop-warning-count) 3))

    (test-case "W2.3: escalation triggers force-distill at threshold"
      ;; First call: counter 0->1, returns warn-only
      (current-loop-warning-count 0)
      (define actions1 (warnings->actions '("repeat: same tool call")))
      (check-equal? (rollback-action-type (car actions1)) 'warn-only)
      (check-equal? (current-loop-warning-count) 1)
      ;; Second call: counter 1 >= threshold(2)... still 1 < 2 so warn-only again
      ;; Actually: at 1, add1 = 2 but check is >= threshold BEFORE increment
      ;; The logic: if (>= counter threshold) -> escalate, else increment
      ;; So we need counter at threshold for escalation
      (current-loop-warning-count escalation-threshold)
      (define actions2 (warnings->actions '("repeat: same tool call")))
      (check-equal? (rollback-action-type (car actions2)) 'force-distill)
      (check-equal? (current-loop-warning-count) 0 "counter reset after escalation"))

    (test-case "W2.4: exploration-loop warning produces force-distill without counter"
      (current-loop-warning-count 0)
      (define actions
        (warnings->actions '("exploration loop detected: 5 consecutive identical tool calls")))
      (check-equal? (length actions) 1)
      (check-equal? (rollback-action-type (car actions)) 'force-distill)
      ;; exploration-loop goes directly to force-distill, does NOT increment counter
      (check-equal? (current-loop-warning-count) 0))

    (test-case "W2.5: stuck warning produces expand-context"
      (current-loop-warning-count 0)
      (define actions (warnings->actions '("stuck: 8 tool calls without recording conclusions")))
      (check-equal? (length actions) 1)
      (check-equal? (rollback-action-type (car actions)) 'expand-context)
      ;; stuck does not increment the repeat counter
      (check-equal? (current-loop-warning-count) 0))))

(run-tests suite)
