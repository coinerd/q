#lang racket/base

;; @speed fast
;; @suite default
;; @boundary unit

;; BOUNDARY: integration

;; test-exploration-loop-counter-wiring.rkt — Integration tests for warning escalation.
;; v0.99.86: Rewritten to use canonical rollback-state API instead of removed
;; current-loop-warning-count parameter.

(require rackunit
         rackunit/text-ui
         (only-in "../runtime/context-assembly/rollback-actions.rkt"
                  current-rollback-state
                  make-default-rollback-state
                  rollback-state
                  rollback-state-force-distill-active?
                  rollback-state-budget-expansion-level
                  rollback-warning-count
                  record-rollback-warning!
                  reset-rollback-warning-count!
                  escalation-threshold
                  warnings->actions
                  rollback-action-type
                  rollback-action?))

(define suite
  (test-suite "Rollback warning count API and escalation wiring"

    (test-case "W2.1: record-rollback-warning! increments by 1"
      (parameterize ([current-rollback-state (make-default-rollback-state)])
        (record-rollback-warning!)
        (check-equal? (rollback-warning-count) 1)))

    (test-case "W2.2: record-rollback-warning! accepts custom amount"
      (parameterize ([current-rollback-state (make-default-rollback-state)])
        (record-rollback-warning! 3)
        (check-equal? (rollback-warning-count) 3)))

    (test-case "W2.3: escalation triggers force-distill at threshold"
      ;; warnings->actions is pure — reads count from rollback-state
      (parameterize ([current-rollback-state (make-default-rollback-state)])
        ;; count=0: first warning → warn-only
        (define actions1 (warnings->actions '("repeat: same tool call")))
        (check-equal? (rollback-action-type (car actions1)) 'warn-only)
        (check-equal? (rollback-warning-count) 0 "pure: does not increment")
        ;; Set count to threshold → escalates
        (record-rollback-warning! escalation-threshold)
        (define actions2 (warnings->actions '("repeat: same tool call")))
        (check-equal? (rollback-action-type (car actions2)) 'force-distill)
        ;; Pure: counter NOT reset by warnings->actions
        (check-equal? (rollback-warning-count) escalation-threshold "pure: does not reset")
        ;; Reset via API
        (reset-rollback-warning-count!)
        (check-equal? (rollback-warning-count) 0 "reset via API")))

    (test-case "W2.4: exploration-loop warning produces force-distill without counter"
      (parameterize ([current-rollback-state (make-default-rollback-state)])
        (define actions
          (warnings->actions '("exploration loop detected: 5 consecutive identical tool calls")))
        (check-equal? (length actions) 1)
        (check-equal? (rollback-action-type (car actions)) 'force-distill)
        ;; exploration-loop goes directly to force-distill, counter unchanged
        (check-equal? (rollback-warning-count) 0)))

    (test-case "W2.5: stuck warning produces expand-context"
      (parameterize ([current-rollback-state (make-default-rollback-state)])
        (define actions (warnings->actions '("stuck: 8 tool calls without recording conclusions")))
        (check-equal? (length actions) 1)
        (check-equal? (rollback-action-type (car actions)) 'expand-context)
        ;; stuck does not increment the repeat counter
        (check-equal? (rollback-warning-count) 0)))

    (test-case "W2.6: reset-rollback-warning-count! preserves force-distill and expansion"
      (parameterize ([current-rollback-state (rollback-state 3 #t 2 '())])
        (reset-rollback-warning-count!)
        (check-equal? (rollback-warning-count) 0)
        (check-true (rollback-state-force-distill-active? (current-rollback-state)))
        (check-equal? (rollback-state-budget-expansion-level (current-rollback-state)) 2)))))

(run-tests suite)
