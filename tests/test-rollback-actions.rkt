#lang racket/base

(require rackunit
         rackunit/text-ui
         (only-in "../runtime/context-assembly/rollback-actions.rkt"
                  rollback-action
                  rollback-action-type
                  rollback-action-severity
                  rollback-action-reason
                  make-warn-action
                  make-expand-context-action
                  make-force-distill-action
                  make-revert-state-action
                  select-highest-priority-action
                  maybe-execute-action
                  warnings->actions
                  current-rollback-action-execution?))

(define suite
  (test-suite "rollback-actions"
    (test-case "make-warn-action has lowest severity"
      (define a (make-warn-action "test"))
      (check-equal? (rollback-action-type a) 'warn-only)
      (check-equal? (rollback-action-severity a) 0))

    (test-case "make-force-distill-action has severity 2"
      (define a (make-force-distill-action "amnesia" (hasheq)))
      (check-equal? (rollback-action-type a) 'force-distill)
      (check-equal? (rollback-action-severity a) 2))

    (test-case "select-highest-priority-action picks highest severity"
      (define actions
        (list (make-warn-action "w1")
              (make-force-distill-action "d1" (hasheq))
              (make-expand-context-action "e1" (hasheq))))
      (define best (select-highest-priority-action actions))
      (check-not-false best)
      (check-equal? (rollback-action-type best) 'force-distill))

    (test-case "select-highest-priority-action empty list returns #f"
      (check-false (select-highest-priority-action '())))

    (test-case "maybe-execute-action disabled returns #f"
      (parameterize ([current-rollback-action-execution? #f])
        (check-false (maybe-execute-action (make-warn-action "test")))))

    (test-case "maybe-execute-action enabled executes non-revert"
      (parameterize ([current-rollback-action-execution? #t])
        (check-equal? (maybe-execute-action (make-force-distill-action "test" (hasheq)))
                      'force-distill)))

    (test-case "maybe-execute-action never executes revert-state"
      (parameterize ([current-rollback-action-execution? #t])
        (check-false (maybe-execute-action (make-revert-state-action "danger" (hasheq))))))

    (test-case "warnings->actions maps amnesia to force-distill"
      (define actions (warnings->actions '("amnesia detected: 80% context lost")))
      (check-equal? (length actions) 1)
      (check-equal? (rollback-action-type (car actions)) 'force-distill))

    (test-case "warnings->actions maps unknown to warn-only"
      (define actions (warnings->actions '("something unusual")))
      (check-equal? (length actions) 1)
      (check-equal? (rollback-action-type (car actions)) 'warn-only))

    (test-case "maybe-execute-action with #f input"
      (check-false (maybe-execute-action #f)))))

(run-tests suite)
