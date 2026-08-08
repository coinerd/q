#lang racket/base

;; @speed fast
;; @suite default

;; tests/test-rollback-actions.rkt — Rollback action model tests
;; v0.77.10 M2: Updated to verify real execution via injectable callbacks
;; v0.99.87: Removed current-rollback-action-log (dead parameter).
;;           Action logging now lives exclusively in rollback-state.action-log
;;           via apply-rollback-plan!.

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
                  current-rollback-action-execution?
                  current-force-distill-fn
                  current-expand-context-fn
                  current-revert-state-fn))

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

    (test-case "maybe-execute-action skips revert-state when no fn wired (GAP-6)"
      (parameterize ([current-rollback-action-execution? #t]
                     [current-revert-state-fn #f])
        (check-false (maybe-execute-action (make-revert-state-action "danger" (hasheq))))))

    (test-case "maybe-execute-action executes revert-state when fn wired (GAP-6)"
      (define executed? (box #f))
      (parameterize ([current-rollback-action-execution? #t]
                     [current-revert-state-fn (lambda (_action) (set-box! executed? #t))])
        (check-equal? (maybe-execute-action (make-revert-state-action "test" (hasheq))) 'revert-state)
        (check-true (unbox executed?))))

    (test-case "warnings->actions maps amnesia to force-distill"
      (define actions (warnings->actions '("amnesia detected: 80% context lost")))
      (check-equal? (length actions) 1)
      (check-equal? (rollback-action-type (car actions)) 'force-distill))

    (test-case "warnings->actions maps unknown to warn-only"
      (define actions (warnings->actions '("something unusual")))
      (check-equal? (length actions) 1)
      (check-equal? (rollback-action-type (car actions)) 'warn-only))

    (test-case "maybe-execute-action with #f input"
      (check-false (maybe-execute-action #f)))

    ;; v0.77.10 M2: Real execution tests

    (test-case "force-distill calls injectable callback"
      (define called? #f)
      (parameterize ([current-rollback-action-execution? #t]
                     [current-force-distill-fn (lambda (a) (set! called? #t))])
        (define result (maybe-execute-action (make-force-distill-action "amnesia" (hasheq))))
        (check-equal? result 'force-distill)
        (check-true called? "callback was invoked")))

    (test-case "expand-context calls injectable callback"
      (define budget-before 2000)
      (define budget-after budget-before)
      (parameterize ([current-rollback-action-execution? #t]
                     [current-expand-context-fn (lambda (a) (set! budget-after (* budget-before 2)))])
        (define result (maybe-execute-action (make-expand-context-action "excessive" (hasheq))))
        (check-equal? result 'expand-context)
        (check-equal? budget-after 4000 "budget was doubled")))

    (test-case "warn-only returns symbol but does not need callback"
      (parameterize ([current-rollback-action-execution? #t])
        (define result (maybe-execute-action (make-warn-action "mild issue")))
        (check-equal? result 'warn-only)))

    (test-case "revert-state blocked when no fn wired"
      (parameterize ([current-rollback-action-execution? #t])
        (define result (maybe-execute-action (make-revert-state-action "danger" (hasheq))))
        (check-false result)))

    ;; ============================================================
    ;; v0.97.11 W2: GAP-H symbol-based matching tests
    ;; ============================================================

    (test-case "GAP-H: symbol-based matching — amnesia-risk → force-distill"
      (define actions (warnings->actions (list (list 'amnesia-risk "coverage 10%"))))
      (check-equal? (length actions) 1)
      (check-equal? (rollback-action-type (car actions)) 'force-distill))

    (test-case "GAP-H: symbol-based matching — excessive-savings → expand-context"
      (define actions (warnings->actions (list (list 'excessive-savings "50% cut"))))
      (check-equal? (length actions) 1)
      (check-equal? (rollback-action-type (car actions)) 'expand-context))

    (test-case "GAP-H: symbol-based matching — exploration-loop → force-distill"
      (define actions (warnings->actions (list (list 'exploration-loop "repeated reads"))))
      (check-equal? (length actions) 1)
      (check-equal? (rollback-action-type (car actions)) 'force-distill))

    (test-case "GAP-H: symbol-based matching — stuck-detected → expand-context"
      (define actions (warnings->actions (list (list 'stuck-detected "no progress"))))
      (check-equal? (length actions) 1)
      (check-equal? (rollback-action-type (car actions)) 'expand-context))

    (test-case "GAP-H: symbol-based matching — unknown symbol → warn-only"
      (define actions (warnings->actions (list (list 'unknown-trigger "something"))))
      (check-equal? (length actions) 1)
      (check-equal? (rollback-action-type (car actions)) 'warn-only))

    (test-case "GAP-H: string fallback still works for backwards compat"
      (define actions (warnings->actions '("amnesia detected")))
      (check-equal? (length actions) 1)
      (check-equal? (rollback-action-type (car actions)) 'force-distill))))

(run-tests suite)
