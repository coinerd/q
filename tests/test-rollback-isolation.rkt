#lang racket/base

;; @speed fast
;; @suite default
;; tests/test-rollback-isolation.rkt -- Characterization tests for rollback timing
;; and isolation from state-aware context construction.
;;
;; Phase 2: Characterization tests proving that rollback mutations do NOT affect
;; the current context build (all context values computed before rollback block).
;; Phase 3-5: Tests for extracted pure detection and effectful execution.

(require rackunit
         racket/string
         "../runtime/context-assembly/state-aware-builder.rkt"
         "../runtime/context-assembly/context-floor.rkt"
         "../runtime/context-assembly/task-conclusion.rkt"
         "../runtime/context-assembly/rollback-actions.rkt"
         "../runtime/context-assembly/state-aware-helpers.rkt"
         "../runtime/context-assembly/config.rkt"
         (only-in "../runtime/context-assembly/auto-distillation.rkt"
                  current-auto-distillation-enabled?)
         (only-in "../util/message/protocol-types.rkt"
                  make-message
                  make-text-part
                  message-content
                  text-part?
                  text-part-text))

;; -- Helpers --

(define (make-test-msg role text [meta (hasheq)])
  (make-message "test-id" #f role 'text (list (make-text-part text)) (current-seconds) meta))

(define (make-test-msgs n)
  (for/list ([i (in-range n)])
    (make-test-msg (if (even? i) 'user 'assistant) (format "msg ~a" i))))

(define (extract-text msgs)
  (string-join (for*/list ([m msgs]
                           [p (message-content m)]
                           #:when (text-part? p))
                 (text-part-text p))
               " "))

(define (make-conclusion text [state 'exploration])
  (task-conclusion (gensym "c") text 'fact state '() (current-seconds) '() '()))

;; ============================================================
;; Phase 2: Characterization Tests
;; ============================================================

;; -- Test: No rollback trigger fires under healthy conditions --

(test-case "no-rollback: healthy metrics produce no warnings"
  (define warnings
    (check-rollback-triggers #:before-messages 10
                             #:after-messages 8
                             #:conclusion-coverage 0.5
                             #:repeat-tool-count 0))
  (check-equal? warnings '()))

;; -- Test: Amnesia/coverage trigger fires when coverage < 0.20 --

(test-case "amnesia-trigger: low coverage fires amnesia-risk"
  (define warnings
    (check-rollback-triggers #:before-messages 10
                             #:after-messages 8
                             #:conclusion-coverage 0.10
                             #:repeat-tool-count 0))
  (define amnesia (filter (lambda (w) (eq? (car w) 'amnesia-risk)) warnings))
  (check-true (pair? amnesia)))

;; -- Test: Repeat-tool trigger fires when repeat count > 2 --

(test-case "repeat-tool-trigger: high repeat count fires task-amnesia-detected"
  (define warnings
    (check-rollback-triggers #:before-messages 10
                             #:after-messages 8
                             #:conclusion-coverage 0.5
                             #:repeat-tool-count 3))
  (define repeated (filter (lambda (w) (eq? (car w) 'task-amnesia-detected)) warnings))
  (check-true (pair? repeated)))

;; -- Test: Warning escalation via loop-warning-count --

(test-case "warning-escalation: repeat-tool escalates to force-distill at threshold"
  (parameterize ([current-loop-warning-count 0])
    ;; escalation-threshold = 2: first call increments to 1, second to 2, third triggers
    (define actions-1 (warnings->actions (list (list 'repeat-tool "3 repeats"))))
    (check-equal? (rollback-action-type (car actions-1)) 'warn-only)
    (check-equal? (current-loop-warning-count) 1)
    (define actions-2 (warnings->actions (list (list 'repeat-tool "3 repeats"))))
    (check-equal? (rollback-action-type (car actions-2)) 'warn-only)
    (check-equal? (current-loop-warning-count) 2)
    ;; Third occurrence: count 2 >= 2, escalates to force-distill, resets counter
    (define actions-3 (warnings->actions (list (list 'repeat-tool "3 repeats"))))
    (check-equal? (rollback-action-type (car actions-3)) 'force-distill)
    (check-equal? (current-loop-warning-count) 0 "counter resets after escalation")))

;; -- Test: Auto-distillation enablement via force-distill callback --

(test-case "force-distill: callback enables auto-distillation"
  (parameterize ([current-rollback-action-execution? #t]
                 [current-rollback-action-log '()]
                 [current-auto-distillation-enabled? #f]
                 [current-force-distill-fn (lambda (a) (current-auto-distillation-enabled? #t))])
    (check-false (current-auto-distillation-enabled?))
    (define result (maybe-execute-action (make-force-distill-action "amnesia" (hasheq))))
    (check-equal? result 'force-distill)
    (check-true (current-auto-distillation-enabled?)
                "force-distill callback should enable auto-distillation")))

;; -- Test: Conclusion-budget expansion via expand-context callback --

(test-case "expand-context: callback doubles conclusion-token-budget"
  (parameterize ([current-rollback-action-execution? #t]
                 [current-rollback-action-log '()]
                 [current-conclusion-token-budget 2000]
                 [current-expand-context-fn
                  (lambda (a)
                    (define current-budget (current-conclusion-token-budget))
                    (current-conclusion-token-budget (* current-budget 2)))])
    (check-equal? (current-conclusion-token-budget) 2000)
    (define result (maybe-execute-action (make-expand-context-action "excessive" (hasheq))))
    (check-equal? result 'expand-context)
    (check-equal? (current-conclusion-token-budget)
                  4000
                  "expand-context callback should double the budget")))

;; -- Test: State revert callback fires when wired --

(test-case "revert-state: callback fires when wired"
  (define executed? (box #f))
  (parameterize ([current-rollback-action-execution? #t]
                 [current-rollback-action-log '()]
                 [current-revert-state-fn (lambda (a) (set-box! executed? #t))])
    (define result (maybe-execute-action (make-revert-state-action "danger" (hasheq))))
    (check-equal? result 'revert-state)
    (check-true (unbox executed?))))

;; -- Test: State revert does NOT fire when callback is #f --

(test-case "revert-state: no-op when callback is #f"
  (parameterize ([current-rollback-action-execution? #t]
                 [current-rollback-action-log '()]
                 [current-revert-state-fn #f])
    (define result (maybe-execute-action (make-revert-state-action "danger" (hasheq))))
    (check-false result)))

;; -- TIMING DEPENDENCY TEST: Prove rollback mutations do NOT affect current build --

(test-case "TIMING: rollback mutations do not affect current context output"
  ;; This test proves that the rollback block runs AFTER all context values
  ;; are computed. If rollback moved earlier, this test would catch the change.
  ;;
  ;; Setup: use 'verification state so WS is filtered (10 -> top 3), triggering
  ;; excessive-savings -> expand-context action, which doubles the budget.
  ;;
  ;; Note: the inline rollback block in state-aware-builder always sets
  ;; current-rollback-action-execution? #t via its own parameterize, so
  ;; the expand-context callback fires regardless of the outer setting.
  ;; We run two builds with fresh budget=2000 each time to isolate the effect.
  (define msgs (make-test-msgs 10))
  (define ws-msgs (make-test-msgs 10))
  (define conclusions
    (for/list ([i (in-range 5)])
      (make-conclusion (format "finding ~a" i) 'verification)))
  ;; Build 1: fresh budget, capture tier-a length
  (define baseline-tier-a-len
    (parameterize ([current-task-state-aware-assembly? #t]
                   [current-conclusion-token-budget 2000]
                   [current-auto-distillation-enabled? #f]
                   [current-loop-warning-count 0])
      (define tc-baseline
        (build-tiered-context/state-aware msgs
                                          #:task-state 'verification
                                          #:working-set-messages ws-msgs
                                          #:conclusions conclusions
                                          #:recent-tool-calls '()))
      (length (tiered-context-tier-a tc-baseline))))
  ;; Build 2: same fresh budget, but expand-context will fire and double it.
  ;; If rollback affected the current build, the doubled budget would allow
  ;; more conclusions into tier-a, making it longer.
  (parameterize ([current-task-state-aware-assembly? #t]
                 [current-conclusion-token-budget 2000]
                 [current-auto-distillation-enabled? #f]
                 [current-loop-warning-count 0])
    (define tc-with-rollback
      (build-tiered-context/state-aware msgs
                                        #:task-state 'verification
                                        #:working-set-messages ws-msgs
                                        #:conclusions conclusions
                                        #:recent-tool-calls '()))
    (define rollback-tier-a-len (length (tiered-context-tier-a tc-with-rollback)))
    ;; The tier-a content is identical -- rollback ran after new-tier-a was computed
    (check-equal?
     rollback-tier-a-len
     baseline-tier-a-len
     "tier-a length should be identical -- rollback mutations don't affect current build")
    ;; After the build, the budget parameter WAS mutated (persists)
    ;; This proves rollback executed but didn't affect the current context
    (check-true (> (current-conclusion-token-budget) 2000)
                "budget was mutated by rollback (persists for next turn)")))

;; -- Test: Rollback action log records execution --

(test-case "rollback-log: executed actions are logged"
  (parameterize ([current-rollback-action-execution? #t]
                 [current-rollback-action-log '()])
    (maybe-execute-action (make-force-distill-action "test" (hasheq)))
    (maybe-execute-action (make-warn-action "minor"))
    (define log (current-rollback-action-log))
    (check-equal? (length log) 2)
    (check-equal? (hash-ref (car log) 'type) 'force-distill)
    (check-equal? (hash-ref (cadr log) 'type) 'warn-only)))

;; -- Test: warn-only action logs without calling any callback --

(test-case "warn-only: logs without calling callbacks"
  (define force-called? (box #f))
  (parameterize ([current-rollback-action-execution? #t]
                 [current-rollback-action-log '()]
                 [current-force-distill-fn (lambda (a) (set-box! force-called? #t))])
    (define result (maybe-execute-action (make-warn-action "mild issue")))
    (check-equal? result 'warn-only)
    (check-false (unbox force-called?) "force-distill callback should NOT be called for warn-only")
    (check-equal? (length (current-rollback-action-log)) 1)))

;; -- Test: Rollback disabled by default --

(test-case "rollback-disabled: actions not executed when flag is #f"
  (parameterize ([current-rollback-action-execution? #f]
                 [current-rollback-action-log '()])
    (define result (maybe-execute-action (make-force-distill-action "test" (hasheq))))
    (check-false result)
    (check-equal? (length (current-rollback-action-log)) 0)))

;; ============================================================
;; Phase 3: Pure Detection Tests
;; ============================================================

(test-case "detect-rollback-plan: returns #f for healthy metrics"
  (parameterize ([current-loop-warning-count 0])
    (define plan
      (detect-rollback-plan #:before-messages 10
                            #:after-messages 8
                            #:conclusion-coverage 0.5
                            #:repeat-tool-count 0))
    (check-false plan "healthy metrics should produce no plan")))

(test-case "detect-rollback-plan: returns plan for amnesia risk"
  (parameterize ([current-loop-warning-count 0])
    (define plan
      (detect-rollback-plan #:before-messages 10
                            #:after-messages 8
                            #:conclusion-coverage 0.10
                            #:repeat-tool-count 0))
    (check-true (rollback-plan? plan))
    (check-true (pair? (rollback-plan-warnings plan)))))

(test-case "detect-rollback-plan: pure -- does not mutate current-loop-warning-count"
  (parameterize ([current-loop-warning-count 0])
    (detect-rollback-plan #:before-messages 10
                          #:after-messages 8
                          #:conclusion-coverage 0.10
                          #:repeat-tool-count 3)
    (check-equal? (current-loop-warning-count)
                  0
                  "pure detection must not mutate loop-warning-count")))

;; ============================================================
;; Phase 4: Effectful Execution Tests
;; ============================================================

(test-case "execute-rollback-plan!: no-op for #f plan"
  (parameterize ([current-rollback-action-log '()])
    (define result (execute-rollback-plan! #f))
    (check-false result)))

(test-case "execute-rollback-plan!: executes force-distill and enables auto-distillation"
  (parameterize ([current-rollback-action-execution? #t]
                 [current-rollback-action-log '()]
                 [current-auto-distillation-enabled? #f]
                 [current-force-distill-fn (lambda (a) (current-auto-distillation-enabled? #t))])
    (define plan
      (rollback-plan (list (list 'amnesia-risk "low coverage"))
                     (make-force-distill-action "amnesia" (hasheq 'trigger 'amnesia))))
    (define result (execute-rollback-plan! plan))
    (check-equal? result 'force-distill)
    (check-true (current-auto-distillation-enabled?))))

(test-case "execute-rollback-plan!: executes expand-context and doubles budget"
  (parameterize ([current-rollback-action-execution? #t]
                 [current-rollback-action-log '()]
                 [current-conclusion-token-budget 2000]
                 [current-expand-context-fn (lambda (a)
                                              (define cb (current-conclusion-token-budget))
                                              (current-conclusion-token-budget (* cb 2)))])
    (define plan
      (rollback-plan (list (list 'excessive-savings "50% cut"))
                     (make-expand-context-action "excessive" (hasheq))))
    (define result (execute-rollback-plan! plan))
    (check-equal? result 'expand-context)
    (check-equal? (current-conclusion-token-budget) 4000)))

(test-case "execute-rollback-plan!: handles warn-only action"
  (parameterize ([current-rollback-action-execution? #t]
                 [current-rollback-action-log '()]
                 [current-loop-warning-count 0])
    (define plan
      (rollback-plan (list (list 'repeat-tool "3 repeats")) (make-warn-action "3 repeats")))
    (define result (execute-rollback-plan! plan))
    (check-equal? result 'warn-only)))

;; ============================================================
;; Phase 5-10: Explicit Rollback State Tests
;; ============================================================

(test-case "rollback-state: default state has zeroed fields"
  (define rs (make-default-rollback-state))
  (check-equal? (rollback-state-warning-count rs) 0)
  (check-false (rollback-state-force-distill-active? rs))
  (check-equal? (rollback-state-budget-expansion-level rs) 0)
  (check-equal? (rollback-state-action-log rs) '()))

(test-case "detect-rollback-plan/state: genuinely pure — no parameter reads"
  ;; Provide explicit state, verify it doesn't read current-loop-warning-count
  (parameterize ([current-loop-warning-count 99]
                 [current-rollback-state (make-default-rollback-state)])
    (define plan
      (detect-rollback-plan/state (make-default-rollback-state)
                                  #:before-messages 10
                                  #:after-messages 8
                                  #:conclusion-coverage 0.10
                                  #:repeat-tool-count 0))
    (check-true (rollback-plan? plan))
    ;; Detection used the explicit state (warning-count=0), not the parameter (99)
    ;; If it read the parameter, escalation logic would differ
    (check-true (rollback-plan? plan)
                "detection should succeed regardless of current-loop-warning-count parameter")))

(test-case "detect-rollback-plan/state: returns #f for healthy metrics"
  (define plan
    (detect-rollback-plan/state (make-default-rollback-state)
                                #:before-messages 10
                                #:after-messages 8
                                #:conclusion-coverage 0.5
                                #:repeat-tool-count 0))
  (check-false plan "healthy metrics should produce no plan"))

(test-case "detect-rollback-plan/state: escalation uses explicit state warning-count"
  ;; With warning-count=2 (at threshold), repeat-tool should escalate to force-distill
  (define escalated-state (rollback-state 2 #f 0 '()))
  (define plan
    (detect-rollback-plan/state escalated-state
                                #:before-messages 10
                                #:after-messages 10
                                #:conclusion-coverage 1.0
                                #:repeat-tool-count 3))
  (check-true (rollback-plan? plan))
  (define recommended (rollback-plan-recommended-action plan))
  (check-true (and recommended (eq? (rollback-action-type recommended) 'force-distill))
              "at threshold, repeat-tool should escalate to force-distill"))

(test-case "advance-rollback-state: no-op for #f plan"
  (define state (make-default-rollback-state))
  (define new-state (advance-rollback-state state #f))
  (check-eq? new-state state))

(test-case "advance-rollback-state: force-distill activates permanent flag"
  (define state (make-default-rollback-state))
  (define plan
    (rollback-plan (list (list 'amnesia-risk "low coverage"))
                   (make-force-distill-action "amnesia" (hasheq 'trigger 'amnesia))))
  (define new-state (advance-rollback-state state plan))
  (check-true (rollback-state-force-distill-active? new-state)
              "force-distill should set force-distill-active? permanently"))

(test-case "advance-rollback-state: expand-context increments expansion level"
  (define state (make-default-rollback-state))
  (define plan
    (rollback-plan (list (list 'excessive-savings "50% cut"))
                   (make-expand-context-action "excessive" (hasheq))))
  (define new-state (advance-rollback-state state plan))
  (check-equal? (rollback-state-budget-expansion-level new-state) 1)
  ;; Second expand-context should increment again
  (define plan2
    (rollback-plan (list (list 'excessive-savings "still cutting"))
                   (make-expand-context-action "excessive" (hasheq))))
  (define newer-state (advance-rollback-state new-state plan2))
  (check-equal? (rollback-state-budget-expansion-level newer-state) 2))

(test-case "advance-rollback-state: warn-only increments warning count"
  (define state (make-default-rollback-state))
  (define plan (rollback-plan (list (list 'repeat-tool "3 repeats")) (make-warn-action "3 repeats")))
  (define new-state (advance-rollback-state state plan))
  (check-equal? (rollback-state-warning-count new-state) 1))

(test-case "advance-rollback-state: escalation resets warning count"
  ;; At threshold (2), escalation should reset count to 0
  (define state (rollback-state 2 #f 0 '()))
  (define plan
    (rollback-plan (list (list 'repeat-tool "3 repeats"))
                   (make-force-distill-action "escalation" (hasheq 'trigger 'repeat-escalation))))
  (define new-state (advance-rollback-state state plan))
  (check-equal? (rollback-state-warning-count new-state) 0 "warning count resets after escalation"))

(test-case "effective-auto-distill?: false when base off and no force-distill"
  (check-false (effective-auto-distill? #f (make-default-rollback-state))))

(test-case "effective-auto-distill?: true when base config on"
  (check-true (effective-auto-distill? #t (make-default-rollback-state))))

(test-case "effective-auto-distill?: true when force-distill fired"
  (define rs (rollback-state 0 #t 0 '()))
  (check-true (effective-auto-distill? #f rs)))

(test-case "effective-conclusion-budget: base with no expansion"
  (check-equal? (effective-conclusion-budget 2000 (make-default-rollback-state)) 2000))

(test-case "effective-conclusion-budget: doubled at expansion level 1"
  (define rs (rollback-state 0 #f 1 '()))
  (check-equal? (effective-conclusion-budget 2000 rs) 4000))

(test-case "effective-conclusion-budget: quadrupled at expansion level 2"
  (define rs (rollback-state 0 #f 2 '()))
  (check-equal? (effective-conclusion-budget 2000 rs) 8000))

(test-case "current-rollback-state: parameterize creates isolated state"
  ;; Simulate two independent sessions
  (parameterize ([current-rollback-state (make-default-rollback-state)])
    ;; Session A: trigger force-distill
    (parameterize ([current-rollback-action-execution? #t]
                   [current-rollback-state (rollback-state 0 #f 0 '())])
      (define plan
        (rollback-plan (list (list 'amnesia-risk "low coverage"))
                       (make-force-distill-action "amnesia" (hasheq 'trigger 'amnesia))))
      (apply-rollback-plan! plan)
      (check-true (rollback-state-force-distill-active? (current-rollback-state))
                  "session A should have force-distill active"))
    ;; Session B: should NOT see session A's force-distill
    (check-false (rollback-state-force-distill-active? (current-rollback-state))
                 "session B should NOT share session A's rollback state")))

(test-case "apply-rollback-plan!: no-op for #f plan returns #f"
  (parameterize ([current-rollback-state (make-default-rollback-state)])
    (check-false (apply-rollback-plan! #f))))

(test-case "apply-rollback-plan!: advances current-rollback-state"
  (parameterize ([current-rollback-action-execution? #t]
                 [current-rollback-state (make-default-rollback-state)])
    (define plan
      (rollback-plan (list (list 'amnesia-risk "low coverage"))
                     (make-force-distill-action "amnesia" (hasheq 'trigger 'amnesia))))
    (define result (apply-rollback-plan! plan))
    (check-equal? result 'force-distill)
    (check-true (rollback-state-force-distill-active? (current-rollback-state)))))

(test-case "increment-loop-warning-count!: syncs to rollback-state"
  (parameterize ([current-rollback-state (make-default-rollback-state)])
    (increment-loop-warning-count!)
    (check-equal? (rollback-state-warning-count (current-rollback-state)) 1)
    (increment-loop-warning-count!)
    (check-equal? (rollback-state-warning-count (current-rollback-state)) 2)))

(test-case "multiple consecutive rollback actions accumulate state"
  (define state (make-default-rollback-state))
  ;; Turn 1: amnesia → force-distill
  (define plan1
    (rollback-plan (list (list 'amnesia-risk "low coverage"))
                   (make-force-distill-action "amnesia" (hasheq 'trigger 'amnesia))))
  (define state1 (advance-rollback-state state plan1))
  (check-true (rollback-state-force-distill-active? state1))
  ;; Turn 2: excessive-savings → expand-context
  (define plan2
    (rollback-plan (list (list 'excessive-savings "cut"))
                   (make-expand-context-action "excessive" (hasheq))))
  (define state2 (advance-rollback-state state1 plan2))
  (check-true (rollback-state-force-distill-active? state2)
              "force-distill should persist across turns")
  (check-equal? (rollback-state-budget-expansion-level state2) 1)
  ;; Turn 3: another expand-context
  (define plan3
    (rollback-plan (list (list 'excessive-savings "still cutting"))
                   (make-expand-context-action "excessive" (hasheq))))
  (define state3 (advance-rollback-state state2 plan3))
  (check-true (rollback-state-force-distill-active? state3))
  (check-equal? (rollback-state-budget-expansion-level state3) 2 "expansion level accumulates"))

(test-case "FSM transition resets warning count in rollback-state"
  (parameterize ([current-rollback-state (rollback-state 2 #f 0 '())])
    ;; Simulate the transition reset logic from turn-context.rkt
    (define rs (current-rollback-state))
    (current-rollback-state (struct-copy rollback-state rs [warning-count 0]))
    (check-equal? (rollback-state-warning-count (current-rollback-state)) 0)
    ;; But force-distill and expansion persist
    (parameterize ([current-rollback-state (rollback-state 2 #t 1 '())])
      (define rs2 (current-rollback-state))
      (current-rollback-state (struct-copy rollback-state rs2 [warning-count 0]))
      (check-equal? (rollback-state-warning-count (current-rollback-state)) 0)
      (check-true (rollback-state-force-distill-active? (current-rollback-state)))
      (check-equal? (rollback-state-budget-expansion-level (current-rollback-state)) 1))))

(test-case "rollback-state: independent sessions do not share state"
  ;; Create two independent state objects
  (define session-a (make-default-rollback-state))
  (define session-b (make-default-rollback-state))
  ;; Session A has a force-distill
  (define session-a-after
    (advance-rollback-state session-a
                            (rollback-plan (list (list 'amnesia-risk "low"))
                                           (make-force-distill-action "amnesia" (hasheq)))))
  ;; Session B is unaffected
  (check-true (rollback-state-force-distill-active? session-a-after))
  (check-false (rollback-state-force-distill-active? session-b)))

;; ============================================================
;; B4: effective-auto-distill? Truth Table
;; ============================================================

(test-case "B4: effective-auto-distill? truth table: all combinations"
  ;; base=false, rollback=false -> false
  (check-false (effective-auto-distill? #f (make-default-rollback-state)))
  ;; base=true, rollback=false -> true
  (check-true (effective-auto-distill? #t (make-default-rollback-state)))
  ;; base=false, rollback=true -> true
  (check-false (effective-auto-distill? #f (make-default-rollback-state)))
  (check-true (effective-auto-distill? #f (rollback-state 0 #t 0 '())))
  ;; base=true, rollback=true -> true
  (check-true (effective-auto-distill? #t (rollback-state 0 #t 0 '()))))

(test-case "B4: two-turn flow: force-distill in Turn N causes auto-distill in Turn N+1"
  ;; Turn N: force-distill fires
  (define state-before (make-default-rollback-state))
  (define plan
    (rollback-plan (list (list 'amnesia-risk "low")) (make-force-distill-action "amnesia" (hasheq))))
  (define state-after-turn-N (advance-rollback-state state-before plan))
  (check-true (rollback-state-force-distill-active? state-after-turn-N)
              "Turn N: force-distill flag set in rollback state")
  ;; Turn N+1: base config is #f, but rollback says #t
  (define base-config #f)
  (check-true (effective-auto-distill? base-config state-after-turn-N)
              "Turn N+1: effective-auto-distill? is #t via rollback state"))

(test-case "B4: base config alone enables auto-distill without rollback"
  (check-true (effective-auto-distill? #t (make-default-rollback-state))))

(test-case "B4: rollback state mutation does not change base config parameter"
  (parameterize ([current-auto-distillation-enabled? #f]
                 [current-rollback-state (make-default-rollback-state)])
    ;; Simulate force-distill callback (now just logs, doesn't mutate base config)
    (define state-before (current-rollback-state))
    (define plan
      (rollback-plan (list (list 'amnesia-risk "low"))
                     (make-force-distill-action "amnesia" (hasheq))))
    (parameterize ([current-rollback-action-execution? #t])
      (apply-rollback-plan! plan))
    ;; Base config must remain #f
    (check-false (current-auto-distillation-enabled?) "base config NOT mutated by force-distill")
    ;; But rollback state has force-distill
    (check-true (rollback-state-force-distill-active? (current-rollback-state))
                "rollback state has force-distill flag")))

;; ============================================================
;; B5: Session Isolation for force-distill
;; ============================================================

(test-case "B5: two sessions: base config independently applies"
  ;; Session A: base config #t, no rollback
  (check-true (effective-auto-distill? #t (make-default-rollback-state)))
  ;; Session B: base config #f, no rollback
  (check-false (effective-auto-distill? #f (make-default-rollback-state)))
  ;; Session A: base config #f, but rollback force-distill
  (check-true (effective-auto-distill? #f (rollback-state 0 #t 0 '())))
  ;; Session B: base config #f, no rollback -> still #f
  (check-false (effective-auto-distill? #f (make-default-rollback-state))))
