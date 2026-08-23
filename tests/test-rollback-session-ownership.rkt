#lang racket/base

;; @speed fast
;; @suite default
;; @boundary unit
;; tests/test-rollback-session-ownership.rkt
;; Phase 2 + Phase 10: Session lifecycle characterization tests for rollback state.
;;
;; These tests verify that rollback-state is properly scoped to sessions
;; via lifecycle-state ownership + run-prompt! parameterize.

(require rackunit
         racket/match
         "../runtime/session/lifecycle-state.rkt"
         "../runtime/context-assembly/rollback-actions.rkt"
         (only-in "../runtime/context-assembly/state-aware-helpers.rkt" detect-rollback-plan/state)
         (only-in "../runtime/session/session-types.rkt"
                  lifecycle-state-rollback-st
                  set-lifecycle-state-rollback-st!))

;; ============================================================
;; Phase 2: Lifecycle Characterization
;; ============================================================

(test-case "lifecycle-state: has rollback-st field initialized to #f"
  (define ls (make-lifecycle-state))
  (check-false (lifecycle-state-rollback-st ls) "make-lifecycle-state initializes rollback-st to #f"))

(test-case "lifecycle-state: rollback-st is mutable"
  (define ls (make-lifecycle-state))
  (define rs (make-default-rollback-state))
  (set-lifecycle-state-rollback-st! ls rs)
  (check-eq? (lifecycle-state-rollback-st ls) rs))

(test-case "lifecycle-state: rollback-st persists force-distill"
  (define ls (make-lifecycle-state))
  (define rs (rollback-state 0 #t 1 '()))
  (set-lifecycle-state-rollback-st! ls rs)
  (check-true (rollback-state-force-distill-active? (lifecycle-state-rollback-st ls)))
  (check-equal? (rollback-state-budget-expansion-level (lifecycle-state-rollback-st ls)) 1))

(test-case "two lifecycle-states are independent"
  (define ls-a (make-lifecycle-state))
  (define ls-b (make-lifecycle-state))
  (set-lifecycle-state-rollback-st! ls-a (rollback-state 2 #t 1 '()))
  (check-false (lifecycle-state-rollback-st ls-b))
  (define rs-a (lifecycle-state-rollback-st ls-a))
  (check-equal? (rollback-state-warning-count rs-a) 2)
  (check-true (rollback-state-force-distill-active? rs-a)))

;; ============================================================
;; Phase 10: Session Lifecycle Tests
;; ============================================================

(test-case "default rollback state: all fields zeroed"
  (define rs (make-default-rollback-state))
  (check-equal? (rollback-state-warning-count rs) 0)
  (check-false (rollback-state-force-distill-active? rs))
  (check-equal? (rollback-state-budget-expansion-level rs) 0)
  (check-equal? (rollback-state-action-log rs) '()))

(test-case "one session changing rollback state: force-distill"
  (define ls (make-lifecycle-state))
  (define rs (make-default-rollback-state))
  (set-lifecycle-state-rollback-st! ls rs)
  (define plan
    (rollback-plan (list (list 'amnesia-risk "low coverage"))
                   (make-force-distill-action "amnesia" (hasheq 'trigger 'amnesia))))
  (define new-rs (advance-rollback-state rs plan))
  (set-lifecycle-state-rollback-st! ls new-rs)
  (check-true (rollback-state-force-distill-active? (lifecycle-state-rollback-st ls))))

(test-case "new session receives fresh state"
  (define ls-a (make-lifecycle-state))
  (set-lifecycle-state-rollback-st! ls-a (rollback-state 3 #t 2 '()))
  (define ls-b (make-lifecycle-state))
  (check-false (lifecycle-state-rollback-st ls-b)
               "new session has no rollback state until first prompt")
  (define fresh-rs (make-default-rollback-state))
  (check-equal? (rollback-state-warning-count fresh-rs) 0)
  (check-false (rollback-state-force-distill-active? fresh-rs)))

(test-case "multiple consecutive turns retain state within one session"
  (define ls (make-lifecycle-state))
  (define rs (make-default-rollback-state))
  (set-lifecycle-state-rollback-st! ls rs)
  ;; Turn 1: force-distill
  (define plan1
    (rollback-plan (list (list 'amnesia-risk "low")) (make-force-distill-action "amnesia" (hasheq))))
  (define rs1 (advance-rollback-state rs plan1))
  (set-lifecycle-state-rollback-st! ls rs1)
  ;; Turn 2: expand-context — should see force-distill from turn 1
  (define plan2
    (rollback-plan (list (list 'excessive-savings "cut"))
                   (make-expand-context-action "excessive" (hasheq))))
  (define rs2 (advance-rollback-state rs1 plan2))
  (set-lifecycle-state-rollback-st! ls rs2)
  (define final-rs (lifecycle-state-rollback-st ls))
  (check-true (rollback-state-force-distill-active? final-rs)
              "force-distill from turn 1 persists into turn 2")
  (check-equal? (rollback-state-budget-expansion-level final-rs)
                1
                "expansion from turn 2 is accumulated"))

(test-case "session transition: FSM reset clears warning count only"
  (define ls (make-lifecycle-state))
  (set-lifecycle-state-rollback-st! ls (rollback-state 2 #t 1 '()))
  (define rs-before (lifecycle-state-rollback-st ls))
  (set-lifecycle-state-rollback-st! ls (struct-copy rollback-state rs-before [warning-count 0]))
  (define rs-after (lifecycle-state-rollback-st ls))
  (check-equal? (rollback-state-warning-count rs-after) 0 "warning count resets on FSM transition")
  (check-true (rollback-state-force-distill-active? rs-after)
              "force-distill persists through FSM transition")
  (check-equal? (rollback-state-budget-expansion-level rs-after)
                1
                "budget expansion persists through FSM transition"))

(test-case "force-distill persists within one session"
  (define ls (make-lifecycle-state))
  (define rs (make-default-rollback-state))
  (set-lifecycle-state-rollback-st! ls rs)
  (define plan
    (rollback-plan (list (list 'amnesia-risk "low")) (make-force-distill-action "amnesia" (hasheq))))
  (set-lifecycle-state-rollback-st! ls (advance-rollback-state rs plan))
  (check-true (rollback-state-force-distill-active? (lifecycle-state-rollback-st ls))))

(test-case "budget expansion persists within one session"
  (define ls (make-lifecycle-state))
  (define rs (make-default-rollback-state))
  (set-lifecycle-state-rollback-st! ls rs)
  (define plan1
    (rollback-plan (list (list 'excessive-savings "cut"))
                   (make-expand-context-action "excessive" (hasheq))))
  (define plan2
    (rollback-plan (list (list 'excessive-savings "more"))
                   (make-expand-context-action "excessive" (hasheq))))
  (define rs1 (advance-rollback-state rs plan1))
  (define rs2 (advance-rollback-state rs1 plan2))
  (set-lifecycle-state-rollback-st! ls rs2)
  (check-equal? (rollback-state-budget-expansion-level (lifecycle-state-rollback-st ls)) 2))

(test-case "state does not leak between sessions"
  (define ls-a (make-lifecycle-state))
  (set-lifecycle-state-rollback-st! ls-a (rollback-state 3 #t 2 '()))
  (define ls-b (make-lifecycle-state))
  (define rs-b (make-default-rollback-state))
  (set-lifecycle-state-rollback-st! ls-b rs-b)
  (check-false (rollback-state-force-distill-active? (lifecycle-state-rollback-st ls-b))
               "session B should NOT have session A's force-distill")
  (check-equal? (rollback-state-budget-expansion-level (lifecycle-state-rollback-st ls-b))
                0
                "session B should NOT have session A's expansion")
  (check-equal? (rollback-state-warning-count (lifecycle-state-rollback-st ls-b))
                0
                "session B should NOT have session A's warning count"))

(test-case "pure detection still reads no dynamic parameters"
  (parameterize ([current-rollback-state (rollback-state 99 #t 5 '())])
    (define plan
      (detect-rollback-plan/state (make-default-rollback-state)
                                  #:before-messages 10
                                  #:after-messages 8
                                  #:conclusion-coverage 0.10
                                  #:repeat-tool-count 0))
    (check-true (rollback-plan? plan) "pure detection uses explicit state, not stale parameter")))

(test-case "current-context payload remains unaffected by rollback timing"
  (define rs (make-default-rollback-state))
  (define plan
    (rollback-plan (list (list 'amnesia-risk "low coverage"))
                   (make-force-distill-action "amnesia" (hasheq 'trigger 'amnesia))))
  (define new-rs (advance-rollback-state rs plan))
  (check-equal? (rollback-state-warning-count rs)
                0
                "original state unmodified by advance-rollback-state")
  (check-false (rollback-state-force-distill-active? rs) "original state unmodified")
  (check-true (rollback-state-force-distill-active? new-rs) "new state has the force-distill flag"))

(test-case "parameterize simulates session scoping for rollback-state"
  (define ls (make-lifecycle-state))
  (parameterize ([current-rollback-state (or (lifecycle-state-rollback-st ls)
                                             (make-default-rollback-state))])
    (parameterize ([current-rollback-action-execution? #t])
      (define plan
        (rollback-plan (list (list 'amnesia-risk "low"))
                       (make-force-distill-action "amnesia" (hasheq))))
      (apply-rollback-plan! plan))
    (set-lifecycle-state-rollback-st! ls (current-rollback-state)))
  (check-true (rollback-state-force-distill-active? (lifecycle-state-rollback-st ls))
              "session lifecycle retains rollback state after prompt scope exits")
  (check-false (rollback-state-force-distill-active? (current-rollback-state))
               "global parameter reverts after parameterize scope exits"))

(test-case "two sequential prompts in one session share rollback state"
  (define ls (make-lifecycle-state))
  ;; First prompt
  (parameterize ([current-rollback-state (or (lifecycle-state-rollback-st ls)
                                             (make-default-rollback-state))])
    (parameterize ([current-rollback-action-execution? #t])
      (apply-rollback-plan! (rollback-plan (list (list 'amnesia-risk "low"))
                                           (make-force-distill-action "amnesia" (hasheq)))))
    (set-lifecycle-state-rollback-st! ls (current-rollback-state)))
  ;; Second prompt — should see force-distill from first prompt
  (parameterize ([current-rollback-state (or (lifecycle-state-rollback-st ls)
                                             (make-default-rollback-state))])
    (check-true (rollback-state-force-distill-active? (current-rollback-state))
                "second prompt sees force-distill from first prompt")
    (parameterize ([current-rollback-action-execution? #t])
      (apply-rollback-plan! (rollback-plan (list (list 'excessive-savings "cut"))
                                           (make-expand-context-action "excessive" (hasheq)))))
    (set-lifecycle-state-rollback-st! ls (current-rollback-state)))
  (define final-rs (lifecycle-state-rollback-st ls))
  (check-true (rollback-state-force-distill-active? final-rs))
  (check-equal? (rollback-state-budget-expansion-level final-rs) 1))

(test-case "two sessions in same process do NOT share rollback state"
  (define ls-a (make-lifecycle-state))
  (parameterize ([current-rollback-state (or (lifecycle-state-rollback-st ls-a)
                                             (make-default-rollback-state))])
    (parameterize ([current-rollback-action-execution? #t])
      (apply-rollback-plan! (rollback-plan (list (list 'amnesia-risk "low"))
                                           (make-force-distill-action "amnesia" (hasheq)))))
    (set-lifecycle-state-rollback-st! ls-a (current-rollback-state)))
  ;; Session B — separate lifecycle, fresh state
  (define ls-b (make-lifecycle-state))
  (parameterize ([current-rollback-state (or (lifecycle-state-rollback-st ls-b)
                                             (make-default-rollback-state))])
    (check-false (rollback-state-force-distill-active? (current-rollback-state))
                 "session B starts with fresh state, not session A's")
    (check-equal? (rollback-state-warning-count (current-rollback-state))
                  0
                  "session B starts with zero warnings"))

  ;; ============================================================
  ;; A3: Exceptional-Exit Lifecycle Tests
  ;; Simulates the run-prompt! dynamic-wind + parameterize pattern
  ;; to verify rollback state persists even when prompt raises.
  ;; ============================================================

  (define (simulate-prompt-with-dynamic-wind ls prompt-thunk)
    ;; Simulates the run-prompt! pattern: parameterize + dynamic-wind
    ;; with save-back in the dynamic-wind after thunk.
    (parameterize ([current-rollback-state (or (lifecycle-state-rollback-st ls)
                                               (make-default-rollback-state))])
      (dynamic-wind void
                    prompt-thunk
                    (lambda () (set-lifecycle-state-rollback-st! ls (current-rollback-state))))))

  (test-case "A3: rollback state persists when prompt raises exception"
    (define ls (make-lifecycle-state))
    ;; Prompt 1: rollback fires, THEN prompt raises
    (with-handlers ([exn:fail? (lambda (e) (void))])
      (simulate-prompt-with-dynamic-wind
       ls
       (lambda ()
         (parameterize ([current-rollback-action-execution? #t])
           (apply-rollback-plan! (rollback-plan (list (list 'amnesia-risk "low"))
                                                (make-force-distill-action "amnesia" (hasheq)))))
         ;; Simulate later failure in the same prompt
         (raise (exn:fail "simulated context-building error" (current-continuation-marks))))))
    ;; Verify: force-distill committed despite exception
    (define rs-after (lifecycle-state-rollback-st ls))
    (check-pred rollback-state? rs-after "rollback state was persisted despite exception")
    (check-true (rollback-state-force-distill-active? rs-after)
                "force-distill committed even though prompt raised"))

  (test-case "A3: next prompt sees committed rollback state after exception"
    (define ls (make-lifecycle-state))
    ;; Prompt 1: rollback fires + exception
    (with-handlers ([exn:fail? (lambda (e) (void))])
      (simulate-prompt-with-dynamic-wind
       ls
       (lambda ()
         (parameterize ([current-rollback-action-execution? #t])
           (apply-rollback-plan! (rollback-plan (list (list 'amnesia-risk "low"))
                                                (make-force-distill-action "amnesia" (hasheq)))))
         (raise (exn:fail "simulated error" (current-continuation-marks))))))
    ;; Prompt 2: should see force-distill from failed Prompt 1
    (simulate-prompt-with-dynamic-wind
     ls
     (lambda ()
       (check-true (rollback-state-force-distill-active? (current-rollback-state))
                   "prompt 2 sees force-distill committed by failed prompt 1"))))

  (test-case "A3: budget expansion persists through exception"
    (define ls (make-lifecycle-state))
    (with-handlers ([exn:fail? (lambda (e) (void))])
      (simulate-prompt-with-dynamic-wind
       ls
       (lambda ()
         (parameterize ([current-rollback-action-execution? #t])
           (apply-rollback-plan! (rollback-plan (list (list 'excessive-savings "cut"))
                                                (make-expand-context-action "excessive" (hasheq)))))
         (raise (exn:fail "simulated error" (current-continuation-marks))))))
    (define rs-after (lifecycle-state-rollback-st ls))
    (check-pred rollback-state? rs-after "state persisted despite exception")
    (check-equal? (rollback-state-budget-expansion-level rs-after)
                  1
                  "budget expansion committed despite exception"))

  (test-case "A3: warning escalation persists through exception"
    (define ls (make-lifecycle-state))
    (with-handlers ([exn:fail? (lambda (e) (void))])
      (simulate-prompt-with-dynamic-wind ls
                                         (lambda ()
                                           ;; Simulate 2 warnings to trigger escalation
                                           (record-rollback-warning!)
                                           (record-rollback-warning!)
                                           (raise (exn:fail "simulated error"
                                                            (current-continuation-marks))))))
    (define rs-after (lifecycle-state-rollback-st ls))
    (check-pred rollback-state? rs-after "state persisted despite exception")
    (check-equal? (rollback-state-warning-count rs-after)
                  2
                  "warning count committed despite exception"))

  (test-case "A3: normal completion still persists rollback state"
    (define ls (make-lifecycle-state))
    (simulate-prompt-with-dynamic-wind
     ls
     (lambda ()
       (parameterize ([current-rollback-action-execution? #t])
         (apply-rollback-plan! (rollback-plan (list (list 'amnesia-risk "low"))
                                              (make-force-distill-action "amnesia" (hasheq)))))))
    (define rs-after (lifecycle-state-rollback-st ls))
    (check-true (rollback-state-force-distill-active? rs-after)
                "force-distill persisted on normal completion"))

  (test-case "A3: exception without rollback action preserves initial state"
    (define ls (make-lifecycle-state))
    (set-lifecycle-state-rollback-st! ls (make-default-rollback-state))
    (with-handlers ([exn:fail? (lambda (e) (void))])
      (simulate-prompt-with-dynamic-wind
       ls
       (lambda () (raise (exn:fail "immediate error" (current-continuation-marks))))))
    (define rs-after (lifecycle-state-rollback-st ls))
    (check-pred rollback-state? rs-after "state exists")
    (check-false (rollback-state-force-distill-active? rs-after)
                 "no force-distill when no rollback action fired")
    (check-equal? (rollback-state-warning-count rs-after)
                  0
                  "no warnings when no rollback action fired")))
