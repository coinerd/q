#lang racket/base

;; tests/test-goal-runner.rkt — Runner tests with mock provider + simulated loop

(require rackunit
         racket/format
         racket/list
         "../llm/model.rkt"
         "../llm/provider.rkt"
         (only-in "../runtime/goal-state.rkt"
                  goal-state?
                  goal-state-status
                  goal-state-turns-used
                  goal-state-last-evaluation
                  goal-state-checks
                  make-goal-state
                  make-evaluation-result
                  evaluation-result?
                  evaluation-result-reason
                  evaluation-result-achieved?)
         (except-in "../runtime/goal-state.rkt" NO-PROGRESS-THRESHOLD)
         "../runtime/goal-evaluator.rkt"
         "../runtime/goal-runner.rkt"
         "../runtime/goal-evidence.rkt")

;; ============================================================
;; Mock evaluator provider — returns JSON evaluation responses
;; ============================================================

(define (make-eval-provider responses)
  (define idx (box 0))
  (make-provider (lambda () "test-eval")
                 (lambda () (hash 'streaming #f 'token-counting #t))
                 (lambda (req)
                   (define resp
                     (if (< (unbox idx) (length responses))
                         (list-ref responses (unbox idx))
                         (last responses)))
                   (set-box! idx (add1 (unbox idx)))
                   resp)
                 (lambda (req)
                   (define resp
                     (if (< (unbox idx) (length responses))
                         (list-ref responses (unbox idx))
                         (last responses)))
                   (set-box! idx (add1 (unbox idx)))
                   resp)))

(define (eval-achieved-response [reason "goal met"])
  (make-model-response
   (list (hasheq 'type "text" 'text (format "{\"ok\": true, \"reason\": \"~a\"}" reason)))
   (hasheq 'total_tokens 30)
   "mock-eval"
   'stop))

(define (eval-failed-response [reason "not yet"])
  (make-model-response
   (list (hasheq 'type "text" 'text (format "{\"ok\": false, \"reason\": \"~a\"}" reason)))
   (hasheq 'total_tokens 30)
   "mock-eval"
   'stop))

;; ============================================================
;; build-continuation-prompt tests
;; ============================================================

(check-equal? (build-continuation-prompt "tests pass" #f)
              "The goal is not yet achieved. Reason: unknown. Continue working toward: tests pass")

(check-equal?
 (build-continuation-prompt "tests pass"
                            (make-evaluation-result #:achieved? #f #:reason "still 2 failures"))
 "The goal is not yet achieved. Reason: still 2 failures. Continue working toward: tests pass")

;; ============================================================
;; goal-run-simulated! — achieve in 1 turn
;; ============================================================

(let ()
  (define eval-prov (make-eval-provider (list (eval-achieved-response))))
  ;; Simulated turn response: transcript with evidence
  (define turn-responses
    (list (hash 'messages (list (hasheq 'role "assistant" 'content "All tests pass now.")))))
  (define result
    (goal-run-simulated! "make tests pass" eval-prov "mock-eval" turn-responses #:max-turns 4))
  (check-equal? (goal-state-status result) 'achieved "1-turn goal achieved")
  (check-equal? (goal-state-turns-used result) 1))

;; ============================================================
;; goal-run-simulated! — achieve in 2 turns
;; ============================================================

(let ()
  (define eval-prov
    (make-eval-provider (list (eval-failed-response "still failing")
                              (eval-achieved-response "all green"))))
  (define turn-responses
    (list (hash 'messages (list (hasheq 'role "assistant" 'content "Attempt 1")))
          (hash 'messages (list (hasheq 'role "assistant" 'content "Fixed, tests pass")))))
  (define result
    (goal-run-simulated! "tests pass" eval-prov "mock-eval" turn-responses #:max-turns 4))
  (check-equal? (goal-state-status result) 'achieved "2-turn goal achieved")
  (check-equal? (goal-state-turns-used result) 2))

;; ============================================================
;; goal-run-simulated! — max turns reached
;; ============================================================

(let ()
  (define eval-prov
    (make-eval-provider (list (eval-failed-response "nope")
                              (eval-failed-response "still no")
                              (eval-failed-response "nope again"))))
  (define turn-responses (list (hash 'messages '()) (hash 'messages '()) (hash 'messages '())))
  (define result
    (goal-run-simulated! "impossible goal" eval-prov "mock-eval" turn-responses #:max-turns 3))
  (check-equal? (goal-state-status result) 'failed "max-turns → failed")
  (check-equal? (goal-state-turns-used result) 3))

;; ============================================================
;; goal-run-simulated! — events emitted
;; ============================================================

(let ()
  (define eval-prov (make-eval-provider (list (eval-achieved-response))))
  (define turn-responses (list (hash 'messages (list (hasheq 'role "assistant" 'content "Done")))))
  (define events '())
  (define result
    (goal-run! "test goal"
               eval-prov
               "mock-eval"
               (lambda (prompt)
                 (values #f (hash 'messages (list (hasheq 'role "assistant" 'content "Done")))))
               #:max-turns 2
               #:on-event (lambda (type data) (set! events (cons (cons type data) events)))))
  (check-equal? (goal-state-status result) 'achieved)
  (check-not-false (assoc 'goal-started events) "goal-started emitted")
  (check-not-false (assoc 'goal-turn-started events) "goal-turn-started emitted")
  (check-not-false (assoc 'goal-evaluated events) "goal-evaluated emitted"))

;; ============================================================
;; goal-loop-step — single step returns updated state
;; ============================================================

(let ()
  (define eval-prov (make-eval-provider (list (eval-achieved-response))))
  (define initial-st (make-goal-state #:goal-text "test" #:max-turns 4))
  (define result-st
    (goal-loop-step initial-st
                    eval-prov
                    "mock-eval"
                    (lambda (prompt)
                      (values #f (hash 'messages (list (hasheq 'role "assistant" 'content "Done")))))
                    void
                    void))
  (check-equal? (goal-state-turns-used result-st) 1 "step increments turns")
  (check-equal? (goal-state-status result-st) 'achieved "step achieves goal")
  (check-true (evaluation-result? (goal-state-last-evaluation result-st))))

(let ()
  (define eval-prov (make-eval-provider (list (eval-failed-response))))
  (define initial-st (make-goal-state #:goal-text "test" #:max-turns 4))
  (define result-st
    (goal-loop-step initial-st
                    eval-prov
                    "mock-eval"
                    (lambda (prompt) (values #f (hash 'messages '())))
                    void
                    void))
  (check-equal? (goal-state-status result-st) 'active "not achieved → active"))

(displayln "All goal-runner tests passed.")

;; ============================================================
;; No-progress detection: 3 same-reason failures → failed
;; ============================================================

(let ()
  (define eval-prov
    (make-eval-provider (list (eval-failed-response "stuck")
                              (eval-failed-response "stuck")
                              (eval-failed-response "stuck"))))
  (define turn-responses (list (hash 'messages '()) (hash 'messages '()) (hash 'messages '())))
  (define result
    (goal-run-simulated! "impossible goal" eval-prov "mock-eval" turn-responses #:max-turns 10))
  (check-equal? (goal-state-status result) 'failed "no-progress → failed")
  (check-true (<= (goal-state-turns-used result) 3) "stops by turn 3"))

;; ============================================================
;; No-progress NOT triggered: different reasons
;; ============================================================

(let ()
  (define eval-prov
    (make-eval-provider (list (eval-failed-response "first issue")
                              (eval-failed-response "different issue")
                              (eval-failed-response "another issue")
                              (eval-achieved-response))))
  (define turn-responses
    (list (hash 'messages '()) (hash 'messages '()) (hash 'messages '()) (hash 'messages '())))
  (define result (goal-run-simulated! "goal" eval-prov "mock-eval" turn-responses #:max-turns 6))
  (check-equal? (goal-state-status result) 'achieved "different reasons → continues"))

;; ============================================================
;; collect-evaluations gathers from goal-state
;; ============================================================

(let ()
  (define eval1 (make-evaluation-result #:achieved? #f #:reason "nope"))
  (define eval2 (make-evaluation-result #:achieved? #f #:reason "still no"))
  (define gs (make-goal-state #:goal-text "test" #:max-turns 5))
  ;; No evaluations yet
  (check-equal? (collect-evaluations gs) '())
  ;; With last-evaluation
  (define gs2 (struct-copy goal-state gs [last-evaluation eval1]))
  (check-equal? (length (collect-evaluations gs2)) 1))

;; ============================================================

;; ============================================================
;; Agent evaluator mode dispatch (W1)
;; ============================================================

(let ()
  ;; Build an evaluator provider that returns achieved JSON
  (define eval-prov (make-eval-provider (list (eval-achieved-response))))
  (define turn-responses
    (list (hash 'messages (list (hasheq 'role "assistant" 'content "Bug fixed.")))))
  (define result
    (goal-run-simulated! "fix the bug"
                         eval-prov
                         "mock-eval"
                         turn-responses
                         #:max-turns 1
                         #:evaluator-mode 'agent))
  (check-true (goal-state? result))
  (check-equal? (goal-state-status result) 'achieved))
