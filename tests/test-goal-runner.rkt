#lang racket/base

;; tests/test-goal-runner.rkt — Runner tests with mock provider + simulated loop

(require rackunit
         racket/format
         racket/list
         "../llm/model.rkt"
         "../llm/provider.rkt"
         (only-in "../runtime/goal/goal-state.rkt"
                  goal-state?
                  goal-state-status
                  goal-state-turns-used
                  goal-state-last-evaluation
                  goal-state-checks
                  goal-state-evaluations
                  goal-state-goal-text
                  make-goal-state
                  make-goal-check
                  make-evaluation-result
                  evaluation-result?
                  evaluation-result-reason
                  evaluation-result-achieved?
                  evaluation-result-token-cost
                  check-result?
                  check-result-label
                  check-result-exit-code
                  check-result-timed-out?
                  check-result-stdout
                  check-result-stderr)
         (except-in "../runtime/goal/goal-state.rkt" NO-PROGRESS-THRESHOLD)
         "../runtime/goal/goal-evaluator.rkt"
         "../runtime/goal/goal-runner.rkt"
         "../runtime/goal/goal-evidence.rkt")

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

;; ============================================================
;; W0: Event emission + shutdown tests (v0.71.7 → v0.71.8 enhanced)
;; ============================================================

;; goal.achieved event emitted on success + payload key verification (INFO-2)
(let ()
  (define eval-prov (make-eval-provider (list (eval-achieved-response))))
  (define turn-responses (list (hash 'messages (list (hasheq 'role "assistant" 'content "Done.")))))
  (define events '())
  (define (track-event type data)
    (set! events (append events (list (cons type data)))))
  (define result
    (goal-run! "pass tests"
               eval-prov
               "mock"
               (lambda (prompt)
                 (values #f (hash 'messages (list (hasheq 'role "assistant" 'content "Done.")))))
               #:max-turns 2
               #:on-event track-event))
  (check-equal? (goal-state-status result) 'achieved)
  (check-not-false (assoc 'goal-achieved events) "goal-achieved event emitted")
  ;; INFO-2: Verify payload keys for goal-achieved
  (define achieved-data (cdr (assoc 'goal-achieved events)))
  (check-not-false (hash-ref achieved-data 'goal-text #f) "achieved payload has goal-text")
  (check-not-false (hash-ref achieved-data 'turns-used #f) "achieved payload has turns-used")
  (check-not-false (hash-ref achieved-data 'total-token-cost #f)
                   "achieved payload has total-token-cost")
  ;; INFO-2: Verify payload keys for goal-started
  (define started-data (cdr (assoc 'goal-started events)))
  (check-not-false (hash-ref started-data 'goal-text #f) "started payload has goal-text")
  (check-not-false (hash-ref started-data 'max-turns #f) "started payload has max-turns"))

;; shutdown-check cancels + emits goal-failed (INFO-3, INFO-4)
(let ()
  (define eval-prov (make-eval-provider (list (eval-achieved-response))))
  (define events '())
  (define (track-event type data)
    (set! events (append events (list (cons type data)))))
  (define result
    (goal-run! "should cancel"
               eval-prov
               "mock"
               (lambda (prompt)
                 (values #f (hash 'messages (list (hasheq 'role "assistant" 'content "Done.")))))
               #:max-turns 2
               #:on-event track-event
               #:shutdown-check (lambda () #t)))
  (check-equal? (goal-state-status result) 'cancelled)
  (check-not-false (assoc 'goal-failed events) "goal-failed emitted on cancellation (INFO-3)")
  ;; INFO-4: Verify cancelled state in event data
  (define failed-data (cdr (assoc 'goal-failed events)))
  (check-equal? (goal-state-status failed-data)
                'cancelled
                "failed event carries cancelled state (INFO-4)"))

;; evaluations field populated after loop-step
(let ()
  (define eval-prov (make-eval-provider (list (eval-achieved-response))))
  (define turn-responses (list (hash 'messages (list (hasheq 'role "assistant" 'content "Done.")))))
  (define result (goal-run-simulated! "test evals" eval-prov "mock" turn-responses #:max-turns 2))
  (check-true (>= (length (goal-state-evaluations result)) 1) "evaluations field has entries")
  (check-true (evaluation-result? (car (goal-state-evaluations result)))
              "entry is evaluation-result"))

;; WARN-1: goal-check-completed emitted when checks are defined
(let ()
  (define eval-prov (make-eval-provider (list (eval-achieved-response))))
  (define events '())
  (define (track-event type data)
    (set! events (append events (list (cons type data)))))
  (define initial-st
    (make-goal-state #:goal-text "pass tests"
                     #:max-turns 2
                     #:checks (list (make-goal-check #:label "unit" #:command "echo ok"))))
  (define result-st
    (goal-loop-step initial-st
                    eval-prov
                    "mock-eval"
                    (lambda (prompt)
                      (values #f (hash 'messages (list (hasheq 'role "assistant" 'content "Done.")))))
                    track-event
                    void))
  (check-not-false (assoc 'goal-check-completed events)
                   "goal-check-completed event emitted when checks exist (WARN-1)"))

;; ============================================================
;; System instruction injection: prompt includes turn context
;; ============================================================

(let ()
  (define captured-prompt #f)
  (define eval-prov (make-eval-provider (list (eval-achieved-response))))
  (define initial-st (make-goal-state #:goal-text "fix the bug" #:max-turns 4))
  (define result-st
    (goal-loop-step initial-st
                    eval-prov
                    "mock-eval"
                    (lambda (prompt)
                      (set! captured-prompt prompt)
                      (values #f (hash 'messages (list (hasheq 'role "assistant" 'content "Done")))))
                    void
                    void))
  ;; First turn should include system instructions with turn count and goal text
  (check-not-false (regexp-match? #rx"autonomous goal loop" captured-prompt)
                   "system instructions injected")
  (check-not-false (regexp-match? #rx"fix the bug" captured-prompt)
                   "goal text in prompt")
  (check-not-false (regexp-match? #rx"turn 1/4" captured-prompt)
                   "turn context in prompt"))

(displayln "System instruction injection test passed.")
