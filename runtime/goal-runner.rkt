#lang racket/base

;; q/runtime/goal-runner.rkt — Main goal loop orchestration
;;
;; Orchestrates the autonomous goal loop:
;; 1. User sets goal via /goal command
;; 2. Runner loops: run-prompt! → evaluate → continue/stop
;; 3. Enforces max-turns, shutdown checks, no-progress detection
;; 4. Emits typed events at each phase

(require racket/contract
         racket/match
         racket/format
         racket/string
         racket/list
         (except-in "goal-state.rkt" NO-PROGRESS-THRESHOLD)
         (only-in "goal-state.rkt"
                  NO-PROGRESS-THRESHOLD
                  evaluation-result?
                  evaluation-result-achieved?
                  evaluation-result-reason
                  evaluation-result-token-cost
                  check-result?
                  check-result-label
                  check-result-exit-code
                  check-result-timed-out?
                  check-result-stdout
                  check-result-stderr
                  goal-check?)
         "goal-evaluator.rkt"
         "goal-agent-evaluator.rkt"
         "goal-evidence.rkt"
         "goal-checks.rkt"
         "../llm/provider.rkt")

;; ============================================================
;; Provides
;; ============================================================

(provide goal-run!
         goal-run-simulated!
         goal-loop-step
         build-continuation-prompt
         collect-evaluations
         execute-checks-for-goal)

;; Collect all evaluations from goal-state evaluations field + last-evaluation
(define (collect-evaluations goal-st)
  (define from-field (goal-state-evaluations goal-st))
  (define last-eval (goal-state-last-evaluation goal-st))
  (if last-eval
      (append from-field (list last-eval))
      from-field))

;; Execute all deterministic checks from goal-state and return results.
;; If no checks defined, returns empty list.
(define (execute-checks-for-goal goal-st)
  (define checks (goal-state-checks goal-st))
  (if (and (pair? checks) (goal-check? (car checks)))
      (execute-all-checks checks #:timeout 30)
      '()))

;; ============================================================
;; Main entry point
;; ============================================================

(define/contract (goal-run! goal-text
                            provider
                            evaluator-model
                            run-prompt-fn!
                            #:max-turns [max-turns 8]
                            #:evaluator-mode [evaluator-mode 'transcript]
                            #:on-event [on-event void]
                            #:on-status [on-status void]
                            #:shutdown-check [shutdown-check (lambda () #f)])
  (->* (string? provider? string? procedure?)
       (#:max-turns exact-nonnegative-integer?
                    #:evaluator-mode symbol?
                    #:on-event procedure?
                    #:on-status procedure?
                    #:shutdown-check procedure?)
       goal-state?)
  ;; Initialize goal state
  (define goal-st
    (make-goal-state #:goal-text goal-text
                     #:max-turns max-turns
                     #:evaluator-model evaluator-model
                     #:evaluator-mode evaluator-mode))

  ;; Emit goal.started
  (on-event 'goal-started
            (hasheq 'goal-text goal-text 'max-turns max-turns 'checks (goal-state-checks goal-st)))
  (on-status (format "Goal set: ~a (max ~a turns)" goal-text max-turns))

  ;; Run the loop
  (run-goal-loop goal-st provider evaluator-model run-prompt-fn! on-event on-status shutdown-check))

;; ============================================================
;; Internal loop
;; ============================================================

(define (run-goal-loop goal-st
                       provider
                       evaluator-model
                       run-prompt-fn!
                       on-event
                       on-status
                       shutdown-check)
  ;; Shutdown check — first priority
  (cond
    [(shutdown-check)
     (define final-st
       (struct-copy goal-state
                    goal-st
                    [status 'cancelled]
                    [updated-at (inexact->exact (round (current-inexact-milliseconds)))]))
     (on-event 'goal-failed
               (hasheq 'goal-text
                       (goal-state-goal-text final-st)
                       'reason
                       "cancelled"
                       'turns-used
                       (goal-state-turns-used final-st)))
     (on-status "Goal cancelled by user")
     final-st]

    ;; Max turns reached
    [(>= (goal-state-turns-used goal-st) (goal-state-max-turns goal-st))
     (define final-st
       (struct-copy goal-state
                    goal-st
                    [status 'failed]
                    [updated-at (inexact->exact (round (current-inexact-milliseconds)))]))
     (on-event 'goal-failed
               (hasheq 'goal-text
                       (goal-state-goal-text final-st)
                       'reason
                       "max turns reached"
                       'turns-used
                       (goal-state-turns-used final-st)))
     (on-status (format "Goal failed: max turns (~a) reached" (goal-state-max-turns goal-st)))
     final-st]

    [(detect-no-progress (collect-evaluations goal-st))
     (define final-st
       (struct-copy goal-state
                    goal-st
                    [status 'failed]
                    [updated-at (inexact->exact (round (current-inexact-milliseconds)))]))
     (on-event 'goal-failed
               (hasheq 'goal-text
                       (goal-state-goal-text final-st)
                       'reason
                       "no progress"
                       'turns-used
                       (goal-state-turns-used final-st)))
     (on-status "Goal failed: no progress — 3 consecutive same-reason evaluations")
     final-st]

    [else
     ;; Run one step
     (define updated-st
       (goal-loop-step goal-st provider evaluator-model run-prompt-fn! on-event on-status))
     (cond
       [(eq? (goal-state-status updated-st) 'achieved)
        (on-event 'goal-achieved
                  (hasheq 'goal-text
                          (goal-state-goal-text updated-st)
                          'turns-used
                          (goal-state-turns-used updated-st)
                          'total-token-cost
                          (goal-state-total-token-cost updated-st)))
        (on-status (format "Goal achieved in ~a turns!" (goal-state-turns-used updated-st)))
        updated-st]
       [(eq? (goal-state-status updated-st) 'failed)
        (on-status (format "Goal failed: ~a" (goal-state-last-evaluation-reason updated-st)))
        updated-st]
       [else
        ;; Active — recurse with updated state
        (run-goal-loop updated-st
                       provider
                       evaluator-model
                       run-prompt-fn!
                       on-event
                       on-status
                       shutdown-check)])]))

;; Extract reason from last evaluation safely
(define (goal-state-last-evaluation-reason gs)
  (define le (goal-state-last-evaluation gs))
  (if le
      (evaluation-result-reason le)
      "unknown"))

;; Sum token costs from all evaluations
(define (goal-state-total-token-cost gs)
  (for/sum ([er (in-list (goal-state-evaluations gs))]) (evaluation-result-token-cost er)))

;; ============================================================
;; Single step
;; ============================================================

(define/contract (goal-loop-step goal-st provider evaluator-model run-prompt-fn! on-event on-status)
  (-> goal-state? provider? string? procedure? procedure? procedure? goal-state?)
  (define turns (add1 (goal-state-turns-used goal-st)))
  (define goal-text (goal-state-goal-text goal-st))

  ;; Emit turn-started
  (on-event 'goal-turn-started (hasheq 'turn-number turns 'goal-text goal-text))
  (on-status (format "Goal turn ~a/~a: working..." turns (goal-state-max-turns goal-st)))

  ;; Build the prompt — use evidence prompt from goal-evidence
  (define prompt
    (if (= turns 1)
        goal-text
        (evidence-prompt-for-goal goal-text (goal-state-last-evaluation goal-st))))

  ;; Run the prompt through the agent
  (define-values (updated-sess loop-result) (run-prompt-fn! prompt))

  ;; Execute deterministic checks if any
  (define check-results (execute-checks-for-goal goal-st))

  ;; Emit goal-check-completed for each check result
  (for ([cr (in-list check-results)])
    (on-event 'goal-check-completed
              (hasheq 'label
                      (check-result-label cr)
                      'exit-code
                      (check-result-exit-code cr)
                      'timed-out?
                      (check-result-timed-out? cr)
                      'stdout
                      (check-result-stdout cr)
                      'stderr
                      (check-result-stderr cr))))

  ;; Evaluate the result
  ;; Extract transcript from loop-result for evaluation
  (define transcript (extract-transcript-from-result loop-result))
  (define eval-result
    (if (eq? (goal-state-evaluator-mode goal-st) 'agent)
        (evaluate-with-agent goal-text
                             transcript
                             provider
                             evaluator-model
                             #:check-results check-results)
        (evaluate-transcript goal-text
                             transcript
                             provider
                             evaluator-model
                             #:check-results check-results)))

  ;; Emit goal.evaluated
  (on-event 'goal-evaluated
            (hasheq 'achieved?
                    (evaluation-result-achieved? eval-result)
                    'reason
                    (evaluation-result-reason eval-result)
                    'turn-number
                    turns
                    'token-cost
                    (evaluation-result-token-cost eval-result)))

  ;; Update goal state
  (define now (inexact->exact (round (current-inexact-milliseconds))))
  (define new-status (if (evaluation-result-achieved? eval-result) 'achieved 'active))

  (struct-copy goal-state
               goal-st
               [turns-used turns]
               [status new-status]
               [evaluations (append (goal-state-evaluations goal-st) (list eval-result))]
               [last-evaluation eval-result]
               [updated-at now]))

;; ============================================================
;; Helpers
;; ============================================================

(define (build-continuation-prompt goal-text last-eval)
  (define reason
    (if last-eval
        (evaluation-result-reason last-eval)
        "unknown"))
  (format "The goal is not yet achieved. Reason: ~a. Continue working toward: ~a" reason goal-text))

;; Extract transcript from loop-result for evaluator
;; loop-result is a struct from the iteration loop
(define (extract-transcript-from-result loop-result)
  ;; Try to extract messages from the loop result
  ;; The loop-result may have an 'assistant-response or 'messages field
  (cond
    [(hash? loop-result)
     (define messages (hash-ref loop-result 'messages '()))
     (if (list? messages)
         messages
         '())]
    [(list? loop-result) loop-result]
    [else '()]))

;; ============================================================
;; Test helper: run a simulated goal loop (no real run-prompt!)
;; ============================================================

(define/contract (goal-run-simulated! goal-text
                                      provider
                                      evaluator-model
                                      turn-responses
                                      #:max-turns [max-turns 8]
                                      #:evaluator-mode [evaluator-mode 'transcript]
                                      #:shutdown-check [shutdown-check (lambda () #f)])
  (->* (string? provider? string? list?)
       (#:max-turns exact-nonnegative-integer? #:evaluator-mode symbol? #:shutdown-check procedure?)
       goal-state?)
  ;; Simulated run-prompt! that returns predefined responses
  (define turn-idx (box 0))
  (define (sim-run-prompt! prompt)
    (define resp
      (if (< (unbox turn-idx) (length turn-responses))
          (list-ref turn-responses (unbox turn-idx))
          (last turn-responses)))
    (set-box! turn-idx (add1 (unbox turn-idx)))
    (values #f resp))

  (goal-run! goal-text
             provider
             evaluator-model
             sim-run-prompt!
             #:max-turns max-turns
             #:evaluator-mode evaluator-mode
             #:shutdown-check shutdown-check))
