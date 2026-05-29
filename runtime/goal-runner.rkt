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
         "goal-state.rkt"
         "goal-evaluator.rkt"
         "../llm/provider.rkt")

;; ============================================================
;; Provides
;; ============================================================

(provide goal-run!
         goal-run-simulated!
         goal-loop-step
         build-continuation-prompt
         NO-PROGRESS-THRESHOLD)

(define NO-PROGRESS-THRESHOLD 3)

;; ============================================================
;; Main entry point
;; ============================================================

(define/contract (goal-run! goal-text
                            provider
                            evaluator-model
                            run-prompt-fn!
                            #:max-turns [max-turns 8]
                            #:on-event [on-event void]
                            #:on-status [on-status void])
  (->* (string? provider? string? procedure?)
       (#:max-turns exact-nonnegative-integer? #:on-event procedure? #:on-status procedure?)
       goal-state?)
  ;; Initialize goal state
  (define goal-st
    (make-goal-state #:goal-text goal-text #:max-turns max-turns #:evaluator-model evaluator-model))

  ;; Emit goal.started
  (on-event 'goal-started goal-st)
  (on-status (format "Goal set: ~a (max ~a turns)" goal-text max-turns))

  ;; Run the loop
  (run-goal-loop goal-st provider evaluator-model run-prompt-fn! on-event on-status 0))

;; ============================================================
;; Internal loop
;; ============================================================

(define (run-goal-loop goal-st
                       provider
                       evaluator-model
                       run-prompt-fn!
                       on-event
                       on-status
                       no-progress-count)
  (cond
    ;; Terminal conditions
    [(>= (goal-state-turns-used goal-st) (goal-state-max-turns goal-st))
     (define final-st
       (struct-copy goal-state
                    goal-st
                    [status 'failed]
                    [updated-at (inexact->exact (round (current-inexact-milliseconds)))]))
     (on-event 'goal-failed final-st)
     (on-status (format "Goal failed: max turns (~a) reached" (goal-state-max-turns goal-st)))
     final-st]

    [(>= no-progress-count NO-PROGRESS-THRESHOLD)
     (define final-st
       (struct-copy goal-state
                    goal-st
                    [status 'failed]
                    [updated-at (inexact->exact (round (current-inexact-milliseconds)))]))
     (on-event 'goal-failed final-st)
     (on-status "Goal failed: no progress after 3 consecutive evaluations")
     final-st]

    [else
     ;; Run one step
     (define updated-st
       (goal-loop-step goal-st provider evaluator-model run-prompt-fn! on-event on-status))
     (cond
       [(eq? (goal-state-status updated-st) 'achieved)
        (on-status (format "Goal achieved in ~a turns!" (goal-state-turns-used updated-st)))
        updated-st]
       [(eq? (goal-state-status updated-st) 'failed)
        (on-status (format "Goal failed: ~a" (goal-state-last-evaluation-reason updated-st)))
        updated-st]
       [else
        ;; Active — check for no-progress
        (define new-no-progress
          (if (no-progress-evaluation? (goal-state-last-evaluation updated-st))
              (add1 no-progress-count)
              0))
        (run-goal-loop updated-st
                       provider
                       evaluator-model
                       run-prompt-fn!
                       on-event
                       on-status
                       new-no-progress)])]))

;; Check if evaluation shows no progress (same reason as before)
(define (no-progress-evaluation? eval-result)
  (and eval-result
       (not (evaluation-result-achieved? eval-result))
       (string-contains? (evaluation-result-reason eval-result) "no progress")))

;; Extract reason from last evaluation safely
(define (goal-state-last-evaluation-reason gs)
  (define le (goal-state-last-evaluation gs))
  (if le
      (evaluation-result-reason le)
      "unknown"))

;; ============================================================
;; Single step
;; ============================================================

(define/contract (goal-loop-step goal-st provider evaluator-model run-prompt-fn! on-event on-status)
  (-> goal-state? provider? string? procedure? procedure? procedure? goal-state?)
  (define turns (add1 (goal-state-turns-used goal-st)))
  (define goal-text (goal-state-goal-text goal-st))

  ;; Emit turn-started
  (on-event 'goal-turn-started (hasheq 'turn turns 'goal goal-st))
  (on-status (format "Goal turn ~a/~a: working..." turns (goal-state-max-turns goal-st)))

  ;; Build the prompt for this turn
  (define prompt
    (if (= turns 1)
        goal-text
        (build-continuation-prompt goal-text (goal-state-last-evaluation goal-st))))

  ;; Run the prompt through the agent
  (define-values (updated-sess loop-result) (run-prompt-fn! prompt))

  ;; Evaluate the result
  ;; Extract transcript from loop-result for evaluation
  (define transcript (extract-transcript-from-result loop-result))
  (define eval-result (evaluate-transcript goal-text transcript provider evaluator-model))

  ;; Emit goal.evaluated
  (on-event 'goal-evaluated (hasheq 'evaluation eval-result 'turn turns))

  ;; Update goal state
  (define now (inexact->exact (round (current-inexact-milliseconds))))
  (define new-status (if (evaluation-result-achieved? eval-result) 'achieved 'active))

  (struct-copy goal-state
               goal-st
               [turns-used turns]
               [status new-status]
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
                                      #:max-turns [max-turns 8])
  (->* (string? provider? string? list?) (#:max-turns exact-nonnegative-integer?) goal-state?)
  ;; Simulated run-prompt! that returns predefined responses
  (define turn-idx (box 0))
  (define (sim-run-prompt! prompt)
    (define resp
      (if (< (unbox turn-idx) (length turn-responses))
          (list-ref turn-responses (unbox turn-idx))
          (last turn-responses)))
    (set-box! turn-idx (add1 (unbox turn-idx)))
    (values #f resp))

  (goal-run! goal-text provider evaluator-model sim-run-prompt! #:max-turns max-turns))
