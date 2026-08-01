#lang racket

;; @speed fast  ;; @suite runtime

;;; test-goal-runner-eval-timeout.rkt — v0.99.78 regression:
;;; The goal-loop evaluator LLM request must be wall-clock bounded so a
;;; stalled (held) request can't freeze the loop indefinitely.

(require rackunit
         "../runtime/goal/goal-runner.rkt"
         "../runtime/goal/goal-state.rkt"
         "../runtime/goal/goal-checks.rkt"
         "../llm/provider.rkt"
         "../util/loop-result.rkt")

;; Mock provider whose send blocks forever (simulates a held request)
(define (make-blocking-provider)
  (make-provider (lambda () "blocking-eval")
                 (lambda () (hash 'streaming #f 'token-counting #t))
                 (lambda (req) (sync never-evt))
                 (lambda (req) (sync never-evt))))

;; Mock run-prompt-fn! that returns immediately with a minimal result
(define (instant-prompt-fn! prompt)
  (values #f (make-loop-result '() 'completed (hasheq))))

(test-case "goal-loop-step recovers from evaluator timeout"
  (define goal-st
    (make-goal-state #:goal-text "test goal"
                     #:max-turns 3
                     #:evaluator-model "mock-eval"
                     #:evaluator-mode 'transcript))
  (define prov (make-blocking-provider))
  (define events '())
  (define (on-event type payload)
    (set! events (cons (cons type payload) events)))
  (define (on-status msg)
    (void))
  (parameterize ([current-eval-timeout-secs 2])
    (define updated-st
      (goal-loop-step goal-st prov "mock-eval" instant-prompt-fn! on-event on-status))
    (define evals (goal-state-evaluations updated-st))
    (check-true (pair? evals) "at least one evaluation recorded")
    (when (pair? evals)
      (check-false (evaluation-result-achieved? (car (reverse evals)))
                   "timed-out eval should be not-achieved"))))
