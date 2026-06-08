#lang racket/base

;; tests/test-goal-agent-evaluator.rkt — Agent evaluator tests

(require rackunit
         racket/format
         racket/string
         "../runtime/goal/goal-agent-evaluator.rkt"
         "../runtime/goal/goal-state.rkt"
         "../llm/model.rkt"
         "../llm/provider.rkt")

;; ============================================================
;; Helper: create mock provider with text response
;; ============================================================

(define (make-mock-agent-provider response-text)
  (define resp
    (make-model-response (list (hasheq 'text response-text 'type "text"))
                         (hasheq 'total_tokens 100 'prompt_tokens 80 'completion_tokens 20)
                         "mock-0"
                         #f))
  (make-mock-provider resp))

;; ============================================================
;; Agent evaluator — achieved
;; ============================================================

(let ()
  (define provider (make-mock-agent-provider "{\"ok\": true, \"reason\": \"tests pass\"}"))
  (define result (evaluate-with-agent "make tests pass" '() provider "mock-model"))
  (check-true (evaluation-result-achieved? result))
  (check-equal? (evaluation-result-reason result) "tests pass")
  (check-equal? (evaluation-result-token-cost result) 100))

;; ============================================================
;; Agent evaluator — not achieved
;; ============================================================

(let ()
  (define provider (make-mock-agent-provider "{\"ok\": false, \"reason\": \"2 tests still fail\"}"))
  (define result (evaluate-with-agent "fix all bugs" '() provider "mock-model"))
  (check-false (evaluation-result-achieved? result))
  (check-equal? (evaluation-result-reason result) "2 tests still fail"))

;; ============================================================
;; Agent evaluator — non-JSON response
;; ============================================================

(let ()
  (define provider (make-mock-agent-provider "I cannot determine the result yet."))
  (define result (evaluate-with-agent "do something" '() provider "mock-model"))
  (check-false (evaluation-result-achieved? result))
  (check-true (string-contains? (evaluation-result-reason result) "non-JSON")))

;; ============================================================
;; Agent evaluator — with check results
;; ============================================================

(let ()
  (define provider (make-mock-agent-provider "{\"ok\": true, \"reason\": \"check passed\"}"))
  (define checks (list (check-result "tests" 0 "ok" "" #f 100)))
  (define result (evaluate-with-agent "tests pass" '() provider "mock-model" #:check-results checks))
  (check-true (evaluation-result-achieved? result)))

;; ============================================================
;; Agent evaluator — with transcript
;; ============================================================

(let ()
  (define provider (make-mock-agent-provider "{\"ok\": false, \"reason\": \"incomplete\"}"))
  (define transcript (list (hasheq 'role "assistant" 'content "I fixed bug A.")))
  (define result (evaluate-with-agent "fix all bugs" transcript provider "mock-model"))
  (check-false (evaluation-result-achieved? result))
  (check-equal? (evaluation-result-reason result) "incomplete"))

;; ============================================================
;; Agent evaluator — provider error
;; ============================================================

(let ()
  (define resp (make-model-response '() (hasheq 'total_tokens 0) "mock-0" #f))
  (define provider
    (make-provider (lambda () "mock-error")
                   (lambda () (hasheq 'streaming #f))
                   (lambda (req) (error "provider failed"))
                   (lambda (req) (error "no stream"))))
  (define result (evaluate-with-agent "goal" '() provider "mock"))
  (check-false (evaluation-result-achieved? result))
  (check-true (string-contains? (evaluation-result-reason result) "Agent evaluator error")))

;; ============================================================
;; AGENT-EVALUATOR-SYSTEM-PROMPT is defined
;; ============================================================

(test-case "test-goal-agent-evaluator: checks block 1"
  (check-true (string? AGENT-EVALUATOR-SYSTEM-PROMPT))
  (check-true (string-contains? AGENT-EVALUATOR-SYSTEM-PROMPT "goal achievement evaluator")))

(displayln "All goal-agent-evaluator tests passed.")
