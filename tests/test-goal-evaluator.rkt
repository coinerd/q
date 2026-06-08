#lang racket/base

;; @speed fast
;; @suite default

;; tests/test-goal-evaluator.rkt — Evaluator unit tests with mock provider

(require rackunit
         racket/format
         racket/list
         racket/string
         json
         "../llm/model.rkt"
         "../llm/provider.rkt"
         "../runtime/goal/goal-evaluator.rkt"
         "../runtime/goal/goal-state.rkt"
         (only-in "../runtime/goal/goal-state.rkt" check-result))

;; ============================================================
;; Helper: create mock provider that returns predefined responses
;; ============================================================

(test-case "test-goal-evaluator"
  (define (make-eval-mock-provider responses)
    (define idx (box 0))
    (make-provider (lambda () "eval-mock")
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

  (define (text-response text [model "mock-eval"])
    (make-model-response (list (hasheq 'type "text" 'text text))
                         (hasheq 'total_tokens 50 'prompt_tokens 30 'completion_tokens 20)
                         model
                         'stop))

  ;; ============================================================
  ;; parse-evaluator-response tests
  ;; ============================================================

  (let ([er (parse-evaluator-response "{\"ok\": true, \"reason\": \"all tests pass\"}" "test-model")])
    (check-true (evaluation-result-achieved? er) "JSON ok=true → achieved")
    (check-equal? (evaluation-result-reason er) "all tests pass")
    (check-equal? (evaluation-result-model-used er) "test-model"))

  (let ([er (parse-evaluator-response "{\"ok\": false, \"reason\": \"missing evidence\"}"
                                      "test-model")])
    (check-false (evaluation-result-achieved? er) "JSON ok=false → not achieved")
    (check-equal? (evaluation-result-reason er) "missing evidence"))

  (let ([er (parse-evaluator-response "not json at all" "test-model")])
    (check-false (evaluation-result-achieved? er) "Non-JSON → not achieved")
    (check-true (string-contains? (evaluation-result-reason er) "non-JSON")
                "Non-JSON reason mentions non-JSON"))

  (let ([er (parse-evaluator-response "{\"ok\": true}" "test-model")])
    (check-true (evaluation-result-achieved? er) "JSON with ok, no reason → achieved")
    (check-equal? (evaluation-result-reason er) "no reason provided"))

  (let ([er (parse-evaluator-response "{\"ok\": false}" "test-model" 100)])
    (check-false (evaluation-result-achieved? er))
    (check-equal? (evaluation-result-token-cost er) 100 "token-cost passed through"))

  ;; ============================================================
  ;; evaluate-transcript with mock provider
  ;; ============================================================

  (let ()
    (define mock
      (make-eval-mock-provider
       (list (text-response "{\"ok\": true, \"reason\": \"evidence found\"}"))))
    (define er
      (evaluate-transcript
       "make tests pass"
       (list (hasheq 'role "assistant" 'content "I ran the tests and they all pass."))
       mock
       "mock-eval"))
    (check-true (evaluation-result-achieved? er) "Mock returns achieved")
    (check-equal? (evaluation-result-reason er) "evidence found")
    (check-equal? (evaluation-result-model-used er) "mock-eval")
    (check-true (> (evaluation-result-token-cost er) 0) "Token cost tracked"))

  (let ()
    (define mock
      (make-eval-mock-provider
       (list (text-response "{\"ok\": false, \"reason\": \"no test output\"}"))))
    (define er
      (evaluate-transcript "tests pass"
                           (list (hasheq 'role "assistant" 'content "I wrote some code."))
                           mock
                           "mock-eval"))
    (check-false (evaluation-result-achieved? er) "Mock returns not achieved")
    (check-equal? (evaluation-result-reason er) "no test output"))

  (let ()
    (define mock (make-eval-mock-provider (list (text-response "I think the goal is met"))))
    (define er (evaluate-transcript "do something" '() mock "mock-eval"))
    (check-false (evaluation-result-achieved? er) "Non-JSON response → not achieved")
    (check-true (string-contains? (evaluation-result-reason er) "non-JSON")))

  ;; ============================================================
  ;; Empty transcript
  ;; ============================================================

  (let ()
    (define mock
      (make-eval-mock-provider
       (list (text-response "{\"ok\": false, \"reason\": \"empty transcript\"}"))))
    (define er (evaluate-transcript "goal" '() mock "mock-eval"))
    (check-false (evaluation-result-achieved? er) "Empty transcript → not achieved"))

  ;; ============================================================
  ;; Multiple evaluator turns
  ;; ============================================================

  (let ()
    (define mock
      (make-eval-mock-provider
       (list (text-response "{\"ok\": false, \"reason\": \"first attempt\"}")
             (text-response "{\"ok\": true, \"reason\": \"second attempt succeeded\"}"))))
    (define er1 (evaluate-transcript "goal" '() mock "mock-eval"))
    (check-false (evaluation-result-achieved? er1))
    (define er2 (evaluate-transcript "goal" '() mock "mock-eval"))
    (check-true (evaluation-result-achieved? er2) "Second evaluation succeeds"))

  (displayln "All goal-evaluator tests passed.")

  ;; ============================================================
  ;; evaluate-transcript with check-results
  ;; ============================================================

  ;; ============================================================
  ;; evaluate-transcript with check-results
  ;; ============================================================

  (let ()
    (define (text-response text)
      (make-model-response (list (hasheq 'type "text" 'text text))
                           (hasheq 'total_tokens 20)
                           "mock"
                           'stop))
    (define (make-check-mock responses)
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
    (define cr-pass (check-result "test-1" 0 "all good" "" #f 10))
    (define cr-fail (check-result "test-2" 1 "" "error" #f 5))
    ;; Provider returns "achieved"
    (define mock-ok
      (make-check-mock (list (text-response "{\"ok\": true, \"reason\": \"checks pass\"}"))))
    (define er-pass
      (evaluate-transcript "goal" '() mock-ok "mock-eval" #:check-results (list cr-pass)))
    (check-true (evaluation-result-achieved? er-pass) "achieved with passing check")
    ;; Provider returns "not yet" for failed check context
    (define mock-fail
      (make-check-mock (list (text-response "{\"ok\": false, \"reason\": \"check failed\"}"))))
    (define er-fail
      (evaluate-transcript "goal" '() mock-fail "mock-eval" #:check-results (list cr-fail)))
    (check-false (evaluation-result-achieved? er-fail) "failed check → not achieved")))
