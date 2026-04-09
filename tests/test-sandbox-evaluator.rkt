#lang racket

(require rackunit
         "../sandbox/evaluator.rkt")

;; ============================================================
;; eval-in-sandbox — simple expressions
;; ============================================================

(test-case "eval-in-sandbox evaluates simple expression"
  (define result (eval-in-sandbox "(+ 1 2)"))
  (check-true (eval-result? result))
  (check-equal? (eval-result-value result) 3)
  (check-false (eval-result-error result)))

(test-case "eval-in-sandbox captures output"
  (define result (eval-in-sandbox "(display \"hello\")"))
  (check-true (eval-result? result))
  (check-equal? (eval-result-output result) "hello"))

(test-case "eval-in-sandbox handles errors"
  (define result (eval-in-sandbox "(+ 1 \"not-a-number\")"))
  (check-true (eval-result? result))
  (check-true (string? (eval-result-error result))))

(test-case "eval-in-sandbox tracks elapsed time"
  (define result (eval-in-sandbox "42"))
  (check-true (>= (eval-result-elapsed-ms result) 0)))

(test-case "eval-in-sandbox returns #f value on error"
  (define result (eval-in-sandbox "(car '())"))
  (check-false (eval-result-value result))
  (check-true (string? (eval-result-error result))))
