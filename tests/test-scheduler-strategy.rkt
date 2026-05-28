#lang racket/base

;; tests/test-scheduler-strategy.rkt — R-15: scheduler strategy tests

(require rackunit
         rackunit/text-ui
         "../tools/scheduler-strategy.rkt")

(define-test-suite test-scheduler-strategy
  (test-case "default-scheduler-strategy is a scheduler-strategy"
    (define s (default-scheduler-strategy))
    (check-pred scheduler-strategy? s))

  (test-case "default preflight-filter passes all tools"
    (define s (default-scheduler-strategy))
    (define calls '(a b c))
    (check-equal? ((scheduler-strategy-preflight-filter s) calls) calls))

  (test-case "default execution-order preserves order"
    (define s (default-scheduler-strategy))
    (define calls '(a b c))
    (check-equal? ((scheduler-strategy-execution-order s) calls) calls))

  (test-case "custom strategy can filter tools"
    (define s
      (scheduler-strategy (lambda (calls) (filter (lambda (c) (not (equal? c 'skip-me))) calls))
                          (lambda (calls) calls)))
    (check-equal? ((scheduler-strategy-preflight-filter s) '(a skip-me b)) '(a b)))

  (test-case "custom strategy can reorder"
    (define s (scheduler-strategy values (lambda (calls) (reverse calls))))
    (check-equal? ((scheduler-strategy-execution-order s) '(a b c)) '(c b a))))

(run-tests test-scheduler-strategy)
