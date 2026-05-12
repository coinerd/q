#lang racket

;; tests/test-scheduler-strategy.rkt — R-15: scheduler strategy tests

(require rackunit
         rackunit/text-ui
         "../tools/scheduler-strategy.rkt")

(define strategy-suite
  (test-suite "Scheduler strategy tests"

    ;; ── Strategy struct ──
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

    (test-case "default parallelism-degree is 1 (serial)"
      (define s (default-scheduler-strategy))
      (check-equal? ((scheduler-strategy-parallelism-degree s)) 1))

    ;; ── Custom strategy ──
    (test-case "custom strategy can filter tools"
      (define s
        (scheduler-strategy (lambda (calls) (filter (lambda (c) (not (equal? c 'skip-me))) calls))
                            (lambda (calls) calls)
                            (lambda () 2)))
      (check-equal? ((scheduler-strategy-preflight-filter s) '(a skip-me b)) '(a b)))

    (test-case "custom strategy can reorder"
      (define s (scheduler-strategy values (lambda (calls) (reverse calls)) (lambda () 1)))
      (check-equal? ((scheduler-strategy-execution-order s) '(a b c)) '(c b a)))

    ;; ── Tool invocation result algebra ──
    (test-case "tool-success"
      (define r (tool-success "read" "content" #f))
      (check-pred tool-success? r)
      (check-pred tool-invocation-result? r)
      (check-equal? (tool-invocation-result-tool-name r) "read")
      (check-equal? (tool-success-content r) "content"))

    (test-case "tool-structured-failure"
      (define r (tool-structured-failure "edit" "bad match" #f))
      (check-pred tool-structured-failure? r)
      (check-pred tool-invocation-result? r)
      (check-equal? (tool-structured-failure-message r) "bad match"))

    (test-case "tool-retryable-failure"
      (define r (tool-retryable-failure "bash" "timeout" #f))
      (check-pred tool-retryable-failure? r)
      (check-pred tool-invocation-result? r))

    (test-case "tool-policy-denied"
      (define r (tool-policy-denied "write" "safe mode"))
      (check-pred tool-policy-denied? r)
      (check-pred tool-invocation-result? r)
      (check-equal? (tool-policy-denied-reason r) "safe mode"))

    ;; ── Result type discrimination ──
    (test-case "each result type is distinct"
      (define results
        (list (tool-success "a" "ok" #f)
              (tool-structured-failure "b" "err" #f)
              (tool-retryable-failure "c" "retry" #f)
              (tool-policy-denied "d" "denied")))
      (check-true (tool-success? (car results)))
      (check-true (tool-structured-failure? (cadr results)))
      (check-true (tool-retryable-failure? (caddr results)))
      (check-true (tool-policy-denied? (cadddr results))))))

(run-tests strategy-suite 'verbose)
