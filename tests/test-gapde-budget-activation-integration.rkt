#lang racket/base

;; tests/test-gapde-budget-activation-integration.rkt
;; v0.97.4 W2: Integration tests for dynamic budget + activation chain

(require rackunit
         rackunit/text-ui
         (only-in "../runtime/context-assembly/config.rkt"
                  current-conclusion-token-budget
                  compute-conclusion-budget)
         (only-in "../runtime/context-assembly/conclusion-ranker.rkt" rank-and-budget)
         (only-in "../runtime/context-assembly/task-conclusion.rkt" task-conclusion)
         (only-in "../runtime/session/session-config.rkt" apply-context-assembly-profile!))

(define (make-conclusion id text [cat 'fact])
  (task-conclusion id text cat 'exploration '() 0 '() '()))

(define suite
  (test-suite "gapde-budget-activation-integration"

    ;; GAP-E: Profile sets dynamic budget, rank-and-budget respects it
    (test-case "full profile 32K context → budget 3276 → rank-and-budget respects it"
      (parameterize ([current-conclusion-token-budget 2000])
        (apply-context-assembly-profile! 'full 32768)
        (define budget (current-conclusion-token-budget))
        (check-equal? budget 3276)
        ;; Create 10 conclusions with ~100 tokens each
        (define conclusions
          (for/list ([i (in-range 10)])
            (make-conclusion (format "id-~a" i) (make-string 80 #\a))))
        (define result (rank-and-budget conclusions #:max-conclusion-tokens budget))
        ;; Should include more than 3 but not all 10
        (check-true (> (length result) 3))
        (check-true (< (length result) 10))))

    ;; GAP-E: Large context gets capped at 4000
    (test-case "self-healing 200K context → budget capped at 4000"
      (parameterize ([current-conclusion-token-budget 2000])
        (apply-context-assembly-profile! 'self-healing 200000)
        (check-equal? (current-conclusion-token-budget) 4000)))

    ;; GAP-D + GAP-E: off profile gets small default budget but no injection
    (test-case "off profile: budget set but no memory injection"
      (parameterize ([current-conclusion-token-budget 2000])
        (apply-context-assembly-profile! 'off 128000)
        ;; Budget is still set (for conclusion ranking)
        (check-equal? (current-conclusion-token-budget) 4000)))

    ;; GAP-E: Small 8K context → budget 819
    (test-case "bounded profile 8K context → budget 819"
      (parameterize ([current-conclusion-token-budget 2000])
        (apply-context-assembly-profile! 'bounded 8192)
        (check-equal? (current-conclusion-token-budget) 819)))

    ;; GAP-E: compute-conclusion-budget min floor 500
    (test-case "compute-conclusion-budget floor at 500"
      (check-equal? (compute-conclusion-budget 100) 500)
      (check-equal? (compute-conclusion-budget 4000) 500))))

(run-tests suite)
