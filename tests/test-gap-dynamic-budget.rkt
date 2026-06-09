#lang racket/base

;; tests/test-gap-dynamic-budget.rkt
;; v0.97.4 W0: GAP-E dynamic conclusion budget

(require rackunit
         rackunit/text-ui
         (only-in "../runtime/context-assembly/config.rkt" compute-conclusion-budget)
         (only-in "../runtime/session/session-config.rkt" apply-context-assembly-profile!)
         (only-in "../runtime/context-assembly/config.rkt" current-conclusion-token-budget))

(define suite
  (test-suite "gap-e-dynamic-conclusion-budget"

    ;; compute-conclusion-budget: 10% of context window
    (test-case "compute-conclusion-budget: 10% of max-context-tokens"
      (check-equal? (compute-conclusion-budget 10000) 1000)
      (check-equal? (compute-conclusion-budget 50000) 4000))

    ;; compute-conclusion-budget: respects minimum 500
    (test-case "compute-conclusion-budget: respects 500 token floor"
      (check-equal? (compute-conclusion-budget 100) 500)
      (check-equal? (compute-conclusion-budget 4000) 500))

    ;; compute-conclusion-budget: respects maximum 4000
    (test-case "compute-conclusion-budget: respects 4000 token ceiling"
      (check-equal? (compute-conclusion-budget 100000) 4000)
      (check-equal? (compute-conclusion-budget 128000) 4000))

    ;; compute-conclusion-budget: typical model sizes
    (test-case "compute-conclusion-budget: typical model context windows"
      (check-equal? (compute-conclusion-budget 8192) 819) ; 8k model
      (check-equal? (compute-conclusion-budget 32768) 3276) ; 32k model
      (check-equal? (compute-conclusion-budget 128000) 4000) ; 128k model → capped
      (check-equal? (compute-conclusion-budget 200000) 4000)) ; 200k model → capped

    ;; apply-context-assembly-profile! uses dynamic budget
    (test-case "profile: bounded uses dynamic budget from max-context-tokens"
      (apply-context-assembly-profile! 'bounded 32768)
      (check-equal? (current-conclusion-token-budget) 3276))

    ;; apply-context-assembly-profile! backward compat: default 128000
    (test-case "profile: default max-context-tokens is 128000"
      (apply-context-assembly-profile! 'observe)
      (check-equal? (current-conclusion-token-budget) 4000))))

(run-tests suite)
