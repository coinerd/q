#lang racket/base

;; tests/test-context-assembly-config.rkt — T2-1: Context assembly config struct
;; STABILITY: evolving

(require rackunit
         rackunit/text-ui
         "../runtime/context-assembly/config.rkt")

;; ── Test Suite ──

(define suite
  (test-suite "Context Assembly Config (T2-1)"

    ;; Test 1: config struct exists with all 12 fields
    (test-case "context-assembly-config has 12 fields"
      (define cfg (default-context-assembly-config))
      (check-true (context-assembly-config? cfg))
      ;; State-aware fields
      (check-false (context-assembly-config-state-aware? cfg))
      (check-false (context-assembly-config-graph-selection? cfg))
      (check-equal? (context-assembly-config-conclusion-token-budget cfg) 2000)
      (check-false (context-assembly-config-ws-evolution? cfg))
      ;; Auto-distillation fields
      (check-false (context-assembly-config-auto-distill? cfg))
      (check-false (context-assembly-config-auto-distill-callback cfg))
      ;; Rollback fields
      (check-false (context-assembly-config-rollback? cfg))
      (check-false (context-assembly-config-force-distill-callback cfg))
      (check-false (context-assembly-config-expand-context-callback cfg))
      (check-false (context-assembly-config-revert-state-callback cfg))
      (check-equal? (context-assembly-config-rollback-log cfg) '())
      ;; State inference
      (check-equal? (context-assembly-config-state-inference-threshold cfg) 0.7))

    ;; Test 2: current-context-assembly-config parameter works
    (test-case "current-context-assembly-config parameter defaults"
      (define cfg (current-context-assembly-config))
      (check-true (context-assembly-config? cfg)))

    ;; Test 3: can create custom config
    (test-case "custom config construction"
      (define cfg (context-assembly-config #t #t 4000 #t #t #f #t #f #f #f '() 0.9))
      (check-true (context-assembly-config-state-aware? cfg))
      (check-equal? (context-assembly-config-conclusion-token-budget cfg) 4000)
      (check-true (context-assembly-config-auto-distill? cfg))
      (check-true (context-assembly-config-rollback? cfg))
      (check-equal? (context-assembly-config-state-inference-threshold cfg) 0.9))))

(run-tests suite)
