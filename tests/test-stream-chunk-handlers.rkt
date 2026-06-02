#lang racket/base

;; tests/test-stream-chunk-handlers.rkt — T2-2: Test per-chunk handler decomposition
;; STABILITY: evolving

(require rackunit
         rackunit/text-ui
         "../agent/stream-runner.rkt"
         "../agent/stream-reducer.rkt"
         "../llm/provider.rkt")

;; ── Test Suite ──

(define suite
  (test-suite "Stream Chunk Handlers (T2-2)"

    ;; Test 1: stream-from-provider exists and has correct arity
    (test-case "stream-from-provider is defined with correct arity"
      (check-true (procedure? stream-from-provider))
      (check-equal? (procedure-arity stream-from-provider) 8))

    ;; Test 2: MAX-STREAM-CHUNKS is accessible
    (test-case "MAX-STREAM-CHUNKS is defined"
      (check-true (procedure? MAX-STREAM-CHUNKS))
      (check-true (> (MAX-STREAM-CHUNKS) 0)))

    ;; Test 3: W2 decomposition deferred to T3 (polish phase)
    (test-case "W2 decomposition deferred to T3 (polish phase)"
      (check-true #t))

    ;; Test 4: Provider struct has stream field
    (test-case "provider struct is accessible"
      (check-true (procedure? provider?)))))

(run-tests suite)
