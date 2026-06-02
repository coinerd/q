#lang racket/base

;; tests/test-error-classify-structured.rkt — T1-4: Structured error classification
;; STABILITY: evolving

(require rackunit
         rackunit/text-ui
         "../llm/provider-errors.rkt"
         "../runtime/auto-retry.rkt")

;; ── Test Suite ──

(define suite
  (test-suite "Structured Error Classification (T1-4)"

    ;; Test 1: provider-error struct works
    (test-case "provider-error has structured category"
      (check-true (procedure? provider-error?))
      (check-true (procedure? provider-error-category))
      (check-true (procedure? provider-error-status-code)))

    ;; Test 2: classify-http-status maps common codes
    (test-case "classify-http-status maps 429 → rate-limit"
      (check-equal? (classify-http-status 429) 'rate-limit)
      (check-equal? (classify-http-status 401) 'auth)
      (check-equal? (classify-http-status 500) 'server)
      (check-equal? (classify-http-status 200) #f))

    ;; Test 3: auto-retry uses provider-error-category fast path
    (test-case "retryable-error? recognizes structured provider-error"
      (define err (provider-error "test" (current-continuation-marks) (hash) 'rate-limit 429))
      (check-true (retryable-error? err)))

    ;; Test 4: non-provider-error falls back to string matching
    (test-case "retryable-error? falls back to string matching"
      (define err (exn:fail "429 Too Many Requests" (current-continuation-marks)))
      (check-true (retryable-error? err)))))

(run-tests suite)
