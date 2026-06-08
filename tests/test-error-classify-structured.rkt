#lang racket/base

;; @speed fast
;; @suite default

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
    (test-case "classify-http-status maps codes to categories"
      (check-equal? (classify-http-status 429) 'rate-limit)
      (check-equal? (classify-http-status 401) 'auth)
      (check-equal? (classify-http-status 500) 'server)
      (check-equal? (classify-http-status 200) #f))

    ;; Test 3: retryable-error? uses structured category fast path
    (test-case "retryable-error? recognizes structured provider-error"
      (define err (provider-error "test" (current-continuation-marks) (hash) 'rate-limit 429))
      (check-true (retryable-error? err)))

    ;; Test 4: non-provider-error falls back to string matching
    (test-case "retryable-error? falls back to string matching"
      (define err (exn:fail "429 Too Many Requests" (current-continuation-marks)))
      (check-true (retryable-error? err)))

    ;; Test 5: timeout-error? uses structured category fast path
    (test-case "timeout-error? recognizes structured provider-error"
      (define err (provider-error "test" (current-continuation-marks) (hash) 'timeout #f))
      (check-true (timeout-error? err)))

    ;; Test 6: rate-limit-error? uses structured category fast path
    (test-case "rate-limit-error? recognizes structured provider-error"
      (define err (provider-error "test" (current-continuation-marks) (hash) 'rate-limit 429))
      (check-true (rate-limit-error? err)))

    ;; Test 7: context-overflow-error? uses structured category fast path
    (test-case "context-overflow-error? recognizes structured provider-error"
      (define err (provider-error "test" (current-continuation-marks) (hash) 'context-overflow 413))
      (check-true (context-overflow-error? err)))

    ;; Test 8: string fallback still works for timeout
    (test-case "timeout-error? falls back to string matching"
      (define err (exn:fail "connection timed out" (current-continuation-marks)))
      (check-true (timeout-error? err)))

    ;; Test 9: string fallback still works for rate limit
    (test-case "rate-limit-error? falls back to string matching"
      (define err (exn:fail "rate limit exceeded" (current-continuation-marks)))
      (check-true (rate-limit-error? err)))))

(run-tests suite)
