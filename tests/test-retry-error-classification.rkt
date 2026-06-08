#lang racket

;; @speed fast
;; @suite default

;; BOUNDARY: integration
;; FIX VERIFICATION TEST — confirms retry/error classification bug is fixed

;; tests/test-retry-error-classification.rkt
;; Verifies that structured provider errors with category 'server are retryable.
;;
;; Fixed in W11 (v0.57.2): retryable-error? now checks for 'server (not 'server-error).
;; Key behaviors:
;; F1: Structured provider-error with category 'server (from 5xx) IS retryable (FIXED)
;; F2: Context-overflow errors are correctly NOT retryable.
;; F3: String-based 5xx patterns ARE retryable via fallback.
;; F4: Structured rate-limit and timeout errors ARE correctly retryable.
;; F5: Auth errors are NOT retryable.

(require rackunit
         rackunit/text-ui
         (only-in "../runtime/auto-retry.rkt"
                  retryable-error?
                  context-overflow-error?
                  classify-error
                  timeout-error?
                  rate-limit-error?)
         (only-in "../llm/provider-errors.rkt"
                  provider-error
                  provider-error?
                  provider-error-category
                  provider-error-status-code
                  classify-http-status))

;; ── Fix Verification Tests ──

(define retry-classification-suite
  (test-suite "retry-error-classification-fix"

    ;; F1: FIXED — structured 5xx (category 'server) IS NOW retryable
    (test-case "FIX: structured 500 provider-error IS retryable (server category)"
      (define err-500
        (provider-error "Internal Server Error" (current-continuation-marks) (hash) 'server 500))
      (check-equal? (provider-error-category err-500) 'server)
      (check-not-false (retryable-error? err-500) "FIXED: structured 500 is retryable")
      (define err-502 (provider-error "Bad Gateway" (current-continuation-marks) (hash) 'server 502))
      (check-not-false (retryable-error? err-502) "FIXED: structured 502 is retryable")
      (define err-503
        (provider-error "Service Unavailable" (current-continuation-marks) (hash) 'server 503))
      (check-not-false (retryable-error? err-503) "FIXED: structured 503 is retryable"))

    ;; F2: Context-overflow errors correctly NOT retryable
    (test-case "FIX: context-overflow string error not retryable"
      (define err (exn:fail "context_length exceeded" (current-continuation-marks)))
      (check-true (context-overflow-error? err) "context-overflow detected")
      (check-false (retryable-error? err) "context-overflow correctly not retryable"))

    ;; F2b: Structured context-overflow is NOT retryable
    (test-case "FIX: structured context-overflow provider-error not retryable"
      (define err
        (provider-error "Context length exceeded"
                        (current-continuation-marks)
                        (hash)
                        'context-overflow
                        413))
      (check-equal? (provider-error-category err) 'context-overflow)
      (check-false (retryable-error? err) "structured context-overflow not retryable"))

    ;; F3: String-based 5xx IS retryable via fallback
    (test-case "FIX: string-based 5xx patterns retryable via fallback"
      (check-not-false (retryable-error? (exn:fail "500 Internal Server Error"
                                                   (current-continuation-marks)))
                       "string '500' is retryable")
      (check-not-false (retryable-error? (exn:fail "503 Service Unavailable"
                                                   (current-continuation-marks)))
                       "string '503' is retryable"))

    ;; F4: Structured rate-limit and timeout ARE correctly retryable
    (test-case "FIX: structured rate-limit provider-error retryable"
      (define err
        (provider-error "Rate limit exceeded" (current-continuation-marks) (hash) 'rate-limit 429))
      (check-equal? (provider-error-category err) 'rate-limit)
      (check-not-false (retryable-error? err) "structured rate-limit IS retryable"))

    (test-case "FIX: structured timeout provider-error retryable"
      (define err
        (provider-error "Request timed out" (current-continuation-marks) (hash) 'timeout #f))
      (check-equal? (provider-error-category err) 'timeout)
      (check-not-false (retryable-error? err) "structured timeout IS retryable"))

    ;; F5: classify-http-status mapping
    (test-case "FIX: classify-http-status maps 5xx to 'server"
      (check-equal? (classify-http-status 500) 'server)
      (check-equal? (classify-http-status 502) 'server)
      (check-equal? (classify-http-status 503) 'server)
      (check-equal? (classify-http-status 504) 'server))

    ;; F6: classify-error for structured server error
    (test-case "FIX: classify-error for structured server error"
      (define err (provider-error "500 error" (current-continuation-marks) (hash) 'server 500))
      (check-equal? (classify-error err) 'server))

    ;; F7: Network error classification
    (test-case "FIX: structured network provider-error retryable"
      (define err
        (provider-error "Connection refused" (current-continuation-marks) (hash) 'network #f))
      (check-equal? (provider-error-category err) 'network)
      (check-not-false (retryable-error? err) "structured network error IS retryable"))

    ;; F8: Auth errors are NOT retryable
    (test-case "FIX: structured auth provider-error not retryable"
      (define err (provider-error "Unauthorized" (current-continuation-marks) (hash) 'auth 401))
      (check-equal? (provider-error-category err) 'auth)
      (check-false (retryable-error? err) "structured auth error NOT retryable"))))

(run-tests retry-classification-suite)
