#lang racket

;; BOUNDARY: integration
;; CHARACTERIZATION TEST — documents current retry/error classification for remediation

;; tests/test-retry-error-classification.rkt
;; Pins current retry/error classification behavior.
;;
;; Key findings:
;; F1: Structured provider-error with category 'server (from 5xx) is NOT retryable
;;     because retryable-error? checks for 'server-error but classify-http-status
;;     returns 'server. This is a CATEGORY MISMATCH BUG.
;; F2: Context-overflow string errors are correctly NOT retryable.
;; F3: String-based 5xx patterns ("500", "503") ARE retryable via fallback.
;; F4: Structured rate-limit and timeout errors ARE correctly retryable.

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

;; ── Characterization Tests ──

(define retry-classification-suite
  (test-suite "retry-error-classification-characterization"

    ;; F1: BUG — structured 5xx (category 'server) NOT retryable
    (test-case "CHAR: structured 500 provider-error NOT retryable (BUG: server vs server-error)"
      ;; classify-http-status returns 'server for >= 500
      ;; retryable-error? checks for 'server-error (with hyphen)
      ;; These don't match → structured 5xx errors are silently NOT retried
      (define err-500
        (provider-error "Internal Server Error" (current-continuation-marks) (hash) 'server 500))
      (check-equal? (provider-error-category err-500)
                    'server
                    "classify-http-status maps 500 → 'server")
      (check-false (retryable-error? err-500)
                   "BUG: structured 500 not retryable — 'server ≠ 'server-error")
      ;; Also test 502, 503
      (define err-502 (provider-error "Bad Gateway" (current-continuation-marks) (hash) 'server 502))
      (check-false (retryable-error? err-502) "BUG: structured 502 not retryable")
      (define err-503
        (provider-error "Service Unavailable" (current-continuation-marks) (hash) 'server 503))
      (check-false (retryable-error? err-503) "BUG: structured 503 not retryable"))

    ;; F2: Context-overflow string errors correctly NOT retryable
    (test-case "CHAR: context-overflow string error not retryable (correct)"
      (define err (exn:fail "context_length exceeded" (current-continuation-marks)))
      (check-true (context-overflow-error? err) "context-overflow detected")
      (check-false (retryable-error? err) "context-overflow correctly not retryable"))

    ;; F3: String-based 5xx IS retryable via fallback
    (test-case "CHAR: string-based 5xx patterns retryable via fallback"
      (check-true (retryable-error? (exn:fail "500 Internal Server Error"
                                              (current-continuation-marks)))
                  "string '500' is retryable")
      (check-true (retryable-error? (exn:fail "503 Service Unavailable" (current-continuation-marks)))
                  "string '503' is retryable"))

    ;; F4: Structured rate-limit and timeout ARE correctly retryable
    (test-case "CHAR: structured rate-limit provider-error retryable"
      (define err
        (provider-error "Rate limit exceeded" (current-continuation-marks) (hash) 'rate-limit 429))
      (check-equal? (provider-error-category err) 'rate-limit)
      (check-not-false (retryable-error? err) "structured rate-limit IS retryable"))

    (test-case "CHAR: structured timeout provider-error retryable"
      (define err
        (provider-error "Request timed out" (current-continuation-marks) (hash) 'timeout #f))
      (check-equal? (provider-error-category err) 'timeout)
      (check-not-false (retryable-error? err) "structured timeout IS retryable"))

    ;; F5: classify-http-status mapping characterization
    (test-case "CHAR: classify-http-status maps 5xx to 'server (not 'server-error)"
      (check-equal? (classify-http-status 500) 'server)
      (check-equal? (classify-http-status 502) 'server)
      (check-equal? (classify-http-status 503) 'server)
      (check-equal? (classify-http-status 504) 'server))

    ;; F6: classify-error for structured server error
    (test-case "CHAR: classify-error for structured server error"
      (define err (provider-error "500 error" (current-continuation-marks) (hash) 'server 500))
      (check-equal? (classify-error err)
                    'server
                    "classify-error returns 'server for structured server error (from category)"))

    ;; F7: Fix recommendation
    (test-case "CHAR: document fix for server vs server-error mismatch"
      ;; Fix: either change classify-http-status to return 'server-error
      ;; or change retryable-error? to check for 'server instead of 'server-error.
      ;; Recommendation: change retryable-error? to (memq cat '(... server ...))
      ;; since 'server is the canonical name from classify-http-status.
      ;; Also check: does ERROR-CLASSIFICATION-TABLE use 'server or 'server-error?
      (check-true #t "fix: align category symbol names between classify and retry"))))

(run-tests retry-classification-suite)
