#lang racket

;;; tests/test-auto-retry.rkt — tests for runtime/auto-retry.rkt

(require rackunit
         rackunit/text-ui
         "../runtime/auto-retry.rkt")

;; ============================================================
;; retryable-error? predicate tests
;; ============================================================

(test-case "retryable-error?: rate limit errors"
  (check-true (retryable-error? (exn:fail "HTTP 429 rate limit" (current-continuation-marks))))
  (check-true (retryable-error? (exn:fail "rate limit exceeded" (current-continuation-marks))))
  (check-true (retryable-error? (exn:fail "Too many requests" (current-continuation-marks)))))

(test-case "retryable-error?: server errors"
  (check-true (retryable-error? (exn:fail "HTTP 500 server error" (current-continuation-marks))))
  (check-true (retryable-error? (exn:fail "HTTP 502 bad gateway" (current-continuation-marks))))
  (check-true (retryable-error? (exn:fail "HTTP 503 service unavailable"
                                          (current-continuation-marks))))
  (check-true (retryable-error? (exn:fail "HTTP 504 gateway timeout" (current-continuation-marks)))))

(test-case "retryable-error?: timeout errors"
  (check-true (retryable-error? (exn:fail "connection timed out" (current-continuation-marks))))
  (check-true (retryable-error? (exn:fail "timeout waiting for response"
                                          (current-continuation-marks))))
  (check-true (retryable-error? (exn:fail "network connection reset" (current-continuation-marks)))))

(test-case "retryable-error?: non-retryable errors"
  (check-false (retryable-error? (exn:fail "invalid API key" (current-continuation-marks))))
  (check-false (retryable-error? (exn:fail "model not found" (current-continuation-marks))))
  (check-false (retryable-error? (exn:fail "bad request: missing field"
                                           (current-continuation-marks)))))

(test-case "retryable-error?: case insensitive"
  (check-true (retryable-error? (exn:fail "RATE LIMIT EXCEEDED" (current-continuation-marks))))
  (check-true (retryable-error? (exn:fail "Overloaded Service" (current-continuation-marks)))))

;; ============================================================
;; with-auto-retry execution tests
;; ============================================================

(test-case "with-auto-retry: succeeds on first try"
  (define result (with-auto-retry (lambda () 42)))
  (check-equal? result 42))

(test-case "with-auto-retry: retries on retryable error then succeeds"
  (define attempt (box 0))
  (define retries (box '()))
  (define result
    (with-auto-retry
     (lambda ()
       (set-box! attempt (add1 (unbox attempt)))
       (if (= (unbox attempt) 1)
           (raise (exn:fail "HTTP 503 service unavailable" (current-continuation-marks)))
           "success"))
     #:max-retries 2
     #:base-delay-ms 10
     #:on-retry (lambda (attempt max-retries delay-ms error-msg)
                  (set-box! retries (cons (list attempt delay-ms error-msg) (unbox retries))))))
  (check-equal? result "success")
  (check-equal? (unbox attempt) 2)
  ;; One retry callback fired
  (check-equal? (length (unbox retries)) 1)
  (check-equal? (first (first (unbox retries))) 1)
  (check-true (string-contains? (third (first (unbox retries))) "503")))

(test-case "with-auto-retry: exhausts retries and re-raises"
  (define attempt (box 0))
  (check-exn exn:fail?
             (lambda ()
               (with-auto-retry (lambda ()
                                  (set-box! attempt (add1 (unbox attempt)))
                                  (raise (exn:fail "HTTP 503 service unavailable"
                                                   (current-continuation-marks))))
                                #:max-retries 2
                                #:base-delay-ms 10)))
  ;; Should have tried 3 times: initial + 2 retries
  (check-equal? (unbox attempt) 3))

(test-case "with-auto-retry: non-retryable error raised immediately"
  (define attempt (box 0))
  (check-exn exn:fail?
             (lambda ()
               (with-auto-retry (lambda ()
                                  (set-box! attempt (add1 (unbox attempt)))
                                  (raise (exn:fail "invalid API key" (current-continuation-marks))))
                                #:max-retries 3
                                #:base-delay-ms 10)))
  ;; Should only try once — non-retryable
  (check-equal? (unbox attempt) 1))

(test-case "with-auto-retry: exponential backoff increases delay"
  (define delays (box '()))
  (check-exn exn:fail?
             (lambda ()
               (with-auto-retry (lambda () (raise (exn:fail "HTTP 503" (current-continuation-marks))))
                                #:max-retries 3
                                #:base-delay-ms 10
                                #:on-retry (lambda (attempt max-retries delay-ms error-msg)
                                             (set-box! delays (cons delay-ms (unbox delays)))))))
  ;; Delays should be: 10, 20, 40 (exponential with base 10ms)
  (define sorted-delays (reverse (unbox delays)))
  (check-equal? (length sorted-delays) 3)
  (check-equal? (first sorted-delays) 10)
  (check-equal? (second sorted-delays) 20)
  (check-equal? (third sorted-delays) 40))

(test-case "with-auto-retry: delay capped at max-delay-ms"
  (define delays (box '()))
  (check-exn exn:fail?
             (lambda ()
               (with-auto-retry (lambda () (raise (exn:fail "HTTP 503" (current-continuation-marks))))
                                #:max-retries 5
                                #:base-delay-ms 100
                                #:max-delay-ms 200
                                #:on-retry (lambda (attempt max-retries delay-ms error-msg)
                                             (set-box! delays (cons delay-ms (unbox delays)))))))
  (define sorted-delays (reverse (unbox delays)))
  ;; Delays should be capped at 200: 100, 200, 200, 200, 200
  (for ([d (in-list sorted-delays)])
    (check-true (<= d 200) (format "delay ~a should be <= 200" d))))

;; ============================================================
;; classify-error tests (v0.11.2 Wave 3)
;; ============================================================

(test-case "classify-error: timeout errors"
  (check-equal? (classify-error (exn:fail "connection timed out" (current-continuation-marks)))
                'timeout)
  (check-equal? (classify-error (exn:fail "HTTP read timeout after 30s" (current-continuation-marks)))
                'timeout)
  (check-equal? (classify-error (exn:fail "Connection reset by peer" (current-continuation-marks)))
                'timeout))

(test-case "classify-error: rate-limit errors"
  (check-equal? (classify-error (exn:fail "HTTP 429 too many requests" (current-continuation-marks)))
                'rate-limit)
  (check-equal? (classify-error (exn:fail "Rate limit exceeded" (current-continuation-marks)))
                'rate-limit)
  (check-equal? (classify-error (exn:fail "Quota exceeded for API" (current-continuation-marks)))
                'rate-limit)
  (check-equal? (classify-error (exn:fail "Model overloaded" (current-continuation-marks)))
                'rate-limit))

(test-case "classify-error: auth errors"
  (check-equal? (classify-error (exn:fail "401 Unauthorized" (current-continuation-marks))) 'auth)
  (check-equal? (classify-error (exn:fail "403 Permission denied" (current-continuation-marks)))
                'auth)
  (check-equal? (classify-error (exn:fail "Authentication failed" (current-continuation-marks)))
                'auth))

(test-case "classify-error: context-overflow errors"
  (check-equal? (classify-error (exn:fail "context_length exceeded" (current-continuation-marks)))
                'context-overflow)
  (check-equal? (classify-error (exn:fail "too many tokens in request" (current-continuation-marks)))
                'context-overflow)
  (check-equal? (classify-error (exn:fail "input is too long for model" (current-continuation-marks)))
                'context-overflow)
  (check-equal? (classify-error (exn:fail "exceeds the maximum number of tokens"
                                          (current-continuation-marks)))
                'context-overflow))

(test-case "classify-error: max-iterations"
  (check-equal? (classify-error (exn:fail "max.iterations reached" (current-continuation-marks)))
                'max-iterations))

(test-case "classify-error: generic provider errors"
  (check-equal? (classify-error (exn:fail "Unknown internal error" (current-continuation-marks)))
                'provider-error)
  (check-equal? (classify-error (exn:fail "Something went wrong" (current-continuation-marks)))
                'provider-error))

;; ============================================================
;; timeout-error? predicate tests
;; ============================================================

(test-case "timeout-error?: positive cases"
  (check-true (timeout-error? (exn:fail "timeout" (current-continuation-marks))))
  (check-true (timeout-error? (exn:fail "timed out" (current-continuation-marks))))
  (check-true (timeout-error? (exn:fail "connection reset" (current-continuation-marks))))
  (check-true (timeout-error? (exn:fail "broken pipe" (current-continuation-marks)))))

(test-case "timeout-error?: negative cases"
  (check-false (timeout-error? (exn:fail "rate limit" (current-continuation-marks))))
  (check-false (timeout-error? (exn:fail "internal error" (current-continuation-marks)))))

;; ============================================================
;; Context reduction on timeout retry tests (v0.11.2 Wave 4)
;; ============================================================

(test-case "with-auto-retry: context-reducer called on timeout"
  (define reduction-log (box '()))
  (define call-count (box 0))
  (check-exn exn:fail?
             (lambda ()
               (with-auto-retry
                (lambda ()
                  (set-box! call-count (add1 (unbox call-count)))
                  (raise (exn:fail "HTTP read timeout" (current-continuation-marks))))
                #:max-retries 2
                #:base-delay-ms 1
                #:context-reducer
                (lambda (attempt)
                  (set-box! reduction-log (cons attempt (unbox reduction-log)))
                  ;; Return same thunk (context reduction is tested in iteration.rkt integration)
                  (lambda ()
                    (set-box! call-count (add1 (unbox call-count)))
                    (raise (exn:fail "HTTP read timeout" (current-continuation-marks))))))))
  ;; Context reducer should have been called for timeout errors
  (check-equal? (reverse (unbox reduction-log)) '(1 2)))

(test-case "with-auto-retry: context-reducer NOT called on non-timeout errors"
  (define reduction-log (box '()))
  (check-exn exn:fail?
             (lambda ()
               (with-auto-retry
                (lambda () (raise (exn:fail "HTTP 429 rate limit" (current-continuation-marks))))
                #:max-retries 1
                #:base-delay-ms 1
                #:context-reducer (lambda (attempt)
                                    (set-box! reduction-log (cons attempt (unbox reduction-log)))
                                    (lambda ()
                                      (raise (exn:fail "HTTP 429 rate limit"
                                                       (current-continuation-marks))))))))
  ;; Context reducer should NOT have been called for rate-limit errors
  (check-equal? (unbox reduction-log) '()))
