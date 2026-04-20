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
;; No context reduction on retry (v0.13.2)
;; ============================================================

(test-case "with-auto-retry: retries use same thunk on timeout (no context reduction)"
  ;; v0.13.2: #:context-reducer removed. Retries always use original thunk.
  (define call-count (box 0))
  (define result
    (with-auto-retry (lambda ()
                       (set-box! call-count (add1 (unbox call-count)))
                       (if (= (unbox call-count) 1)
                           (raise (exn:fail "timeout" (current-continuation-marks)))
                           'success))
                     #:max-retries 1
                     #:base-delay-ms 1))
  (check-equal? result 'success)
  (check-equal? (unbox call-count) 2))

;; ============================================================
;; A1: Rate-limit-specific backoff tests (v0.12.2)
;; ============================================================

(test-case "A1: rate-limit backoff uses 10s base delay"
  (define delays (box '()))
  (check-exn exn:fail?
             (lambda ()
               (with-auto-retry
                (lambda () (raise (exn:fail "HTTP 429 rate limit" (current-continuation-marks))))
                #:max-retries 2
                #:base-delay-ms 10
                #:rate-limit-base-delay-ms 50
                #:on-retry (lambda (attempt max-retries delay-ms error-msg)
                             (set-box! delays (cons delay-ms (unbox delays)))))))
  (define sorted-delays (reverse (unbox delays)))
  ;; Should use rate-limit base (50ms): 50, 100
  (check-equal? (length sorted-delays) 2)
  (check-equal? (first sorted-delays) 50)
  (check-equal? (second sorted-delays) 100))

(test-case "A1: non-rate-limit backoff uses normal base delay"
  (define delays (box '()))
  (check-exn exn:fail?
             (lambda ()
               (with-auto-retry (lambda ()
                                  (raise (exn:fail "HTTP 503 service unavailable"
                                                   (current-continuation-marks))))
                                #:max-retries 2
                                #:base-delay-ms 10
                                #:on-retry (lambda (attempt max-retries delay-ms error-msg)
                                             (set-box! delays (cons delay-ms (unbox delays)))))))
  (define sorted-delays (reverse (unbox delays)))
  ;; Should use normal base (10ms): 10, 20
  (check-equal? (length sorted-delays) 2)
  (check-equal? (first sorted-delays) 10)
  (check-equal? (second sorted-delays) 20))

(test-case "A1: rate-limit backoff capped at max-delay-ms"
  (define delays (box '()))
  (check-exn exn:fail?
             (lambda ()
               (with-auto-retry (lambda ()
                                  (raise (exn:fail "HTTP 429 too many requests"
                                                   (current-continuation-marks))))
                                #:max-retries 4
                                #:base-delay-ms 10
                                #:rate-limit-base-delay-ms 50
                                #:max-delay-ms 150
                                #:on-retry (lambda (attempt max-retries delay-ms error-msg)
                                             (set-box! delays (cons delay-ms (unbox delays)))))))
  ;; 50, 100, 150(cap), 150(cap)
  (for ([d (in-list (reverse (unbox delays)))])
    (check-true (<= d 150) (format "delay ~a should be <= 150" d))))

;; ============================================================
;; A3: Retry-exhausted struct tests (v0.12.2)
;; ============================================================

(test-case "A3: retry-exhausted raised after retries exhausted"
  (define exn-result (box #f))
  (with-handlers ([retry-exhausted? (lambda (e) (set-box! exn-result e))])
    (with-auto-retry (lambda () (raise (exn:fail "HTTP 503" (current-continuation-marks))))
                     #:max-retries 2
                     #:base-delay-ms 1))
  (define e (unbox exn-result))
  (check-true (retry-exhausted? e))
  (check-equal? (retry-exhausted-attempts e) 2)
  (check-equal? (retry-exhausted-last-error-type e) 'provider-error)
  (check-true (> (retry-exhausted-total-delay-ms e) 0)))

(test-case "A3: retry-exhausted has rate-limit type for 429"
  (define exn-result (box #f))
  (with-handlers ([retry-exhausted? (lambda (e) (set-box! exn-result e))])
    (with-auto-retry (lambda () (raise (exn:fail "HTTP 429 rate limit" (current-continuation-marks))))
                     #:max-retries 1
                     #:base-delay-ms 1))
  (define e (unbox exn-result))
  (check-true (retry-exhausted? e))
  (check-equal? (retry-exhausted-last-error-type e) 'rate-limit))

(test-case "A3: non-retryable error is NOT wrapped in retry-exhausted"
  (check-exn exn:fail?
             (lambda ()
               (with-auto-retry (lambda ()
                                  (raise (exn:fail "invalid API key" (current-continuation-marks))))
                                #:max-retries 2
                                #:base-delay-ms 1))
             "non-retryable should be plain exn:fail"))

(test-case "A3: retry-exhausted wraps original exception"
  (define exn-result (box #f))
  (with-handlers ([retry-exhausted? (lambda (e) (set-box! exn-result e))])
    (with-auto-retry (lambda () (raise (exn:fail "timeout" (current-continuation-marks))))
                     #:max-retries 1
                     #:base-delay-ms 1))
  (define e (unbox exn-result))
  (check-true (retry-exhausted? e))
  (check-true (string-contains? (exn-message e) "after 1 retries")))

;; ============================================================
;; rate-limit-error? predicate tests (v0.12.2)
;; ============================================================

(test-case "rate-limit-error?: positive cases"
  (check-true (rate-limit-error? (exn:fail "HTTP 429" (current-continuation-marks))))
  (check-true (rate-limit-error? (exn:fail "rate limit exceeded" (current-continuation-marks))))
  (check-true (rate-limit-error? (exn:fail "Quota exceeded" (current-continuation-marks)))))

(test-case "rate-limit-error?: negative cases"
  (check-false (rate-limit-error? (exn:fail "timeout" (current-continuation-marks))))
  (check-false (rate-limit-error? (exn:fail "internal error" (current-continuation-marks))))
  (check-false (rate-limit-error? (exn:fail "invalid API key" (current-continuation-marks)))))

;; ============================================================
;; context-reducer tests removed (v0.13.2)
;; ============================================================
;; The #:context-reducer parameter was removed in v0.13.2.
;; Retries now always use the same thunk without any context reduction.
;; See "retries use same thunk on timeout" test above.
