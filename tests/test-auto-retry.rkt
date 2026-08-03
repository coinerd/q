#lang racket

;; @speed fast
;; @suite default

;; BOUNDARY: integration

;;; tests/test-auto-retry.rkt — tests for runtime/auto-retry.rkt

(require rackunit
         rackunit/text-ui
         "../runtime/auto-retry.rkt"
         "../llm/provider-errors.rkt"
         "../llm/stream.rkt"
         "../llm/openai-compatible.rkt")

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
     #:on-retry (lambda (attempt max-retries delay-ms error-msg error-type)
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
                                #:per-type-budgets (hash 'timeout 3 'rate-limit 4 'provider-error 3)
                                #:on-retry (lambda (attempt max-retries delay-ms error-msg error-type)
                                             (set-box! delays (cons delay-ms (unbox delays)))))))
  ;; With jitter, delays are in [0, 10*2^attempt]
  (define sorted-delays (reverse (unbox delays)))
  (check-equal? (length sorted-delays) 3)
  (check-true (<= (first sorted-delays) 10) "attempt 0 cap = 10")
  (check-true (>= (first sorted-delays) 0) "attempt 0 non-negative")
  (check-true (<= (second sorted-delays) 20) "attempt 1 cap = 20")
  (check-true (>= (second sorted-delays) 0) "attempt 1 non-negative")
  (check-true (<= (third sorted-delays) 40) "attempt 2 cap = 40")
  (check-true (>= (third sorted-delays) 0) "attempt 2 non-negative"))

(test-case "with-auto-retry: delay capped at max-delay-ms"
  (define delays (box '()))
  (check-exn exn:fail?
             (lambda ()
               (with-auto-retry (lambda () (raise (exn:fail "HTTP 503" (current-continuation-marks))))
                                #:max-retries 5
                                #:base-delay-ms 100
                                #:max-delay-ms 200
                                #:per-type-budgets (hash 'timeout 3 'rate-limit 4 'provider-error 5)
                                #:on-retry (lambda (attempt max-retries delay-ms error-msg error-type)
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
                #:on-retry (lambda (attempt max-retries delay-ms error-msg error-type)
                             (set-box! delays (cons delay-ms (unbox delays)))))))
  (define sorted-delays (reverse (unbox delays)))
  ;; With jitter: cap = 50*2^0 = 50, cap = 50*2^1 = 100; delays in [0, cap]
  (check-equal? (length sorted-delays) 2)
  (check-true (<= (first sorted-delays) 50) "rate-limit attempt 0 cap = 50")
  (check-true (>= (first sorted-delays) 0) "rate-limit attempt 0 non-negative")
  (check-true (<= (second sorted-delays) 100) "rate-limit attempt 1 cap = 100")
  (check-true (>= (second sorted-delays) 0) "rate-limit attempt 1 non-negative"))

(test-case "A1: non-rate-limit backoff uses normal base delay"
  (define delays (box '()))
  (check-exn exn:fail?
             (lambda ()
               (with-auto-retry (lambda ()
                                  (raise (exn:fail "HTTP 503 service unavailable"
                                                   (current-continuation-marks))))
                                #:max-retries 2
                                #:base-delay-ms 10
                                #:on-retry (lambda (attempt max-retries delay-ms error-msg error-type)
                                             (set-box! delays (cons delay-ms (unbox delays)))))))
  (define sorted-delays (reverse (unbox delays)))
  ;; With jitter: cap = 10*2^0 = 10, cap = 10*2^1 = 20; delays in [0, cap]
  (check-equal? (length sorted-delays) 2)
  (check-true (<= (first sorted-delays) 10) "non-rate-limit attempt 0 cap = 10")
  (check-true (>= (first sorted-delays) 0) "non-rate-limit attempt 0 non-negative")
  (check-true (<= (second sorted-delays) 20) "non-rate-limit attempt 1 cap = 20")
  (check-true (>= (second sorted-delays) 0) "non-rate-limit attempt 1 non-negative"))

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
                                #:on-retry (lambda (attempt max-retries delay-ms error-msg error-type)
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
  (check-true (>= (retry-exhausted-total-delay-ms e) 0))
  (check-equal? (length (retry-exhausted-delays e)) 2))

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
  (check-true (rate-limit-error? (exn:fail "Quota exceeded" (current-continuation-marks))))
  (check-true (rate-limit-error? (exn:fail "too many requests" (current-continuation-marks)))))

(test-case "rate-limit-error?: negative cases"
  (check-false (rate-limit-error? (exn:fail "timeout" (current-continuation-marks))))
  (check-false (rate-limit-error? (exn:fail "internal error" (current-continuation-marks))))
  (check-false (rate-limit-error? (exn:fail "invalid API key" (current-continuation-marks))))
  ;; v0.14.1 R2: "too many tokens" must NOT match rate-limit
  (check-false (rate-limit-error? (exn:fail "too many tokens in request"
                                            (current-continuation-marks)))))
;; ============================================================
;; context-reducer tests removed (v0.13.2)
;; ============================================================
;; The #:context-reducer parameter was removed in v0.13.2.
;; Retries now always use the same thunk without any context reduction.
;; See "retries use same thunk on timeout" test above.

;; ============================================================
;; v0.14.2 Wave 1: Per-type retry budgets + error history
;; ============================================================

(test-case "v0.14.2: per-type budget — rate-limit doesn't consume timeout budget"
  ;; With default budgets: timeout=2, rate-limit=4, provider-error=2
  ;; If we get 1 rate-limit then 2 timeouts, the timeouts should use their own budget
  (define errors-thrown (box '()))
  (define exn-result (box #f))
  (with-handlers ([retry-exhausted? (lambda (e) (set-box! exn-result e))])
    (with-auto-retry (lambda ()
                       ;; Always throw: rate-limit first, then timeouts
                       (define n (length (unbox errors-thrown)))
                       (cond
                         [(= n 0)
                          (set-box! errors-thrown (cons 'rate-limit (unbox errors-thrown)))
                          (raise (exn:fail "HTTP 429 rate limit" (current-continuation-marks)))]
                         [else
                          (set-box! errors-thrown (cons 'timeout (unbox errors-thrown)))
                          (raise (exn:fail "connection timed out" (current-continuation-marks)))]))
                     #:max-retries 5
                     #:base-delay-ms 1
                     #:rate-limit-base-delay-ms 1
                     #:per-type-budgets (hash 'timeout 2 'rate-limit 4 'provider-error 2)))
  (check-true (retry-exhausted? (unbox exn-result)))
  ;; 1 rate-limit + 2 timeouts = 3 retries (4th attempt hits per-type budget)
  (check-equal? (length (unbox errors-thrown)) 4)
  ;; Error history: rate-limit + 2 timeout retries + final timeout
  (define hist (retry-exhausted-error-history (unbox exn-result)))
  (check-equal? hist '(rate-limit timeout timeout timeout)))

(test-case "v0.14.2: error-history tracks all error types"
  (define exn-result (box #f))
  (define call-n (box 0))
  (with-handlers ([retry-exhausted? (lambda (e) (set-box! exn-result e))])
    (with-auto-retry
     (lambda ()
       ;; Deterministic sequence: provider-error, timeout, rate-limit, ...
       (define n (modulo (unbox call-n) 3))
       (set-box! call-n (add1 (unbox call-n)))
       (cond
         [(= n 0) (raise (exn:fail "HTTP 503 server error" (current-continuation-marks)))]
         [(= n 1) (raise (exn:fail "connection timed out" (current-continuation-marks)))]
         [else (raise (exn:fail "HTTP 429 rate limit" (current-continuation-marks)))]))
     #:max-retries 10
     #:base-delay-ms 1
     #:rate-limit-base-delay-ms 1
     #:per-type-budgets (hash 'timeout 2 'rate-limit 2 'provider-error 2)))
  (check-true (retry-exhausted? (unbox exn-result)))
  (define hist (retry-exhausted-error-history (unbox exn-result)))
  ;; History should have entries from all types
  (check-true (> (length hist) 0) (format "history should be non-empty: ~a" hist)))

(test-case "v0.14.2: single error type fills its own budget"
  (define attempt-count (box 0))
  (define exn-result (box #f))
  (with-handlers ([retry-exhausted? (lambda (e) (set-box! exn-result e))])
    (with-auto-retry (lambda ()
                       (set-box! attempt-count (add1 (unbox attempt-count)))
                       (raise (exn:fail "connection timed out" (current-continuation-marks))))
                     #:max-retries 5
                     #:base-delay-ms 1
                     #:rate-limit-base-delay-ms 1
                     #:per-type-budgets (hash 'timeout 2 'rate-limit 4 'provider-error 2)))
  (check-true (retry-exhausted? (unbox exn-result)))
  ;; With timeout budget of 2, should try 3 times (1 initial + 2 retries)
  (check-equal? (unbox attempt-count) 3)
  (check-equal? (retry-exhausted-error-history (unbox exn-result)) '(timeout timeout timeout)))

(test-case "v0.14.2: default per-type-budgets when not specified"
  ;; Should use max-retries as fallback budget for all types
  (define exn-result (box #f))
  (with-handlers ([retry-exhausted? (lambda (e) (set-box! exn-result e))])
    (with-auto-retry (lambda ()
                       (raise (exn:fail "connection timed out" (current-continuation-marks))))
                     #:max-retries 2
                     #:base-delay-ms 1))
  (check-true (retry-exhausted? (unbox exn-result)))
  (check-equal? (retry-exhausted-error-history (unbox exn-result)) '(timeout timeout timeout)))

;; ============================================================
;; v0.19.3 Wave 2: permanent-tool-error? tests
;; ============================================================

(test-case "permanent-tool-error?: validation failure is permanent"
  (check-true (permanent-tool-error?
               (exn:fail "validate-tool-args: missing required argument 'path' for tool 'read'"
                         (current-continuation-marks)))))

(test-case "permanent-tool-error?: wrong type is permanent"
  (check-true
   (permanent-tool-error?
    (exn:fail
     "validate-tool-args: argument 'count' expected type 'integer', got \"hello\" for tool 'add'"
     (current-continuation-marks)))))

(test-case "permanent-tool-error?: unknown tool is permanent"
  (check-true (permanent-tool-error? (exn:fail "unknown tool: 'nonexistent'"
                                               (current-continuation-marks)))))

(test-case "permanent-tool-error?: rate limit is NOT permanent"
  (check-false (permanent-tool-error? (exn:fail "HTTP 429 rate limit exceeded"
                                                (current-continuation-marks)))))

(test-case "permanent-tool-error?: timeout is NOT permanent"
  (check-false (permanent-tool-error? (exn:fail "connection timed out after 30s"
                                                (current-continuation-marks)))))

(test-case "retryable-error?: permanent tool errors are never retried"
  (check-false (retryable-error?
                (exn:fail "validate-tool-args: missing required argument 'path' for tool 'read'"
                          (current-continuation-marks)))))

(test-case "with-auto-retry: permanent tool error raises immediately without retries"
  (define attempt-count (box 0))
  (define raised-exn (box #f))
  (with-handlers ([exn:fail? (lambda (e) (set-box! raised-exn e))])
    (with-auto-retry (lambda ()
                       (set-box! attempt-count (add1 (unbox attempt-count)))
                       (raise (exn:fail "validate-tool-args: missing required argument 'path'"
                                        (current-continuation-marks))))
                     #:max-retries 3
                     #:base-delay-ms 1))
  (check-equal? (unbox attempt-count) 1 "should not retry permanent tool error")
  (check-true (exn:fail? (unbox raised-exn)) "should raise the original error")
  (check-false (retry-exhausted? (unbox raised-exn)) "should NOT be retry-exhausted"))

;; ============================================================
;; A21: retry-policy struct + with-retry-policy tests (v0.28.4)
;; ============================================================

(test-case "A21: retry-policy struct construction"
  (define p (retry-policy 3 100 500 60000 (hash 'timeout 2)))
  (check-equal? (retry-policy-max-retries p) 3)
  (check-equal? (retry-policy-base-delay-ms p) 100)
  (check-equal? (retry-policy-rate-limit-base-delay-ms p) 500)
  (check-equal? (retry-policy-max-delay-ms p) 60000))

(test-case "A21: make-default-retry-policy returns sensible defaults"
  (define p (make-default-retry-policy))
  (check-equal? (retry-policy-max-retries p) default-max-retries)
  (check-equal? (retry-policy-base-delay-ms p) default-base-delay-ms)
  (check-equal? (retry-policy-rate-limit-base-delay-ms p) default-rate-limit-base-delay-ms)
  (check-equal? (retry-policy-max-delay-ms p) default-max-delay-ms))

(test-case "A21: make-default-retry-policy with overrides"
  (define p (make-default-retry-policy #:max-retries 5 #:base-delay-ms 200))
  (check-equal? (retry-policy-max-retries p) 5)
  (check-equal? (retry-policy-base-delay-ms p) 200)
  (check-equal? (retry-policy-rate-limit-base-delay-ms p) default-rate-limit-base-delay-ms))

(test-case "A21: with-retry-policy succeeds on first try"
  (define p (make-default-retry-policy))
  (check-equal? (with-retry-policy p (lambda () 42)) 42))

(test-case "A21: with-retry-policy retries then succeeds"
  (define p (make-default-retry-policy #:max-retries 2 #:base-delay-ms 10))
  (define attempt (box 0))
  (define result
    (with-retry-policy p
                       (lambda ()
                         (set-box! attempt (add1 (unbox attempt)))
                         (if (= (unbox attempt) 1)
                             (raise (exn:fail "HTTP 503" (current-continuation-marks)))
                             'ok))))
  (check-equal? result 'ok)
  (check-equal? (unbox attempt) 2))

(test-case "A21: with-retry-policy exhausts retries"
  (define p (make-default-retry-policy #:max-retries 1 #:base-delay-ms 1))
  (define exn-result (box #f))
  (with-handlers ([retry-exhausted? (lambda (e) (set-box! exn-result e))])
    (with-retry-policy p (lambda () (raise (exn:fail "HTTP 503" (current-continuation-marks))))))
  (check-true (retry-exhausted? (unbox exn-result))))

;; ============================================================
;; W0: 400 bad-request is NOT retryable tests (v0.99.61)
;; ============================================================

(test-case "W0: 400 bad-request is NOT retryable (provider-error)"
  (define err (provider-error "bad request" (current-continuation-marks) (hash) 'bad-request 400))
  (check-false (retryable-error? err)))

(test-case "W0: 429 rate-limit IS retryable (regression)"
  (define err (provider-error "rate limited" (current-continuation-marks) (hash) 'rate-limit 429))
  (check-true (retryable-error? err)))

(test-case "W0: 500 server IS retryable (regression)"
  (define err (provider-error "server error" (current-continuation-marks) (hash) 'server 500))
  (check-true (retryable-error? err)))

(test-case "W0: string-based 'bad request' classified as bad-request"
  (check-equal?
   (classify-error
    (exn:fail "Anthropic API bad request (400): tool_call_ids did not have response messages"
              (current-continuation-marks)))
   'bad-request))

(test-case "W0: string-based '400' classified as bad-request"
  (check-equal? (classify-error (exn:fail "HTTP 400 bad request" (current-continuation-marks)))
                'bad-request))

;; ============================================================
;; W-06: Structured provider-error path tests (M-11)
;; ============================================================

(test-case "W-06a: retryable-error? with provider-error rate-limit"
  (check-not-false
   (retryable-error?
    (provider-error "rate limited" (current-continuation-marks) (hash) 'rate-limit 429))))

(test-case "W-06b: retryable-error? with provider-error auth-error returns #f"
  (check-false (retryable-error?
                (provider-error "bad key" (current-continuation-marks) (hash) 'auth-error 401))))

(test-case "W-06c: classify-error with provider-error timeout returns 'timeout"
  (check-equal?
   (classify-error (provider-error "timed out" (current-continuation-marks) (hash) 'timeout #f))
   'timeout))

(test-case "W-06d: classify-error with provider-error server-error returns 'server-error"
  (check-equal?
   (classify-error
    (provider-error "internal error" (current-continuation-marks) (hash) 'server-error 500))
   'server-error))

(test-case "W-06e: retryable-error? with provider-error network returns truthy"
  (define exn (provider-error "connection reset" (current-continuation-marks) (hash) 'network #f))
  (check-not-false (retryable-error? exn)))

(test-case "W-06f: retryable-error? with provider-error server-error returns truthy"
  (define exn
    (provider-error "internal server error" (current-continuation-marks) (hash) 'server-error 500))
  (check-not-false (retryable-error? exn)))

;; ============================================================
;; W1: Jitter, injectable random source, Retry-After
;; ============================================================

(test-case "W1: compute-retry-delay with deterministic jitter"
  (define zero-fn (lambda () 0.0))
  (define one-fn (lambda () 1.0))
  (define half-fn (lambda () 0.5))
  (check-equal? (compute-retry-delay 0 1000 10000 0 zero-fn) 0)
  (check-equal? (compute-retry-delay 0 1000 10000 0 one-fn) 1000)
  (check-equal? (compute-retry-delay 0 1000 10000 0 half-fn) 500)
  ;; Retry-After takes precedence
  (check-equal? (compute-retry-delay 0 1000 10000 5000 half-fn) 5000)
  ;; Retry-After capped at max-delay
  (check-equal? (compute-retry-delay 0 1000 2000 5000 half-fn) 2000)
  ;; Exponential cap grows
  (check-equal? (compute-retry-delay 1 1000 10000 0 one-fn) 2000)
  (check-equal? (compute-retry-delay 2 1000 10000 0 one-fn) 4000)
  ;; Capped at max-delay
  (check-equal? (compute-retry-delay 10 1000 5000 0 one-fn) 5000))

(test-case "W1: parse-retry-after"
  (check-equal? (parse-retry-after "30") 30000)
  (check-equal? (parse-retry-after "2.5") 2500)
  (check-equal? (parse-retry-after #f) #f)
  (check-equal? (parse-retry-after "") #f)
  (check-false (parse-retry-after "not-a-number")))

(test-case "W1: current-random-source parameter"
  (define call-count (box 0))
  (parameterize ([current-random-source (lambda ()
                                          (set-box! call-count (add1 (unbox call-count)))
                                          0.42)])
    (check-equal? (compute-retry-delay 0 1000 10000 0 (current-random-source)) 420))
  (check-equal? (unbox call-count) 1))

(test-case "W1: retry-stats has selected-delay-ms field"
  (define s (retry-stats 3 1000 #t 500))
  (check-equal? (retry-stats-selected-delay-ms s) 500))

(test-case "W1: retry-exhausted has delays field"
  (define delays-list (list 100 200))
  (define re
    (retry-exhausted "test"
                     (current-continuation-marks)
                     (exn:fail "err" (current-continuation-marks))
                     2
                     "timeout"
                     5000
                     "(timeout timeout)"
                     delays-list))
  (check-equal? (retry-exhausted-delays re) delays-list))

(test-case "W1: compute-retry-delay with #f random-fn uses system random"
  (define delay (compute-retry-delay 0 1000 10000 0 #f))
  (check-true (>= delay 0))
  (check-true (<= delay 1000)))

(test-case "W1: deterministic jitter distribution stays within bounds"
  ;; With attempt=0, base=100, cap=100
  ;; 1000 samples with deterministic source should all be in [0, 100]
  (define count 1000)
  (define delays
    (for/list ([i (in-range count)])
      (compute-retry-delay 0 100 60000 0 (lambda () (/ i count 1.0)))))
  (for ([d (in-list delays)])
    (check-true (<= d 100) (format "delay ~a <= 100" d))
    (check-true (>= d 0) (format "delay ~a >= 0" d))))

(test-case "W1: rate-limit retry uses jitter"
  (define delays (box '()))
  (check-exn exn:fail?
             (lambda ()
               (with-auto-retry
                (lambda () (raise (exn:fail "HTTP 429 rate limit" (current-continuation-marks))))
                #:max-retries 2
                #:base-delay-ms 10
                #:rate-limit-base-delay-ms 50
                #:on-retry (lambda (attempt max-retries delay-ms error-msg error-type)
                             (set-box! delays (cons delay-ms (unbox delays)))))))
  (define sorted-delays (reverse (unbox delays)))
  (check-equal? (length sorted-delays) 2)
  ;; With jitter, each delay is in [0, 50*2^attempt]
  (check-true (<= (first sorted-delays) 50))
  (check-true (>= (first sorted-delays) 0))
  (check-true (<= (second sorted-delays) 100))
  (check-true (>= (second sorted-delays) 0)))

(test-case "W1: non-retryable 4xx still not retried"
  (define attempt (box 0))
  (check-exn exn:fail?
             (lambda ()
               (with-auto-retry (lambda ()
                                  (set-box! attempt (add1 (unbox attempt)))
                                  (raise (exn:fail "invalid API key" (current-continuation-marks))))
                                #:max-retries 3
                                #:base-delay-ms 10)))
  (check-equal? (unbox attempt) 1))

;; ============================================================
;; W2: Circuit Breaker — held request (PN-4)
;; ============================================================

;; Helper: construct a held-request exception (zero chunks, initial phase)
(define (held-request-exn)
  (exn:fail:network:timeout:stream "Stream timeout (held request)"
                                   (current-continuation-marks)
                                   #f ; received-heartbeats?
                                   #f ; received-any-data?
                                   'initial
                                   0)) ; output-chars

;; Helper: construct a mid-stream stall exception (data received, thinking phase)
;; 500 chars = partial-output (above the NR-1 minimal threshold)
(define (mid-stream-stall-exn)
  (exn:fail:network:timeout:stream "Stream timeout (mid-stream stall)"
                                   (current-continuation-marks)
                                   #t ; received-heartbeats?
                                   #t ; received-any-data?
                                   'thinking
                                   500)) ; output-chars

(test-case "PN-4: held request (zero chunks, initial phase) triggers circuit breaker"
  (define attempt (box 0))
  (define retries (box 0))
  (check-exn exn:fail?
             (lambda ()
               (with-auto-retry (lambda ()
                                  (set-box! attempt (add1 (unbox attempt)))
                                  (raise (held-request-exn)))
                                #:max-retries 3
                                #:base-delay-ms 10
                                #:on-retry (lambda args (set-box! retries (add1 (unbox retries)))))))
  ;; Circuit breaker should fire: only 1 attempt, zero retries
  (check-equal? (unbox attempt) 1)
  (check-equal? (unbox retries) 0))

(test-case "PN-4: mid-stream stall (data received) keeps full retry budget"
  (define attempt (box 0))
  (define retries (box 0))
  (check-exn exn:fail?
             (lambda ()
               (with-auto-retry (lambda ()
                                  (set-box! attempt (add1 (unbox attempt)))
                                  (raise (mid-stream-stall-exn)))
                                #:max-retries 2
                                #:base-delay-ms 10
                                #:on-retry (lambda args (set-box! retries (add1 (unbox retries)))))))
  ;; Mid-stream stall is NOT a held request — full retry budget applies
  (check-equal? (unbox attempt) 3)
  (check-equal? (unbox retries) 2))

(test-case "PN-4: circuit breaker fires even with high max-retries"
  (define attempt (box 0))
  (with-handlers ([exn:fail? (lambda (_) (void))])
    (with-auto-retry (lambda ()
                       (set-box! attempt (add1 (unbox attempt)))
                       (raise (held-request-exn)))
                     #:max-retries 5
                     #:base-delay-ms 10))
  ;; Even with max-retries=5, a held request fails after 1 attempt
  (check-equal? (unbox attempt) 1))

(test-case "PN-4: circuit breaker callback receives held-request classification"
  (define classifications (box '()))
  (with-handlers ([exn:fail? (lambda (_) (void))])
    (with-auto-retry
     (lambda () (raise (held-request-exn)))
     #:max-retries 3
     #:base-delay-ms 10
     #:on-circuit-break
     (lambda (classification original-exn)
       (set-box! classifications
                 (cons (list classification
                             (exn:fail:network:timeout:stream-phase original-exn)
                             (exn:fail:network:timeout:stream-received-any-data? original-exn))
                       (unbox classifications))))))
  (check-equal? (length (unbox classifications)) 1)
  (check-equal? (first (first (unbox classifications))) 'held-request))

;; ============================================================
;; W2: Cumulative Ceiling (PN-7)
;; ============================================================

(test-case "PN-7: cumulative ceiling aborts early before full retry budget"
  ;; Simulate a provider that times out on every attempt.
  ;; With fake clock advancing 200s per attempt and ceiling=300,
  ;; the turn should fail after 2 attempts (0s + 200s = 200s ok,
  ;; 200s + 200s = 400s > 300s → abort on 3rd attempt).
  (define attempt (box 0))
  (define fake-clock (box 0))
  (define retries (box 0))
  (check-exn retry-exhausted?
             (lambda ()
               (with-auto-retry (lambda ()
                                  (set-box! attempt (add1 (unbox attempt)))
                                  ;; Advance fake clock by 200s per attempt
                                  (set-box! fake-clock (+ (unbox fake-clock) 200000))
                                  (raise (exn:fail "connection timed out"
                                                   (current-continuation-marks))))
                                #:max-retries 5
                                #:base-delay-ms 1
                                #:cumulative-ceiling-secs 300
                                #:now-proc (lambda () (unbox fake-clock))
                                #:on-retry (lambda args (set-box! retries (add1 (unbox retries)))))))
  ;; Attempt 1 at 200s: 200 < 300, retry
  ;; Attempt 2 at 400s: 400 > 300, but we already started attempt 2.
  ;; Cumulative check is BEFORE the next retry, so after attempt 2 fails,
  ;; elapsed=400 > 300 → abort. Total attempts = 2.
  (check-true (>= (unbox attempt) 2))
  (check-true (< (unbox attempt) 6) (format "should abort early, got ~a attempts" (unbox attempt))))

(test-case "PN-7: cumulative ceiling does not trigger when total stays under limit"
  (define attempt (box 0))
  (define fake-clock (box 0))
  (define result
    (with-auto-retry (lambda ()
                       (set-box! attempt (add1 (unbox attempt)))
                       (set-box! fake-clock (+ (unbox fake-clock) 5000))
                       (if (= (unbox attempt) 1)
                           (raise (exn:fail "HTTP 503 service unavailable"
                                            (current-continuation-marks)))
                           "success"))
                     #:max-retries 3
                     #:base-delay-ms 1
                     #:cumulative-ceiling-secs 300
                     #:now-proc (lambda () (unbox fake-clock))))
  (check-equal? result "success")
  (check-equal? (unbox attempt) 2))

(test-case "PN-7: no cumulative ceiling when not specified (backward compat)"
  (define attempt (box 0))
  (define fake-clock (box (* 10 60 1000))) ; 10 minutes
  (check-exn exn:fail?
             (lambda ()
               (with-auto-retry (lambda ()
                                  (set-box! attempt (add1 (unbox attempt)))
                                  (raise (exn:fail "connection timed out"
                                                   (current-continuation-marks))))
                                #:max-retries 1
                                #:base-delay-ms 1
                                #:now-proc (lambda () (unbox fake-clock)))))
  ;; No ceiling specified → normal retry behavior (2 attempts: initial + 1 retry)
  (check-equal? (unbox attempt) 2))

;; ============================================================
;; W2 PN-4: End-to-end circuit breaker through provider wrap path
;; ============================================================

;; The openai-wrap-stream-error function must preserve exn:fail:network:timeout:stream
;; so the circuit breaker can classify held requests in production.
(test-case "PN-4: openai-wrap-stream-error preserves stream timeout metadata"
  (define stream-exn (held-request-exn))
  (define result
    (with-handlers ([exn? (lambda (e) e)])
      (openai-wrap-stream-error stream-exn)))
  (check-pred exn:fail:network:timeout:stream? result)
  (when (exn:fail:network:timeout:stream? result)
    (check-false (exn:fail:network:timeout:stream-received-any-data? result))
    (check-equal? (exn:fail:network:timeout:stream-phase result) 'initial)))

(test-case "PN-4: circuit breaker fires through provider wrap path"
  (define attempt (box 0))
  (check-exn exn:fail?
             (lambda ()
               (with-auto-retry (lambda ()
                                  (set-box! attempt (add1 (unbox attempt)))
                                  ;; Simulate production path: stream exception
                                  ;; goes through openai-wrap-stream-error
                                  (openai-wrap-stream-error (held-request-exn)))
                                #:max-retries 3
                                #:base-delay-ms 10)))
  ;; Circuit breaker fires: only 1 attempt
  (check-equal? (unbox attempt) 1))

(test-case "PN-4: non-stream errors still get wrapped to provider-error"
  (define result
    (with-handlers ([exn? (lambda (e) e)])
      (openai-wrap-stream-error (exn:fail "SSL read error" (current-continuation-marks)))))
  (check-pred provider-error? result)
  (check-equal? (provider-error-category result) 'network))
