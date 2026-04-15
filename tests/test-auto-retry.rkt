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
  (check-true (retryable-error? (exn:fail "HTTP 503 service unavailable" (current-continuation-marks))))
  (check-true (retryable-error? (exn:fail "HTTP 504 gateway timeout" (current-continuation-marks)))))

(test-case "retryable-error?: timeout errors"
  (check-true (retryable-error? (exn:fail "connection timed out" (current-continuation-marks))))
  (check-true (retryable-error? (exn:fail "timeout waiting for response" (current-continuation-marks))))
  (check-true (retryable-error? (exn:fail "network connection reset" (current-continuation-marks)))))

(test-case "retryable-error?: non-retryable errors"
  (check-false (retryable-error? (exn:fail "invalid API key" (current-continuation-marks))))
  (check-false (retryable-error? (exn:fail "model not found" (current-continuation-marks))))
  (check-false (retryable-error? (exn:fail "bad request: missing field" (current-continuation-marks)))))

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
  (check-exn
   exn:fail?
   (lambda ()
     (with-auto-retry
      (lambda ()
        (set-box! attempt (add1 (unbox attempt)))
        (raise (exn:fail "HTTP 503 service unavailable" (current-continuation-marks))))
      #:max-retries 2
      #:base-delay-ms 10)))
  ;; Should have tried 3 times: initial + 2 retries
  (check-equal? (unbox attempt) 3))

(test-case "with-auto-retry: non-retryable error raised immediately"
  (define attempt (box 0))
  (check-exn
   exn:fail?
   (lambda ()
     (with-auto-retry
      (lambda ()
        (set-box! attempt (add1 (unbox attempt)))
        (raise (exn:fail "invalid API key" (current-continuation-marks))))
      #:max-retries 3
      #:base-delay-ms 10)))
  ;; Should only try once — non-retryable
  (check-equal? (unbox attempt) 1))

(test-case "with-auto-retry: exponential backoff increases delay"
  (define delays (box '()))
  (check-exn
   exn:fail?
   (lambda ()
     (with-auto-retry
      (lambda ()
        (raise (exn:fail "HTTP 503" (current-continuation-marks))))
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
  (check-exn
   exn:fail?
   (lambda ()
     (with-auto-retry
      (lambda ()
        (raise (exn:fail "HTTP 503" (current-continuation-marks))))
      #:max-retries 5
      #:base-delay-ms 100
      #:max-delay-ms 200
      #:on-retry (lambda (attempt max-retries delay-ms error-msg)
                   (set-box! delays (cons delay-ms (unbox delays)))))))
  (define sorted-delays (reverse (unbox delays)))
  ;; Delays should be capped at 200: 100, 200, 200, 200, 200
  (for ([d (in-list sorted-delays)])
    (check-true (<= d 200) (format "delay ~a should be <= 200" d))))
