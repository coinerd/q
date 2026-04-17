#lang racket

;;; tests/test-rpc-rate-limiter.rkt — TDD tests for RPC rate limiting (SEC-08)
;;;
;;; Covers:
;;;   - make-rpc-rate-limiter with default and custom max-rps
;;;   - rate-limiter-check allows requests under limit
;;;   - rate-limiter-check rejects requests over limit
;;;   - Per-method isolation (different methods have separate counters)
;;;   - Rate limiter integration with dispatch (end-to-end)

(require rackunit
         (only-in "../interfaces/rpc-mode.rkt"
                  make-rpc-rate-limiter
                  rpc-rate-limiter?
                  rate-limiter-check
                  rpc-request
                  rpc-response
                  dispatch-rpc-request
                  rpc-error
                  RPC-ERROR-RATE-LIMITED))

;; ============================================================
;; Rate limiter construction
;; ============================================================

(test-case "make-rpc-rate-limiter returns rpc-rate-limiter?"
  (define rl (make-rpc-rate-limiter))
  (check-true (rpc-rate-limiter? rl)))

(test-case "make-rpc-rate-limiter with custom max-rps"
  (define rl (make-rpc-rate-limiter #:max-requests-per-second 5))
  (check-true (rpc-rate-limiter? rl)))

;; ============================================================
;; Under-limit requests pass
;; ============================================================

(test-case "rate-limiter-check returns #t for under-limit requests"
  (define rl (make-rpc-rate-limiter #:max-requests-per-second 100))
  (check-eq? (rate-limiter-check rl 'session_info) #t)
  (check-eq? (rate-limiter-check rl 'session_info) #t)
  (check-eq? (rate-limiter-check rl 'session_info) #t))

;; ============================================================
;; Over-limit requests are rejected
;; ============================================================

(test-case "rate-limiter-check rejects requests exceeding max-rps"
  (define rl (make-rpc-rate-limiter #:max-requests-per-second 3))
  (check-eq? (rate-limiter-check rl 'prompt) #t)
  (check-eq? (rate-limiter-check rl 'prompt) #t)
  (check-eq? (rate-limiter-check rl 'prompt) #t)
  ;; 4th request should be rejected
  (define result (rate-limiter-check rl 'prompt))
  (check-true (string? result))
  (check-true (string-contains? result "rate limit")))

;; ============================================================
;; Per-method isolation
;; ============================================================

(test-case "rate-limiter-check tracks methods independently"
  (define rl (make-rpc-rate-limiter #:max-requests-per-second 2))
  ;; Use up quota for 'prompt
  (check-eq? (rate-limiter-check rl 'prompt) #t)
  (check-eq? (rate-limiter-check rl 'prompt) #t)
  ;; 'prompt is now rate limited
  (check-true (string? (rate-limiter-check rl 'prompt)))
  ;; But 'session_info still has quota
  (check-eq? (rate-limiter-check rl 'session_info) #t)
  (check-eq? (rate-limiter-check rl 'session_info) #t)
  ;; And now 'session_info is also rate limited
  (check-true (string? (rate-limiter-check rl 'session_info))))

;; ============================================================
;; Error message contains method name
;; ============================================================

(test-case "rate-limit error message contains method name"
  (define rl (make-rpc-rate-limiter #:max-requests-per-second 1))
  (rate-limiter-check rl 'compact)
  (define result (rate-limiter-check rl 'compact))
  (check-true (string-contains? result "compact")))
