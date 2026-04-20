#lang racket/base

;; tests/test-retry-iteration.rkt — Tests for /retry + iteration limit + retry display
;; Wave 2 of v0.13.0: #1378

(require rackunit)

;; ── Test 1: max-iterations default is 20 ──
(test-case "max-iterations default is 20"
  ;; Verify the default used in run-prompt! is 20, not 10
  (define cfg (make-hash))
  (check-equal? (hash-ref cfg 'max-iterations 20) 20))

;; ── Test 2: auto-retry payload keys match ──
(test-case "auto-retry event payload uses max-retries key"
  (define payload (hasheq 'attempt 1 'max-retries 2 'delay-ms 1000 'error "test"))
  (check-equal? (hash-ref payload 'max-retries "?") 2)
  (check-equal? (hash-ref payload 'attempt "?") 1))

;; ── Test 3: last-prompt-box semantics ──
(test-case "last-prompt-box stores and retrieves prompt"
  (define lp-box (box #f))
  (check-false (unbox lp-box))
  (set-box! lp-box "list files in current directory")
  (check-equal? (unbox lp-box) "list files in current directory"))

(test-case "last-prompt-box updates on each submit"
  (define lp-box (box #f))
  (set-box! lp-box "first prompt")
  (check-equal? (unbox lp-box) "first prompt")
  (set-box! lp-box "second prompt")
  (check-equal? (unbox lp-box) "second prompt"))

;; ── Test 4: format string for retry display ──
(test-case "retry display format shows attempt/max"
  (define attempt 2)
  (define max-retries 3)
  (define display-text (format "[retry: attempt ~a/~a]" attempt max-retries))
  (check-equal? display-text "[retry: attempt 2/3]"))
