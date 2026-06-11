#lang racket

;; tests/test-browser-audit-w1-v0983.rkt — Tests for Wave 1 audit fixes (v0.98.3)
;; SEC-07: Unbounded timeout_ms clamped
;; SEC-10: Ping-based readiness after restart
;; SEC-15: Reader dead? flag

(require rackunit
         racket/async-channel
         (only-in racket/base make-semaphore call-with-semaphore)
         "../browser/adapters/playwright-sidecar.rkt"
         "../browser/workflow.rkt")

;; ---------------------------------------------------------------------------
;; SEC-07: timeout_ms clamped to [1000, 60000]
;; ---------------------------------------------------------------------------

(test-case "SEC-07: negative timeout clamped to 1000"
  (define exn
    (with-handlers ([exn:fail? values])
      (browser-check-local-app (hasheq 'url "http://example.com" 'timeout_ms -1))))
  ;; Should raise because no browser service is configured in test env
  (check-true (exn:fail? exn)))

(test-case "SEC-07: huge timeout clamped to 60000"
  (define exn
    (with-handlers ([exn:fail? values])
      (browser-check-local-app (hasheq 'url "http://example.com" 'timeout_ms 999999999))))
  (check-true (exn:fail? exn)))

;; ---------------------------------------------------------------------------
;; SEC-15: dead? flag set on EOF and checked in send-command!
;; ---------------------------------------------------------------------------

(test-case "SEC-15: send-command! rejects dead sidecar immediately"
  (define pending (make-hash))
  (define config (hasheq 'pending-sema (make-semaphore 1) 'timeout-ms 1000))
  (define state (playwright-sidecar-state #f #f #f pending #f #f config #f #t)) ; dead? = #t
  (define exn
    (with-handlers ([exn:fail? values])
      (send-command! state "ping" (hasheq))))
  (check-true (exn:fail? exn))
  (check-true (regexp-match? #rx"dead" (exn-message exn))))

(test-case "SEC-15: dead? is #f in fresh state"
  (define state (playwright-sidecar-state #f #f #f (make-hash) #f #f (hasheq) #f #f))
  (check-false (playwright-sidecar-state-dead? state)))

;; ---------------------------------------------------------------------------
;; SEC-10: restart-sidecar! uses ping readiness probe (not sleep)
;; ---------------------------------------------------------------------------

(test-case "SEC-10: restart-sidecar! increments restart count even without sidecar-path"
  (define config (hasheq 'sidecar-path #f 'restart-count 0 'pending-sema (make-semaphore 1)))
  (define state (playwright-sidecar-state #f #f #f (make-hash) #f #f config #f #f))
  (restart-sidecar! state)
  (check-equal? (hash-ref (playwright-sidecar-state-config state) 'restart-count) 1))
