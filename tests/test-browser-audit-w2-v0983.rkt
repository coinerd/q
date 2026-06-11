#lang racket

;; tests/test-browser-audit-w2-v0983.rkt — Tests for Wave 2 audit fixes (v0.98.3)
;; DUP-01: make-reader-body extracted
;; DUP-02: launch-sidecar-process! extracted
;; SEC-05: Heartbeat reads reader-thread from state, not closure

(require rackunit
         racket/async-channel
         (only-in racket/base make-semaphore call-with-semaphore)
         "../browser/adapters/playwright-sidecar.rkt")

;; ---------------------------------------------------------------------------
;; DUP-01: make-reader-body is provided
;; ---------------------------------------------------------------------------

(test-case "DUP-01: make-reader-body exists as a function"
  (check-true (procedure? make-reader-body)))

(test-case "DUP-01: make-reader-body returns a thunk"
  (define state (playwright-sidecar-state #f #f #f (make-hash) #f #f (hasheq) #f #f))
  (check-true (procedure? (make-reader-body state))))

;; ---------------------------------------------------------------------------
;; DUP-02: launch-sidecar-process! is provided
;; ---------------------------------------------------------------------------

(test-case "DUP-02: launch-sidecar-process! exists as a function"
  (check-true (procedure? launch-sidecar-process!)))

;; ---------------------------------------------------------------------------
;; SEC-05: Heartbeat detects fresh reader after restart
;; ---------------------------------------------------------------------------

(test-case "SEC-05: start-heartbeat! works with stale thread reference"
  (define dead-thread (thread (lambda () (void))))
  (sleep 0.1) ; ensure thread finishes
  (define state
    (playwright-sidecar-state #f
                              #f
                              #f
                              (make-hash)
                              dead-thread
                              (make-custodian)
                              (hasheq 'timeout-ms 5000)
                              #f
                              #f))
  ;; start-heartbeat! should read rt from state dynamically, not closure
  (start-heartbeat! state)
  (check-not-false (playwright-sidecar-state-heartbeat-thread state)))
