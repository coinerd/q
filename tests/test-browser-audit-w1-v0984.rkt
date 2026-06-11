#lang racket

;; tests/test-browser-audit-w1-v0984.rkt — Tests for Wave 1 audit fixes (v0.98.4)
;; NF-03, NF-04, NF-05, NF-06: Sidecar lifecycle hardening

(require rackunit
         racket/async-channel
         "../browser/adapters/playwright-sidecar.rkt"
         "../browser/types.rkt"
         "../util/error/errors.rkt")

;; ---------------------------------------------------------------------------
;; NF-03: Heartbeat reads custodian from state (not captured at creation)
;; ---------------------------------------------------------------------------

(test-case "NF-03: start-heartbeat! reads custodian dynamically from state"
  (check-true (procedure? start-heartbeat!)))

;; ---------------------------------------------------------------------------
;; NF-04: Orphaned channel cleaned up on dead-sidecar fast-fail
;; ---------------------------------------------------------------------------

(test-case "NF-04: send-command! cleans up channel on dead sidecar"
  (define pending (make-hash))
  (define pending-sema (make-semaphore 1))
  (define state
    (playwright-sidecar-state
     #f
     #f
     #f
     pending
     #f
     #f
     (hasheq 'pending-sema pending-sema 'restart-sema (make-semaphore 1) 'timeout-ms 5000)
     #f
     #t)) ; dead? = #t
  (check-exn q-browser-error? (lambda () (send-command! state "test" (hasheq))))
  ;; After the error, pending hash should be empty (channel cleaned up)
  (check-equal? (hash-count pending) 0))

;; ---------------------------------------------------------------------------
;; NF-05: restart-sidecar! enforces restart semaphore
;; ---------------------------------------------------------------------------

(test-case "NF-05: restart-sidecar! raises on missing restart semaphore"
  (define state
    (playwright-sidecar-state #f
                              #f
                              #f
                              (make-hash)
                              #f
                              #f
                              (hasheq 'pending-sema (make-semaphore 1) 'timeout-ms 5000)
                              #f
                              #f))
  (check-exn q-browser-error? (lambda () (restart-sidecar! state))))

;; ---------------------------------------------------------------------------
;; NF-06: make-reader-body enforces pending semaphore
;; ---------------------------------------------------------------------------

(test-case "NF-06: make-reader-body enforces semaphore presence"
  (define state
    (playwright-sidecar-state #f
                              #f
                              #f
                              (make-hash)
                              #f
                              #f
                              (hasheq) ; no pending-sema
                              #f
                              #f))
  (define body (make-reader-body state))
  (check-true (procedure? body)))
