#lang racket

;; @speed fast  ;; @suite extensions

;; tests/test-gsd-rework-limit.rkt
;; W1 (v0.99.20): Rework-loop protection — prevent infinite verifier-rework cycles.

(require rackunit
         rackunit/text-ui
         (only-in "../extensions/gsd/session-state.rkt" make-gsd-context)
         (only-in "../extensions/gsd/state-machine.rkt"
                  gsm-ctx-transition!
                  gsm-ctx-reset!
                  gsm-ctx-current
                  ok?
                  ok-to
                  err?
                  err-reason)
         (only-in "../extensions/gsd/session-state.rkt"
                  gsd-ctx-rework-count
                  gsd-ctx-reset-rework-count!))

(define rework-limit-suite
  (test-suite "gsd-rework-limit"

    (test-case "rework counter starts at 0"
      (define ctx (make-gsd-context))
      (check-equal? (gsd-ctx-rework-count ctx) 0))

    (test-case "rework counter increments on verifying→executing transition"
      (define ctx (make-gsd-context))
      ;; Navigate to verifying
      (gsm-ctx-transition! ctx 'exploring)
      (gsm-ctx-transition! ctx 'plan-written)
      (gsm-ctx-transition! ctx 'executing)
      (gsm-ctx-transition! ctx 'verifying)
      ;; Trigger rework
      (define res (gsm-ctx-transition! ctx 'executing #:event 'rework))
      (check-true (ok? res) "rework transition succeeds")
      (check-equal? (gsd-ctx-rework-count ctx) 1))

    (test-case "rework counter enforces limit (default 3)"
      (define ctx (make-gsd-context))
      (gsm-ctx-transition! ctx 'exploring)
      (gsm-ctx-transition! ctx 'plan-written)
      (gsm-ctx-transition! ctx 'executing)
      (gsm-ctx-transition! ctx 'verifying)
      ;; Rework 1
      (gsm-ctx-transition! ctx 'executing #:event 'rework)
      (gsm-ctx-transition! ctx 'verifying)
      ;; Rework 2
      (gsm-ctx-transition! ctx 'executing #:event 'rework)
      (gsm-ctx-transition! ctx 'verifying)
      ;; Rework 3
      (gsm-ctx-transition! ctx 'executing #:event 'rework)
      (gsm-ctx-transition! ctx 'verifying)
      ;; Rework 4 — should be blocked
      (define res (gsm-ctx-transition! ctx 'executing #:event 'rework))
      (check-true (err? res) "4th rework blocked")
      (check-equal? (gsm-ctx-current ctx) 'verifying "stays in verifying"))

    (test-case "reset-rework-count! allows fresh reworks"
      (define ctx (make-gsd-context))
      (gsm-ctx-transition! ctx 'exploring)
      (gsm-ctx-transition! ctx 'plan-written)
      (gsm-ctx-transition! ctx 'executing)
      (gsm-ctx-transition! ctx 'verifying)
      (gsm-ctx-transition! ctx 'executing #:event 'rework)
      (check-equal? (gsd-ctx-rework-count ctx) 1)
      (gsd-ctx-reset-rework-count! ctx)
      (check-equal? (gsd-ctx-rework-count ctx) 0))

    (test-case "fresh execution start resets rework counter"
      (define ctx (make-gsd-context))
      (gsm-ctx-transition! ctx 'exploring)
      (gsm-ctx-transition! ctx 'plan-written)
      (gsm-ctx-transition! ctx 'executing)
      (gsm-ctx-transition! ctx 'verifying)
      (gsm-ctx-transition! ctx 'executing #:event 'rework)
      (check-equal? (gsd-ctx-rework-count ctx) 1)
      ;; Go through the full cycle back to idle, then fresh execute
      (gsm-ctx-transition! ctx 'verifying)
      (gsm-ctx-transition! ctx 'idle)
      (gsm-ctx-transition! ctx 'exploring)
      (gsm-ctx-transition! ctx 'plan-written)
      (gsm-ctx-transition! ctx 'executing)
      ;; Counter should be reset
      (check-equal? (gsd-ctx-rework-count ctx) 0 "counter reset on fresh execution"))))

(run-tests rework-limit-suite)
