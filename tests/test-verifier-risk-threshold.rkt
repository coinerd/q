#lang racket/base

;; @speed fast
;; @suite default

;; tests/test-verifier-risk-threshold.rkt
;; v0.99.22 §6.2: Tests for dynamic risk threshold based on wave capability profile.
;;
;; Tests effective-risk-threshold as a pure function, and verifies that
;; execute-verification-gate applies the dynamic threshold during verification.

(require rackunit
         rackunit/text-ui
         json
         "../agent/verification/verifier-gate.rkt"
         "../agent/verification/verifier-core.rkt"
         (only-in "../llm/provider.rkt" make-mock-provider make-provider)
         (only-in "../llm/model.rkt" make-model-response)
         (only-in "../extensions/gsd/session-state.rkt" make-gsd-context)
         (only-in "../extensions/gsd/state-machine.rkt" gsm-ctx-current gsm-ctx-transition!))

;; ── Helpers ──

(define (make-plan-context #:capabilities [caps '()])
  (hasheq 'capabilities-used caps 'files-changed '("a.rkt") 'plan-summary "test" 'wave-name "W1"))

(define (make-verifying-ctx)
  (define ctx (make-gsd-context))
  (gsm-ctx-transition! ctx 'exploring)
  (gsm-ctx-transition! ctx 'plan-written)
  (gsm-ctx-transition! ctx 'executing)
  (gsm-ctx-transition! ctx 'verifying)
  ctx)

;; Mock provider that returns an approve decision with 'low risk level
(define (make-approve-low-risk-provider)
  (make-mock-provider (make-model-response (list (hasheq 'text
                                                         (jsexpr->string (hasheq 'verdict
                                                                                 "approve"
                                                                                 'reason
                                                                                 "OK"
                                                                                 'risk_level
                                                                                 "low"
                                                                                 'requires_human
                                                                                 #f
                                                                                 'artifact_refs
                                                                                 '("a.rkt")
                                                                                 'timestamp
                                                                                 #f))))
                                           (hasheq)
                                           "mock"
                                           'end_turn)))

;; ── Test Suite ──

(define suite
  (test-suite "Verifier Dynamic Risk Threshold (v0.99.22 §6.2)"

    ;; ── effective-risk-threshold — Pure function tests ──

    (test-case "shell-exec → 'low (strictest)"
      (define pc (make-plan-context #:capabilities '(read-only shell-exec)))
      (check-equal? (effective-risk-threshold pc 'high) 'low))

    (test-case "git-write → 'low (strictest)"
      (define pc (make-plan-context #:capabilities '(read-only git-write)))
      (check-equal? (effective-risk-threshold pc 'high) 'low))

    (test-case "file-write → 'medium"
      (define pc (make-plan-context #:capabilities '(read-only file-write)))
      (check-equal? (effective-risk-threshold pc 'high) 'medium))

    (test-case "read-only only → base threshold (defer to config)"
      (define pc (make-plan-context #:capabilities '(read-only)))
      (check-equal? (effective-risk-threshold pc 'high) 'high)
      (check-equal? (effective-risk-threshold pc 'medium) 'medium)
      (check-equal? (effective-risk-threshold pc 'low) 'low))

    (test-case "empty capabilities → base threshold"
      (define pc (make-plan-context #:capabilities '()))
      (check-equal? (effective-risk-threshold pc 'medium) 'medium))

    (test-case "no capabilities key → base threshold (backward compat)"
      (define pc (hasheq 'files-changed '("a.rkt")))
      (check-equal? (effective-risk-threshold pc 'high) 'high))

    (test-case "shell-exec takes precedence over file-write"
      (define pc (make-plan-context #:capabilities '(file-write shell-exec)))
      (check-equal? (effective-risk-threshold pc 'high) 'low))

    ;; ── Integration: execute-verification-gate applies dynamic threshold ──

    (test-case "gate with read-only caps uses base threshold (no behavior change)"
      (parameterize ([current-verifier-enabled #t]
                     [current-verifier-risk-threshold 'high])
        (define ctx (make-verifying-ctx))
        ;; Read-only wave with 1 file → should skip entirely (§6.1)
        ;; So this test verifies §6.1 + §6.2 interaction: read-only skips
        (define pc (make-plan-context #:capabilities '(read-only)))
        (define result (execute-verification-gate ctx pc))
        (check-equal? result 'approved)
        (check-equal? (gsm-ctx-current ctx) 'idle)))

    (test-case "gate with file-write caps uses medium threshold, not skipped"
      (parameterize ([current-verifier-enabled #t]
                     [current-verifier-risk-threshold 'high]
                     [current-verifier-provider (make-approve-low-risk-provider)])
        (define ctx (make-verifying-ctx))
        (define pc (make-plan-context #:capabilities '(file-write)))
        ;; Should NOT skip (file-write is dangerous), should run verifier
        (define result (execute-verification-gate ctx pc))
        (check-equal? result 'approved)
        (check-equal? (gsm-ctx-current ctx) 'idle)))))

(run-tests suite)
