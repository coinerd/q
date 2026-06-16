#lang racket

;; @speed fast  ;; @suite verifier

;; tests/test-verifier-integration.rkt — W7 (v0.99.5) Integration Tests
;;
;; End-to-end integration tests that verify the full wiring path:
;;   config.json settings → settings-query accessors → runtime parameters
;;   → verifier-gate execution → state transitions.

(require rackunit
         rackunit/text-ui
         json
         "../runtime/settings-core.rkt"
         "../runtime/settings-query.rkt"
         "../agent/verification/verifier-core.rkt"
         "../agent/verification/verifier-types.rkt"
         "../agent/verification/verifier-gate.rkt"
         "../llm/provider.rkt"
         "../llm/model.rkt"
         (only-in "../extensions/gsd/session-state.rkt" make-gsd-context gsd-ctx-set-event-bus!)
         (only-in "../extensions/gsd/state-machine.rkt" gsm-ctx-current gsm-ctx-transition!))

;; ── Helper: build a mock provider that returns a given JSON string ──

(define (make-json-response-provider json-string)
  (make-mock-provider
   (make-model-response (list (hasheq 'text json-string)) (hasheq) "mock-model" 'end_turn)))

;; Helper: valid JSON decision strings
(define approve-json
  (jsexpr->string (hasheq 'verdict
                          "approve"
                          'reason
                          "Tests pass"
                          'risk_level
                          "low"
                          'requires_human
                          #f
                          'artifact_refs
                          '("a.rkt")
                          'timestamp
                          #f)))

(define reject-json
  (jsexpr->string (hasheq 'verdict
                          "reject"
                          'reason
                          "Tests fail"
                          'risk_level
                          "medium"
                          'requires_human
                          #f
                          'artifact_refs
                          '("b.rkt")
                          'timestamp
                          #f)))

(define escalate-json
  (jsexpr->string (hasheq 'verdict
                          "escalate"
                          'reason
                          "Uncertain"
                          'risk_level
                          "high"
                          'requires_human
                          #t
                          'artifact_refs
                          '("c.rkt")
                          'timestamp
                          #f)))

;; ── Helper: build a plan context hash for the gate ──

(define (make-plan-context)
  (hasheq 'plan-summary
          "Implement feature X"
          'wave-name
          "W1"
          'files-changed
          '("src/a.rkt" "tests/test-a.rkt")
          'test-summary
          "5 tests pass"
          'diff-excerpt
          "(define (foo) 42)"))

;; ── Helper: create a fresh GSD context in 'verifying state ──

(define (make-verifying-ctx)
  (define ctx (make-gsd-context))
  ;; Walk through state machine to reach verifying
  (gsm-ctx-transition! ctx 'exploring)
  (gsm-ctx-transition! ctx 'plan-written)
  (gsm-ctx-transition! ctx 'executing)
  (gsm-ctx-transition! ctx 'verifying)
  ctx)

;; ── Helper: save/restore parameters around tests ──

(define saved-enabled (box #f))
(define saved-model (box #f))
(define saved-threshold (box #f))

(define (save-params!)
  (set-box! saved-enabled (current-verifier-enabled))
  (set-box! saved-model (current-verifier-model))
  (set-box! saved-threshold (current-verifier-risk-threshold)))

(define (restore-params!)
  (current-verifier-enabled (unbox saved-enabled))
  (current-verifier-model (unbox saved-model))
  (current-verifier-risk-threshold (unbox saved-threshold)))

;; ============================================================
;; Test Suite
;; ============================================================

(define suite
  (test-suite "Verifier Integration (W7 v0.99.5)"

    ;; ── Config Accessor Tests ──

    (test-case "verifier-enabled? returns #t by default (v0.99.15 default-on)"
      (define s (make-minimal-settings))
      (check-true (verifier-enabled? s)))

    (test-case "verifier-enabled? returns #t when mas.verifier.enabled is true"
      (define s
        (make-minimal-settings #:overrides (hasheq 'mas (hasheq 'verifier (hasheq 'enabled #t)))))
      (check-true (verifier-enabled? s)))

    (test-case "verifier-enabled? parses string \"true\""
      (define s
        (make-minimal-settings #:overrides (hasheq 'mas (hasheq 'verifier (hasheq 'enabled "true")))))
      (check-true (verifier-enabled? s)))

    (test-case "verifier-enabled? parses string \"yes\""
      (define s
        (make-minimal-settings #:overrides (hasheq 'mas (hasheq 'verifier (hasheq 'enabled "yes")))))
      (check-true (verifier-enabled? s)))

    (test-case "verifier-enabled? returns #f for \"false\""
      (define s
        (make-minimal-settings #:overrides
                               (hasheq 'mas (hasheq 'verifier (hasheq 'enabled "false")))))
      (check-false (verifier-enabled? s)))

    (test-case "verifier-model returns #f by default"
      (define s (make-minimal-settings))
      (check-false (verifier-model s)))

    (test-case "verifier-model returns configured model"
      (define s
        (make-minimal-settings #:overrides
                               (hasheq 'mas (hasheq 'verifier (hasheq 'model "glm-5.1")))))
      (check-equal? (verifier-model s) "glm-5.1"))

    (test-case "verifier-model coerces symbol to string"
      (define s
        (make-minimal-settings #:overrides (hasheq 'mas (hasheq 'verifier (hasheq 'model 'glm-5.1)))))
      (check-equal? (verifier-model s) "glm-5.1"))

    (test-case "verifier-risk-threshold returns 'high by default (v0.99.15)"
      (define s (make-minimal-settings))
      (check-equal? (verifier-risk-threshold s) 'high))

    (test-case "verifier-risk-threshold parses string \"high\""
      (define s
        (make-minimal-settings #:overrides
                               (hasheq 'mas (hasheq 'verifier (hasheq 'risk-threshold "high")))))
      (check-equal? (verifier-risk-threshold s) 'high))

    (test-case "verifier-risk-threshold parses string \"low\""
      (define s
        (make-minimal-settings #:overrides
                               (hasheq 'mas (hasheq 'verifier (hasheq 'risk-threshold "low")))))
      (check-equal? (verifier-risk-threshold s) 'low))

    (test-case "verifier-risk-threshold coerces invalid to 'high (v0.99.15 default)"
      (define s
        (make-minimal-settings #:overrides
                               (hasheq 'mas (hasheq 'verifier (hasheq 'risk-threshold "extreme")))))
      (check-equal? (verifier-risk-threshold s) 'high))

    (test-case "verifier-risk-threshold accepts symbol directly"
      (define s
        (make-minimal-settings #:overrides
                               (hasheq 'mas (hasheq 'verifier (hasheq 'risk-threshold 'high)))))
      (check-equal? (verifier-risk-threshold s) 'high))

    ;; ── Config → Parameter Wiring Tests ──

    (test-case "config-enabled→parameter wiring (manual simulation)"
      (save-params!)
      (define s
        (make-minimal-settings
         #:overrides
         (hasheq 'mas
                 (hasheq 'verifier (hasheq 'enabled #t 'model "test-model" 'risk-threshold "high")))))
      ;; Wire manually (same logic as run-modes.rkt)
      (current-verifier-enabled (verifier-enabled? s))
      (current-verifier-model (verifier-model s))
      (current-verifier-risk-threshold (verifier-risk-threshold s))

      (check-true (current-verifier-enabled))
      (check-equal? (current-verifier-model) "test-model")
      (check-equal? (current-verifier-risk-threshold) 'high)

      (restore-params!))

    (test-case "config-disabled→parameter wiring (manual simulation)"
      (save-params!)
      ;; Explicitly disable to test the disabled path (default is now #t)
      (define s
        (make-minimal-settings #:overrides (hasheq 'mas (hasheq 'verifier (hasheq 'enabled #f)))))
      (current-verifier-enabled (verifier-enabled? s))
      (current-verifier-model (verifier-model s))
      (current-verifier-risk-threshold (verifier-risk-threshold s))

      (check-false (current-verifier-enabled))
      (check-false (current-verifier-model))
      (check-equal? (current-verifier-risk-threshold) 'high)

      (restore-params!))

    ;; ── Full Integration: Gate with Config-Driven Parameters ──

    (test-case "integration: config enabled + approve JSON → approved"
      (save-params!)
      (current-verifier-enabled #t)
      (current-verifier-model #f)
      (current-verifier-risk-threshold 'medium)

      (define prov (make-json-response-provider approve-json))
      (define ctx (make-verifying-ctx))
      (define plan-ctx (make-plan-context))

      (define result
        (parameterize ([current-verifier-provider prov])
          (execute-verification-gate ctx plan-ctx)))
      (check-equal? result 'approved)
      (check-equal? (gsm-ctx-current ctx) 'idle)

      (restore-params!))

    (test-case "integration: config enabled + reject JSON → rejected"
      (save-params!)
      (current-verifier-enabled #t)
      (current-verifier-model #f)
      (current-verifier-risk-threshold 'medium)

      (define prov (make-json-response-provider reject-json))
      (define ctx (make-verifying-ctx))
      (define plan-ctx (make-plan-context))

      (define result
        (parameterize ([current-verifier-provider prov])
          (execute-verification-gate ctx plan-ctx)))
      (check-equal? result 'rejected)
      (check-equal? (gsm-ctx-current ctx) 'executing)

      (restore-params!))

    (test-case "integration: config enabled + escalate JSON → escalated"
      (save-params!)
      (current-verifier-enabled #t)
      (current-verifier-model #f)
      (current-verifier-risk-threshold 'medium)

      (define prov (make-json-response-provider escalate-json))
      (define ctx (make-verifying-ctx))
      (define plan-ctx (make-plan-context))

      (define result
        (parameterize ([current-verifier-provider prov])
          (execute-verification-gate ctx plan-ctx)))
      (check-equal? result 'escalated)
      ;; Escalation stays in verifying (HITL pause)
      (check-equal? (gsm-ctx-current ctx) 'verifying)

      (restore-params!))

    ;; ── Feature Flag Off → No Verification ──

    (test-case "integration: config disabled → auto-approve, no LLM call"
      (save-params!)
      (current-verifier-enabled #f)

      ;; Provider returns empty content (would fail if called)
      (define prov (make-mock-provider (make-model-response '() (hasheq) "mock" 'error)))
      (define ctx (make-verifying-ctx))
      (define plan-ctx (make-plan-context))

      (define result
        (parameterize ([current-verifier-provider prov])
          (execute-verification-gate ctx plan-ctx)))
      (check-equal? result 'approved)
      (check-equal? (gsm-ctx-current ctx) 'idle)

      (restore-params!))

    ;; ── Error Recovery ──

    (test-case "integration: provider error → escalate (safe default)"
      (save-params!)
      (current-verifier-enabled #t)
      (current-verifier-model #f)
      (current-verifier-risk-threshold 'medium)

      ;; Provider that throws an exception on send
      (define prov
        (make-provider (lambda () "error-provider")
                       (lambda () (hasheq))
                       (lambda (req) (error 'test-provider "simulated failure"))
                       (lambda (req) '())))
      (define ctx (make-verifying-ctx))
      (define plan-ctx (make-plan-context))

      (define result
        (parameterize ([current-verifier-provider prov])
          (execute-verification-gate ctx plan-ctx)))
      ;; Error → escalate (safe default from verifier-core)
      (check-equal? result 'escalated)

      (restore-params!))

    (test-case "integration: non-JSON response → reject (safe default)"
      (save-params!)
      (current-verifier-enabled #t)
      (current-verifier-model #f)
      (current-verifier-risk-threshold 'medium)

      (define prov
        (make-mock-provider
         (make-model-response (list (hasheq 'text "this is not JSON")) (hasheq) "mock" 'end_turn)))
      (define ctx (make-verifying-ctx))
      (define plan-ctx (make-plan-context))

      (define result
        (parameterize ([current-verifier-provider prov])
          (execute-verification-gate ctx plan-ctx)))
      (check-equal? result 'rejected)

      (restore-params!))

    ;; ── Rework Cycle ──

    (test-case "integration: rework cycle — reject then approve"
      (save-params!)
      (current-verifier-enabled #t)
      (current-verifier-model #f)
      (current-verifier-risk-threshold 'medium)

      (define reject-prov (make-json-response-provider reject-json))
      (define approve-prov (make-json-response-provider approve-json))
      (define ctx (make-verifying-ctx))
      (define plan-ctx (make-plan-context))

      ;; First verification: rejected
      (define r1
        (parameterize ([current-verifier-provider reject-prov])
          (execute-verification-gate ctx plan-ctx)))
      (check-equal? r1 'rejected)
      (check-equal? (gsm-ctx-current ctx) 'executing)

      ;; Simulate rework: go back to verifying
      (gsm-ctx-transition! ctx 'verifying)

      ;; Second verification: approved
      (define r2
        (parameterize ([current-verifier-provider approve-prov])
          (execute-verification-gate ctx plan-ctx)))
      (check-equal? r2 'approved)
      (check-equal? (gsm-ctx-current ctx) 'idle)

      (restore-params!))

    ;; ── Risk Threshold Enforcement via Config ──

    (test-case "integration: high risk decision with low threshold → force human review"
      (save-params!)
      (current-verifier-enabled #t)
      (current-verifier-model #f)
      (current-verifier-risk-threshold 'low)

      (define prov
        (make-json-response-provider (jsexpr->string (hasheq 'verdict
                                                             "escalate"
                                                             'reason
                                                             "test"
                                                             'risk_level
                                                             "low"
                                                             'requires_human
                                                             #f
                                                             'artifact_refs
                                                             '()
                                                             'timestamp
                                                             #f))))
      (define decision (run-verification prov "plan" "W1" '("a.rkt") "tests"))
      (check-true (verifier-decision-requires-human? decision)
                  "risk >= threshold should force human review")

      (restore-params!))

    (test-case "integration: low risk decision with high threshold → no forced human review"
      (save-params!)
      (current-verifier-enabled #t)
      (current-verifier-model #f)
      (current-verifier-risk-threshold 'high)

      (define prov
        (make-json-response-provider (jsexpr->string (hasheq 'verdict
                                                             "approve"
                                                             'reason
                                                             "test"
                                                             'risk_level
                                                             "low"
                                                             'requires_human
                                                             #f
                                                             'artifact_refs
                                                             '()
                                                             'timestamp
                                                             #f))))
      (define decision (run-verification prov "plan" "W1" '("a.rkt") "tests"))
      (check-false (verifier-decision-requires-human? decision)
                   "risk < threshold should NOT force human review")

      (restore-params!))

    ;; ── Multiple Successive Approvals ──

    (test-case "integration: multiple successive verification cycles"
      (save-params!)
      (current-verifier-enabled #t)
      (current-verifier-model #f)
      (current-verifier-risk-threshold 'medium)

      (define prov (make-json-response-provider approve-json))

      ;; Cycle 1
      (define ctx1 (make-verifying-ctx))
      (define r1
        (parameterize ([current-verifier-provider prov])
          (execute-verification-gate ctx1 (make-plan-context))))
      (check-equal? r1 'approved)

      ;; Cycle 2 (fresh context)
      (define ctx2 (make-verifying-ctx))
      (define r2
        (parameterize ([current-verifier-provider prov])
          (execute-verification-gate ctx2 (make-plan-context))))
      (check-equal? r2 'approved)

      ;; Cycle 3 (fresh context)
      (define ctx3 (make-verifying-ctx))
      (define r3
        (parameterize ([current-verifier-provider prov])
          (execute-verification-gate ctx3 (make-plan-context))))
      (check-equal? r3 'approved)

      (restore-params!))

    ;; ── Config-Driven Threshold Integration ──

    (test-case "integration: settings threshold value reaches parameters"
      (save-params!)
      (define s
        (make-minimal-settings #:overrides
                               (hasheq 'mas (hasheq 'verifier (hasheq 'risk-threshold "high")))))
      ;; Simulate wire-runtime-parameters! wiring
      (current-verifier-risk-threshold (verifier-risk-threshold s))
      (check-equal? (current-verifier-risk-threshold) 'high)

      (restore-params!))))

(run-tests suite)
