#lang racket

;; @speed fast  ;; @suite extensions

;; tests/test-verifier-gate.rkt — W5 v0.99.5 Verification Gate Tests
;;
;; Tests the GSD verification gate:
;;   - Feature flag OFF → no verification, transitions normally
;;   - Feature flag ON + approve → transitions to idle
;;   - Feature flag ON + reject → transitions to executing (rework)
;;   - Feature flag ON + escalate → emits escalation, pauses
;;   - Events emitted correctly (started, completed, escalated)
;;   - should-run-verification-gate? guard

(require rackunit
         rackunit/text-ui
         json
         "../agent/verification/verifier-gate.rkt"
         "../agent/verification/verifier-types.rkt"
         "../agent/verification/verifier-core.rkt"
         "../llm/provider.rkt"
         "../llm/model.rkt"
         "../extensions/gsd/runtime-state-types.rkt"
         (only-in "../extensions/gsd/session-state.rkt"
                  make-gsd-context
                  gsd-ctx-state
                  gsd-ctx-set-state!
                  gsd-ctx-set-event-bus!)
         (only-in "../extensions/gsd/state-machine.rkt"
                  gsm-ctx-current
                  gsm-ctx-transition!
                  gsm-ctx-reset!))

;; ── Helpers ──

;; Create a mock provider that returns a given JSON string.
(define (make-json-response-provider json-string)
  (make-mock-provider
   (make-model-response (list (hasheq 'text json-string)) (hasheq) "mock" 'end_turn)))

;; Create a provider that throws on send.
(define (make-error-provider)
  (make-provider (lambda () "error-provider")
                 (lambda () (hasheq))
                 (lambda (req) (error 'test-provider "simulated failure"))
                 (lambda (req) '())))

;; Valid JSON decision strings.
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
                          "high"
                          'requires_human
                          #t
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

;; Create a plan-context hash for tests.
(define (make-test-plan-context #:files [files '("a.rkt" "b.rkt")])
  (hasheq 'plan-summary
          "Test plan"
          'wave-name
          "W5"
          'files-changed
          files
          'test-summary
          "10/10 pass"
          'diff-excerpt
          ""))

;; Create a fresh GSD context in 'verifying state.
(define (make-verifying-ctx)
  (define ctx (make-gsd-context))
  ;; Walk through: idle → exploring → plan-written → executing → verifying
  (gsm-ctx-transition! ctx 'exploring)
  (gsm-ctx-transition! ctx 'plan-written)
  (gsm-ctx-transition! ctx 'executing)
  (gsm-ctx-transition! ctx 'verifying)
  ctx)

(define (make-idle-ctx)
  (make-gsd-context))

;; ── Test Suite ──

(define suite
  (test-suite "Verifier Gate (W5 v0.99.5)"

    ;; ── gate-result? predicate ──

    (test-case "gate-result? recognizes valid results"
      (check-true (gate-result? 'approved))
      (check-true (gate-result? 'rejected))
      (check-true (gate-result? 'escalated)))

    (test-case "gate-result? rejects invalid results"
      (check-false (gate-result? 'approve))
      (check-false (gate-result? 'unknown))
      (check-false (gate-result? 42)))

    ;; ── extract-plan-context ──

    (test-case "extract-plan-context extracts all fields"
      (define pc (make-test-plan-context))
      (define-values (summary wave files tests diff) (extract-plan-context pc))
      (check-equal? summary "Test plan")
      (check-equal? wave "W5")
      (check-equal? files '("a.rkt" "b.rkt"))
      (check-equal? tests "10/10 pass")
      (check-equal? diff ""))

    (test-case "extract-plan-context uses defaults for missing keys"
      (define pc (hasheq))
      (define-values (summary wave files tests diff) (extract-plan-context pc))
      (check-equal? summary "Plan summary not available")
      (check-equal? wave "unknown-wave")
      (check-equal? files '())
      (check-equal? tests "Test summary not available")
      (check-equal? diff ""))

    ;; ── should-run-verification-gate? ──

    (test-case "should-run-verification-gate? #f when disabled"
      (parameterize ([current-verifier-enabled #f])
        (define ctx (make-verifying-ctx))
        (check-false (should-run-verification-gate? ctx))))

    (test-case "should-run-verification-gate? #f when not in verifying state"
      (parameterize ([current-verifier-enabled #t])
        (define ctx (make-idle-ctx))
        (check-false (should-run-verification-gate? ctx))))

    (test-case "should-run-verification-gate? #t when enabled and verifying"
      (parameterize ([current-verifier-enabled #t])
        (define ctx (make-verifying-ctx))
        (check-true (should-run-verification-gate? ctx))))

    ;; ── Feature Flag OFF ──

    (test-case "gate OFF: no verification, returns approved"
      (parameterize ([current-verifier-enabled #f])
        (define ctx (make-verifying-ctx))
        (define result
          (execute-verification-gate ctx
                                     (make-json-response-provider approve-json)
                                     (make-test-plan-context)))
        (check-equal? result 'approved)
        (check-equal? (gsm-ctx-current ctx) 'idle)))

    (test-case "gate OFF: does not call verifier (provider irrelevant)"
      (parameterize ([current-verifier-enabled #f])
        (define ctx (make-verifying-ctx))
        ;; Even an error provider should succeed when flag is off
        (define result (execute-verification-gate ctx (make-error-provider) (make-test-plan-context)))
        (check-equal? result 'approved)))

    ;; ── Feature Flag ON + Approve ──

    (test-case "gate ON + approve: transitions to idle"
      (parameterize ([current-verifier-enabled #t])
        (define ctx (make-verifying-ctx))
        (define result
          (execute-verification-gate ctx
                                     (make-json-response-provider approve-json)
                                     (make-test-plan-context)))
        (check-equal? result 'approved)
        (check-equal? (gsm-ctx-current ctx) 'idle)))

    ;; ── Feature Flag ON + Reject ──

    (test-case "gate ON + reject: transitions to executing"
      (parameterize ([current-verifier-enabled #t])
        (define ctx (make-verifying-ctx))
        (define result
          (execute-verification-gate ctx
                                     (make-json-response-provider reject-json)
                                     (make-test-plan-context)))
        (check-equal? result 'rejected)
        (check-equal? (gsm-ctx-current ctx) 'executing)))

    ;; ── Feature Flag ON + Escalate ──

    (test-case "gate ON + escalate: stays in verifying (HITL pause)"
      (parameterize ([current-verifier-enabled #t])
        (define ctx (make-verifying-ctx))
        (define result
          (execute-verification-gate ctx
                                     (make-json-response-provider escalate-json)
                                     (make-test-plan-context)))
        (check-equal? result 'escalated)
        ;; Escalation pauses — state stays verifying
        (check-equal? (gsm-ctx-current ctx) 'verifying)))

    ;; ── Feature Flag ON + Provider Error → Escalate ──

    (test-case "gate ON + provider error: escalates (safe default)"
      (parameterize ([current-verifier-enabled #t])
        (define ctx (make-verifying-ctx))
        (define result (execute-verification-gate ctx (make-error-provider) (make-test-plan-context)))
        (check-equal? result 'escalated)
        (check-equal? (gsm-ctx-current ctx) 'verifying)))

    ;; ── Feature Flag ON + Non-JSON → Auto-Reject ──

    (test-case "gate ON + non-JSON response: auto-rejects"
      (parameterize ([current-verifier-enabled #t])
        (define ctx (make-verifying-ctx))
        (define provider
          (make-mock-provider
           (make-model-response (list (hasheq 'text "not json")) (hasheq) "mock" 'end_turn)))
        (define result (execute-verification-gate ctx provider (make-test-plan-context)))
        (check-equal? result 'rejected)
        (check-equal? (gsm-ctx-current ctx) 'executing)))

    ;; ── Events Emitted ──

    (test-case "gate ON + approve: emits started + completed events"
      (parameterize ([current-verifier-enabled #t])
        (define ctx (make-verifying-ctx))
        (define events-collected (box '()))
        (gsd-ctx-set-event-bus! ctx
                                (lambda (event-name wrapped-event)
                                  (set-box! events-collected
                                            (append (unbox events-collected) (list event-name)))))
        (execute-verification-gate ctx
                                   (make-json-response-provider approve-json)
                                   (make-test-plan-context))
        (define events (unbox events-collected))
        (check-true (and (member 'gsd.verification.started events) #t) "started event emitted")
        (check-true (and (member 'gsd.verification.completed events) #t) "completed event emitted")))

    (test-case "gate ON + escalate: emits started + completed + escalated events"
      (parameterize ([current-verifier-enabled #t])
        (define ctx (make-verifying-ctx))
        (define events-collected (box '()))
        (gsd-ctx-set-event-bus! ctx
                                (lambda (event-name wrapped-event)
                                  (set-box! events-collected
                                            (append (unbox events-collected) (list event-name)))))
        (execute-verification-gate ctx
                                   (make-json-response-provider escalate-json)
                                   (make-test-plan-context))
        (define events (unbox events-collected))
        (check-true (and (member 'gsd.verification.started events) #t) "started event emitted")
        (check-true (and (member 'gsd.verification.completed events) #t) "completed event emitted")
        (check-true (and (member 'gsd.verification.escalated events) #t) "escalated event emitted")))

    (test-case "gate OFF: emits no verification events"
      (parameterize ([current-verifier-enabled #f])
        (define ctx (make-verifying-ctx))
        (define events-collected (box '()))
        (gsd-ctx-set-event-bus! ctx
                                (lambda (event-name wrapped-event)
                                  (set-box! events-collected
                                            (append (unbox events-collected) (list event-name)))))
        (execute-verification-gate ctx
                                   (make-json-response-provider approve-json)
                                   (make-test-plan-context))
        (define events (unbox events-collected))
        (check-false (member 'gsd.verification.started events) "no started event")))

    ;; ── State Machine Integration ──

    (test-case "gate ON: rework cycle (reject then approve)"
      (parameterize ([current-verifier-enabled #t])
        (define ctx (make-verifying-ctx))
        ;; First wave: reject → executing
        (execute-verification-gate ctx
                                   (make-json-response-provider reject-json)
                                   (make-test-plan-context))
        (check-equal? (gsm-ctx-current ctx) 'executing)
        ;; Re-enter verifying state
        (gsm-ctx-transition! ctx 'verifying)
        (check-equal? (gsm-ctx-current ctx) 'verifying)
        ;; Second wave: approve → idle
        (execute-verification-gate ctx
                                   (make-json-response-provider approve-json)
                                   (make-test-plan-context))
        (check-equal? (gsm-ctx-current ctx) 'idle)))

    ;; ── Multiple waves ──

    (test-case "gate ON: multiple successive approvals"
      (parameterize ([current-verifier-enabled #t])
        (define ctx (make-verifying-ctx))
        ;; First approval
        (execute-verification-gate ctx
                                   (make-json-response-provider approve-json)
                                   (make-test-plan-context))
        (check-equal? (gsm-ctx-current ctx) 'idle)
        ;; Walk back to verifying for second wave
        (gsm-ctx-transition! ctx 'exploring)
        (gsm-ctx-transition! ctx 'plan-written)
        (gsm-ctx-transition! ctx 'executing)
        (gsm-ctx-transition! ctx 'verifying)
        ;; Second approval
        (execute-verification-gate ctx
                                   (make-json-response-provider approve-json)
                                   (make-test-plan-context))
        (check-equal? (gsm-ctx-current ctx) 'idle)))

    ;; ── Empty files list ──

    (test-case "gate ON + empty files: still works"
      (parameterize ([current-verifier-enabled #t])
        (define ctx (make-verifying-ctx))
        (define result
          (execute-verification-gate ctx
                                     (make-json-response-provider approve-json)
                                     (make-test-plan-context #:files '())))
        (check-equal? result 'approved)))))

(run-tests suite)
