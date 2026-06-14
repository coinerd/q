#lang racket

;; @speed fast  ;; @suite extensions

;; tests/test-verifier-wiring.rkt — W0 (v0.99.6) Verifier Wiring Tests
;;
;; Tests that execute-verification-gate is properly wired into the
;; production flow via current-verifier-provider and that the
;; 2-arg signature works correctly.

(require rackunit
         rackunit/text-ui
         json
         "../agent/verification/verifier-gate.rkt"
         "../agent/verification/verifier-core.rkt"
         "../agent/verification/verifier-types.rkt"
         "../llm/provider.rkt"
         "../llm/model.rkt"
         (only-in "../extensions/gsd/session-state.rkt" make-gsd-context gsd-ctx-set-event-bus!)
         (only-in "../extensions/gsd/state-machine.rkt" gsm-ctx-current gsm-ctx-transition!))

(define (make-json-response-provider json-string)
  (make-mock-provider
   (make-model-response (list (hasheq 'text json-string)) (hasheq) "mock" 'end_turn)))

(define approve-json
  (jsexpr->string (hasheq 'verdict
                          "approve"
                          'reason
                          "ok"
                          'risk_level
                          "low"
                          'requires_human
                          #f
                          'artifact_refs
                          '("a.rkt")
                          'timestamp
                          #f)))

(define (make-verifying-ctx)
  (define ctx (make-gsd-context))
  (gsm-ctx-transition! ctx 'exploring)
  (gsm-ctx-transition! ctx 'plan-written)
  (gsm-ctx-transition! ctx 'executing)
  (gsm-ctx-transition! ctx 'verifying)
  ctx)

(define suite
  (test-suite "Verifier Wiring (W0 v0.99.6)"

    (test-case "current-verifier-provider parameter exists"
      (check-false (current-verifier-provider)))

    (test-case "gate uses current-verifier-provider"
      (define prov (make-json-response-provider approve-json))
      (parameterize ([current-verifier-enabled #t]
                     [current-verifier-provider prov])
        (define ctx (make-verifying-ctx))
        (define result
          (execute-verification-gate ctx
                                     (hasheq 'plan-summary
                                             ""
                                             'wave-name
                                             "W0"
                                             'files-changed
                                             '()
                                             'test-summary
                                             ""
                                             'diff-excerpt
                                             "")))
        (check-equal? result 'approved)))

    (test-case "gate with no provider auto-approves"
      (parameterize ([current-verifier-enabled #t]
                     [current-verifier-provider #f])
        (define ctx (make-verifying-ctx))
        (define result (execute-verification-gate ctx (hasheq)))
        (check-equal? result 'approved)))

    (test-case "gate with flag OFF auto-approves regardless of provider"
      (define prov (make-json-response-provider approve-json))
      (parameterize ([current-verifier-enabled #f]
                     [current-verifier-provider prov])
        (define ctx (make-verifying-ctx))
        (define result (execute-verification-gate ctx (hasheq)))
        (check-equal? result 'approved)))))

(run-tests suite)
