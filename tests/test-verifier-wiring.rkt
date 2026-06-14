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
         (only-in "../extensions/gsd/session-state.rkt"
                  make-gsd-context
                  gsd-ctx-set-event-bus!
                  current-gsd-session-id)
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
        (check-equal? result 'approved)))

    ;; ── W1 (v0.99.6): Session-ID propagation ──

    (test-case "current-gsd-session-id parameter exists with default"
      (check-equal? (current-gsd-session-id) ""))

    (test-case "current-gsd-session-id can be parameterized"
      (parameterize ([current-gsd-session-id "test-session-123"])
        (check-equal? (current-gsd-session-id) "test-session-123")))

    (test-case "gate ON + approve: event carries session-id"
      (define prov (make-json-response-provider approve-json))
      (parameterize ([current-verifier-enabled #t]
                     [current-verifier-provider prov]
                     [current-gsd-session-id "session-abc"])
        (define ctx (make-verifying-ctx))
        (define events-collected (box '()))
        (gsd-ctx-set-event-bus! ctx
                                (lambda (event-name wrapped-event)
                                  (set-box! events-collected
                                            (append (unbox events-collected) (list wrapped-event)))))
        (execute-verification-gate ctx
                                   (hasheq 'plan-summary
                                           ""
                                           'wave-name
                                           "W1"
                                           'files-changed
                                           '("a.rkt")
                                           'test-summary
                                           ""
                                           'diff-excerpt
                                           ""))
        (define events (unbox events-collected))
        (check-true (and (pair? events) #t) "at least one event emitted")
        ;; The started event should carry session-id = "session-abc"
        (define started-event
          (findf (lambda (e)
                   (and (hash? e) (equal? (hash-ref e 'event #f) 'gsd.verification.started)))
                 events))
        (check-true (and started-event #t) "verification-started event found")
        (when started-event
          (define sid (hash-ref (hash-ref started-event 'data (hasheq)) 'session-id #f))
          (check-equal? sid "session-abc"))))))

(run-tests suite)
