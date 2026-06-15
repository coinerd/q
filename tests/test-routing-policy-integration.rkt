#lang racket

;; @speed fast  ;; @suite security

;; tests/test-routing-policy-integration.rkt — W0 (v0.99.10) Routing Policy Integration Characterization
;;
;; CHARACTERIZATION TESTS — These document the CURRENT (buggy) behavior.
;; They will be flipped in W4 to assert the CORRECT behavior.
;;
;; Blocker characterized:
;;   M4: Risk-routing policy is pure but not integrated into actual gateway path

(require rackunit
         rackunit/text-ui
         "../agent/roles/tool-gateway.rkt"
         (only-in "../util/message/mas-envelope.rkt"
                  make-mas-envelope
                  mas-envelope?
                  mas-envelope-risk-level))

;; ════════════════════════════════════════════════════════════
;; Helpers
;; ════════════════════════════════════════════════════════════

;; Build a mas-envelope with given risk level for testing routing decisions.
(define (make-test-envelope risk-level)
  (make-mas-envelope 'supervisor 'tool-gateway 'shell-exec "test-payload" #:risk-level risk-level))

(define suite
  (test-suite "Routing Policy Integration (W0 Characterization)"

    ;; ════════════════════════════════════════════════════════════
    ;; M4: Routing policy is pure, not integrated into actual gateway
    ;; ════════════════════════════════════════════════════════════

    (test-case "CHARACTERIZE M4: route-execution-request is a pure function"
      ;; CURRENT: route-execution-request computes routing decisions,
      ;; but the tool-gateway role handler does not call it.
      ;; W4 FIX: integrate routing decision into actual execution path.
      (parameterize ([current-routing-policy 'risk-based])
        (define low-env (make-test-envelope 'low))
        (define high-env (make-test-envelope 'critical))
        (check-equal? (route-execution-request low-env) 'local "low risk routes to local")
        (check-equal? (route-execution-request high-env) 'remote "critical risk routes to remote"))
      ;; CHARACTERIZATION: function exists and works, but is dead code
      ;; — nobody calls it from the gateway handler.

      (parameterize ([current-routing-policy 'local-only])
        (define high-env (make-test-envelope 'critical))
        (check-equal? (route-execution-request high-env)
                      'local
                      "local-only policy: everything stays local")))

    (test-case "CHARACTERIZE M4: gateway handler does not use routing policy"
      ;; CURRENT: default-tool-executor is called directly in the handler.
      ;; It does NOT call route-execution-request or check current-routing-policy.
      ;; CHARACTERIZATION: the handler returns a stub response without
      ;; any routing metadata.
      (define env (make-test-envelope 'high))
      (parameterize ([current-routing-policy 'risk-based])
        (define result ((current-tool-executor) env))
        (check-true (hash? result) "handler returns a hash")
        ;; CHARACTERIZATION: result has NO routing metadata
        ;; W4 will add route/decision info to the result
        (check-false (hash-ref result 'route #f)
                     "CURRENT BUG: no routing metadata in execution result")
        (check-false (hash-ref result 'routing-decision #f)
                     "CURRENT BUG: no routing decision in result")))

    (test-case "CHARACTERIZE M4: routing policy change has no effect on execution"
      ;; Changing the routing policy should affect real execution in W4.
      ;; Currently it has zero effect on the gateway handler output.
      (define env (make-test-envelope 'critical))
      (define result-local
        (parameterize ([current-routing-policy 'local-only])
          ((current-tool-executor) env)))
      (define result-risk
        (parameterize ([current-routing-policy 'risk-based])
          ((current-tool-executor) env)))
      ;; CHARACTERIZATION: both produce identical results — policy is ignored
      (check-equal? result-local
                    result-risk
                    "CURRENT BUG: routing policy has no effect on execution"))))

(run-tests suite)
