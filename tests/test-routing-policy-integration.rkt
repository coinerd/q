#lang racket

;; @speed fast  ;; @suite security

;; tests/test-routing-policy-integration.rkt — W4 (v0.99.10) Routing Policy Integration Remediation
;;
;; FIXED in W4:
;;   M4: Risk-routing policy is integrated into the actual tool-gateway path.

(require rackunit
         rackunit/text-ui
         "../agent/roles/tool-gateway.rkt"
         (only-in "../agent/roles/base.rkt" agent-role-handle-envelope)
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
  (test-suite "Routing Policy Integration (W4 Remediation)"

    ;; ════════════════════════════════════════════════════════════
    ;; M4: Routing policy is integrated into actual gateway execution
    ;; ════════════════════════════════════════════════════════════

    (test-case "M4 baseline: route-execution-request keeps pure policy semantics"
      (parameterize ([current-routing-policy 'risk-based])
        (define low-env (make-test-envelope 'low))
        (define high-env (make-test-envelope 'critical))
        (check-equal? (route-execution-request low-env) 'local "low risk routes to local")
        (check-equal? (route-execution-request high-env) 'remote "critical risk routes to remote"))

      (parameterize ([current-routing-policy 'local-only])
        (define high-env (make-test-envelope 'critical))
        (check-equal? (route-execution-request high-env)
                      'local
                      "local-only policy: everything stays local")))

    (test-case "M4 FIXED: gateway handler annotates local route metadata"
      (define role (make-tool-gateway-role))
      (define env (make-test-envelope 'high))
      (parameterize ([current-routing-policy 'local-only])
        (define result (agent-role-handle-envelope role env))
        (check-true (hash? result) "handler returns a hash")
        (check-equal? (hash-ref result 'routing-decision #f) 'local)
        (check-equal? (hash-ref result 'route #f) 'local)
        (check-equal? (hash-ref result 'executed-locally? #f) #t)))

    (test-case "M4 FIXED: risk-based high/critical route is explicit but executed locally in phase 1"
      (define role (make-tool-gateway-role))
      (define env (make-test-envelope 'critical))
      (parameterize ([current-routing-policy 'risk-based])
        (define result (agent-role-handle-envelope role env))
        (check-true (hash? result) "handler returns a hash")
        (check-equal? (hash-ref result 'routing-decision #f) 'remote)
        (check-equal? (hash-ref result 'route #f) 'remote-tagged-but-executed-local)
        (check-equal? (hash-ref result 'executed-locally? #f) #t)
        (check-equal? (hash-ref result 'risk-level #f) 'critical)))

    (test-case "M4 FIXED: routing policy changes execution metadata"
      (define role (make-tool-gateway-role))
      (define env (make-test-envelope 'critical))
      (define result-local
        (parameterize ([current-routing-policy 'local-only])
          (agent-role-handle-envelope role env)))
      (define result-risk
        (parameterize ([current-routing-policy 'risk-based])
          (agent-role-handle-envelope role env)))
      (check-not-equal? result-local result-risk "routing policy affects gateway result")
      (check-equal? (hash-ref result-local 'route #f) 'local)
      (check-equal? (hash-ref result-risk 'route #f) 'remote-tagged-but-executed-local))

    (test-case "M4 FIXED: injected executor output is preserved and annotated"
      (define role (make-tool-gateway-role))
      (define env (make-test-envelope 'low))
      (parameterize ([current-routing-policy 'risk-based]
                     [current-tool-executor (lambda (envelope)
                                              (check-true (mas-envelope? envelope))
                                              (hasheq 'status 'custom 'payload "from-executor"))])
        (define result (agent-role-handle-envelope role env))
        (check-equal? (hash-ref result 'status #f) 'custom)
        (check-equal? (hash-ref result 'payload #f) "from-executor")
        (check-equal? (hash-ref result 'routing-decision #f) 'local)
        (check-equal? (hash-ref result 'route #f) 'local)))))

(run-tests suite)
