#lang racket

;; @speed fast  ;; @suite security

;; tests/test-tool-gateway-remote-routing.rkt — W3 (v0.99.12) Remote Routing Tests
;;
;; Tests the risk-based routing integration in tool-gateway.rkt:
;; - High/critical risk routes to 'remote when policy is 'risk-based
;; - Remote executor function is called for 'remote decisions
;; - Default remote executor returns fail-closed error
;; - Local execution unchanged when routing to 'local
;; - Routing metadata correctly reflects execution location

(require rackunit
         rackunit/text-ui
         "../agent/roles/tool-gateway.rkt"
         (only-in "../agent/roles/base.rkt" agent-role-handle-envelope)
         (only-in "../util/message/mas-envelope.rkt"
                  make-mas-envelope
                  mas-envelope?
                  mas-envelope-risk-level))

;; ── Helpers ──

(define (make-test-envelope risk-level)
  (make-mas-envelope 'supervisor
                     'tool-gateway
                     'shell-exec
                     (hasheq 'tool-name "bash" 'arguments (hasheq 'command "echo test"))
                     #:risk-level risk-level))

(define (make-stub-remote-executor [response-status 'ok])
  (lambda (envelope)
    (hasheq 'status response-status 'role 'tool-gateway 'result "executed-remotely" 'trace-id #f)))

;; ── Tests ──

(define suite
  (test-suite "Tool Gateway Remote Routing (W3 v0.99.12)"

    ;; ════════════════════════════════════════════════════════════
    ;; routing-decision->execution-route now returns 'remote (not tagged)
    ;; ════════════════════════════════════════════════════════════

    (test-case "routing-decision->execution-route returns 'remote for remote"
      (check-equal? (routing-decision->execution-route 'remote) 'remote))

    (test-case "routing-decision->execution-route returns 'local for local"
      (check-equal? (routing-decision->execution-route 'local) 'local))

    ;; ════════════════════════════════════════════════════════════
    ;; Default remote executor is fail-closed
    ;; ════════════════════════════════════════════════════════════

    (test-case "default-remote-tool-executor returns fail-closed error"
      (define env (make-test-envelope 'high))
      (define result (default-remote-tool-executor env))
      (check-equal? (hash-ref result 'status) 'error)
      (check-true (string-contains? (hash-ref result 'error-message "")
                                    "remote executor not configured")))

    ;; ════════════════════════════════════════════════════════════
    ;; Risk-based routing dispatches to remote executor for high/critical
    ;; ════════════════════════════════════════════════════════════

    (test-case "risk-based routing dispatches high risk to remote executor"
      (define role (make-tool-gateway-role))
      (define env (make-test-envelope 'high))
      (define remote-called? (box #f))
      (parameterize ([current-routing-policy 'risk-based]
                     [current-remote-tool-executor (lambda (envelope)
                                                     (set-box! remote-called? #t)
                                                     (hasheq 'status 'ok 'result "remote-result"))])
        (define result (agent-role-handle-envelope role env))
        (check-true (unbox remote-called?) "remote executor was called")
        (check-equal? (hash-ref result 'routing-decision) 'remote)
        (check-equal? (hash-ref result 'route) 'remote)
        (check-equal? (hash-ref result 'executed-locally?) #f)
        (check-equal? (hash-ref result 'result) "remote-result")))

    (test-case "risk-based routing dispatches critical risk to remote executor"
      (define role (make-tool-gateway-role))
      (define env (make-test-envelope 'critical))
      (parameterize ([current-routing-policy 'risk-based]
                     [current-remote-tool-executor (make-stub-remote-executor)])
        (define result (agent-role-handle-envelope role env))
        (check-equal? (hash-ref result 'routing-decision) 'remote)
        (check-equal? (hash-ref result 'route) 'remote)))

    ;; ════════════════════════════════════════════════════════════
    ;; Risk-based routing still routes low/medium to local
    ;; ════════════════════════════════════════════════════════════

    (test-case "risk-based routing dispatches low risk to local executor"
      (define role (make-tool-gateway-role))
      (define env (make-test-envelope 'low))
      (define remote-called? (box #f))
      (parameterize
          ([current-routing-policy 'risk-based]
           [current-tool-executor (lambda (envelope) (hasheq 'status 'ok 'result "local-result"))]
           [current-remote-tool-executor (lambda (envelope)
                                           (set-box! remote-called? #t)
                                           (hasheq 'status 'ok 'result "should-not-happen"))])
        (define result (agent-role-handle-envelope role env))
        (check-false (unbox remote-called?) "remote executor should NOT be called for low risk")
        (check-equal? (hash-ref result 'routing-decision) 'local)
        (check-equal? (hash-ref result 'route) 'local)
        (check-equal? (hash-ref result 'executed-locally?) #t)))

    ;; ════════════════════════════════════════════════════════════
    ;; Local-only policy never dispatches to remote
    ;; ════════════════════════════════════════════════════════════

    (test-case "local-only policy never dispatches to remote even for critical risk"
      (define role (make-tool-gateway-role))
      (define env (make-test-envelope 'critical))
      (define remote-called? (box #f))
      (parameterize ([current-routing-policy 'local-only]
                     [current-remote-tool-executor (lambda (envelope)
                                                     (set-box! remote-called? #t)
                                                     (hasheq 'status 'ok))])
        (define result (agent-role-handle-envelope role env))
        (check-false (unbox remote-called?) "remote executor should NOT be called in local-only")
        (check-equal? (hash-ref result 'routing-decision) 'local)
        (check-equal? (hash-ref result 'route) 'local)))

    ;; ════════════════════════════════════════════════════════════
    ;; Default state: no remote executor configured → fail-closed
    ;; ════════════════════════════════════════════════════════════

    (test-case "risk-based routing without configured remote executor returns error"
      (define role (make-tool-gateway-role))
      (define env (make-test-envelope 'critical))
      ;; Default current-remote-tool-executor is default-remote-tool-executor
      (parameterize ([current-routing-policy 'risk-based])
        (define result (agent-role-handle-envelope role env))
        (check-equal? (hash-ref result 'status) 'error)
        (check-true (string-contains? (hash-ref result 'error-message "")
                                      "remote executor not configured"))
        (check-equal? (hash-ref result 'route) 'remote)))

    ;; ════════════════════════════════════════════════════════════
    ;; Remote executor error propagates correctly
    ;; ════════════════════════════════════════════════════════════

    (test-case "remote executor error is propagated in routing metadata"
      (define role (make-tool-gateway-role))
      (define env (make-test-envelope 'high))
      (parameterize ([current-routing-policy 'risk-based]
                     [current-remote-tool-executor
                      (lambda (envelope)
                        (hasheq 'status 'error 'error-message "connection refused"))])
        (define result (agent-role-handle-envelope role env))
        (check-equal? (hash-ref result 'status) 'error)
        (check-equal? (hash-ref result 'error-message) "connection refused")
        (check-equal? (hash-ref result 'routing-decision) 'remote)))

    ;; ════════════════════════════════════════════════════════════
    ;; current-remote-tool-executor parameter is thread-local
    ;; ════════════════════════════════════════════════════════════

    (test-case "parameterize scope restores default after exit"
      (check-eq? (current-remote-tool-executor) default-remote-tool-executor)
      (parameterize ([current-remote-tool-executor (lambda (e) (hasheq 'custom #t))])
        (check-false (eq? (current-remote-tool-executor) default-remote-tool-executor)))
      (check-eq? (current-remote-tool-executor) default-remote-tool-executor))))

(run-tests suite)
