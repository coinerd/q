#lang racket/base

;; tests/test-tool-gateway-bridge.rkt — Bridge and tool-gateway activation tests

(require rackunit
         rackunit/text-ui
         racket/match
         (only-in "../util/capability.rkt" ROLE-CAPABILITIES)
         (only-in "../util/message/mas-envelope.rkt"
                  make-mas-envelope
                  mas-envelope?
                  mas-envelope-payload
                  mas-envelope-trace-id
                  mas-envelope-capability)
         (only-in "../sandbox/ipc-protocol.rkt"
                  ipc-request?
                  ipc-request-tool-name
                  ipc-request-arguments
                  ipc-request-timeout-ms
                  ipc-request-capability
                  ipc-response
                  ipc-response-status
                  ipc-response-content
                  ipc-response-details
                  ipc-response-error-message
                  make-error-response)
         "../sandbox/gateway-bridge.rkt"
         "../agent/roles/tool-gateway.rkt")

;; ── Test Suite ──────────────────────────────────────────────────

(define suite
  (test-suite "Tool-Gateway Bridge + Activation"

    (current-execution-plane-enabled #f)

    ;; ── extract-tool-call ──

    (test-case "extract-tool-call extracts tool-name and arguments"
      (define env
        (make-mas-envelope
         'supervisor
         'tool-gateway
         'shell-exec
         (hasheq 'tool-name "bash" 'arguments (hasheq 'command "echo hi") 'timeout-ms 5000)))
      (define-values (tool-name arguments timeout-ms) (extract-tool-call env))
      (check-equal? tool-name "bash")
      (check-equal? (hash-ref arguments 'command) "echo hi")
      (check-equal? timeout-ms 5000))

    (test-case "extract-tool-call with non-hash payload returns falsy"
      (define env (make-mas-envelope 'supervisor 'tool-gateway 'shell-exec "not a hash"))
      (define-values (tool-name arguments timeout-ms) (extract-tool-call env))
      (check-false tool-name))

    ;; ── envelope->ipc-request ──

    (test-case "envelope->ipc-request builds correct ipc-request"
      (define env
        (make-mas-envelope
         'supervisor
         'tool-gateway
         'shell-exec
         (hasheq 'tool-name "bash" 'arguments (hasheq 'command "echo test") 'timeout-ms 3000)))
      (define req (envelope->ipc-request env))
      (check-true (ipc-request? req))
      (check-equal? (ipc-request-tool-name req) "bash")
      (check-equal? (hash-ref (ipc-request-arguments req) 'command) "echo test")
      (check-equal? (ipc-request-timeout-ms req) 3000)
      (check-equal? (ipc-request-capability req) 'shell-exec))

    (test-case "envelope->ipc-request uses default timeout when missing"
      (define env
        (make-mas-envelope 'supervisor
                           'tool-gateway
                           'shell-exec
                           (hasheq 'tool-name "git" 'arguments (hasheq))))
      (define req (envelope->ipc-request env))
      (check-true (ipc-request? req))
      (check-equal? (ipc-request-tool-name req) "git")
      (check-pred (lambda (ms) (> ms 0)) (ipc-request-timeout-ms req)))

    (test-case "envelope->ipc-request accepts symbol tool-name"
      (define env
        (make-mas-envelope 'supervisor
                           'tool-gateway
                           'shell-exec
                           (hasheq 'tool-name 'bash 'arguments (hasheq))))
      (define req (envelope->ipc-request env))
      (check-equal? (ipc-request-tool-name req) "bash"))

    ;; ── ipc-response→result-hash ──

    (test-case "ipc-response->result-hash translates ok response"
      (define env
        (make-mas-envelope 'supervisor 'tool-gateway 'shell-exec (hasheq 'tool-name "bash")))
      (define resp (ipc-response "req-1" 'ok "hello" (hasheq 'exit-code 0) #f 1))
      (define result (ipc-response->result-hash resp env))
      (check-equal? (hash-ref result 'status) 'ok)
      (check-equal? (hash-ref result 'role) 'tool-gateway)
      (check-equal? (hash-ref result 'result) "hello")
      (check-equal? (mas-envelope-trace-id env) (hash-ref result 'trace-id)))

    (test-case "ipc-response->result-hash translates error response"
      (define env
        (make-mas-envelope 'supervisor 'tool-gateway 'shell-exec (hasheq 'tool-name "bash")))
      (define resp (ipc-response "req-2" 'error #f (hasheq) "command failed" 1))
      (define result (ipc-response->result-hash resp env))
      (check-equal? (hash-ref result 'status) 'error)
      (check-equal? (hash-ref result 'error-message) "command failed"))

    (test-case "ipc-response->result-hash translates timeout response"
      (define env
        (make-mas-envelope 'supervisor 'tool-gateway 'shell-exec (hasheq 'tool-name "bash")))
      (define resp (ipc-response "req-3" 'timeout #f (hasheq) "timed out" 1))
      (define result (ipc-response->result-hash resp env))
      (check-equal? (hash-ref result 'status) 'error)
      (check-equal? (hash-ref result 'error) 'timeout))

    (test-case "ipc-response->result-hash preserves trace-id"
      (define env
        (make-mas-envelope 'supervisor 'tool-gateway 'shell-exec (hasheq 'tool-name "bash")))
      (define resp (ipc-response "req-4" 'ok "ok" (hasheq) #f 1))
      (define result (ipc-response->result-hash resp env))
      (check-equal? (hash-ref result 'trace-id) (mas-envelope-trace-id env)))

    ;; ── Tool-Gateway Backward Compat (execution plane disabled) ──

    (test-case "tool-gateway with execution plane disabled returns stub"
      (parameterize ([current-execution-plane-enabled #f])
        (define role (make-tool-gateway-role))
        (define env
          (make-mas-envelope 'supervisor
                             'tool-gateway
                             'shell-exec
                             (hasheq 'tool-name "bash" 'arguments (hasheq 'command "echo hi"))))
        (define result (agent-role-handle-envelope role env))
        (check-equal? (hash-ref result 'status) 'ok)
        (check-equal? (hash-ref result 'role) 'tool-gateway)
        (check-true (hash-has-key? result 'message))))

    (test-case "tool-gateway rejects non-envelope input"
      (define role (make-tool-gateway-role))
      (define result (agent-role-handle-envelope role "not-an-envelope"))
      (check-equal? (hash-ref result 'status) 'error))

    (test-case "tool-gateway rejects capability not in its grant"
      (define role (make-tool-gateway-role))
      ;; tool-gateway doesn't have 'read-only
      (define env (make-mas-envelope 'supervisor 'tool-gateway 'read-only (hasheq 'tool-name "read")))
      (define result (agent-role-handle-envelope role env))
      (check-equal? (hash-ref result 'status) 'error)
      (check-true (string-contains? (hash-ref result 'message "") "capability denied")))

    (test-case "tool-gateway accepts shell-exec capability"
      (parameterize ([current-execution-plane-enabled #f])
        (define role (make-tool-gateway-role))
        (define env
          (make-mas-envelope 'supervisor 'tool-gateway 'shell-exec (hasheq 'tool-name "bash")))
        (define result (agent-role-handle-envelope role env))
        (check-equal? (hash-ref result 'status) 'ok)))

    ;; ── H2: Feature Flag Toggle (parameterized executor) ──

    (test-case "gateway-start! is a no-op (H2: wiring sets executor)"
      ;; H2: gateway-start! is now a no-op; the wiring layer sets current-tool-executor
      (define before (current-tool-executor))
      (gateway-start!)
      (check-equal? (current-tool-executor) before)
      ;; Reset for other tests
      (gateway-stop!))

    (test-case "gateway-stop! resets executor to default"
      ;; H2: gateway-stop! resets current-tool-executor to default-tool-executor
      (gateway-stop!)
      (check-equal? (current-tool-executor) default-tool-executor))

    ;; ── execute-via-worker with non-envelope ──

    (test-case "execute-via-worker with non-envelope returns error hash"
      (parameterize ([current-execution-plane-enabled #t])
        (define result (execute-via-worker "not-an-envelope"))
        (check-equal? (hash-ref result 'status) 'error)))

    ;; ── Full Integration: execute via real worker ──

    (test-case "full integration: gateway role executes bash via worker"
      ;; H2: Inject executor via parameter instead of feature flag
      (parameterize ([current-tool-executor execute-via-worker])
        (define role (make-tool-gateway-role))
        (define env
          (make-mas-envelope 'supervisor
                             'tool-gateway
                             'shell-exec
                             (hasheq 'tool-name
                                     "bash"
                                     'arguments
                                     (hasheq 'command "echo integration-ok")
                                     'timeout-ms
                                     10000)))
        (define result (agent-role-handle-envelope role env))
        (check-equal? (hash-ref result 'status) 'ok)
        (check-equal? (hash-ref result 'result) "integration-ok")
        (check-equal? (hash-ref result 'role) 'tool-gateway)
        (shutdown-worker!)))))

;; ── Requires for string-contains? ───────────────────────────────

(require racket/string)

;; Need agent-role-handle-envelope
(require (only-in "../agent/roles/base.rkt" agent-role-handle-envelope))

;; ── Run ─────────────────────────────────────────────────────────

(run-tests suite)
