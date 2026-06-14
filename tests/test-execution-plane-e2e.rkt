#lang racket/base

;; tests/test-execution-plane-e2e.rkt
;; End-to-end integration tests for the execution plane.
;;
;; Tests the full dispatch chain: config → parameter → scheduler →
;; bridge → worker → bash → response.
;;
;; These tests spawn real worker subprocesses.

(require rackunit
         rackunit/text-ui
         (only-in racket/string string-contains?)
         (only-in "../util/capability.rkt" ROLE-CAPABILITIES)
         (only-in "../util/message/mas-envelope.rkt" make-mas-envelope mas-envelope-trace-id)
         (only-in "../sandbox/gateway-bridge.rkt"
                  current-execution-plane-enabled
                  current-execution-plane-timeout-ms
                  current-worker-command
                  current-worker-args
                  execute-via-worker
                  ensure-worker!
                  shutdown-worker!)
         (only-in "../agent/roles/tool-gateway.rkt" make-tool-gateway-role current-tool-executor)
         (only-in "../agent/roles/base.rkt" agent-role-handle-envelope agent-role-capabilities)
         (only-in "../tools/tool.rkt"
                  make-tool
                  make-tool-registry
                  register-tool!
                  tool-result?
                  tool-result-content
                  tool-result-is-error?
                  make-success-result
                  make-tool-call)
         (only-in "../tools/tool-struct.rkt" tool-dangerous? tool-externalizable?)
         (only-in "../tools/scheduler.rkt" run-tool-batch scheduler-result-results))

;; ── Test Helpers ────────────────────────────────────────────────

(define call-counter (box 0))

(define (make-dangerous-bash-tool)
  (make-tool "bash"
             "Test bash tool"
             (hasheq 'type "function" 'function (hasheq 'name "bash" 'parameters (hasheq)))
             (lambda (args [ctx #f])
               (set-box! call-counter (add1 (unbox call-counter)))
               (make-success-result (hash-ref args 'command "no command")))
             #:dangerous? #t
             #:required-capability 'shell-exec
             #:externalizable? #t))

(define (make-safe-tool)
  (make-tool "echo"
             "Safe echo tool"
             (hasheq 'type "function" 'function (hasheq 'name "echo" 'parameters (hasheq)))
             (lambda (args [ctx #f]) (make-success-result "echoed"))
             #:dangerous? #f
             #:required-capability 'read-only
             #:externalizable? #t))

;; ── Test Suite ──────────────────────────────────────────────────

(define suite
  (test-suite "Execution Plane E2E"

    ;; Reset state
    (current-execution-plane-enabled #f)
    (shutdown-worker!)

    ;; ── Feature Flag Default ──

    (test-case "feature flag defaults to disabled"
      (check-false (current-execution-plane-enabled)))

    (test-case "timeout parameter has default value"
      (check-equal? (current-execution-plane-timeout-ms) 120000))

    ;; ── Scheduler: flag off → local execution ──

    (test-case "dangerous tool executes locally when flag off"
      (set-box! call-counter 0)
      (parameterize ([current-execution-plane-enabled #f])
        (define reg (make-tool-registry))
        (register-tool! reg (make-dangerous-bash-tool))
        (define result
          (run-tool-batch (list (make-tool-call-from-name "bash" (hasheq 'command "echo local")))
                          reg))
        (define results (scheduler-result-results result))
        (check-equal? (unbox call-counter) 1)
        (check-false (tool-result-is-error? (car results)))))

    ;; ── Scheduler: flag on → worker routing ──

    (test-case "dangerous tool routes through worker when flag on"
      (set-box! call-counter 0)
      (parameterize ([current-execution-plane-enabled #t])
        (define reg (make-tool-registry))
        (register-tool! reg (make-dangerous-bash-tool))
        (define result
          (run-tool-batch (list (make-tool-call-from-name "bash" (hasheq 'command "echo via-worker")))
                          reg))
        (define results (scheduler-result-results result))
        ;; Local execute NOT called when routed through worker
        (check-equal? (unbox call-counter) 0)
        ;; Worker executes the bash command
        (check-false (tool-result-is-error? (car results)))
        (shutdown-worker!)))

    ;; ── Scheduler: safe tool always local ──

    (test-case "safe tool always executes locally"
      (parameterize ([current-execution-plane-enabled #t])
        (define reg (make-tool-registry))
        (register-tool! reg (make-safe-tool))
        (define result (run-tool-batch (list (make-tool-call-from-name "echo" (hasheq))) reg))
        (define results (scheduler-result-results result))
        (check-false (tool-result-is-error? (car results)))
        (check-equal? (tool-result-content (car results)) "echoed")
        (shutdown-worker!)))

    ;; ── Scheduler: mixed batch routing ──

    (test-case "mixed batch: dangerous routed, safe local"
      (set-box! call-counter 0)
      (parameterize ([current-execution-plane-enabled #t])
        (define reg (make-tool-registry))
        (register-tool! reg (make-dangerous-bash-tool))
        (register-tool! reg (make-safe-tool))
        (define result
          (run-tool-batch (list (make-tool-call-from-name "echo" (hasheq))
                                (make-tool-call-from-name "bash" (hasheq 'command "ls")))
                          reg))
        (define results (scheduler-result-results result))
        (check-equal? (length results) 2)
        ;; Both succeed
        (for ([r (in-list results)])
          (check-false (tool-result-is-error? r)))
        ;; Only the dangerous tool's local counter is relevant
        ;; (bash routed, so local counter stays 0)
        (check-equal? (unbox call-counter) 0)
        (shutdown-worker!)))

    ;; ── Tool-Gateway Role: execute-via-worker ──

    (test-case "tool-gateway role dispatches via worker when enabled"
      (parameterize ([current-execution-plane-enabled #t]
                     [current-tool-executor execute-via-worker])
        (define gw (make-tool-gateway-role))
        (define envelope
          (make-mas-envelope
           'supervisor
           'tool-gateway
           'shell-exec
           (hasheq 'tool-name "bash" 'arguments (hasheq 'command "echo gateway-test"))
           #:trace-id "trace-e2e-1"))
        (define result (agent-role-handle-envelope gw envelope))
        (check-equal? (hash-ref result 'status) 'ok)
        (check-equal? (hash-ref result 'trace-id) "trace-e2e-1")
        (shutdown-worker!)))

    (test-case "tool-gateway role returns stub when disabled"
      (parameterize ([current-execution-plane-enabled #f])
        (define gw (make-tool-gateway-role))
        (define envelope
          (make-mas-envelope
           'supervisor
           'tool-gateway
           'shell-exec
           (hasheq 'tool-name "bash" 'arguments (hasheq 'command "echo stub-test"))))
        (define result (agent-role-handle-envelope gw envelope))
        (check-equal? (hash-ref result 'status) 'ok)
        (check-not-false (hash-ref result 'message #f))))

    ;; ── Trace-id propagation ──

    (test-case "trace-id propagates through worker response"
      (parameterize ([current-execution-plane-enabled #t])
        (define envelope
          (make-mas-envelope 'supervisor
                             'tool-gateway
                             'shell-exec
                             (hasheq 'tool-name "bash" 'arguments (hasheq 'command "echo trace-test"))
                             #:trace-id "trace-propagation-test"))
        (define result (execute-via-worker envelope))
        (check-equal? (hash-ref result 'status) 'ok)
        (check-equal? (hash-ref result 'trace-id) "trace-propagation-test")
        (shutdown-worker!)))

    ;; ── Capability enforcement ──

    (test-case "tool-gateway rejects read-only capability"
      (parameterize ([current-execution-plane-enabled #t])
        (define gw (make-tool-gateway-role))
        (define caps (agent-role-capabilities gw))
        ;; tool-gateway has shell-exec, file-write, git-write, network, browser
        ;; but NOT read-only
        (check-not-false (member 'shell-exec caps))
        (check-false (member 'read-only caps))
        (shutdown-worker!)))

    ;; ── Worker lifecycle ──

    (test-case "worker starts lazily on first dangerous call"
      (shutdown-worker!)
      (set-box! call-counter 0)
      (parameterize ([current-execution-plane-enabled #t])
        (define reg (make-tool-registry))
        (register-tool! reg (make-dangerous-bash-tool))
        (define result
          (run-tool-batch (list (make-tool-call-from-name "bash" (hasheq 'command "echo lazy-start")))
                          reg))
        ;; Worker should have been spawned by now
        (check-false (tool-result-is-error? (car (scheduler-result-results result))))
        (shutdown-worker!)))

    (test-case "shutdown kills worker cleanly"
      (parameterize ([current-execution-plane-enabled #t])
        ;; Start a worker
        (define reg (make-tool-registry))
        (register-tool! reg (make-dangerous-bash-tool))
        (run-tool-batch (list (make-tool-call-from-name "bash"
                                                        (hasheq 'command "echo before-shutdown")))
                        reg)
        ;; Now shut it down
        (shutdown-worker!)
        ;; Next request should auto-start a new worker
        (define result2
          (run-tool-batch (list (make-tool-call-from-name "bash"
                                                          (hasheq 'command "echo after-restart")))
                          reg))
        (check-false (tool-result-is-error? (car (scheduler-result-results result2))))
        (shutdown-worker!)))

    ;; ── Multiple sequential requests ──

    (test-case "same worker handles multiple sequential requests"
      (parameterize ([current-execution-plane-enabled #t])
        (define reg (make-tool-registry))
        (register-tool! reg (make-dangerous-bash-tool))
        (for ([i (in-range 3)])
          (define result
            (run-tool-batch
             (list (make-tool-call-from-name "bash" (hasheq 'command (format "echo req-~a" i))))
             reg))
          (check-false (tool-result-is-error? (car (scheduler-result-results result)))))
        (shutdown-worker!)))

    ;; ── Error recovery ──

    (test-case "error from unknown tool returns error result"
      (parameterize ([current-execution-plane-enabled #t])
        (define reg (make-tool-registry))
        (register-tool! reg (make-dangerous-bash-tool))
        (define result (run-tool-batch (list (make-tool-call-from-name "nonexistent" (hasheq))) reg))
        (define results (scheduler-result-results result))
        (check-true (tool-result-is-error? (car results)))
        (shutdown-worker!)))))

;; ── Helper ──────────────────────────────────────────────────────

(define (make-tool-call-from-name name args)
  (make-tool-call (format "tc-~a" (gensym)) name args))

;; ── Run ─────────────────────────────────────────────────────────

(current-execution-plane-enabled #f)
(shutdown-worker!)
(run-tests suite)
(shutdown-worker!)
(current-execution-plane-enabled #f)
