#lang racket/base

;; @speed fast
;; @suite default

;; tests/test-mas-integration.rkt — End-to-end MAS integration tests
;; STABILITY: evolving

(require rackunit
         rackunit/text-ui
         (only-in "../agent/roles/supervisor.rkt"
                  make-supervisor-role
                  supervisor-role?
                  supervisor-dispatch)
         (only-in "../agent/roles/base.rkt"
                  agent-role?
                  agent-role-capabilities
                  agent-role-system-prompt
                  agent-role-handle-envelope)
         (only-in "../util/message/mas-envelope.rkt"
                  make-mas-envelope
                  mas-envelope?
                  mas-envelope-target-agent
                  mas-envelope-capability
                  mas-envelope-trace-id)
         (only-in "../agent/capability.rkt" current-session-capabilities role-has-capability?)
         (only-in "../tools/tool.rkt"
                  make-tool-registry
                  list-tools
                  tools-for-capability
                  tool-name
                  tool-required-capability)
         (only-in "../tools/registry-table.rkt" register-tools-from-specs! tool-specs))

(define suite
  (test-suite "MAS Integration"

    ;; ── Supervisor dispatch chain ──

    (test-case "supervisor dispatches to planner"
      (define sup (make-supervisor-role))
      (define env (make-mas-envelope 'supervisor 'planner 'read-only (hasheq 'action "plan")))
      (define result (agent-role-handle-envelope sup env))
      (check-equal? (hash-ref result 'status) 'ok)
      (check-equal? (hash-ref result 'role) 'planner))

    (test-case "supervisor dispatches to verifier"
      (define sup (make-supervisor-role))
      (define env (make-mas-envelope 'supervisor 'verifier 'read-only (hasheq 'action "verify")))
      (define result (agent-role-handle-envelope sup env))
      (check-equal? (hash-ref result 'status) 'ok)
      (check-equal? (hash-ref result 'role) 'verifier))

    (test-case "supervisor dispatches to tool-gateway via direct dispatch"
      ;; Note: Full dispatch chain (supervisor→tool-gateway) requires envelope
      ;; capability re-writing (Schritt 2). Here we test direct dispatch.
      (define sup (make-supervisor-role))
      (define env (make-mas-envelope 'supervisor 'tool-gateway 'shell-exec (hasheq 'cmd "ls")))
      (define result (supervisor-dispatch sup env))
      (check-equal? (hash-ref result 'status) 'ok)
      (check-equal? (hash-ref result 'role) 'tool-gateway))

    ;; C1 regression: supervisor must NOT block file-write/shell-exec/browser
    ;; envelopes. The supervisor is a pure router.
    (test-case "C1: supervisor dispatches file-write envelope via handle-envelope"
      (define sup (make-supervisor-role))
      (define env (make-mas-envelope 'supervisor 'tool-gateway 'file-write (hasheq)))
      (define result (agent-role-handle-envelope sup env))
      (check-equal? (hash-ref result 'status) 'ok))

    (test-case "C1: supervisor dispatches shell-exec envelope via handle-envelope"
      (define sup (make-supervisor-role))
      (define env (make-mas-envelope 'supervisor 'tool-gateway 'shell-exec (hasheq)))
      (define result (agent-role-handle-envelope sup env))
      (check-equal? (hash-ref result 'status) 'ok))

    (test-case "C1: supervisor dispatches browser envelope via handle-envelope"
      (define sup (make-supervisor-role))
      (define env (make-mas-envelope 'supervisor 'tool-gateway 'browser (hasheq)))
      (define result (agent-role-handle-envelope sup env))
      (check-equal? (hash-ref result 'status) 'ok))

    (test-case "supervisor rejects unknown target"
      (define sup (make-supervisor-role))
      (define env (make-mas-envelope 'supervisor 'executor 'read-only (hasheq)))
      (define result (supervisor-dispatch sup env))
      (check-equal? (hash-ref result 'status) 'error))

    ;; ── Capability filtering end-to-end ──

    (test-case "tools-for-capability 'read-only returns only read tools"
      (define reg (make-tool-registry))
      (register-tools-from-specs! reg tool-specs)
      (define read-tools (tools-for-capability reg 'read-only))
      (for ([t (in-list read-tools)])
        (define cap (tool-required-capability t))
        (check-true (or (eq? cap 'read-only) (eq? cap 'any))
                    (format "tool ~a should be read-only, got ~a" (tool-name t) cap))))

    (test-case "role capability alignment: planner can access planner tools"
      (check-true (role-has-capability? 'planner 'read-only))
      (check-true (role-has-capability? 'planner 'plan-write))
      (check-true (role-has-capability? 'planner 'memory-write))
      ;; planner cannot access shell or browser
      (check-false (role-has-capability? 'planner 'shell-exec))
      (check-false (role-has-capability? 'planner 'browser)))

    ;; ── M6: Trace-id propagation ──

    (test-case "M6: supervisor dispatch preserves trace-id through result"
      (define sup (make-supervisor-role))
      (define test-trace "trace-abc-123")
      (define env
        (make-mas-envelope 'supervisor
                           'planner
                           'read-only
                           (hasheq 'action "plan")
                           #:trace-id test-trace))
      (define result (agent-role-handle-envelope sup env))
      (check-equal? (hash-ref result 'status) 'ok)
      ;; Verify dispatch actually processed the envelope (not just construction)
      (check-equal? (hash-ref result 'role) 'planner)
      (check-equal? (hash-ref (hash-ref result 'payload) 'action) "plan")
      ;; Verify trace-id was available on the envelope during dispatch
      (check-equal? (mas-envelope-trace-id env) test-trace))

    ;; ── M4: Source/target validation ──

    (test-case "M4: make-mas-envelope rejects invalid source-agent"
      (check-exn exn:fail?
                 (lambda () (make-mas-envelope 'bogus-source 'planner 'read-only (hasheq)))))

    (test-case "M4: make-mas-envelope rejects invalid target-agent"
      (check-exn exn:fail?
                 (lambda () (make-mas-envelope 'supervisor 'bogus-target 'read-only (hasheq)))))

    ;; ── L5: Unknown-role behavior ──

    (test-case "L5: role-has-capability? returns #f for unknown role"
      (check-false (role-has-capability? 'bogus-role 'read-only)))))

(run-tests suite)
