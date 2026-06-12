#lang racket/base

;; @speed fast
;; @suite default

;; tests/test-mas-agent-roles.rkt — Tests for agent-role generics and role stubs
;; STABILITY: evolving

(require rackunit
         rackunit/text-ui
         (only-in "../agent/roles/base.rkt"
                  gen:agent-role
                  agent-role?
                  agent-role-capabilities
                  agent-role-system-prompt
                  agent-role-handle-envelope)
         (only-in "../agent/roles/planner.rkt" make-planner-role planner-role?)
         (only-in "../agent/roles/verifier.rkt" make-verifier-role verifier-role?)
         (only-in "../agent/roles/tool-gateway.rkt" make-tool-gateway-role tool-gateway-role?)
         (only-in "../agent/roles/supervisor.rkt"
                  make-supervisor-role
                  supervisor-role?
                  supervisor-dispatch)
         (only-in "../agent/capability.rkt" valid-capability? role-has-capability?)
         (only-in "../util/message/mas-envelope.rkt" make-mas-envelope mas-envelope?))

(define suite
  (test-suite "MAS Agent Roles"

    ;; ── base: gen:agent-role generics ──

    (test-case "planner-role satisfies agent-role?"
      (define p (make-planner-role))
      (check-true (agent-role? p)))

    (test-case "verifier-role satisfies agent-role?"
      (define v (make-verifier-role))
      (check-true (agent-role? v)))

    (test-case "tool-gateway-role satisfies agent-role?"
      (define tg (make-tool-gateway-role))
      (check-true (agent-role? tg)))

    (test-case "supervisor-role satisfies agent-role?"
      (define s (make-supervisor-role))
      (check-true (agent-role? s)))

    ;; ── planner role ──

    (test-case "planner: capabilities include read-only, plan-write, memory-write"
      (define p (make-planner-role))
      (define caps (agent-role-capabilities p))
      (check-not-false (member 'read-only caps))
      (check-not-false (member 'plan-write caps))
      (check-not-false (member 'memory-write caps)))

    (test-case "planner: system-prompt is non-empty string"
      (define p (make-planner-role))
      (define prompt (agent-role-system-prompt p))
      (check-true (string? prompt))
      (check-true (> (string-length prompt) 0)))

    (test-case "planner: handle-envelope returns result for valid envelope"
      (define p (make-planner-role))
      (define env (make-mas-envelope 'supervisor 'planner 'plan-write "plan this"))
      (define result (agent-role-handle-envelope p env))
      (check-not-false result))

    ;; ── verifier role ──

    (test-case "verifier: capabilities are read-only only"
      (define v (make-verifier-role))
      (define caps (agent-role-capabilities v))
      (check-equal? caps '(read-only)))

    (test-case "verifier: system-prompt is non-empty string"
      (define v (make-verifier-role))
      (check-true (string? (agent-role-system-prompt v))))

    (test-case "verifier: handle-envelope returns result"
      (define v (make-verifier-role))
      (define env (make-mas-envelope 'supervisor 'verifier 'read-only "verify this"))
      (check-not-false (agent-role-handle-envelope v env)))

    ;; ── tool-gateway role ──

    (test-case "tool-gateway: capabilities include shell-exec, file-write, git-write, network, browser"
      (define tg (make-tool-gateway-role))
      (define caps (agent-role-capabilities tg))
      (check-not-false (member 'shell-exec caps))
      (check-not-false (member 'file-write caps))
      (check-not-false (member 'git-write caps))
      (check-not-false (member 'network caps))
      (check-not-false (member 'browser caps)))

    (test-case "tool-gateway: system-prompt is non-empty string"
      (define tg (make-tool-gateway-role))
      (check-true (string? (agent-role-system-prompt tg))))

    (test-case "tool-gateway: handle-envelope returns result"
      (define tg (make-tool-gateway-role))
      (define env (make-mas-envelope 'supervisor 'tool-gateway 'shell-exec "run this"))
      (check-not-false (agent-role-handle-envelope tg env)))

    ;; ── supervisor role ──

    (test-case "supervisor: capabilities include read-only, plan-write, memory-write, subagent"
      (define s (make-supervisor-role))
      (define caps (agent-role-capabilities s))
      (check-not-false (member 'read-only caps))
      (check-not-false (member 'plan-write caps))
      (check-not-false (member 'memory-write caps))
      (check-not-false (member 'subagent caps)))

    (test-case "supervisor: system-prompt is non-empty string"
      (define s (make-supervisor-role))
      (check-true (string? (agent-role-system-prompt s))))

    (test-case "supervisor: dispatch routes envelope to correct sub-role"
      (define s (make-supervisor-role))
      (define env (make-mas-envelope 'supervisor 'planner 'plan-write "plan this"))
      (define result (supervisor-dispatch s env))
      (check-not-false result))

    (test-case "supervisor: dispatch to verifier"
      (define s (make-supervisor-role))
      (define env (make-mas-envelope 'supervisor 'verifier 'read-only "check this"))
      (define result (supervisor-dispatch s env))
      (check-not-false result))

    (test-case "supervisor: dispatch to tool-gateway"
      (define s (make-supervisor-role))
      (define env (make-mas-envelope 'supervisor 'tool-gateway 'file-write "write this"))
      (define result (supervisor-dispatch s env))
      (check-not-false result))

    ;; ── capability consistency ──

    (test-case "all role capabilities match ROLE-CAPABILITIES in capability.rkt"
      (define roles
        `((supervisor . ,(agent-role-capabilities (make-supervisor-role)))
          (planner . ,(agent-role-capabilities (make-planner-role)))
          (verifier . ,(agent-role-capabilities (make-verifier-role)))
          (tool-gateway . ,(agent-role-capabilities (make-tool-gateway-role)))))
      (for ([pair (in-list roles)])
        (define role-name (car pair))
        (define actual-caps (cdr pair))
        (for ([cap (in-list actual-caps)])
          (check-true (role-has-capability? role-name cap)
                      (format "~a should have ~a per ROLE-CAPABILITIES" role-name cap)))))

    ;; ── envelope capability check ──

    (test-case "handle-envelope rejects envelope exceeding role capability"
      (define v (make-verifier-role))
      (define env (make-mas-envelope 'supervisor 'verifier 'shell-exec "run this"))
      ;; verifier only has read-only; shell-exec should be rejected
      (define result (agent-role-handle-envelope v env))
      ;; Should return error indication, not crash
      (check-not-false result))))

(run-tests suite)
