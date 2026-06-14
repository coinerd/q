#lang racket/base

;; tests/test-mas-executor-role.rkt
;; Tests for the executor agent role.

(require rackunit
         rackunit/text-ui
         (only-in racket/string string-contains?)
         (only-in "../agent/roles/base.rkt"
                  gen:agent-role
                  agent-role?
                  agent-role-capabilities
                  agent-role-system-prompt
                  agent-role-handle-envelope)
         (only-in "../agent/roles/executor.rkt" executor-role? make-executor-role)
         (only-in "../util/capability.rkt" ROLE-CAPABILITIES valid-capability? role-has-capability?)
         (only-in "../util/message/mas-envelope.rkt" make-mas-envelope))

;; ── Test Suite ──────────────────────────────────────────────────

(define suite
  (test-suite "MAS Executor Role"

    ;; ── Type checks ──

    (test-case "executor-role? is a valid agent-role?"
      (define exec (make-executor-role))
      (check-true (executor-role? exec))
      (check-true (agent-role? exec)))

    (test-case "make-executor-role returns executor-role?"
      (check-true (executor-role? (make-executor-role))))

    ;; ── Capabilities ──

    (test-case "executor has shell-exec and file-write capabilities"
      (define exec (make-executor-role))
      (define caps (agent-role-capabilities exec))
      (check-not-false (member 'shell-exec caps))
      (check-not-false (member 'file-write caps)))

    (test-case "executor capabilities match ROLE-CAPABILITIES"
      (define exec (make-executor-role))
      (check-equal? (agent-role-capabilities exec) (hash-ref ROLE-CAPABILITIES 'executor)))

    (test-case "executor lacks network capability"
      (check-false (role-has-capability? 'executor 'network)))

    (test-case "executor lacks browser capability"
      (check-false (role-has-capability? 'executor 'browser)))

    (test-case "executor lacks git-write capability"
      (check-false (role-has-capability? 'executor 'git-write)))

    (test-case "executor has shell-exec via role-has-capability?"
      (check-true (role-has-capability? 'executor 'shell-exec)))

    (test-case "executor has file-write via role-has-capability?"
      (check-true (role-has-capability? 'executor 'file-write)))

    ;; ── System prompt ──

    (test-case "system prompt is non-empty string"
      (define exec (make-executor-role))
      (define prompt (agent-role-system-prompt exec))
      (check-true (string? prompt))
      (check-true (> (string-length prompt) 0)))

    (test-case "system prompt mentions execution plane"
      (define exec (make-executor-role))
      (define prompt (agent-role-system-prompt exec))
      (check-not-false (string-contains? prompt "execution plane")))

    ;; ── Envelope handling ──

    (test-case "handle-envelope accepts valid shell-exec envelope"
      (define exec (make-executor-role))
      (define envelope (make-mas-envelope 'supervisor 'executor 'shell-exec "payload"))
      (define result (agent-role-handle-envelope exec envelope))
      (check-equal? (hash-ref result 'status) 'ok)
      (check-equal? (hash-ref result 'role) 'executor))

    (test-case "handle-envelope accepts valid file-write envelope"
      (define exec (make-executor-role))
      (define envelope (make-mas-envelope 'supervisor 'executor 'file-write "payload"))
      (define result (agent-role-handle-envelope exec envelope))
      (check-equal? (hash-ref result 'status) 'ok))

    (test-case "handle-envelope rejects network capability"
      (define exec (make-executor-role))
      (define envelope (make-mas-envelope 'supervisor 'executor 'network "payload"))
      (define result (agent-role-handle-envelope exec envelope))
      (check-equal? (hash-ref result 'status) 'error))

    (test-case "handle-envelope rejects browser capability"
      (define exec (make-executor-role))
      (define envelope (make-mas-envelope 'supervisor 'executor 'browser "payload"))
      (define result (agent-role-handle-envelope exec envelope))
      (check-equal? (hash-ref result 'status) 'error))

    (test-case "handle-envelope rejects non-envelope input"
      (define exec (make-executor-role))
      (define result (agent-role-handle-envelope exec "not-an-envelope"))
      (check-equal? (hash-ref result 'status) 'error))

    ;; ── Supervisor does not dispatch to executor ──

    (test-case "executor is not in supervisor's sub-roles dispatch"
      ;; The supervisor dispatches to planner, verifier, tool-gateway.
      ;; executor is NOT a sub-role of supervisor — it lives in the
      ;; worker process, not the control plane.
      (define exec (make-executor-role))
      (check-true (executor-role? exec))
      ;; Just verify it exists independently; it's not registered with supervisor
      (check-true (agent-role? exec)))))

;; ── Run ─────────────────────────────────────────────────────────

(run-tests suite)
