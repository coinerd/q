#lang racket/base

;; agent/roles/tool-gateway.rkt — Tool Gateway agent role
;; STABILITY: evolving
;;
;; The tool-gateway role has access to shell execution, file writing,
;; git operations, network access, and browser automation.
;; It acts as the execution agent for tool calls delegated by the supervisor.
;;
;; H2 (v0.99.3): No longer imports from sandbox/ directly.
;; The execution function is injected via current-tool-executor parameter.
;; The wiring layer (run-modes.rkt) sets the real executor when enabled.

(require racket/generic
         (only-in "base.rkt" gen:agent-role agent-role-capabilities make-capability-guarded-handler)
         (only-in "../../util/capability.rkt" ROLE-CAPABILITIES)
         (only-in "../../util/message/mas-envelope.rkt" mas-envelope? mas-envelope-payload))

;; H2: Default executor — returns stub response (backward compat)
(define (default-tool-executor envelope)
  (hasheq 'status
          'ok
          'role
          'tool-gateway
          'payload
          (mas-envelope-payload envelope)
          'message
          "tool-gateway acknowledged envelope (execution plane disabled)"))

;; H2: The active executor — set by wiring layer when feature flag is ON
(define current-tool-executor (make-parameter default-tool-executor))

;; ============================================================
;; Struct Definition
;; ============================================================

(struct tool-gateway-role ()
  #:transparent
  #:methods gen:agent-role
  [(define (agent-role-capabilities self)
     (hash-ref ROLE-CAPABILITIES 'tool-gateway))
   (define (agent-role-system-prompt self)
     (string-append "You are the Tool Gateway agent in a multi-agent system. "
                    "Your role is to execute tool calls on behalf of the supervisor. "
                    "You have access to shell execution, file operations, git, "
                    "network requests, and browser automation."))
   (define agent-role-handle-envelope
     (make-capability-guarded-handler (lambda (self) (agent-role-capabilities self))
                                      ;; H2: Use injected executor instead of direct sandbox import
                                      (lambda (self envelope) ((current-tool-executor) envelope))))])

;; ============================================================
;; Convenience Functions
;; ============================================================

;; ============================================================
;; Provides
;; ============================================================

;; M8: gateway-start!/gateway-stop! removed — execution plane is
;; controlled exclusively by config in wiring/run-modes.rkt.
;; The parameter current-tool-executor is set by the wiring layer.

(provide tool-gateway-role?
         make-tool-gateway-role
         current-tool-executor
         default-tool-executor)

(define (make-tool-gateway-role)
  (tool-gateway-role))
