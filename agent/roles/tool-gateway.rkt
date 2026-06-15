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
         racket/match
         (only-in "base.rkt" gen:agent-role agent-role-capabilities make-capability-guarded-handler)
         (only-in "../../util/capability.rkt" ROLE-CAPABILITIES)
         (only-in "../../util/message/mas-envelope.rkt" mas-envelope-payload mas-envelope-risk-level))

;; ============================================================
;; W3 (v0.99.9): Risk-Based Routing Policy
;; ============================================================

;; Routing policy parameter.
;; 'local-only (default): all requests go to local executor — zero behavioral change.
;; 'risk-based: high/critical risk requests are tagged for remote execution.
;; Phase 1: even with 'risk-based, the remote executor doesn't exist yet.
;; The routing decision is logged but execution stays local.
;; Phase 2: remote executor will handle 'remote-tagged requests via TCP broker.
(define current-routing-policy (make-parameter 'local-only))

;; Decide where to route an execution request based on risk level.
;; Returns: 'local or 'remote
(define (route-execution-request envelope)
  (define risk (mas-envelope-risk-level envelope))
  (match (current-routing-policy)
    ['local-only 'local]
    ['risk-based (if (memq risk '(high critical)) 'remote 'local)]
    [_ 'local]))

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

(define (routing-decision->execution-route decision)
  (match decision
    ['local 'local]
    ;; W4/M4 phase 1: no remote executor exists yet, so high/critical risk
    ;; requests are explicitly tagged but still executed locally.
    ['remote 'remote-tagged-but-executed-local]
    [_ 'local]))

(define (annotate-routing-result result decision envelope)
  (define base-result
    (if (hash? result)
        result
        (hasheq 'status 'ok 'result result)))
  (hash-set* base-result
             'routing-decision
             decision
             'route
             (routing-decision->execution-route decision)
             'executed-locally?
             #t
             'risk-level
             (mas-envelope-risk-level envelope)))

(define (execute-tool-gateway-with-routing envelope)
  (define decision (route-execution-request envelope))
  (define result ((current-tool-executor) envelope))
  (annotate-routing-result result decision envelope))

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
                                      ;; H2: Use injected executor instead of direct sandbox import.
                                      ;; W4/M4: route through the policy integration wrapper so
                                      ;; the real gateway path exposes routing metadata.
                                      (lambda (self envelope)
                                        (execute-tool-gateway-with-routing envelope))))])

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
         default-tool-executor
         current-routing-policy
         route-execution-request
         execute-tool-gateway-with-routing)

(define (make-tool-gateway-role)
  (tool-gateway-role))
