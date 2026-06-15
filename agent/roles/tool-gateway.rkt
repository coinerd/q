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

;; W3 (v0.99.12): Default remote executor — returns clear error.
;; When broker is disabled (default), no remote executor is configured.
;; This produces a fail-closed error if routing tries 'remote without one.
(define (default-remote-tool-executor envelope)
  (hasheq 'status
          'error
          'role
          'tool-gateway
          'error-message
          "remote executor not configured: mas.broker.enabled is #f or no executor connected"
          'trace-id
          #f))

;; H2: The active executor — set by wiring layer when feature flag is ON
(define current-tool-executor (make-parameter default-tool-executor))

;; W3 (v0.99.12): The active remote executor — set by wiring layer when broker is ON.
;; When routing decision is 'remote, this function handles the request.
;; Default: fail-closed error (no remote executor configured).
(define current-remote-tool-executor (make-parameter default-remote-tool-executor))

(define (routing-decision->execution-route decision)
  (match decision
    ['local 'local]
    ;; W3 (v0.99.12): remote routing is now real — the wiring layer
    ;; sets current-remote-tool-executor to the actual remote bridge.
    ['remote 'remote]
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
             (eq? decision 'local)
             'risk-level
             (mas-envelope-risk-level envelope)))

(define (execute-tool-gateway-with-routing envelope)
  (define decision (route-execution-request envelope))
  (define result
    (match decision
      ['remote ((current-remote-tool-executor) envelope)]
      [_ ((current-tool-executor) envelope)]))
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
         current-remote-tool-executor
         default-remote-tool-executor
         current-routing-policy
         route-execution-request
         execute-tool-gateway-with-routing
         routing-decision->execution-route)

(define (make-tool-gateway-role)
  (tool-gateway-role))
