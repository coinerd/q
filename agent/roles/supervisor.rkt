#lang racket/base

;; agent/roles/supervisor.rkt — Supervisor orchestrator role
;; STABILITY: evolving
;;
;; The supervisor role orchestrates sub-agents. It can read, plan,
;; write to memory, and spawn sub-agents. It dispatches envelopes
;; to the appropriate sub-role based on target-agent.

(require racket/generic
         (only-in "base.rkt" gen:agent-role agent-role-capabilities agent-role-handle-envelope)
         (only-in "../../util/capability.rkt" ROLE-CAPABILITIES)
         (only-in "planner.rkt" make-planner-role)
         (only-in "verifier.rkt" make-verifier-role)
         (only-in "tool-gateway.rkt" make-tool-gateway-role)
         (only-in "../../util/message/mas-envelope.rkt"
                  mas-envelope?
                  mas-envelope-payload
                  mas-envelope-target-agent)
         ;; Registry import — used only when current-use-registry is #t.
         ;; The lazy require pattern avoids a hard dependency at load time
         ;; when the registry is not enabled.
         (only-in "../registry.rkt" make-agent-instance registered-roles))

;; ============================================================
;; Struct Definition
;; ============================================================

(struct supervisor-role (sub-roles)
  #:transparent
  #:methods gen:agent-role
  [(define (agent-role-capabilities self)
     (hash-ref ROLE-CAPABILITIES 'supervisor))
   (define (agent-role-system-prompt self)
     (string-append "You are the Supervisor agent in a multi-agent system. "
                    "Your role is to orchestrate sub-agents: planner, verifier, "
                    "and tool-gateway. You dispatch work based on capability "
                    "requirements and coordinate results."))
   ;; C1 fix: Supervisor is a pure router. Each sub-role enforces its own
   ;; capability guard. Wrapping the supervisor with a guard blocks all
   ;; dispatch since the supervisor lacks execution capabilities.
   (define (agent-role-handle-envelope self envelope)
     (supervisor-dispatch self envelope))])

;; ============================================================
;; Dispatch
;; ============================================================

;; Routes an envelope to the appropriate sub-role based on target-agent.
(define (supervisor-dispatch supervisor envelope)
  (cond
    [(not (mas-envelope? envelope))
     (hasheq 'status 'error 'message (format "expected mas-envelope?, got ~a" envelope))]
    [else
     (define target (mas-envelope-target-agent envelope))
     (define sub-roles (supervisor-role-sub-roles supervisor))
     (define target-role (hash-ref sub-roles target #f))
     (cond
       [target-role (agent-role-handle-envelope target-role envelope)]
       [else
        (hasheq 'status
                'error
                'message
                (format "unknown target agent: ~a" target)
                'target
                target)])]))

;; ============================================================
;; Constructor
;; ============================================================

;; When #f (default), the supervisor uses direct construction (backward compat).
;; When #t, the supervisor resolves sub-roles from the agent registry.
;; Set to #t by the wiring layer when mas.hot-swap.enabled is true.
(define current-use-registry (make-parameter #f))

(define (make-supervisor-role)
  (define sub-roles
    (if (current-use-registry)
        ;; Registry path: resolve each sub-role from the registry.
        ;; Falls back to direct construction if the role is not registered.
        (hasheq 'planner
                (resolve-sub-role 'planner make-planner-role)
                'verifier
                (resolve-sub-role 'verifier make-verifier-role)
                'tool-gateway
                (resolve-sub-role 'tool-gateway make-tool-gateway-role))
        ;; Backward compat: direct construction (no registry dependency)
        (hasheq 'planner
                (make-planner-role)
                'verifier
                (make-verifier-role)
                'tool-gateway
                (make-tool-gateway-role))))
  (supervisor-role sub-roles))

;; Helper: try registry first, fall back to direct construction if
;; the role is not registered. This makes registry adoption graceful —
;; even with current-use-registry #t, missing registrations degrade
;; safely instead of erroring.
(define (resolve-sub-role role-name direct-factory)
  (with-handlers ([exn:fail? (lambda (_) (direct-factory))])
    (if (member role-name (registered-roles))
        (make-agent-instance role-name)
        (direct-factory))))

;; ============================================================
;; Provides
;; ============================================================

(provide supervisor-role?
         supervisor-role-sub-roles
         make-supervisor-role
         supervisor-dispatch
         current-use-registry)
