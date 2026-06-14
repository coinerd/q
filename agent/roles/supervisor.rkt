#lang racket/base

;; agent/roles/supervisor.rkt — Supervisor orchestrator role
;; STABILITY: evolving
;;
;; The supervisor role orchestrates sub-agents. It can read, plan,
;; write to memory, and spawn sub-agents. It dispatches envelopes
;; to the appropriate sub-role based on target-agent.

(require racket/generic
         (only-in "base.rkt" gen:agent-role agent-role-capabilities agent-role-handle-envelope)
         (only-in "planner.rkt" make-planner-role)
         (only-in "verifier.rkt" make-verifier-role)
         (only-in "tool-gateway.rkt" make-tool-gateway-role)
         (only-in "../../util/message/mas-envelope.rkt"
                  mas-envelope?
                  mas-envelope-payload
                  mas-envelope-target-agent))

;; ============================================================
;; Struct Definition
;; ============================================================

(struct supervisor-role (sub-roles)
  #:transparent
  #:methods gen:agent-role
  [(define (agent-role-capabilities self)
     '(read-only plan-write memory-write subagent))
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
  (define target (mas-envelope-target-agent envelope))
  (define sub-roles (supervisor-role-sub-roles supervisor))
  (define target-role (hash-ref sub-roles target #f))
  (cond
    [target-role (agent-role-handle-envelope target-role envelope)]
    [else
     (hasheq 'status 'error 'message (format "unknown target agent: ~a" target) 'target target)]))

;; ============================================================
;; Constructor
;; ============================================================

(define (make-supervisor-role)
  (supervisor-role (hasheq 'planner
                           (make-planner-role)
                           'verifier
                           (make-verifier-role)
                           'tool-gateway
                           (make-tool-gateway-role))))

;; ============================================================
;; Provides
;; ============================================================

(provide supervisor-role?
         make-supervisor-role
         supervisor-dispatch)
