#lang racket/base

;; agent/roles/tool-gateway.rkt — Tool Gateway agent role
;; STABILITY: evolving
;;
;; The tool-gateway role has access to shell execution, file writing,
;; git operations, network access, and browser automation.
;; It acts as the execution agent for tool calls delegated by the supervisor.
;;
;; When current-execution-plane-enabled is #t (feature flag), tool calls
;; are routed through the isolated worker process via JSON-RPC.
;; When #f (default), a stub response is returned (backward compat).

(require racket/generic
         (only-in "base.rkt" gen:agent-role agent-role-capabilities make-capability-guarded-handler)
         (only-in "../../agent/capability.rkt" ROLE-CAPABILITIES)
         (only-in "../../util/message/mas-envelope.rkt"
                  mas-envelope?
                  mas-envelope-payload
                  mas-envelope-trace-id)
         (only-in "../../sandbox/gateway-bridge.rkt"
                  execute-via-worker
                  current-execution-plane-enabled
                  shutdown-worker!))

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
     (make-capability-guarded-handler
      (lambda (self) (agent-role-capabilities self))
      (lambda (self envelope)
        (cond
          ;; Route through the execution plane (isolated worker)
          [(current-execution-plane-enabled) (execute-via-worker envelope)]
          [else
           ;; Stub: acknowledge the envelope (backward compat)
           (hasheq 'status
                   'ok
                   'role
                   'tool-gateway
                   'payload
                   (mas-envelope-payload envelope)
                   'message
                   "tool-gateway acknowledged envelope (execution plane disabled)")]))))])

;; ============================================================
;; Convenience Functions
;; ============================================================

(define (gateway-start!)
  ;; Enable the execution plane for this role
  (current-execution-plane-enabled #t))

(define (gateway-stop!)
  ;; Disable the execution plane and shut down any running worker
  (current-execution-plane-enabled #f)
  (shutdown-worker!))

;; ============================================================
;; Provides
;; ============================================================

(provide tool-gateway-role?
         make-tool-gateway-role
         gateway-start!
         gateway-stop!)

(define (make-tool-gateway-role)
  (tool-gateway-role))
