#lang racket/base

;; agent/roles/tool-gateway.rkt — Tool Gateway agent role (stub for Schritt 2)
;; STABILITY: evolving
;;
;; The tool-gateway role has access to shell execution, file writing,
;; git operations, network access, and browser automation.
;; It acts as the execution agent for tool calls delegated by the supervisor.

(require racket/generic
         (only-in "base.rkt" gen:agent-role agent-role-capabilities make-capability-guarded-handler)
         (only-in "../../util/message/mas-envelope.rkt" mas-envelope-payload))

;; ============================================================
;; Struct Definition
;; ============================================================

(struct tool-gateway-role ()
  #:transparent
  #:methods gen:agent-role
  [(define (agent-role-capabilities self)
     '(shell-exec file-write git-write network browser))
   (define (agent-role-system-prompt self)
     (string-append "You are the Tool Gateway agent in a multi-agent system. "
                    "Your role is to execute tool calls on behalf of the supervisor. "
                    "You have access to shell execution, file operations, git, "
                    "network requests, and browser automation."))
   (define agent-role-handle-envelope
     (make-capability-guarded-handler (lambda (_) '(shell-exec file-write git-write network browser))
                                      (lambda (self envelope)
                                        (hasheq 'status
                                                'ok
                                                'role
                                                'tool-gateway
                                                'payload
                                                (mas-envelope-payload envelope)
                                                'message
                                                "tool-gateway acknowledged envelope"))))])

;; ============================================================
;; Provides
;; ============================================================

(provide tool-gateway-role?
         make-tool-gateway-role)

(define (make-tool-gateway-role)
  (tool-gateway-role))
