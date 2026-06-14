#lang racket/base

;; agent/roles/planner.rkt — Planner agent role
;; STABILITY: evolving
;;
;; The planner role can read files, create/modify GSD plans,
;; and write to memory. It cannot execute shell commands or
;; modify source files directly.

(require racket/generic
         (only-in "base.rkt" gen:agent-role agent-role-capabilities make-capability-guarded-handler)
         (only-in "../../agent/capability.rkt" ROLE-CAPABILITIES)
         (only-in "../../util/message/mas-envelope.rkt" mas-envelope-payload))

;; ============================================================
;; Struct Definition
;; ============================================================

(struct planner-role ()
  #:transparent
  #:methods gen:agent-role
  [(define (agent-role-capabilities self)
     (hash-ref ROLE-CAPABILITIES 'planner))
   (define (agent-role-system-prompt self)
     (string-append "You are the Planner agent in a multi-agent system. "
                    "Your role is to analyze tasks, create structured plans, "
                    "and break work into discrete waves. "
                    "You can read files and modify plans, but cannot execute "
                    "shell commands or edit source files."))
   (define agent-role-handle-envelope
     (make-capability-guarded-handler (lambda (self) (agent-role-capabilities self))
                                      (lambda (self envelope)
                                        (hasheq 'status
                                                'ok
                                                'role
                                                'planner
                                                'payload
                                                (mas-envelope-payload envelope)
                                                'message
                                                "planner acknowledged envelope"))))])

;; ============================================================
;; Provides
;; ============================================================

(provide planner-role?
         make-planner-role)

(define (make-planner-role)
  (planner-role))
