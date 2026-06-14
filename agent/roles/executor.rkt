#lang racket/base

;; agent/roles/executor.rkt — Executor agent role
;; STABILITY: evolving
;;
;; The executor role runs INSIDE the worker process.
;; It has shell-exec and file-write capabilities but no
;; network, git-write, or browser access.
;;
;; This resolves A5 finding: 'executor is no longer vestigial —
;; it has a struct, capabilities, and a purpose (validate requests
;; inside the execution plane worker).

(require racket/generic
         (only-in "base.rkt" gen:agent-role agent-role-capabilities make-capability-guarded-handler)
         (only-in "../../util/capability.rkt" ROLE-CAPABILITIES)
         (only-in "../../util/message/mas-envelope.rkt" mas-envelope-payload))

;; ============================================================
;; Struct Definition
;; ============================================================

(struct executor-role ()
  #:transparent
  #:methods gen:agent-role
  [(define (agent-role-capabilities self)
     (hash-ref ROLE-CAPABILITIES 'executor))
   (define (agent-role-system-prompt self)
     (string-append "You are the Executor agent running inside the execution plane worker. "
                    "Your role is to execute dangerous tool operations (shell commands, "
                    "file writes) in an isolated process. You have shell-exec and file-write "
                    "capabilities only. You cannot perform network requests, git operations, "
                    "or browser automation."))
   (define agent-role-handle-envelope
     (make-capability-guarded-handler (lambda (self) (agent-role-capabilities self))
                                      (lambda (self envelope)
                                        (hasheq 'status
                                                'ok
                                                'role
                                                'executor
                                                'payload
                                                (mas-envelope-payload envelope)
                                                'message
                                                "executor acknowledged envelope"))))])

;; ============================================================
;; Provides
;; ============================================================

(provide executor-role?
         make-executor-role)

(define (make-executor-role)
  (executor-role))
