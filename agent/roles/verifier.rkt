#lang racket/base

;; agent/roles/verifier.rkt — Verifier agent role (stub for Schritt 3)
;; STABILITY: evolving
;;
;; The verifier role has read-only access. It reviews plans,
;; code, and outputs without modification capability.

(require racket/generic
         (only-in "base.rkt" gen:agent-role agent-role-capabilities make-capability-guarded-handler)
         (only-in "../../agent/capability.rkt" ROLE-CAPABILITIES)
         (only-in "../../util/message/mas-envelope.rkt" mas-envelope-payload))

;; ============================================================
;; Struct Definition
;; ============================================================

(struct verifier-role ()
  #:transparent
  #:methods gen:agent-role
  [(define (agent-role-capabilities self)
     (hash-ref ROLE-CAPABILITIES 'verifier))
   (define (agent-role-system-prompt self)
     (string-append "You are the Verifier agent in a multi-agent system. "
                    "Your role is to review, validate, and test outputs "
                    "from other agents. You have read-only access — "
                    "you cannot modify files or execute commands."))
   (define agent-role-handle-envelope
     (make-capability-guarded-handler (lambda (_) '(read-only))
                                      (lambda (self envelope)
                                        (hasheq 'status
                                                'ok
                                                'role
                                                'verifier
                                                'payload
                                                (mas-envelope-payload envelope)
                                                'message
                                                "verifier acknowledged envelope"))))])

;; ============================================================
;; Provides
;; ============================================================

(provide verifier-role?
         make-verifier-role)

(define (make-verifier-role)
  (verifier-role))
