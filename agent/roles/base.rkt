#lang racket/base

;; agent/roles/base.rkt — Agent role generics and base definitions
;; STABILITY: evolving
;;
;; Defines gen:agent-role — the generic interface all MAS agent roles
;; must implement. Each role provides:
;;   - capabilities: which capabilities this role has
;;   - system-prompt: the system prompt for this role
;;   - handle-envelope: process an incoming mas-envelope

(require racket/generic
         (only-in "../../util/message/mas-envelope.rkt"
                  mas-envelope?
                  mas-envelope-capability
                  mas-envelope-target-agent
                  mas-envelope-payload))

;; ============================================================
;; Generic Interface
;; ============================================================

(define-generics agent-role
                 (agent-role-capabilities agent-role)
                 (agent-role-system-prompt agent-role)
                 (agent-role-handle-envelope agent-role envelope)
                 #:fallbacks [(define (agent-role-handle-envelope agent-role envelope)
                                (hasheq 'status
                                        'error
                                        'message
                                        (format "unhandled envelope for role ~a"
                                                (object-name agent-role))))])

;; ============================================================
;; Capability Guard for handle-envelope
;; ============================================================

;; Wraps a handler with capability checking.
;; Returns error hash if the envelope's capability exceeds the role's grant.
(define (make-capability-guarded-handler role-getter handler)
  (lambda (self envelope)
    (define required (mas-envelope-capability envelope))
    (define granted (role-getter self))
    (cond
      [(or (eq? required 'any) (member required granted)) (handler self envelope)]
      [else
       (hasheq 'status
               'error
               'message
               (format "capability denied: ~a not in ~a" required granted)
               'required
               required
               'granted
               granted)])))

;; ============================================================
;; Provides
;; ============================================================

(provide gen:agent-role
         agent-role?
         agent-role-capabilities
         agent-role-system-prompt
         agent-role-handle-envelope
         make-capability-guarded-handler)
