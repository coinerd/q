#lang racket/base
;; runtime/memory/service.rkt — Runtime-owned memory service boundary
;;
;; v0.95.13: Extracts current-memory-backend and current-memory-policy from
;; tools/builtins/memory-tools.rkt to fix the runtime→tools layering inversion.
;;
;; Rules:
;;   - tools/builtins/memory-tools.rkt imports from this module
;;   - runtime/context-assembly/memory-builder.rkt imports from this module
;;   - No runtime module imports q/tools/* for memory service
;;   - This module does NOT import concrete tool modules or UI modules

(require "policy.rkt")

;; ---------------------------------------------------------------------------
;; Service parameters
;; ---------------------------------------------------------------------------

;; The active memory backend. #f when memory is disabled (default).
(define current-memory-backend (make-parameter #f))

;; The active memory policy. Defaults to the standard safe policy.
(define current-memory-policy (make-parameter default-memory-policy))

;; ---------------------------------------------------------------------------
;; Service helpers
;; ---------------------------------------------------------------------------

;; Check whether memory service is available (backend configured and not #f)
(define (memory-service-available?)
  (and (current-memory-backend) #t))

;; Resolve the effective backend, returning #f if unavailable
(define (resolve-memory-backend)
  (current-memory-backend))

;; ---------------------------------------------------------------------------
;; Provide
;; ---------------------------------------------------------------------------

(provide current-memory-backend
         current-memory-policy
         memory-service-available?
         resolve-memory-backend)
