#lang racket/base

;; agent/registry-defaults.rkt — Default agent registrations
;; STABILITY: evolving
;;
;; MAS Schritt 5: Populate the registry with built-in agent roles.
;; Called at startup by the wiring layer.

(require "registry.rkt"
         (only-in "roles/planner.rkt" make-planner-role)
         (only-in "roles/verifier.rkt" make-verifier-role)
         (only-in "roles/tool-gateway.rkt" make-tool-gateway-role)
         (only-in "roles/executor.rkt" make-executor-role))

;; Register all built-in agent roles at version "1.0.0".
;; Idempotent: calling multiple times is safe because the registry
;; appends new descriptors — but typically called once at startup.
(define (register-default-agents!)
  (register-agent! 'planner "1.0.0" make-planner-role)
  (register-agent! 'verifier "1.0.0" make-verifier-role)
  (register-agent! 'tool-gateway "1.0.0" make-tool-gateway-role)
  (register-agent! 'executor "1.0.0" make-executor-role))

(provide register-default-agents!)
