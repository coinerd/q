#lang racket/base

;; agent/registry-defaults.rkt — Default agent registrations
;; STABILITY: evolving
;;
;; MAS Schritt 5: Populate the registry with built-in agent roles.
;; Called at startup by the wiring layer.
;;
;; v0.99.13 W2 (G-3): Now passes #:module-path and #:factory-name
;; to enable dynamic-require-based hot-swapping.
;; v0.99.15 W1 (F-11): Use variable-reference-based path resolution
;; so module paths resolve from any CWD (fixes raco test failures).

(require "registry.rkt"
         (only-in "roles/planner.rkt" make-planner-role)
         (only-in "roles/verifier.rkt" make-verifier-role)
         (only-in "roles/tool-gateway.rkt" make-tool-gateway-role)
         (only-in "roles/executor.rkt" make-executor-role))

;; ── F-11: CWD-independent module path resolution ──
;; Uses variable-reference to resolve the directory containing this
;; module, then builds absolute paths from there. This ensures
;; dynamic-require works regardless of (current-directory).
(define this-module-dir
  (let* ([vr (#%variable-reference)]
         [resolved (variable-reference->resolved-module-path vr)]
         [path (resolved-module-path-name resolved)])
    (define-values (dir _name _dir?) (split-path path))
    dir))

(define (role-module-path role-sym)
  (build-path this-module-dir "roles" (format "~a.rkt" role-sym)))

;; Register all built-in agent roles at version "1.0.0".
;; Each role provides both a static factory (for backward compat) and
;; module-path + factory-name (for dynamic-require hot-swapping).
;; Idempotent: calling multiple times is safe because the registry
;; appends new descriptors — but typically called once at startup.
(define (register-default-agents!)
  (register-agent! 'planner
                   "1.0.0"
                   make-planner-role
                   #:module-path (role-module-path 'planner)
                   #:factory-name 'make-planner-role)
  (register-agent! 'verifier
                   "1.0.0"
                   make-verifier-role
                   #:module-path (role-module-path 'verifier)
                   #:factory-name 'make-verifier-role)
  (register-agent! 'tool-gateway
                   "1.0.0"
                   make-tool-gateway-role
                   #:module-path (role-module-path 'tool-gateway)
                   #:factory-name 'make-tool-gateway-role)
  (register-agent! 'executor
                   "1.0.0"
                   make-executor-role
                   #:module-path (role-module-path 'executor)
                   #:factory-name 'make-executor-role))

(provide register-default-agents!)
