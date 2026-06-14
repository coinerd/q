#lang racket/base

;; agent/capability.rkt — MAS capability taxonomy
;; STABILITY: evolving
;;
;; Single source of truth for all capability symbols.
;; Tools declare required-capability from this set.
;; Agent roles are granted subsets of this set.

(require racket/contract)

(provide VALID-CAPABILITIES
         ROLE-CAPABILITIES
         current-session-capabilities
         (contract-out [valid-capability? (-> any/c boolean?)]
                       [role-has-capability? (-> symbol? symbol? boolean?)]
                       [all-capabilities (-> (listof symbol?))]))

;; ============================================================
;; Capability Taxonomy
;; ============================================================

;; All valid capability symbols in the MAS system.
;; 'any is the permissive wildcard — tools with 'any require no special capability.
(define VALID-CAPABILITIES
  '(read-only ; Read files, search, grep, find
    plan-write ; Create and modify GSD plans
    shell-exec ; Execute shell commands
    file-write ; Write and edit files
    git-write ; Git operations (commit, push)
    network ; External HTTP requests
    memory-write ; Write to memory store
    browser ; Browser automation
    subagent ; Spawn child agents
    any)) ; No restriction (legacy/transition)

(define (valid-capability? v)
  (and (symbol? v) (memq v VALID-CAPABILITIES) #t))

(define (all-capabilities)
  (filter (lambda (c) (not (eq? c 'any))) VALID-CAPABILITIES))

;; ============================================================
;; Role → Capability Mapping
;; ============================================================

;; Maps agent roles to the capabilities they are granted.
;; The supervisor can delegate to sub-agents with different capabilities.
(define ROLE-CAPABILITIES
  (hasheq 'supervisor
          '(read-only plan-write memory-write subagent)
          'planner
          '(read-only plan-write memory-write)
          'verifier
          '(read-only)
          'tool-gateway
          '(shell-exec file-write git-write network browser)
          'executor
          '(shell-exec file-write)))

(define (role-has-capability? role cap)
  (define granted (hash-ref ROLE-CAPABILITIES role #f))
  (and granted (or (eq? cap 'any) (memq cap granted)) #t))

;; ============================================================
;; Session-level capability parameter
;; ============================================================

;; Controls which capabilities a session's tools may exercise.
;; Default is '(any) — all tools available (backward compat).
;; When set to a specific list, tools-for-capability filters the registry.
(define current-session-capabilities (make-parameter '(any)))
