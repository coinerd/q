#lang racket/base

;; util/capability.rkt — MAS capability taxonomy
;; STABILITY: evolving
;;
;; Single source of truth for all capability symbols.
;; Tools declare required-capability from this set.
;; Agent roles are granted subsets of this set.
;;
;; NOTE: This file was moved from agent/capability.rkt to util/capability.rkt
;; in v0.99.2 to resolve layer-violation findings A1/M2 and A4.
;; agent/capability.rkt remains as a re-export shim for backward compat.

(require racket/contract)

(provide VALID-CAPABILITIES
         ROLE-CAPABILITIES
         current-session-capabilities
         (contract-out [valid-capability? (-> any/c boolean?)]
                       [canonical-capabilities-snapshot (-> any/c (listof symbol?))]
                       [capability-authorized? (-> any/c any/c boolean?)]
                       [role-has-capability? (-> symbol? symbol? boolean?)]
                       [all-capabilities (-> (listof symbol?))]))

;; ============================================================
;; Capability Taxonomy
;; ============================================================

;; All valid capability symbols in the MAS system.
;; 'any is the legacy wildcard. As a grant it authorizes every valid tool;
;; as a requirement it is authorized only by an explicit 'any grant.
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

;; Capture an authority value as a fresh immutable, duplicate-free list.
;; Invalid authority is represented by the empty snapshot so downstream
;; authorization checks fail closed.
(define (canonical-capabilities-snapshot capabilities)
  (if (and (list? capabilities) (andmap valid-capability? capabilities))
      (for/fold ([snapshot '()]) ([capability (in-list capabilities)])
        (if (memq capability snapshot)
            snapshot
            (append snapshot (list capability))))
      '()))

;; A required 'any capability is a legacy wildcard declaration, not an
;; unprivileged declaration: only legacy '(any) authority may invoke it.
;; Concrete requirements accept either an exact grant or the 'any grant.
(define (capability-authorized? required granted)
  (and (valid-capability? required)
       (list? granted)
       (andmap valid-capability? granted)
       (if (eq? required 'any)
           (and (memq 'any granted) #t)
           (and (or (memq required granted) (memq 'any granted)) #t))))

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
;; New execution contexts snapshot this parameter when no authority is supplied.
(define current-session-capabilities (make-parameter '(any)))
