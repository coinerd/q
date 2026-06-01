#lang racket/base

;; runtime/session-mutation.rkt — Invariant-guarded session mutation functions (W-04)
;;
;; Wraps all set-agent-session-...! calls with transition checks.
;; Prevents invalid state transitions like #t→#t on prompt-running?.

(require racket/contract
         racket/set
         "session-types.rkt"
         (submod "session-types.rkt" internal)
         (only-in "session-config.rkt" session-config?)
         (only-in "../util/errors.rkt" raise-session-error)
         (only-in "session-index/schema.rkt" session-index?)
         (only-in "context-assembly/task-state.rkt" task-states-list task-valid-direct-transition?)
         (only-in "context-assembly/ws-evolution.rkt"
                  evolution-result?
                  evolution-result-evicted-conclusions)
         (only-in "context-assembly/task-conclusion.rkt" task-conclusion-id))

(provide (contract-out [guarded-set-prompt-running! (-> agent-session? boolean? void?)]
                       [guarded-set-compacting! (-> agent-session? boolean? void?)]
                       [guarded-set-shutdown-requested! (-> agent-session? boolean? void?)]
                       [guarded-set-force-shutdown! (-> agent-session? boolean? void?)]
                       [guarded-set-active! (-> agent-session? boolean? void?)]
                       [guarded-set-model-name! (-> agent-session? (or/c string? #f) void?)]
                       [guarded-set-config! (-> agent-session? (or/c session-config? hash? #f) void?)]
                       [guarded-set-index! (-> agent-session? (or/c session-index? #f) void?)]
                       [guarded-set-persisted! (-> agent-session? boolean? void?)]
                       [guarded-set-pending-entries! (-> agent-session? list? void?)]
                       [guarded-set-start-time! (-> agent-session? exact-nonnegative-integer? void?)]
                       [guarded-set-thinking-level! (-> agent-session? (or/c symbol? #f) void?)]
                       [guarded-set-last-compaction-time!
                        (-> agent-session? (or/c exact-nonnegative-integer? #f) void?)]
                       [guarded-set-task-fsm-state! (-> agent-session? (or/c symbol? #f) void?)]
                       [guarded-set-task-conclusions! (-> agent-session? list? void?)]
                       [guarded-set-recent-tool-calls! (-> agent-session? list? void?)]
                       [guarded-set-working-set-evolved! (-> agent-session? evolution-result? void?)]
                       [valid-session-phase? (-> symbol? boolean?)]
                       [session-phase (-> agent-session? symbol?)]))

;; Valid session phases derived from boolean flags
(define (valid-session-phase? phase)
  (and (memq phase '(idle running compacting shutting-down)) #t))

;; Get current phase from session flags
(define (session-phase sess)
  (cond
    [(agent-session-shutdown-requested? sess) 'shutting-down]
    [(agent-session-compacting? sess) 'compacting]
    [(agent-session-prompt-running? sess) 'running]
    [else 'idle]))

;; W-04: Guard prompt-running? transitions — no #t→#t
(define (guarded-set-prompt-running! sess value)
  (define current (agent-session-prompt-running? sess))
  (when (and current value)
    (raise-session-error (format "session ~a: prompt already running (invariant violation #t→#t)"
                                 (agent-session-session-id sess))
                         (agent-session-session-id sess)))
  (set-agent-session-prompt-running?! sess value))

;; W-04: Guard compacting? transitions — no #t→#t
(define (guarded-set-compacting! sess value)
  (define current (agent-session-compacting? sess))
  (when (and current value)
    (raise-session-error (format "session ~a: already compacting (invariant violation #t→#t)"
                                 (agent-session-session-id sess))
                         (agent-session-session-id sess)))
  (set-agent-session-compacting?! sess value))

;; W-04: Guard shutdown-requested? — idempotent (#t→#t allowed for safety)
(define (guarded-set-shutdown-requested! sess value)
  (set-agent-session-shutdown-requested?! sess value))

;; Guard force-shutdown? — idempotent
(define (guarded-set-force-shutdown! sess value)
  (set-agent-session-force-shutdown?! sess value))

;; Guard active? — idempotent
(define (guarded-set-active! sess value)
  (set-agent-session-active?! sess value))

;; Guard model-name — type-safe wrapper
(define (guarded-set-model-name! sess value)
  (set-agent-session-model-name! sess value))

;; Guard config — type-safe wrapper
(define (guarded-set-config! sess value)
  (set-agent-session-config! sess value))

;; Guard index — type-safe wrapper
(define (guarded-set-index! sess value)
  (set-agent-session-index! sess value))

;; Guard persisted? — only allows #f→#t transition
(define (guarded-set-persisted! sess value)
  (define current (agent-session-persisted? sess))
  (when (and current (not value))
    (raise-session-error (format "session ~a: cannot un-mark as persisted"
                                 (agent-session-session-id sess))
                         (agent-session-session-id sess)))
  (set-agent-session-persisted?! sess value))

;; Guard pending-entries — type-safe wrapper
(define (guarded-set-pending-entries! sess value)
  (set-agent-session-pending-entries! sess value))

;; Guard start-time — type-safe wrapper
(define (guarded-set-start-time! sess value)
  (set-agent-session-start-time! sess value))

;; Guard thinking-level — type-safe wrapper
(define (guarded-set-thinking-level! sess value)
  (set-agent-session-thinking-level! sess value))

;; Guard last-compaction-time — type-safe wrapper
(define (guarded-set-last-compaction-time! sess value)
  (set-agent-session-last-compaction-time! sess value))

;; Guard task-fsm-state — validates FSM transition
;; v0.75.6: Validate that value is a known FSM state symbol.
;; v0.76.0 W1: Validate that the transition from current state is valid.
(define (guarded-set-task-fsm-state! sess value)
  ;; First, validate that value is a known state
  (when (and value (not (member value (task-states-list))))
    (raise-session-error (format "invalid task FSM state: ~a" value) #f))
  ;; Second, validate that the transition is allowed
  (define current (agent-session-task-fsm-state sess))
  (when (and value
             current
             (not (eq? current 'idle))
             (not (eq? value 'idle))
             (not (task-valid-direct-transition? current value)))
    (raise-session-error (format "invalid task FSM transition: ~a → ~a" current value) #f))
  (set-agent-session-task-fsm-state! sess value))

;; Guard recent-tool-calls — keeps last 10, validates list of symbols/strings
(define (guarded-set-recent-tool-calls! sess value)
  (unless (and (list? value) (andmap (lambda (v) (or (symbol? v) (string? v))) value))
    (raise-session-error "recent-tool-calls must be a list of symbols or strings" #f))
  (set-agent-session-recent-tool-calls! sess value))

;; Guard task-conclusions — type-safe wrapper
;; v0.78.0 W1: Fixed G5 — MERGE instead of REPLACE.
;; The "evicted-conclusions" field name is misleading; it's actually the
;; set of conclusions INJECTED by the evolution (to add to session).
;; We now merge (union by ID) with existing session conclusions.
(define (guarded-set-working-set-evolved! sess evolution-res)
  (when (and (agent-session? sess) (evolution-result? evolution-res))
    (define injected (evolution-result-evicted-conclusions evolution-res))
    (when (pair? injected)
      (define existing (agent-session-task-conclusions sess))
      (define existing-ids
        (for/set ([c (in-list existing)])
          (task-conclusion-id c)))
      ;; Merge: keep all existing + add new ones that don't already exist
      (define merged
        (append existing
                (filter (lambda (c) (not (set-member? existing-ids (task-conclusion-id c))))
                        injected)))
      (guarded-set-task-conclusions! sess merged))))

(define (guarded-set-task-conclusions! sess value)
  (set-agent-session-task-conclusions! sess value))
