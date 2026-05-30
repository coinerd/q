#lang racket/base

;; runtime/session-mutation.rkt — Invariant-guarded session mutation functions (W-04)
;;
;; Wraps all set-agent-session-...! calls with transition checks.
;; Prevents invalid state transitions like #t→#t on prompt-running?.

(require racket/contract
         "session-types.rkt"
         (submod "session-types.rkt" internal)
         (only-in "../util/errors.rkt" raise-session-error))

(provide (contract-out [guarded-set-prompt-running! (-> agent-session? boolean? void?)]
                       [guarded-set-compacting! (-> agent-session? boolean? void?)]
                       [guarded-set-shutdown-requested! (-> agent-session? boolean? void?)]
                       [guarded-set-force-shutdown! (-> agent-session? boolean? void?)]
                       [guarded-set-active! (-> agent-session? boolean? void?)]
                       [guarded-set-model-name! (-> agent-session? (or/c string? #f) void?)]
                       [guarded-set-config! (-> agent-session? any/c void?)]
                       [guarded-set-index! (-> agent-session? any/c void?)]
                       [guarded-set-persisted! (-> agent-session? boolean? void?)]
                       [guarded-set-pending-entries! (-> agent-session? list? void?)]
                       [guarded-set-start-time! (-> agent-session? any/c void?)]
                       [guarded-set-thinking-level! (-> agent-session? (or/c symbol? #f) void?)]
                       [guarded-set-last-compaction-time! (-> agent-session? any/c void?)]
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
