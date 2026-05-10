#lang racket/base

;; runtime/session-mutation.rkt — Invariant-guarded session mutation functions (W-04)
;;
;; Wraps all set-agent-session-...! calls with transition checks.
;; Prevents invalid state transitions like #t→#t on prompt-running?.

(require "session-types.rkt")

(provide guarded-set-prompt-running!
         guarded-set-compacting!
         guarded-set-shutdown-requested!
         valid-session-phase?)

;; Valid session phases derived from boolean flags
(define (valid-session-phase? phase)
  (memq phase '(idle running compacting shutting-down)))

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
    (error 'set-prompt-running!
           "session ~a: prompt already running (invariant violation #t→#t)"
           (agent-session-session-id sess)))
  (set-agent-session-prompt-running?! sess value))

;; W-04: Guard compacting? transitions — no #t→#t
(define (guarded-set-compacting! sess value)
  (define current (agent-session-compacting? sess))
  (when (and current value)
    (error 'set-compacting!
           "session ~a: already compacting (invariant violation #t→#t)"
           (agent-session-session-id sess)))
  (set-agent-session-compacting?! sess value))

;; W-04: Guard shutdown-requested? — idempotent (#t→#t allowed for safety)
(define (guarded-set-shutdown-requested! sess value)
  (set-agent-session-shutdown-requested?! sess value))
