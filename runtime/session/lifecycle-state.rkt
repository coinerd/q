#lang racket/base
;; runtime/session/lifecycle-state.rkt — Lifecycle state extracted from agent-session
;; A1-05: God Struct Decomposition — extracted mutable lifecycle fields
;; STABILITY: stable

(provide (struct-out lifecycle-state)
         make-lifecycle-state
         lifecycle-state-closed?
         set-lifecycle-state-closed?!
         lifecycle-state-rollback-st
         set-lifecycle-state-rollback-st!
         lifecycle-state-prev-task-fsm-state
         set-lifecycle-state-prev-task-fsm-state!
         lifecycle-state-pending-force-reset?
         set-lifecycle-state-pending-force-reset?!
         consume-pending-force-reset!)

;; Lifecycle state: mutable flags that track session lifecycle.
;; Extracted from agent-session (24 fields → 15 fields + this struct).
;; Only lifecycle code (compaction, shutdown, prompt-running, task-fsm)
;; should access these fields directly.

(struct lifecycle-state
        ([compacting? #:mutable] ; boolean — guard against recursive compaction
         [last-compaction-time #:mutable] ; integer or #f — timestamp of last compaction
         [persisted? #:mutable] ; boolean — #f until directory + first write
         [shutdown-requested? #:mutable] ; boolean — graceful shutdown flag
         [force-shutdown? #:mutable] ; boolean — force immediate shutdown
         [prompt-running? #:mutable] ; boolean — is a prompt currently executing?
         [task-fsm-state #:mutable] ; symbol or #f — current task FSM state
         [task-conclusions #:mutable] ; (listof task-conclusion?) — agent task conclusions
         [recent-tool-calls #:mutable] ; (listof symbol?) — recent tool call history
         [closed? #:mutable] ; boolean — guard against repeated close
         [rollback-st #:mutable] ; rollback-state? — per-session rollback state (v0.99.86)
         [prev-task-fsm-state #:mutable] ; symbol or #f — previous turn's task FSM state
         [pending-force-reset? #:mutable]) ; boolean — one-shot WS reset signal
  #:transparent)

;; Constructor: create a lifecycle-state with safe defaults.
;; rollback-st defaults to #f; the session lifecycle layer initializes it
;; from make-default-rollback-state at session creation time.
;; prev-task-fsm-state defaults to #f (no previous state at session start).
;; pending-force-reset? defaults to #f (no pending reset).
(define (make-lifecycle-state)
  (lifecycle-state #f #f #f #f #f #f 'idle '() '() #f #f #f #f))

;; ─────────────────────────────────────────────────────────────
;; One-shot consumption API for pending-force-reset?
;;
;; Atomically reads, clears, and returns the pending-force-reset? flag.
;; Returns #t if a reset was pending (and clears it), #f otherwise.
;;
;; Exception semantics (Phase 7 choice: consume-before-reset):
;; The flag is cleared BEFORE the caller performs the working-set reset.
;; If the reset throws, the signal is consumed and will NOT retry on
;; the next turn. This prevents repeated resets of an already-partially-
;; reset working set. The caller is responsible for any recovery.
;; ─────────────────────────────────────────────────────────────
(define (consume-pending-force-reset! ls)
  (define was-pending? (lifecycle-state-pending-force-reset? ls))
  (when was-pending?
    (set-lifecycle-state-pending-force-reset?! ls #f))
  was-pending?)
