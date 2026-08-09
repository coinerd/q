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
         consume-pending-force-reset!
         lifecycle-state-pending-reflection-event
         set-lifecycle-state-pending-reflection-event!
         consume-reflection-event!)

;; Lifecycle state: mutable flags that track session lifecycle.
;; Extracted from agent-session (24 fields → 15 fields + this struct).
;; Only lifecycle code (compaction, shutdown, prompt-running, task-fsm)
;; should access these fields directly.

(struct lifecycle-state
        ([compacting? #:mutable] [last-compaction-time #:mutable]
                                 [persisted? #:mutable]
                                 [shutdown-requested? #:mutable]
                                 [force-shutdown? #:mutable]
                                 [prompt-running? #:mutable]
                                 [task-fsm-state #:mutable]
                                 [task-conclusions #:mutable]
                                 [recent-tool-calls #:mutable]
                                 [closed? #:mutable]
                                 [rollback-st #:mutable]
                                 [prev-task-fsm-state #:mutable]
                                 [pending-force-reset? #:mutable]
                                 [pending-reflection-event #:mutable])
  #:transparent)

(define (make-lifecycle-state)
  (lifecycle-state #f #f #f #f #f #f 'idle '() '() #f #f #f #f #f))

;; One-shot consumption: pending-force-reset?
;; Consume-before-reset semantics: flag cleared before caller resets.
(define (consume-pending-force-reset! ls)
  (define was-pending? (lifecycle-state-pending-force-reset? ls))
  (when was-pending?
    (set-lifecycle-state-pending-force-reset?! ls #f))
  was-pending?)

;; One-shot consumption: pending-reflection-event
;; Returns the event (or #f), then clears. Last-write-wins semantics:
;; multiple "reflection-suggested" events before consumption — last one wins.
;; Consume-before-formatting semantics: event consumed before preamble build.
(define (consume-reflection-event! ls)
  (define evt (lifecycle-state-pending-reflection-event ls))
  (when evt
    (set-lifecycle-state-pending-reflection-event! ls #f))
  evt)
