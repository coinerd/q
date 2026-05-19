#lang racket/base

;; runtime/iteration/effect-executor.rkt — Thin executor for step effects (F2)
;; STABILITY: evolving
;;
;; Executes lists of step-level effects produced by interpret-step/effects.
;; Separates WHAT happens (effects) from HOW it happens (executor).

(require racket/contract
         racket/match
         (only-in "loop-state.rkt"
                  loop-infra
                  loop-infra-bus
                  loop-infra-session-id
                  loop-infra-log-path)
         (only-in "../../runtime/runtime-helpers.rkt" emit-session-event!)
         (only-in "../../runtime/session-store.rkt" append-entries!))

(provide step-effect:append-entries
         step-effect:append-entries?
         step-effect:append-entries-entries
         step-effect:emit-event
         step-effect:emit-event?
         step-effect:emit-event-name
         step-effect:emit-event-payload
         step-effect?
         run-step-effects!)

;; ---------------------------------------------------------------------------
;; Step effect descriptors
;; ---------------------------------------------------------------------------

;; Append entries to the session log
(struct step-effect:append-entries (entries) #:transparent)

;; Emit a session event
(struct step-effect:emit-event (name payload) #:transparent)

;; Predicate
(define (step-effect? v)
  (or (step-effect:append-entries? v) (step-effect:emit-event? v)))

;; ---------------------------------------------------------------------------
;; Executor
;; ---------------------------------------------------------------------------

(define (run-step-effects! effects infra)
  ;; Execute a list of step effects against real infrastructure.
  (for ([eff (in-list effects)])
    (cond
      [(step-effect:append-entries? eff)
       (append-entries! (loop-infra-log-path infra) (step-effect:append-entries-entries eff))]
      [(step-effect:emit-event? eff)
       (emit-session-event! (loop-infra-bus infra)
                            (loop-infra-session-id infra)
                            (step-effect:emit-event-name eff)
                            (step-effect:emit-event-payload eff))])))
