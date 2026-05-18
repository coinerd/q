#lang racket/base

;; agent/effect-executor.rkt — Executor for agent-level effects (F1, W2-T3)
;; STABILITY: evolving
;;
;; Executes lists of effect descriptors produced by loop-phases.
;; Separates WHAT happens (effect descriptors in effect-types.rkt)
;; from HOW it happens (this executor).
;;
;; Layering: effect-types.rkt defines structs (no infrastructure deps).
;;           effect-executor.rkt imports infrastructure to execute them.

(require "event-bus.rkt"
         "event-emitter.rkt"
         "loop-fsm.rkt"
         "effect-types.rkt")

(provide execute-effects!)

;; ---------------------------------------------------------------------------
;; Executor
;; ---------------------------------------------------------------------------

(define (execute-effects! effects #:bus [bus #f] #:state [st #f] #:hook-dispatcher [hook-disp #f])
  ;; Execute a list of effect descriptors against real infrastructure.
  ;; This is the ONLY place where effects become side effects.
  (for ([eff (in-list effects)])
    (cond
      [(effect:emit-event? eff)
       (when (and bus (effect:emit-event-payload eff))
         (emit-typed-event! bus (effect:emit-event-payload eff) #:state st))]
      [(effect:update-fsm? eff)
       (current-turn-fsm-state (next-turn-state (effect:update-fsm-from-state eff)
                                                (effect:update-fsm-event eff)))]
      [(effect:dispatch-hook? eff)
       (when hook-disp
         (hook-disp (effect:dispatch-hook-hook-point eff) (effect:dispatch-hook-payload eff)))]
      [(effect:none? eff) (void)]
      [else (void)])))
