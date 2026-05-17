#lang racket/base

;; agent/effect-types.rkt — Effect descriptor struct union (F1)
;; STABILITY: evolving
;;
;; Defines effect descriptors returned by pure phase functions.
;; Effects describe WHAT should happen; the executor decides HOW.
;; This separation enables dry-run testing of agent loop phases.

(require racket/contract)

(provide (struct-out effect:emit-event)
         (struct-out effect:update-fsm)
         (struct-out effect:dispatch-hook)
         (struct-out effect:stream-from-provider)
         (struct-out effect:none)
         effect?
         execute-effects!)

;; ---------------------------------------------------------------------------
;; Effect descriptors
;; ---------------------------------------------------------------------------

;; Emit a typed event to the event bus
(struct effect:emit-event (type payload) #:transparent)

;; Update the turn FSM state machine
(struct effect:update-fsm (from-state event) #:transparent)

;; Dispatch a hook at the given hook point
(struct effect:dispatch-hook (hook-point payload) #:transparent)

;; Stream from provider with given config and messages
(struct effect:stream-from-provider
        (provider request bus session-id turn-id state hook-dispatcher cancellation-token)
  #:transparent)

;; No-op effect (identity)
(struct effect:none () #:transparent)

;; Predicate: is this an effect descriptor?
(define effect?
  (or/c effect:emit-event?
        effect:update-fsm?
        effect:dispatch-hook?
        effect:stream-from-provider?
        effect:none?))

;; ---------------------------------------------------------------------------
;; Default executor (runs effects with real side effects)
;; ---------------------------------------------------------------------------

(require "event-bus.rkt"
         "event-emitter.rkt"
         "loop-fsm.rkt")

(define (execute-effects! effects #:bus [bus #f] #:state [st #f])
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
      [(effect:dispatch-hook? eff) (void)] ;; hooks dispatched separately
      [(effect:none? eff) (void)]
      [else (void)])))
