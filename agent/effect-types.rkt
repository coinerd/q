#lang racket/base

;; agent/effect-types.rkt — Effect descriptor struct union (F1)
;; STABILITY: evolving
;;
;; Defines effect descriptors returned by pure phase functions.
;; Effects describe WHAT should happen; the executor decides HOW.
;; This separation enables dry-run testing of agent loop phases.

(require racket/contract
         (only-in "../util/fsm.rkt" fsm-state? fsm-event?))

(provide (contract-out (struct effect:emit-event ([type symbol?] [payload any/c])))
         (contract-out (struct effect:update-fsm ([from-state fsm-state?] [event fsm-event?])))
         (contract-out (struct effect:dispatch-hook ([hook-point symbol?] [payload any/c])))
         (contract-out (struct effect:none ()))
         effect?)

;; ---------------------------------------------------------------------------
;; Effect descriptors
;; ---------------------------------------------------------------------------

;; Emit a typed event to the event bus
(struct effect:emit-event (type payload) #:transparent)

;; Update the turn FSM state machine
(struct effect:update-fsm (from-state event) #:transparent)

;; Dispatch a hook at the given hook point
(struct effect:dispatch-hook (hook-point payload) #:transparent)

;; No-op effect (identity)
(struct effect:none () #:transparent)

;; ---------------------------------------------------------------------------
;; Predicates
;; ---------------------------------------------------------------------------

;; Predicate: is this an effect descriptor?
;; v0.46.10 (M-1): Proper predicate instead of or/c alias.
(define (effect? v)
  (or (effect:emit-event? v) (effect:update-fsm? v) (effect:dispatch-hook? v) (effect:none? v)))
