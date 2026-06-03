#lang racket/base

;; agent/effect-types.rkt — Effect descriptor struct union (F1)
;; STABILITY: evolving
;;
;; Defines effect descriptors returned by pure phase functions.
;; Effects describe WHAT should happen; the executor decides HOW.
;; This separation enables dry-run testing of agent loop phases.

(require racket/contract
         (only-in "../util/fsm/fsm.rkt" fsm-state? fsm-event?))

(provide (contract-out (struct effect-base ()))
         (contract-out (struct effect:emit-event ([type symbol?] [payload any/c])))
         (contract-out (struct effect:update-fsm ([from-state fsm-state?] [event fsm-event?])))
         (contract-out (struct effect:dispatch-hook ([hook-point symbol?] [payload any/c])))
         (contract-out (struct effect:none ()))
         effect?)

;; ---------------------------------------------------------------------------
;; Effect base type
;; ---------------------------------------------------------------------------

;; Base supertype for all effect descriptors.
;; New effect types should inherit from this.
(struct effect-base () #:transparent)

;; ---------------------------------------------------------------------------
;; Effect descriptors
;; ---------------------------------------------------------------------------

;; Emit a typed event to the event bus
(struct effect:emit-event effect-base (type payload) #:transparent)

;; Update the turn FSM state machine
(struct effect:update-fsm effect-base (from-state event) #:transparent)

;; Dispatch a hook at the given hook point
(struct effect:dispatch-hook effect-base (hook-point payload) #:transparent)

;; No-op effect (identity)
(struct effect:none effect-base () #:transparent)

;; ---------------------------------------------------------------------------
;; Predicates
;; ---------------------------------------------------------------------------

;; Predicate: is this an effect descriptor?
;; Uses effect-base? supertype predicate (W16).
(define effect? effect-base?)
