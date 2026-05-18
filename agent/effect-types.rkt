#lang racket/base

;; agent/effect-types.rkt — Effect descriptor struct union (F1)
;; STABILITY: evolving
;;
;; Defines effect descriptors returned by pure phase functions.
;; Effects describe WHAT should happen; the executor decides HOW.
;; This separation enables dry-run testing of agent loop phases.
;;
;; Also defines streaming-plan — pure computation result for the
;; streaming phase (v0.47.2).

(require racket/contract)

(provide (contract-out (struct effect:emit-event ([type any/c] [payload any/c])))
         (contract-out (struct effect:update-fsm ([from-state any/c] [event any/c])))
         (contract-out (struct effect:dispatch-hook ([hook-point any/c] [payload any/c])))
         (contract-out (struct effect:none ()))
         (contract-out (struct streaming-plan
                               ([session-id string?] [turn-id any/c]
                                                     [raw-messages (listof hash?)]
                                                     [req any/c]
                                                     [provider any/c]
                                                     [tools list?]
                                                     [hook-dispatcher (or/c procedure? #f)]
                                                     [cancellation-token any/c]
                                                     [expected-effects (listof effect?)])))
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
;; Streaming plan (pure computation result) — v0.47.2
;; ---------------------------------------------------------------------------

;; Pure result from compute-streaming-plan: describes what the streaming
;; phase needs to do, without actually doing it.
(struct streaming-plan
        (session-id turn-id
                    raw-messages ; (listof hash?) — validated message sequence
                    req ; model-request?
                    provider ; provider?
                    tools ; (listof any/c)
                    hook-dispatcher ; (or/c procedure? #f)
                    cancellation-token ; any/c
                    expected-effects ; (listof effect?) — effects to emit before streaming
                    )
  #:transparent)

;; ---------------------------------------------------------------------------
;; Predicates
;; ---------------------------------------------------------------------------

;; Predicate: is this an effect descriptor?
;; v0.46.10 (M-1): Proper predicate instead of or/c alias.
(define (effect? v)
  (or (effect:emit-event? v) (effect:update-fsm? v) (effect:dispatch-hook? v) (effect:none? v)))
