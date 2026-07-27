#lang racket/base

;; agent/effect-types.rkt — Effect descriptor struct union (F1)
;; STABILITY: evolving
;;
;; Defines effect descriptors returned by pure phase functions.
;; Effects describe WHAT should happen; the executor decides HOW.
;; This separation enables dry-run testing of agent loop phases.
;;
;; v0.99.70 W0: Added effect descriptors for all direct I/O in turn loop:
;;   - effect:build-result — construct loop-result with messages and metadata
;;   - effect:cancel — cancellation with hook cleanup
;;   - effect:log — diagnostic logging (validation warnings, DIAG)
;;   - effect:validate-messages — API message sequence validation
;;   - effect:stream — streaming from provider (immutable request descriptor)

(require racket/contract
         (only-in "../util/fsm/fsm.rkt" fsm-state? fsm-event?))

(provide (contract-out (struct effect-base ()))
         (contract-out (struct effect:emit-event ([type symbol?] [payload any/c])))
         (contract-out (struct effect:update-fsm ([from-state fsm-state?] [event fsm-event?])))
         (contract-out (struct effect:dispatch-hook ([hook-point symbol?] [payload any/c])))
         ;; v0.99.70 W0: New effect descriptors
         (contract-out (struct effect:build-result
                               ([state any/c] [result-type symbol?] [metadata (or/c hash? #f)])))
         (contract-out (struct effect:cancel
                               ([turn-id string?] [session-id string?] [reason string?])))
         (contract-out (struct effect:log ([level symbol?] [message string?] [data (or/c list? #f)])))
         (contract-out (struct effect:validate-messages ([messages (listof any/c)])))
         (contract-out (struct effect:stream
                               ([provider any/c] [request any/c]
                                                 [bus any/c]
                                                 [session-id string?]
                                                 [turn-id string?]
                                                 [state any/c]
                                                 [raw-messages (listof any/c)]
                                                 [tools (or/c (listof hash?) #f)]
                                                 [hook-dispatcher (or/c procedure? #f)]
                                                 [cancellation-token (or/c any/c #f)])))
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

;; v0.99.70 W0: Build a loop-result with messages and metadata
(struct effect:build-result effect-base (state result-type metadata) #:transparent)

;; v0.99.70 W0: Cancel streaming with hook cleanup
(struct effect:cancel effect-base (turn-id session-id reason) #:transparent)

;; v0.99.70 W0: Diagnostic logging (validation warnings, DIAG messages)
(struct effect:log effect-base (level message data) #:transparent)

;; v0.99.70 W0: Validate API message sequence
(struct effect:validate-messages effect-base (messages) #:transparent)

;; v0.99.70 W0: Stream from provider (immutable request descriptor)
;; v0.99.70 W2: Added raw-messages and tools fields for build-stream-result
(struct effect:stream
        effect-base
        (provider request
                  bus
                  session-id
                  turn-id
                  state
                  raw-messages
                  tools
                  hook-dispatcher
                  cancellation-token)
  #:transparent)

;; No-op effect (identity)
(struct effect:none effect-base () #:transparent)

;; ---------------------------------------------------------------------------
;; Predicates
;; ---------------------------------------------------------------------------

;; Predicate: is this an effect descriptor?
;; Uses effect-base? supertype predicate (W16).
(define effect? effect-base?)
