#lang racket

;; agent/state.rkt — ephemeral loop state for a single run
;;
;; Provides mutable accumulators for messages and events during
;; a single agent turn. Must NOT persist state directly.

(require racket/contract)

(provide
 make-loop-state
 loop-state?
 loop-state-session-id
 loop-state-turn-id
 loop-state-messages
 loop-state-events
 state-add-message!
 state-add-event!)

;; ============================================================
;; Internal struct — mutable boxes for accumulation
;; ============================================================

(struct loop-state (session-id turn-id messages-box events-box)
  #:transparent)

;; ============================================================
;; Constructor
;; ============================================================

(define (make-loop-state session-id turn-id)
  (loop-state session-id turn-id (box '()) (box '())))

;; ============================================================
;; Accessors
;; ============================================================

;; Access accumulated messages (snapshot)
(define (loop-state-messages st)
  (unbox (loop-state-messages-box st)))

;; Access accumulated events (snapshot)
(define (loop-state-events st)
  (unbox (loop-state-events-box st)))

;; ============================================================
;; Mutation helpers
;; ============================================================

;; Add a message to the end of the accumulated messages
(define (state-add-message! st msg)
  (set-box! (loop-state-messages-box st)
            (append (unbox (loop-state-messages-box st)) (list msg)))
  (void))

;; Add an event to the end of the accumulated events
(define (state-add-event! st evt)
  (set-box! (loop-state-events-box st)
            (append (unbox (loop-state-events-box st)) (list evt)))
  (void))
