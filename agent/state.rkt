#lang racket/base

;; agent/state.rkt — ephemeral loop state for a single run
;;
;; Provides mutable accumulators for messages and events during
;; a single agent turn. Must NOT persist state directly.

(require racket/contract)

;; Agent loop mutable state
(provide make-loop-state
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

(struct loop-state (session-id turn-id messages-box events-box) #:transparent)

;; ============================================================
;; Constructor
;; ============================================================

(define (make-loop-state session-id turn-id)
  (loop-state session-id turn-id (box '()) (box '())))

;; ============================================================
;; Accessors
;; ============================================================

;; Access accumulated messages (snapshot)
;; v0.12.3 Wave 0.3: Messages stored in reverse (cons), reversed on read.
;; This changes O(n²) append to O(1) per add.
(define (loop-state-messages st)
  (reverse (unbox (loop-state-messages-box st))))

;; Access accumulated events (snapshot)
(define (loop-state-events st)
  (reverse (unbox (loop-state-events-box st))))

;; ============================================================
;; Mutation helpers
;; ============================================================

;; Add a message — O(1) cons instead of O(n) append
;; v0.12.3 Wave 0.3: Fixed O(n²) pattern.
(define (state-add-message! st msg)
  (set-box! (loop-state-messages-box st) (cons msg (unbox (loop-state-messages-box st))))
  (void))

;; Add an event — O(1) cons instead of O(n) append
(define (state-add-event! st evt)
  (set-box! (loop-state-events-box st) (cons evt (unbox (loop-state-events-box st))))
  (void))
