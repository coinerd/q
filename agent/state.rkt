#lang racket/base
;; STABILITY: public

;; agent/state.rkt — ephemeral loop state for a single run
;;
;; Provides mutable accumulators for messages and events during
;; a single agent turn. Must NOT persist state directly.

(require racket/contract)

;; Agent loop mutable state
(provide (contract-out [make-loop-state (-> string? string? loop-state?)]
                       [loop-state? (-> any/c boolean?)]
                       [loop-state-session-id (-> loop-state? string?)]
                       [loop-state-turn-id (-> loop-state? string?)]
                       [loop-state-messages (-> loop-state? (listof any/c))]
                       [loop-state-events (-> loop-state? (listof any/c))]
                       [state-add-message! (-> loop-state? any/c void?)]
                       [state-add-event! (-> loop-state? any/c void?)]
                       [current-empty-response-retried? (parameter/c boolean?)]
                       [current-reflection-event (parameter/c (or/c any/c #f))]))

;; ============================================================
;; v0.99.83 W2: Empty-response auto-retry tracking
;; ============================================================

;; Set by the iteration loop when an empty-response retry has been
;; attempted. Prevents infinite retry loops when the model repeatedly
;; produces thinking-only responses.
(define current-empty-response-retried? (make-parameter #f))

;; v0.99.85: Reflection event signal — moved from
;; runtime/context-assembly/state-aware-builder.rkt
;; ============================================================

;; Set by runtime/iteration/step-executor.rkt when large tool results
;; are detected. Read by runtime/context-assembly/state-aware-builder.rkt
;; when building the preamble for the next turn, then cleared.
;; Lives here (agent layer) so that the agent does not import from
;; runtime context-assembly.
(define current-reflection-event (make-parameter #f))

;; ============================================================
;; Internal struct — mutable boxes for accumulation
;; ============================================================

(struct loop-state (session-id turn-id messages-box events-box))

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
