#lang racket/base

;; runtime/context-pressure.rkt — context pressure signaling
;; STABILITY: internal
;;
;; Emits context-pressure events after context assembly so the TUI and
;; extensions can react to budget consumption levels.

(require racket/contract
         "../llm/token-budget.rkt"
         "../agent/event-emitter.rkt"
         "../agent/event-structs/context-pressure-events.rkt"
         "session-types.rkt")

(provide (contract-out [check-context-pressure
                        (-> agent-session?
                            exact-nonnegative-integer?
                            exact-nonnegative-integer?
                            (or/c 'green 'yellow 'red))]
                       [context-pressure-level (-> real? (or/c 'green 'yellow 'red))]))

;; ============================================================
;; Thresholds
;; ============================================================

;; Percentage boundaries for pressure levels.
(define YELLOW-THRESHOLD 60.0)
(define RED-THRESHOLD 80.0)

;; context-pressure-level : real? -> symbol?
;; Returns 'green, 'yellow, or 'red based on usage percentage.
(define (context-pressure-level usage-percent)
  (cond
    [(>= usage-percent RED-THRESHOLD) 'red]
    [(>= usage-percent YELLOW-THRESHOLD) 'yellow]
    [else 'green]))

;; ============================================================
;; Pressure check
;; ============================================================

;;; check-context-pressure : agent-session? integer? integer? -> symbol?
;;;
;;; Computes context usage from token count and budget, emits a
;;; `context-pressure-event` on the session event bus, and returns the
;;; level symbol ('green / 'yellow / 'red).
(define (check-context-pressure sess token-count budget-threshold)
  (define bus (agent-session-event-bus sess))
  (define sid (agent-session-session-id sess))
  (define usage (get-context-usage token-count budget-threshold))
  (define pct (context-usage-usage-percent usage))
  (define level (context-pressure-level pct))
  (emit-typed-event! bus
                     (make-context-pressure-event #:session-id sid
                                                  #:turn-id #f
                                                  #:timestamp (current-inexact-milliseconds)
                                                  #:level level
                                                  #:usage-percent pct))
  level)
