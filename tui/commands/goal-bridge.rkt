#lang racket/base

;; tui/commands/goal-bridge.rkt — adapter between goal-runner and TUI event bus
;;
;; Two functions:
;; 1. make-goal-event-bridge: maps goal-runner symbol events → typed event bus events
;; 2. make-goal-run-prompt!: wraps run-prompt! for goal-runner consumption

(require racket/match
         (only-in "../../runtime/agent-session.rkt" agent-session?)
         (only-in "../../runtime/session/session-lifecycle.rkt" run-prompt!)
         "../../agent/event-bus.rkt"
         "../../runtime/runtime-helpers.rkt")

(provide make-goal-event-bridge
         make-goal-run-prompt!)

;; ============================================================
;; Event bridge: goal-runner symbols → event bus strings
;; ============================================================

(define (make-goal-event-bridge event-bus session-id)
  ;; Maps goal-runner symbol events to typed event bus events.
  ;; event-bus: event-bus? — the live event bus from agent-session
  ;; session-id: string? — the session identifier
  ;; Returns: (-> symbol? hash? void?)
  (lambda (event-type payload)
    (define event-name
      (case event-type
        [(goal-started) "goal.started"]
        [(goal-turn-started) "goal.turn.started"]
        [(goal-evaluated) "goal.evaluated"]
        [(goal-check-completed) "goal.check.completed"]
        [(goal-achieved) "goal.achieved"]
        [(goal-failed) "goal.failed"]
        [else #f]))
    (when (and event-name event-bus)
      (emit-session-event! event-bus session-id event-name payload))))

;; ============================================================
;; run-prompt wrapper: adapts agent-session for goal-runner
;; ============================================================

(define (make-goal-run-prompt! sess)
  ;; Wraps run-prompt! for goal-runner consumption.
  ;; sess: agent-session? — the live session
  ;; Returns: (-> string? (values (or/c #f agent-session?) loop-result?))
  ;; goal-run! expects run-prompt-fn! to accept a prompt string and
  ;; return (values updated-sess loop-result).
  (lambda (prompt)
    (run-prompt! sess prompt)))
