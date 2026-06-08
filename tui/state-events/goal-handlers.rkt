#lang racket/base

;; tui/state-events/goal-handlers.rkt -- GOAL event handlers for UI state
;;
;; Extracted from state-events.rkt to keep goal logic modular.

(require racket/string
         (only-in "../../util/event/event.rkt" event event-payload event-time)
         "../state-types.rkt"
         "helpers.rkt"
         "registry.rkt")

;; ============================================================
;; Goal event reducers (v0.71.4)
;; ============================================================

(define (handle-goal-started state evt)
  (define payload (event-payload evt))
  (define goal-text (hash-ref payload 'goal-text ""))
  (define max-turns (hash-ref payload 'max-turns 8))
  (struct-copy ui-state
               state
               [active-goal (goal-display-info (truncate-string goal-text 40) 0 max-turns 'active)]))

(define (handle-goal-turn-started state evt)
  (define payload (event-payload evt))
  (define turn (hash-ref payload 'turn-number 1))
  (define current (ui-state-active-goal state))
  (define updated
    (if current
        (struct-copy ui-state
                     state
                     [active-goal
                      (struct-copy goal-display-info current [turns-used turn] [status 'active])])
        state))
  (set-busy-since (set-busy updated #t) (event-time evt)))

(define (handle-goal-evaluated state evt)
  ;; Evaluation complete, status stays active but turns updated
  (define current (ui-state-active-goal state))
  (if current
      (struct-copy ui-state
                   state
                   [active-goal (struct-copy goal-display-info current [status 'active])])
      state))

(define (handle-goal-check-completed state evt)
  ;; Informational only — no state change
  state)

(define (handle-goal-achieved state evt)
  (define payload (event-payload evt))
  (define turns (hash-ref payload 'turns-used 0))
  (define current (ui-state-active-goal state))
  (if current
      (struct-copy ui-state
                   state
                   [active-goal
                    (struct-copy goal-display-info current [turns-used turns] [status 'achieved])])
      state))

(define (handle-goal-failed state evt)
  (define payload (event-payload evt))
  (define turns (hash-ref payload 'turns-used 0))
  (define current (ui-state-active-goal state))
  (if current
      (struct-copy ui-state
                   state
                   [active-goal
                    (struct-copy goal-display-info current [turns-used turns] [status 'failed])])
      state))

(define (handle-goal-status state evt)
  (define payload (event-payload evt))
  (define msg
    (if (hash? payload)
        (hash-ref payload 'message "")
        ""))
  (define with-entry
    (append-entry state (make-entry 'system (format "[goal] ~a" msg) (event-time evt) (hash))))
  (if (string=? msg "")
      with-entry
      (set-status-message with-entry msg)))

;; ============================================================
;; Register all goal handlers at module load time
;; ============================================================

(register-event-reducer! "goal.started" handle-goal-started)
(register-event-reducer! "goal.turn.started" handle-goal-turn-started)
(register-event-reducer! "goal.evaluated" handle-goal-evaluated)
(register-event-reducer! "goal.check.completed" handle-goal-check-completed)
(register-event-reducer! "goal.achieved" handle-goal-achieved)
(register-event-reducer! "goal.failed" handle-goal-failed)
(register-event-reducer! "goal.status" handle-goal-status)
