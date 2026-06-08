#lang racket/base

;; BOUNDARY: regression
;; tests/test-effect-update-fsm-contract.rkt — Regression for effect:update-fsm contract mismatch (v0.55.4)
;;
;; Before fix: contract violation because effect:update-fsm expects symbol?
;; but loop-phases.rkt passes fsm-state? / fsm-event? structs.
;; After fix: should pass.

(require rackunit
         "../agent/loop-phases.rkt"
         "../agent/loop-fsm.rkt"
         (only-in "../agent/event-bus.rkt" make-event-bus)
         "../agent/effect-types.rkt"
         (only-in "../agent/state.rkt" make-loop-state loop-state?)
         (only-in "../util/fsm/fsm.rkt" fsm-state? fsm-event?))

;; ---------------------------------------------------------------------------
;; Regression tests
;; ---------------------------------------------------------------------------

(test-case "phase-emit-start returns effect:update-fsm with FSM struct values"
  (define-values (ctx fx)
    (phase-emit-start "session-1" "1" (make-loop-state "session-1" "1") (list)))
  (check-equal? (length fx) 2 "should return 2 effects")
  (define update-eff (cadr fx))
  (check-true (effect:update-fsm? update-eff)
              "second effect should be effect:update-fsm")
  (check-true (fsm-state? (effect:update-fsm-from-state update-eff))
              "from-state should be fsm-state? struct")
  (check-true (fsm-event? (effect:update-fsm-event update-eff))
              "event should be fsm-event? struct"))

(test-case "phase-build-context returns effect:update-fsm with FSM struct values"
  (define-values (raw-msgs fx)
    (phase-build-context (make-event-bus) "session-1" "1" (make-loop-state "session-1" "1") '()))
  (check-equal? (length fx) 2 "should return 2 effects")
  (define update-eff (cadr fx))
  (check-true (effect:update-fsm? update-eff)
              "second effect should be effect:update-fsm")
  (check-true (fsm-state? (effect:update-fsm-from-state update-eff))
              "from-state should be fsm-state? struct")
  (check-true (fsm-event? (effect:update-fsm-event update-eff))
              "event should be fsm-event? struct"))

(test-case "effect:update-fsm can be constructed with FSM structs"
  (define eff (effect:update-fsm turn-state-emit-start turn-event-start))
  (check-true (effect:update-fsm? eff))
  (check-true (fsm-state? (effect:update-fsm-from-state eff)))
  (check-true (fsm-event? (effect:update-fsm-event eff))))

(require rackunit/text-ui)
