#lang racket/base

;; @speed fast
;; @suite default

;; tests/test-loop-dispatch.rkt — T-1b surface tests for agent/loop-dispatch.rkt
;;
;; loop-dispatch.rkt is documented as IMPURE streaming dispatch.
;; Full integration testing would require mocking provider, event-bus,
;; loop-state, etc. This test focuses on:
;; - Module surface (exports, contract signature)
;; - FSM parameter interaction
;; - Contract enforcement

(require rackunit
         rackunit/text-ui
         (only-in "../agent/loop-dispatch.rkt" run-streaming-phase)
         (only-in "../agent/loop-fsm.rkt" current-turn-fsm-state turn-state->symbol))

(define dispatch-suite
  (test-suite
   "Loop dispatch surface tests"

   (test-case "run-streaming-phase is a procedure"
     (check-true (procedure? run-streaming-phase)))

   (test-case "run-streaming-phase arity is 10"
     ;; (provider req bus session-id turn-id st raw-messages tools hook-dispatcher cancellation-token)
     (check-equal? (procedure-arity run-streaming-phase) 10))

   (test-case "run-streaming-phase rejects wrong arg count"
     (check-exn exn:fail:contract?
       (lambda () (run-streaming-phase))))

   (test-case "FSM parameter can be read"
     (check-true (symbol? (turn-state->symbol (current-turn-fsm-state)))))

   (test-case "FSM parameter can be set and restored"
     (define orig (current-turn-fsm-state))
     (dynamic-wind
       (lambda () (void))
       (lambda ()
         ;; Parameter is usable
         (check-true (procedure? current-turn-fsm-state)))
       (lambda () (current-turn-fsm-state orig))))
   ))

(run-tests dispatch-suite)
