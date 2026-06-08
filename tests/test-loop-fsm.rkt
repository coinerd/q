#lang racket/base

;; @speed fast
;; @suite default

;; tests/test-loop-fsm.rkt — T-1b tests for agent/loop-fsm.rkt

(require rackunit
         rackunit/text-ui
         (only-in "../util/fsm/fsm.rkt" fsm-states fsm-state-name)
         (only-in "../agent/loop-fsm.rkt"
                  turn-state? turn-event?
                  turn-state->symbol turn-event->symbol
                  next-turn-state valid-turn-transition?
                  turn-state-emit-start
                  turn-state-build-context
                  turn-state-pre-hook
                  turn-state-stream
                  turn-state-post-hook
                  turn-state-complete
                  turn-state-blocked
                  turn-event-start
                  turn-event-context-built
                  turn-event-hook-pass
                  turn-event-hook-block
                  turn-event-stream-complete
                  turn-event-stream-cancel
                  turn-event-post-hook-done
                  turn-event-msg-hook-block
                  current-turn-fsm-state
                  TURN-TRANSITIONS
                  turn-machine))

(define fsm-suite
  (test-suite
   "Loop FSM tests"

   ;; 1. Initial state is emit-start
   (test-case "initial state is emit-start"
     (check-equal? (turn-state->symbol turn-state-emit-start) 'emit-start))

   ;; 2. All valid state transitions
   (test-case "loop-fsm: emit-start + start -> build-context"
     (define next (next-turn-state turn-state-emit-start turn-event-start))
     (check-equal? (turn-state->symbol next) 'build-context))

   (test-case "loop-fsm: build-context + context-built -> pre-hook"
     (define next (next-turn-state turn-state-build-context turn-event-context-built))
     (check-equal? (turn-state->symbol next) 'pre-hook))

   (test-case "loop-fsm: pre-hook + hook-pass -> stream"
     (define next (next-turn-state turn-state-pre-hook turn-event-hook-pass))
     (check-equal? (turn-state->symbol next) 'stream))

   (test-case "loop-fsm: pre-hook + hook-block -> blocked"
     (define next (next-turn-state turn-state-pre-hook turn-event-hook-block))
     (check-equal? (turn-state->symbol next) 'blocked))

   (test-case "loop-fsm: stream + stream-complete -> post-hook"
     (define next (next-turn-state turn-state-stream turn-event-stream-complete))
     (check-equal? (turn-state->symbol next) 'post-hook))

   (test-case "stream + msg-hook-block -> blocked"
     (define next (next-turn-state turn-state-stream turn-event-msg-hook-block))
     (check-equal? (turn-state->symbol next) 'blocked))

   (test-case "loop-fsm: stream + stream-cancel -> complete"
     (define next (next-turn-state turn-state-stream turn-event-stream-cancel))
     (check-equal? (turn-state->symbol next) 'complete))

   (test-case "loop-fsm: post-hook + post-hook-done -> complete"
     (define next (next-turn-state turn-state-post-hook turn-event-post-hook-done))
     (check-equal? (turn-state->symbol next) 'complete))

   (test-case "complete + start -> complete (re-entry)"
     (define next (next-turn-state turn-state-complete turn-event-start))
     (check-equal? (turn-state->symbol next) 'complete))

   (test-case "blocked + start -> blocked (re-entry)"
     (define next (next-turn-state turn-state-blocked turn-event-start))
     (check-equal? (turn-state->symbol next) 'blocked))

   ;; 3. Invalid transitions -> error
   (test-case "invalid transition emits error"
     (check-exn exn:fail?
       (lambda ()
         (next-turn-state turn-state-emit-start turn-event-hook-pass))))

   ;; 4. State predicates
   (test-case "state singletons are fsm-state?"
     (check-true (turn-state? turn-state-emit-start))
     (check-true (turn-state? turn-state-complete)))

   (test-case "event singletons are fsm-event?"
     (check-true (turn-event? turn-event-start))
     (check-true (turn-event? turn-event-stream-complete)))

   ;; 5. valid-turn-transition? checks
   (test-case "valid-turn-transition? returns #t for valid"
     (check-true (valid-turn-transition? turn-state-emit-start turn-event-start)))

   (test-case "valid-turn-transition? returns #f for invalid"
     (check-false (valid-turn-transition? turn-state-emit-start turn-event-hook-pass)))

   ;; 6. FSM parameter default
   (test-case "current-turn-fsm-state default is emit-start"
     (check-equal? (turn-state->symbol (current-turn-fsm-state)) 'emit-start))

   ;; 7. Transition table non-empty
   (test-case "TURN-TRANSITIONS is non-empty"
     (check-true (> (length TURN-TRANSITIONS) 0)))

   ;; 8. turn-machine is valid
   (test-case "turn-machine has expected states"
     (define states (fsm-states turn-machine))
     (check-not-false (member 'emit-start states))
     (check-not-false (member 'complete states))
     (check-not-false (member 'blocked states)))
   ))

(run-tests fsm-suite)
