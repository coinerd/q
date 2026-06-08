#lang racket

;; BOUNDARY: fsm

;; tests/test-iteration-fsm.rkt — R-06/R-07: Iteration FSM transition tests

(require rackunit
         rackunit/text-ui
         "../runtime/iteration/fsm-types.rkt"
         (only-in "../agent/iteration/main-loop.rkt" current-iteration-fsm-state))

(define fsm-suite
  (test-suite "Iteration FSM tests"

    ;; ── State construction ──
    (test-case "states are iteration-state structs"
      (for ([s (in-list (list state-idle
                              state-provider-turn
                              state-tool-exec
                              state-decision
                              state-complete
                              state-retrying
                              state-aborted))])
        (check-pred iteration-state? s)))

    (test-case "state symbols are correct"
      (check-eq? (state->symbol state-idle) 'idle)
      (check-eq? (state->symbol state-provider-turn) 'provider-turn)
      (check-eq? (state->symbol state-tool-exec) 'tool-exec)
      (check-eq? (state->symbol state-decision) 'decision)
      (check-eq? (state->symbol state-complete) 'complete)
      (check-eq? (state->symbol state-retrying) 'retrying)
      (check-eq? (state->symbol state-aborted) 'aborted))

    ;; ── Event construction ──
    (test-case "events are iteration-event structs"
      (for ([e (in-list (list event-start-loop
                              event-model-response
                              event-tool-result
                              event-tool-calls-present
                              event-termination-reason
                              event-hook-block
                              event-error
                              event-retry-requested
                              event-cancel))])
        (check-pred iteration-event? e)))

    ;; ── Valid transitions ──
    (test-case "idle + start-loop -> provider-turn"
      (check-equal? (state->symbol (next-iteration-state state-idle event-start-loop))
                    'provider-turn))

    (test-case "provider-turn + model-response -> decision"
      (check-equal? (state->symbol (next-iteration-state state-provider-turn event-model-response))
                    'decision))

    (test-case "provider-turn + hook-block -> aborted"
      (check-equal? (state->symbol (next-iteration-state state-provider-turn event-hook-block))
                    'aborted))

    (test-case "provider-turn + cancel -> aborted"
      (check-equal? (state->symbol (next-iteration-state state-provider-turn event-cancel)) 'aborted))

    (test-case "provider-turn + error -> retrying"
      (check-equal? (state->symbol (next-iteration-state state-provider-turn event-error)) 'retrying))

    (test-case "decision + tool-calls-present -> tool-exec"
      (check-equal? (state->symbol (next-iteration-state state-decision event-tool-calls-present))
                    'tool-exec))

    (test-case "decision + termination-reason -> complete"
      (check-equal? (state->symbol (next-iteration-state state-decision event-termination-reason))
                    'complete))

    (test-case "decision + hook-block -> aborted"
      (check-equal? (state->symbol (next-iteration-state state-decision event-hook-block)) 'aborted))

    (test-case "tool-exec + tool-result -> decision"
      (check-equal? (state->symbol (next-iteration-state state-tool-exec event-tool-result))
                    'decision))

    (test-case "tool-exec + error -> retrying"
      (check-equal? (state->symbol (next-iteration-state state-tool-exec event-error)) 'retrying))

    (test-case "retrying + retry-requested -> provider-turn"
      (check-equal? (state->symbol (next-iteration-state state-retrying event-retry-requested))
                    'provider-turn))

    (test-case "retrying + error -> aborted"
      (check-equal? (state->symbol (next-iteration-state state-retrying event-error)) 'aborted))

    ;; ── Terminal states ──
    (test-case "iteration-fsm: complete is terminal"
      (check-equal? (state->symbol (next-iteration-state state-complete event-cancel)) 'complete))

    (test-case "aborted is terminal"
      (check-equal? (state->symbol (next-iteration-state state-aborted event-cancel)) 'aborted))

    ;; ── valid-transition? ──
    (test-case "valid-transition? returns #t for known transitions"
      (check-true (valid-transition? state-idle event-start-loop))
      (check-true (valid-transition? state-decision event-tool-calls-present)))

    (test-case "valid-transition? returns #f for unknown transitions"
      (check-false (valid-transition? state-idle event-model-response))
      (check-false (valid-transition? state-complete event-start-loop)))

    ;; ── Invalid transitions raise error ──
    (test-case "iteration-fsm: invalid transition raises error"
      (check-exn exn:fail? (lambda () (next-iteration-state state-idle event-model-response))))

    (test-case "invalid transition from complete raises error"
      (check-exn exn:fail? (lambda () (next-iteration-state state-complete event-start-loop))))

    ;; ── Full cycle test ──
    (test-case "happy path: idle -> provider-turn -> decision -> tool-exec -> decision -> complete"
      (define s0 state-idle)
      (define s1 (next-iteration-state s0 event-start-loop))
      (check-eq? (state->symbol s1) 'provider-turn)
      (define s2 (next-iteration-state s1 event-model-response))
      (check-eq? (state->symbol s2) 'decision)
      (define s3 (next-iteration-state s2 event-tool-calls-present))
      (check-eq? (state->symbol s3) 'tool-exec)
      (define s4 (next-iteration-state s3 event-tool-result))
      (check-eq? (state->symbol s4) 'decision)
      (define s5 (next-iteration-state s4 event-termination-reason))
      (check-eq? (state->symbol s5) 'complete))

    ;; -- FSM state parameter integration (N-04) --
    (test-case "current-iteration-fsm-state parameter defaults to state-idle"
      (check-eq? (current-iteration-fsm-state) state-idle))

    (test-case "current-iteration-fsm-state tracks transitions"
      (parameterize ([current-iteration-fsm-state state-idle])
        (current-iteration-fsm-state (next-iteration-state state-idle event-start-loop))
        (check-eq? (current-iteration-fsm-state) state-provider-turn)
        (current-iteration-fsm-state (next-iteration-state state-provider-turn event-model-response))
        (check-eq? (current-iteration-fsm-state) state-decision)
        (current-iteration-fsm-state (next-iteration-state state-decision event-termination-reason))
        (check-eq? (current-iteration-fsm-state) state-complete)))))

(run-tests fsm-suite 'verbose)
