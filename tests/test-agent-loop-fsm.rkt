#lang racket

;; BOUNDARY: fsm

;; tests/test-agent-loop-fsm.rkt — R-06/R-07: Agent turn FSM tests

(require rackunit
         rackunit/text-ui
         "../agent/loop-fsm.rkt")

(define turn-fsm-suite
  (test-suite "Agent turn FSM tests"

    ;; ── State construction ──
    (test-case "turn states are turn-state structs"
      (for ([s (in-list (list turn-state-emit-start
                              turn-state-build-context
                              turn-state-pre-hook
                              turn-state-stream
                              turn-state-post-hook
                              turn-state-complete
                              turn-state-blocked))])
        (check-pred turn-state? s)))

    (test-case "turn state symbols are correct"
      (check-eq? (turn-state->symbol turn-state-emit-start) 'emit-start)
      (check-eq? (turn-state->symbol turn-state-build-context) 'build-context)
      (check-eq? (turn-state->symbol turn-state-pre-hook) 'pre-hook)
      (check-eq? (turn-state->symbol turn-state-stream) 'stream)
      (check-eq? (turn-state->symbol turn-state-post-hook) 'post-hook)
      (check-eq? (turn-state->symbol turn-state-complete) 'complete)
      (check-eq? (turn-state->symbol turn-state-blocked) 'blocked))

    ;; ── Valid transitions ──
    (test-case "agent-loop-fsm: emit-start + start -> build-context"
      (check-equal? (turn-state->symbol (next-turn-state turn-state-emit-start turn-event-start))
                    'build-context))

    (test-case "agent-loop-fsm: build-context + context-built -> pre-hook"
      (check-equal? (turn-state->symbol (next-turn-state turn-state-build-context
                                                         turn-event-context-built))
                    'pre-hook))

    (test-case "agent-loop-fsm: pre-hook + hook-pass -> stream"
      (check-equal? (turn-state->symbol (next-turn-state turn-state-pre-hook turn-event-hook-pass))
                    'stream))

    (test-case "agent-loop-fsm: pre-hook + hook-block -> blocked"
      (check-equal? (turn-state->symbol (next-turn-state turn-state-pre-hook turn-event-hook-block))
                    'blocked))

    (test-case "agent-loop-fsm: stream + stream-complete -> post-hook"
      (check-equal? (turn-state->symbol (next-turn-state turn-state-stream
                                                         turn-event-stream-complete))
                    'post-hook))

    (test-case "agent-loop-fsm: stream + stream-cancel -> complete"
      (check-equal? (turn-state->symbol (next-turn-state turn-state-stream turn-event-stream-cancel))
                    'complete))

    (test-case "agent-loop-fsm: post-hook + post-hook-done -> complete"
      (check-equal? (turn-state->symbol (next-turn-state turn-state-post-hook
                                                         turn-event-post-hook-done))
                    'complete))

    ;; ── Terminal states ──
    (test-case "agent-loop-fsm: complete is terminal"
      (check-equal? (turn-state->symbol (next-turn-state turn-state-complete turn-event-start))
                    'complete))

    (test-case "blocked is terminal"
      (check-equal? (turn-state->symbol (next-turn-state turn-state-blocked turn-event-start))
                    'blocked))

    ;; ── Invalid transitions ──
    (test-case "agent-loop-fsm: invalid transition raises error"
      (check-exn exn:fail? (lambda () (next-turn-state turn-state-emit-start turn-event-hook-block))))

    ;; ── valid-turn-transition? ──
    (test-case "valid-turn-transition? returns #t for known"
      (check-true (valid-turn-transition? turn-state-pre-hook turn-event-hook-pass)))

    (test-case "valid-turn-transition? returns #f for unknown"
      (check-false (valid-turn-transition? turn-state-blocked turn-event-hook-pass)))

    ;; ── Happy path ──
    (test-case "happy path: emit-start -> build-context -> pre-hook -> stream -> post-hook -> complete"
      (define s0 turn-state-emit-start)
      (define s1 (next-turn-state s0 turn-event-start))
      (check-eq? (turn-state->symbol s1) 'build-context)
      (define s2 (next-turn-state s1 turn-event-context-built))
      (check-eq? (turn-state->symbol s2) 'pre-hook)
      (define s3 (next-turn-state s2 turn-event-hook-pass))
      (check-eq? (turn-state->symbol s3) 'stream)
      (define s4 (next-turn-state s3 turn-event-stream-complete))
      (check-eq? (turn-state->symbol s4) 'post-hook)
      (define s5 (next-turn-state s4 turn-event-post-hook-done))
      (check-eq? (turn-state->symbol s5) 'complete))

    ;; ── Blocked path ──
    (test-case "blocked path: emit-start -> build-context -> pre-hook -> blocked"
      (define s0 turn-state-emit-start)
      (define s1 (next-turn-state s0 turn-event-start))
      (define s2 (next-turn-state s1 turn-event-context-built))
      (define s3 (next-turn-state s2 turn-event-hook-block))
      (check-eq? (turn-state->symbol s3) 'blocked))))

(run-tests turn-fsm-suite 'verbose)
