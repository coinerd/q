#lang racket

;; tests/test-session-owned-cross-turn-state.rkt
;; v0.99.88: Session isolation and cross-turn behavior tests for
;; prev-task-fsm-state and pending-force-reset? lifecycle-state fields.
;;
;; Covers:
;;   - Session A's prev-task state doesn't leak to Session B
;;   - Session A's pending-force-reset doesn't reset Session B
;;   - Cross-turn prev-task-state transitions
;;   - Force-reset consumed exactly once
;;   - Exception semantics: consume-before-reset

;; @speed fast  ;; @suite runtime

(require rackunit
         rackunit/text-ui
         "../runtime/working-set.rkt"
         "../runtime/session/lifecycle-state.rkt")

;; ============================================================
;; Tests
;; ============================================================

(define cross-turn-tests
  (test-suite "session-owned-cross-turn-state"

    ;; ============================================================
    ;; Previous task FSM state — session isolation
    ;; ============================================================

    (test-suite "prev-task-fsm-state-isolation"

      (test-case "Session A exploration→planning does not leak to Session B"
        ;; Session A: set prev-task-fsm-state to 'planning
        (define ls-a (make-lifecycle-state))
        (set-lifecycle-state-prev-task-fsm-state! ls-a 'planning)

        ;; Session B: fresh session, prev should be #f
        (define ls-b (make-lifecycle-state))
        (check-false (lifecycle-state-prev-task-fsm-state ls-b)
                     "Session B must not see Session A's previous planning state"))

      (test-case "Independent prev-task-state evolution"
        (define ls-a (make-lifecycle-state))
        (define ls-b (make-lifecycle-state))

        ;; Session A: turn 1 → exploration
        (set-lifecycle-state-prev-task-fsm-state! ls-a 'exploration)
        ;; Session B: turn 1 → planning
        (set-lifecycle-state-prev-task-fsm-state! ls-b 'planning)

        (check-eq? (lifecycle-state-prev-task-fsm-state ls-a) 'exploration)
        (check-eq? (lifecycle-state-prev-task-fsm-state ls-b) 'planning)

        ;; Session A turn 2 → planning (different state)
        (set-lifecycle-state-prev-task-fsm-state! ls-a 'planning)
        ;; Session B should be unaffected
        (check-eq? (lifecycle-state-prev-task-fsm-state ls-b) 'planning)))

    ;; ============================================================
    ;; Cross-turn behavior: prev-task-state transitions
    ;; ============================================================

    (test-suite "prev-task-fsm-state-cross-turn"

      (test-case "Turn N: state changes → prev recorded; Turn N+1: same state → no dup"
        (define ls (make-lifecycle-state))
        (define ws (make-working-set))

        ;; Turn N: prev=#f, current='exploration → transition detected
        (define prev-1 (lifecycle-state-prev-task-fsm-state ls))
        (define current-1 'exploration)
        (check-false prev-1)
        (check-not-eq? prev-1 current-1 "transition detected")
        (set-lifecycle-state-prev-task-fsm-state! ls current-1)

        ;; Turn N+1: prev='exploration, current='exploration → NO transition
        (define prev-2 (lifecycle-state-prev-task-fsm-state ls))
        (define current-2 'exploration)
        (check-eq? prev-2 current-2 "no transition — same state")
        ;; No WS evolution should occur

        ;; Turn N+2: prev='exploration, current='planning → transition detected
        (define prev-3 (lifecycle-state-prev-task-fsm-state ls))
        (define current-3 'planning)
        (check-not-eq? prev-3 current-3 "transition detected")
        (set-lifecycle-state-prev-task-fsm-state! ls current-3)))

    ;; ============================================================
    ;; Pending force-reset — session isolation
    ;; ============================================================

    (test-suite "pending-force-reset-isolation"

      (test-case "Session A receives force-reset, Session B does not"
        (define ls-a (make-lifecycle-state))
        (define ls-b (make-lifecycle-state))

        ;; Session A gets a force-reset event
        (set-lifecycle-state-pending-force-reset?! ls-a #t)

        ;; Session B should not be affected
        (check-true (lifecycle-state-pending-force-reset? ls-a))
        (check-false (lifecycle-state-pending-force-reset? ls-b)))

      (test-case "Session B builds context → no reset; Session A → reset exactly once"
        (define ls-a (make-lifecycle-state))
        (define ls-b (make-lifecycle-state))
        (define ws-a (make-working-set))
        (define ws-b (make-working-set))

        ;; Both have entries
        (working-set-add! ws-a "/tmp/a.rkt" "msg-a" 50)
        (working-set-add! ws-b "/tmp/b.rkt" "msg-b" 50)

        ;; Session A gets force-reset
        (set-lifecycle-state-pending-force-reset?! ls-a #t)

        ;; Session B builds context — should NOT reset
        (when (consume-pending-force-reset! ls-b)
          (working-set-reset! ws-b))
        (check-equal? (working-set-entry-count ws-b) 1 "Session B WS should NOT be reset")

        ;; Session A builds next context — reset occurs
        (when (consume-pending-force-reset! ls-a)
          (working-set-reset! ws-a))
        (check-equal? (working-set-entry-count ws-a) 0 "Session A WS should be reset")

        ;; Session A builds again — no additional reset
        (working-set-add! ws-a "/tmp/c.rkt" "msg-c" 30)
        (when (consume-pending-force-reset! ls-a)
          (working-set-reset! ws-a))
        (check-equal? (working-set-entry-count ws-a) 1 "no additional reset — signal consumed")))

    ;; ============================================================
    ;; Cross-turn: force-reset lifecycle
    ;; ============================================================

    (test-suite "force-reset-cross-turn"

      (test-case "Turn N: event; Turn N+1: reset; Turn N+2: no additional reset"
        (define ls (make-lifecycle-state))
        (define ws (make-working-set))

        ;; Turn N: force-reset event occurs
        (set-lifecycle-state-pending-force-reset?! ls #t)
        (check-true (lifecycle-state-pending-force-reset? ls))

        ;; Turn N+1: context preparation
        (working-set-add! ws "/tmp/x.rkt" "msg-x" 100)
        (when (consume-pending-force-reset! ls)
          (working-set-reset! ws))
        (check-equal? (working-set-entry-count ws) 0 "WS resets")
        (check-false (lifecycle-state-pending-force-reset? ls) "flag cleared")

        ;; Turn N+2: no additional reset
        (working-set-add! ws "/tmp/y.rkt" "msg-y" 100)
        (when (consume-pending-force-reset! ls)
          (working-set-reset! ws))
        (check-equal? (working-set-entry-count ws) 1 "no additional reset")))

    ;; ============================================================
    ;; Exception semantics: consume-before-reset
    ;; ============================================================

    (test-suite "exception-semantics"

      (test-case "consume-before-reset: signal consumed even if reset throws"
        (define ls (make-lifecycle-state))
        (set-lifecycle-state-pending-force-reset?! ls #t)

        ;; Simulate: consume succeeds, reset throws
        (define consumed? (consume-pending-force-reset! ls))
        (check-true consumed? "signal consumed")
        ;; Simulate reset failing
        (with-handlers ([exn:fail? (lambda (_) (void))])
          (error 'reset-working-set! "simulated failure"))

        ;; Signal is consumed — will NOT retry next turn
        (check-false (lifecycle-state-pending-force-reset? ls)
                     "signal consumed even if reset throws — does NOT retry")))

    ;; ============================================================
    ;; Lifecycle-state defaults
    ;; ============================================================

    (test-suite "defaults"

      (test-case "fresh lifecycle-state has correct defaults"
        (define ls (make-lifecycle-state))
        (check-false (lifecycle-state-prev-task-fsm-state ls))
        (check-false (lifecycle-state-pending-force-reset? ls))))))

(run-tests cross-turn-tests)
