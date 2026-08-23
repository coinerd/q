#lang racket

;; tests/test-r0-force-reset.rkt
;; R2: Test coverage for force-reset event flow and default budget
;; v0.99.88: Rewritten for session-owned cross-turn state (lifecycle-state).
;;
;; Covers:
;;   - Force-reset lifecycle: produce (session-events) → pending →
;;     consume (prepare-turn-context-state) → reset → cleared
;;   - Session isolation: two sessions don't interfere
;;   - One-shot consumption: signal is consumed exactly once
;;   - Default memory-injection-budget is 500

;; @speed fast  ;; @suite runtime
;; @boundary unit

(require rackunit
         rackunit/text-ui
         "../util/event/event-bus.rkt"
         "../util/event/event.rkt"
         "../runtime/working-set.rkt"
         "../runtime/session/lifecycle-state.rkt"
         (only-in "../runtime/context-assembly/memory-builder.rkt" current-memory-injection-budget))

;; ============================================================
;; Tests
;; ============================================================

(define r0-tests
  (test-suite "r0-tests"

    ;; ============================================================
    ;; R2a: Force-reset lifecycle via session-owned state
    ;; ============================================================

    (test-suite "r0-force-reset-lifecycle"

      (test-case "consume-pending-force-reset! returns #f when not set"
        (define ls (make-lifecycle-state))
        (check-false (consume-pending-force-reset! ls)))

      (test-case "consume-pending-force-reset! returns #t and clears when set"
        (define ls (make-lifecycle-state))
        (set-lifecycle-state-pending-force-reset?! ls #t)
        (check-true (consume-pending-force-reset! ls))
        ;; Second call should return #f — one-shot
        (check-false (consume-pending-force-reset! ls)))

      (test-case "set + consume + verify cleared (full lifecycle)"
        (define ls (make-lifecycle-state))
        ;; Initially false
        (check-false (lifecycle-state-pending-force-reset? ls))
        ;; Producer sets it
        (set-lifecycle-state-pending-force-reset?! ls #t)
        (check-true (lifecycle-state-pending-force-reset? ls))
        ;; Consumer reads and clears
        (check-true (consume-pending-force-reset! ls))
        (check-false (lifecycle-state-pending-force-reset? ls)))

      (test-case "force-reset resets working-set entries"
        (define ws (make-working-set))
        (define ls (make-lifecycle-state))
        ;; Set pending
        (set-lifecycle-state-pending-force-reset?! ls #t)
        ;; Add entries
        (working-set-add! ws "/tmp/a.rkt" "msg-a" 50)
        (working-set-add! ws "/tmp/b.rkt" "msg-b" 75)
        (check-equal? (working-set-entry-count ws) 2)
        ;; Consume signal and reset
        (when (consume-pending-force-reset! ls)
          (working-set-reset! ws))
        (check-equal? (working-set-entry-count ws) 0)
        ;; Signal is consumed
        (check-false (lifecycle-state-pending-force-reset? ls)))

      (test-case "one-shot: second consume after reset returns #f"
        (define ws (make-working-set))
        (define ls (make-lifecycle-state))
        (set-lifecycle-state-pending-force-reset?! ls #t)
        (when (consume-pending-force-reset! ls)
          (working-set-reset! ws))
        ;; Add more entries
        (working-set-add! ws "/tmp/c.rkt" "msg-c" 25)
        ;; Second consume should NOT trigger reset
        (when (consume-pending-force-reset! ls)
          (working-set-reset! ws))
        (check-equal? (working-set-entry-count ws) 1 "second consume should not reset")))

    ;; ============================================================
    ;; R2b: Session isolation
    ;; ============================================================

    (test-suite "r0-session-isolation"

      (test-case "two sessions have independent pending-force-reset"
        (define ls-a (make-lifecycle-state))
        (define ls-b (make-lifecycle-state))
        ;; Session A gets a force-reset
        (set-lifecycle-state-pending-force-reset?! ls-a #t)
        ;; Session B should not see it
        (check-true (lifecycle-state-pending-force-reset? ls-a))
        (check-false (lifecycle-state-pending-force-reset? ls-b))
        ;; Session B consuming should NOT affect session A
        (check-false (consume-pending-force-reset! ls-b))
        (check-true (lifecycle-state-pending-force-reset? ls-a) "session A flag should still be set"))

      (test-case "two sessions have independent prev-task-fsm-state"
        (define ls-a (make-lifecycle-state))
        (define ls-b (make-lifecycle-state))
        ;; Session A transitions to 'planning
        (set-lifecycle-state-prev-task-fsm-state! ls-a 'planning)
        ;; Session B should not see it
        (check-eq? (lifecycle-state-prev-task-fsm-state ls-a) 'planning)
        (check-false (lifecycle-state-prev-task-fsm-state ls-b))))

    ;; ============================================================
    ;; R2c: Default memory-injection-budget is 500
    ;; ============================================================

    (test-suite "r0-default-budget"

      (test-case "current-memory-injection-budget defaults to 500"
        (check-equal? (current-memory-injection-budget) 500 "default budget should be 500"))

      (test-case "current-memory-injection-budget can be parameterized"
        (parameterize ([current-memory-injection-budget 999])
          (check-equal? (current-memory-injection-budget) 999))
        (current-memory-injection-budget 500))

      (test-case "current-memory-injection-budget resets to 500 after parameterize"
        (parameterize ([current-memory-injection-budget 0])
          (check-equal? (current-memory-injection-budget) 0))
        (check-equal? (current-memory-injection-budget)
                      500
                      "should reset to 500 after parameterize")))))

(run-tests r0-tests)
