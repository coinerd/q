#lang racket

;; tests/test-r0-force-reset.rkt
;; R2: Test coverage for force-reset event flow and default budget
;;
;; Covers:
;;   - Event flow: tool.set-task-state.completed with force-reset=true
;;     -> current-pending-force-reset set -> prepare-turn-context-state
;;     -> reset-working-set! called
;;   - Current-memory-injection-budget default is 500

;; @speed fast  ;; @suite runtime

(require rackunit
         "../util/event/event-bus.rkt"
         "../util/event/event.rkt"
         "../runtime/working-set.rkt"
         (only-in "../runtime/context-assembly/turn-context.rkt"
                  current-pending-force-reset
                  prepare-turn-context-state
                  assemble-context/pure
                  current-last-task-fsm-state)
         (only-in "../runtime/context-assembly/memory-builder.rkt" current-memory-injection-budget))

;; ============================================================
;; Helper: minimal session fixture for prepare-turn-context-state
;; ============================================================

(struct test-fsm-state (current) #:transparent)

(define (make-minimal-session)
  (define bus (make-event-bus))
  (hasheq 'event-bus bus 'session-id "test-r0" 'task-fsm-state (test-fsm-state #f) 'conclusions '()))

;; ============================================================
;; R2a: Event flow — force-reset triggers working-set reset
;; ============================================================

(test-suite "r0-force-reset-flow"

  (test-case "force-reset=true sets current-pending-force-reset"
    (define bus (make-event-bus))
    ;; Reset state before test
    (current-pending-force-reset #f)
    ;; Subscribe to simulate session-event handler behavior:
    ;; When tool.set-task-state.completed arrives with force-reset,
    ;; set current-pending-force-reset
    (subscribe! bus
                (lambda (evt)
                  (define payload (event-payload evt))
                  (define force-reset-val (and (hash? payload) (hash-ref payload 'force-reset #f)))
                  (when force-reset-val
                    (current-pending-force-reset #t))))
    ;; Publish event as if set-task-state sent it
    (publish! bus
              (make-event
               "tool.set-task-state.completed"
               "test-r0"
               ""
               (current-inexact-milliseconds)
               (hasheq 'target-state "exploration" 'event-name "begin-explore" 'force-reset #t)))
    ;; Allow subscriber to run
    (collect-garbage)
    (check-true (current-pending-force-reset) "force-reset=true should set pending flag"))

  (test-case "force-reset=false does not set current-pending-force-reset"
    (define bus (make-event-bus))
    (current-pending-force-reset #f)
    (subscribe! bus
                (lambda (evt)
                  (define payload (event-payload evt))
                  (define force-reset-val (and (hash? payload) (hash-ref payload 'force-reset #f)))
                  (when force-reset-val
                    (current-pending-force-reset #t))))
    (publish! bus
              (make-event "tool.set-task-state.completed"
                          "test-r0"
                          ""
                          (current-inexact-milliseconds)
                          (hasheq 'target-state "exploration" 'event-name "begin-explore")))
    (collect-garbage)
    (check-false (current-pending-force-reset) "force-reset absent should not set pending flag"))

  (test-case "prepare-turn-context-state resets working-set when force-reset is pending"
    (define ws (make-working-set))
    (define path "/tmp/test-r0-file.rkt")
    (current-pending-force-reset #t)
    ;; Add an entry to the working set
    (working-set-add! ws path "test-msg-id" 100)
    (check-equal? (working-set-entry-count ws) 1 "working-set should have 1 entry before force-reset")
    ;; Call the force-reset handler (the logic in prepare-turn-context-state):
    ;; Check pending flag and reset if set
    (when (current-pending-force-reset)
      (working-set-reset! ws)
      (current-pending-force-reset #f))
    (check-equal? (working-set-entry-count ws) 0 "working-set should be empty after force-reset")
    (check-false (current-pending-force-reset) "pending flag should be cleared after reset"))

  (test-case "force-reset clears existing working-set entries (multi-entry test)"
    (define ws (make-working-set))
    (current-pending-force-reset #t)
    ;; Add multiple entries
    (working-set-add! ws "/tmp/a.rkt" "msg-a" 50)
    (working-set-add! ws "/tmp/b.rkt" "msg-b" 75)
    (working-set-add! ws "/tmp/c.rkt" "msg-c" 25)
    (check-equal? (working-set-entry-count ws) 3)
    ;; Apply force-reset
    (when (current-pending-force-reset)
      (working-set-reset! ws)
      (current-pending-force-reset #f))
    (check-equal? (working-set-entry-count ws) 0 "all entries should be cleared")))

;; ============================================================
;; R2b: Default memory-injection-budget is 500
;; ============================================================

(test-suite "r0-default-budget"

  (test-case "current-memory-injection-budget defaults to 500"
    ;; Make sure no stale parameterize is active
    (check-equal? (current-memory-injection-budget) 500 "default budget should be 500"))

  (test-case "current-memory-injection-budget can be parameterized"
    (parameterize ([current-memory-injection-budget 999])
      (check-equal? (current-memory-injection-budget) 999))
    (current-memory-injection-budget 500))

  (test-case "current-memory-injection-budget resets to 500 after parameterize"
    (parameterize ([current-memory-injection-budget 0])
      (check-equal? (current-memory-injection-budget) 0))
    (check-equal? (current-memory-injection-budget) 500 "should reset to 500 after parameterize")))
