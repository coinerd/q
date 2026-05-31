#lang racket/base

;; tests/test-task-state.rkt — tests for task-state FSM, conclusion types, and task memory
;; v0.75.0: Foundation milestone tests.

(require rackunit
         rackunit/text-ui
         "../runtime/context-assembly/task-state.rkt"
         (only-in "../util/fsm.rkt" fsm-state-name))

;; ── FSM Tests ──

(define suite
  (test-suite "task-state-fsm"

    ;; ── State singleton identity ──

    (test-case "task-state predicates recognize all 6 states"
      (check-true (task-state? task-idle))
      (check-true (task-state? task-exploration))
      (check-true (task-state? task-planning))
      (check-true (task-state? task-implementation))
      (check-true (task-state? task-verification))
      (check-true (task-state? task-debugging)))

    (test-case "task-state rejects non-FSM values"
      (check-false (task-state? 'idle))
      (check-false (task-state? 42))
      (check-false (task-state? "exploration")))

    ;; ── Transition validity (12 core + 6 revisit + 5 task-complete + force = all) ──

    (test-case "idle → exploration is valid via begin-explore"
      (check-true (task-valid-transition? task-idle task-begin-explore)))

    (test-case "exploration → planning is valid via begin-plan"
      (check-true (task-valid-transition? task-exploration task-begin-plan)))

    (test-case "exploration → implementation is valid via begin-implement"
      (check-true (task-valid-transition? task-exploration task-begin-implement)))

    (test-case "planning → implementation is valid via begin-implement"
      (check-true (task-valid-transition? task-planning task-begin-implement)))

    (test-case "implementation → verification is valid via begin-verify"
      (check-true (task-valid-transition? task-implementation task-begin-verify)))

    (test-case "implementation → debugging is valid via begin-debug"
      (check-true (task-valid-transition? task-implementation task-begin-debug)))

    (test-case "verification → idle is valid via task-complete"
      (check-true (task-valid-transition? task-verification task-task-complete)))

    (test-case "verification → debugging is valid via begin-debug"
      (check-true (task-valid-transition? task-verification task-begin-debug)))

    (test-case "debugging → implementation is valid via begin-implement"
      (check-true (task-valid-transition? task-debugging task-begin-implement)))

    (test-case "debugging → verification is valid via begin-verify"
      (check-true (task-valid-transition? task-debugging task-begin-verify)))

    ;; ── Invalid transitions ──

    (test-case "idle → planning is invalid (must go through exploration)"
      (check-false (task-valid-transition? task-idle task-begin-plan)))

    (test-case "idle → implementation is invalid"
      (check-false (task-valid-transition? task-idle task-begin-implement)))

    (test-case "idle → debugging is invalid"
      (check-false (task-valid-transition? task-idle task-begin-debug)))

    (test-case "exploration → idle is invalid via begin-explore"
      (check-false (task-valid-transition? task-exploration task-begin-explore)))

    ;; ── Revisit (any → exploration) ──

    (test-case "revisit works from all states"
      (for ([state (in-list (list task-idle
                                  task-exploration
                                  task-planning
                                  task-implementation
                                  task-verification
                                  task-debugging))])
        (check-true (task-valid-transition? state task-revisit)
                    (format "revisit should work from ~a" (fsm-state-name state)))))

    ;; ── task-complete (any → idle) ──

    (test-case "task-complete works from all non-idle states"
      (for ([state (in-list (list task-exploration
                                  task-planning
                                  task-implementation
                                  task-verification
                                  task-debugging))])
        (check-true (task-valid-transition? state task-task-complete)
                    (format "task-complete should work from ~a" (fsm-state-name state)))))

    ;; ── force-transition escape hatch ──

    (test-case "force-transition works from any state to any state"
      (for ([from (in-list (list task-idle
                                 task-exploration
                                 task-planning
                                 task-implementation
                                 task-verification
                                 task-debugging))])
        (for ([to-name (in-list '(idle exploration planning implementation verification debugging))])
          (check-true (task-valid-transition? from task-force-transition)
                      (format "force-transition from ~a" (fsm-state-name from))))))

    ;; ── next-state ──

    (test-case "task-next-state returns correct state"
      (check-equal? (fsm-state-name (task-next-state task-idle task-begin-explore)) 'exploration)
      (check-equal? (fsm-state-name (task-next-state task-exploration task-begin-plan)) 'planning)
      (check-equal? (fsm-state-name (task-next-state task-implementation task-begin-verify))
                    'verification)
      (check-equal? (fsm-state-name (task-next-state task-debugging task-begin-implement))
                    'implementation))

    (test-case "task-next-state returns #f for invalid transition"
      (check-false (task-next-state task-idle task-begin-plan)))

    ;; ── Metadata ──

    (test-case "task-states-list returns 6 states"
      (check-equal? (length (task-states-list)) 6))

    (test-case "task-events-list returns 8 events"
      (check-equal? (length (task-events-list)) 8))))

(run-tests suite)
