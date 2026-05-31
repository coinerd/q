#lang racket/base

;; tests/test-state-relevance.rkt — tests for state-relevance matrix
;; v0.76.0 W3: Tighten state-relevance matrix

(require rackunit
         rackunit/text-ui
         "../runtime/context-assembly/state-relevance.rkt"
         (only-in "../runtime/context-assembly/task-state.rkt"
                  task-idle
                  task-exploration
                  task-planning
                  task-implementation
                  task-verification
                  task-debugging))

(define suite
  (test-suite "state-relevance"

    (test-case "context-categories lists 6 categories"
      (check-equal? (length context-categories) 6))

    (test-case "relevance-levels lists 4 levels"
      (check-equal? (length relevance-levels) 4))

    (test-case "idle state defaults"
      (check-equal? (context-level-for-state 'idle 'system-prompt) 'full)
      (check-equal? (context-level-for-state 'idle 'working-set) 'excluded)
      (check-equal? (context-level-for-state 'idle 'conclusions) 'summary))

    (test-case "exploration state favors full context"
      (check-equal? (context-level-for-state 'exploration 'working-set) 'full)
      (check-equal? (context-level-for-state 'exploration 'conclusions) 'full)
      (check-equal? (context-level-for-state 'exploration 'recent-messages) 'full))

    (test-case "planning state excludes working-set"
      (check-equal? (context-level-for-state 'planning 'working-set) 'excluded)
      (check-equal? (context-level-for-state 'planning 'plan-notes) 'full))

    (test-case "implementation state hard-excludes working-set"
      (check-equal? (context-level-for-state 'implementation 'working-set) 'excluded)
      (check-equal? (context-level-for-state 'implementation 'conclusions) 'full)
      (check-equal? (context-level-for-state 'implementation 'recent-messages) 'summary))

    (test-case "verification state includes tool-results"
      (check-equal? (context-level-for-state 'verification 'tool-results) 'full)
      (check-equal? (context-level-for-state 'verification 'working-set) 'filtered))

    (test-case "debugging state includes full working-set"
      (check-equal? (context-level-for-state 'debugging 'working-set) 'full)
      (check-equal? (context-level-for-state 'debugging 'tool-results) 'full))

    (test-case "fsm-state struct input works"
      (check-equal? (context-level-for-state task-implementation 'working-set) 'excluded)
      (check-equal? (context-level-for-state task-exploration 'conclusions) 'full))

    (test-case "unknown state defaults to full"
      (check-equal? (context-level-for-state 'unknown-state 'anything) 'full))

    (test-case "unknown category defaults to full"
      (check-equal? (context-level-for-state 'implementation 'unknown-category) 'full))))

(run-tests suite)
