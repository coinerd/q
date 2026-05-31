#lang racket/base

;; tests/test-session-task-state.rkt — tests for task-state session fields + persistence
;; v0.75.1 W1: Session fields + guarded setters

(require rackunit
         rackunit/text-ui
         "../runtime/session-types.rkt"
         "../runtime/session-mutation.rkt"
         (only-in "../runtime/context-assembly/task-state.rkt"
                  task-idle
                  task-exploration
                  task-planning
                  task-implementation
                  task-verification
                  task-debugging)
         (only-in "../runtime/context-assembly/task-conclusion.rkt"
                  task-conclusion
                  task-conclusion?
                  task-conclusion-text))

;; Helper: create a minimal agent-session with default fields
(define (make-test-session)
  (apply agent-session
         (list "test-session"
               "/tmp/test-session"
               #f ; provider
               #f ; tool-registry
               #f ; event-bus
               #f ; extension-registry
               #f ; model-name
               '() ; system-instructions
               #f ; index
               #f ; queue
               (hasheq) ; config
               #f ; active?
               0 ; start-time
               #f ; compacting?
               #f ; last-compaction-time
               #f ; persisted?
               '() ; pending-entries
               #f ; thinking-level
               #f ; shutdown-requested?
               #f ; force-shutdown?
               #f ; prompt-running?
               'idle ; task-fsm-state
               '() ; task-conclusions
               '() ; recent-tool-calls
               )))

(define suite
  (test-suite "session-task-state"

    ;; ── Default values ──

    (test-case "new session defaults to idle task state"
      (define s (make-test-session))
      (check-equal? (agent-session-task-fsm-state s) 'idle))

    (test-case "new session defaults to empty conclusions"
      (define s (make-test-session))
      (check-equal? (agent-session-task-conclusions s) '()))

    ;; ── Guarded setters ──

    (test-case "guarded-set-task-fsm-state! updates state"
      (define s (make-test-session))
      (guarded-set-task-fsm-state! s 'exploration)
      (check-equal? (agent-session-task-fsm-state s) 'exploration))

    (test-case "guarded-set-task-conclusions! updates conclusions"
      (define s (make-test-session))
      (define c (task-conclusion "c1" "test" 'fact 'exploration '() 1000 '()))
      (guarded-set-task-conclusions! s (list c))
      (check-equal? (length (agent-session-task-conclusions s)) 1)
      (check-equal? (task-conclusion-text (car (agent-session-task-conclusions s))) "test"))

    (test-case "guarded-set-task-fsm-state! allows any symbol"
      (define s (make-test-session))
      (for ([state (in-list '(idle exploration planning implementation verification debugging))])
        (guarded-set-task-fsm-state! s state)
        (check-equal? (agent-session-task-fsm-state s) state)))

    (test-case "guarded-set-task-conclusions! can accumulate"
      (define s (make-test-session))
      (define c1 (task-conclusion "c1" "first" 'fact 'exploration '() 1000 '()))
      (define c2 (task-conclusion "c2" "second" 'decision 'planning '() 2000 '()))
      (guarded-set-task-conclusions! s (list c1))
      (guarded-set-task-conclusions! s (append (agent-session-task-conclusions s) (list c2)))
      (check-equal? (length (agent-session-task-conclusions s)) 2))))

(run-tests suite)
