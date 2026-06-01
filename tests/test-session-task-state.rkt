#lang racket/base

;; tests/test-session-task-state.rkt — tests for task-state session fields + persistence
;; v0.75.1 W1: Session fields + guarded setters
;; v0.76.7 W9: Refactored to use shared session fixture

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
                  task-conclusion-text)
         (only-in "helpers/session-fixture.rkt" make-test-session)
         (only-in "../agent/event-bus.rkt" make-event-bus publish!)
         (only-in "../util/event.rkt" make-event)
         (only-in "../runtime/session-events.rkt" wire-session-event-handlers!))

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
      (define c (task-conclusion "c1" "test" 'fact 'exploration '() 1000 '() '()))
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
      (define c1 (task-conclusion "c1" "first" 'fact 'exploration '() 1000 '() '()))
      (define c2 (task-conclusion "c2" "second" 'decision 'planning '() 2000 '() '()))
      (guarded-set-task-conclusions! s (list c1))
      (guarded-set-task-conclusions! s (append (agent-session-task-conclusions s) (list c2)))
      (check-equal? (length (agent-session-task-conclusions s)) 2))))

(test-case "tool.set-task-state.completed event triggers guarded-set-task-fsm-state! (C3 integration)"
  (define bus (make-event-bus))
  (define sess (make-test-session #:event-bus bus))
  (wire-session-event-handlers! sess (lambda (s e) s))
  ;; Verify initial state
  (check-equal? (agent-session-task-fsm-state sess) (quote idle))
  ;; Publish set-task-state event (as if from tool)
  (publish! bus
            (make-event
             "tool.set-task-state.completed"
             (current-seconds)
             (agent-session-session-id sess)
             #f
             (hasheq (quote target-state) "implementation" (quote event-name) "begin-implement")))
  ;; Verify state transitioned
  (check-equal? (agent-session-task-fsm-state sess) (quote implementation)))
(run-tests suite)
