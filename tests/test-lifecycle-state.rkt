#lang racket/base
;; test-lifecycle-state.rkt — Tests for extracted lifecycle-state struct (A1-05)
;; @suite fast

(require rackunit
         (only-in "../runtime/session/lifecycle-state.rkt"
                  lifecycle-state
                  lifecycle-state?
                  make-lifecycle-state
                  lifecycle-state-compacting?
                  lifecycle-state-last-compaction-time
                  lifecycle-state-persisted?
                  lifecycle-state-shutdown-requested?
                  lifecycle-state-force-shutdown?
                  lifecycle-state-prompt-running?
                  lifecycle-state-task-fsm-state
                  lifecycle-state-task-conclusions
                  lifecycle-state-recent-tool-calls
                  set-lifecycle-state-compacting?!
                  set-lifecycle-state-last-compaction-time!
                  set-lifecycle-state-persisted?!
                  set-lifecycle-state-shutdown-requested?!
                  set-lifecycle-state-force-shutdown?!
                  set-lifecycle-state-prompt-running?!
                  set-lifecycle-state-task-fsm-state!
                  set-lifecycle-state-task-conclusions!
                  set-lifecycle-state-recent-tool-calls!)
         (only-in "../runtime/agent-session.rkt" make-agent-session)
         (only-in "../runtime/session/session-types.rkt" agent-session-lifecycle)
         (only-in "../agent/event-bus.rkt" make-event-bus))

;; ============================================================
;; Construction
;; ============================================================

(test-case "make-lifecycle-state returns correct defaults"
  (define ls (make-lifecycle-state))
  (check-true (lifecycle-state? ls))
  (check-false (lifecycle-state-compacting? ls))
  (check-false (lifecycle-state-last-compaction-time ls))
  (check-false (lifecycle-state-persisted? ls))
  (check-false (lifecycle-state-shutdown-requested? ls))
  (check-false (lifecycle-state-force-shutdown? ls))
  (check-false (lifecycle-state-prompt-running? ls))
  (check-false (lifecycle-state-task-fsm-state ls))
  (check-equal? (lifecycle-state-task-conclusions ls) '())
  (check-equal? (lifecycle-state-recent-tool-calls ls) '()))

;; ============================================================
;; Mutation
;; ============================================================

(test-case "lifecycle-state mutable setters work"
  (define ls (make-lifecycle-state))
  (set-lifecycle-state-compacting?! ls #t)
  (check-true (lifecycle-state-compacting? ls))
  (set-lifecycle-state-compacting?! ls #f)
  (check-false (lifecycle-state-compacting? ls)))

(test-case "lifecycle-state last-compaction-time tracks timestamps"
  (define ls (make-lifecycle-state))
  (set-lifecycle-state-last-compaction-time! ls 12345)
  (check-equal? (lifecycle-state-last-compaction-time ls) 12345))

(test-case "lifecycle-state shutdown flags work independently"
  (define ls (make-lifecycle-state))
  (set-lifecycle-state-shutdown-requested?! ls #t)
  (check-true (lifecycle-state-shutdown-requested? ls))
  (check-false (lifecycle-state-force-shutdown? ls))
  (set-lifecycle-state-force-shutdown?! ls #t)
  (check-true (lifecycle-state-force-shutdown? ls)))

(test-case "lifecycle-state prompt-running tracks execution"
  (define ls (make-lifecycle-state))
  (set-lifecycle-state-prompt-running?! ls #t)
  (check-true (lifecycle-state-prompt-running? ls))
  (set-lifecycle-state-prompt-running?! ls #f)
  (check-false (lifecycle-state-prompt-running? ls)))

(test-case "lifecycle-state task-fsm-state evolves"
  (define ls (make-lifecycle-state))
  (check-false (lifecycle-state-task-fsm-state ls))
  (set-lifecycle-state-task-fsm-state! ls 'implementing)
  (check-eq? (lifecycle-state-task-fsm-state ls) 'implementing))

(test-case "lifecycle-state task-conclusions accumulate"
  (define ls (make-lifecycle-state))
  (set-lifecycle-state-task-conclusions! ls '((task . "done")))
  (check-equal? (lifecycle-state-task-conclusions ls) '((task . "done"))))

(test-case "lifecycle-state recent-tool-calls tracks history"
  (define ls (make-lifecycle-state))
  (set-lifecycle-state-recent-tool-calls! ls '(bash read))
  (check-equal? (lifecycle-state-recent-tool-calls ls) '(bash read)))

;; ============================================================
;; agent-session integration
;; ============================================================

(test-case "agent-session-lifecycle returns lifecycle-state"
  (define bus (make-event-bus))
  (define sess (make-agent-session
                (hasheq 'provider #f
                        'tool-registry #f
                        'event-bus bus
                        'session-dir "/tmp/q-lifecycle-test"
                        'model-name "test")))
  (define ls (agent-session-lifecycle sess))
  (check-true (lifecycle-state? ls))
  (check-false (lifecycle-state-compacting? ls))
  (check-false (lifecycle-state-shutdown-requested? ls)))
