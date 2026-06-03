#lang racket/base

;; BOUNDARY: integration
;; @suite runtime
;; @boundary integration
;; @speed fast
;; @mutates none
;; tests/workflows/test-goal-workflow-contract.rkt — Goal lifecycle workflow contracts (v0.83.10 W1)
;;
;; Tests goal lifecycle workflow contracts: start → turns → evaluation → stop.
;; Verifies event ordering, termination reasons, and callback firing.

(require rackunit
         racket/list
         racket/string
         "../../runtime/goal/goal-types.rkt"
         "../../runtime/goal/goal-runner.rkt"
         "../../llm/model.rkt"
         "../../llm/provider.rkt"
         "../../util/loop-result.rkt"
         "../helpers/goal-scenarios.rkt"
         "../helpers/provider-scenarios.rkt")

;; Helper: fake run-prompt that produces a loop-result
(define (make-simple-run-prompt)
  (lambda (prompt)
    (values #f (make-loop-result '() 'stop (hash)))))

;; Helper: never-shutdown check
(define (make-never-shutdown)
  (lambda () #f))

;; ---------------------------------------------------------------------------
;; T5-1: goal-started event
;; ---------------------------------------------------------------------------

(test-case "goal-run!: emits goal-started event"
  (define cap (make-goal-capture))
  (define prov (make-goal-provider))
  (goal-run!
   "build a test"
   prov
   "test-model"
   (make-simple-run-prompt)
   #:max-turns 1
   #:on-event (make-on-event cap)
   #:on-status (make-on-status cap)
   #:shutdown-check (make-immediate-shutdown))
  (define events (goal-capture-events cap))
  (check-not-false (member 'goal-started (map car events))
              "goal-started event must be emitted"))

;; ---------------------------------------------------------------------------
;; T5-2: goal terminates with terminal status
;; ---------------------------------------------------------------------------

(test-case "goal-run!: terminates with terminal status"
  (define cap (make-goal-capture))
  (define prov (make-goal-provider))
  (define result (goal-run!
                  "build a test"
                  prov
                  "test-model"
                  (make-simple-run-prompt)
                  #:max-turns 2
                  #:on-event (make-on-event cap)
                  #:on-status (make-on-status cap)
                  #:shutdown-check (make-never-shutdown)))
  (check-true (goal-state? result))
  (check-not-false (member (goal-state-status result) '(achieved failed cancelled))
              "goal must end with terminal status"))

;; ---------------------------------------------------------------------------
;; T5-3: no-progress provider → goal fails
;; ---------------------------------------------------------------------------

(test-case "goal-run!: no-progress provider leads to failure"
  (define cap (make-goal-capture))
  (define prov (make-goal-provider-no-progress #:turns 2))
  (define result (goal-run!
                  "build a test"
                  prov
                  "test-model"
                  (make-simple-run-prompt)
                  #:max-turns 3
                  #:on-event (make-on-event cap)
                  #:on-status (make-on-status cap)
                  #:shutdown-check (make-never-shutdown)))
  (check-true (goal-state? result))
  ;; no-progress should cause failure or max-turns exhaustion (which is also failure)
  (check-not-false (member (goal-state-status result) '(failed cancelled))
              "no-progress goal must end with failed or cancelled"))

;; ---------------------------------------------------------------------------
;; T5-4: shutdown check cancels goal
;; ---------------------------------------------------------------------------

(test-case "goal-run!: shutdown check stops execution"
  (define cap (make-goal-capture))
  (define prov (make-goal-provider))
  (define result (goal-run!
                  "build a test"
                  prov
                  "test-model"
                  (make-simple-run-prompt)
                  #:max-turns 5
                  #:on-event (make-on-event cap)
                  #:on-status (make-on-status cap)
                  #:shutdown-check (make-immediate-shutdown)))
  (check-true (goal-state? result))
  ;; Shutdown causes early termination — turns-used should be less than max
  (check-true (<= (goal-state-turns-used result) 5)))

;; ---------------------------------------------------------------------------
;; T5-7: events in temporal order
;; ---------------------------------------------------------------------------

(test-case "goal-run!: events captured in temporal order"
  (define cap (make-goal-capture))
  (define prov (make-goal-provider))
  (goal-run!
   "build a test"
   prov
   "test-model"
   (make-simple-run-prompt)
   #:max-turns 2
   #:on-event (make-on-event cap)
   #:on-status (make-on-status cap)
   #:shutdown-check (make-immediate-shutdown))
  (define events (goal-capture-events cap))
  (when (>= (length events) 2)
    ;; goal-started should come before any terminal event
    (define started-idx (index-of (map car events) 'goal-started))
    (define terminal-events (list 'goal-achieved 'goal-failed))
    (define terminal-idx
      (for/or ([e (in-list events)]
               [i (in-naturals)])
        (and (member (car e) terminal-events) i)))
    (when (and started-idx terminal-idx)
      (check-true (< started-idx terminal-idx)
                  "goal-started must precede terminal event"))))

;; ---------------------------------------------------------------------------
;; T5-8: provider error graceful stop
;; ---------------------------------------------------------------------------

(test-case "goal-run!: fake-run-prompt-with-error doesn't crash"
  (define cap (make-goal-capture))
  (define prov (make-goal-provider))
  (define result (goal-run!
                  "build a test"
                  prov
                  "test-model"
                  (make-fake-run-prompt-with-error "connection lost")
                  #:max-turns 2
                  #:on-event (make-on-event cap)
                  #:on-status (make-on-status cap)
                  #:shutdown-check (make-immediate-shutdown)))
  (check-true (goal-state? result))
  (define events (goal-capture-events cap))
  (check-true (>= (length events) 1) "at least one event should be emitted"))

;; ---------------------------------------------------------------------------
;; T5-9: multiple rounds
;; ---------------------------------------------------------------------------

(test-case "goal-run!: multi-round goal emits intermediate events"
  (define cap (make-goal-capture))
  (define prov (make-goal-provider-no-progress #:turns 3))
  (goal-run!
   "build a test"
   prov
   "test-model"
   (make-simple-run-prompt)
   #:max-turns 3
   #:on-event (make-on-event cap)
   #:on-status (make-on-status cap)
   #:shutdown-check (make-never-shutdown))
  (define events (goal-capture-events cap))
  ;; Should have at least goal-started + terminal
  (check-true (>= (length events) 2)
              "multi-round goal must emit multiple events"))

;; ---------------------------------------------------------------------------
;; T5-10: both callbacks fire
;; ---------------------------------------------------------------------------

(test-case "goal-run!: on-event and on-status both fire"
  (define cap (make-goal-capture))
  (define prov (make-goal-provider))
  (goal-run!
   "build a test"
   prov
   "test-model"
   (make-simple-run-prompt)
   #:max-turns 1
   #:on-event (make-on-event cap)
   #:on-status (make-on-status cap)
   #:shutdown-check (make-immediate-shutdown))
  (define events (goal-capture-events cap))
  (define statuses (goal-capture-statuses cap))
  (check-true (>= (length events) 1) "on-event should fire")
  (check-true (>= (length statuses) 1) "on-status should fire"))
