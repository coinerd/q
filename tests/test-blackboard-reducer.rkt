#lang racket

;; @speed fast  ;; @suite extensions

;; tests/test-blackboard-reducer.rkt — W2 (v0.99.7) Pure Event Reducer Tests
;;
;; Tests for the pure event reducer:
;; - Each event type produces correct state transition
;; - Unknown event → state unchanged
;; - Multiple events in sequence → correct accumulated state
;; - Event with missing fields → safe defaults

(require rackunit
         rackunit/text-ui
         "../agent/blackboard.rkt"
         "../agent/blackboard-reducer.rkt")

;; Helper: create a GSD-wrapped event hash (matches emit-gsd-event! format)
(define (make-gsd-event name data)
  (hasheq 'event name 'data data 'timestamp 1000 'correlation-id #f))

(define suite
  (test-suite "Blackboard Reducer (W2 v0.99.7)"

    ;; ── Wave Status Events ──

    (test-case "gsd.wave.started → wave-status in-progress"
      (define evt (make-gsd-event 'gsd.wave.started (hasheq 'wave "W0")))
      (define result (apply-event empty-blackboard evt))
      (check-equal? (hash-ref (blackboard-state-wave-status result) "W0") 'in-progress))

    (test-case "gsd.wave.started with symbol wave name"
      (define evt (make-gsd-event 'gsd.wave.started (hasheq 'wave 'W0)))
      (define result (apply-event empty-blackboard evt))
      (check-equal? (hash-ref (blackboard-state-wave-status result) "W0") 'in-progress))

    (test-case "gsd.wave.completed → wave-status completed"
      (define evt (make-gsd-event 'gsd.wave.completed (hasheq 'wave "W1")))
      (define result (apply-event empty-blackboard evt))
      (check-equal? (hash-ref (blackboard-state-wave-status result) "W1") 'completed))

    (test-case "gsd.wave.failed → wave-status failed"
      (define evt (make-gsd-event 'gsd.wave.failed (hasheq 'wave "W2" 'error "oops")))
      (define result (apply-event empty-blackboard evt))
      (check-equal? (hash-ref (blackboard-state-wave-status result) "W2") 'failed))

    (test-case "gsd.wave.skipped → wave-status skipped"
      (define evt (make-gsd-event 'gsd.wave.skipped (hasheq 'wave "W3" 'reason "dep failed")))
      (define result (apply-event empty-blackboard evt))
      (check-equal? (hash-ref (blackboard-state-wave-status result) "W3") 'skipped))

    ;; ── Plan Parsed Event ──

    (test-case "gsd.plan.parsed → active-plan set"
      (define evt (make-gsd-event 'gsd.plan.parsed (hasheq 'wave-count 8)))
      (define result (apply-event empty-blackboard evt))
      (define plan (blackboard-state-active-plan result))
      (check-true (hash? plan))
      (check-equal? (hash-ref plan 'wave-count) 8))

    ;; ── Verification Events ──

    (test-case "gsd.verification.completed → verifier-decisions appended"
      (define evt
        (make-gsd-event 'gsd.verification.completed
                        (hasheq 'verdict 'approve 'reason "looks good" 'risk-level 'low)))
      (define result (apply-event empty-blackboard evt))
      (define decisions (blackboard-state-verifier-decisions result))
      (check-equal? (length decisions) 1)
      (check-equal? (hash-ref (car decisions) 'verdict) 'approve))

    (test-case "gsd.verification.escalated → verifier-decisions appended with escalate"
      (define evt (make-gsd-event 'gsd.verification.escalated (hasheq 'reason "risky")))
      (define result (apply-event empty-blackboard evt))
      (define decisions (blackboard-state-verifier-decisions result))
      (check-equal? (length decisions) 1)
      (check-equal? (hash-ref (car decisions) 'verdict) 'escalate)
      (check-true (hash-ref (car decisions) 'requires-human?)))

    ;; ── MAS Events ──

    (test-case "mas.artifact.produced → artifact-refs appended"
      (define evt
        (make-gsd-event 'mas.artifact.produced
                        (hasheq 'name "plan.md" 'path "/tmp/plan.md" 'artifact-type 'markdown)))
      (define result (apply-event empty-blackboard evt))
      (define artifacts (blackboard-state-artifact-refs result))
      (check-equal? (length artifacts) 1)
      (check-equal? (hash-ref (car artifacts) 'name) "plan.md"))

    (test-case "mas.test.result → test-results appended"
      (define evt
        (make-gsd-event 'mas.test.result (hasheq 'file "test.rkt" 'result 'pass 'duration-ms 42)))
      (define result (apply-event empty-blackboard evt))
      (define results (blackboard-state-test-results result))
      (check-equal? (length results) 1)
      (check-equal? (hash-ref (car results) 'file) "test.rkt")
      (check-equal? (hash-ref (car results) 'result) 'pass))

    (test-case "mas.hypothesis.opened → open-hypotheses appended"
      (define evt
        (make-gsd-event 'mas.hypothesis.opened
                        (hasheq 'id "h1" 'question "why?" 'agent-name "explorer")))
      (define result (apply-event empty-blackboard evt))
      (define hyps (blackboard-state-open-hypotheses result))
      (check-equal? (length hyps) 1)
      (check-equal? (hash-ref (car hyps) 'id) "h1"))

    (test-case "mas.hypothesis.resolved → open-hypotheses removed"
      ;; First open a hypothesis
      (define open-evt
        (make-gsd-event 'mas.hypothesis.opened
                        (hasheq 'id "h1" 'question "why?" 'agent-name "explorer")))
      (define state1 (apply-event empty-blackboard open-evt))
      (check-equal? (length (blackboard-state-open-hypotheses state1)) 1)
      ;; Then resolve it
      (define resolve-evt (make-gsd-event 'mas.hypothesis.resolved (hasheq 'id "h1")))
      (define state2 (apply-event state1 resolve-evt))
      (check-equal? (length (blackboard-state-open-hypotheses state2)) 0))

    ;; ── Unknown Event ──

    (test-case "unknown event → state unchanged"
      (define evt (make-gsd-event 'some.unknown.event (hasheq 'foo "bar")))
      (define result (apply-event empty-blackboard evt))
      (check-eq? result empty-blackboard))

    (test-case "event with #f event name → state unchanged"
      (define evt (hasheq 'data (hasheq 'foo "bar")))
      (define result (apply-event empty-blackboard evt))
      (check-eq? result empty-blackboard))

    ;; ── Missing Fields → Safe Defaults ──

    (test-case "wave.started with missing wave field → defaults to unknown"
      (define evt (make-gsd-event 'gsd.wave.started (hasheq)))
      (define result (apply-event empty-blackboard evt))
      (check-equal? (hash-ref (blackboard-state-wave-status result) "unknown") 'in-progress))

    (test-case "plan.parsed with missing wave-count → defaults to 0"
      (define evt (make-gsd-event 'gsd.plan.parsed (hasheq)))
      (define result (apply-event empty-blackboard evt))
      (check-equal? (hash-ref (blackboard-state-active-plan result) 'wave-count) 0))

    (test-case "test.result with missing fields → safe defaults"
      (define evt (make-gsd-event 'mas.test.result (hasheq)))
      (define result (apply-event empty-blackboard evt))
      (define results (blackboard-state-test-results result))
      (check-equal? (length results) 1)
      (check-equal? (hash-ref (car results) 'file) "unknown")
      (check-equal? (hash-ref (car results) 'result) 'unknown))

    ;; ── Sequential Application ──

    (test-case "multiple events in sequence → correct accumulated state"
      (define events
        (list (make-gsd-event 'gsd.plan.parsed (hasheq 'wave-count 4))
              (make-gsd-event 'gsd.wave.started (hasheq 'wave "W0"))
              (make-gsd-event 'gsd.wave.completed (hasheq 'wave "W0"))
              (make-gsd-event 'gsd.wave.started (hasheq 'wave "W1"))
              (make-gsd-event 'mas.test.result (hasheq 'file "test.rkt" 'result 'pass))
              (make-gsd-event 'gsd.verification.completed (hasheq 'verdict 'approve))))
      (define result (apply-events events))
      ;; Plan
      (check-equal? (hash-ref (blackboard-state-active-plan result) 'wave-count) 4)
      ;; Waves
      (check-equal? (hash-ref (blackboard-state-wave-status result) "W0") 'completed)
      (check-equal? (hash-ref (blackboard-state-wave-status result) "W1") 'in-progress)
      ;; Test results
      (check-equal? (length (blackboard-state-test-results result)) 1)
      ;; Verifier decisions
      (check-equal? (length (blackboard-state-verifier-decisions result)) 1))

    (test-case "apply-events with custom initial state"
      (define initial (blackboard-state (hasheq 'custom #t) '() '() '() (hash) '() '() 0))
      (define events (list (make-gsd-event 'gsd.wave.started (hasheq 'wave "W0"))))
      (define result (apply-events events initial))
      (check-equal? (hash-ref (blackboard-state-wave-status result) "W0") 'in-progress)
      ;; Initial active-plan preserved
      (check-true (hash-ref (blackboard-state-active-plan result) 'custom)))

    ;; ── Timestamp Updates ──

    (test-case "last-updated advances with event timestamp"
      (define evt1 (make-gsd-event 'gsd.wave.started (hasheq 'wave "W0")))
      (define evt2
        (hash-set (make-gsd-event 'gsd.wave.completed (hasheq 'wave "W0")) 'timestamp 5000))
      (define result (apply-events (list evt1 evt2)))
      (check-equal? (blackboard-state-last-updated result) 5000))

    ;; ── Flat Event Format Compatibility ──

    (test-case "flat event format (no 'data wrapper) still works"
      (define evt (hasheq 'event 'gsd.wave.started 'wave "W0" 'timestamp 1000))
      (define result (apply-event empty-blackboard evt))
      (check-equal? (hash-ref (blackboard-state-wave-status result) "W0") 'in-progress))))

(run-tests suite)
