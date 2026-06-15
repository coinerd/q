#lang racket

;; @speed fast  ;; @suite extensions

;; tests/test-hot-swap-events.rkt — W5 (v0.99.8) Hot-Swap Event Types Tests
;;
;; Tests for:
;;   - mas.agent.registered event → reducer handles it
;;   - mas.agent.activated event → reducer handles it
;;   - Subscriber includes new events in relevant-event-names
;;   - Blackboard activities accumulate correctly

(require rackunit
         rackunit/text-ui
         (only-in "../agent/blackboard.rkt"
                  blackboard-state-agent-activities
                  blackboard-state-last-updated
                  empty-blackboard)
         (only-in "../agent/blackboard-reducer.rkt" apply-event apply-events)
         (only-in "../agent/blackboard-subscriber.rkt" blackboard-relevant-event?))

(define suite
  (test-suite "Hot-Swap Event Types (v0.99.8 W5)"

    ;; ── mas.agent.registered event ──

    (test-case "registered event adds activity entry"
      (define evt
        (hash 'event
              'mas.agent.registered
              'data
              (hash 'role-name 'planner 'version "2.0.0")
              'timestamp
              1000))
      (define state (apply-event empty-blackboard evt))
      (define activities (blackboard-state-agent-activities state))
      (check-equal? (length activities) 1)
      (define act (car activities))
      (check-equal? (hash-ref act 'agent-name) 'planner)
      (check-equal? (hash-ref act 'action) 'registered)
      (check-equal? (hash-ref act 'version) "2.0.0"))

    (test-case "registered event updates last-updated"
      (define evt
        (hash 'event
              'mas.agent.registered
              'data
              (hash 'role-name 'planner 'version "2.0.0")
              'timestamp
              5555))
      (define state (apply-event empty-blackboard evt))
      (check-equal? (blackboard-state-last-updated state) 5555))

    ;; ── mas.agent.activated event ──

    (test-case "activated event adds activity entry"
      (define evt
        (hash 'event
              'mas.agent.activated
              'data
              (hash 'role-name 'verifier 'version "3.0.0")
              'timestamp
              2000))
      (define state (apply-event empty-blackboard evt))
      (define activities (blackboard-state-agent-activities state))
      (check-equal? (length activities) 1)
      (define act (car activities))
      (check-equal? (hash-ref act 'agent-name) 'verifier)
      (check-equal? (hash-ref act 'action) 'activated)
      (check-equal? (hash-ref act 'version) "3.0.0"))

    (test-case "activated event updates last-updated"
      (define evt
        (hash 'event
              'mas.agent.activated
              'data
              (hash 'role-name 'verifier 'version "3.0.0")
              'timestamp
              6666))
      (define state (apply-event empty-blackboard evt))
      (check-equal? (blackboard-state-last-updated state) 6666))

    ;; ── Subscriber includes new events ──

    (test-case "subscriber recognizes mas.agent.registered"
      (define evt
        (hash 'event
              'mas.agent.registered
              'data
              (hash 'role-name 'planner 'version "1.0")
              'timestamp
              100))
      (check-true (blackboard-relevant-event? evt)))

    (test-case "subscriber recognizes mas.agent.activated"
      (define evt
        (hash 'event
              'mas.agent.activated
              'data
              (hash 'role-name 'planner 'version "2.0")
              'timestamp
              100))
      (check-true (blackboard-relevant-event? evt)))

    ;; ── Mixed events accumulate ──

    (test-case "mixed registry events accumulate in activities"
      (define events
        (list (hash 'event
                    'mas.agent.registered
                    'data
                    (hash 'role-name 'planner 'version "1.0")
                    'timestamp
                    1000)
              (hash 'event
                    'mas.agent.activated
                    'data
                    (hash 'role-name 'planner 'version "2.0")
                    'timestamp
                    2000)
              (hash 'event
                    'mas.agent.version.pinned
                    'data
                    (hash 'role-name 'planner 'version "1.0")
                    'timestamp
                    3000)))
      (define state (apply-events events))
      (define activities (blackboard-state-agent-activities state))
      (check-equal? (length activities) 3)
      (check-equal? (hash-ref (car activities) 'action) 'registered)
      (check-equal? (hash-ref (cadr activities) 'action) 'activated)
      (check-equal? (hash-ref (caddr activities) 'action) 'version-pinned)
      (check-equal? (blackboard-state-last-updated state) 3000))))

(run-tests suite)
