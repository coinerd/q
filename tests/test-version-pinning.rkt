#lang racket

;; @speed fast  ;; @suite extensions

;; tests/test-version-pinning.rkt — W4 (v0.99.8) Version Pinning + Session Tests
;;
;; Tests for:
;;   - pin-current-versions returns correct pins
;;   - mas.agent.version.pinned event updates blackboard
;;   - Pinned versions override active flag for instance creation
;;   - make-agent-with-pin uses pinned version even after activation switch

(require rackunit
         rackunit/text-ui
         (only-in "../agent/registry.rkt"
                  reset-registry!
                  register-agent!
                  activate-agent-version!
                  pin-current-versions
                  make-agent-with-pin
                  make-agent-instance)
         (only-in "../agent/registry-types.rkt"
                  version-pin?
                  version-pin-role-name
                  version-pin-pinned-version)
         (only-in "../agent/blackboard.rkt"
                  blackboard-state-agent-activities
                  blackboard-state-last-updated
                  empty-blackboard)
         (only-in "../agent/blackboard-reducer.rkt" apply-event apply-events))

;; Helpers
(define (make-pinned-event role version ts)
  (hash 'event 'mas.agent.version.pinned 'data (hash 'role-name role 'version version) 'timestamp ts))

(define suite
  (test-suite "Version Pinning + Session Integration (v0.99.8 W4)"

    ;; ── pin-current-versions ──

    (test-case "pin-current-versions returns pins for all roles"
      (reset-registry!)
      (register-agent! 'planner "1.0" (lambda () 'v1))
      (register-agent! 'verifier "2.0" (lambda () 'v2))
      (define pins (pin-current-versions))
      (check-equal? (hash-count pins) 2)
      (check-true (version-pin? (hash-ref pins 'planner)))
      (check-equal? (version-pin-pinned-version (hash-ref pins 'planner)) "1.0")
      (check-equal? (version-pin-pinned-version (hash-ref pins 'verifier)) "2.0")
      (reset-registry!))

    (test-case "pin captures current active version, not all versions"
      (reset-registry!)
      (register-agent! 'planner "1.0" (lambda () 'v1))
      (register-agent! 'planner "2.0" (lambda () 'v2))
      ;; v1 is active by default (first registered)
      (define pins (pin-current-versions))
      (check-equal? (version-pin-pinned-version (hash-ref pins 'planner)) "1.0")
      (reset-registry!))

    ;; ── make-agent-with-pin ──

    (test-case "pinned version overrides active flag"
      (reset-registry!)
      (register-agent! 'planner "1.0" (lambda () 'v1-result))
      (register-agent! 'planner "2.0" (lambda () 'v2-result))
      (define pins (pin-current-versions))
      ;; Pin captured v1 (active). Now activate v2.
      (activate-agent-version! 'planner "2.0")
      ;; make-agent-instance now returns v2 (active)
      (check-equal? (make-agent-instance 'planner) 'v2-result)
      ;; But make-agent-with-pin still returns v1 (pinned)
      (define pin (hash-ref pins 'planner))
      (check-equal? (make-agent-with-pin 'planner pin) 'v1-result)
      (reset-registry!))

    (test-case "make-agent-with-pin with #f uses active version"
      (reset-registry!)
      (register-agent! 'planner "1.0" (lambda () 'v1))
      (check-equal? (make-agent-with-pin 'planner #f) 'v1)
      (reset-registry!))

    ;; ── mas.agent.version.pinned event ──

    (test-case "version.pinned event adds activity to blackboard"
      (define evt (make-pinned-event 'planner "1.0.0" 5000))
      (define state (apply-event empty-blackboard evt))
      (define activities (blackboard-state-agent-activities state))
      (check-equal? (length activities) 1)
      (define activity (car activities))
      (check-equal? (hash-ref activity 'agent-name) 'planner)
      (check-equal? (hash-ref activity 'action) 'version-pinned)
      (check-equal? (hash-ref activity 'version) "1.0.0"))

    (test-case "version.pinned event updates last-updated"
      (define evt (make-pinned-event 'planner "1.0.0" 7777))
      (define state (apply-event empty-blackboard evt))
      (check-equal? (blackboard-state-last-updated state) 7777))

    (test-case "multiple version.pinned events accumulate in activities"
      (define events
        (list (make-pinned-event 'planner "1.0.0" 1000)
              (make-pinned-event 'verifier "2.0.0" 2000)
              (make-pinned-event 'tool-gateway "1.0.0" 3000)))
      (define state (apply-events events))
      (define activities (blackboard-state-agent-activities state))
      (check-equal? (length activities) 3)
      (check-equal? (blackboard-state-last-updated state) 3000))

    ;; ── Edge cases ──

    (test-case "pin on empty registry returns empty hash"
      (reset-registry!)
      (define pins (pin-current-versions))
      (check-equal? (hash-count pins) 0)
      (reset-registry!))))

(run-tests suite)
