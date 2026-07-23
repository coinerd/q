#lang racket

;; tests/test-verification-events.rkt — W2 v0.99.5 Verification Event Tests
;;
;; Tests the three verification lifecycle events:
;;   verification-started-event, verification-completed-event,
;;   verification-escalated-event

;; @speed fast
(require rackunit
         rackunit/text-ui
         "../agent/event-structs/verification-events.rkt"
         (only-in "../agent/event-structs/base.rkt"
                  typed-event?
                  typed-event-type
                  typed-event-timestamp
                  typed-event-session-id
                  typed-event-turn-id))

(define suite
  (test-suite "Verification Events (W2 v0.99.5)"

    ;; ── verification-started-event ──

    (test-case "verification-started-event constructs with required fields"
      (define evt
        (make-verification-started-event #:session-id "sess-1" #:turn-id "turn-1" #:artifact-count 5))
      (check-true (verification-started-event? evt))
      (check-equal? (verification-started-event-artifact-count evt) 5)
      (check-equal? (typed-event-session-id evt) "sess-1")
      (check-equal? (typed-event-turn-id evt) "turn-1"))

    (test-case "verification-started-event has correct type string"
      (define evt (make-verification-started-event #:session-id "s" #:turn-id "t" #:artifact-count 1))
      (check-equal? (typed-event-type evt) "gsd.verification.started"))

    (test-case "verification-started-event accepts optional wave-number and plan-path"
      (define evt
        (make-verification-started-event #:session-id "s"
                                         #:turn-id "t"
                                         #:artifact-count 3
                                         #:wave-number 2
                                         #:plan-path "/plan.md"))
      (check-equal? (verification-started-event-wave-number evt) 2)
      (check-equal? (verification-started-event-plan-path evt) "/plan.md"))

    (test-case "verification-started-event optional fields default to #f"
      (define evt (make-verification-started-event #:session-id "s" #:turn-id "t" #:artifact-count 1))
      (check-false (verification-started-event-wave-number evt))
      (check-false (verification-started-event-plan-path evt)))

    (test-case "verification-started-event-fields lists all fields"
      (check-equal? verification-started-event-fields '(artifact-count wave-number plan-path)))

    ;; ── verification-completed-event ──

    (test-case "verification-completed-event constructs with verdict"
      (define evt
        (make-verification-completed-event #:session-id "s" #:turn-id "t" #:verdict 'approve))
      (check-true (verification-completed-event? evt))
      (check-equal? (verification-completed-event-verdict evt) 'approve))

    (test-case "verification-completed-event has correct type string"
      (define evt
        (make-verification-completed-event #:session-id "s" #:turn-id "t" #:verdict 'reject))
      (check-equal? (typed-event-type evt) "gsd.verification.completed"))

    (test-case "verification-completed-event accepts optional fields"
      (define evt
        (make-verification-completed-event #:session-id "s"
                                           #:turn-id "t"
                                           #:verdict 'escalate
                                           #:reason "too risky"
                                           #:risk-level 'high
                                           #:requires-human #t))
      (check-equal? (verification-completed-event-reason evt) "too risky")
      (check-equal? (verification-completed-event-risk-level evt) 'high)
      (check-true (verification-completed-event-requires-human evt)))

    (test-case "verification-completed-event optional fields default to #f"
      (define evt
        (make-verification-completed-event #:session-id "s" #:turn-id "t" #:verdict 'approve))
      (check-false (verification-completed-event-reason evt))
      (check-false (verification-completed-event-risk-level evt))
      (check-false (verification-completed-event-requires-human evt)))

    ;; ── verification-escalated-event ──

    (test-case "verification-escalated-event constructs with required fields"
      (define evt
        (make-verification-escalated-event #:session-id "s"
                                           #:turn-id "t"
                                           #:reason "uncertain code quality"
                                           #:risk-level 'medium))
      (check-true (verification-escalated-event? evt))
      (check-equal? (verification-escalated-event-reason evt) "uncertain code quality")
      (check-equal? (verification-escalated-event-risk-level evt) 'medium))

    (test-case "verification-escalated-event has correct type string"
      (define evt
        (make-verification-escalated-event #:session-id "s"
                                           #:turn-id "t"
                                           #:reason "x"
                                           #:risk-level 'high))
      (check-equal? (typed-event-type evt) "gsd.verification.escalated"))

    (test-case "verification-escalated-event accepts optional artifact-refs"
      (define evt
        (make-verification-escalated-event #:session-id "s"
                                           #:turn-id "t"
                                           #:reason "review needed"
                                           #:risk-level 'high
                                           #:artifact-refs '("a.rkt" "b.rkt")))
      (check-equal? (verification-escalated-event-artifact-refs evt) '("a.rkt" "b.rkt")))

    (test-case "verification-escalated-event optional artifact-refs defaults to #f"
      (define evt
        (make-verification-escalated-event #:session-id "s"
                                           #:turn-id "t"
                                           #:reason "x"
                                           #:risk-level 'low))
      (check-false (verification-escalated-event-artifact-refs evt)))

    ;; ── Inheritance from typed-event ──

    (test-case "verification events inherit typed-event"
      (define started
        (make-verification-started-event #:session-id "s" #:turn-id "t" #:artifact-count 1))
      (define completed
        (make-verification-completed-event #:session-id "s" #:turn-id "t" #:verdict 'approve))
      (define escalated
        (make-verification-escalated-event #:session-id "s"
                                           #:turn-id "t"
                                           #:reason "x"
                                           #:risk-level 'high))
      (check-true (typed-event? started))
      (check-true (typed-event? completed))
      (check-true (typed-event? escalated)))

    (test-case "verification events have timestamp"
      (define evt (make-verification-started-event #:session-id "s" #:turn-id "t" #:artifact-count 1))
      (check-true (real? (typed-event-timestamp evt))))

    (test-case "verification events support custom timestamp"
      (define evt
        (make-verification-started-event #:session-id "s"
                                         #:turn-id "t"
                                         #:artifact-count 1
                                         #:timestamp 99999))
      (check-equal? (typed-event-timestamp evt) 99999))

    ;; ── Transparency ──

    (test-case "verification events are transparent (equal? works)"
      (define e1
        (make-verification-completed-event #:session-id "s"
                                           #:turn-id "t"
                                           #:verdict 'approve
                                           #:timestamp 100))
      (define e2
        (make-verification-completed-event #:session-id "s"
                                           #:turn-id "t"
                                           #:verdict 'approve
                                           #:timestamp 100))
      (check-equal? e1 e2))

    (test-case "verification events with different values are not equal"
      (define e1
        (make-verification-completed-event #:session-id "s"
                                           #:turn-id "t"
                                           #:verdict 'approve
                                           #:timestamp 100))
      (define e2
        (make-verification-completed-event #:session-id "s"
                                           #:turn-id "t"
                                           #:verdict 'reject
                                           #:timestamp 100))
      (check-not-equal? e1 e2))))

(run-tests suite)
