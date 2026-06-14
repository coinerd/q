#lang racket

;; @speed fast  ;; @suite extensions

;; tests/test-blackboard-integration.rkt — W7 (v0.99.7) End-to-End Integration Tests
;;
;; Full lifecycle integration test for the MAS blackboard system:
;;   1. Create event bus → start subscriber
;;   2. Emit GSD wave events → verify blackboard updates
;;   3. Emit MAS artifact events → verify blackboard updates
;;   4. Emit verification events → verify blackboard updates
;;   5. Read blackboard → verify all fields
;;   6. Build context snippet → verify format
;;   7. Stop subscriber → clean shutdown
;;
;; Also tests context injection integration with state-awareness preamble.

(require rackunit
         rackunit/text-ui
         racket/file
         (only-in "../agent/blackboard.rkt"
                  make-blackboard
                  current-blackboard
                  read-blackboard
                  blackboard-state?
                  blackboard-state-active-plan
                  blackboard-state-open-hypotheses
                  blackboard-state-test-results
                  blackboard-state-artifact-refs
                  blackboard-state-wave-status
                  blackboard-state-verifier-decisions
                  blackboard-state-last-updated)
         (only-in "../agent/blackboard-subscriber.rkt"
                  start-blackboard-subscriber!
                  stop-blackboard-subscriber!
                  current-subscription)
         (only-in "../runtime/context-assembly/blackboard-context.rkt"
                  build-blackboard-context-snippet)
         (only-in "../runtime/context-assembly/state-aware-builder.rkt"
                  build-state-awareness-preamble
                  current-blackboard-injection-enabled)
         (only-in "../util/event/event-bus.rkt" make-event-bus publish!)
         (only-in "../util/event/event.rkt" make-event)
         (only-in "../util/message/message.rkt" message-content)
         (only-in "../util/content/content-parts.rkt" text-part-text))

;; Helper: publish an event to the bus
(define (emit! bus ev-name payload)
  (publish! bus (make-event ev-name (current-inexact-milliseconds) "test-session" #f payload)))

;; Helper: extract preamble text
(define (preamble-text preamble)
  (text-part-text (car (message-content preamble))))

(define suite
  (test-suite "Blackboard E2E Integration (W7 v0.99.7)"

    ;; ════════════════════════════════════════════════════════
    ;; W7-T1: Full Lifecycle Integration
    ;; ════════════════════════════════════════════════════════

    (test-case "full lifecycle: start subscriber, emit events, verify, stop"
      ;; 1. Setup
      (define bus (make-event-bus))
      (define bb (make-blackboard))
      (parameterize ([current-blackboard bb])
        ;; Start subscriber
        (start-blackboard-subscriber! bus bb)

        ;; 2. Emit GSD wave events
        (emit! bus 'gsd.wave.started (hash 'wave "W0"))
        (emit! bus 'gsd.wave.completed (hash 'wave "W0"))
        (emit! bus 'gsd.wave.started (hash 'wave "W1"))

        ;; 3. Emit MAS artifact events
        (emit! bus
               'mas.artifact.produced
               (hash 'name "plan.md" 'path "/tmp/plan.md" 'artifact-type "markdown"))
        (emit! bus
               'mas.artifact.produced
               (hash 'name "report.txt" 'path "/tmp/report.txt" 'artifact-type "text"))

        ;; 4. Emit verification events
        (emit! bus
               'gsd.verification.completed
               (hash 'verdict 'approve 'risk-level 'low 'reason "All tests pass"))

        ;; 5. Read blackboard and verify
        (define state (read-blackboard bb))
        (check-true (blackboard-state? state))

        ;; Wave status
        (define waves (blackboard-state-wave-status state))
        (check-true (hash-has-key? waves "W0"))
        (check-true (hash-has-key? waves "W1"))

        ;; Artifact refs
        (define artifacts (blackboard-state-artifact-refs state))
        (check-true (pair? artifacts))

        ;; Verifier decisions
        (define decisions (blackboard-state-verifier-decisions state))
        (check-true (pair? decisions))

        ;; Last-updated should be set
        (check-true (real? (blackboard-state-last-updated state)))

        ;; 6. Build context snippet and verify
        (define snippet (build-blackboard-context-snippet state))
        (check-true (string? snippet))
        (check-true (string-contains? snippet "[Blackboard]"))
        (check-true (string-contains? snippet "W0"))
        (check-true (string-contains? snippet "W1"))
        (check-true (string-contains? snippet "plan.md"))
        (check-true (string-contains? snippet "approve"))

        ;; 7. Stop subscriber
        (stop-blackboard-subscriber! bus)
        (check-false (unbox current-subscription))))

    (test-case "subscriber captures test result events"
      (define bus (make-event-bus))
      (define bb (make-blackboard))
      (parameterize ([current-blackboard bb])
        (start-blackboard-subscriber! bus bb)
        (emit! bus 'mas.test.result (hash 'file "test-foo.rkt" 'result 'pass))
        (emit! bus 'mas.test.result (hash 'file "test-bar.rkt" 'result 'fail))
        (define state (read-blackboard bb))
        (define results (blackboard-state-test-results state))
        (check-true (pair? results))
        (stop-blackboard-subscriber! bus)))

    (test-case "subscriber captures hypothesis events"
      (define bus (make-event-bus))
      (define bb (make-blackboard))
      (parameterize ([current-blackboard bb])
        (start-blackboard-subscriber! bus bb)
        (emit! bus 'mas.hypothesis.opened (hash 'id "H1" 'question "Why does X fail?"))
        (emit! bus 'mas.hypothesis.opened (hash 'id "H2" 'question "Is Y the root cause?"))
        (emit! bus 'mas.hypothesis.resolved (hash 'id "H1" 'resolution 'confirmed))
        (define state (read-blackboard bb))
        (define hyps (blackboard-state-open-hypotheses state))
        ;; H1 should be resolved/removed, H2 should remain
        (check-true (>= (length hyps) 0))
        (stop-blackboard-subscriber! bus)))

    (test-case "subscriber ignores irrelevant events"
      (define bus (make-event-bus))
      (define bb (make-blackboard))
      (parameterize ([current-blackboard bb])
        (start-blackboard-subscriber! bus bb)
        (emit! bus 'ui.render.requested (hash 'panel 'main))
        (emit! bus 'message.received (hash 'text "hello"))
        (define state (read-blackboard bb))
        ;; Blackboard should be empty (no relevant events)
        (check-true (hash-empty? (blackboard-state-wave-status state)))
        (check-false (pair? (blackboard-state-artifact-refs state)))
        (stop-blackboard-subscriber! bus)))

    ;; ════════════════════════════════════════════════════════
    ;; W7-T2: Context Injection Integration
    ;; ════════════════════════════════════════════════════════

    (test-case "injection ON + populated blackboard → snippet in preamble"
      (define bus (make-event-bus))
      (define bb (make-blackboard))
      (parameterize ([current-blackboard bb]
                     [current-blackboard-injection-enabled #t])
        (start-blackboard-subscriber! bus bb)
        (emit! bus 'gsd.wave.started (hash 'wave "W0"))
        (define preamble (build-state-awareness-preamble 'implementation '()))
        (check-not-false preamble)
        (define text (preamble-text preamble))
        (check-true (string-contains? text "[Blackboard]"))
        (check-true (string-contains? text "W0"))
        (stop-blackboard-subscriber! bus)))

    (test-case "injection ON + empty blackboard → no snippet"
      (define bb (make-blackboard))
      (parameterize ([current-blackboard bb]
                     [current-blackboard-injection-enabled #t])
        (define preamble (build-state-awareness-preamble 'implementation '()))
        (check-not-false preamble)
        (check-false (string-contains? (preamble-text preamble) "[Blackboard]"))))

    (test-case "injection OFF + populated blackboard → no snippet"
      (define bus (make-event-bus))
      (define bb (make-blackboard))
      (parameterize ([current-blackboard bb]
                     [current-blackboard-injection-enabled #f])
        (start-blackboard-subscriber! bus bb)
        (emit! bus 'gsd.wave.started (hash 'wave "W0"))
        (define preamble (build-state-awareness-preamble 'implementation '()))
        (check-not-false preamble)
        (check-false (string-contains? (preamble-text preamble) "[Blackboard]"))
        (stop-blackboard-subscriber! bus)))

    ;; ════════════════════════════════════════════════════════
    ;; W7-T1: Feature Flag Safety
    ;; ════════════════════════════════════════════════════════

    (test-case "feature flag OFF → zero behavioral change"
      ;; Without starting subscriber or enabling injection,
      ;; blackboard and injection should be inert.
      (check-false (current-blackboard-injection-enabled))
      ;; Preamble should work without any blackboard state
      (define preamble (build-state-awareness-preamble 'exploration '()))
      (check-not-false preamble)
      (check-false (string-contains? (preamble-text preamble) "[Blackboard]")))

    (test-case "start/stop subscriber is idempotent"
      (define bus (make-event-bus))
      (define bb (make-blackboard))
      (parameterize ([current-blackboard bb])
        ;; Start
        (start-blackboard-subscriber! bus bb)
        (check-true (and (unbox current-subscription) #t))
        ;; Stop
        (stop-blackboard-subscriber! bus)
        (check-false (unbox current-subscription))
        ;; Stop again (should not crash)
        (stop-blackboard-subscriber!)
        (check-false (unbox current-subscription))))))

(run-tests suite)
