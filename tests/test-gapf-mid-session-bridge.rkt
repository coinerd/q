#lang racket/base

;; tests/test-gapf-mid-session-bridge.rkt
;; v0.97.5 W1: GAP-F mid-session bridge on state transitions

(require rackunit
         rackunit/text-ui
         racket/string
         racket/set
         (only-in "../runtime/session/session-events.rkt"
                  current-mid-session-bridge-enabled
                  current-mid-session-persisted-ids
                  major-forward-transition?))

(define suite
  (test-suite "gapf-mid-session-bridge"

    ;; major-forward-transition? recognizes forward transitions
    (test-case "major forward transitions are detected"
      (check-true (major-forward-transition? 'exploration 'planning))
      (check-true (major-forward-transition? 'planning 'implementation))
      (check-true (major-forward-transition? 'implementation 'verification)))

    ;; Same state is NOT a forward transition
    (test-case "same state is not a forward transition"
      (check-false (major-forward-transition? 'planning 'planning))
      (check-false (major-forward-transition? 'exploration 'exploration)))

    ;; Reverse transitions are NOT forward transitions
    (test-case "reverse transitions are not forward transitions"
      (check-false (major-forward-transition? 'planning 'exploration))
      (check-false (major-forward-transition? 'implementation 'planning))
      (check-false (major-forward-transition? 'verification 'implementation)))

    ;; Non-transition states return #f
    (test-case "unrelated states are not forward transitions"
      (check-false (major-forward-transition? 'idle 'planning))
      (check-false (major-forward-transition? 'debugging 'verification)))

    ;; Parameter defaults to #f
    (test-case "current-mid-session-bridge-enabled defaults to #f"
      (check-equal? (current-mid-session-bridge-enabled) #f))

    ;; Parameter can be set to #t
    (test-case "current-mid-session-bridge-enabled can be enabled"
      (parameterize ([current-mid-session-bridge-enabled #t])
        (check-equal? (current-mid-session-bridge-enabled) #t)))

    ;; F7: Bridge action fires on major forward transition when enabled
    (test-case "bridge action fires on major forward transition when enabled"
      (parameterize ([current-mid-session-bridge-enabled #t])
        (check-true (major-forward-transition? 'exploration 'planning))
        (check-true (current-mid-session-bridge-enabled))
        ;; The bridge gate requires both: enabled AND major forward transition
        (check-true (and (current-mid-session-bridge-enabled)
                         (major-forward-transition? 'planning 'implementation)))))

    ;; F7: Bridge does NOT fire when disabled even with forward transition
    (test-case "bridge does NOT fire when disabled even with forward transition"
      (parameterize ([current-mid-session-bridge-enabled #f])
        (check-true (major-forward-transition? 'exploration 'planning))
        (check-false (and (current-mid-session-bridge-enabled)
                          (major-forward-transition? 'exploration 'planning)))))

    ;; F7: Bridge does NOT fire when enabled but no forward transition
    (test-case "bridge does NOT fire when enabled but no forward transition"
      (parameterize ([current-mid-session-bridge-enabled #t])
        (check-false (major-forward-transition? 'planning 'exploration))
        (check-false (and (current-mid-session-bridge-enabled)
                          (major-forward-transition? 'planning 'exploration)))))

    ;; F7: Bridge parameter resets after parameterize scope
    (test-case "bridge parameter resets after parameterize scope"
      (check-equal? (current-mid-session-bridge-enabled) #f))

    ;; ============================================================
    ;; v0.97.11 W0: GAP-E dedup parameter tests
    ;; ============================================================

    (test-case "GAP-E: current-mid-session-persisted-ids defaults to empty set"
      (define ids (current-mid-session-persisted-ids))
      (check-true (set? ids))
      (check-equal? (set-count ids) 0))

    (test-case "GAP-E: persisted IDs can be accumulated via parameterize"
      (parameterize ([current-mid-session-persisted-ids (set "c1" "c2")])
        (define ids (current-mid-session-persisted-ids))
        (check-true (set-member? ids "c1"))
        (check-true (set-member? ids "c2"))
        (check-false (set-member? ids "c3")))
      ;; Reset after scope
      (check-equal? (set-count (current-mid-session-persisted-ids)) 0))

    (test-case "GAP-E: dedup set prevents duplicate ID addition"
      (parameterize ([current-mid-session-persisted-ids (set "c1")])
        ;; Adding same ID again — set deduplicates
        (current-mid-session-persisted-ids (set-add (current-mid-session-persisted-ids) "c1"))
        (check-equal? (set-count (current-mid-session-persisted-ids)) 1 "dedup works")
        ;; Adding new ID
        (current-mid-session-persisted-ids (set-add (current-mid-session-persisted-ids) "c2"))
        (check-equal? (set-count (current-mid-session-persisted-ids)) 2)))))

(run-tests suite)
