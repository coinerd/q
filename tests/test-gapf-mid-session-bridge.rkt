#lang racket/base

;; tests/test-gapf-mid-session-bridge.rkt
;; v0.97.5 W1: GAP-F mid-session bridge on state transitions

(require rackunit
         rackunit/text-ui
         racket/string
         (only-in "../runtime/session/session-events.rkt"
                  current-mid-session-bridge-enabled
                  major-forward-transition?))

(define suite
  (test-suite "gapf-mid-session-bridge"

    ;; major-forward-transition? recognizes forward transitions
    (test-case "major forward transitions are detected"
      (check-true (major-forward-transition? 'exploration 'planning))
      (check-true (major-forward-transition? 'planning 'implementation))
      (check-true (major-forward-transition? 'implementation 'review)))

    ;; Same state is NOT a forward transition
    (test-case "same state is not a forward transition"
      (check-false (major-forward-transition? 'planning 'planning))
      (check-false (major-forward-transition? 'exploration 'exploration)))

    ;; Reverse transitions are NOT forward transitions
    (test-case "reverse transitions are not forward transitions"
      (check-false (major-forward-transition? 'planning 'exploration))
      (check-false (major-forward-transition? 'implementation 'planning))
      (check-false (major-forward-transition? 'review 'implementation)))

    ;; Non-transition states return #f
    (test-case "unrelated states are not forward transitions"
      (check-false (major-forward-transition? 'idle 'planning))
      (check-false (major-forward-transition? 'debugging 'review)))

    ;; Parameter defaults to #f
    (test-case "current-mid-session-bridge-enabled defaults to #f"
      (check-equal? (current-mid-session-bridge-enabled) #f))

    ;; Parameter can be set to #t
    (test-case "current-mid-session-bridge-enabled can be enabled"
      (parameterize ([current-mid-session-bridge-enabled #t])
        (check-equal? (current-mid-session-bridge-enabled) #t)))))

(run-tests suite)
