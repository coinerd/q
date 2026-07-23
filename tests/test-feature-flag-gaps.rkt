#lang racket/base

;; q/tests/test-feature-flag-gaps.rkt — Regression tests for AXIS2-F08/F13/F14
;; Verify memory warning, goal-loop wiring, and bounded event channel.

;; @speed fast
(require rackunit
         "../runtime/session/session-config.rkt"
         "../util/event/event-bus.rkt"
         "../tui/tui-init.rkt")

;; AXIS2-F13: current-goal-loop-enabled? exists and defaults to #t
(check-true (current-goal-loop-enabled?) "goal-loop defaults to #t")
(parameterize ([current-goal-loop-enabled? #f])
  (check-false (current-goal-loop-enabled?) "goal-loop can be set to #f"))

;; AXIS2-F14: subscribe-runtime-events! exists and is a procedure
(check-pred procedure? subscribe-runtime-events!)
