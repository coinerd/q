#lang racket/base
;; @speed fast @suite default
;; tests/test-gap5-conclusion-bridge.rkt — GAP-5 TDD tests
;; Validates conclusion bridge activation in profiles

(require rackunit
         rackunit/text-ui
         (only-in "../runtime/session/session-config.rkt"
                  apply-context-assembly-profile!
                  current-context-assembly-profile)
         (only-in "../runtime/memory/conclusion-bridge.rkt"
                  current-conclusion-to-memory-bridge-enabled
                  persist-high-value-conclusions!
                  high-value-categories))

(define-test-suite gap-5-tests
  (test-case "GAP-5: self-healing profile enables conclusion bridge"
    (apply-context-assembly-profile! 'self-healing)
    (check-true (current-conclusion-to-memory-bridge-enabled)))

  (test-case "GAP-5: full profile enables conclusion bridge"
    (apply-context-assembly-profile! 'full)
    (check-true (current-conclusion-to-memory-bridge-enabled)))

  (test-case "GAP-5: observe profile does NOT enable bridge"
    (current-conclusion-to-memory-bridge-enabled #f)
    (apply-context-assembly-profile! 'observe)
    (check-false (current-conclusion-to-memory-bridge-enabled)))

  (test-case "GAP-5: bounded profile does NOT enable bridge"
    (current-conclusion-to-memory-bridge-enabled #f)
    (apply-context-assembly-profile! 'bounded)
    (check-false (current-conclusion-to-memory-bridge-enabled)))

  (test-case "GAP-5: high-value-categories includes fact"
    (check-not-false (memq 'fact high-value-categories)
                     "fact category must be in high-value-categories")))

(run-tests gap-5-tests)
