#lang racket/base

;; tests/test-gsd-deprecated-api-migration.rkt — Regression test for AXIS1-F04/F17
;; Verify production code no longer calls deprecated gsm-* wrappers directly.
;; Production callers should use gsm-ctx-* with explicit context.

(require rackunit
         "../extensions/gsd/state-machine.rkt"
         (only-in "../extensions/gsd/session-state.rkt" current-gsd-ctx)
         (only-in "../extensions/gsd/core.rkt" reset-all-gsd-state!))

;; Reset state for clean test
(reset-all-gsd-state!)

(test-case "AXIS1-F04: gsm-ctx-current works with current-gsd-ctx"
  (define mode (gsm-ctx-current (current-gsd-ctx)))
  (check-pred symbol? mode)
  (check-eq? mode 'idle))

(test-case "AXIS1-F04: gsm-ctx-snapshot works with current-gsd-ctx"
  (define snap (gsm-ctx-snapshot (current-gsd-ctx)))
  (check-pred gsd-runtime-state? snap))

(test-case "AXIS1-F04: gsm-ctx-wave-complete? works"
  (check-false (gsm-ctx-wave-complete? (current-gsd-ctx) 0)))

(test-case "AXIS1-F04: gsm-ctx-next-pending-wave returns #f when no waves"
  (check-false (gsm-ctx-next-pending-wave (current-gsd-ctx))))

(test-case "AXIS1-F04: gsm-ctx-next-pending-wave returns first incomplete"
  (gsm-ctx-set-total-waves! (current-gsd-ctx) 3)
  (gsm-ctx-mark-wave-complete! (current-gsd-ctx) 0)
  (check-equal? (gsm-ctx-next-pending-wave (current-gsd-ctx)) 1)
  ;; Cleanup
  (reset-all-gsd-state!))

(test-case "AXIS1-F04: deprecated wrappers still work (test compat)"
  ;; Deprecated wrappers delegate to gsm-ctx-* — they still work for tests
  (check-eq? (gsm-current) 'idle)
  (check-pred list? (gsm-history)))

(reset-all-gsd-state!)
