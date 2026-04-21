#lang racket

;; test-exploration-steering.rkt — Tests for exploration steering levels (BUG-STEERING-LEVEL1-WEAK)
;;
;; Verify Level 1 uses imperative "MUST now" instead of permissive "Consider".
;; Level 2 and Level 3 texts should remain unchanged.

(require rackunit
         rackunit/text-ui)

;; ============================================================
;; Steering level text verification
;;
;; These tests verify the steering message text templates by
;; reading the source directly. The source uses string-append
;; across lines, so we use regexp-match? on the full file text.
;; ============================================================

;; Try relative to current directory (q/) and tests/ subdirectory
(define iteration-source
  (if (file-exists? "runtime/iteration.rkt") "runtime/iteration.rkt" "../runtime/iteration.rkt"))

(define steering-tests
  (test-suite "Exploration Steering Levels (BUG-STEERING-LEVEL1-WEAK)"

    ;; ── Verify Level 1 text contains "MUST now" ──
    (test-case "Level 1 steering text contains 'MUST now' not 'Consider'"
      (define source (file->string iteration-source))
      ;; Level 1 uses [steering] (no :strong or :hard suffix)
      ;; After fix: should contain "MUST now"
      (check-not-false (regexp-match? #rx"\"\\[steering\\] .*MUST now" source)
                       "Level 1 steering should contain 'MUST now'")
      ;; After fix: should NOT contain "Consider using"
      (check-false (regexp-match? #rx"\"\\[steering\\] .*Consider using" source)
                   "Level 1 steering should NOT contain 'Consider using'"))

    ;; ── Verify Level 2 text unchanged ──
    (test-case "Level 2 steering text contains 'Stop exploring'"
      (define source (file->string iteration-source))
      (check-not-false (regexp-match? #rx"\\[steering:strong\\]" source)
                       "Level 2 should use [steering:strong] prefix"))

    ;; ── Verify Level 3 text unchanged ──
    (test-case "Level 3 steering text contains 'exploration budget'"
      (define source (file->string iteration-source))
      (check-not-false (regexp-match? #rx"\\[steering:hard\\]" source)
                       "Level 3 should use [steering:hard] prefix"))))

(run-tests steering-tests)
