#lang racket/base

;; @speed fast
;; @suite default

;; tests/test-adaptive-verification-e2e.rkt
;; v0.99.24 W2: End-to-End integration tests for adaptive verification.
;;
;; These tests verify the FULL production data path:
;;   gsd-plan/gsd-wave structs
;;     → build-enriched-plan-ctx (capability inference from files + tasks)
;;       → should-skip-verification? (§6.1 complexity heuristic)
;;         → effective-risk-threshold (§6.2 dynamic risk threshold)
;;
;; Each test constructs REAL gsd-plan/gsd-wave structs (no mocks),
;; feeds them through the enrichment pipeline, and verifies that the
;; verifier gate's skip/threshold functions make the correct decisions.

(require rackunit
         rackunit/text-ui
         racket/list
         "../extensions/gsd/plan-types.rkt"
         "../extensions/gsd/plan-context-builder.rkt"
         "../agent/verification/verifier-gate.rkt")

;; ── Helpers ──

;; Build a gsd-plan with a single wave.
;; #:files — list of file paths for the wave
;; #:tasks — list of gsd-task structs (default: empty)
;; #:title — wave title
(define (make-plan #:files [files '()] #:tasks [tasks '()] #:title [title "E2E Test Wave"])
  (gsd-plan (list (gsd-wave 0 title 'pending "" files tasks "" '())) #f '() '()))

(define (make-task name action)
  (gsd-task name '() action "" "" 'pending))

;; ── Test Suite ──

(define suite
  (test-suite "Adaptive Verification E2E Integration (v0.99.24 W2)"

    ;; ════════════════════════════════════════════════════════
    ;; §6.1: should-skip-verification? — Complexity Heuristic
    ;; ════════════════════════════════════════════════════════

    (test-case "E2E-1: read-only wave (1 .txt file) skips verification"
      ;; A wave with only a .txt file produces no capabilities.
      ;; should-skip-verification? returns #t because:
      ;;   - capabilities is '() (truthy, is a list)
      ;;   - ≤ 2 files
      ;;   - no dangerous capabilities
      (define plan (make-plan #:files '("notes.txt")))
      (define ctx (build-enriched-plan-ctx "." plan 0))
      (check-equal? (hash-ref ctx 'capabilities-used) '())
      (check-equal? (hash-ref ctx 'files-changed) '("notes.txt"))
      (check-true (should-skip-verification? ctx)))

    (test-case "E2E-2: small .rkt wave (1 file) does NOT skip verification"
      ;; A wave with a .rkt file produces '(file-write).
      ;; should-skip-verification? returns #f because file-write is dangerous.
      (define plan (make-plan #:files '("agent/core.rkt")))
      (define ctx (build-enriched-plan-ctx "." plan 0))
      (check-not-false (member 'file-write (hash-ref ctx 'capabilities-used)))
      (check-false (should-skip-verification? ctx)))

    (test-case "E2E-3: shell-exec wave (.sh file) does NOT skip verification"
      ;; A wave with a .sh file produces '(shell-exec).
      ;; should-skip-verification? returns #f because shell-exec is dangerous.
      (define plan (make-plan #:files '("deploy.sh")))
      (define ctx (build-enriched-plan-ctx "." plan 0))
      (check-not-false (member 'shell-exec (hash-ref ctx 'capabilities-used)))
      (check-false (should-skip-verification? ctx)))

    (test-case "E2E-4: large wave (>2 files) never skips even with safe capabilities"
      ;; Even if all files are safe types, >2 files means verification runs.
      (define plan (make-plan #:files '("a.txt" "b.txt" "c.txt" "d.txt")))
      (define ctx (build-enriched-plan-ctx "." plan 0))
      (check-equal? (hash-ref ctx 'capabilities-used) '())
      (check-false (should-skip-verification? ctx)))

    (test-case "E2E-5: empty wave (0 files, 0 tasks) skips verification"
      ;; No files, no tasks → capabilities '(), ≤2 files → skip
      (define plan (make-plan #:files '()))
      (define ctx (build-enriched-plan-ctx "." plan 0))
      (check-equal? (hash-ref ctx 'capabilities-used) '())
      (check-true (should-skip-verification? ctx)))

    ;; ════════════════════════════════════════════════════════
    ;; §6.2: effective-risk-threshold — Dynamic Risk Threshold
    ;; ════════════════════════════════════════════════════════

    (test-case "E2E-6: file-write wave gets 'medium risk threshold"
      ;; .rkt files → file-write capability → threshold escalated to 'medium
      (define plan (make-plan #:files '("agent/core.rkt")))
      (define ctx (build-enriched-plan-ctx "." plan 0))
      (check-equal? (effective-risk-threshold ctx 'high) 'medium))

    (test-case "E2E-7: shell-exec wave gets 'low risk threshold"
      ;; .sh file → shell-exec capability → threshold escalated to 'low
      (define plan (make-plan #:files '("scripts/build.sh")))
      (define ctx (build-enriched-plan-ctx "." plan 0))
      (check-equal? (effective-risk-threshold ctx 'high) 'low))

    (test-case "E2E-8: git-write wave (from tasks) gets 'low risk threshold"
      ;; No .sh or .rkt files, but task text mentions git → git-write inferred
      (define plan
        (make-plan #:files '("CHANGELOG.md")
                   #:tasks (list (make-task "Release" "git commit and push changes"))))
      (define ctx (build-enriched-plan-ctx "." plan 0))
      (check-not-false (member 'git-write (hash-ref ctx 'capabilities-used)))
      (check-equal? (effective-risk-threshold ctx 'high) 'low))

    (test-case "E2E-9: read-only wave preserves base threshold"
      ;; No dangerous capabilities → threshold stays at base value
      (define plan (make-plan #:files '("notes.txt")))
      (define ctx (build-enriched-plan-ctx "." plan 0))
      (check-equal? (effective-risk-threshold ctx 'high) 'high)
      (check-equal? (effective-risk-threshold ctx 'medium) 'medium))

    ;; ════════════════════════════════════════════════════════
    ;; Combined: multiple capability sources
    ;; ════════════════════════════════════════════════════════

    (test-case "E2E-10: mixed .rkt + .md files → only file-write capability"
      ;; Both .rkt and .md map to file-write → deduplicated to single entry
      (define plan (make-plan #:files '("agent/core.rkt" "README.md")))
      (define ctx (build-enriched-plan-ctx "." plan 0))
      (check-equal? (hash-ref ctx 'capabilities-used) '(file-write))
      (check-equal? (effective-risk-threshold ctx 'high) 'medium))

    (test-case "E2E-11: combined .rkt file + .sh file → file-write + shell-exec"
      ;; Both capabilities present, shell-exec takes priority in threshold
      (define plan (make-plan #:files '("agent/core.rkt" "scripts/run.sh")))
      (define ctx (build-enriched-plan-ctx "." plan 0))
      (define caps (hash-ref ctx 'capabilities-used))
      (check-not-false (member 'file-write caps))
      (check-not-false (member 'shell-exec caps))
      (check-equal? (length caps) 2)
      ;; shell-exec present → 'low (stricter than 'medium from file-write alone)
      (check-equal? (effective-risk-threshold ctx 'high) 'low))

    (test-case "E2E-12: .rkt file + git task → file-write + git-write, threshold 'low"
      ;; File inference + task inference combined
      (define plan
        (make-plan #:files '("agent/core.rkt")
                   #:tasks (list (make-task "Deploy" "git push to origin"))))
      (define ctx (build-enriched-plan-ctx "." plan 0))
      (define caps (hash-ref ctx 'capabilities-used))
      (check-not-false (member 'file-write caps))
      (check-not-false (member 'git-write caps))
      (check-equal? (effective-risk-threshold ctx 'high) 'low))

    ;; ════════════════════════════════════════════════════════
    ;; Edge cases: null plan, missing wave
    ;; ════════════════════════════════════════════════════════

    (test-case "E2E-13: null plan produces empty context, skips verification"
      (define ctx (build-enriched-plan-ctx "." #f 0))
      (check-equal? (hash-ref ctx 'capabilities-used) '())
      (check-equal? (hash-ref ctx 'files-changed) '())
      (check-true (should-skip-verification? ctx))
      ;; No caps → base threshold preserved
      (check-equal? (effective-risk-threshold ctx 'high) 'high))

    (test-case "E2E-14: missing wave index produces empty context"
      (define plan (make-plan #:files '("agent/core.rkt")))
      (define ctx (build-enriched-plan-ctx "." plan 99))
      (check-equal? (hash-ref ctx 'capabilities-used) '())
      (check-equal? (hash-ref ctx 'files-changed) '())
      (check-true (should-skip-verification? ctx)))))

(run-tests suite)
