#lang racket/base

;; @speed fast
;; @suite default

;; tests/test-plan-context-enrichment.rkt
;; v0.99.23 B-1/B-2: Tests for plan-context enrichment.
;; v0.99.24 W1: Enhanced capability inference tests (file-extension table + tasks).
;; Verifies that build-enriched-plan-ctx populates real wave data
;; (files-changed, capabilities-used, plan-summary, wave-name).

(require rackunit
         rackunit/text-ui
         racket/string
         racket/list
         "../extensions/gsd/plan-context-builder.rkt"
         "../extensions/gsd/plan-types.rkt")

;; ── Helpers ──

(define (make-test-plan #:files [files '("a.rkt" "b.rkt")]
                        #:title [title "Test Wave"]
                        #:tasks [tasks '()])
  (gsd-plan (list (gsd-wave 0 title 'pending "" files tasks "" '())) #f '() '()))

(define (make-empty-plan)
  (gsd-plan '() #f '() '()))

(define (make-task name action)
  (gsd-task name '() action "" "" 'pending))

;; ── Test Suite ──

(define suite
  (test-suite "Plan Context Enrichment (v0.99.23 + v0.99.24 W1)"

    ;; ── infer-capabilities-from-files (v0.99.23 original) ──

    (test-case "infer-capabilities-from-files returns file-write for .rkt files"
      (check-equal? (infer-capabilities-from-files '("a.rkt" "b.rkt")) '(file-write)))

    (test-case "infer-capabilities-from-files returns file-write for single .rkt file"
      (check-equal? (infer-capabilities-from-files '("agent/core.rkt")) '(file-write)))

    (test-case "infer-capabilities-from-files returns empty for unknown file types"
      ;; v0.99.24 W1: .md now maps to file-write, so use .txt/.csv (not in table)
      (check-equal? (infer-capabilities-from-files '("data.txt" "log.csv")) '()))

    (test-case "infer-capabilities-from-files returns empty for no files"
      (check-equal? (infer-capabilities-from-files '()) '()))

    ;; ── infer-capabilities-from-files (v0.99.24 W1 enhanced) ──

    (test-case "infer-capabilities-from-files detects shell-exec from .sh files"
      (check-equal? (infer-capabilities-from-files '("deploy.sh")) '(shell-exec)))

    (test-case "infer-capabilities-from-files detects file-write from .md files"
      (check-equal? (infer-capabilities-from-files '("README.md")) '(file-write)))

    (test-case "infer-capabilities-from-files infers multiple capabilities from mixed files"
      (define caps (infer-capabilities-from-files '("agent/core.rkt" "scripts/deploy.sh")))
      (check-not-false (member 'file-write caps))
      (check-not-false (member 'shell-exec caps))
      (check-equal? (length caps) 2))

    (test-case "infer-capabilities-from-files deduplicates capabilities"
      ;; Multiple .rkt files should still produce only one 'file-write
      (check-equal? (infer-capabilities-from-files '("a.rkt" "b.rkt" "c.rkt" "d.rkt")) '(file-write)))

    ;; ── infer-capabilities-from-tasks (v0.99.24 W1 new) ──

    (test-case "infer-capabilities-from-tasks detects shell-exec from task text"
      (define wave
        (gsd-wave 0
                  "Test"
                  'pending
                  ""
                  '()
                  (list (make-task "Run tests" "Execute bash test runner"))
                  ""
                  '()))
      (define caps (infer-capabilities-from-tasks wave))
      (check-not-false (member 'shell-exec caps)))

    (test-case "infer-capabilities-from-tasks detects git-write from task text"
      (define wave
        (gsd-wave 0
                  "Test"
                  'pending
                  ""
                  '()
                  (list (make-task "Deploy" "git commit and push changes"))
                  ""
                  '()))
      (define caps (infer-capabilities-from-tasks wave))
      (check-not-false (member 'git-write caps)))

    (test-case "infer-capabilities-from-tasks returns empty for read-only tasks"
      (define wave
        (gsd-wave 0
                  "Test"
                  'pending
                  ""
                  '()
                  (list (make-task "Review" "Read documentation and provide feedback"))
                  ""
                  '()))
      (define caps (infer-capabilities-from-tasks wave))
      (check-equal? caps '()))

    (test-case "infer-capabilities-from-tasks returns empty for wave with no tasks"
      (define wave (gsd-wave 0 "Test" 'pending "" '() '() "" '()))
      (define caps (infer-capabilities-from-tasks wave))
      (check-equal? caps '()))

    (test-case "infer-capabilities-from-tasks returns empty for #f wave"
      (check-equal? (infer-capabilities-from-tasks #f) '()))

    ;; ── build-enriched-plan-ctx ──

    (test-case "build-enriched-plan-ctx returns hash with capabilities-used key"
      (define plan (make-test-plan))
      (define ctx (build-enriched-plan-ctx "." plan 0))
      (check-true (hash? ctx))
      (check-true (hash-has-key? ctx 'capabilities-used)))

    (test-case "build-enriched-plan-ctx includes file-write for .rkt files"
      (define plan (make-test-plan #:files '("src/core.rkt" "tests/test.rkt")))
      (define ctx (build-enriched-plan-ctx "." plan 0))
      (check-equal? (hash-ref ctx 'capabilities-used) '(file-write)))

    (test-case "build-enriched-plan-ctx returns empty capabilities when no files"
      (define plan (make-test-plan #:files '()))
      (define ctx (build-enriched-plan-ctx "." plan 0))
      (check-equal? (hash-ref ctx 'capabilities-used) '()))

    (test-case "build-enriched-plan-ctx returns real file list from wave"
      (define plan (make-test-plan #:files '("a.rkt" "b.rkt" "c.rkt")))
      (define ctx (build-enriched-plan-ctx "." plan 0))
      (check-equal? (hash-ref ctx 'files-changed) '("a.rkt" "b.rkt" "c.rkt")))

    (test-case "build-enriched-plan-ctx wave-name includes wave title"
      (define plan (make-test-plan #:title "My Cool Wave"))
      (define ctx (build-enriched-plan-ctx "." plan 0))
      (check-true (string-contains? (hash-ref ctx 'wave-name) "My Cool Wave")))

    (test-case "build-enriched-plan-ctx plan-summary includes wave titles"
      (define plan (make-test-plan #:title "Test Wave"))
      (define ctx (build-enriched-plan-ctx "." plan 0))
      (check-true (string-contains? (hash-ref ctx 'plan-summary) "Test Wave")))

    (test-case "build-enriched-plan-ctx handles missing wave index"
      (define plan (make-test-plan))
      (define ctx (build-enriched-plan-ctx "." plan 99))
      (check-equal? (hash-ref ctx 'files-changed) '())
      (check-equal? (hash-ref ctx 'capabilities-used) '()))

    (test-case "build-enriched-plan-ctx handles null plan"
      (define ctx (build-enriched-plan-ctx "." #f 0))
      (check-equal? (hash-ref ctx 'files-changed) '())
      (check-equal? (hash-ref ctx 'capabilities-used) '())
      (check-equal? (hash-ref ctx 'plan-summary) ""))

    (test-case "build-enriched-plan-ctx returns test-summary string"
      (define plan (make-test-plan))
      (define ctx (build-enriched-plan-ctx "." plan 0))
      (check-true (string? (hash-ref ctx 'test-summary))))

    (test-case "build-enriched-plan-ctx test-summary is descriptive (not 'tests not run')"
      ;; v0.99.24 W1: test-summary should now have a meaningful message
      (define plan (make-test-plan))
      (define ctx (build-enriched-plan-ctx "." plan 0))
      (define ts (hash-ref ctx 'test-summary))
      (check-true (and (string? ts) (> (string-length ts) 0)))
      (check-false (string=? ts "tests not run")))

    ;; ── v0.99.24 W1: Combined file + task inference ──

    (test-case "build-enriched-plan-ctx combines file and task capabilities"
      (define plan
        (make-test-plan #:files '("agent/core.rkt" "scripts/deploy.sh")
                        #:tasks (list (make-task "Deploy" "git commit and push"))))
      (define ctx (build-enriched-plan-ctx "." plan 0))
      (define caps (hash-ref ctx 'capabilities-used))
      ;; Should have file-write from .rkt, shell-exec from .sh, git-write from task
      (check-not-false (member 'file-write caps))
      (check-not-false (member 'shell-exec caps))
      (check-not-false (member 'git-write caps))
      (check-equal? (length caps) 3))

    ;; ── Integration: enriched data makes §6.1 and §6.2 work ──

    (test-case "enriched ctx with .rkt files has file-write capability (enables §6.2)"
      ;; With 'file-write present, effective-risk-threshold should return 'medium
      (define plan (make-test-plan #:files '("agent/core.rkt")))
      (define ctx (build-enriched-plan-ctx "." plan 0))
      (check-not-false (member 'file-write (hash-ref ctx 'capabilities-used))))

    (test-case "enriched ctx with no files has empty capabilities (§6.1 won't skip)"
      ;; With no capabilities, should-skip-verification? returns #f (conservative)
      (define plan (make-test-plan #:files '()))
      (define ctx (build-enriched-plan-ctx "." plan 0))
      (check-equal? (hash-ref ctx 'capabilities-used) '()))))

(run-tests suite)
