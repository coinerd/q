#lang racket/base

;; @speed fast
;; @suite default

;; tests/test-plan-context-enrichment.rkt
;; v0.99.23 B-1/B-2: Tests for plan-context enrichment.
;; Verifies that build-enriched-plan-ctx populates real wave data
;; (files-changed, capabilities-used, plan-summary, wave-name).

(require rackunit
         rackunit/text-ui
         racket/string
         "../extensions/gsd/plan-context-builder.rkt"
         "../extensions/gsd/plan-types.rkt")

;; ── Helpers ──

(define (make-test-plan #:files [files '("a.rkt" "b.rkt")] #:title [title "Test Wave"])
  (gsd-plan (list (gsd-wave 0 title 'pending "" files '() "" '())) #f '() '()))

(define (make-empty-plan)
  (gsd-plan '() #f '() '()))

;; ── Test Suite ──

(define suite
  (test-suite "Plan Context Enrichment (v0.99.23 B-1/B-2)"

    ;; ── infer-capabilities-from-files ──

    (test-case "infer-capabilities-from-files returns file-write for .rkt files"
      (check-equal? (infer-capabilities-from-files '("a.rkt" "b.rkt")) '(file-write)))

    (test-case "infer-capabilities-from-files returns file-write for single .rkt file"
      (check-equal? (infer-capabilities-from-files '("agent/core.rkt")) '(file-write)))

    (test-case "infer-capabilities-from-files returns empty for non-.rkt files"
      (check-equal? (infer-capabilities-from-files '("README.md" "doc.txt")) '()))

    (test-case "infer-capabilities-from-files returns empty for no files"
      (check-equal? (infer-capabilities-from-files '()) '()))

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
      ;; capabilities-used is '() (empty list) — should-skip-verification? returns #f
      ;; because it requires capabilities-used to be truthy AND not contain dangerous caps
      (check-equal? (hash-ref ctx 'capabilities-used) '()))))

(run-tests suite)
