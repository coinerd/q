#lang racket

;; tests/test-gsd-context-bundle.rkt — Context bundle assembly tests
;;
;; Wave 1b of v0.21.0: Tests for role-specific context bundles.

(require rackunit
         "../extensions/gsd/context-bundle.rkt")

;; ============================================================
;; Explorer bundle
;; ============================================================

(test-case "explorer bundle: includes user request"
  (define artifacts (hasheq 'user-request "Fix the login bug"
                            'project-context "A web app"))
  (define bundle (assemble-context 'explorer artifacts (hasheq)))
  (check-true (string-contains? bundle "Fix the login bug")))

(test-case "explorer bundle: includes project context"
  (define artifacts (hasheq 'user-request ""
                            'project-context "Racket coding agent"))
  (define bundle (assemble-context 'explorer artifacts (hasheq)))
  (check-true (string-contains? bundle "Racket coding agent")))

(test-case "explorer bundle: works with empty artifacts"
  (define artifacts (hasheq 'user-request "" 'project-context ""))
  (define bundle (assemble-context 'explorer artifacts (hasheq)))
  (check-true (string? bundle))
  (check-true (> (string-length bundle) 0)))

;; ============================================================
;; Executor bundle
;; ============================================================

(test-case "executor bundle: includes plan text"
  (define artifacts (hasheq 'plan "## Wave 0: Fix bug\n- File: src/fix.rkt\n- Verify: test\n"
                            'state "In progress"
                            'wave-index 0))
  (define bundle (assemble-context 'executor artifacts (hasheq)))
  (check-true (string-contains? bundle "Wave 0")))

(test-case "executor bundle: includes state"
  (define artifacts (hasheq 'plan "## Wave 0: Fix\n- File: a\n- Verify: t\n"
                            'state "Wave 0 in progress"
                            'wave-index 0))
  (define bundle (assemble-context 'executor artifacts (hasheq)))
  (check-true (string-contains? bundle "Wave 0 in progress")))

(test-case "executor bundle: includes referenced files"
  (define artifacts (hasheq 'plan "## Wave 0: Fix\n- File: src/fix.rkt\n- Verify: t\n"
                            'state ""
                            'wave-index 0))
  (define files (hasheq "src/fix.rkt" "(define (fix) 42)"))
  (define bundle (assemble-context 'executor artifacts files))
  (check-true (string-contains? bundle "src/fix.rkt"))
  (check-true (string-contains? bundle "(define (fix) 42)")))

;; ============================================================
;; Verifier bundle
;; ============================================================

(test-case "verifier bundle: includes plan"
  (define artifacts (hasheq 'plan "## Wave 0: Fix\n- Verify: raco test tests/\n"
                            'summaries "Wave 0: fixed the bug"))
  (define bundle (assemble-context 'verifier artifacts (hasheq)))
  (check-true (string-contains? bundle "Wave 0")))

(test-case "verifier bundle: includes summaries"
  (define artifacts (hasheq 'plan "## Wave 0: Fix\n- Verify: t\n"
                            'summaries "All waves completed"))
  (define bundle (assemble-context 'verifier artifacts (hasheq)))
  (check-true (string-contains? bundle "All waves completed")))

(test-case "verifier bundle: extracts verify commands"
  (define artifacts (hasheq 'plan "## Wave 0: Fix\n- Verify: raco test tests/\n"
                            'summaries ""))
  (define bundle (assemble-context 'verifier artifacts (hasheq)))
  (check-true (string-contains? bundle "raco test")))

;; ============================================================
;; Size warnings
;; ============================================================

(test-case "bundle-size-warning?: returns #f for small bundles"
  (define artifacts (hasheq 'user-request "Fix bug" 'project-context ""))
  (define bundle (assemble-context 'explorer artifacts (hasheq)))
  (check-false (bundle-size-warning? bundle)))

(test-case "bundle-size-warning?: returns #t for large bundles"
  (define big-plan (make-string 5000 #\X))
  (define artifacts (hasheq 'plan big-plan 'state big-plan 'wave-index 0))
  (define bundle (assemble-context 'executor artifacts (hasheq)))
  (check-true (bundle-size-warning? bundle)))

;; ============================================================
;; Error cases
;; ============================================================

(test-case "assemble-context: unknown role raises error"
  (check-exn exn:fail?
             (lambda () (assemble-context 'unknown (hasheq) (hasheq)))))
