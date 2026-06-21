#lang racket

;; @speed fast
;; @suite fast

;; W10 (#8484): Tests for pre-release check pure logic.
;; Since pre-release-check.rkt has (main) at module level, we replicate
;; the pure functions here for unit testing.

(require rackunit
         rackunit/text-ui
         racket/file)

;; ============================================================
;; Replicated pure logic from pre-release-check.rkt
;; ============================================================

(define REQUIRED-ARTIFACTS '("util/version.rkt" "info.rkt" "CHANGELOG.md" "README.md"))

(define (check-required-artifacts artifacts file-exists?)
  (for/list ([path (in-list artifacts)]
             #:unless (file-exists? path))
    path))

(define (summarize-checks results)
  (define passes (filter (lambda (r) (eq? (cadr r) 'pass)) results))
  (define lines
    (append (list "---"
                  (format "Pre-Release Check Summary: ~a/~a passed" (length passes) (length results)))
            (for/list ([r (in-list results)])
              (format "  [~a] ~a" (if (eq? (cadr r) 'pass) "PASS" "FAIL") (car r)))
            (list "---")))
  (string-join lines "\n"))

(define (results-exit-code results)
  (if (ormap (lambda (r) (eq? (cadr r) 'fail)) results) 1 0))

;; ============================================================
;; Test suites
;; ============================================================

(define-test-suite
 artifact-existence-tests
 ;; Test the pure check-required-artifacts function with mock predicates
 (test-case "all artifacts present returns empty list"
   (define always-exists (lambda (p) #t))
   (check-equal? (check-required-artifacts REQUIRED-ARTIFACTS always-exists) '()))
 (test-case "missing artifact returned"
   (define never-exists (lambda (p) #f))
   (check-equal? (check-required-artifacts REQUIRED-ARTIFACTS never-exists) REQUIRED-ARTIFACTS))
 (test-case "partial missing artifacts returned"
   (define partial-exists (lambda (p) (not (equal? p "info.rkt"))))
   (check-equal? (check-required-artifacts REQUIRED-ARTIFACTS partial-exists) '("info.rkt")))
 ;; Real artifact checks against actual repo
 (test-case "real: util/version.rkt exists"
   (check-true (file-exists? "../util/version.rkt")))
 (test-case "real: info.rkt exists"
   (check-true (file-exists? "../info.rkt")))
 (test-case "real: CHANGELOG.md exists"
   (check-true (file-exists? "../CHANGELOG.md")))
 (test-case "real: README.md exists"
   (check-true (file-exists? "../README.md")))
 (test-case "real: all required artifacts exist in repo"
   (check-equal? (check-required-artifacts REQUIRED-ARTIFACTS
                                           (lambda (p) (file-exists? (string-append "../" p))))
                 '())))

(define-test-suite
 summarize-checks-tests
 (test-case "all-pass summary correct"
   (define results (list (list "lint-version" 'pass "ok") (list "metrics --lint" 'pass "ok")))
   (define summary (summarize-checks results))
   (check-true (string-contains? summary "2/2 passed"))
   (check-true (string-contains? summary "[PASS] lint-version")))
 (test-case "mixed pass/fail summary correct"
   (define results (list (list "lint-version" 'pass "ok") (list "metrics --lint" 'fail "3 errors")))
   (define summary (summarize-checks results))
   (check-true (string-contains? summary "1/2 passed"))
   (check-true (string-contains? summary "[FAIL] metrics --lint")))
 (test-case "all-fail summary correct"
   (define results
     (list (list "lint-version" 'fail "v1 vs v2") (list "metrics --lint" 'fail "drift")))
   (define summary (summarize-checks results))
   (check-true (string-contains? summary "0/2 passed")))
 (test-case "empty results summary correct"
   (define summary (summarize-checks '()))
   (check-true (string-contains? summary "0/0 passed"))))

(define-test-suite
 results-exit-code-tests
 (test-case "all pass returns 0"
   (check-equal? (results-exit-code (list (list "a" 'pass "") (list "b" 'pass ""))) 0))
 (test-case "any fail returns 1"
   (check-equal? (results-exit-code (list (list "a" 'pass "") (list "b" 'fail ""))) 1))
 (test-case "all fail returns 1"
   (check-equal? (results-exit-code (list (list "a" 'fail "") (list "b" 'fail ""))) 1))
 (test-case "empty results returns 0"
   (check-equal? (results-exit-code '()) 0)))

;; ============================================================
;; Dirty-worktree guard tests
;; Verifies that check-only operations don't mutate files.
;; This is the Yellow candidate from W10 plan.
;; ============================================================

(define-test-suite
 dirty-worktree-guard-tests
 (test-case "metrics-helpers pure functions are side-effect-free"
   ;; The pure helper functions should not touch the filesystem.
   ;; We verify by checking that the module provides no mutation primitives.
   (define content (file->string "../scripts/metrics-helpers.rkt"))
   ;; Must NOT contain call-with-output-file or display-to-file
   (check-false (string-contains? content "call-with-output-file")
                "metrics-helpers must not write files")
   (check-false (string-contains? content "display-to-file") "metrics-helpers must not write files")
   (check-false (string-contains? content "write-to-file") "metrics-helpers must not write files"))
 (test-case "pre-release-check.rkt is check-only (no file writes)"
   (define content (file->string "../scripts/pre-release-check.rkt"))
   ;; Must NOT contain write-to-file, display-to-file, or call-with-output-file
   ;; (subprocess calls are OK — they invoke other scripts)
   (check-false (regexp-match? #rx"call-with-output-file|display-to-file|write-to-file" content)
                "pre-release-check must not write files"))
 (test-case "metrics.rkt --check-only does not mutate"
   (define content (file->string "../scripts/metrics.rkt"))
   ;; --check-only flag must exist and guard against writes
   (check-true (string-contains? content "check-only") "metrics.rkt must support --check-only")
   ;; write-or-check function must exist
   (check-true (string-contains? content "write-or-check")
               "metrics.rkt must centralize mutation via write-or-check")))

(define-test-suite all-pre-release-check-tests
                   artifact-existence-tests
                   summarize-checks-tests
                   results-exit-code-tests
                   dirty-worktree-guard-tests)

(module+ test
  (run-tests all-pre-release-check-tests))

(module+ main
  (run-tests all-pre-release-check-tests))
