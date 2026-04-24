#!/usr/bin/env racket
#lang racket/base

;; scripts/ci-local.rkt — Run the full CI lint suite locally.
;;
;; Mirrors the checks in .github/workflows/ci.yml so that issues are caught
;; before pushing to main.
;;
;; Usage:
;;   cd q/ && racket scripts/ci-local.rkt          # all checks
;;   cd q/ && racket scripts/ci-local.rkt --quick   # skip slow checks
;;   cd q/ && racket scripts/ci-local.rkt --fix     # auto-fix drift, then re-lint
;;
;; Exit 0 if all pass, 1 if any fail.

(require racket/list
         racket/string
         racket/system
         racket/match)

;; ---------------------------------------------------------------------------
;; CLI flags
;; ---------------------------------------------------------------------------

(define quick? #f)
(define fix? #f)

(define (parse-flags!)
  (for ([arg (in-vector (current-command-line-arguments))])
    (cond
      [(string=? arg "--quick") (set! quick? #t)]
      [(string=? arg "--fix") (set! fix? #t)]
      [else (printf "Unknown flag: ~a~n" arg)])))

;; ---------------------------------------------------------------------------
;; Check runner
;; ---------------------------------------------------------------------------

(struct check-result (name passed? output) #:transparent)

(define (run-check name command)
  "Run a shell command, return a check-result."
  (printf "  Running: ~a ... " name)
  (flush-output)
  (define out (open-output-string))
  (define exit-code
    (parameterize ([current-output-port out]
                   [current-error-port out])
      (system/exit-code command)))
  (define passed? (= exit-code 0))
  (if passed?
      (printf "OK~n")
      (printf "FAIL~n"))
  (check-result name passed? (get-output-string out)))

;; ---------------------------------------------------------------------------
;; Fix runner
;; ---------------------------------------------------------------------------

(define (run-fix! name command)
  "Run a fix command, print output."
  (printf "  Fixing: ~a ... " name)
  (flush-output)
  (define exit-code
    (parameterize ([current-output-port (current-output-port)]
                   [current-error-port (current-output-port)])
      (system/exit-code command)))
  (if (= exit-code 0)
      (printf "OK~n")
      (printf "FAILED (exit ~a)~n" exit-code))
  exit-code)

;; ---------------------------------------------------------------------------
;; Check definitions
;; ---------------------------------------------------------------------------

;; Each check: (list name command)
;; Ordered to match CI workflow.

(define all-checks
  ;; Group 1: Version consistency
  (list (list "sync-version (dry-run)" "racket scripts/sync-version.rkt")
        (list "lint-version" "racket scripts/lint-version.rkt")
        (list "sync-readme-status --check" "racket scripts/sync-readme-status.rkt --check")
        ;; Group 2: Code quality
        (list "lint-tests" "racket scripts/lint-tests.rkt")
        (list "lint-format" "racket scripts/lint-format.rkt")
        ;; Group 2b: Compilation (W6.1)
        (list "compile-all" "raco make main.rkt")
        ;; Group 3: Metrics + protocol consistency
        (list "metrics --lint --lint-prose" "racket scripts/metrics.rkt --lint --lint-prose")
        (list "check-protocols" "racket scripts/check-protocols.rkt")
        (list "check-imports" "racket scripts/check-imports.rkt")
        ;; Group 4: Dependency completeness
        (list "check-deps" "racket scripts/check-deps.rkt")
        ;; Group 5: Security (W6.3)
        (list "lint-security" "racket scripts/lint-security.rkt")))

(define quick-checks
  (list (list "sync-version (dry-run)" "racket scripts/sync-version.rkt")
        (list "lint-version" "racket scripts/lint-version.rkt")
        (list "sync-readme-status --check" "racket scripts/sync-readme-status.rkt --check")
        (list "lint-format" "racket scripts/lint-format.rkt")
        (list "compile-main" "raco make main.rkt 2>&1")))

;; ---------------------------------------------------------------------------
;; Fix definitions
;; ---------------------------------------------------------------------------

(define fix-commands
  (list (list "sync-readme-status --sync" "racket scripts/sync-readme-status.rkt --sync")
        (list "metrics --sync-all" "racket scripts/metrics.rkt --sync-all")))

;; ---------------------------------------------------------------------------
;; Main
;; ---------------------------------------------------------------------------

(define (main)
  (parse-flags!)

  ;; Ensure we're in the q/ directory
  (unless (file-exists? "main.rkt")
    (printf "ERROR: Run from the q/ directory (main.rkt not found).~n")
    (exit 1))

  (printf "~n=== CI Local Lint Suite ===~n")
  (when quick?
    (printf "Mode: --quick (skipping slow checks)~n"))
  (when fix?
    (printf "Mode: --fix (auto-fixing drift before lint)~n"))
  (printf "~n")

  ;; Phase 1: Auto-fix (if --fix)
  (when fix?
    (printf "--- Phase 1: Auto-fix drift ---~n")
    (for ([f (in-list fix-commands)])
      (run-fix! (car f) (cadr f)))
    (printf "~n"))

  ;; Phase 2: Run checks
  (printf "--- Phase 2: Lint checks ---~n")
  (define checks (if quick? quick-checks all-checks))
  (define results
    (for/list ([c (in-list checks)])
      (run-check (car c) (cadr c))))

  ;; Phase 3: Summary
  (define failures (filter (λ (r) (not (check-result-passed? r))) results))
  (printf "~n--- Summary ---~n")
  (printf "Checks: ~a total, ~a passed, ~a failed~n"
          (length results)
          (- (length results) (length failures))
          (length failures))

  (when (not (null? failures))
    (printf "~nFailed checks:~n")
    (for ([f (in-list failures)])
      (printf "  ✗ ~a~n" (check-result-name f))
      ;; Print first 5 lines of error output
      (define lines (string-split (check-result-output f) "\n"))
      (define error-lines (filter (λ (l) (regexp-match? #rx"ERROR|FAIL|MISMATCH" l)) lines))
      (for ([l (in-list (take (if (> (length error-lines) 5) error-lines error-lines)
                              (min 5 (length error-lines))))])
        (printf "    ~a~n" l))))

  (if (null? failures)
      (begin
        (printf "~nAll checks passed. ✓~n")
        (exit 0))
      (begin
        (printf "~nSome checks failed. ✗~n")
        (exit 1))))

(main)
