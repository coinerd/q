#!/usr/bin/env racket
#lang racket/base

;; pre-commit.rkt — Pre-commit hook for q.
;;
;; Checks staged .rkt files: runs lint checks (via lint-all.rkt) and affected tests.
;; Never drifts from lint-all.rkt — lint checks are delegated.
;;
;; Flags:
;;   --install        Create .git/hooks/pre-commit (lint + affected tests)
;;   --install-full   Create .git/hooks/pre-commit (full lint + full tests)
;;   --all            Run the full test suite instead of just affected tests
;;   --full           Run ALL lint checks (including slow ones) + full tests
;;   --no-tests       Skip test phase (lint only — used by test-pre-commit.rkt)
;;
;; Default (no flags): fast lint checks + affected tests.
;; Exit 0 if all checks pass, 1 otherwise.

(require racket/file
         racket/list
         racket/match
         racket/path
         racket/port
         racket/string
         racket/system)

;; --- CLI flag parsing ---

(define install-mode? #f)
(define install-full-mode? #f)
(define all-mode? #f)
(define full-mode? #f)
(define no-tests? #f)

(define (parse-flags!)
  (for ([arg (in-vector (current-command-line-arguments))])
    (cond
      [(string=? arg "--install") (set! install-mode? #t)]
      [(string=? arg "--install-full") (set! install-full-mode? #t)]
      [(string=? arg "--all") (set! all-mode? #t)]
      [(string=? arg "--full") (set! full-mode? #t)]
      [(string=? arg "--no-tests") (set! no-tests? #t)]
      [else (printf "Unknown flag: ~a~n" arg)])))

;; --- Install hook ---

(define (install-hook! #:full? [full? #f])
  (define script-path
    (simplify-path
     (build-path (path-only (resolved-module-path-name (variable-reference->resolved-module-path
                                                        (#%variable-reference))))
                 "pre-commit.rkt")))
  (define hook-path ".git/hooks/pre-commit")
  (define hook-content
    (if full?
        (format "#!/bin/sh\nexec racket ~a --full\n" script-path)
        (format "#!/bin/sh\nexec racket ~a\n" script-path)))
  (call-with-output-file hook-path (λ (out) (display hook-content out)) #:exists 'truncate)
  ;; Make executable
  (system (format "chmod +x ~a" hook-path))
  (printf "Installed pre-commit hook: ~a~n" hook-path)
  (when full?
    (printf "  Mode: --full (all lints + full tests)~n")))

;; --- Get staged .rkt files ---

(define (get-staged-rkt-files)
  (define output
    (with-output-to-string (λ () (system "git diff --cached --name-only --diff-filter=ACM"))))
  (define lines (string-split output "\n"))
  (filter (λ (f) (string-suffix? f ".rkt")) lines))

;; --- Map source file to test file ---

;; Tests that should NOT be auto-discovered (they run the full lint pipeline
;; internally and would cause deep recursion / excessive slowness).
(define excluded-test-names '("test-pre-commit.rkt"))

(define (source->test-files src-path)
  (define fname (file-name-from-path src-path))
  (if fname
      (let* ([name (bytes->string/utf-8 (path->bytes fname))]
             [test-name (string-append "test-" name)])
        (if (and (not (member test-name excluded-test-names))
                 (file-exists? (build-path "tests" test-name)))
            (list (path->string (build-path "tests" test-name)))
            '()))
      '()))

;; --- Run lint checks via lint-all.rkt ---

(define fast-lint-checks
  (string-join '("format" "version-sync"
                          "version-validate"
                          "version-cross"
                          "protocols"
                          "imports"
                          "deps"
                          "metrics-sync"
                          "metrics-lint"
                          "prose"
                          "readme-status"
                          "changelog-dates"
                          "tests"
                          "deprecation"
                          "ci-readiness"
                          "ivg")
               ","))

(define (run-lint-checks #:full? [full? #f])
  (printf "~n--- Lint Checks (~a) ---~n" (if full? "all" "fast"))
  (define lint-script (build-path "scripts" "lint-all.rkt"))
  (unless (file-exists? lint-script)
    (printf "ERROR: lint-all.rkt not found~n")
    (exit 1))
  (define cmd
    (if full?
        (format "racket ~a" lint-script)
        (format "racket ~a --only=~a" lint-script fast-lint-checks)))
  (define exit-code (system/exit-code cmd))
  (if (= exit-code 0)
      (begin
        (printf "Lint checks: PASS~n")
        #t)
      (begin
        (printf "Lint checks: FAIL~n")
        #f)))

;; --- Run test file ---

(define (run-test test-path)
  (printf "  Testing: ~a ... " test-path)
  (flush-output)
  (define exit-code (system/exit-code (format "raco test ~a 2>&1" test-path)))
  (if (= exit-code 0)
      (begin
        (printf "PASS~n")
        #t)
      (begin
        (printf "FAIL~n")
        #f)))

;; --- Run full test suite ---

(define (run-full-suite)
  (printf "~n--- Full Test Suite (parallel) ---~n")
  (define exit-code (system/exit-code "racket scripts/run-tests.rkt 2>&1"))
  (if (= exit-code 0)
      (begin
        (printf "Full test suite: PASS~n")
        #t)
      (begin
        (printf "Full test suite: FAIL~n")
        #f)))

;; --- Main ---

(define (main)
  (parse-flags!)

  (cond
    [install-mode?
     (install-hook!)
     (exit 0)]
    [install-full-mode?
     (install-hook! #:full? #t)
     (exit 0)])

  ;; Ensure we're in the q/ directory
  (unless (file-exists? "main.rkt")
    (printf "ERROR: Run from the q/ directory (main.rkt not found).~n")
    (exit 1))

  (define mode-label
    (cond
      [full-mode? "--full (all lints + tests)"]
      [all-mode? "--all (fast lints + full tests)"]
      [no-tests? "--no-tests (lint only)"]
      [else "quick (fast lints + affected tests)"]))

  (printf "=== q Pre-commit Check [~a] ===~n" mode-label)

  (define all-pass #t)

  ;; 1. Lint checks (delegated to lint-all.rkt)
  (unless (run-lint-checks #:full? full-mode?)
    (set! all-pass #f))

  ;; 2. Tests (skipped when --no-tests to avoid infinite recursion)
  (unless no-tests?
    (cond
      [all-mode?
       (unless (run-full-suite)
         (set! all-pass #f))]
      [else
       (define staged (get-staged-rkt-files))
       (printf "~n--- Staged .rkt files: ~a ---~n"
               (if (null? staged)
                   "(none)"
                   (string-join staged ", ")))

       (define test-files (remove-duplicates (append-map source->test-files staged)))

       (if (null? test-files)
           (printf "No affected test files found.~n")
           (begin
             (printf "~n--- Affected Tests ---~n")
             (for ([tf (in-list (sort test-files string<?))])
               (unless (run-test tf)
                 (set! all-pass #f)))))]))

  ;; 3. Summary
  (printf "~n=== Summary ===~n")
  (if all-pass
      (begin
        (printf "All checks passed.~n")
        (exit 0))
      (begin
        (printf "Some checks failed.~n")
        (exit 1))))

(main)
