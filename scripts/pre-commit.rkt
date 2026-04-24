#!/usr/bin/env racket
#lang racket/base

;; pre-commit.rkt — Pre-commit hook for q.
;;
;; Checks staged .rkt files: runs format lint and associated tests.
;;
;; Flags:
;;   --install        Create .git/hooks/pre-commit (format + compile + tests)
;;   --install-full   Create .git/hooks/pre-commit (format + compile + ci-local + tests)
;;   --all            Run the full test suite instead of just affected tests
;;   --ci             Also run ci-local.rkt (full CI lint suite)
;;   --full           Format + compile + ci-local + affected tests
;;
;; Default (no flags): format lint + version sync + metrics lint + affected tests.
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
(define ci-mode? #f)
(define full-mode? #f)

(define (parse-flags!)
  (for ([arg (in-vector (current-command-line-arguments))])
    (cond
      [(string=? arg "--install") (set! install-mode? #t)]
      [(string=? arg "--install-full") (set! install-full-mode? #t)]
      [(string=? arg "--all") (set! all-mode? #t)]
      [(string=? arg "--ci") (set! ci-mode? #t)]
      [(string=? arg "--full") (set! full-mode? #t)]
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
    (printf "  Mode: --full (format + compile + ci-local + tests)~n")))

;; --- Get staged .rkt files ---

(define (get-staged-rkt-files)
  (define output
    (with-output-to-string (λ () (system "git diff --cached --name-only --diff-filter=ACM"))))
  (define lines (string-split output "\n"))
  (filter (λ (f) (string-suffix? f ".rkt")) lines))

;; --- Map source file to test file ---

(define (source->test-files src-path)
  ;; "agent/types.rkt" -> "tests/test-types.rkt"
  ;; "runtime/session-store.rkt" -> "tests/test-session-store.rkt"
  ;; "llm/provider.rkt" -> "tests/test-provider.rkt"
  (define fname (file-name-from-path src-path))
  (if fname
      (let* ([name (bytes->string/utf-8 (path->bytes fname))]
             [test-name (string-append "test-" name)]
             [test-path (build-path "tests" test-name)])
        (if (file-exists? test-path)
            (list (path->string test-path))
            '()))
      '()))

;; --- Run format lint ---

(define (run-format-lint)
  (printf "~n--- Format Lint ---~n")
  (define lint-script (build-path "scripts" "lint-format.rkt"))
  (if (file-exists? lint-script)
      (let ([exit-code (system/exit-code (format "racket ~a" lint-script))])
        (if (= exit-code 0)
            (begin
              (printf "Format lint: PASS~n")
              #t)
            (begin
              (printf "Format lint: FAIL~n")
              #f)))
      (begin
        (printf "WARNING: ~a not found, skipping format lint~n" lint-script)
        #t)))

;; --- Run version sync lint (dry-run) ---

(define (run-version-lint)
  (printf "~n--- Version Sync Lint (dry-run) ---~n")
  (define script (build-path "scripts" "sync-version.rkt"))
  (if (file-exists? script)
      (let ([exit-code (system/exit-code (format "racket ~a" script))])
        (if (= exit-code 0)
            (begin
              (printf "Version sync: PASS~n")
              #t)
            (begin
              (printf "Version sync: FAIL (drift detected)~n")
              #f)))
      (begin
        (printf "WARNING: ~a not found, skipping version lint~n" script)
        #t)))

;; --- Run metrics lint (static) ---

(define (run-metrics-lint)
  (printf "~n--- Metrics Lint (static) ---~n")
  (define script (build-path "scripts" "metrics.rkt"))
  (if (file-exists? script)
      (let ([exit-code (system/exit-code (format "racket ~a --lint" script))])
        (if (= exit-code 0)
            (begin
              (printf "Metrics lint: PASS~n")
              #t)
            (begin
              (printf "Metrics lint: FAIL (README metrics diverged)~n")
              #f)))
      (begin
        (printf "WARNING: ~a not found, skipping metrics lint~n" script)
        #t)))

;; --- Run CI local lint suite ---

(define (run-ci-local)
  (printf "~n--- CI Local Lint Suite ---~n")
  (define ci-script (build-path "scripts" "ci-local.rkt"))
  (if (file-exists? ci-script)
      (let ([exit-code (system/exit-code (format "racket ~a" ci-script))])
        (if (= exit-code 0)
            (begin
              (printf "CI Local: PASS~n")
              #t)
            (begin
              (printf "CI Local: FAIL~n")
              #f)))
      (begin
        (printf "WARNING: ~a not found, skipping CI local~n" ci-script)
        #t)))

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
      [full-mode? "--full (format + compile + ci-local + tests)"]
      [ci-mode? "--ci (format + compile + ci-local)"]
      [else "quick (format + version + metrics + tests)"]))

  (printf "=== q Pre-commit Check [~a] ===~n" mode-label)

  (define all-pass #t)

  ;; 1. Format lint
  (unless (run-format-lint)
    (set! all-pass #f))

  ;; 2. Version sync lint (default mode)
  (unless (run-version-lint)
    (set! all-pass #f))

  ;; 3. Metrics lint (default mode)
  (unless (run-metrics-lint)
    (set! all-pass #f))

  ;; 4. CI local lint (if --ci or --full)
  (when (or ci-mode? full-mode?)
    (unless (run-ci-local)
      (set! all-pass #f)))

  (cond
    [all-mode?
     ;; Run full suite
     (unless (run-full-suite)
       (set! all-pass #f))]
    [else
     ;; 5. Get staged files and run affected tests
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
               (set! all-pass #f)))))])

  ;; 6. Summary
  (printf "~n=== Summary ===~n")
  (if all-pass
      (begin
        (printf "All checks passed.~n")
        (exit 0))
      (begin
        (printf "Some checks failed.~n")
        (exit 1))))

(main)
