#!/usr/bin/env racket
#lang racket/base

;; pre-commit.rkt — Pre-commit hook for q.
;;
;; Checks staged .rkt files: runs format lint and associated tests.
;;
;; Flags:
;;   --install   Create .git/hooks/pre-commit that invokes this script
;;   --all       Run the full test suite instead of just affected tests
;;
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
(define all-mode? #f)

(define (parse-flags!)
  (for ([arg (in-vector (current-command-line-arguments))])
    (cond
      [(string=? arg "--install") (set! install-mode? #t)]
      [(string=? arg "--all")     (set! all-mode? #t)]
      [else (printf "Unknown flag: ~a~n" arg)])))

;; --- Install hook ---

(define (install-hook!)
  (define script-path
    (simplify-path
     (build-path (path-only (resolved-module-path-name
                              (variable-reference->resolved-module-path
                               (#%variable-reference))))
                 "pre-commit.rkt")))
  (define hook-path ".git/hooks/pre-commit")
  (define hook-content
    (format "#!/bin/sh\nexec racket ~a\n" script-path))
  (call-with-output-file hook-path
    (λ (out) (display hook-content out))
    #:exists 'truncate)
  ;; Make executable
  (system (format "chmod +x ~a" hook-path))
  (printf "Installed pre-commit hook: ~a~n" hook-path))

;; --- Get staged .rkt files ---

(define (get-staged-rkt-files)
  (define output
    (with-output-to-string
      (λ ()
        (system "git diff --cached --name-only --diff-filter=ACM"))))
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
            (begin (printf "Format lint: PASS~n") #t)
            (begin (printf "Format lint: FAIL~n") #f)))
      (begin
        (printf "WARNING: ~a not found, skipping format lint~n" lint-script)
        #t)))

;; --- Run test file ---

(define (run-test test-path)
  (printf "  Testing: ~a ... " test-path)
  (flush-output)
  (define exit-code
    (system/exit-code (format "raco test ~a 2>&1" test-path)))
  (if (= exit-code 0)
      (begin (printf "PASS~n") #t)
      (begin (printf "FAIL~n") #f)))

;; --- Run full test suite ---

(define (run-full-suite)
  (printf "~n--- Full Test Suite ---~n")
  (define exit-code
    (system/exit-code "raco test tests/ 2>&1"))
  (if (= exit-code 0)
      (begin (printf "Full test suite: PASS~n") #t)
      (begin (printf "Full test suite: FAIL~n") #f)))

;; --- Main ---

(define (main)
  (parse-flags!)

  (when install-mode?
    (install-hook!)
    (exit 0))

  ;; Ensure we're in the q/ directory
  (unless (file-exists? "main.rkt")
    (printf "ERROR: Run from the q/ directory (main.rkt not found).~n")
    (exit 1))

  (printf "=== q Pre-commit Check ===~n")

  (define all-pass #t)

  ;; 1. Format lint
  (unless (run-format-lint)
    (set! all-pass #f))

  (cond
    [all-mode?
     ;; Run full suite
     (unless (run-full-suite)
       (set! all-pass #f))]
    [else
     ;; 2. Get staged files and run affected tests
     (define staged (get-staged-rkt-files))
     (printf "~n--- Staged .rkt files: ~a ---~n"
             (if (null? staged) "(none)" (string-join staged ", ")))

     (define test-files
       (remove-duplicates
        (append-map source->test-files staged)))

     (if (null? test-files)
         (printf "No affected test files found.~n")
         (begin
           (printf "~n--- Affected Tests ---~n")
           (for ([tf (in-list (sort test-files string<?))])
             (unless (run-test tf)
               (set! all-pass #f)))))])

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
