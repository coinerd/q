#!/usr/bin/env racket
#lang racket/base

;; scripts/setup-dev.rkt — One-command developer environment bootstrap.
;;
;; Installs pre-commit hook, verifies tooling, and runs a quick health check.
;;
;; Usage:
;;   cd q/ && racket scripts/setup-dev.rkt

(require racket/file
         racket/port
         racket/string
         racket/system)

;; --- Helpers ---

(define (run-cmd name cmd)
  (printf "  ~a ... " name)
  (flush-output)
  (define exit-code (system/exit-code cmd))
  (if (= exit-code 0)
      (begin
        (printf "OK~n")
        #t)
      (begin
        (printf "FAIL~n")
        #f)))

(define (file-executable? path)
  (and (file-exists? path) (member 'execute (file-or-directory-permissions path))))

;; --- Steps ---

(define (step-verify-racket)
  (printf "~nStep 1: Verify Racket toolchain~n")
  (define all-pass #t)
  (unless (run-cmd "racket" "racket --version > /dev/null 2>&1")
    (set! all-pass #f))
  (unless (run-cmd "raco" "raco help > /dev/null 2>&1")
    (set! all-pass #f))
  all-pass)

(define (step-install-hook)
  (printf "~nStep 2: Install pre-commit hook~n")
  (define hook-path ".git/hooks/pre-commit")
  (if (and (file-exists? hook-path) (file-executable? hook-path))
      (begin
        (printf "  Pre-commit hook already installed.~n")
        #t)
      (run-cmd "install hook" "racket scripts/pre-commit.rkt --install")))

(define (step-verify-scripts)
  (printf "~nStep 3: Verify scripts~n")
  (define required
    '("scripts/lint-all.rkt" "scripts/pre-commit.rkt"
                             "scripts/sync-version.rkt"
                             "scripts/sync-readme-status.rkt"
                             "scripts/run-tests.rkt"))
  (define all-pass #t)
  (for ([s (in-list required)])
    (if (file-exists? s)
        (printf "  ~a: found~n" s)
        (begin
          (printf "  ~a: MISSING~n" s)
          (set! all-pass #f))))
  all-pass)

(define (step-quick-lint)
  (printf "~nStep 4: Quick lint health check~n")
  (run-cmd "lint" "racket scripts/lint-all.rkt --only=format,version-cross,deps,ci-readiness"))

;; --- Main ---

(define (main)
  (printf "=== q Developer Environment Setup ===~n")

  (unless (file-exists? "main.rkt")
    (printf "ERROR: Run from the q/ directory (main.rkt not found).~n")
    (exit 1))

  (define all-pass #t)
  (unless (step-verify-racket)
    (set! all-pass #f))
  (unless (step-install-hook)
    (set! all-pass #f))
  (unless (step-verify-scripts)
    (set! all-pass #f))
  (unless (step-quick-lint)
    (set! all-pass #f))

  (printf "~n=== Summary ===~n")
  (if all-pass
      (begin
        (printf "Dev environment ready. Happy hacking!~n")
        (exit 0))
      (begin
        (printf "Some setup steps failed. Please fix the issues above.~n")
        (exit 1))))

(main)
