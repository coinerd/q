#!/usr/bin/env racket
#lang racket/base

;; scripts/pre-release-check.rkt — Single-command pre-release truth check
;;
;; W10 (#8484): Release-tooling hardening.
;; Runs all release-truth checks in check-only mode:
;;   1. lint-version          (version consistency)
;;   2. sync-readme-status    (README status block matches CHANGELOG)
;;   3. metrics --lint        (README metrics table matches reality)
;;   4. metrics --lint-prose  (README prose counts match table)
;;   5. Required artifacts    (version.rkt, info.rkt, CHANGELOG.md, README.md)
;;
;; No mutations — all checks are read-only.
;; Exit 0 if all pass, 1 if any fail.
;;
;; Usage:
;;   cd q/ && racket scripts/pre-release-check.rkt

(require racket/system
         racket/string
         racket/port
         racket/list)

;; ============================================================
;; Pure logic (testable without subprocesses)
;; ============================================================

;; Required release artifacts relative to q/ directory.
(define REQUIRED-ARTIFACTS '("util/version.rkt" "info.rkt" "CHANGELOG.md" "README.md"))

;; Check that all required artifacts exist.
;; Pure: takes a file-exists? predicate.
(define (check-required-artifacts artifacts file-exists?)
  (for/list ([path (in-list artifacts)]
             #:unless (file-exists? path))
    path))

;; Summarize a list of check results into a report string.
;; Each result is (list name status output) where status is 'pass or 'fail.
(define (summarize-checks results)
  (define passes (filter (λ (r) (eq? (cadr r) 'pass)) results))
  (define fails (filter (λ (r) (eq? (cadr r) 'fail)) results))
  (define lines
    (append (list "---"
                  (format "Pre-Release Check Summary: ~a/~a passed" (length passes) (length results)))
            (for/list ([r (in-list results)])
              (format "  [~a] ~a" (if (eq? (cadr r) 'pass) "PASS" "FAIL") (car r)))
            (list "---")))
  (string-join lines "\n"))

;; Determine overall exit code from results.
(define (results-exit-code results)
  (if (ormap (λ (r) (eq? (cadr r) 'fail)) results) 1 0))

;; ============================================================
;; Subprocess runner (I/O layer)
;; ============================================================

;; Run a command, return (list name 'pass/'fail exit-code).
;; Output goes to the inherited stdout/stderr so the user sees it directly.
(define (run-check name command-args)
  (printf "  Running ~a ... " name)
  (flush-output)
  (define-values (proc out-port in-port err-port)
    (apply subprocess #f #f #f (find-executable-path "racket") command-args))
  (define exit-code
    (begin
      (subprocess-wait proc)
      (subprocess-status proc)))
  (printf "[~a]~n" (if (zero? exit-code) "PASS" "FAIL"))
  (list name (if (zero? exit-code) 'pass 'fail) exit-code))

;; ============================================================
;; Main
;; ============================================================

(define (main)
  (displayln "=== Pre-Release Truth Check ===")
  (displayln "")

  ;; 1. Check required artifacts
  (displayln "Checking required artifacts ...")
  (define missing (check-required-artifacts REQUIRED-ARTIFACTS file-exists?))
  (unless (null? missing)
    (for ([m (in-list missing)])
      (printf "  MISSING: ~a~n" m)))
  (displayln "")

  ;; 2. Run release-truth checks as subprocesses
  (displayln "Running release-truth checks ...")
  (define results
    (list (run-check "lint-version" '("scripts/lint-version.rkt"))
          (run-check "sync-readme-status --check" '("scripts/sync-readme-status.rkt" "--check"))
          (run-check "metrics --lint" '("scripts/metrics.rkt" "--lint"))
          (run-check "metrics --lint-prose" '("scripts/metrics.rkt" "--lint-prose"))))

  (displayln "")

  ;; 3. Summary
  (displayln (summarize-checks results))

  ;; 4. Exit
  (define exit-code (results-exit-code results))
  (if (zero? exit-code)
      (displayln "All release-truth checks PASSED")
      (displayln "Some release-truth checks FAILED"))
  (exit exit-code))

(module+ main
  (main))
