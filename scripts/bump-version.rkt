#!/usr/bin/env racket
#lang racket/base

;; scripts/bump-version.rkt — Single-command version sweep.
;;
;; Updates all version surfaces across the project to a new version string.
;;
;; Usage:
;;   cd q/ && racket scripts/bump-version.rkt X.Y.Z            # update all
;;   cd q/ && racket scripts/bump-version.rkt X.Y.Z --dry-run  # preview only
;;   cd q/ && racket scripts/bump-version.rkt X.Y.Z --no-verify # skip ci-local check
;;
;; Surfaces updated:
;;   1. util/version.rkt (canonical source)
;;   2. info.rkt
;;   3. docs/**/*.md — version references
;;   4. README.md — badge, table, prose, status block
;;   5. Runs metrics.rkt --sync-all
;;   6. Runs sync-readme-status.rkt --sync
;;   7. Runs ci-local.rkt to verify (unless --no-verify)

(require racket/file
         racket/list
         racket/path
         racket/string
         racket/system)

;; --- CLI parsing ---

(define args (vector->list (current-command-line-arguments)))
(define dry-run? (member "--dry-run" args))
(define no-verify? (member "--no-verify" args))

(define new-version
  (for/first ([a (in-list args)]
              #:when (regexp-match? #rx"^[0-9]+\\.[0-9]+\\.[0-9]+$" a))
    a))

;; --- Read current version from canonical source ---

(define (read-current-version)
  (define ver-path "util/version.rkt")
  (define content (file->string ver-path))
  (define m (regexp-match #rx"\"([0-9]+\\.[0-9]+\\.[0-9]+)\"" content))
  (if m
      (cadr m)
      (error "Cannot read version from" ver-path)))

;; --- Collect all surfaces that reference the old version ---

(define (collect-surfaces old-ver)
  (define surfaces
    (list (list "util/version.rkt" "canonical version") (list "info.rkt" "package version")))
  ;; docs/**/*.md
  (define docs-surfaces
    (for/list ([f (in-directory "docs")]
               #:when (and (file-exists? f)
                           (string-suffix? (path->string f) ".md")
                           (string-contains? (file->string f) old-ver)))
      (list (path->string (find-relative-path (current-directory) f)) "doc version ref")))
  ;; README.md
  (define readme-surfaces
    (if (and (file-exists? "README.md") (string-contains? (file->string "README.md") old-ver))
        (list (list "README.md" "README version refs"))
        '()))
  (append surfaces docs-surfaces readme-surfaces))

;; --- Apply version replacement to a file ---

(define (update-file! path old-ver)
  (define content (file->string path))
  (define new-content (string-replace content old-ver new-version))
  (call-with-output-file path (λ (out) (display new-content out)) #:exists 'replace))

;; --- Main ---

(define (main)
  (unless new-version
    (printf "Usage: racket scripts/bump-version.rkt X.Y.Z [--dry-run] [--no-verify]~n")
    (exit 1))

  ;; Ensure we're in q/
  (unless (file-exists? "main.rkt")
    (printf "ERROR: Run from the q/ directory~n")
    (exit 1))

  (define old-ver (read-current-version))

  (printf "=== Version Bump: ~a → ~a ===~n~n" old-ver new-version)

  (define surfaces (collect-surfaces old-ver))

  (printf "Found ~a surfaces to update:~n~n" (length surfaces))

  (for ([s (in-list surfaces)])
    (define path (car s))
    (define desc (cadr s))
    (if dry-run?
        (printf "  Would update: ~a (~a)~n" path desc)
        (begin
          (update-file! path old-ver)
          (printf "  Updated: ~a (~a)~n" path desc))))

  (when dry-run?
    (printf "~nDry run — no files modified.~n")
    (exit 0))

  ;; Post-bump: sync metrics and status
  (printf "~n--- Post-bump sync ---~n")
  (system "racket scripts/metrics.rkt --sync-all 2>&1")
  (system "racket scripts/sync-readme-status.rkt --sync 2>&1")

  ;; Verify with ci-local
  (unless no-verify?
    (printf "~n--- Verification ---~n")
    (define exit-code (system/exit-code "racket scripts/ci-local.rkt 2>&1"))
    (if (= exit-code 0)
        (begin
          (printf "~nBump complete. All checks pass. ✓~n")
          (exit 0))
        (begin
          (printf "~nBump applied but verification FAILED. ✗~n")
          (exit 1))))

  (printf "~nBump complete (--no-verify).~n"))

(main)
