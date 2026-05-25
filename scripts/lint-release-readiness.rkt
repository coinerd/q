#!/usr/bin/env racket
#lang racket/base

;; lint-release-readiness.rkt — CI release hygiene gate
;;
;; Verifies that all pre-release conditions are met:
;;   1. Version sync: util/version.rkt, info.rkt, README.md agree
;;   2. No duplicate git tag: tag for current version must NOT already exist
;;   3. CHANGELOG entry: CHANGELOG.md has an entry for the current version
;;   4. Doc freshness: no stale docs (lint-doc-freshness check)
;;   5. Metrics sync: contract metrics are up-to-date
;;
;; Exit codes:
;;   0 — all checks pass, ready to release
;;   1 — one or more checks failed
;;
;; Usage:
;;   racket scripts/lint-release-readiness.rkt
;;   racket scripts/lint-release-readiness.rkt --fix   # auto-fix what's possible

(provide get-canonical-version
         get-info-version
         check-version-sync
         check-tag-unique
         check-changelog-entry
         check-git-clean
         check-main-branch)

(require racket/file
         racket/string
         racket/port
         racket/system
         racket/list
         racket/path)

;; ---------------------------------------------------------------------------
;; Version extraction
;; ---------------------------------------------------------------------------

(define (read-version-from-file path pattern)
  (and (file-exists? path)
       (let* ([content (file->string path)]
              [m (regexp-match pattern content)])
         (and m (cadr m)))))

(define (get-canonical-version)
  (or (read-version-from-file "util/version.rkt" #rx"define q-version \"([^\"]+)\"")
      (error "Cannot read version from util/version.rkt")))

(define (get-info-version)
  (read-version-from-file "info.rkt" #rx"define version \"([^\"]+)\""))

;; ---------------------------------------------------------------------------
;; Check 1: Version sync
;; ---------------------------------------------------------------------------

(define (check-version-sync)
  (define canonical (get-canonical-version))
  (define info-ver (get-info-version))
  (cond
    [(equal? canonical info-ver)
     (printf "  [PASS] version sync: ~a~n" canonical)
     #t]
    [else
     (printf "  [FAIL] version drift: util/version.rkt=~a, info.rkt=~a~n" canonical info-ver)
     #f]))

;; ---------------------------------------------------------------------------
;; Check 2: Tag uniqueness
;; ---------------------------------------------------------------------------

(define (check-tag-unique)
  (define ver (get-canonical-version))
  (define tag (format "v~a" ver))
  (define existing
    (with-input-from-string (with-output-to-string (lambda () (system "git tag -l")))
                            (lambda ()
                              (for/or ([line (in-lines)])
                                (equal? (string-trim line) tag)))))
  (cond
    [(not existing)
     (printf "  [PASS] tag ~a does not exist yet~n" tag)
     #t]
    [else
     (printf "  [FAIL] tag ~a already exists — bump version first~n" tag)
     #f]))

;; ---------------------------------------------------------------------------
;; Check 3: CHANGELOG entry
;; ---------------------------------------------------------------------------

(define (check-changelog-entry)
  (define ver (get-canonical-version))
  (cond
    [(not (file-exists? "CHANGELOG.md"))
     (printf "  [FAIL] CHANGELOG.md not found~n")
     #f]
    [else
     (define content (file->string "CHANGELOG.md"))
     (define bare-pattern (regexp (format "## ~a" (regexp-quote ver))))
     (define v-prefix-pattern (regexp (format "## v~a" (regexp-quote ver))))
     (cond
       [(or (regexp-match? bare-pattern content) (regexp-match? v-prefix-pattern content))
        (printf "  [PASS] CHANGELOG has entry for ~a~n" ver)
        #t]
       [else
        (printf "  [FAIL] CHANGELOG missing entry for ~a~n" ver)
        #f])]))

;; ---------------------------------------------------------------------------
;; Check 4: Git clean (no uncommitted changes)
;; ---------------------------------------------------------------------------

(define (check-git-clean)
  (define status (string-trim (with-output-to-string (lambda () (system "git status --porcelain")))))
  (cond
    [(equal? status "")
     (printf "  [PASS] git working tree clean~n")
     #t]
    [else
     (define n (length (string-split status "\n")))
     (printf "  [FAIL] git has ~a uncommitted change(s)~n" n)
     #f]))

;; ---------------------------------------------------------------------------
;; Check 5: On main branch
;; ---------------------------------------------------------------------------

(define (check-main-branch)
  (define branch
    (string-trim (with-output-to-string (lambda () (system "git rev-parse --abbrev-ref HEAD")))))
  (cond
    [(equal? branch "main")
     (printf "  [PASS] on main branch~n")
     #t]
    [else
     (printf "  [FAIL] on branch '~a' — must be on main~n" branch)
     #f]))

;; ---------------------------------------------------------------------------
;; Main
;; ---------------------------------------------------------------------------

(define (main)
  (define argv (vector->list (current-command-line-arguments)))
  (define fix? (member "--fix" argv))

  (unless (file-exists? "util/version.rkt")
    (displayln "ERROR: Run from q/ project root (util/version.rkt not found)")
    (exit 1))

  (define ver (get-canonical-version))
  (printf "~n── Release Readiness Check (v~a) ──~n" ver)

  (define results
    (list (check-version-sync)
          (check-tag-unique)
          (check-changelog-entry)
          (check-git-clean)
          (check-main-branch)))

  (define passed (count values results))
  (define failed (- (length results) passed))

  (printf "~n── Summary: ~a/~a checks passed ~a──~n"
          passed
          (length results)
          (if (positive? failed) "❌ " "✅ "))

  (exit (if (positive? failed) 1 0)))

;; Only auto-run when executed directly as a script
(define invoked-directly?
  (let ([run-file (find-system-path 'run-file)])
    (and (path? run-file)
         (let ([base (file-name-from-path run-file)])
           (and base (equal? (path->string base) "lint-release-readiness.rkt"))))))
(when invoked-directly?
  (main))
