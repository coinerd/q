#!/usr/bin/env racket
#lang racket/base

;; scripts/check-lint-alignment.rkt — Verify pre-commit.rkt stays aligned with lint-all.rkt.
;;
;; Ensures the fast-lint-checks list in pre-commit.rkt covers all
;; non-continue-on-error checks defined in lint-all.rkt.
;;
;; Exit 0 if aligned, 1 if drift detected.

(require racket/file
         racket/list
         racket/string
         racket/port)

(define (read-checks-from-lint-all)
  "Parse lint-all.rkt and return list of (name continue-on-error?) pairs."
  (define path "scripts/lint-all.rkt")
  (unless (file-exists? path)
    (error "lint-all.rkt not found"))
  (define lines (file->lines path))
  (for/list ([line (in-list lines)]
             #:when (regexp-match? #rx"\"scripts/" line))
    (define name-m (regexp-match #rx"\"([^\"]+)\".*\"scripts/" line))
    (define continue? (regexp-match? #rx"#t[ )]+" line))
    (when name-m
      (list (cadr name-m) continue?))))

(define (read-fast-checks-from-pre-commit)
  "Parse pre-commit.rkt and return list of fast check names."
  (define path "scripts/pre-commit.rkt")
  (unless (file-exists? path)
    (error "pre-commit.rkt not found"))
  (define lines (file->lines path))
  (define in-list? #f)
  (define names '())
  (for ([line (in-list lines)])
    (cond
      [(and (not in-list?) (regexp-match? #rx"string-join.*'[(]" line))
       (set! in-list? #t)
       ;; Also process quoted strings on this same line
       (define found (regexp-match* #rx"\"([^\"]+)\"" line #:match-select cadr))
       (set! names (append names found))]
      [in-list?
       ;; Collect quoted strings inside the '() list only
       (define found (regexp-match* #rx"\"([^\"]+)\"" line #:match-select cadr))
       (set! names (append names found))
       ;; Stop at the closing paren of the quoted list
       (when (regexp-match? #rx"[)]" line)
         (set! in-list? #f))]))
  names)

(define (main)
  (printf "=== Lint Alignment Check ===\n")

  (define lint-all-checks (filter values (read-checks-from-lint-all)))
  (define fast-checks (read-fast-checks-from-pre-commit))

  (printf "lint-all.rkt checks: ~a~n" (length lint-all-checks))
  (printf "pre-commit fast checks: ~a~n" (length fast-checks))

  ;; Required checks = all non-continue-on-error checks
  (define required (filter-map (λ (c) (and (not (cadr c)) (car c))) lint-all-checks))

  (define missing (filter (λ (r) (not (member r fast-checks))) required))
  (define extra (filter (λ (f) (not (member f (map car lint-all-checks)))) fast-checks))

  (when (not (null? missing))
    (printf "~nMISSING from pre-commit.rkt (required non-optional checks):~n")
    (for ([m (in-list missing)])
      (printf "  ✗ ~a~n" m)))

  (when (not (null? extra))
    (printf "~nEXTRA in pre-commit.rkt (not in lint-all.rkt):~n")
    (for ([e (in-list extra)])
      (printf "  ⚠ ~a~n" e)))

  (if (and (null? missing) (null? extra))
      (begin
        (printf "~nLint alignment: PASS ✓~n")
        (exit 0))
      (begin
        (printf "~nLint alignment: FAIL ✗~n")
        (exit 1))))

(main)
