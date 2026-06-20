#lang racket/base

;; scripts/version-surface.rkt — Centralized version surface registry
;;
;; Consolidates the version-parsing logic that was independently duplicated
;; across lint-version.rkt, sync-version.rkt, lint-widened-ledger.rkt, and
;; audit-project.rkt. Each of those scripts had its own regex, error fallback,
;; and path-construction logic for reading the canonical version from
;; util/version.rkt.
;;
;; W2 (#8415): Created during v0.99.36 to establish a single source of truth
;; for version-surface parsing.

(require racket/file
         racket/match
         racket/path
         racket/string)

;; Content-level parsing (no I/O)
(provide parse-q-version-from-content
         parse-info-version-from-content
         parse-version-components
         ;; Comparison
         version<=?
         version<?
         version=?
         ;; File-level reading (with I/O)
         read-canonical-version
         read-canonical-version/strict
         ;; Version formatting
         version->string)

;; ---------------------------------------------------------------------------
;; Constants
;; ---------------------------------------------------------------------------

(define VERSION-FILE-NAME "version.rkt")
(define VERSION-FILE-REL-PATH '("util" "version.rkt"))
(define FALLBACK-VERSION "0.0.0")
(define STRICT-FAIL-SENTINEL #f)

;; ---------------------------------------------------------------------------
;; Content-level parsing (pure functions, no I/O)
;; ---------------------------------------------------------------------------

;; Parse `(define q-version "X.Y.Z")` from content string.
;; Handles both #lang racket and #lang typed/racket (multi-line) formats.
(define (parse-q-version-from-content content)
  (define start (regexp-match-positions #rx"\\(define q-version" content))
  (cond
    [(not start) #f]
    [else
     (define after (substring content (cdar start)))
     (define m (regexp-match #rx"([0-9]+\\.[0-9]+\\.[0-9]+)" after))
     (and m (cadr m))]))

;; Parse `(define version "X.Y.Z")` from info.rkt content string.
(define (parse-info-version-from-content content)
  (define m (regexp-match #rx"\\(define version \"([0-9]+\\.[0-9]+\\.[0-9]+)\"" content))
  (and m (cadr m)))

;; Split "X.Y.Z" into list of numbers: (1 2 3)
(define (parse-version-components s)
  (map string->number (string-split s ".")))

;; ---------------------------------------------------------------------------
;; Comparison (pure)
;; ---------------------------------------------------------------------------

(define (version<=? a b)
  (let loop ([as (parse-version-components a)]
             [bs (parse-version-components b)])
    (cond
      [(and (null? as) (null? bs)) #t]
      [(null? as) #t] ; shorter version is "earlier"
      [(null? bs) #f]
      [(< (car as) (car bs)) #t]
      [(> (car as) (car bs)) #f]
      [else (loop (cdr as) (cdr bs))])))

(define (version<? a b)
  (and (version<=? a b) (not (version=? a b))))

(define (version=? a b)
  (equal? a b))

;; ---------------------------------------------------------------------------
;; File-level reading (with I/O)
;; ---------------------------------------------------------------------------

;; Build the path to util/version.rkt relative to an optional base directory.
;; If base-dir is #f, uses current-directory.
(define (version-file-path base-dir)
  (if base-dir
      (apply build-path base-dir VERSION-FILE-REL-PATH)
      (apply build-path (current-directory) VERSION-FILE-REL-PATH)))

;; Read canonical version from util/version.rkt.
;; Returns the version string, or FALLBACK-VERSION on error.
(define (read-canonical-version [base-dir #f])
  (define path (version-file-path base-dir))
  (if (file-exists? path)
      (let* ([content (file->string path)]
             [v (parse-q-version-from-content content)])
        (or v FALLBACK-VERSION))
      FALLBACK-VERSION))

;; Strict variant: returns #f on any failure (file not found, parse error).
(define (read-canonical-version/strict [base-dir #f])
  (define path (version-file-path base-dir))
  (and (file-exists? path)
       (let ([content (file->string path)]) (parse-q-version-from-content content))))

;; ---------------------------------------------------------------------------
;; Formatting
;; ---------------------------------------------------------------------------

;; Format version components back to string: (1 2 3) -> "1.2.3"
(define (version->string components)
  (string-join (map number->string components) "."))
