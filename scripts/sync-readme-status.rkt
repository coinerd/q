#!/usr/bin/env racket
#lang racket/base

;; scripts/sync-readme-status.rkt — Keep README Status block in sync with version
;;
;; Issue #1284: GAP-04 — Auto-generate README status block from release metadata
;;
;; Usage:
;;   cd q/ && racket scripts/sync-readme-status.rkt --version       # print current version
;;   cd q/ && racket scripts/sync-readme-status.rkt --check [FILE]  # check README status block
;;   cd q/ && racket scripts/sync-readme-status.rkt --sync [FILE]   # update README status block
;;
;; Default FILE is README.md. Exit 0 on success, 1 on mismatch/failure.

(require racket/file
         racket/string
         racket/port
         racket/match)

;; ---------------------------------------------------------------------------
;; Version extraction
;; ---------------------------------------------------------------------------

(define (read-version)
  (define content (file->string "util/version.rkt"))
  (define m (regexp-match #rx"\\(define q-version \"([0-9]+\\.[0-9]+\\.[0-9]+)\"" content))
  (if m
      (cadr m)
      (error "Could not parse version from util/version.rkt")))

;; ---------------------------------------------------------------------------
;; Status block operations
;; ---------------------------------------------------------------------------

;; Find the first version bold marker in the Status section.
;; The Status section starts with "## Status" and ends at the next "##" heading.
(define status-heading-rx #rx"^## Status")
(define next-heading-rx #rx"^## ")
(define status-version-rx #rx"\\*\\*v([0-9]+\\.[0-9]+\\.[0-9]+)\\*\\*")

(define (find-status-version lines)
  ;; Returns (values version-in-status line-index) or (values #f #f)
  (define in-section? #f)
  (for/or ([line (in-list lines)]
           [idx (in-naturals)])
    (cond
      [(regexp-match? status-heading-rx line)
       (set! in-section? #t)
       #f]
      [(and in-section? (regexp-match? next-heading-rx line)) #f] ;; past section, stop
      [(and in-section? (regexp-match status-version-rx line))
       =>
       (λ (m) (cons (cadr m) idx))]
      [else #f])))

(define (replace-status-version lines new-version)
  ;; Replace the first **vX.Y.Z** in the Status section with the new version
  (define in-section? #f)
  (define replaced? #f)
  (for/list ([line (in-list lines)])
    (cond
      [(regexp-match? status-heading-rx line)
       (set! in-section? #t)
       line]
      [(and in-section? (not replaced?) (regexp-match? next-heading-rx line))
       (set! in-section? #f)
       line]
      [(and in-section? (not replaced?) (regexp-match status-version-rx line))
       (set! replaced? #t)
       (regexp-replace status-version-rx line (format "**v~a**" new-version))]
      [else line])))

;; ---------------------------------------------------------------------------
;; CLI
;; ---------------------------------------------------------------------------

(define args (vector->list (current-command-line-arguments)))

(define (readme-path)
  ;; If an extra arg after the flag is provided, use it; otherwise README.md
  (define file-arg
    (for/first ([a (in-list args)]
                #:when (not (string-prefix? a "--")))
      a))
  (or file-arg "README.md"))

(define (main)
  (match args
    [(list "--version") (displayln (read-version))]

    [(or (list "--check") (list "--check" _))
     (define version (read-version))
     (define path (readme-path))
     (unless (file-exists? path)
       (printf "ERROR: ~a not found~n" path)
       (exit 1))
     (define lines (file->lines path))
     (define result (find-status-version lines))
     (cond
       [(not result)
        (printf "MISSING: No version found in Status section of ~a~n" path)
        (exit 1)]
       [else
        (define status-ver (car result))
        (if (equal? status-ver version)
            (begin
              (printf "OK: README Status block version (~a) matches q-version~n" version)
              (exit 0))
            (begin
              (printf "MISMATCH: README Status says v~a, q-version is v~a~n" status-ver version)
              (exit 1)))])]

    [(or (list "--sync") (list "--sync" _))
     (define version (read-version))
     (define path (readme-path))
     (unless (file-exists? path)
       (printf "ERROR: ~a not found~n" path)
       (exit 1))
     (define lines (file->lines path))
     (define updated (replace-status-version lines version))
     (call-with-output-file path
                            (λ (out)
                              (for ([line (in-list updated)])
                                (displayln line out)))
                            #:exists 'replace)
     (printf "Synced README Status block to v~a~n" version)]

    [_
     (displayln "Usage: racket scripts/sync-readme-status.rkt [--version|--check|--sync] [FILE]")
     (exit 1)]))

(main)
