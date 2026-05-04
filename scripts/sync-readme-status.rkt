#!/usr/bin/env racket
#lang racket/base

;; scripts/sync-readme-status.rkt — Keep README Status block in sync with CHANGELOG
;;
;; Usage:
;;   cd q/ && racket scripts/sync-readme-status.rkt --version       # print current version
;;   cd q/ && racket scripts/sync-readme-status.rkt --check [FILE]  # check README status block
;;   cd q/ && racket scripts/sync-readme-status.rkt --sync [FILE]   # update README status block
;;
;; --sync now generates the top Status entry from the CHANGELOG top entry,
;; ensuring the description matches what was actually released.
;;
;; Default FILE is README.md. Exit 0 on success, 1 on mismatch/failure.

(require racket/file
         racket/string
         racket/list
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
;; CHANGELOG parsing
;; ---------------------------------------------------------------------------

(define changelog-header-rx #rx"^## v([0-9]+\\.[0-9]+\\.[0-9]+) — .+")
(define sub-header-rx #rx"^### ")

(define (parse-changelog-top-entry [path "CHANGELOG.md"])
  (unless (file-exists? path)
    (values #f #f))
  (define lines (file->lines path))
  (define header-line
    (for/first ([line (in-list lines)]
                #:when (regexp-match? changelog-header-rx line))
      line))
  (define version
    (let ([m (and header-line (regexp-match changelog-header-rx header-line))]) (and m (cadr m))))
  (define summary-lines
    (let loop ([ls lines]
               [found-header? #f]
               [acc '()])
      (cond
        [(null? ls) (reverse acc)]
        [(and (not found-header?) (regexp-match? changelog-header-rx (car ls)))
         (loop (cdr ls) #t acc)]
        [(and found-header? (regexp-match? #rx"^## " (car ls))) (reverse acc)]
        [(and found-header? (regexp-match? sub-header-rx (car ls))) (loop (cdr ls) #t acc)]
        [(and found-header? (> (string-length (string-trim (car ls))) 0))
         (loop (cdr ls) #t (cons (string-trim (car ls)) acc))]
        [else (loop (cdr ls) found-header? acc)])))
  (define summary
    (cond
      [(null? summary-lines) #f]
      [else (string-join summary-lines " ")]))
  (values version summary))

;; ---------------------------------------------------------------------------
;; Status block operations
;; ---------------------------------------------------------------------------

(define status-heading-rx #rx"^## Status")
(define next-heading-rx #rx"^## ")
(define status-version-rx #rx"\\*\\*v([0-9]+\\.[0-9]+\\.[0-9]+)\\*\\*")

(define (find-status-section-range lines)
  (define start
    (for/first ([line (in-list lines)]
                [idx (in-naturals)]
                #:when (regexp-match? status-heading-rx line))
      idx))
  (define end
    (if start
        (for/first ([line (in-list (drop lines (add1 start)))]
                    [idx (in-naturals (add1 start))]
                    #:when (regexp-match? next-heading-rx line))
          idx)
        #f))
  (values start (or end (and start (length lines)))))

(define (find-status-version lines)
  (define in-section? #f)
  (for/or ([line (in-list lines)]
           [idx (in-naturals)])
    (cond
      [(regexp-match? status-heading-rx line)
       (set! in-section? #t)
       #f]
      [(and in-section? (regexp-match? next-heading-rx line)) #f]
      [(and in-section? (regexp-match status-version-rx line))
       =>
       (λ (m) (cons (cadr m) idx))]
      [else #f])))

(define (find-status-entry-line lines)
  (define in-section? #f)
  (for/or ([line (in-list lines)])
    (cond
      [(regexp-match? status-heading-rx line)
       (set! in-section? #t)
       #f]
      [(and in-section? (regexp-match? next-heading-rx line)) #f]
      [(and in-section? (regexp-match status-version-rx line)) line]
      [else #f])))

(define (insert-status-entry lines version summary)
  ;; Insert a new Status entry at top of section.
  (define-values (start-idx _) (find-status-section-range lines))
  (unless start-idx
    (error 'insert-status-entry "No Status section found"))
  ;; Find the first non-blank line after ## Status
  (define insert-idx
    (for/first ([i (in-range (add1 start-idx) (length lines))]
                #:when (> (string-length (string-trim (list-ref lines i))) 0))
      i))
  (define actual-idx (or insert-idx (add1 start-idx)))
  (define new-entry (format "**v~a** — ~a" version summary))
  (append (take lines actual-idx) (list "" new-entry "") (drop lines actual-idx)))

(define (replace-status-entry lines version summary)
  ;; Replace the first Status entry with new version + description.
  (define in-section? #f)
  (define replaced? #f)
  (for/list ([line (in-list lines)])
    (cond
      [(and (not in-section?) (regexp-match? status-heading-rx line))
       (set! in-section? #t)
       line]
      [(and in-section? (not replaced?) (regexp-match? next-heading-rx line))
       (set! in-section? #f)
       line]
      [(and in-section? (not replaced?) (regexp-match status-version-rx line))
       (set! replaced? #t)
       (format "**v~a** — ~a" version summary)]
      [else line])))

;; ---------------------------------------------------------------------------
;; CLI
;; ---------------------------------------------------------------------------

(define args (vector->list (current-command-line-arguments)))

(define (readme-path)
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
        (cond
          [(not (equal? status-ver version))
           (printf "MISMATCH: README Status says v~a, q-version is v~a~n" status-ver version)
           (exit 1)]
          [else
           ;; Also check description matches CHANGELOG
           (define-values (cl-version cl-summary) (parse-changelog-top-entry))
           (define status-line (find-status-entry-line lines))
           (cond
             [(and cl-version cl-summary status-line)
              (define expected-entry (format "**v~a** — ~a" cl-version cl-summary))
              (define cmp-len (min 60 (string-length status-line) (string-length expected-entry)))
              (define status-prefix (substring status-line 0 cmp-len))
              (define expected-prefix (substring expected-entry 0 cmp-len))
              (if (string=? status-prefix expected-prefix)
                  (begin
                    (printf "OK: README Status block version (~a) and description match CHANGELOG~n"
                            version)
                    (exit 0))
                  (begin
                    (printf "MISMATCH: Status description differs from CHANGELOG~n")
                    (printf "  Status: ~a~n"
                            (substring status-line 0 (min 80 (string-length status-line))))
                    (printf "  Expected: ~a~n"
                            (substring expected-entry 0 (min 80 (string-length expected-entry))))
                    (exit 1)))]
             [else
              (printf "OK: README Status block version (~a) matches q-version~n" version)
              (exit 0)])])])]

    [(or (list "--sync") (list "--sync" _))
     (define version (read-version))
     (define path (readme-path))
     (unless (file-exists? path)
       (printf "ERROR: ~a not found~n" path)
       (exit 1))
     (define-values (cl-version cl-summary) (parse-changelog-top-entry))
     (define lines (file->lines path))
     (define result (find-status-version lines))
     (define summary
       (or cl-summary
           (begin
             (printf "WARNING: Could not parse CHANGELOG top entry; version-only sync~n")
             (format "v~a release" version))))
     (define updated
       (cond
         ;; If Status top version already matches, just update description
         [(and result (equal? (car result) version)) (replace-status-entry lines version summary)]
         ;; If Status top version differs, insert new entry
         [result (insert-status-entry lines version summary)]
         ;; No Status section found
         [else
          (printf "ERROR: No Status section found in ~a~n" path)
          (exit 1)]))
     (call-with-output-file path
                            (λ (out)
                              (for ([line (in-list updated)])
                                (displayln line out)))
                            #:exists 'replace)
     (printf "Synced README Status block to v~a (from CHANGELOG)~n" version)]

    [_
     (displayln "Usage: racket scripts/sync-readme-status.rkt [--version|--check|--sync] [FILE]")
     (exit 1)]))

(main)
