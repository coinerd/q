#!/usr/bin/env racket
#lang racket/base

;; scripts/lint-changelog-dates.rkt — Validate CHANGELOG.md date entries
;;
;; Checks:
;;   1. No version entry has a future date (beyond today) → ERROR
;;   2. Dates are in reverse chronological order → WARNING
;;   3. Each version entry has a valid date in the expected format → ERROR
;;
;; Exit 0 if clean (warnings are informational only), 1 if any ERROR found.

(require racket/date
         racket/file
         racket/list
         racket/string
         racket/port)

(define CHANGELOG-PATH "CHANGELOG.md")

;; ── Parsing ──

;; Match: ## vX.Y.Z — YYYY-MM-DD
(define version-line-rx #px"^## v[0-9]+[.][0-9]+[.][0-9]+ — ([0-9]{4}-[0-9]{2}-[0-9]{2})")
(define date-only-rx #px"^## v[0-9]+[.][0-9]+[.][0-9]+")

(define (parse-version-line line)
  ;; Returns (cons version-str date-str) or #f
  (define m (regexp-match version-line-rx line))
  (and m
       (let ([full-match (car m)])
         ;; Extract version by removing "## " prefix and date suffix
         (define version-full (substring full-match 3))
         (define dash-pos (regexp-match-positions #px" — " version-full))
         (define version-str
           (if dash-pos
               (substring version-full 0 (caar dash-pos))
               version-full))
         (cons version-str (cadr m)))))

(define (parse-date-entries changelog-text)
  ;; Returns list of (version-str . date-str) in file order
  (for/fold ([entries '()]) ([line (in-list (string-split changelog-text "\n"))])
    (define entry (parse-version-line line))
    (if entry
        (append entries (list entry))
        entries)))

;; ── Date helpers ──

(define (date->seconds date-str)
  ;; Parse YYYY-MM-DD to seconds since epoch (at midnight UTC)
  (define parts (string-split date-str "-"))
  (if (= (length parts) 3)
      (let ([yr (string->number (list-ref parts 0))]
            [mo (string->number (list-ref parts 1))]
            [dy (string->number (list-ref parts 2))])
        (if (and yr mo dy)
            (find-seconds 0 0 0 dy mo yr #f)
            #f))
      #f))

(define (today-str)
  (define secs (current-seconds))
  (define d (seconds->date secs #f))
  (format "~a-~a-~a" (date-year d) (pad2 (date-month d)) (pad2 (date-day d))))

(define (pad2 n)
  (if (< n 10)
      (format "0~a" n)
      (format "~a" n)))

;; ── Checks ──

(define (check-future-dates entries)
  ;; Returns list of error strings for entries with future dates
  (define today-secs (date->seconds (today-str)))
  (for/list ([e (in-list entries)]
             #:when (let ([entry-secs (date->seconds (cdr e))])
                      (and entry-secs (> entry-secs today-secs))))
    (format "ERROR: ~a has future date ~a (today: ~a)" (car e) (cdr e) (today-str))))

(define (check-chronological entries)
  ;; Returns list of warning strings if dates are not in reverse chronological order
  (define date-secs (map (lambda (e) (date->seconds (cdr e))) entries))
  (for/list ([i (in-range (sub1 (length date-secs)))]
             #:when (and (list-ref date-secs i)
                         (list-ref date-secs (add1 i))
                         (< (list-ref date-secs i) (list-ref date-secs (add1 i)))))
    (format "WARNING: ~a (~a) is older than ~a (~a) — not in reverse chronological order"
            (car (list-ref entries i))
            (cdr (list-ref entries i))
            (car (list-ref entries (add1 i)))
            (cdr (list-ref entries (add1 i))))))

(define (check-missing-dates changelog-lines)
  ;; Check for version headers that are missing a date
  (for/list ([line (in-list changelog-lines)]
             #:when (and (regexp-match? date-only-rx line)
                         (not (regexp-match? version-line-rx line))))
    (format "ERROR: version entry missing date: ~a" (string-trim line))))

;; ── Main ──

(define (main)
  (define argv (vector->list (current-command-line-arguments)))
  ;; Allow --changelog <path> to override default
  (define changelog-path
    (let loop ([args argv])
      (cond
        [(null? args) CHANGELOG-PATH]
        [(and (equal? (car args) "--changelog") (pair? (cdr args))) (cadr args)]
        [else (loop (cdr args))])))
  (unless (file-exists? changelog-path)
    (displayln "SKIP: CHANGELOG.md not found")
    (exit 0))

  (define changelog-text (file->string changelog-path))
  (define changelog-lines (string-split changelog-text "\n"))
  (define entries (parse-date-entries changelog-text))

  ;; Check missing dates first (even if no dated entries exist)
  (define missing-date-errors (check-missing-dates changelog-lines))

  (when (and (null? entries) (null? missing-date-errors))
    (displayln "ERROR: no version entries found in CHANGELOG.md")
    (exit 1))

  (define errors (append missing-date-errors (check-future-dates entries)))
  (define warnings (check-chronological entries))

  ;; Print warnings first (informational)
  (for ([w (in-list warnings)])
    (displayln w))
  ;; Print errors (blockers)
  (for ([e (in-list errors)])
    (displayln e))

  (cond
    [(null? errors)
     (printf "CHANGELOG dates OK (~a entries, ~a warnings)~n" (length entries) (length warnings))
     (exit 0)]
    [else (exit 1)]))

(main)
