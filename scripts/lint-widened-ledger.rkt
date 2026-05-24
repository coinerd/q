#!/usr/bin/env racket
#lang racket/base

;; lint-widened-ledger.rkt — Check WIDENED_BACK_LEDGER.md for overdue entries
;;
;; Reads the ledger from .planning/WIDENED_BACK_LEDGER.md, parses the table,
;; and flags any "open" entries whose "Revisit By" version is <= current version.
;;
;; Exit codes:
;;   0 — no overdue entries (or no ledger found)
;;   1 — overdue entries found
;;
;; Usage:
;;   racket scripts/lint-widened-ledger.rkt
;;   racket scripts/lint-widened-ledger.rkt --path ../.planning/WIDENED_BACK_LEDGER.md

(require racket/file
         racket/string
         racket/list
         racket/path
         racket/match)

;; ---------------------------------------------------------------------------
;; Version comparison
;; ---------------------------------------------------------------------------

(define (parse-version s)
  (map string->number (string-split s ".")))

(define (version<=? a b)
  (let loop ([as (parse-version a)]
             [bs (parse-version b)])
    (cond
      [(and (null? as) (null? bs)) #t]
      [(null? as) #t] ; shorter version is "earlier"
      [(null? bs) #f]
      [(< (car as) (car bs)) #t]
      [(> (car as) (car bs)) #f]
      [else (loop (cdr as) (cdr bs))])))

;; ---------------------------------------------------------------------------
;; Read current project version
;; ---------------------------------------------------------------------------

(define (get-current-version)
  (define path "util/version.rkt")
  (if (file-exists? path)
      (let* ([content (file->string path)]
             [m (regexp-match #rx"define q-version \"([^\"]+)\"" content)])
        (if m
            (cadr m)
            "0.0.0"))
      "0.0.0"))

;; ---------------------------------------------------------------------------
;; Parse ledger table
;; ---------------------------------------------------------------------------

(define (parse-ledger-table lines)
  ;; Find lines that look like table rows: | ... | ... |
  (define rows
    (filter (lambda (l)
              (and (string-prefix? (string-trim l) "|") (> (length (string-split l "|")) 5)))
            lines))
  ;; Skip header and separator rows
  (define data-rows (filter (lambda (l) (not (regexp-match? #rx"\\|[-\\s]+\\|" l))) rows))
  ;; Skip the header row (first data row after separator)
  (define header-skipped
    (if (pair? data-rows)
        (cdr data-rows)
        '()))
  header-skipped)

(define (parse-row line)
  (define fields (map string-trim (string-split line "|")))
  ;; Remove empty strings from leading/trailing |
  (define clean (filter (lambda (s) (> (string-length s) 0)) fields))
  (if (>= (length clean) 6)
      (list (list-ref clean 0) ; File
            (list-ref clean 1) ; Function
            (list-ref clean 2) ; Original
            (list-ref clean 3) ; Widened To
            (list-ref clean 4) ; Reason
            (list-ref clean 5) ; Revisit By
            (if (>= (length clean) 7)
                (list-ref clean 6)
                "open")) ; Status
      #f))

;; ---------------------------------------------------------------------------
;; Main logic
;; ---------------------------------------------------------------------------

(define (find-ledger-path args)
  (cond
    [(member "--path" args)
     =>
     (lambda (rest)
       (if (pair? (cdr rest))
           (cadr rest)
           #f))]
    [(file-exists? "../.planning/WIDENED_BACK_LEDGER.md") "../.planning/WIDENED_BACK_LEDGER.md"]
    [(file-exists? ".planning/WIDENED_BACK_LEDGER.md") ".planning/WIDENED_BACK_LEDGER.md"]
    [else #f]))

(define (run-lint args)
  (define ledger-path (find-ledger-path args))
  (cond
    [(not ledger-path)
     (displayln "  [SKIP] WIDENED_BACK_LEDGER.md not found")
     0]
    [(not (file-exists? ledger-path))
     (displayln "  [SKIP] WIDENED_BACK_LEDGER.md not found")
     0]
    [else
     (define content (file->string ledger-path))
     (define lines (string-split content "\n"))
     (define rows (parse-ledger-table lines))
     (define current-ver (get-current-version))

     (define parsed-rows (filter (lambda (x) x) (map parse-row rows)))

     (define open-entries (filter (lambda (r) (equal? (list-ref r 6) "open")) parsed-rows))

     (define overdue-entries
       (filter (lambda (r)
                 (define revisit (list-ref r 5))
                 (define revisit-ver (regexp-match #rx"v?(\\d+\\.\\d+\\.\\d+)" revisit))
                 (if revisit-ver
                     (version<=? (cadr revisit-ver) current-ver)
                     #f))
               open-entries))

     (define not-yet-due
       (filter (lambda (r)
                 (define revisit (list-ref r 5))
                 (define revisit-ver (regexp-match #rx"v?(\\d+\\.\\d+\\.\\d+)" revisit))
                 (if revisit-ver
                     (not (version<=? (cadr revisit-ver) current-ver))
                     #f))
               open-entries))

     (printf "  Ledger: ~a open entries, ~a overdue, ~a future~n"
             (length open-entries)
             (length overdue-entries)
             (length not-yet-due))

     (when (pair? overdue-entries)
       (displayln "  [FAIL] Overdue widened-back entries:")
       (for ([e (in-list overdue-entries)])
         (printf "    ~a :: ~a (revisit by ~a, current ~a)~n"
                 (list-ref e 0)
                 (list-ref e 1)
                 (list-ref e 5)
                 current-ver)))

     (when (and (null? overdue-entries) (pair? open-entries))
       (printf "  [PASS] No overdue entries (~a open, all future)~n" (length open-entries)))

     (when (null? open-entries)
       (displayln "  [PASS] No open entries in ledger"))

     (if (pair? overdue-entries) 1 0)]))

;; ---------------------------------------------------------------------------
;; Entry point
;; ---------------------------------------------------------------------------

(provide run-lint
         version<=?
         parse-ledger-table
         parse-row
         get-current-version)

;; Only auto-run when executed directly
(define invoked-directly?
  (let ([run-file (find-system-path 'run-file)])
    (and (path? run-file)
         (let ([base (file-name-from-path run-file)])
           (and base (equal? (path->string base) "lint-widened-ledger.rkt"))))))
(when invoked-directly?
  (define args (vector->list (current-command-line-arguments)))
  (define result (run-lint args))
  (exit result))
