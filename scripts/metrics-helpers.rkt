#lang racket/base

;; scripts/metrics-helpers.rkt — Pure functions for metrics sync/lint
;;
;; W5 (#8479): Non-mutating release/report/check boundary pilot.
;;
;; Extracts the pure computation from scripts/metrics.rkt so that:
;;   1. Unit tests can test computation without subprocess/file I/O.
;;   2. Mutation is confined to a thin I/O shell in metrics.rkt.
;;   3. A --check-only / dry-run mode can compute "what would change"
;;      without writing to disk.
;;
;; Pattern: same as sync-version.rkt (pure string→string + thin I/O shell).
;;
;; All functions are pure: no file I/O, no displayln, no exit.
;; DESIGN FACT (W5, v0.99.38): This module establishes the pure-helpers +
;; thin-I/O-shell pattern. Other mutating scripts (sync-readme-status.rkt,
;; sync-version.rkt) should follow the same extraction when refactored.
;; The --check-only flag in metrics.rkt depends on this separation.
;; W8 scanner `mutation-site-rx` flags modules that write files outside
;; this pattern.

(require racket/contract
         racket/string
         racket/list
         racket/dict)

(provide (contract-out
          ;; Sync (string → string)
          [compute-marker-sync (-> string? (hash/c string? string?) string?)]
          [compute-table-sync (-> string? (listof (cons/c string? string?)) string?)]
          [compute-prose-sync (-> string? string? string? string?)]
          [compute-all-sync
           (-> string? (hash/c string? string?) (listof (cons/c string? string?)) string?)]
          ;; Lint (string → listof error strings)
          [lint-table-values (-> string? (listof (cons/c string? string?)) (listof string?))]
          [lint-prose-values (-> string? string? string? (listof string?))]
          ;; Section extraction (pure helper)
          [extract-table-section (-> string? (listof string?))]))

;; ============================================================
;; §1: Marker sync — replace <!-- METRICS: key --> with values
;; ============================================================

(define (compute-marker-sync content metrics)
  (define marker-rx #rx"<!-- METRICS: ([a-z-]+) -->")
  (regexp-replace*
   marker-rx
   content
   (lambda (match key)
     (hash-ref metrics key (lambda () (format "<!-- METRICS: ~a (unknown) -->" key))))))

;; ============================================================
;; §2: Table sync — replace | Name | NNN | rows with computed values
;; ============================================================

(define (compute-table-sync content computed-metrics)
  (for/fold ([c content]) ([(name value) (in-dict computed-metrics)])
    (regexp-replace* (regexp (format "\\| ~a \\| [0-9,]+ \\|" (regexp-quote name)))
                     c
                     (format "| ~a | ~a |" name value))))

;; ============================================================
;; §3: Prose sync — replace prose count patterns
;; ============================================================

(define (compute-prose-sync content test-file-count src-mod-count)
  (define updated
    (regexp-replace* #rx"Full test suite \\([0-9]+ files\\)"
                     content
                     (format "Full test suite (~a files)" test-file-count)))
  (regexp-replace* #rx"[0-9]+ source modules" updated (format "~a source modules" src-mod-count)))

;; ============================================================
;; §4: All-in-one sync
;; ============================================================

(define (compute-all-sync content metrics-map computed-metrics)
  (define s1 (compute-marker-sync content metrics-map))
  (define s2 (compute-table-sync s1 computed-metrics))
  (define test-count (hash-ref metrics-map "test-files" "0"))
  (define src-count (hash-ref metrics-map "source-modules" "0"))
  (compute-prose-sync s2 test-count src-count))

;; ============================================================
;; §5: Table section extraction — extract lines from "## Test Suite" section
;; ============================================================

(define (extract-table-section content)
  (define lines (string-split content "\n"))
  (define in-section? (box #f))
  (filter values
          (for/list ([line lines])
            (cond
              [(regexp-match? #rx"^## Test Suite" line)
               (set-box! in-section? #t)
               #f]
              [(and (unbox in-section?) (regexp-match? #rx"^##" line))
               (set-box! in-section? #f)
               #f]
              [(unbox in-section?) line]
              [else #f]))))

;; ============================================================
;; §6: Table lint — compare README table against computed metrics
;; ============================================================

(define (lint-table-values content computed-metrics)
  (define section-lines (extract-table-section content))
  (define readme-values (make-hash))
  (for ([line section-lines])
    (define m (regexp-match #rx"^\\| *([^|]+?) *\\| *([^|]+?) *\\|$" line))
    (when m
      (define name (string-trim (cadr m)))
      (define value (string-trim (caddr m)))
      (when (regexp-match? #rx"^[0-9,.]+$" value)
        (hash-set! readme-values name (string-replace value "," "")))))
  (for/fold ([errs '()]) ([(name value) (in-dict computed-metrics)])
    (define readme-val (hash-ref readme-values name #f))
    (cond
      [(not readme-val) (cons (format "ERROR: ~a: metric not found in README table" name) errs)]
      [(not (equal? value readme-val))
       (cons (format "ERROR: ~a: expected ~a, found ~a" name value readme-val) errs)]
      [else errs])))

;; ============================================================
;; §7: Prose lint — check prose counts match computed metrics
;; ============================================================

(define (lint-prose-values content test-file-count src-mod-count)
  (define prose-rxs
    `(("test-files" . #rx"Full test suite \\(([0-9]+) files\\)")
      ("test-files" . #rx"tests? suite:? \\(([0-9]+) files\\)")
      ("source-modules" . #rx"([0-9]+) source modules")))
  (define errors
    (for/fold ([errs '()]) ([pr (in-list prose-rxs)])
      (define key (car pr))
      (define rx (cdr pr))
      (define m (regexp-match rx content))
      (cond
        [(not m) errs]
        [else
         (define prose-val (cadr m))
         (define computed
           (cond
             [(equal? key "test-files") test-file-count]
             [(equal? key "source-modules") src-mod-count]
             [else "unknown"]))
         (if (equal? prose-val computed)
             errs
             (cons (format "MISMATCH: prose says ~a but computed ~a is ~a" prose-val key computed)
                   errs))])))
  (reverse errors))
