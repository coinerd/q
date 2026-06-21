#lang racket/base

;; scripts/metrics-lint-result.rkt — Result types for metrics linting
;;
;; W4 (#8444): Expected-failure/result-boundary pilot.
;;
;; This module demonstrates the result-type pattern for metrics drift
;; detection. Instead of returning an exit code and printing errors via
;; printf, the check function returns structured results that callers can
;; dispatch on programmatically.
;;
;; Design follows the pattern established in status-result.rkt (v0.99.36 W5).

(require racket/match
         racket/string)

;; ---------------------------------------------------------------------------
;; Result types
;; ---------------------------------------------------------------------------

(provide (struct-out lint-ok)
         (struct-out lint-mismatch)
         (struct-out lint-metric-not-found)
         (struct-out lint-file-error)
         lint-result?
         lint-result-kind
         lint-ok?
         format-lint-result
         lint-results-exit-code
         check-metrics)

;; A single metric matches its expected value.
;; Fields: metric-name (string)
(struct lint-ok (metric-name) #:transparent)

;; A metric value differs from the expected value.
;; Fields: metric-name (string), expected (string), found (string)
(struct lint-mismatch (metric-name expected found) #:transparent)

;; A metric is present in computed set but missing from README.
;; Fields: metric-name (string)
(struct lint-metric-not-found (metric-name) #:transparent)

;; File could not be read or parsed.
;; Fields: path (string), message (string)
(struct lint-file-error (path message) #:transparent)

;; Predicate: is this any lint result variant?
(define (lint-result? v)
  (or (lint-ok? v) (lint-mismatch? v) (lint-metric-not-found? v) (lint-file-error? v)))

;; Tag for dispatch
(define (lint-result-kind r)
  (cond
    [(lint-ok? r) 'ok]
    [(lint-mismatch? r) 'mismatch]
    [(lint-metric-not-found? r) 'not-found]
    [(lint-file-error? r) 'file-error]
    [else 'unknown]))

;; True when all results are lint-ok
(define (lint-ok?/all results)
  (andmap lint-ok? results))

;; Human-readable formatting
(define (format-lint-result r)
  (match r
    [(lint-ok name) (format "OK: ~a" name)]
    [(lint-mismatch name expected found)
     (format "MISMATCH: ~a: expected ~a, found ~a" name expected found)]
    [(lint-metric-not-found name) (format "NOT FOUND: ~a not in README table" name)]
    [(lint-file-error path msg) (format "FILE ERROR: ~a: ~a" path msg)]
    [_ (format "UNKNOWN: ~a" r)]))

;; Compute exit code from list of results: 0 if all ok, 1 if any failure
(define (lint-results-exit-code results)
  (if (andmap lint-ok? results) 0 1))

;; ---------------------------------------------------------------------------
;; Pure check function
;; ---------------------------------------------------------------------------

;; check-metrics : (hash/c string? string?) (listof (cons string? string?)) -> (listof lint-result)
;;
;; Compares computed metrics against README values.
;; Returns one result per computed metric.
;; Pure: no I/O, no side effects.
(define (check-metrics readme-values computed-metrics)
  (for/list ([pair (in-list computed-metrics)])
    (define name (car pair))
    (define expected (cdr pair))
    (define found (hash-ref readme-values name #f))
    (cond
      [(not found) (lint-metric-not-found name)]
      [(not (equal? expected found)) (lint-mismatch name expected found)]
      [else (lint-ok name)])))
