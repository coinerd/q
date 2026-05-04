#lang racket/base

;; test-event-struct-coverage.rkt — Counts emission sites vs defined structs
;; v0.29.2 W0: Test scaffolding (coverage check function does not exist yet — tests should FAIL)

(require rackunit
         racket/base
         (only-in "../agent/event-struct-coverage.rkt"
                  count-typed-emission-sites
                  count-raw-hasheq-emission-sites
                  event-struct-coverage-report))

;; ── Coverage report ──

(test-case "event-struct-coverage-report: returns a hash with site counts"
  (define report (event-struct-coverage-report))
  (check-true (hash? report))
  (check-true (>= (hash-ref report 'typed-sites 0) 0))
  (check-true (>= (hash-ref report 'raw-hasheq-sites 0) 0)))

(test-case "count-typed-emission-sites: returns non-negative integer"
  (define count (count-typed-emission-sites))
  (check-true (exact-nonnegative-integer? count)))

(test-case "count-raw-hasheq-emission-sites: initially > 0 (pre-migration)"
  (define count (count-raw-hasheq-emission-sites))
  ;; Before migration, most sites still use raw hasheq
  (check-true (> count 0)
              (format "Expected raw hasheq sites > 0, got ~a" count)))
