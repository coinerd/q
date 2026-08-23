#lang racket

;; @speed fast
;; @suite extensions
;; @boundary unit

;; tests/test-plan-wave-header-tolerance.rkt — v1.00.14 hotfix regression
;;
;; BUG (session MN5MQHQP, 2026-08-23): a plan written with em-dash headers
;; (`## Wave 1 — Title`) and 1-based numbering failed /go normalization with
;; "Wave indices not sequential 0..3: (0 0 0 0)" — the parser only matched
;; the colon separator and fell back to index 0 for every wave.

(require rackunit
         (only-in "../extensions/gsd/plan-types-parser.rkt" parse-waves-from-markdown-raw)
         "../extensions/gsd/plan-types.rkt")

(define CLASSIC
  #<<PLAN
## Wave 0: Alpha
- File: a.rkt
- Verify: rackcat a.rkt

## Wave 1: Beta
- File: b.rkt
- Verify: rackcat b.rkt
PLAN
  )

(define EMDASH-1BASED
  #<<PLAN
# Plan

## Wave 1 — Release pre-flight (local-only) — DONE 2026-08-23
- File: a.rkt
- Verify: racket a.rkt

## Wave 2 — Observation tools (local-only)
- File: b.rkt
- Verify: racket b.rkt

## Wave 3 — Wave-finish + release helpers (mixed) — DONE 2026-08-23
- File: c.rkt
- Verify: racket c.rkt

## Wave 4 — CI-repro skill, hook fix (mixed) — DONE 2026-08-23
- File: d.rkt
- Verify: racket d.rkt
PLAN
  )

(test-case "classic colon headers still parse with 0-based indices"
  (define waves (parse-waves-from-markdown CLASSIC))
  (check-equal? (length waves) 2)
  (check-equal? (map gsd-wave-index waves) '(0 1))
  (check-equal? (gsd-wave-title (first waves)) "Alpha"))

(test-case "em-dash 1-based headers parse and normalize to 0-based"
  (define waves (parse-waves-from-markdown EMDASH-1BASED))
  (check-equal? (length waves) 4)
  ;; THE regression: previously every index was 0 → "(0 0 0 0)"
  (check-equal? (map gsd-wave-index waves) '(0 1 2 3))
  (check-false (string=? (gsd-wave-title (first waves)) "") "title must not be empty on dash headers")
  (check-true (string-contains? (gsd-wave-title (first waves)) "Release pre-flight")))

(test-case "hyphen separator headers parse"
  (define md
    #<<PLAN
## Wave 1 - First
- Verify: true

## Wave 2 - Second
- Verify: true
PLAN
    )
  (define waves (parse-waves-from-markdown md))
  (check-equal? (map gsd-wave-index waves) '(0 1)))

(test-case "non-sequential indices are NOT silently shifted"
  (define md
    #<<PLAN
## Wave 2 — A
- Verify: true

## Wave 5 — B
- Verify: true
PLAN
    )
  ;; 2,5 is neither 0..n-1 nor 1..n — left untouched so normalize-plan rejects.
  (define waves (parse-waves-from-markdown md))
  (check-equal? (map gsd-wave-index waves) '(2 5)))

(test-case "normalize-plan accepts the user's failing plan shape"
  (define plan (gsd-plan (parse-waves-from-markdown EMDASH-1BASED) #f '() '()))
  (define result (normalize-plan plan))
  (check-pred gsd-normalized-plan? result)
  (unless (gsd-normalized-plan? result)
    (fail (format "normalization failed: ~a" result))))
