#lang racket/base

;; @speed fast
;; @suite runtime
;; @boundary unit
;; @covers scripts/run-tests/work-mass-comparison.rkt

;; tests/test-work-mass-comparison.rkt — v1.00.28 W7: contract tests for the
;; W0-vs-W7 work-mass comparison tool.
;;
;; PLAN-v1.00.28 §W7 / Measurement Contract §3 contracts exercised here:
;; §W7.2 — inventory equality: a file present in the W0 census but silently
;;         absent from the W7 census is a red error; new files get explicit
;;         rows (never silent additions);
;; §W7.2 — percentage math is computed from the stored per-record medians and
;;         cross-checked against the stored aggregates (mismatch is red);
;; §W7.4 — every required comparison row family is present: fast work mass,
;;         top 10/25/50/100 contribution, bucket counts, total process
;;         launches, Git command launches, fixture construction counts,
;;         total requested real sleep, grouped-safe share;
;; §W7.4 — unknown counters stay null (never coerced to 0);
;; §W7.5 — verdict guide: <10% insufficient; 10–25% partial; 25–40%
;;         meaningful; ≥40% strong.

(require rackunit
         racket/list
         racket/string
         (only-in "../scripts/run-tests/work-mass-comparison.rkt"
                  exn:fail:work-mass-comparison?
                  work-mass-compare
                  work-mass-verdict))

;; ---------------------------------------------------------------- fixtures

;; Minimal census-shaped fixture: a hash with `aggregates` and `records`.
;; Record shape mirrors the runtime census: path + status ("pass" or
;; "collection-failure") + median_ms (pass) + reason (failure).
(define (make-record path median-ms)
  (hasheq 'path path 'status "pass" 'median_ms median-ms))

(define (make-failure path reason)
  (hasheq 'path path 'status "collection-failure" 'reason reason))

(define (make-census records
                     #:work-mass [work-mass #f]
                     #:buckets [buckets '()]
                     #:work-type-totals [totals (hasheq)]
                     #:q7 [q7 (hasheq 'file_count 0 'mass_ms 0)])
  (define passing (filter (lambda (r) (equal? (hash-ref r 'status) "pass")) records))
  (define computed-mass (for/sum ([r (in-list passing)]) (hash-ref r 'median_ms)))
  (hasheq 'records
          records
          'collection_failures
          (filter (lambda (r) (equal? (hash-ref r 'status) "collection-failure")) records)
          'aggregates
          (hasheq 'work_mass_ms
                  (or work-mass computed-mass)
                  'measured_file_count
                  (length passing)
                  'collection_failure_count
                  (length (filter (lambda (r) (equal? (hash-ref r 'status) "collection-failure"))
                                  records))
                  'buckets
                  buckets
                  'work_type_totals
                  totals
                  'queues
                  (hasheq 'q7 q7))))

(define baseline-census
  (make-census (list (make-record "tests/a.rkt" 1000)
                     (make-record "tests/b.rkt" 500)
                     (make-record "tests/c.rkt" 200)
                     (make-record "tests/d.rkt" 300))
               #:buckets (list (hasheq 'label "<250 ms" 'file_count 2 'mass_ms 500)
                               (hasheq 'label "250 ms-1 s" 'file_count 2 'mass_ms 1500))
               #:work-type-totals (hasheq 'subprocesses
                                          10
                                          'racket_subprocesses
                                          2
                                          'git_commands
                                          3
                                          'git_fixtures
                                          1
                                          'session_fixtures
                                          4
                                          'sleep_requested_ms
                                          700)
               #:q7 (list "tests/a.rkt" "tests/c.rkt")))

(define post-census
  (make-census (list (make-record "tests/a.rkt" 800) ; -200  (-20%)
                     (make-record "tests/b.rkt" 450) ; -50   (-10%)
                     (make-record "tests/c.rkt" 200) ; unchanged
                     (make-record "tests/d.rkt" 300) ; unchanged
                     (make-record "tests/new-e.rkt" 100)) ; new file: explicit row
               #:buckets (list (hasheq 'label "<250 ms" 'file_count 3 'mass_ms 600)
                               (hasheq 'label "250 ms-1 s" 'file_count 2 'mass_ms 1250))
               #:work-type-totals (hasheq 'subprocesses
                                          8
                                          'racket_subprocesses
                                          1
                                          'git_commands
                                          1
                                          'git_fixtures
                                          0
                                          'session_fixtures
                                          2
                                          'sleep_requested_ms
                                          250)
               #:q7 (list "tests/a.rkt" "tests/c.rkt" "tests/new-e.rkt")))

;; ------------------------------------------------------------ §W7.5 verdict

(check-equal? (work-mass-verdict 4.76) "insufficient")
(check-equal? (work-mass-verdict 9.99) "insufficient")
(check-equal? (work-mass-verdict 10.0) "partial")
(check-equal? (work-mass-verdict 25.0) "meaningful")
(check-equal? (work-mass-verdict 39.9) "meaningful")
(check-equal? (work-mass-verdict 40.0) "strong")
;; regression (work mass grew) is also not an improvement: insufficient
(check-equal? (work-mass-verdict -3.2) "insufficient")

;; ------------------------------------------------- §W7.2 inventory equality

;; A W0 file silently absent from W7 is a red error (custom exn).
(define (missing-inventory-compare)
  (work-mass-compare baseline-census
                     (make-census (list (make-record "tests/a.rkt" 800)
                                        (make-record "tests/c.rkt" 200)))))
(check-exn exn:fail:work-mass-comparison? missing-inventory-compare)
(check-true (with-handlers ([exn:fail:work-mass-comparison?
                             (lambda (e) (string-contains? (exn-message e) "tests/b.rkt"))])
              (missing-inventory-compare)
              #f))

;; New W7 files get explicit rows, never silent additions.
(define comparison (work-mass-compare baseline-census post-census))
(define inventory-added (hash-ref (hash-ref comparison 'inventory) 'added))
(check-equal? (length inventory-added) 1)
(check-equal? (hash-ref (car inventory-added) 'path) "tests/new-e.rkt")
(check-equal? (hash-ref (car inventory-added) 'median_ms) 100)
(check-false (null? (hash-ref (hash-ref comparison 'inventory) 'added)))
(check-equal? (hash-ref (hash-ref comparison 'inventory) 'removed) '())

;; Collection failures on both sides are retained as explicit rows too.
(define post-with-new-failure
  (make-census (list (make-record "tests/a.rkt" 800)
                     (make-record "tests/b.rkt" 450)
                     (make-record "tests/c.rkt" 200)
                     (make-record "tests/d.rkt" 300)
                     (make-failure "tests/flaky.rkt" "compile error"))))
(define cmp-with-failure (work-mass-compare baseline-census post-with-new-failure))
(check-true (ormap (lambda (row) (equal? (hash-ref row 'path) "tests/flaky.rkt"))
                   (hash-ref (hash-ref cmp-with-failure 'inventory) 'added)))
(check-equal? (hash-ref (car (filter (lambda (r) (equal? (hash-ref r 'path) "tests/flaky.rkt"))
                                     (hash-ref (hash-ref cmp-with-failure 'inventory) 'added)))
                        'status)
              "collection-failure")

;; --------------------------------------- §W7.2 median math + aggregate check

;; Work mass row: recomputed from stored medians; percentage from medians.
(define rows (hash-ref comparison 'rows))
(define (find-row metric)
  (car (filter (lambda (r) (equal? (hash-ref r 'metric) metric)) rows)))

(define work-mass-row (find-row "fast work mass"))
(check-equal? (hash-ref work-mass-row 'baseline) 2000) ; 1000+500+200+300
(check-equal? (hash-ref work-mass-row 'post) 1850) ; 800+450+200+300+100
(check-equal? (hash-ref work-mass-row 'delta) -150)
(check-equal? (hash-ref work-mass-row 'delta_pct) -7.5) ; (1850-2000)/2000*100

;; Stored aggregate disagreeing with the median sum is a red error.
(define (lying-aggregate-compare)
  (work-mass-compare (make-census (list (make-record "tests/a.rkt" 1000)) #:work-mass 999999)
                     (make-census (list (make-record "tests/a.rkt" 1000)))))
(check-exn exn:fail:work-mass-comparison? lying-aggregate-compare)

;; Top-N contribution rows: math over sorted stored medians.
(define top10 (find-row "top 10 contribution"))
(check-equal? (hash-ref top10 'baseline) 2000) ; fewer than 10 files: all of them
(check-equal? (hash-ref top10 'post) 1850)
;; top-1 row keeps the largest median: tests/a.rkt dominates both sides.
(check-equal? (hash-ref (find-row "top 1 contribution") 'baseline) 1000)
(check-equal? (hash-ref (find-row "top 1 contribution") 'post) 800)

;; Bucket count rows carry baseline/post file_count by label.
(define bucket-row
  (car (filter (lambda (r)
                 (and (equal? (hash-ref r 'metric) "bucket count")
                      (equal? (hash-ref r 'label) "<250 ms")))
               rows)))
(check-equal? (hash-ref bucket-row 'baseline) 2)
(check-equal? (hash-ref bucket-row 'post) 3)

;; ---------------------------------------------------- §W7.4 work-type rows

;; Total process launches = subprocesses + racket_subprocesses.
(define proc-row (find-row "total process launches"))
(check-equal? (hash-ref proc-row 'baseline) 12)
(check-equal? (hash-ref proc-row 'post) 9)
(check-equal? (hash-ref proc-row 'delta) -3)

(check-equal? (hash-ref (find-row "git command launches") 'baseline) 3)
(check-equal? (hash-ref (find-row "git command launches") 'post) 1)
(check-equal? (hash-ref (find-row "fixture constructions") 'baseline) 5)
(check-equal? (hash-ref (find-row "fixture constructions") 'post) 2)
(check-equal? (hash-ref (find-row "total requested real sleep (ms)") 'baseline) 700)
(check-equal? (hash-ref (find-row "total requested real sleep (ms)") 'post) 250)

;; Unknown (null) counters stay null and produce an "unknown" row, never 0.
(define cmp-unknown
  (work-mass-compare (make-census (list (make-record "tests/a.rkt" 1000))
                                  #:work-type-totals (hasheq 'subprocesses 'null))
                     (make-census (list (make-record "tests/a.rkt" 1000))
                                  #:work-type-totals (hasheq 'subprocesses 5))))
(define unknown-row
  (car (filter (lambda (r) (equal? (hash-ref r 'metric) "total process launches"))
               (hash-ref cmp-unknown 'rows))))
(check-equal? (hash-ref unknown-row 'baseline) 'null)
(check-equal? (hash-ref unknown-row 'status) "unknown")

;; ------------------------------------------------- grouped-safe share (Q7)

(define q7-row (find-row "grouped-safe share"))
;; baseline: a+c = 1200 of 2000 -> 60.0; post: a+c+new-e = 1100 of 1850 -> 59.46
(check-equal? (hash-ref q7-row 'baseline_pct) 60.0)
(check-equal? (hash-ref q7-row 'post_pct) 59.46)
(check-equal? (hash-ref q7-row 'baseline_files) 2)
(check-equal? (hash-ref q7-row 'post_files) 3)

;; --------------------------------------------------------------- summary

(define summary (hash-ref comparison 'summary))
(check-equal? (hash-ref summary 'work_mass_delta_pct) -7.5)
(check-equal? (hash-ref summary 'verdict) "insufficient")
(check-true (hash-has-key? summary 'generated_at_utc))

(printf "tests/test-work-mass-comparison.rkt: all contract checks passed\n")
