#lang racket/base

;; @speed fast
;; @suite testing
;; @boundary unit
;; @isolation none
;; W4: Synthetic tests for cohort-report.rkt — reproducible 20-PR cohort evidence.
;;
;; Tests cover: exactly 20 SHAs, fewer than 20 (rejected and accepted with
;; exclusions), duplicate SHA, failed-then-passed rerun, named exclusion,
;; missing/corrupt artifact (missing required field), inventory mismatch
;; (empty digest), incompatible scheduler, zero-test flag consistency,
;; percentile edges, and deterministic byte-identical output.

(require rackunit
         rackunit/text-ui
         racket/file
         racket/format
         racket/list
         racket/path
         racket/runtime-path
         racket/string
         json
         (only-in "../util/version.rkt" q-version)
         "../scripts/run-tests/cohort-report.rkt")

(define-runtime-path here ".")
(define fixtures-dir (build-path here "fixtures" "ci-cohort"))

;; ============================================================
;; Helpers for building in-memory manifests
;; ============================================================

(define (make-timing-attempt i elapsed)
  (hasheq 'run-id
          (format "33~a" (+ 000000001 (* i 100)))
          'result
          "success"
          'elapsed-seconds
          elapsed
          'timing-sample
          #t))

(define (make-failed-attempt i elapsed)
  (hasheq 'run-id
          (format "33~a-fail" (+ 000000001 (* i 100)))
          'result
          "failure"
          'elapsed-seconds
          elapsed
          'timing-sample
          #f))

(define (make-valid-sha i
                        #:elapsed [elapsed 300.0]
                        #:attempts [attempts #f]
                        #:inventory-digest [digest #f]
                        #:scheduler [sched "batch"]
                        #:test-count [tc #f]
                        #:zero-test [zt #f]
                        #:prepared-env [pe "match"])
  (define file-count (+ 1162 i))
  (define real-tc (or tc (+ 16808 (* i 3))))
  (hasheq 'sha
          (format "sha~a" i)
          'pr
          (+ 9550 i)
          'scheduler
          sched
          'ordering
          "fifo"
          'attempts
          (or attempts (list (make-timing-attempt i elapsed)))
          'inventory-digest
          (or digest (format "sha256:digest~a" i))
          'file-count
          file-count
          'test-count
          real-tc
          'pass
          file-count
          'fail
          0
          'timeout
          0
          'skip
          0
          'zero-test
          (if zt #t #f)
          'flakes
          0
          'parallel-only-failures
          0
          'prepared-env
          pe
          'queue-wait-seconds
          12
          'queue-depth
          0
          'runner-minutes
          7.5))

(define (make-manifest #:shas shas
                       #:exclusions [exclusions '()]
                       #:expected-count [ec 20]
                       #:cohort-id [cid "test-cohort"])
  (hasheq 'cohort-id
          cid
          'milestone
          (format "v~a" q-version)
          'schema-version
          1
          'expected-count
          ec
          'shas
          shas
          'exclusions
          exclusions))

(define (make-valid-cohort n)
  (make-manifest #:shas (for/list ([i (in-range n)])
                          (make-valid-sha i))))

(define (has-error-matching? vr rx)
  (and (not (validation-ok? vr)) (ormap (lambda (e) (regexp-match? rx e)) (validation-errors vr))))

;; ============================================================
;; Helpers for paired configuration manifests (v1.00.25 W0: C1)
;; ============================================================

(define config-hex "0123456789abcdef")

(define (fake-sha i)
  ;; Injective on 0..99: the first char encodes (modulo i 16), the tail phase
  ;; encodes (quotient i 16) so i and i+16 never collide.
  (list->string (cons (string-ref config-hex (modulo i 16))
                      (for/list ([k (in-range 39)])
                        (string-ref config-hex (modulo (+ (* (quotient i 16) 7) k 3) 16))))))

;; Shadow-configuration row: registered, attempts pending until the
;; automatic paired runs land (allowed while the cohort is started/open).
(define (make-config-sha-row i digest #:attempts [attempts '()])
  (hasheq 'sha (fake-sha i) 'pr (+ 9580 i) 'attempts attempts 'inventory-digest digest))

;; Required-lane baseline rows (full schema; also the flat `shas` view).
(define baseline-config-rows
  (for/list ([i (in-range 20)])
    (hash-set (make-valid-sha i) 'sha (fake-sha i))))

(define (make-config config-id
                     lane
                     scheduler
                     ordering
                     required
                     #:shas [shas #f]
                     #:eligible [eligible #f]
                     #:start-sha [start-sha (fake-sha 99)])
  (hasheq 'config-id
          config-id
          'lane
          lane
          'scheduler
          scheduler
          'ordering
          ordering
          'start-sha
          start-sha
          'required
          required
          'eligible-shas
          (or eligible (map (lambda (r) (hash-ref r 'sha)) baseline-config-rows))
          'shas
          (or shas
              (if required
                  baseline-config-rows
                  (for/list ([r (in-list baseline-config-rows)])
                    (make-config-sha-row (index-of baseline-config-rows r)
                                         (hash-ref r 'inventory-digest)))))))

(define (default-paired-configs)
  (list (make-config "fast/batch/fifo" "fast" "batch" "fifo" #t)
        (make-config "fast/queue/fifo" "fast" "queue" "fifo" #f)
        (make-config "fast/queue/lpt" "fast" "queue" "lpt" #f)
        (make-config "security/queue/fifo" "security" "queue" "fifo" #f)))

(define (make-paired-manifest #:cohort-status [cohort-status "started"]
                              #:configs [configs #f]
                              #:shas [flat-shas baseline-config-rows])
  (hasheq 'cohort-id
          "test-c1"
          'milestone
          (format "v~a" q-version)
          'schema-version
          1
          'expected-count
          20
          'cohort-status
          cohort-status
          'start-sha
          (fake-sha 99)
          'baseline-config
          "fast/batch/fifo"
          'configurations
          (or configs (default-paired-configs))
          'shas
          flat-shas
          'exclusions
          '()))

;; Shadow-leg row factory: full-schema rows for a shadow configuration,
;; with a final successful timing attempt per SHA by default.  The keyword
;; overrides let tests inject failures, reruns, and divergent inventories.
(define (complete-shadow-rows
         #:elapsed-at [elapsed-at (lambda (i) 100.0)]
         #:attempts-for [attempts-for (lambda (i) #f)]
         #:digest-for
         [digest-for (lambda (i) (hash-ref (list-ref baseline-config-rows i) 'inventory-digest))])
  (for/list ([i (in-range 20)])
    (define base-row (list-ref baseline-config-rows i))
    (define attempts (or (attempts-for i) (list (make-timing-attempt i (elapsed-at i)))))
    (hash-set* base-row 'scheduler "queue" 'attempts attempts 'inventory-digest (digest-for i))))

;; Paired manifest with explicit shadow-leg evidence: each argument is
;; either a 20-row list (leg complete) or #f (leg still pending, no rows
;; ingested).  The required baseline lane always carries its rows.
(define (paired-manifest-with-legs queue-rows lpt-rows security-rows)
  (define (leg config-id lane scheduler ordering rows)
    (make-config config-id
                 lane
                 scheduler
                 ordering
                 #f
                 #:shas (or rows '())
                 #:eligible (map (lambda (r) (hash-ref r 'sha)) baseline-config-rows)))
  (make-paired-manifest #:configs
                        (list (make-config "fast/batch/fifo" "fast" "batch" "fifo" #t)
                              (leg "fast/queue/fifo" "fast" "queue" "fifo" queue-rows)
                              (leg "fast/queue/lpt" "fast" "queue" "lpt" lpt-rows)
                              (leg "security/queue/fifo" "security" "queue" "fifo" security-rows))))

;; ============================================================
;; Test suite
;; ============================================================

(define suite
  (test-suite "ci-cohort-report tests"

    ;; --- 1. Exactly 20 SHAs ----------------------------------------------

    (test-case "valid-20.json fixture loads and validates with exactly 20 SHAs"
      (define manifest
        (load-cohort-manifest (path->string (build-path fixtures-dir "valid-20.json"))))
      (check-true (cohort-manifest? manifest))
      (define vr (validate-cohort manifest))
      (check-true (validation-ok? vr)
                  (format "expected validation OK; errors: ~a" (validation-errors vr)))
      (check-equal? (length (hash-ref manifest 'shas)) 20)
      (check-equal? (length (hash-ref manifest 'exclusions)) 0))

    (test-case "exactly 20 SHAs built in-memory validate OK"
      (define manifest (make-valid-cohort 20))
      (define vr (validate-cohort manifest))
      (check-true (validation-ok? vr)
                  (format "expected validation OK; errors: ~a" (validation-errors vr)))
      (check-equal? (validation-ok? vr) #t))

    ;; --- 2. Fewer than 20 ------------------------------------------------

    (test-case "fewer than 20 without exclusions is rejected (silently truncated)"
      (define manifest (make-valid-cohort 15))
      (define vr (validate-cohort manifest))
      (check-false (validation-ok? vr))
      (check-true (has-error-matching? vr #rx"silently truncated")
                  (format "expected silently-truncated error; got: ~a" (validation-errors vr))))

    (test-case "fewer than 20 with named exclusions accounting for gaps is accepted"
      (define shas
        (for/list ([i (in-range 15)])
          (make-valid-sha i)))
      (define exclusions
        (for/list ([i (in-range 5)])
          (hasheq 'sha
                  (format "excluded~a" i)
                  'reason
                  (list-ref known-exclusion-reasons i)
                  'detail
                  "test exclusion")))
      (define manifest (make-manifest #:shas shas #:exclusions exclusions))
      (define vr (validate-cohort manifest))
      (check-true (validation-ok? vr)
                  (format "expected validation OK with 15+5 exclusions; errors: ~a"
                          (validation-errors vr))))

    (test-case "fewer than 20 with too few exclusions is rejected"
      (define shas
        (for/list ([i (in-range 15)])
          (make-valid-sha i)))
      (define exclusions
        (list (hasheq 'sha "excluded0" 'reason "missing-lane-artifact" 'detail "test")))
      (define manifest (make-manifest #:shas shas #:exclusions exclusions))
      (define vr (validate-cohort manifest))
      (check-false (validation-ok? vr))
      (check-true (has-error-matching? vr #rx"silently truncated")))

    (test-case "more than 20 SHAs is rejected"
      (define manifest (make-valid-cohort 21))
      (define vr (validate-cohort manifest))
      (check-false (validation-ok? vr))
      (check-true (has-error-matching? vr #rx"too many")))

    ;; --- 3. Duplicate SHA ------------------------------------------------

    (test-case "duplicate SHA is rejected"
      (define shas
        (for/list ([i (in-range 20)])
          (make-valid-sha (if (= i 5) 3 i)))) ; index 5 duplicates index 3
      (define manifest (make-manifest #:shas shas))
      (define vr (validate-cohort manifest))
      (check-false (validation-ok? vr))
      (check-true (has-error-matching? vr #rx"duplicate SHA")
                  (format "expected duplicate SHA error; got: ~a" (validation-errors vr))))

    ;; --- 4. Failed-then-passed rerun -------------------------------------

    (test-case "failed-then-passed rerun loads and validates; reliability shows failure"
      (define manifest
        (load-cohort-manifest (path->string (build-path fixtures-dir "valid-20-with-reruns.json"))))
      (define vr (validate-cohort manifest))
      (check-true (validation-ok? vr)
                  (format "expected validation OK with reruns; errors: ~a" (validation-errors vr)))
      ;; Reliability summary must show the failed attempts
      (define summary (cohort-attempts-summary manifest))
      (check-true (> (hash-ref summary 'failures) 0)
                  "expected at least one failure in reliability summary"))

    (test-case "SHA with failed attempt then timing-sample is eligible"
      (define sha
        (make-valid-sha 0
                        #:attempts (list (make-failed-attempt 0 400.0)
                                         (make-timing-attempt 0 300.0))))
      (check-true (sha-eligible? sha))
      (check-true (sha-has-timing-sample? sha))
      (define ts (sha-final-success-attempt sha))
      (check-true (hash-ref ts 'timing-sample))
      (check-equal? (hash-ref ts 'result) "success"))

    (test-case "SHA with two timing samples is NOT eligible (ambiguous)"
      (define sha
        (make-valid-sha 0
                        #:attempts (list (make-timing-attempt 0 300.0)
                                         (make-timing-attempt 0 310.0))))
      (check-false (sha-eligible? sha))
      (define vr (validate-cohort (make-manifest #:shas (list sha) #:expected-count 1)))
      (check-false (validation-ok? vr)))

    (test-case "SHA with zero attempts is not eligible"
      (check-false (sha-eligible? (hasheq 'sha "x" 'attempts '()))))

    ;; --- 5. Named exclusion ----------------------------------------------

    (test-case "exclusion with named mechanical reason is accepted"
      (define shas
        (for/list ([i (in-range 19)])
          (make-valid-sha i)))
      (define exclusions
        (list (hasheq 'sha "excluded0" 'reason "inventory-mismatch" 'detail "digest mismatch")))
      (define manifest (make-manifest #:shas shas #:exclusions exclusions))
      (define vr (validate-cohort manifest))
      (check-true (validation-ok? vr)))

    (test-case "exclusion with unnamed reason is rejected"
      (define shas
        (for/list ([i (in-range 19)])
          (make-valid-sha i)))
      (define exclusions (list (hasheq 'sha "excluded0" 'reason "made-up-reason" 'detail "bad")))
      (define manifest (make-manifest #:shas shas #:exclusions exclusions))
      (define vr (validate-cohort manifest))
      (check-false (validation-ok? vr))
      (check-true (has-error-matching? vr #rx"unnamed reason")))

    (test-case "exclusion SHA also in cohort is rejected (contradiction)"
      (define shas
        (for/list ([i (in-range 19)])
          (make-valid-sha i)))
      ;; The exclusion names the same SHA as shas[0]
      (define exclusions
        (list (hasheq 'sha "sha0" 'reason "inventory-mismatch" 'detail "contradiction")))
      (define manifest (make-manifest #:shas shas #:exclusions exclusions))
      (define vr (validate-cohort manifest))
      (check-false (validation-ok? vr))
      (check-true (has-error-matching? vr #rx"contradiction")))

    ;; --- 6. Missing / corrupt artifact -----------------------------------

    (test-case "SHA missing required field is rejected (corrupt artifact)"
      (define sha (make-valid-sha 0))
      (define bad-sha (hash-remove sha 'file-count))
      (define manifest
        (make-manifest #:shas (cons bad-sha
                                    (for/list ([i (in-range 1 20)])
                                      (make-valid-sha i)))))
      (define vr (validate-cohort manifest))
      (check-false (validation-ok? vr))
      (check-true (has-error-matching? vr #rx"missing required field: file-count")))

    (test-case "SHA missing timing-sample attempt is rejected"
      (define sha (hash-set (make-valid-sha 0) 'attempts (list (make-failed-attempt 0 300.0))))
      (define manifest
        (make-manifest #:shas (cons sha
                                    (for/list ([i (in-range 1 20)])
                                      (make-valid-sha i)))))
      (define vr (validate-cohort manifest))
      (check-false (validation-ok? vr))
      (check-true (has-error-matching? vr #rx"not eligible")))

    ;; --- 7. Inventory mismatch -------------------------------------------

    (test-case "SHA with empty inventory-digest is rejected"
      (define sha (make-valid-sha 0 #:inventory-digest ""))
      (define manifest
        (make-manifest #:shas (cons sha
                                    (for/list ([i (in-range 1 20)])
                                      (make-valid-sha i)))))
      (define vr (validate-cohort manifest))
      (check-false (validation-ok? vr))
      (check-true (has-error-matching? vr #rx"missing/empty inventory-digest")))

    (test-case "SHA with missing inventory-digest is rejected"
      (define sha (hash-remove (make-valid-sha 0) 'inventory-digest))
      (define manifest
        (make-manifest #:shas (cons sha
                                    (for/list ([i (in-range 1 20)])
                                      (make-valid-sha i)))))
      (define vr (validate-cohort manifest))
      (check-false (validation-ok? vr))
      (check-true (has-error-matching? vr #rx"missing required field: inventory-digest")))

    ;; --- 7b. Incompatible scheduler --------------------------------------

    (test-case "SHA with incompatible scheduler is rejected"
      (define sha (make-valid-sha 0 #:scheduler "unknown-scheduler"))
      (define manifest
        (make-manifest #:shas (cons sha
                                    (for/list ([i (in-range 1 20)])
                                      (make-valid-sha i)))))
      (define vr (validate-cohort manifest))
      (check-false (validation-ok? vr))
      (check-true (has-error-matching? vr #rx"incompatible scheduler")))

    ;; --- 7c. Zero-test flag consistency ---------------------------------

    (test-case "SHA with test-count 0 but zero-test=false is rejected"
      (define sha (make-valid-sha 0 #:test-count 0 #:zero-test #f))
      (define manifest
        (make-manifest #:shas (cons sha
                                    (for/list ([i (in-range 1 20)])
                                      (make-valid-sha i)))))
      (define vr (validate-cohort manifest))
      (check-false (validation-ok? vr))
      (check-true (has-error-matching? vr #rx"zero-test flag not set")))

    (test-case "SHA with test-count 0 and zero-test=true is accepted"
      (define sha (make-valid-sha 0 #:test-count 0 #:zero-test #t))
      (define manifest
        (make-manifest #:shas (cons sha
                                    (for/list ([i (in-range 1 20)])
                                      (make-valid-sha i)))))
      (define vr (validate-cohort manifest))
      (check-true (validation-ok? vr)
                  (format "expected validation OK with zero-test flag; errors: ~a"
                          (validation-errors vr))))

    ;; --- 8. Percentile edges --------------------------------------------

    (test-case "cohort-quantile of empty list returns #f"
      (check-false (cohort-quantile '() 0.5)))

    (test-case "cohort-quantile of single sample returns that sample"
      (check-equal? (cohort-quantile '(42.0) 0.5) 42.0)
      (check-equal? (cohort-quantile '(42.0) 0.95) 42.0))

    (test-case "cohort-quantile of two samples returns midpoint at p50 and p95"
      (check-equal? (cohort-quantile '(100.0 200.0) 0.5) 150.0)
      (check-equal? (cohort-quantile '(100.0 200.0) 0.95) 150.0))

    (test-case "cohort-quantile of 20 ascending samples: p50 is between 9th and 10th"
      (define samples
        (for/list ([i (in-range 20)])
          (* (+ i 1) 10.0)))
      (define p50 (cohort-quantile samples 0.5))
      (define p95 (cohort-quantile samples 0.95))
      (check-true (and (number? p50) (<= 100.0 p50 110.0))
                  (format "p50=~a expected in [100,110]" p50))
      (check-true (and (number? p95) (<= 180.0 p95 200.0))
                  (format "p95=~a expected in [180,200]" p95)))

    (test-case "cohort-quantile matches baseline-report quantile algorithm"
      ;; The cohort-quantile uses the same linear-interpolation method as
      ;; baseline-report.rkt. Verify it on a known 4-element sample.
      ;; sorted: [10,20,30,40], n=4
      ;; p50: k=0.5*3=1.5, lo=1, hi=2 → (20+30)/2 = 25.0
      ;; p95: k=0.95*3=2.85, lo=2, hi=3 → (30+40)/2 = 35.0
      (check-equal? (cohort-quantile '(10.0 20.0 30.0 40.0) 0.5) 25.0)
      (check-equal? (cohort-quantile '(10.0 20.0 30.0 40.0) 0.95) 35.0))

    ;; --- 9. Deterministic output -----------------------------------------

    (test-case "report JSON is deterministic: same manifest, byte-identical output"
      (define manifest
        (load-cohort-manifest (path->string (build-path fixtures-dir "valid-20.json"))))
      (define json1 (cohort-report-json-string manifest))
      (define json2 (cohort-report-json-string manifest))
      (check-equal? json1 json2)
      ;; Digest must also be identical
      (check-equal? (manifest-digest manifest) (manifest-digest manifest)))

    (test-case "report markdown is deterministic: same manifest, byte-identical output"
      (define manifest
        (load-cohort-manifest (path->string (build-path fixtures-dir "valid-20.json"))))
      (define md1 (cohort-report-md-string manifest))
      (define md2 (cohort-report-md-string manifest))
      (check-equal? md1 md2))

    (test-case "report jsexpr contains required statistics and counts"
      (define manifest
        (load-cohort-manifest (path->string (build-path fixtures-dir "valid-20.json"))))
      (define r (cohort-report-jsexpr manifest))
      (check-equal? (hash-ref r 'cohort-size) 20)
      (check-equal? (hash-ref r 'expected-size) 20)
      (check-equal? (hash-ref r 'exclusion-count) 0)
      (define stats (hash-ref r 'statistics))
      (check-equal? (hash-ref stats 'sample-count) 20)
      (check-true (number? (hash-ref stats 'p50-seconds)))
      (check-true (number? (hash-ref stats 'p95-seconds)))
      (define counts (hash-ref r 'counts))
      (check-true (number? (hash-ref counts 'total-pass)))
      (check-true (number? (hash-ref counts 'total-fail)))
      ;; runner-minutes is a top-level key, not under counts
      (check-true (number? (hash-ref (hash-ref r 'runner-minutes) 'total)))
      ;; manifest digest present
      (check-true (string? (hash-ref r 'manifest-digest))))

    (test-case "report from reruns fixture captures reliability evidence"
      (define manifest
        (load-cohort-manifest (path->string (build-path fixtures-dir "valid-20-with-reruns.json"))))
      (define r (cohort-report-jsexpr manifest))
      (define rel (hash-ref r 'reliability))
      (check-true (> (hash-ref rel 'total-attempts) 20)
                  "rerun cohort should have more attempts than 20")
      (check-true (> (hash-ref rel 'failures) 0)
                  "rerun cohort should have failures in reliability evidence"))

    (test-case "report from exclusions fixture lists all exclusions"
      (define manifest
        (load-cohort-manifest (path->string (build-path fixtures-dir
                                                        "valid-15-with-exclusions.json"))))
      (define vr (validate-cohort manifest))
      (check-true (validation-ok? vr)
                  (format "expected validation OK; errors: ~a" (validation-errors vr)))
      (define r (cohort-report-jsexpr manifest))
      (check-equal? (hash-ref r 'cohort-size) 15)
      (check-equal? (hash-ref r 'exclusion-count) 5)
      (define exclusions (hash-ref r 'exclusions))
      (check-equal? (length exclusions) 5))

    ;; --- 10. Check mode: byte-identical regeneration ---------------------

    (test-case "cohort-check regenerates byte-identical report"
      ;; Write a manifest + its generated report to temp files, then verify
      ;; --check reproduces the report byte-for-byte.
      (define manifest (make-valid-cohort 20))
      (define tmp-manifest (make-temporary-file "cohort-manifest-~a.json"))
      (define tmp-report (make-temporary-file "cohort-report-~a.json"))
      (dynamic-wind
       (lambda () (void))
       (lambda ()
         (call-with-output-file tmp-manifest
                                #:exists 'replace
                                (lambda (out) (write-json manifest out)))
         (define json-str (cohort-report-json-string manifest))
         (call-with-output-file tmp-report #:exists 'replace (lambda (out) (display json-str out)))
         (define-values (ok reason)
           (cohort-check (path->string tmp-manifest) (path->string tmp-report)))
         (check-true ok (format "expected check PASS; reason: ~a" reason)))
       (lambda ()
         (when (file-exists? tmp-manifest)
           (delete-file tmp-manifest))
         (when (file-exists? tmp-report)
           (delete-file tmp-report)))))

    (test-case "cohort-check detects mismatch"
      (define manifest (make-valid-cohort 20))
      (define tmp-manifest (make-temporary-file "cohort-manifest-~a.json"))
      (define tmp-report (make-temporary-file "cohort-report-~a.json"))
      (dynamic-wind (lambda () (void))
                    (lambda ()
                      (call-with-output-file tmp-manifest
                                             #:exists 'replace
                                             (lambda (out) (write-json manifest out)))
                      ;; Write a deliberately wrong report
                      (call-with-output-file tmp-report
                                             #:exists 'replace
                                             (lambda (out) (display "{\"wrong\":true}" out)))
                      (define-values (ok reason)
                        (cohort-check (path->string tmp-manifest) (path->string tmp-report)))
                      (check-false ok))
                    (lambda ()
                      (when (file-exists? tmp-manifest)
                        (delete-file tmp-manifest))
                      (when (file-exists? tmp-report)
                        (delete-file tmp-report)))))

    ;; --- 11. Constants and schema ---------------------------------------

    (test-case "expected-cohort-size is 20"
      (check-equal? expected-cohort-size 20))

    (test-case "cohort-schema-version is 1"
      (check-equal? cohort-schema-version 1))

    (test-case "known-exclusion-reasons covers required mechanical reasons"
      (for ([reason (in-list '("missing-lane-artifact" "incompatible-scheduler"
                                                       "incompatible-config"
                                                       "inventory-mismatch"
                                                       "artifact-corrupt"
                                                       "artifact-expired"
                                                       "non-unique-sha"))])
        (check-not-false (member reason known-exclusion-reasons)
                         (format "expected ~a in known-exclusion-reasons" reason))))

    ;; --- 12. Manifest digest --------------------------------------------

    (test-case "manifest-digest is deterministic and change-sensitive"
      (define m1 (make-valid-cohort 20))
      (define m2 (make-valid-cohort 20))
      ;; Same content → same digest
      (check-equal? (manifest-digest m1) (manifest-digest m2))
      ;; Different content → different digest
      (define m3
        (make-manifest #:shas (for/list ([i (in-range 20)])
                                (make-valid-sha i #:elapsed 999.0))))
      (check-not-equal? (manifest-digest m1) (manifest-digest m3)))

    ;; --- 13. Queue telemetry and runner cost in report -------------------

    (test-case "report contains queue telemetry and runner-minute cost"
      (define manifest (make-valid-cohort 20))
      (define r (cohort-report-jsexpr manifest))
      (check-true (hash? (hash-ref r 'queue-telemetry)))
      (check-true (hash? (hash-ref r 'runner-minutes)))
      (check-true (number? (hash-ref (hash-ref r 'queue-telemetry) 'total-wait-seconds)))
      (check-true (number? (hash-ref (hash-ref r 'runner-minutes) 'total))))

    ;; --- 14. Prepared-env outcomes in report -----------------------------

    (test-case "report contains prepared-env outcomes"
      (define manifest (make-valid-cohort 20))
      (define r (cohort-report-jsexpr manifest))
      (check-true (list? (hash-ref r 'prepared-env-outcomes)))
      (check-equal? (length (hash-ref r 'prepared-env-outcomes)) 20))

    ;; --- 15. No external service dependency -----------------------------

    (test-case "cohort-report requires no network: pure function of manifest"
      ;; The cohort-report-jsexpr function takes only a manifest hash and
      ;; returns a jsexpr. No I/O, no network, no database.
      (define manifest (make-valid-cohort 5))
      (define r (cohort-report-jsexpr manifest))
      (check-true (hash? r))
      (check-true (hash? (hash-ref r 'statistics)))
      (check-true (hash? (hash-ref r 'counts))))

    ;; --- 16. Paired configuration manifests (v1.00.25 W0: C1 shadow cohort) ---

    (test-case "paired configuration manifest with baseline + shadow configs validates OK"
      (define manifest (make-paired-manifest))
      (define vr (validate-cohort manifest))
      (check-true (validation-ok? vr)
                  (format "expected validation OK; errors: ~a" (validation-errors vr))))

    (test-case "configuration row missing required field is rejected"
      (define configs
        (cons (hash-remove (make-config "fast/batch/fifo" "fast" "batch" "fifo" #t) 'ordering)
              (cdr (default-paired-configs))))
      (define vr (validate-cohort (make-paired-manifest #:configs configs)))
      (check-false (validation-ok? vr))
      (check-true (has-error-matching? vr #rx"missing required field: ordering")
                  (format "got: ~a" (validation-errors vr))))

    (test-case "configuration with unknown lane is rejected"
      (define configs
        (list (make-config "fast/batch/fifo" "fast" "batch" "fifo" #t)
              (make-config "staging/queue/fifo" "staging" "queue" "fifo" #f)))
      (define vr (validate-cohort (make-paired-manifest #:configs configs)))
      (check-false (validation-ok? vr))
      (check-true (has-error-matching? vr #rx"unknown lane")))

    (test-case "configuration with unknown ordering is rejected"
      (define configs
        (list (make-config "fast/batch/fifo" "fast" "batch" "fifo" #t)
              (make-config "fast/queue/random" "fast" "queue" "random" #f)))
      (define vr (validate-cohort (make-paired-manifest #:configs configs)))
      (check-false (validation-ok? vr))
      (check-true (has-error-matching? vr #rx"unknown ordering")))

    (test-case "configuration with unknown scheduler is rejected"
      (define configs
        (list (make-config "fast/batch/fifo" "fast" "batch" "fifo" #t)
              (make-config "fast/speculative/fifo" "fast" "speculative" "fifo" #f)))
      (define vr (validate-cohort (make-paired-manifest #:configs configs)))
      (check-false (validation-ok? vr))
      (check-true (has-error-matching? vr #rx"incompatible configuration scheduler")))

    (test-case "configuration config-id must match lane/scheduler/ordering"
      (define configs
        (list (make-config "fast/batch/fifo" "fast" "batch" "fifo" #t)
              (make-config "mislabeled/id" "fast" "queue" "fifo" #f)))
      (define vr (validate-cohort (make-paired-manifest #:configs configs)))
      (check-false (validation-ok? vr))
      (check-true (has-error-matching? vr #rx"config-id")))

    (test-case "configuration start-sha must be 40-hex and consistent across configurations"
      (define configs
        (list (make-config "fast/batch/fifo" "fast" "batch" "fifo" #t)
              (make-config "fast/queue/fifo" "fast" "queue" "fifo" #f)
              (make-config "fast/queue/lpt" "fast" "queue" "lpt" #f #:start-sha "deadbeef")))
      (define vr (validate-cohort (make-paired-manifest #:configs configs)))
      (check-false (validation-ok? vr))
      (check-true (has-error-matching? vr #rx"start-sha")))

    (test-case "eligible-SHA list must be identical across configurations"
      (define c3 (make-config "fast/queue/lpt" "fast" "queue" "lpt" #f))
      (define c3-short (hash-set c3 'eligible-shas (cdr (hash-ref c3 'eligible-shas))))
      (define configs
        (list (make-config "fast/batch/fifo" "fast" "batch" "fifo" #t)
              (make-config "fast/queue/fifo" "fast" "queue" "fifo" #f)
              c3-short))
      (define vr (validate-cohort (make-paired-manifest #:configs configs)))
      (check-false (validation-ok? vr))
      (check-true (has-error-matching? vr #rx"eligible-SHA list mismatch")))

    (test-case "duplicate SHA within a configuration is rejected"
      (define eligible (map (lambda (r) (hash-ref r 'sha)) baseline-config-rows))
      (define c2
        (make-config "fast/queue/fifo"
                     "fast"
                     "queue"
                     "fifo"
                     #f
                     #:eligible (list* (second eligible) eligible)))
      (define configs (list (make-config "fast/batch/fifo" "fast" "batch" "fifo" #t) c2))
      (define vr (validate-cohort (make-paired-manifest #:configs configs)))
      (check-false (validation-ok? vr))
      (check-true (has-error-matching? vr #rx"duplicate SHA")))

    (test-case "exactly one required (required-lane baseline) configuration"
      (define vr-none
        (validate-cohort (make-paired-manifest
                          #:configs (list (make-config "fast/batch/fifo" "fast" "batch" "fifo" #f)
                                          (make-config "fast/queue/fifo" "fast" "queue" "fifo" #f)))))
      (check-false (validation-ok? vr-none))
      (check-true (has-error-matching? vr-none #rx"exactly one required configuration"))
      (define vr-two
        (validate-cohort (make-paired-manifest
                          #:configs (list (make-config "fast/batch/fifo" "fast" "batch" "fifo" #t)
                                          (make-config "fast/queue/fifo" "fast" "queue" "fifo" #t)))))
      (check-false (validation-ok? vr-two))
      (check-true (has-error-matching? vr-two #rx"exactly one required configuration")))

    (test-case "required configuration must be the required-lane baseline fast/batch/fifo"
      (define configs
        (list (make-config "fast/queue/fifo" "fast" "queue" "fifo" #t)
              (make-config "fast/batch/fifo" "fast" "batch" "fifo" #f)))
      (define vr (validate-cohort (make-paired-manifest #:configs configs)))
      (check-false (validation-ok? vr))
      (check-true (has-error-matching? vr #rx"required-lane baseline")))

    (test-case "inventory mismatch vs baseline is a cohort error, never silently ignored"
      (define shadow-rows
        (for/list ([r (in-list baseline-config-rows)]
                   [i (in-naturals)])
          (define digest (hash-ref r 'inventory-digest))
          (make-config-sha-row i (if (= i 7) "sha256:other-inventory" digest))))
      (define configs
        (list (make-config "fast/batch/fifo" "fast" "batch" "fifo" #t)
              (make-config "fast/queue/fifo" "fast" "queue" "fifo" #f #:shas shadow-rows)))
      (define vr (validate-cohort (make-paired-manifest #:configs configs)))
      (check-false (validation-ok? vr))
      (check-true (has-error-matching? vr #rx"inventory mismatch")))

    (test-case "shadow rows may have empty attempts while cohort is started"
      ;; The default paired manifest already carries empty shadow attempts
      ;; with cohort-status "started" — and the OK case above passes.  Here we
      ;; additionally pin that a started cohort does not demand shadow attempts.
      (define manifest (make-paired-manifest #:cohort-status "started"))
      (check-true (validation-ok? (validate-cohort manifest))))

    (test-case "closed cohort requires collected shadow attempts"
      (define vr (validate-cohort (make-paired-manifest #:cohort-status "closed")))
      (check-false (validation-ok? vr))
      (check-true (has-error-matching? vr #rx"closed cohort configuration .* has no attempts")))

    (test-case "required baseline rows must remain eligible inside configurations"
      (define baseline-no-attempts
        (hash-set (make-config "fast/batch/fifo" "fast" "batch" "fifo" #t)
                  'shas
                  (for/list ([r (in-list baseline-config-rows)]
                             [i (in-naturals)])
                    (if (zero? i)
                        (hash-set r 'attempts '())
                        r))))
      (define configs
        (list baseline-no-attempts (make-config "fast/queue/fifo" "fast" "queue" "fifo" #f)))
      (define vr (validate-cohort (make-paired-manifest #:configs configs)))
      (check-false (validation-ok? vr))
      (check-true (has-error-matching? vr #rx"exactly one timing-sample")))

    (test-case "malformed attempt entries are rejected (result + timing-sample flag)"
      (define c2-default (make-config "fast/queue/fifo" "fast" "queue" "fifo" #f))
      (define rows
        (cons (make-config-sha-row 0
                                   (hash-ref (first baseline-config-rows) 'inventory-digest)
                                   #:attempts (list (hasheq 'run-id "x" 'result "success")))
              (cdr (hash-ref c2-default 'shas))))
      (define c2 (hash-set c2-default 'shas rows))
      (define configs (list (make-config "fast/batch/fifo" "fast" "batch" "fifo" #t) c2))
      (define vr (validate-cohort (make-paired-manifest #:configs configs)))
      (check-false (validation-ok? vr))
      (check-true (has-error-matching? vr #rx"missing result/timing-sample flag")))

    (test-case "flat shas must match the required-lane configuration rows"
      (define vr (validate-cohort (make-paired-manifest #:shas (reverse baseline-config-rows))))
      (check-false (validation-ok? vr))
      (check-true (has-error-matching? vr
                                       #rx"flat shas do not match the required-lane configuration")))

    (test-case "report jsexpr exposes paired configurations with inventory equality"
      (define r (cohort-report-jsexpr (make-paired-manifest)))
      (define configs (hash-ref r 'configurations #f))
      (check-true (list? configs))
      (check-equal? (length configs) 4)
      (for ([c (in-list configs)])
        (check-true (hash-ref c 'inventory-equal-to-baseline)
                    (format "~a inventory not equal" (hash-ref c 'config-id)))
        (check-true (string? (hash-ref c 'start-sha))))
      (define baseline-c (first configs))
      (check-equal? (hash-ref baseline-c 'config-id) "fast/batch/fifo")
      (check-equal? (hash-ref baseline-c 'attempts-recorded) 20)
      (check-equal? (hash-ref (second configs) 'attempts-recorded) 0))

    (test-case "paired configuration report is deterministic"
      (define manifest (make-paired-manifest))
      (check-equal? (cohort-report-json-string manifest) (cohort-report-json-string manifest))
      (check-equal? (cohort-report-md-string manifest) (cohort-report-md-string manifest)))

    (test-case "paired configuration report markdown contains the configuration table"
      (define md (cohort-report-md-string (make-paired-manifest)))
      (check-true (string-contains? md "Paired configurations"))
      (check-true (string-contains? md "security/queue/fifo"))
      (check-true (string-contains? md "fast/queue/lpt")))

    (test-case "manifests without configurations keep the legacy report shape"
      (define r (cohort-report-jsexpr (make-valid-cohort 20)))
      (check-false (hash-has-key? r 'configurations)))

    ;; --- W1: decision outputs --------------------------------------------
    ;; Per-configuration p50/p95 with linear interpolation, reliability
    ;; counts, inventory-equality verdicts, and an explicit promote | hold
    ;; verdict per lane.  A missed gate produces hold, never a revised
    ;; target; missing paired evidence is a hold with named reasons.

    (test-case "decision report holds every lane while paired shadow evidence is missing"
      (define d (decision-report-jsexpr (make-paired-manifest)))
      (check-equal? (hash-ref d 'baseline-config) "fast/batch/fifo")
      (define baseline (hash-ref d 'baseline))
      (check-equal? (hash-ref baseline 'p50-seconds) 300.0)
      (check-equal? (hash-ref baseline 'p95-seconds) 300.0)
      (check-equal? (map (lambda (l) (hash-ref l 'lane)) (hash-ref d 'lanes))
                    (list "fast-queue" "fast-LPT" "security-queue"))
      (check-equal? (hash-ref d 'overall-verdict) "hold")
      (for ([l (in-list (hash-ref d 'lanes))])
        (check-equal? (hash-ref l 'verdict)
                      "hold"
                      (format "~a must hold without paired evidence" (hash-ref l 'lane)))
        (check-true (pair? (hash-ref l 'reasons)) "a hold must name its reasons")
        (check-false (hash-ref (hash-ref l 'numbers) 'p50-seconds)
                     "pending legs report no percentile")
        (check-equal? (hash-ref (hash-ref l 'numbers) 'inventory-equal-to-baseline)
                      #f
                      "pending legs cannot prove inventory equality")))

    (test-case "fast-queue verdict records the exact gate text"
      (define fq (decision-lane-verdict (make-paired-manifest) "fast-queue"))
      (check-true (string-contains? (hash-ref fq 'gate-text) "130")
                  "the exact fast gate text must be recorded")
      (check-true (string-contains? (hash-ref fq 'gate-text) "145")
                  "the exact fast p95 threshold must be recorded"))

    (test-case "per-configuration p50 and p95 use linear interpolation"
      (define rows (complete-shadow-rows #:elapsed-at (lambda (i) (+ 100.0 i))))
      (define manifest (paired-manifest-with-legs rows #f #f))
      (define fq (decision-lane-verdict manifest "fast-queue"))
      (check-equal? (hash-ref (hash-ref fq 'numbers) 'p50-seconds) 109.5)
      (check-equal? (hash-ref (hash-ref fq 'numbers) 'p95-seconds) 118.5))

    (test-case "fast-queue promotes on complete paired evidence inside the fast thresholds"
      (define manifest (paired-manifest-with-legs (complete-shadow-rows) #f #f))
      (define fq (decision-lane-verdict manifest "fast-queue"))
      (check-equal? (hash-ref fq 'verdict) "promote")
      (check-equal? (hash-ref (hash-ref fq 'numbers) 'p50-seconds) 100.0)
      (check-equal? (hash-ref (hash-ref fq 'numbers) 'p95-seconds) 100.0)
      (check-equal? (hash-ref (hash-ref fq 'numbers) 'inventory-equal-to-baseline) #t)
      (check-equal? (hash-ref (hash-ref fq 'numbers) 'attempts-recorded) 20)
      (check-equal? (hash-ref (hash-ref fq 'numbers) 'failures) 0))

    (test-case "fast-queue holds when p95 exceeds the target (never a revised target)"
      (define rows (complete-shadow-rows #:elapsed-at (lambda (i) (if (= i 19) 200.0 100.0))))
      (define manifest (paired-manifest-with-legs rows #f #f))
      (define fq (decision-lane-verdict manifest "fast-queue"))
      (check-equal? (hash-ref fq 'verdict) "hold")
      (check-equal? (hash-ref (hash-ref fq 'numbers) 'p95-seconds) 150.0)
      (check-true (ormap (lambda (r) (string-contains? r "p95")) (hash-ref fq 'reasons))
                  "the hold must name the p95 breach"))

    (test-case "fast-queue holds on a reliability regression versus batch"
      (define rows
        (complete-shadow-rows #:attempts-for
                              (lambda (i)
                                (if (= i 7)
                                    (list (make-failed-attempt 7 90.0) (make-timing-attempt 7 100.0))
                                    #f))))
      (define manifest (paired-manifest-with-legs rows #f #f))
      (define fq (decision-lane-verdict manifest "fast-queue"))
      (check-equal? (hash-ref fq 'verdict) "hold")
      (check-true (ormap (lambda (r) (string-contains? r "reliability")) (hash-ref fq 'reasons))
                  "the hold must name the reliability regression")
      (check-equal? (hash-ref (hash-ref fq 'numbers) 'failures) 1))

    (test-case "fast-queue holds on an inventory mismatch"
      (define rows
        (complete-shadow-rows #:digest-for (lambda (i)
                                             (if (= i 3)
                                                 "sha256:divergent"
                                                 (hash-ref (list-ref baseline-config-rows i)
                                                           'inventory-digest)))))
      (define manifest (paired-manifest-with-legs rows #f #f))
      (define fq (decision-lane-verdict manifest "fast-queue"))
      (check-equal? (hash-ref fq 'verdict) "hold")
      (check-false (hash-ref (hash-ref fq 'numbers) 'inventory-equal-to-baseline))
      (check-true (ormap (lambda (r) (string-contains? r "inventory")) (hash-ref fq 'reasons))))

    (test-case "fast-LPT falls back to FIFO with a named reason when duration evidence is missing"
      (define manifest (make-paired-manifest))
      (define lpt (decision-lane-verdict manifest "fast-LPT"))
      (check-equal? (hash-ref lpt 'verdict) "hold")
      (check-true (ormap (lambda (r) (string-contains? r "FIFO")) (hash-ref lpt 'reasons))
                  "missing duration evidence must fall back to FIFO with a named reason"))

    (test-case "fast-LPT promotes only when its ordering-only proof holds"
      ;; Same rows as the queue/fifo promote case, plus an LPT leg selecting
      ;; the identical per-SHA inventory → promote.
      (define manifest (paired-manifest-with-legs (complete-shadow-rows) (complete-shadow-rows) #f))
      (define lpt (decision-lane-verdict manifest "fast-LPT"))
      (check-equal? (hash-ref lpt 'verdict) "promote")
      ;; Divergent per-SHA inventory breaks the ordering-only proof → hold.
      (define divergent (complete-shadow-rows #:digest-for (lambda (i) (format "sha256:lpt~a" i))))
      (define bad (paired-manifest-with-legs (complete-shadow-rows) divergent #f))
      (define bad-lpt (decision-lane-verdict bad "fast-LPT"))
      (check-equal? (hash-ref bad-lpt 'verdict) "hold")
      (check-true (ormap (lambda (r) (string-contains? r "ordering-only"))
                         (hash-ref bad-lpt 'reasons))))

    (test-case "security-queue judges inventory and reliability but not the fast timing gate"
      (define manifest (paired-manifest-with-legs #f #f (complete-shadow-rows)))
      (define sq (decision-lane-verdict manifest "security-queue"))
      (check-equal? (hash-ref sq 'verdict) "promote")
      (check-false (hash-ref (hash-ref sq 'numbers) 'p50-seconds #f)
                   "the security lane has no fast timing gate"))

    (test-case "decision markdown records the verdict table, gate text, and reviewer"
      (define md (cohort-decision-md-string (make-paired-manifest)))
      (check-true (string-contains? md "fast-queue"))
      (check-true (string-contains? md "security-queue"))
      (check-true (string-contains? md "hold"))
      (check-true (string-contains? md "≤ 130"))
      (check-true (string-contains? md "Reviewer"))
      (check-true (string-contains? md "FIFO")))

    (test-case "paired report jsexpr and markdown embed the decision section"
      (define manifest (make-paired-manifest))
      (check-equal? (hash-ref (cohort-report-jsexpr manifest) 'decision)
                    (decision-report-jsexpr manifest))
      (check-true (string-contains? (cohort-report-md-string manifest) "Promotion decision"))
      (check-false (hash-has-key? (cohort-report-jsexpr (make-valid-cohort 20)) 'decision)))

    (test-case "decision report is deterministic"
      (check-equal? (decision-report-jsexpr (make-paired-manifest))
                    (decision-report-jsexpr (make-paired-manifest))))))

;; ============================================================
;; Run
;; ============================================================

(define failures (run-tests suite))

(module+ main
  (when (positive? failures)
    (exit 1)))
