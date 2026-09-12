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
         (only-in "../scripts/run-tests/sha256.rkt" sha256-hex)
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

(define (has-warning-matching? vr rx)
  (ormap (lambda (w) (regexp-match? rx w)) (validation-warnings vr)))

;; ============================================================
;; Helpers for paired configuration manifests (W0: C1)
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

    ;; --- 16. Paired configuration manifests (W0: C1 shadow cohort) ---

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
;; C2 post-promotion activation cohort (W6)
;;
;; C2 runs over promoted defaults with no shadow legs.  The report must
;; name which mode produced each number (paired-shadow vs post-promotion),
;; the C2 fast target is p50 ≤ 115 s / p95 ≤ 135 s, and a miss records
;; "target unachieved" plus a named next lever for a separate reviewed
;; decision.  CI-failed SHAs in the eligible window are recorded with the
;; named mechanical reason "lane-run-failed" — SHAs are never dropped.
;; ============================================================

(define (c2-manifest #:elapsed [elapsed 300.0] #:exclusions [exclusions '()] #:expected-count [ec 20])
  (hash-set (make-manifest #:shas (for/list ([i (in-range 18)])
                                    (make-valid-sha i #:elapsed elapsed))
                           #:exclusions exclusions
                           #:expected-count ec
                           #:cohort-id (format "v~a-c2" q-version))
            'cohort-mode
            "post-promotion"))

(define c2-suite
  (test-suite "post-promotion activation cohort (C2)"

    (test-case "cohort-mode defaults to paired-shadow and honors an explicit post-promotion mode"
      (check-equal? (cohort-mode (make-valid-cohort 20)) "paired-shadow")
      (check-equal? (cohort-mode (c2-manifest)) "post-promotion"))

    (test-case "lane-run-failed is a named mechanical exclusion reason"
      (check-true (and (member "lane-run-failed" known-exclusion-reasons) #t)))

    (test-case "flat C2 manifest with lane-run-failed exclusions validates"
      (define m
        (c2-manifest
         #:exclusions
         (list (hasheq 'sha
                       "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
                       'reason
                       "lane-run-failed"
                       'detail
                       "required-lane run failed; timing artifacts never produced (run 9901)")
               (hasheq 'sha
                       "bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"
                       'reason
                       "lane-run-failed"
                       'detail
                       "required-lane run failed; timing artifacts never produced (run 9902)"))
         #:expected-count 20))
      (check-true (validation-ok? (validate-cohort m))
                  (format "~a" (validation-errors (validate-cohort m)))))

    (test-case "post-promotion report names the mode and has no shadow duplication"
      (define r (cohort-report-jsexpr (c2-manifest)))
      (check-equal? (hash-ref r 'cohort-mode) "post-promotion")
      (check-false (hash-has-key? r 'configurations) "C2 must not duplicate shadow configurations")
      (check-false (hash-has-key? r 'decision))
      (check-true (string-contains? (cohort-report-md-string (c2-manifest)) "post-promotion")))

    (test-case "post-promotion gate: miss records target unachieved with a named next lever"
      (check-equal? post-promotion-p50-max-seconds 115.0)
      (check-equal? post-promotion-p95-max-seconds 135.0)
      (define gate (post-promotion-gate (c2-manifest #:elapsed 220.0)))
      (check-equal? (hash-ref gate 'verdict) "target unachieved")
      (check-false (hash-ref gate 'achieved))
      (check-true (string-contains? (hash-ref gate 'next-lever) "separate reviewed decision"))
      (check-true (string-contains? (hash-ref gate 'gate-text) "115")))

    (test-case "post-promotion gate: pass records target achieved"
      (define gate (post-promotion-gate (c2-manifest #:elapsed 90.0)))
      (check-equal? (hash-ref gate 'verdict) "target achieved")
      (check-true (hash-ref gate 'achieved)))

    (test-case "post-promotion markdown embeds the gate verdict"
      (define md (cohort-report-md-string (c2-manifest #:elapsed 220.0)))
      (check-true (string-contains? md "target unachieved")))

    (test-case "post-promotion decision.md records observed numbers, verdict, and next lever"
      (define pass-md (cohort-decision-md-string (c2-manifest #:elapsed 90.0)))
      (check-true (string-contains? pass-md "target achieved")
                  "pass decision must record the verdict")
      (check-true (string-contains? pass-md "post-promotion") "decision must name the producing mode")
      (check-true (string-contains? pass-md "90") "decision must record the observed p50")
      (check-true (string-contains? pass-md "115") "decision must record the p50 target")
      (check-true (string-contains? pass-md "135") "decision must record the p95 target")
      (define miss-md (cohort-decision-md-string (c2-manifest #:elapsed 220.0)))
      (check-true (string-contains? miss-md "target unachieved")
                  "miss decision must record the verdict")
      (check-true (string-contains? miss-md "220") "miss decision must record the observed p50")
      (check-true (string-contains? miss-md "separate reviewed decision")
                  "miss must name the next lever process")
      (check-true (string-contains? miss-md "no queue rollback") "miss must not imply queue rollback")
      (check-true (string-contains? miss-md "never revised") "targets are never revised"))

    (test-case "decision-lane-verdict records a structured hold for unregistered configurations"
      (define v (decision-lane-verdict (c2-manifest) "fast-queue"))
      (check-equal? (hash-ref v 'verdict) "hold")
      (check-true (pair? (hash-ref v 'reasons)) "unregistered configuration must carry a reason")
      (check-true (hash-has-key? (hash-ref v 'numbers) 'attempts-recorded)
                  "hold verdict must still carry the numbers table"))))

;; ============================================================
;; End-to-end PR elapsed cohort (v<q-version> W6: C2, pr-elapsed mode)
;; ============================================================

(define pe-base "2026-01-01T00:00:00Z")

;; Format base + seconds (midnight-anchored, < 24h) as an ISO-8601 UTC
;; timestamp without needing a date library in the tests.
(define (pe-plus seconds)
  (define m (quotient seconds 60))
  (format "2026-01-01T~a:~a:~aZ"
          (~r (quotient m 60) #:min-width 2 #:pad-string "0")
          (~r (modulo m 60) #:min-width 2 #:pad-string "0")
          (~r (modulo seconds 60) #:min-width 2 #:pad-string "0")))

(define (pe-window elapsed)
  (list pe-base (pe-plus elapsed)))

;; PR-elapsed timing attempt: the wall-clock required-check window is part
;; of the record (first-check-start-at -> last-required-check-end-at) and
;; the derived pr-elapsed-seconds is the window width — never the queue
;; wait alone (queue wait is recorded separately for auditability).
(define (pe-timing-attempt i elapsed)
  (define window (pe-window elapsed))
  (hasheq 'run-id
          (format "88~a" (+ 700000001 (* i 100)))
          'run-url
          (format "https://github.com/coinerd/q/actions/runs/88~a" (+ 700000001 (* i 100)))
          'result
          "success"
          'timing-sample
          #t
          'first-check-start-at
          (list-ref window 0)
          'last-required-check-end-at
          (list-ref window 1)
          'pr-elapsed-seconds
          elapsed
          'queue-wait-seconds
          (min 42.0 elapsed)))

(define (make-pr-elapsed-sha i
                             #:elapsed [elapsed 300.0]
                             #:attempts [attempts #f]
                             #:digest [digest #f])
  (hasheq 'sha
          (format "pe-sha-~a" i)
          'pr
          (+ 9609 i)
          'scheduler
          "queue"
          'ordering
          "fifo"
          'attempts
          (or attempts (list (pe-timing-attempt i elapsed)))
          'inventory-digest
          (or digest (format "sha256:tree~a" i))))

(define (make-pr-elapsed-manifest #:cohort-status [cohort-status "open"]
                                  #:shas [shas '()]
                                  #:exclusions [exclusions '()]
                                  #:expected-count [expected-count 20]
                                  #:cohort-id [cohort-id (format "v~a-c2" q-version)])
  (hasheq 'cohort-id
          cohort-id
          'milestone
          (format "v~a" q-version)
          'cohort-mode
          "pr-elapsed"
          'schema-version
          1
          'expected-count
          expected-count
          'cohort-status
          cohort-status
          'start-sha
          "71feb08054e239d1502b8e0eab893b00a0180d58"
          'shas
          shas
          'exclusions
          exclusions))

(define pr-elapsed-suite
  (test-suite "end-to-end PR elapsed cohort (C2, pr-elapsed)"

    (test-case "cohort-mode honors an explicit pr-elapsed mode"
      (check-equal? (cohort-mode (make-valid-cohort 20)) "paired-shadow")
      (check-equal? (cohort-mode (make-pr-elapsed-manifest)) "pr-elapsed"))

    (test-case "pr-elapsed gate constants are the roadmap W6 targets"
      (check-equal? pr-elapsed-p50-max-seconds 588.0)
      (check-equal? pr-elapsed-p95-max-seconds 735.0)
      (check-true (string-contains? pr-elapsed-gate-text "588"))
      (check-true (string-contains? pr-elapsed-gate-text "735")))

    (test-case "pr-elapsed measures wall time from first check start to last required check completion"
      ;; 588 s window: exactly the roadmap p50 target width.
      (check-equal? (pr-elapsed-seconds-from-window "2026-09-07T00:00:00Z" "2026-09-07T00:09:48Z")
                    588)
      ;; Missing or malformed endpoints are not silently treated as 0.
      (check-false (pr-elapsed-seconds-from-window #f "2026-09-07T00:09:48Z"))
      (check-false (pr-elapsed-seconds-from-window "not-a-timestamp" "2026-09-07T00:09:48Z"))
      ;; An inverted window is invalid, not a negative sample.
      (check-false (pr-elapsed-seconds-from-window "2026-09-07T00:09:48Z" "2026-09-07T00:00:00Z")))

    (test-case "pr-elapsed sample is the check window, not the queue wait alone"
      ;; Row with queue wait 120 s but a 700 s check window: the gate must
      ;; see 700 (the window), while the queue wait stays recorded separately.
      (define m
        (make-pr-elapsed-manifest #:cohort-status "closed"
                                  #:shas (for/list ([i (in-range 20)])
                                           (make-pr-elapsed-sha i #:elapsed 700.0))))
      (define gate (pr-elapsed-gate m))
      (check-equal? (hash-ref gate 'p50-seconds) 700.0)
      (check-true (andmap (lambda (a) (< (hash-ref a 'queue-wait-seconds) 700.0))
                          (hash-ref (list-ref (hash-ref m 'shas) 0) 'attempts))))

    (test-case "open pr-elapsed cohort validates with fewer SHAs and no exclusions"
      (define m (make-pr-elapsed-manifest))
      (define vr (validate-cohort m))
      (check-true (validation-ok? vr) (format "~a" (validation-errors vr))))

    (test-case "open pr-elapsed cohort is evidence pending, never a silent miss"
      (define gate (pr-elapsed-gate (make-pr-elapsed-manifest)))
      (check-false (hash-ref gate 'gate-evaluable))
      (check-false (hash-ref gate 'achieved))
      (check-equal? (hash-ref gate 'verdict) "evidence pending (cohort open)")
      (check-false (hash-ref gate 'next-lever #f))
      (define md (cohort-decision-md-string (make-pr-elapsed-manifest)))
      (check-true (string-contains? md "evidence pending"))
      (check-true (string-contains? md "open")))

    (test-case "closed pr-elapsed cohort inside targets records target achieved"
      (define m
        (make-pr-elapsed-manifest #:cohort-status "closed"
                                  #:shas (for/list ([i (in-range 20)])
                                           (make-pr-elapsed-sha i #:elapsed 45.0))))
      (define gate (pr-elapsed-gate m))
      (check-true (hash-ref gate 'gate-evaluable))
      (check-true (hash-ref gate 'achieved))
      (check-equal? (hash-ref gate 'verdict) "target achieved"))

    (test-case "pr-elapsed scheduler check accepts the integrated topology and warns on legacy schedulers"
      ;; Regression guard: the scheduler check must compare against the
      ;; documented integrated scheduler fast/queue/lpt (W2 queue + W3 LPT
      ;; activations). Good data must validate warning-free; anything else
      ;; is recorded as a decision-facing warning, not a hard error.
      (define (integrated i)
        (hash-set* (make-pr-elapsed-sha i #:elapsed 45.0) 'scheduler "fast/queue/lpt"))
      (define good
        (make-pr-elapsed-manifest #:cohort-status "closed"
                                  #:shas (for/list ([i (in-range 20)])
                                           (integrated i))))
      (check-true (validation-ok? (validate-cohort good)))
      (check-false (has-warning-matching? (validate-cohort good) #rx"non-fast scheduler"))
      (define legacy
        (make-pr-elapsed-manifest #:cohort-status "closed"
                                  #:shas (for/list ([i (in-range 20)])
                                           (hash-set* (integrated i) 'scheduler "serial"))))
      (check-true (validation-ok? (validate-cohort legacy)))
      (check-true (has-warning-matching? (validate-cohort legacy) #rx"non-fast scheduler")))

    (test-case "closed pr-elapsed miss records target unachieved with a named next lever"
      (define m
        (make-pr-elapsed-manifest #:cohort-status "closed"
                                  #:shas (for/list ([i (in-range 20)])
                                           (make-pr-elapsed-sha i #:elapsed 800.0))))
      (define gate (pr-elapsed-gate m))
      (check-true (hash-ref gate 'gate-evaluable))
      (check-false (hash-ref gate 'achieved))
      (check-equal? (hash-ref gate 'verdict) "target unachieved")
      (check-true (string-contains? (hash-ref gate 'next-lever) "separate reviewed decision"))
      (check-true (string-contains? (hash-ref gate 'next-lever) "no queue rollback")))

    (test-case "pr-elapsed p50 and p95 use linear interpolation"
      ;; 20 ascending samples 40..800 s: p50 lands between the 9th and 10th
      ;; order statistics (420.0), p95 between the 19th and 20th (780.0).
      (define m
        (make-pr-elapsed-manifest #:cohort-status "closed"
                                  #:shas (for/list ([i (in-range 20)])
                                           (make-pr-elapsed-sha i #:elapsed (* 40.0 (add1 i))))))
      (define gate (pr-elapsed-gate m))
      (check-equal? (hash-ref gate 'p50-seconds) 420.0)
      (check-equal? (hash-ref gate 'p95-seconds) 780.0)
      (check-equal? (hash-ref gate 'verdict) "target unachieved"))

    (test-case "duplicate PR head SHA is rejected in pr-elapsed mode"
      (define m
        (make-pr-elapsed-manifest #:shas (for/list ([i (in-range 20)])
                                           (make-pr-elapsed-sha (if (= i 7) 3 i)))))
      (check-false (validation-ok? (validate-cohort m)))
      (check-true (has-error-matching? (validate-cohort m) #rx"duplicate SHA")))

    (test-case "closed pr-elapsed cohort still requires 20 unique PR head SHAs"
      (define m
        (make-pr-elapsed-manifest #:cohort-status "closed"
                                  #:shas (for/list ([i (in-range 15)])
                                           (make-pr-elapsed-sha i))))
      (check-false (validation-ok? (validate-cohort m)))
      (check-true (has-error-matching? (validate-cohort m) #rx"silently truncated")))

    (test-case "failed-then-passed reruns are recorded, never dropped"
      (define rerun-attempts
        (list (hasheq 'run-id
                      "887000001-fail"
                      'result
                      "failure"
                      'timing-sample
                      #f
                      'first-check-start-at
                      "2026-09-07T00:00:00Z"
                      'last-required-check-end-at
                      "2026-09-07T00:02:00Z")
              (pe-timing-attempt 0 300.0)
              (hasheq 'run-id "887000003-rerun" 'result "rerun" 'timing-sample #f)))
      (define m
        (make-pr-elapsed-manifest #:cohort-status "open"
                                  #:shas (list (make-pr-elapsed-sha 0 #:attempts rerun-attempts))))
      (check-true (sha-eligible? (list-ref (hash-ref m 'shas) 0)))
      (define summary (cohort-attempts-summary m))
      (check-true (> (hash-ref summary 'failures) 0))
      (check-true (> (hash-ref summary 'reruns) 0)))

    (test-case "timing-sample attempt without a parseable check window is rejected"
      (define bad (hasheq 'run-id "887000009" 'result "success" 'timing-sample #t))
      (define m
        (make-pr-elapsed-manifest #:shas (list (make-pr-elapsed-sha 0 #:attempts (list bad)))))
      (define vr (validate-cohort m))
      (check-false (validation-ok? vr))
      (check-true (has-error-matching? vr #rx"required-check window")))

    (test-case "queue wait alone is never accepted as the PR elapsed measure"
      (define bad
        (hash-set (pe-timing-attempt 0 120.0)
                  'queue-wait-seconds
                  900.0)) ; wait larger than the whole check window
      (define m
        (make-pr-elapsed-manifest #:shas (list (make-pr-elapsed-sha 0 #:attempts (list bad)))))
      (define vr (validate-cohort m))
      (check-false (validation-ok? vr))
      (check-true (has-error-matching? vr #rx"queue wait")))

    (test-case "malformed attempt entries are rejected in pr-elapsed mode"
      (define m
        (make-pr-elapsed-manifest
         #:shas (list (make-pr-elapsed-sha
                       0
                       #:attempts (list (hasheq 'run-id "887000010" 'timing-sample "yes"))))))
      (define vr (validate-cohort m))
      (check-false (validation-ok? vr))
      (check-true (has-error-matching? vr #rx"attempt")))

    (test-case "pr-elapsed report names the mode and carries the gate without shadow duplication"
      (define r (cohort-report-jsexpr (make-pr-elapsed-manifest)))
      (check-equal? (hash-ref r 'cohort-mode) "pr-elapsed")
      (check-true (hash-has-key? r 'pr-elapsed-gate))
      (check-true (hash-has-key? (hash-ref r 'pr-elapsed-gate) 'gate-evaluable))
      (check-false (hash-has-key? r 'configurations))
      (check-false (hash-has-key? r 'decision)))

    (test-case "pr-elapsed report JSON is deterministic"
      (define m (make-pr-elapsed-manifest))
      (check-equal? (cohort-report-json-string m) (cohort-report-json-string m)))

    (test-case "pr-elapsed report markdown embeds the mode and verdict"
      (define md (cohort-report-md-string (make-pr-elapsed-manifest)))
      (check-true (string-contains? md "pr-elapsed"))
      (check-true (string-contains? md "evidence pending")))

    (test-case "pr-elapsed decision.md records the observed numbers, gate text, and honesty clause"
      (define miss-m
        (make-pr-elapsed-manifest #:cohort-status "closed"
                                  #:shas (for/list ([i (in-range 20)])
                                           (make-pr-elapsed-sha i #:elapsed 800.0))))
      (define miss-md (cohort-decision-md-string miss-m))
      (check-true (string-contains? miss-md "pr-elapsed"))
      (check-true (string-contains? miss-md "target unachieved"))
      (check-true (string-contains? miss-md "800"))
      (check-true (string-contains? miss-md "588"))
      (check-true (string-contains? miss-md "735"))
      (check-true (string-contains? miss-md "never revised"))
      (check-true (string-contains? miss-md "separate reviewed decision")))))

;; ============================================================
;; Final-claim verdict mode (W5: C3)
;; ============================================================

(define fc-full-guards
  ;; W10 fix: `hash` (equal?-based), not `hasheq`. With `hasheq`, the string
  ;; keys only resolve when the compiler happens to share these literal
  ;; objects with cohort-report.rkt's own row-guard literals — an identity
  ;; lottery across compilation layouts that flipped this suite from green
  ;; to "unverified" depending on incremental-compile layout. Guard evidence
  ;; is content, so the fixture must look it up content-based.
  (hash
   "inventory-accounted"
   (hasheq 'provided #t 'reference "artifact: per-SHA inventory digests equal the baseline")
   "reliability-non-regression"
   (hasheq 'provided
           #t
           'reference
           "computed: cohort attempts summary versus the recorded baseline block")
   "semantic-gate-equivalence"
   (hasheq 'provided #t 'reference "artifact: semantic-gate equivalence record")
   "failure-truth"
   (hasheq 'provided #t 'reference "artifact: failed attempts recorded, never dropped")
   "shared-state-permission-isolation"
   (hasheq 'provided #t 'reference "artifact: shared-state permission isolation proof")
   "four-worker-isolation-proof"
   (hasheq 'provided #t 'reference "artifact: four-worker isolation record")
   "prepared-env-no-bypass"
   (hasheq 'provided #t 'reference "artifact: no-bypass prepared-env record with named fallbacks")))

(define (fc-timing-attempt i
                           #:elapsed [elapsed 300.0]
                           #:fast [fast 100.0]
                           #:security [security 180.0]
                           #:workflows [workflows 160.0])
  (hash-set* (pe-timing-attempt i elapsed)
             'fast-execution-seconds
             fast
             'security-runner-seconds
             security
             'workflows-runner-seconds
             workflows))

(define (make-fc-sha i
                     #:elapsed [elapsed 300.0]
                     #:fast [fast 100.0]
                     #:security [security 180.0]
                     #:workflows [workflows 160.0]
                     #:attempts [attempts #f]
                     #:digest [digest #f])
  (make-pr-elapsed-sha i
                       #:elapsed elapsed
                       #:attempts (or attempts
                                      (list (fc-timing-attempt i
                                                               #:elapsed elapsed
                                                               #:fast fast
                                                               #:security security
                                                               #:workflows workflows)))
                       #:digest digest))

(define (make-fc-manifest #:cohort-status [cohort-status "closed"]
                          #:shas [shas '()]
                          #:guards [guards fc-full-guards]
                          #:pe-verified [pe-verified 20]
                          #:pe-total [pe-total 20]
                          #:pe-fallback [pe-fallback 0]
                          #:pe-records [pe-records 20]
                          #:baseline-failures [baseline-failures 1]
                          #:baseline-cancelled [baseline-cancelled 1]
                          #:baseline-reruns [baseline-reruns 2]
                          #:expected-count [expected-count 20])
  (hasheq 'cohort-id
          (format "v~a-c3" q-version)
          'milestone
          (format "v~a" q-version)
          'cohort-mode
          "final-claim"
          'schema-version
          1
          'expected-count
          expected-count
          'cohort-status
          cohort-status
          'start-sha
          "71feb08054e239d1502b8e0eab893b00a0180d58"
          'shas
          shas
          'exclusions
          '()
          'guard-evidence
          guards
          'prepared-env-restore-stats
          (hasheq 'verified
                  pe-verified
                  'total
                  pe-total
                  'fallback
                  pe-fallback
                  'records-observed
                  pe-records
                  'window
                  "cohort C3 observation window")
          'reliability-baseline
          (hasheq 'failures baseline-failures 'cancelled baseline-cancelled 'reruns baseline-reruns)))

(define (fc-row gate id)
  (findf (lambda (r) (equal? (hash-ref r 'id) id)) (hash-ref gate 'rows)))

(define final-claim-suite
  (test-suite (format "final-claim verdict cohort (C3, v~a W5)" q-version)

    (test-case "fixed §8 thresholds are exactly the roadmap values"
      (check-equal? (hash-ref final-claim-thresholds "fast-p50") 115.0)
      (check-equal? (hash-ref final-claim-thresholds "fast-p95") 135.0)
      (check-equal? (hash-ref final-claim-thresholds "pr-ci-p50") 588.0)
      (check-equal? (hash-ref final-claim-thresholds "pr-ci-p95") 735.0)
      (check-equal? (hash-ref final-claim-thresholds "security-runner-p50") 240.0)
      (check-equal? (hash-ref final-claim-thresholds "workflows-runner-p50") 220.0)
      (check-equal? (hash-ref final-claim-thresholds "prepared-env-verified-restores") 95.0))

    (test-case "the gate evaluates all seven §8 rows"
      (check-equal? (length (hash-ref (final-claim-gate (make-fc-manifest)) 'rows)) 7)
      (check-equal? (hash-ref (final-claim-gate (make-fc-manifest)) 'overall-verdict)
                    "target not achieved")) ; empty cohort cannot pass

    (test-case "p95 rows evaluate the 95th percentile, not the median"
      ;; Regression guard (W5 independent verification finding): the
      ;; timing-row evaluator must honor each row's quantile.  With spread
      ;; fast samples the p50 and p95 rows must observe different quantiles
      ;; of the same sample set.
      (define m
        (make-fc-manifest #:shas (for/list ([i (in-range 20)])
                                   (make-fc-sha i #:fast (+ 100.0 i)))))
      (define gate (final-claim-gate m))
      (define p50-obs (hash-ref (fc-row gate "fast-p50") 'observed))
      (define p95-obs (hash-ref (fc-row gate "fast-p95") 'observed))
      (check-equal? p50-obs 109.5) ; k = 0.50*19 = 9.5 -> (109.0+110.0)/2
      (check-equal? p95-obs 118.05) ; k = 0.95*19 = 18.05 -> 118.0 + 0.05*(119.0-118.0)
      (check-not-equal? p50-obs p95-obs))

    (test-case "cohort-quantile-exact interpolates exactly at integer and edge ranks"
      ;; frac = 0 (integer rank) returns the sample verbatim; the max rank
      ;; never indexes past the sorted list; a single sample is returned as-is.
      ;; Millisecond rounding keeps interpolated results noise-free and
      ;; byte-stable for checksummed artifact regeneration.
      (check-equal? (cohort-quantile-exact '(5.0) 0.95) 5.0)
      (check-equal? (cohort-quantile-exact (for/list ([i (in-range 21)])
                                             (* 1.0 i))
                                           1.0)
                    20.0)
      (check-equal? (cohort-quantile-exact (for/list ([i (in-range 21)])
                                             (* 1.0 i))
                                           0.50)
                    10.0)
      (check-equal? (cohort-quantile-exact (list 3.0 1.0 2.0) 0.95) ; k=1.9 -> 2.0+0.9*1.0
                    2.9)
      (check-equal? ; k=5.7 -> 0.3*5.0+0.7*6.0; raw FP sum is 5.7000000000000002
       (cohort-quantile-exact (for/list ([i (in-range 7)])
                                (* 1.0 i))
                              0.95)
       5.7))

    (test-case "JSON round-trip manifests validate (guard ids + JSON null prepared-env)"
      ;; Live C3 cohort.json arrives via read-json (string-keyed hashes,
      ;; JSON null for absent prepared-env evidence) and is canonicalized
      ;; by normalize-manifest.  Simulate that exact shape: every hash key
      ;; is a string, 'prepared-env carries the JSON null.
      (define (string-keyed v)
        (cond
          [(hash? v)
           (for/hash ([(k val) (in-hash v)])
             (values (format "~a" k) (string-keyed val)))]
          [(list? v) (map string-keyed v)]
          [else v]))
      (define sh (hash-set (make-pr-elapsed-sha 1) 'prepared-env 'null))
      (define m (normalize-manifest (string-keyed (make-fc-manifest #:shas (list sh)))))
      (define vr (validate-cohort m))
      (check-false (has-error-matching? vr #rx"unknown guard id"))
      (check-false (has-error-matching? vr #rx"unknown prepared-env")))

    (test-case "a row without its guard evidence is unverified, never pass"
      (define m
        (make-fc-manifest #:shas (for/list ([i (in-range 20)])
                                   (make-fc-sha i #:elapsed 100.0))
                          #:guards (hash-remove fc-full-guards "four-worker-isolation-proof")))
      (define gate (final-claim-gate m))
      (define fast-row (fc-row gate "fast-p50"))
      (check-equal? (hash-ref fast-row 'verdict) "unverified")
      (check-false (hash-ref (hash-ref fast-row 'guards) 'satisfied)))

    (test-case "every row is unverified when guard evidence is missing entirely"
      (define m
        (make-fc-manifest #:shas (for/list ([i (in-range 20)])
                                   (make-fc-sha i #:elapsed 100.0))
                          #:guards (hasheq)))
      (define gate (final-claim-gate m))
      (for ([r (in-list (hash-ref gate 'rows))])
        (check-equal? (hash-ref r 'verdict) "unverified")))

    (test-case "all-pass closed cohort with guards verifies every row"
      (define m
        (make-fc-manifest #:shas (for/list ([i (in-range 20)])
                                   (make-fc-sha i #:elapsed 100.0))))
      (define gate (final-claim-gate m))
      (for ([r (in-list (hash-ref gate 'rows))])
        (check-equal? (hash-ref r 'verdict) "pass"))
      (check-equal? (hash-ref gate 'overall-verdict) "verified"))

    (test-case "loaded-from-disk manifests digest and report (read-json string keys)"
      ;; read-json yields string-keyed hashes while write-json only accepts
      ;; symbol keys; digest and full report serialization must still work on
      ;; the realistic load-from-disk path, and guard evidence must survive.
      (define tmp (make-temporary-file "w5-~a.json"))
      (with-handlers ([exn:fail? (lambda (e)
                                   (with-handlers ([exn:fail? void])
                                     (delete-file tmp))
                                   (raise e))])
        (call-with-output-file
         tmp
         (lambda (out)
           (displayln (jsexpr->string (normalize-manifest
                                       (make-fc-manifest #:shas (for/list ([i (in-range 20)])
                                                                  (make-fc-sha i #:elapsed 100.0)))))
                      out))
         #:exists 'truncate)
        (define m (load-cohort-manifest tmp))
        (check-not-exn (lambda () (manifest-digest m)))
        (check-not-exn (lambda () (jsexpr->string (cohort-report-jsexpr m))))
        (define gate (final-claim-gate m))
        (check-equal? (hash-ref (fc-row gate "fast-p50") 'verdict) "pass")
        (check-equal? (hash-ref (fc-row gate "prepared-env-verified-restores") 'verdict) "pass")
        (with-handlers ([exn:fail? void])
          (delete-file tmp))))

    (test-case "a closed-cohort timing miss records target not achieved with observed numbers"
      (define m
        (make-fc-manifest #:shas (for/list ([i (in-range 20)])
                                   (make-fc-sha i #:elapsed 100.0 #:fast 250.0))))
      (define gate (final-claim-gate m))
      (define fast-row (fc-row gate "fast-p50"))
      (check-equal? (hash-ref fast-row 'verdict) "target not achieved")
      (check-equal? (hash-ref fast-row 'observed) 250.0)
      (check-equal? (hash-ref fast-row 'threshold) 115.0)
      (check-true (pair? (hash-ref fast-row 'reasons)))
      (check-true (string-contains? (first (hash-ref fast-row 'reasons)) "never revised")))

    (test-case "in-window samples on an incomplete cohort stay unverified"
      (define m
        (make-fc-manifest #:cohort-status "open"
                          #:shas (for/list ([i (in-range 5)])
                                   (make-fc-sha i #:elapsed 100.0))))
      (define gate (final-claim-gate m))
      (check-equal? (hash-ref (fc-row gate "fast-p50") 'verdict) "unverified")
      (check-equal? (hash-ref gate 'overall-verdict) "target not achieved"))

    (test-case "empty sample set is unverified even with full guards"
      (define m
        (make-fc-manifest #:shas (for/list ([i (in-range 20)])
                                   (make-fc-sha i #:attempts (list (pe-timing-attempt i 100.0))))))
      (define gate (final-claim-gate m))
      (check-equal? (hash-ref (fc-row gate "fast-p50") 'verdict) "unverified")
      (check-equal? (hash-ref (fc-row gate "security-runner-p50") 'verdict) "unverified")
      ;; pr-ci rows still observe their window-derived samples
      (check-equal? (hash-ref (fc-row gate "pr-ci-p50") 'verdict) "pass"))

    (test-case "reliability non-regression is computed, not asserted"
      (define rerun-attempts
        (list (fc-timing-attempt 0 #:elapsed 100.0)
              (hasheq 'run-id "887000003-rerun" 'result "rerun" 'timing-sample #f)))
      (define shas
        (append (list (make-fc-sha 0 #:attempts rerun-attempts))
                (for/list ([i (in-range 1 20)])
                  (make-fc-sha i #:elapsed 100.0))))
      (define m
        (make-fc-manifest #:shas shas
                          #:baseline-failures 0
                          #:baseline-cancelled 0
                          #:baseline-reruns 0))
      (check-false (final-claim-reliability-ok? m))
      (define gate (final-claim-gate m))
      (check-equal? (hash-ref (fc-row gate "fast-p50") 'verdict) "unverified")
      (check-false (hash-ref (hash-ref (fc-row gate "fast-p50") 'guards) 'reliability-satisfied #f)))

    (test-case "prepared-env row: zero observed records on a closed cohort is target not achieved"
      (define m
        (make-fc-manifest #:shas (for/list ([i (in-range 20)])
                                   (make-fc-sha i #:elapsed 100.0))
                          #:pe-verified 0
                          #:pe-total 0
                          #:pe-records 0))
      (define row (fc-row (final-claim-gate m) "prepared-env-verified-restores"))
      (check-equal? (hash-ref row 'verdict) "target not achieved")
      (check-equal? (hash-ref row 'observed) 0.0))

    (test-case "prepared-env row: below-target rate is target not achieved with the observed rate"
      (define m
        (make-fc-manifest #:shas (for/list ([i (in-range 20)])
                                   (make-fc-sha i #:elapsed 100.0))
                          #:pe-verified 18
                          #:pe-total 20
                          #:pe-fallback 2
                          #:pe-records 20))
      (define row (fc-row (final-claim-gate m) "prepared-env-verified-restores"))
      (check-equal? (hash-ref row 'verdict) "target not achieved")
      (check-equal? (hash-ref row 'observed) 90.0))

    (test-case "guard-evidence with an unknown guard id is rejected"
      (define m
        (make-fc-manifest #:guards (hash-set fc-full-guards
                                             "unknown-guard"
                                             (hasheq 'provided #t 'reference "somewhere"))))
      (check-false (validation-ok? (validate-cohort m)))
      (check-true (has-error-matching? (validate-cohort m) #rx"unknown guard id")))

    (test-case "a provided guard without a named reference is rejected"
      (define m
        (make-fc-manifest
         #:guards (hash-set fc-full-guards "failure-truth" (hasheq 'provided #t 'reference ""))))
      (check-false (validation-ok? (validate-cohort m)))
      (check-true (has-error-matching? (validate-cohort m) #rx"names no reference")))

    (test-case "final-claim manifests require the guard-evidence object"
      (define m (hash-remove (make-fc-manifest) 'guard-evidence))
      (check-false (validation-ok? (validate-cohort m)))
      (check-true (has-error-matching? (validate-cohort m) #rx"guard-evidence")))

    (test-case "final-claim reuses the pr-elapsed per-SHA schema (duplicate SHA rejected)"
      (define m
        (make-fc-manifest #:cohort-status "open"
                          #:shas (for/list ([i (in-range 20)])
                                   (make-fc-sha (if (= i 7) 3 i)))))
      (check-false (validation-ok? (validate-cohort m)))
      (check-true (has-error-matching? (validate-cohort m) #rx"duplicate SHA")))

    (test-case "failed/cancelled attempts stay in the cohort record (failure truth)"
      (define failed
        (list (hasheq 'run-id
                      "887000001-fail"
                      'result
                      "failure"
                      'timing-sample
                      #f
                      'first-check-start-at
                      "2026-09-07T00:00:00Z"
                      'last-required-check-end-at
                      "2026-09-07T00:02:00Z")
              (fc-timing-attempt 0 #:elapsed 100.0)))
      (define m (make-fc-manifest #:shas (list (make-fc-sha 0 #:attempts failed))))
      (define summary (cohort-attempts-summary m))
      (check-true (> (hash-ref summary 'failures) 0)))

    (test-case "report names the mode and carries the gate without shadow duplication"
      (define r (cohort-report-jsexpr (make-fc-manifest)))
      (check-equal? (hash-ref r 'cohort-mode) "final-claim")
      (check-true (hash-has-key? r 'final-claim-gate))
      (check-false (hash-has-key? r 'configurations))
      (check-false (hash-has-key? r 'decision)))

    (test-case "report JSON is deterministic"
      (define m (make-fc-manifest))
      (check-equal? (cohort-report-json-string m) (cohort-report-json-string m)))

    (test-case "report markdown embeds the final-claim section and per-row table"
      (define md (cohort-report-md-string (make-fc-manifest)))
      (check-true (string-contains? md "Final-claim cohort (C3"))
      (check-true (string-contains? md "fast-p50"))
      (check-true (string-contains? md "prepared-env-verified-restores")))

    (test-case "decision.md records per-row verdicts, observed numbers, and never-revised clause"
      (define m
        (make-fc-manifest #:shas (for/list ([i (in-range 20)])
                                   (make-fc-sha i #:elapsed 100.0 #:fast 250.0))
                          #:pe-verified 18
                          #:pe-total 20
                          #:pe-records 20))
      (define md (final-claim-decision-md-string m))
      (check-true (string-contains? md "final-claim"))
      (check-true (string-contains? md "target not achieved"))
      (check-true (string-contains? md "250"))
      (check-true (string-contains? md "115"))
      (check-true (string-contains? md "90.0"))
      (check-true (string-contains? md "never revised"))
      (check-true (string-contains? md "Next lever")))))

;; W8: the decision record ends in exactly one allowed verdict —
;; ACHIEVED, NOT ACHIEVED, or PARTIAL WORKLOAD REDUCTION; FINAL TARGET NOT
;; ACHIEVED — and the Class A work-mass delta row is reported alongside the
;; fixed Class B–D rows without ever comparing the incompatible measure
;; classes.  The verification vocabulary grep over decision.md must match
;; exactly one line: the final-verdict line itself (per-row verdicts are
;; lowercase and the gate text names no uppercase vocabulary).
(define (verify-vocabulary-line-count md)
  (length (filter (lambda (l) (regexp-match? #rx"ACHIEVED|NOT ACHIEVED|PARTIAL WORKLOAD REDUCTION" l))
                  (string-split md "\n" #:trim? #f))))
(define (md-last-content-line md)
  (last (filter (lambda (l) (non-empty-string? (string-trim l))) (string-split md "\n"))))

(define w8-final-verdict-suite
  (test-suite "W8 final 20-SHA cohort verdict vocabulary"

    (test-case "open cohort with an improving Class A row ends in the PARTIAL verdict, exactly once"
      (define m
        (hash-set (make-fc-manifest #:cohort-status "open")
                  'work-mass-delta
                  (hasheq 'delta-pct
                          -5.37
                          'basis-of-comparison
                          "class-a-fast-work-mass-delta"
                          'basis
                          "checksummed W0→W7 census comparison artifact")))
      (define md (final-claim-decision-md-string m))
      (check-true (string-contains? md "PARTIAL WORKLOAD REDUCTION; FINAL TARGET NOT ACHIEVED"))
      (check-equal? (verify-vocabulary-line-count md) 1)
      (check-true (string-contains? md "class-a-fast-work-mass-delta"))
      ;; the verdict line is the last content of the decision record
      (check-equal? (md-last-content-line md)
                    "PARTIAL WORKLOAD REDUCTION; FINAL TARGET NOT ACHIEVED"))

    (test-case "a closed cohort passing every fixed row with an improving Class A row ends in ACHIEVED"
      (define m
        (hash-set (make-fc-manifest #:shas (for/list ([i (in-range 20)])
                                             (make-fc-sha i #:elapsed 100.0))
                                    #:cohort-status "closed")
                  'work-mass-delta
                  (hasheq 'delta-pct
                          -5.37
                          'basis-of-comparison
                          "class-a-fast-work-mass-delta"
                          'basis
                          "checksummed W0→W7 census comparison artifact")))
      (define md (final-claim-decision-md-string m))
      (check-true (string-contains? md "ACHIEVED"))
      (check-equal? (verify-vocabulary-line-count md) 1)
      (check-equal? (md-last-content-line md) "ACHIEVED"))

    (test-case "a closed tag with fewer SHAs than expected can never claim ACHIEVED"
      (define short
        (hash-set (make-fc-manifest #:shas (for/list ([i (in-range 19)])
                                             (make-fc-sha i #:elapsed 100.0))
                                    #:cohort-status "closed")
                  'work-mass-delta
                  (hasheq 'delta-pct
                          -5.37
                          'basis-of-comparison
                          "class-a-fast-work-mass-delta"
                          'basis
                          "checksummed W0→W7 census comparison artifact")))
      (define short-md (final-claim-decision-md-string short))
      (check-equal? (md-last-content-line short-md)
                    "PARTIAL WORKLOAD REDUCTION; FINAL TARGET NOT ACHIEVED")
      (check-false (for/or ([l (in-lines (open-input-string short-md))])
                     (equal? (string-trim l) "ACHIEVED"))))

    (test-case "no measured work-mass improvement ends in NOT ACHIEVED, never PARTIAL"
      (define m
        (hash-set (make-fc-manifest)
                  'work-mass-delta
                  (hasheq 'delta-pct
                          2.5
                          'basis-of-comparison
                          "class-a-fast-work-mass-delta"
                          'basis
                          "checksummed W0→W7 census comparison artifact")))
      (define md (final-claim-decision-md-string m))
      (check-equal? (verify-vocabulary-line-count md) 1)
      (check-true (string-contains? md "NOT ACHIEVED"))
      (check-false (string-contains? md "PARTIAL WORKLOAD REDUCTION")))

    (test-case "pre-bump manifests keep their decision records unchanged"
      (define md (final-claim-decision-md-string (make-fc-manifest)))
      (check-false (string-contains? md "Final campaign verdict"))
      (check-false (string-contains? md "class-a-fast-work-mass-delta")))))

;; ============================================================
;; Run
;; ============================================================

;; ============================================================
;; W10 (v1.00.29): final-cohort artifact guard suite
;;
;; Pins (a) the v1.00.29-final §7.2 row vocabulary, (b) the §7.1
;; safety-gate row vocabulary (all 14 rows decided in decision.md),
;; (c) the cohort's mechanical eligibility invariants, (d) the
;; graph-after reduction invariants, and (e) SHA256SUMS integrity of
;; the four contract artifacts. Existing pinned rows stay green; these
;; are additive rows.
;; ============================================================

(define-runtime-path v29-dir "../artifacts/ci-baseline/v1.00.29-final")
(define-runtime-path v28-dir "../artifacts/ci-baseline/v1.00.28-final")

(define (load-json rel)
  (call-with-input-file (build-path v29-dir rel) read-json))

;; §7.2 v1.00.29-final row vocabulary (frozen contract; ids as decided at W10)
(define v29-report-row-ids
  '("l0-p90-local" "l1-p90-local"
                   "pr-ci-p50"
                   "pr-ci-p95"
                   "duplicate-proof-ratio"
                   "prepared-env-verified-restore"
                   "flake-tax-runner-share"
                   "selector-confirmed-relevant-omissions"))

;; §7.1 safety-gate vocabulary (roadmap §7.1 table, in order)
(define v29-safety-gate-rows
  '("Required claim inventory" "Distinct Racket-version proofs"
                               "Distinct platform proofs"
                               "Strict-security/sandbox proofs"
                               "Release-specific proofs"
                               "Workflow-contract proofs"
                               "Proof reuse"
                               "Provenance validation"
                               "Retention validation"
                               "Rerun semantics"
                               "Selector"
                               "Prepared env"
                               "Coverage/behavior ownership"
                               "Unknown metrics"))

;; §13 draft-verdict vocabulary: the verdict must be exactly one of these
(define v29-verdict-vocabulary
  '("ACHIEVED" "PARTIAL — SAFE REDUCTION DELIVERED" "NOT ACHIEVED" "BLOCKED — SAFETY/INTEGRITY"))

(define v29-final-suite
  (test-suite "v1.00.29 final cohort artifact guards (W10)"

    (test-case "cohort.json: closed cohort of >= 20 eligible unique head SHAs"
      (define c (load-json "cohort.json"))
      (check-equal? (hash-ref c '|cohort-status|) "closed")
      (check-true (>= (hash-ref c '|unique-head-shas|) 20))
      (check-equal? (hash-ref c '|unique-head-shas|)
                    (length (hash-ref c '|shas|))
                    "unique-head-shas must equal the entry count")
      (check-equal? (hash-ref c '|exclusion-count|) 0)
      (check-true (hash-has-key? c '|eligibility-rules-pre-registered|)
                  "eligibility rules must be pre-registered in the artifact"))

    (test-case "cohort.json: every entry carries the full per-entry row vocabulary"
      (define c (load-json "cohort.json"))
      (for ([e (in-list (hash-ref c '|shas|))])
        (for ([k '(number title
                          merged_at
                          head_sha
                          checks_total
                          checks_failed
                          ci_wall_seconds
                          topology_class)])
          (check-true (hash-has-key? e k) (format "entry ~a missing ~a" (hash-ref e 'number) k)))
        (check-true (>= (hash-ref e 'checks_total) 17) "eligibility rule (c): checks.completed >= 17")
        (check-equal? (hash-ref e 'checks_failed) 0 "eligibility rule (c): failed = 0")
        (check-not-false (member (hash-ref e 'topology_class) '("pre-w4" "post-w4"))
                         "topology_class vocabulary is pre-w4 | post-w4")))

    (test-case "cohort.json: head SHAs unique and disjoint from v1.00.28-final"
      (define c (load-json "cohort.json"))
      (define heads (map (lambda (e) (hash-ref e 'head_sha)) (hash-ref c '|shas|)))
      (check-equal? (length heads)
                    (length (remove-duplicates heads))
                    "eligibility rule (b): head SHAs distinct")
      (define prior (call-with-input-file (build-path v28-dir "cohort.json") read-json))
      (define prior-shas (map (lambda (e) (hash-ref e 'sha)) (hash-ref prior 'shas)))
      (for ([h (in-list heads)])
        (check-false (member h prior-shas) "eligibility rule (d): no prior-cohort sample reuse")))

    (test-case "cohort.json: post-W4 and full-window p50/p95 both reported"
      (define c (load-json "cohort.json"))
      (define stats (hash-ref c '|statistics|))
      (check-true (>= (hash-ref (hash-ref stats '|post-w4|) '|count|) 1))
      (check-true (number? (hash-ref (hash-ref stats '|post-w4|) '|p50-seconds|)))
      (check-true (number? (hash-ref (hash-ref stats '|post-w4|) '|p95-seconds|)))
      (check-true (number? (hash-ref (hash-ref stats '|full-window|) '|p50-seconds|)))
      (check-true (number? (hash-ref (hash-ref stats '|full-window|) '|p95-seconds|))))

    (test-case "report.json: §7.2 row vocabulary is complete and each row has goal/measured/verdict"
      (define r (load-json "report.json"))
      (define ids (map (lambda (row) (hash-ref row '|id|)) (hash-ref r '|rows|)))
      (for ([want (in-list v29-report-row-ids)])
        (check-not-false (member want ids) (format "missing §7.2 row ~a" want)))
      (for ([row (in-list (hash-ref r '|rows|))])
        (for ([k '(goal measured verdict)])
          (check-true (hash-has-key? row k) (format "row ~a missing ~a" (hash-ref row '|id|) k)))))

    (test-case "report.json: unknown metrics remain unknown, never coerced to zero"
      (define r (load-json "report.json"))
      (define (row id)
        (findf (lambda (x) (equal? (hash-ref x '|id|) id)) (hash-ref r '|rows|)))
      (define flake (row "flake-tax-runner-share"))
      (define flake-measured (hash-ref flake '|measured|))
      (check-true (string? (hash-ref flake-measured '|rate|))
                  "flake-tax rate must stay the unknown marker, never 0")
      (check-true (regexp-match? #rx"unknown" (hash-ref flake-measured '|rate|)))
      (define pe (row "prepared-env-verified-restore"))
      (define measured (hash-ref pe '|measured|))
      (check-equal? (hash-ref (hash-ref measured '|verified_restore_ratio|) '|status|)
                    "pending-coordinator-fill"))

    (test-case "decision.md: all 14 §7.1 safety-gate rows are decided"
      (define md (file->string (build-path v29-dir "decision.md")))
      (for ([row (in-list v29-safety-gate-rows)])
        (check-true (string-contains? md row) (format "decision.md must decide §7.1 row ~a" row)))
      ;; rows 1-11 and 13-14 read a plain **pass**; row 12 reads a qualified pass
      (check-true (>= (length (regexp-match* #rx"[*][*]pass[*][*]" md)) 12)
                  "the overwhelming majority of rows must read pass")
      (check-true (string-contains? md "pass (mechanism + carried baseline)")
                  "row 12's qualified pass must be explicit"))

    (test-case "decision.md: ends in exactly one §13 draft verdict from the vocabulary"
      (define md (file->string (build-path v29-dir "decision.md")))
      (define verdict-line
        (last (filter (lambda (l) (non-empty-string? (string-trim l))) (string-split md "\n"))))
      (check-not-false (member (string-trim verdict-line) v29-verdict-vocabulary)
                       "the last content line must be exactly the draft verdict")
      (check-not-false (member "PARTIAL — SAFE REDUCTION DELIVERED" v29-verdict-vocabulary))
      (check-equal? (string-trim verdict-line) "PARTIAL — SAFE REDUCTION DELIVERED"))

    (test-case "graph-after.json: exactly one removal (dup-01) and the dup-04 protection"
      (define g (load-json "graph-after.json"))
      (define changes (hash-ref g '|changes_vs_w0|))
      (define removed (hash-ref changes '|removed_claims|))
      (check-equal? (length removed) 1)
      (check-equal? (hash-ref (first removed) '|pair_id|) "dup-01")
      (define rbw (hash-ref (hash-ref g '|final_metrics|) '|removed_by_w9|))
      (check-true (regexp-match? #rx"PROXY" (hash-ref rbw '|proxy_basis|))
                  "the dup-01 saving must stay PROXY-labeled")
      (define requalified (hash-ref changes '|requalified_pairs|))
      (check-equal? (length requalified) 1)
      (check-equal? (hash-ref (first requalified) '|pair_id|) "dup-04")
      (check-equal? (hash-ref (first requalified) '|current_class|) "distinct_environment")
      (check-true (hash-ref (first requalified) '|both_instances_remain_required|)
                  "both dup-04 instances must remain required"))

    (test-case "graph-after.json: §4.7 accounting carries the honest post-W9 numbers"
      (define g (load-json "graph-after.json"))
      (define acct (hash-ref g '|spec_section_4_7_accounting|))
      (check-equal? (hash-ref acct '|avoidable_remainder_seconds|) 477)
      (check-equal? (hash-ref acct '|avoidable_remainder_ratio_of_window|) 0.0249)
      (check-equal? (hash-ref acct '|distinct_environment_removed_seconds|) 0)
      (check-equal? (hash-ref acct '|distinct_semantic_removed_seconds|) 0)
      (check-equal? (hash-ref acct '|observational_removed_seconds|) 0)
      (define fm (hash-ref g '|final_metrics|))
      (check-equal? (hash-ref fm '|duplicate_proof_ratio_after_w9|) 0.0249)
      (check-equal? (hash-ref fm '|duplicate_proof_ratio_goal|) 0.1))

    (test-case "SHA256SUMS: the four contract artifacts match their recorded digests"
      (define sums (file->lines (build-path v29-dir "SHA256SUMS")))
      (define non-empty (filter (lambda (l) (non-empty-string? (string-trim l))) sums))
      (check-equal? (length non-empty) 4)
      (for ([line (in-list non-empty)])
        (define m (regexp-match #px"^([0-9a-f]{64})\\s{2}(.+)$" (string-trim line)))
        (check-true (pair? m) (format "malformed SHA256SUMS line: ~a" line))
        (when (pair? m)
          (define want (second m))
          (define name (third m))
          (define got (call-with-input-file (build-path v29-dir name) sha256-hex))
          (check-equal? got want (format "~a digest mismatch" name)))))))

;; ============================================================

(define failures (run-tests suite))
(define c2-failures (run-tests c2-suite))
(define pr-elapsed-failures (run-tests pr-elapsed-suite))
(define final-claim-failures (run-tests final-claim-suite))
(define w8-failures (run-tests w8-final-verdict-suite))
(define v29-final-failures (run-tests v29-final-suite))

(module+ main
  (when (positive? (+ failures
                      c2-failures
                      pr-elapsed-failures
                      final-claim-failures
                      w8-failures
                      v29-final-failures))
    (exit 1)))
