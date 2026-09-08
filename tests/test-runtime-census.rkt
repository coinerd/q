#lang racket/base

;; @speed fast
;; @suite runtime
;; @boundary unit
;; @covers scripts/run-tests/runtime-census.rkt

;; tests/test-runtime-census.rkt — v1.00.28 W0: contract tests for the
;; repository-wide fast-tier runtime census.
;;
;; RUNTIME-AUDIT-SPEC-v1.00.28.md contracts exercised here:
;; §2  — per-test record schema; unknown counters serialize as null, never 0;
;; §3  — sampling floor (>= 3 successful samples), failures/timeouts retained;
;; §4  — aggregates (work mass, Pareto, buckets, boundary split, counters);
;; §5  — static wait/subprocess scan (triage input);
;; §6  — generated review queues Q1..Q7;
;; inventory completeness — every `fast` file has a record or an explicit
;; collection failure (silent omissions are contract violations).

(require rackunit
         racket/file
         racket/list
         racket/string
         (only-in "helpers/metadata.rkt" get-file-metadata)
         (only-in "helpers/fast-inventory.rkt"
                  fast-inventory-files
                  fast-inventory-file?
                  fast-inventory-metadata)
         (only-in "../scripts/run-tests/hotspot-benchmark.rkt"
                  hotspot-manifest-errors
                  current-hotspot-sample-floor)
         (only-in "../scripts/run-tests/runtime-census.rkt"
                  census-record
                  census-record-required-fields
                  census-instrumentation-unknown
                  census-completeness-errors
                  census-work-type-counters
                  census-aggregates
                  census-static-scan
                  census-canonical-bytes
                  census-manifest-errors
                  census-collect))

;; ============================================================
;; §2 — record schema and unknown-counter serialization
;; ============================================================

(test-case "record schema: required fields present"
  (define rec
    (census-record "tests/test-census-fixture.rkt"
                   '(100 120 110)
                   (list (hasheq 'attempt 1 'status "pass" 'duration_ms 100)
                         (hasheq 'attempt 2 'status "pass" 'duration_ms 120)
                         (hasheq 'attempt 3 'status "pass" 'duration_ms 110))
                   (hasheq 'speed "fast" 'suite "runtime" 'boundary "unit" 'covers '())))
  (for ([f (in-list census-record-required-fields)])
    (check-true (hash-has-key? rec f) (format "record missing field ~a" f)))
  (check-equal? (hash-ref rec 'path) "tests/test-census-fixture.rkt")
  (check-equal? (hash-ref rec 'samples_ms) '(100 120 110))
  ;; median of 100/120/110 = 110
  (check-equal? (hash-ref rec 'median_ms) 110)
  ;; p95 must be computed with the same linear interpolation as the
  ;; hotspot tooling, not the min or max.
  (check-true (real? (hash-ref rec 'p95_ms)))
  (check-equal? (hash-ref rec 'status) "pass"))

(test-case "unknown counters serialize as null/unknown, never 0"
  (define unknown (census-instrumentation-unknown))
  (for ([f (in-list '(racket_subprocesses subprocesses
                                          git_commands
                                          temp_dirs
                                          temp_files
                                          session_fixtures
                                          git_fixtures
                                          worktrees
                                          sleep_requested_ms
                                          timeouts
                                          server_or_socket_lifecycles))])
    (check-equal? (hash-ref unknown f)
                  'unknown
                  (format "counter ~a must default to unknown, not 0" f)))
  ;; JSON serialization of an unknown counter must produce JSON null, and the
  ;; canonical bytes must never contain the JSON number 0 for that field.
  (define rec
    (census-record "tests/test-census-fixture.rkt"
                   '(5 6 7)
                   '()
                   (hasheq 'speed "fast" 'suite "runtime" 'boundary "unit" 'covers '())))
  (define bytes (census-canonical-bytes (hasheq 'schema "t" 'records (list rec))))
  (define body (bytes->string/utf-8 bytes))
  (check-false (regexp-match? #rx"\"racket_subprocesses\":0" body)
               "unknown counter serialized as JSON 0")
  (check-true (regexp-match? #rx"\"racket_subprocesses\":null" body)
              "unknown counter must serialize as JSON null"))

(test-case "failures and timeouts are retained attempts, never discarded"
  (define attempts
    (list (hasheq 'attempt 1 'status "fail" 'duration_ms 40)
          (hasheq 'attempt 2 'status "pass" 'duration_ms 100)
          (hasheq 'attempt 3 'status "timeout" 'duration_ms 5000)
          (hasheq 'attempt 4 'status "pass" 'duration_ms 120)
          (hasheq 'attempt 5 'status "pass" 'duration_ms 110)))
  (define rec
    (census-record "tests/test-census-fixture.rkt"
                   '(100 120 110)
                   attempts
                   (hasheq 'speed "fast" 'suite "runtime" 'boundary "unit" 'covers '())))
  (check-equal? (length (hash-ref rec 'all_attempts))
                5
                "all attempts (including fail/timeout) must be retained")
  (check-equal? (hash-ref rec 'status) "pass")
  (check-true (for/or ([a (in-list (hash-ref rec 'all_attempts))])
                (equal? (hash-ref a 'status) "timeout"))
              "timeout attempt must survive in the record")
  (check-true (for/or ([a (in-list (hash-ref rec 'all_attempts))])
                (equal? (hash-ref a 'status) "fail"))
              "failed attempt must survive in the record"))

(test-case "collection failure record keeps explicit failure status"
  (define rec
    (census-record "tests/test-census-fixture.rkt"
                   '()
                   (list (hasheq 'attempt 1 'status "timeout" 'duration_ms 5000)
                         (hasheq 'attempt 2 'status "timeout" 'duration_ms 5000)
                         (hasheq 'attempt 3 'status "timeout" 'duration_ms 5000))
                   (hasheq 'speed "fast" 'suite "runtime" 'boundary "unit" 'covers '())))
  (check-equal? (hash-ref rec 'status) "collection-failure")
  (check-equal? (hash-ref rec 'samples_ms) '()))

;; ============================================================
;; inventory completeness — silent omissions are red
;; ============================================================

(test-case "completeness: missing census record is an error"
  (check-true (pair? (census-completeness-errors (list "tests/test-a.rkt" "tests/test-b.rkt")
                                                 (list (hasheq 'path "tests/test-a.rkt"))))
              "a fast file present on disk but absent from the census is red"))

(test-case "completeness: extra census record is an error"
  (check-true (pair? (census-completeness-errors (list "tests/test-a.rkt")
                                                 (list (hasheq 'path "tests/test-a.rkt")
                                                       (hasheq 'path "tests/test-ghost.rkt"))))
              "a census record with no on-disk fast file is red"))

(test-case "completeness: explicit collection failures are not omissions"
  (check-false (pair? (census-completeness-errors
                       (list "tests/test-a.rkt" "tests/test-b.rkt")
                       (list (hasheq 'path "tests/test-a.rkt" 'status "pass")
                             (hasheq 'path "tests/test-b.rkt" 'status "collection-failure"))))
               "explicit collection-failure records satisfy the contract"))

(test-case "completeness: full inventory consistent with itself"
  (check-false (pair? (census-completeness-errors
                       (list "tests/test-a.rkt")
                       (list (hasheq 'path "tests/test-a.rkt" 'status "pass"))))))

;; ============================================================
;; §4 — aggregates
;; ============================================================

(test-case "aggregates: work mass, pareto, buckets, boundary split"
  (define recs
    (list (hasheq 'path
                  "t/a.rkt"
                  'median_ms
                  100
                  'status
                  "pass"
                  'speed
                  "fast"
                  'boundary
                  "unit"
                  'instrumentation
                  (census-instrumentation-unknown))
          (hasheq 'path
                  "t/b.rkt"
                  'median_ms
                  12000
                  'status
                  "pass"
                  'speed
                  "fast"
                  'boundary
                  "integration"
                  'instrumentation
                  (census-instrumentation-unknown))
          (hasheq 'path
                  "t/c.rkt"
                  'median_ms
                  7000
                  'status
                  "pass"
                  'speed
                  "unit-fast"
                  'boundary
                  "unit"
                  'instrumentation
                  (census-instrumentation-unknown))
          (hasheq 'path
                  "t/d.rkt"
                  'median_ms
                  300
                  'status
                  "pass"
                  'speed
                  "fast"
                  'boundary
                  "e2e"
                  'instrumentation
                  (census-instrumentation-unknown))))
  (define ag (census-aggregates recs))
  (check-equal? (hash-ref ag 'work_mass_ms) 19400)
  (define pareto (hash-ref ag 'pareto))
  ;; top 1 of 4 = 12000 ms = ~62.18%
  (define top1 (hash-ref pareto 'top1))
  (check-equal? (hash-ref top1 'mass_ms) 12000)
  (check-true (and (real? (hash-ref top1 'pct)) (< 61 (hash-ref top1 'pct) 62)))
  (check-equal? (hash-ref (hash-ref pareto 'remainder) 'mass_ms) 0)
  (define buckets (hash-ref ag 'buckets))
  (check-equal? (length buckets) 6)
  (check-equal? (hash-ref (hash-ref ag 'boundary) "fast/other") (hasheq 'file_count 1 'mass_ms 300))
  (check-equal? (hash-ref (hash-ref ag 'boundary) "unit-fast/unit")
                (hasheq 'file_count 1 'mass_ms 7000)))

(test-case "aggregates: review queues generated (Q1..Q7)"
  (define recs
    (list (hasheq 'path
                  "t/slow.rkt"
                  'median_ms
                  11000
                  'status
                  "pass"
                  'speed
                  "fast"
                  'boundary
                  "integration"
                  'instrumentation
                  (hash-set (census-instrumentation-unknown) 'sleep_requested_ms 250))
          (hasheq 'path
                  "t/clean.rkt"
                  'median_ms
                  100
                  'status
                  "pass"
                  'speed
                  "unit-fast"
                  'boundary
                  "unit"
                  'instrumentation
                  (census-instrumentation-unknown))))
  (define ag (census-aggregates recs))
  (define queues (hash-ref ag 'queues))
  (check-equal? (hash-ref queues 'q1) '("t/slow.rkt") "Q1: fast >10s")
  (check-not-false (member "t/slow.rkt" (hash-ref queues 'q3)) "Q3: fast+integration")
  (check-not-false (member "t/slow.rkt" (hash-ref queues 'q4)) "Q4: real-time wait candidates")
  (check-equal? (hash-ref queues 'q7) '("t/clean.rkt") "Q7: grouped-execution candidates"))

;; ============================================================
;; §5 — static scan
;; ============================================================

(test-case "static scan: finds seeded terms, misses are absent, non-defect"
  (define tmp-root (make-temporary-file "census-scan-~a" 'directory))
  (define tests-dir (build-path tmp-root "tests"))
  (make-directory tests-dir)
  (call-with-output-file (build-path tests-dir "seeded.rkt")
                         (lambda (o)
                           (display "#lang racket/base\n(sleep 30)\n(sync/timeout 5 evt)\n" o)))
  (call-with-output-file (build-path tests-dir "clean.rkt")
                         (lambda (o) (display "#lang racket/base\n(define x 1)\n" o)))
  (define scan (census-static-scan tests-dir))
  (define seeded (hash-ref scan "tests/seeded.rkt" '()))
  (check-not-false (member "sleep" seeded) "sleep term must be found")
  (check-not-false (member "sync/timeout" seeded) "sync/timeout term must be found")
  (check-false (member "tcp-listen" seeded))
  (check-false (hash-has-key? scan "tests/clean.rkt") "clean files must not appear in the scan")
  (delete-directory/files tmp-root))

;; ============================================================
;; census-level manifest schema (--check contract)
;; ============================================================

(test-case "manifest errors: well-formed census passes, broken ones fail"
  (define unknown (census-instrumentation-unknown))
  (define rec
    (hasheq 'path
            "t/a.rkt"
            'speed
            "fast"
            'suite
            "runtime"
            'boundary
            "unit"
            'covers
            '()
            'samples_ms
            '(1 2 3)
            'median_ms
            2
            'p95_ms
            3
            'status
            "pass"
            'all_attempts
            '()
            'instrumentation
            unknown))
  (define good
    (hasheq 'schema
            "q.census.fast-runtime/1"
            'suite
            "fast"
            'inventory
            (hasheq 'file_count 1 'selected_paths_sha256 "x")
            'records
            (list rec)
            'collection_failures
            '()
            'aggregates
            (census-aggregates (list rec))
            'static_scan
            (hasheq 'terms '() 'files (hasheq))))
  (check-false (pair? (census-manifest-errors good))
               "a spec-conformant census manifest must have no schema errors")
  (check-true (pair? (census-manifest-errors (hash-remove good 'records)))
              "missing records section must be a schema error")
  (check-true
   (pair? (census-manifest-errors (hash-set good 'records (list (hash-remove rec 'instrumentation)))))
   "record without instrumentation must be a schema error"))

;; ============================================================
;; §3 — sampling runner regression (round-1 empty records hash)
;; ============================================================

(test-case "sampling: census-collect records an attempt on the very first round"
  ;; Regression: the per-round records update used (hash-ref records f)
  ;; without a default, so the first sampled file of the first round raised
  ;; "no value found for key". One file, one sample must simply produce one
  ;; retained attempt.
  (define fixture ".census-regression-fixture.rkt")
  (with-output-to-file fixture (lambda () (displayln "#lang racket")) #:exists 'replace)
  (dynamic-wind
   (lambda () (void))
   (lambda ()
     (define records (census-collect (list fixture) #:samples 1 #:jobs 1 #:timeout-s 120))
     (check-equal? (length records) 1 "one file in, one record out")
     (define rec (first records))
     (check-equal? (hash-ref rec 'path) fixture "record must be keyed to the fixture path")
     (define attempts (hash-ref rec 'all_attempts))
     (check-true (list? attempts) "sampled file must have an attempt list")
     (check-equal? (length attempts) 1 "one sample requested, one attempt retained")
     (check-not-false (member (hash-ref (first attempts) 'status) '("pass" "fail" "timeout"))
                      "attempt status must be pass/fail/timeout"))
   (lambda ()
     (with-handlers ([exn:fail? void])
       (delete-file fixture)))))

;; ============================================================
;; live inventory exposure (extended in
;; tests/test-run-tests-metadata-discovery.rkt; sanity here)
;; ============================================================

(test-case "fast inventory: resolved, non-empty, all real test files"
  (define files (fast-inventory-files))
  (check-true (pair? files) "fast inventory must be non-empty")
  (check-true (fast-inventory-file? "tests/test-runtime-census.rkt")
              "this test file itself must be in the fast inventory")
  (for ([f (in-list (take files 25))])
    (check-true (string-suffix? f ".rkt") (format "~a is not a .rkt path" f))
    (check-false (string-contains? f "/compiled/") (format "~a hits compiled/" f)))
  (check-true (hash? (fast-inventory-metadata "tests/test-runtime-census.rkt"))
              "per-file metadata must be exposed for census records"))

;; ============================================================
;; hotspot machinery reuse: sample-floor parameterization
;; (the census runs at the >= 3-sample floor via
;; current-hotspot-sample-floor; the hotspot default stays 10)
;; ============================================================

(define (mk-floor-manifest n)
  (hasheq 'schema
          "test-runtime/hotspot-baseline/v1"
          'milestone
          "v1.00.27-testwork"
          'wave
          "W0"
          'mode
          "subprocess"
          'jobs
          3
          'scheduler
          "batch"
          'q_sha
          "0000000000000000000000000000000000000000"
          'command
          "racket scripts/run-tests/hotspot-baseline.rkt"
          'selected_paths_digest
          (make-string 64 #\b)
          'environment
          (hasheq 'config_digest "cfg-test" 'machine "test-host" 'os "linux" 'racket_version "8.15")
          'inputs
          (hasheq 'allowlist_sha256 (make-string 64 #\a))
          'families
          (list (hasheq 'file
                        "tests/example.rkt"
                        'samples
                        (for/list ([i (in-range n)])
                          (hasheq 'sample i 'status "pass" 'duration_ms 100))
                        'stats
                        (hasheq 'median_ms
                                100
                                'p95_ms
                                100
                                'min_ms
                                100
                                'max_ms
                                100
                                'successful
                                n
                                'failures
                                0
                                'timeouts
                                0)))))

(test-case "sampling: hotspot sample floor is parameterized for census reuse"
  (check-true (null? (hotspot-manifest-errors (mk-floor-manifest 10)))
              "default floor (10) accepts a 10-sample manifest")
  (check-true (pair? (hotspot-manifest-errors (mk-floor-manifest 9)))
              "default floor (10) rejects a below-floor manifest")
  (parameterize ([current-hotspot-sample-floor 3])
    (check-true (null? (hotspot-manifest-errors (mk-floor-manifest 3)))
                "census floor (3) accepts a 3-sample manifest")
    (check-true (pair? (hotspot-manifest-errors (mk-floor-manifest 2)))
                "census floor (3) rejects a 2-sample manifest"))
  (parameterize ([current-hotspot-sample-floor 5])
    (check-true (null? (hotspot-manifest-errors (mk-floor-manifest 6)))
                "floor honors its parameter value, not the default")
    (check-true (pair? (hotspot-manifest-errors (mk-floor-manifest 4)))
                "floor rejects samples below its parameter value")))
