#lang racket

(require (only-in "../util/version.rkt" q-version))

;; @speed fast
;; @suite default
;; @boundary unit

;;; tests/test-hotspot-benchmark.rkt — W0 focused hotspot baseline
;;;
;;; Unit/component tests for scripts/run-tests/hotspot-benchmark.rkt. All
;;; coverage is deterministic: percentile interpolation, per-file stats,
;;; canonical JSON, schema validation of synthetic manifests, and byte-drift
;;; detection against temp-dir artifacts. No real test-file timing runs here
;;; (the ≥10-sample collection is the wave verify command, never the fast
;;; suite).

(require rackunit
         rackunit/text-ui
         racket/string
         racket/file
         racket/port
         (prefix-in hb: (file "../scripts/run-tests/hotspot-benchmark.rkt")))

;; ---------------------------------------------------------------
;; Linear-interpolated percentiles
;; ---------------------------------------------------------------

(test-case "percentiles interpolate linearly"
  (define xs '(10 20 30 40))
  (check-equal? (hb:hotspot-percentile xs 0) 10.0)
  (check-equal? (hb:hotspot-percentile xs 100) 40.0)
  (check-equal? (hb:hotspot-percentile xs 50) 25.0)
  (check-equal? (hb:hotspot-percentile xs 75) 32.5)
  (check-equal? (hb:hotspot-percentile '(1 2 3 4 5 6 7 8 9 10) 95) 9.55))

(test-case "percentile requires data"
  (check-exn exn:fail? (lambda () (hb:hotspot-percentile '() 50))))

;; ---------------------------------------------------------------
;; Per-file sample statistics
;; ---------------------------------------------------------------

(test-case "file stats summarize successful samples and record failures"
  (define samples
    (append (for/list ([i (in-range 10)])
              (hasheq 'sample i 'status "pass" 'duration_ms (* 100 (add1 i))))
            (list (hasheq 'sample 10 'status "timeout" 'duration_ms 0)
                  (hasheq 'sample 11 'status "fail" 'duration_ms 0))))
  (define stats (hb:hotspot-file-stats samples))
  (check-equal? (hash-ref stats 'successful) 10)
  (check-equal? (hash-ref stats 'failures) 1)
  (check-equal? (hash-ref stats 'timeouts) 1)
  (check-equal? (hash-ref stats 'median_ms) 550.0)
  (check-equal? (hash-ref stats 'p95_ms)
                955.0) ; 900 + 0.55*(1000-900); max is 1000 so 1450 was impossible
  (check-equal? (hash-ref stats 'min_ms) 100)
  (check-equal? (hash-ref stats 'max_ms) 1000))

;; ---------------------------------------------------------------
;; Canonical JSON
;; ---------------------------------------------------------------

(test-case "canonical JSON is key-sorted and deterministic"
  (define obj (hasheq "zeta" 1 "alpha" (list 2 1) "mid" (hasheq "b" #t "a" "x")))
  (define once (hb:hotspot-canonical-json obj))
  (check-equal? once (hb:hotspot-canonical-json obj))
  (check-equal? once "{\"alpha\":[2,1],\"mid\":{\"a\":\"x\",\"b\":true},\"zeta\":1}"))

;; ---------------------------------------------------------------
;; Manifest schema validation
;; ---------------------------------------------------------------

(define (valid-manifest #:samples [n 10])
  (hasheq "schema"
          "test-runtime/hotspot-baseline/v1"
          "milestone"
          (format "v~a" q-version)
          "wave"
          "W0"
          "command"
          "racket scripts/run-tests/hotspot-baseline.rkt --manifest ... --samples 10"
          "q_sha"
          "f0b8f8cf00000000000000000000000000000000"
          "scheduler"
          "batch"
          "mode"
          "subprocess"
          "jobs"
          2
          "selected_paths_digest"
          (make-string 64 #\a)
          "environment"
          (hasheq "racket_version" "8.x" "os" "linux" "machine" "x86_64" "config_digest" "cfg")
          "inputs"
          (hasheq "allowlist_sha256" (make-string 64 #\b))
          "families"
          (list (hasheq "file"
                        "tests/example.rkt"
                        "samples"
                        (for/list ([i (in-range n)])
                          (hasheq "sample" i "status" "pass" "duration_ms" 100))
                        "stats"
                        (hasheq "successful"
                                n
                                "failures"
                                0
                                "timeouts"
                                0
                                "median_ms"
                                100
                                "p95_ms"
                                100
                                "min_ms"
                                100
                                "max_ms"
                                100)))))

(test-case "valid ten-sample manifest has no errors"
  (check-equal? (hb:hotspot-manifest-errors (valid-manifest)) '()))

(test-case "fewer than ten successful samples is rejected"
  (define errors (hb:hotspot-manifest-errors (valid-manifest #:samples 9)))
  (check-true (for/or ([e (in-list errors)])
                (string-contains? e "fewer than 10"))
              (format "expected sample-floor error, got: ~a" errors)))

(test-case "samples_per_family below ten sets an honest sample floor"
  (define m (hash-set (valid-manifest #:samples 2) "samples_per_family" 2))
  (check-equal? (hb:hotspot-manifest-errors m) '()))

(test-case "missing environment/scheduler/digest fields are rejected"
  (for ([field '("environment" "scheduler"
                               "mode"
                               "jobs"
                               "q_sha"
                               "selected_paths_digest"
                               "command"
                               "families")])
    (define m (hash-remove (valid-manifest) field))
    (check-true (pair? (hb:hotspot-manifest-errors m)) (format "missing ~a must be rejected" field))))

(test-case "failed and timed-out attempts must be recorded in samples"
  (define m (valid-manifest))
  (define truncated-samples
    (list (hasheq "file"
                  "tests/example.rkt"
                  "samples"
                  (for/list ([i (in-range 10)])
                    (hasheq "sample" i "status" "pass" "duration_ms" 100))
                  "stats"
                  (hasheq "successful"
                          10
                          "failures"
                          0
                          "timeouts"
                          0
                          "median_ms"
                          100
                          "p95_ms"
                          100
                          "min_ms"
                          100
                          "max_ms"
                          100))))
  ;; stats claim one failure but samples record none -> inconsistent
  (define m2
    (hash-set m
              "families"
              (list (hash-set (first (hash-ref m "families"))
                              "stats"
                              (hasheq "successful"
                                      9
                                      "failures"
                                      1
                                      "timeouts"
                                      0
                                      "median_ms"
                                      100
                                      "p95_ms"
                                      100
                                      "min_ms"
                                      100
                                      "max_ms"
                                      100)))))
  (check-true (pair? (hb:hotspot-manifest-errors m2)) "stats/sample inconsistency must be rejected"))

;; ---------------------------------------------------------------
;; Byte-drift check against written artifacts
;; ---------------------------------------------------------------

(test-case "manifest --check passes on untouched artifact and fails on byte drift"
  (define tmp (make-temporary-file "hbtest~a" 'directory))
  (define manifest-path (build-path tmp "baseline.json"))
  (define sums-path (build-path tmp "SHA256SUMS"))
  (define m (valid-manifest))
  (call-with-output-file manifest-path (lambda (out) (display (hb:hotspot-canonical-json m) out)))
  (hb:hotspot-write-sha256sums! sums-path (list manifest-path))
  (check-equal? (hb:hotspot-artifact-errors manifest-path sums-path m) '())
  ;; flip a byte: drift must be detected
  (call-with-output-file manifest-path
                         (lambda (out)
                           (display (string-append (hb:hotspot-canonical-json m) " "))
                           out)
                         #:exists 'truncate/replace)
  (define errors (hb:hotspot-artifact-errors manifest-path sums-path m))
  (check-true (for/or ([e (in-list errors)])
                (string-contains? e "drift"))
              (format "expected byte-drift error, got: ~a" errors)))
