#lang racket

;; @speed fast
;; @suite default
;; @boundary unit

;;; scripts/run-tests/test-gate-ownership.rkt — v1.00.24 W0 gate ownership
;;;
;;; The check branch of hotspot-benchmark.rkt is the single owner of manifest
;;; validation. These tests pin the contract that matters for ownership:
;;;
;;;   1. An honestly-declared small run (samples_per_family below ten) passes
;;;      the gate — the floor is min(10, requested), never a fabricated
;;;      absolute ten when the builder requested fewer.
;;;   2. Passing fewer samples than the floor is rejected with an explicit
;;;      error naming the threshold.
;;;   3. Legacy manifests without samples_per_family still require ten
;;;      passing samples (backward-compatible absolute gate).
;;;   4. Artifacts missing required top-level fields cannot sneak past the
;;;      gate — the builder cannot ship an unvalidated manifest.
;;;
;;; All fixtures are synthetic hashes; no real timing collection runs here.

(require rackunit
         racket/string
         (prefix-in hb: (file "hotspot-benchmark.rkt")))

;; ---------------------------------------------------------------
;; Fixture: well-formed manifest with n passing samples
;; ---------------------------------------------------------------

(define (valid-manifest #:samples [n 10] #:per-family [per-family #f])
  (define m
    (hasheq "schema" "test-runtime/hotspot-baseline/v1"
            "milestone" "v1.00.24"
            "wave" "W0"
            "command" "racket scripts/run-tests/hotspot-baseline.rkt --manifest ... --samples 10"
            "q_sha" (make-string 40 #\0)
            "scheduler" "batch"
            "mode" "subprocess"
            "jobs" 2
            "selected_paths_digest" (make-string 64 #\a)
            "environment" (hasheq "racket_version" "8.x" "os" "linux" "machine" "x86_64" "config_digest" "cfg")
            "inputs" (hasheq "allowlist_sha256" (make-string 64 #\b))
            "families"
            (list
             (hasheq
              "file" "tests/example.rkt"
              "samples" (for/list ([i (in-range n)]) (hasheq "sample" i "status" "pass" "duration_ms" 100))
              "stats" (hasheq "successful" n "failures" 0 "timeouts" 0
                              "median_ms" 100 "p95_ms" 100 "min_ms" 100 "max_ms" 100)))))
  (if per-family (hash-set m "samples_per_family" per-family) m))

;; ---------------------------------------------------------------
;; 1. Honest small run passes the gate
;; ---------------------------------------------------------------

(test-case "declared samples_per_family below ten sets an honest floor"
  (define errors
    (hb:hotspot-manifest-errors (valid-manifest #:samples 2 #:per-family 2)))
  (check-equal? errors '()
                "a run that requested 2 samples and got 2 passes must pass"))

;; ---------------------------------------------------------------
;; 2. Below-floor passes are rejected explicitly
;; ---------------------------------------------------------------

(test-case "fewer passes than the declared floor is rejected"
  (define errors
    (hb:hotspot-manifest-errors (valid-manifest #:samples 1 #:per-family 3)))
  (check-true (for/or ([e (in-list errors)])
                (string-contains? e "fewer than 3"))
              (format "expected threshold named in error, got: ~a" errors)))

;; ---------------------------------------------------------------
;; 3. Legacy manifests keep the absolute ten-sample gate
;; ---------------------------------------------------------------

(test-case "manifest without samples_per_family still requires ten passes"
  (define errors
    (hb:hotspot-manifest-errors (valid-manifest #:samples 9)))
  (check-true (for/or ([e (in-list errors)])
                (string-contains? e "fewer than 10"))
              (format "expected legacy ten-sample gate, got: ~a" errors)))

;; ---------------------------------------------------------------
;; 4. Missing required fields cannot pass the gate
;; ---------------------------------------------------------------

(test-case "required top-level fields are enforced by the gate"
  (for ([field '("schema" "milestone" "wave" "command" "q_sha" "scheduler"
                 "mode" "jobs" "selected_paths_digest" "environment"
                 "inputs" "families")])
    (define m (hash-remove (valid-manifest) field))
    (check-true (pair? (hb:hotspot-manifest-errors m))
                (format "missing ~a must be rejected by the check branch" field))))
