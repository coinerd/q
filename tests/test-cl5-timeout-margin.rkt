#lang racket

;; Tests for the concurrency-aware timeout-margin policy (#9982, P2-D).
;;
;; Two jobs: prove the arithmetic and the refusals, and pin the committed registry
;; against the actual `@timeout` declarations so a silent timeout change cannot drift
;; away from its evidence.

(require json
         rackunit
         racket/file
         racket/runtime-path
         "../scripts/cl5/timeout-margin.rkt"
         "../scripts/testing/full-plan.rkt")

;; raco test runs with the file's own directory as cwd, so repo-relative paths are
;; resolved through this anchor rather than the process cwd.
(define-runtime-path repo-root "../")

;; ---- policy arithmetic -------------------------------------------------------------

(test-case "percentile is nearest-rank and refuses an empty sample"
  (check-equal? (percentile '(1 2 3 4 5) 0.95) 5.0)
  (check-equal? (percentile '(5 1 9 3 7) 0.95) 9.0)
  (check-equal? (percentile '(10 20) 0.95) 20.0)
  (check-exn exn:fail? (lambda () (percentile '() 0.95))))

(test-case "limits round up to 30 s"
  (check-equal? (ceil-to-30 1) 30)
  (check-equal? (ceil-to-30 30) 30)
  (check-equal? (ceil-to-30 31) 60)
  (check-equal? (ceil-to-30 215.4) 240))

(test-case "T = max(existing, ceil_30(1.5 * p95)) and never lowers a declaration"
  ;; 1.5 x 100 s = 150 -> 150 (already a multiple of 30); floor 120 does not apply
  (check-equal? (derive-limit 120 100000) 150)
  ;; 1.5 x 50 s = 75 -> 90; the existing 120 s declaration is a floor
  (check-equal? (derive-limit 120 50000) 120)
  ;; 1.5 x 215.4 s = 323.1 -> 330
  (check-equal? (derive-limit 120 215400) 330)
  ;; 1.5 x 350.8 s = 526.2 -> 540
  (check-equal? (derive-limit 210 350800) 540))

;; ---- registry refusals -------------------------------------------------------------

(define (entry #:path [path "tests/test-gsd-executor-ergonomics-prompt.rkt"]
               #:samples [samples '(100000 101000 102000 103000 104000)]
               #:existing [existing 120])
  (define p95 (percentile samples 0.95))
  (hash 'path
        path
        'existing_limit_s
        existing
        'job_count
        4
        'samples_ms
        samples
        'sample_count
        (length samples)
        'p95_ms
        (inexact->exact (round p95))
        'limit_s
        (derive-limit existing p95)
        'margin
        (/ (derive-limit existing p95) (/ p95 1000.0))
        'uncertainty
        "p95 = max of the samples; lower bound"))

(define (registry #:entries [entries (list (entry))])
  (hasheq 'schema
          "cl5-slow-tests/1"
          'policy
          (hasheq 'formula
                  "max(existing_limit_s, ceil_30s(1.5 * p95_job_count))"
                  'min_samples
                  5
                  'rounding_s
                  30)
          'allowed_paths
          default-allowed-paths
          'entries
          entries))

(test-case "a consistent registry verifies"
  (check-equal? (hash-ref (verify-registry (registry)) 'entries) 1))

(test-case "the policy is not repository-wide: a path outside the allow-list is refused"
  (check-exn #rx"not in the registry's allow-list"
             (lambda ()
               (verify-registry (registry #:entries
                                          (list (entry #:path "tests/test-settings.rkt")))))))

(test-case "a registry may not widen its own allow-list"
  ;; The authoritative list lives in the module; a registry that adds a path is
  ;; refused, so the policy cannot authorise itself to cover another file.
  (check-exn #rx"differs from the authoritative one"
             (lambda ()
               (verify-registry (hash-set (registry)
                                          'allowed_paths
                                          (cons "tests/test-settings.rkt" default-allowed-paths))))))

(test-case "a margin that does not match the numbers is refused"
  (define e (hash-set (entry) 'margin 99.0))
  (check-exn #rx"margin 99.0 does not match"
             (lambda () (verify-registry (registry #:entries (list e))))))

(test-case "too few samples are refused (sparse data cannot derive a limit)"
  (check-exn #rx"too sparse"
             (lambda ()
               (verify-registry (registry #:entries
                                          (list (entry #:samples '(100000 101000 102000))))))))

(test-case "a p95 or limit that does not re-derive is refused"
  (define e (entry))
  (check-exn #rx"declares p95"
             (lambda () (verify-registry (registry #:entries (list (hash-set e 'p95_ms 1))))))
  (check-exn #rx"policy derives"
             (lambda () (verify-registry (registry #:entries (list (hash-set e 'limit_s 120)))))))

(test-case "a limit below 1.5x p95 is refused even if the arithmetic is consistent"
  (define e (entry))
  ;; existing floor 120 with p95 104 s -> derived 156 -> but force a too-small limit
  (define bad (hash-set e 'limit_s 120))
  (check-exn #rx"policy derives|below 1.5"
             (lambda () (verify-registry (registry #:entries (list bad))))))

(test-case "missing uncertainty, job count or sample count is refused"
  (define e (entry))
  (check-exn #rx"uncertainty"
             (lambda () (verify-registry (registry #:entries (list (hash-remove e 'uncertainty))))))
  (check-exn #rx"job count"
             (lambda () (verify-registry (registry #:entries (list (hash-remove e 'job_count))))))
  (check-exn #rx"sample_count"
             (lambda () (verify-registry (registry #:entries (list (hash-set e 'sample_count 99)))))))

(test-case "an unknown schema is refused"
  (check-exn #rx"unknown schema"
             (lambda () (verify-registry (hash-set (registry) 'schema "cl5-slow-tests/0")))))

;; ---- the committed registry is pinned to the declarations --------------------------

(define registry-path (build-path repo-root "docs/reports/CL5-P2D-TIMEOUT-MARGIN.json"))

(test-case "the committed registry verifies"
  (check-true (file-exists? registry-path))
  (define reg (with-input-from-file registry-path read-json))
  (check-equal? (hash-ref (verify-registry reg) 'entries) (length (hash-ref reg 'allowed_paths))))

(test-case "every registry entry equals the file's actual @timeout declaration"
  ;; A silent raise or a forgotten update fails here: the declaration and the
  ;; evidence that justified it cannot drift apart.
  (define reg (with-input-from-file registry-path read-json))
  (for ([e (in-list (hash-ref reg 'entries))])
    (check-equal? (full-file-timeout (path->string (build-path repo-root (hash-ref e 'path))))
                  (hash-ref e 'limit_s)
                  (format "~a declaration != registry" (hash-ref e 'path)))))

(test-case "the committed allow-list is exactly the measured files"
  ;; The policy must not spread: an entry means a measurement here, in the registry and
  ;; in the declaration. The list is asserted literally so a silent addition fails.
  (define reg (with-input-from-file registry-path read-json))
  (check-equal? (sort (hash-ref reg 'allowed_paths) string<?)
                (sort (list "tests/test-ci-recovery-trial.rkt"
                            "tests/test-golden-flows.rkt"
                            "tests/test-gsd-branch-delivery-verification.rkt"
                            "tests/test-gsd-delivery-coordinator.rkt"
                            "tests/test-gsd-delivery-verifier-git-contract.rkt"
                            "tests/test-gsd-evidence-discovery.rkt"
                            "tests/test-gsd-evidence-repair-production-contract.rkt"
                            "tests/test-gsd-executor-ergonomics-prompt.rkt"
                            "tests/test-milestone-gate.rkt"
                            "tests/test-qt-lanes.rkt"
                            "tests/test-qt-quick.rkt")
                      string<?)))

;; ---- censored samples and probe copies --------------------------------------------

(test-case "censored-samples separates killed runs from durations"
  (check-equal? (censored-samples (list (hash 'run_status "pass" 'elapsed_ms 5)
                                        (hash 'run_status "timeout" 'elapsed_ms 120001)
                                        (hash 'run_status "fail" 'elapsed_ms 900)))
                (list (hash 'run_status "timeout" 'elapsed_ms 120001)
                      (hash 'run_status "fail" 'elapsed_ms 900)))
  (check-equal? (censored-samples (list (hash 'run_status "pass" 'elapsed_ms 5))) '())
  ;; An unreadable sample is treated as censored, never as a duration.
  (check-equal? (censored-samples (list (hash 'elapsed_ms 5))) (list (hash 'elapsed_ms 5))))

(test-case "a probe copy raises the declaration instead of adding a shadowed one"
  ;; The metadata parser keeps the LAST @timeout in the first 50 lines. Inserting a line
  ;; above an existing declaration would leave the old value in force, so the copy must
  ;; REPLACE it -- otherwise every "uncensored" sample is still taken at the old limit.
  (define fixture (build-path repo-root "tests" ".cl5-probe-fixture.rkt"))
  (call-with-output-file
   fixture
   #:exists 'replace
   (lambda (o)
     (display "#lang racket/base\n\n;; @suite fast\n;; @timeout 120\n\n(module+ test (void))\n" o)))
  (dynamic-wind void
                (lambda ()
                  (define copy (probe-copy-source (path->string fixture) 1800))
                  (check-equal? (full-file-timeout (path->string copy)) 1800)
                  (define text (file->string copy))
                  (check-false (regexp-match? #rx"@timeout 120" text))
                  (delete-file copy))
                (lambda ()
                  (when (file-exists? fixture)
                    (delete-file fixture)))))
