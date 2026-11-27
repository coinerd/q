#lang racket

(require json
         rackunit
         racket/file
         racket/runtime-path
         racket/string
         "../util/sha256.rkt")

(define-runtime-path repo-root "..")
(define workload-path (build-path repo-root "docs" "reports" "CL5-P2F-REVISED-WORKLOAD.json"))
(define design-path (build-path repo-root "docs" "reports" "CL5-P2F-REVISED-DESIGN.md"))
(define workflow-path (build-path repo-root ".github" "workflows" "cl5-p2f-pilot.yml"))
(define original-workload-path (build-path repo-root "docs" "reports" "CL5-P2F-WORKLOAD.json"))

(define workload (call-with-input-file workload-path read-json))
(define design (file->string design-path))
(define workflow (file->string workflow-path))

(define expected-files
  '("tests/test-gsd-evidence-repair-production-contract.rkt"
    "tests/test-full-regression-invocation.rkt"
    "tests/test-gsd-delivery-merge.rkt"
    "tests/test-gsd-auto-review-loop-contract.rkt"
    "tests/test-gsd-supplemental-disposition-store.rkt"
    "tests/test-gsd-delivery-read-ladder.rkt"
    "tests/test-milestone-gate.rkt"
    "tests/test-gsd-go-orchestrator.rkt"))

(define (string-index haystack needle)
  (let loop ([i 0])
    (cond
      [(> i (- (string-length haystack) (string-length needle))) #f]
      [(string=? (substring haystack i (+ i (string-length needle))) needle) i]
      [else (loop (add1 i))])))

(define (string-contains* haystack needle)
  (or (string-index haystack needle) (error 'string-contains* "missing needle: ~a" needle)))

(define workload-digest "50fbb772db8c4d6ba9a1f6b6b79a7cf7ed72352acae8ab59deaf214767260a5e")

(define (workflow-allowed-paths)
  (define m (regexp-match #px"allowed='([^']*)'" workflow))
  (map string-trim
       (filter (lambda (s) (not (string=? (string-trim s) "")))
               (string-split (if m
                                 (cadr m)
                                 "")
                             "\n"))))

(test-case "revised workload pins the post-repair tree and frozen eight-file command"
  (check-equal? (hash-ref workload 'schema) "cl5-p2f-workload/2")
  (check-equal? (hash-ref workload 'budget_id) "CL5-P2F-REV1")
  (check-equal? (hash-ref workload 'pinned_test_tree_sha) "dd5c9995fc13bbc1253995768b2d78c4531e0a60")
  (check-equal? (map (lambda (f) (hash-ref f 'path)) (hash-ref workload 'files)) expected-files)
  (for ([f (in-list (hash-ref workload 'files))])
    (check-true (> (hash-ref f 'ci_elapsed_ms) 0))
    (check-equal? (hash-ref f 'ci_status) "pass"))
  (let ([cmd (hash-ref workload 'command)])
    (check-equal? (hash-ref cmd 'jobs) 4)
    (check-equal? (hash-ref cmd 'warmups) 1)
    (check-equal? (hash-ref cmd 'measured_runs) 5)))

(test-case "revised workload pins the measured production host"
  (let ([h (hash-ref workload 'expected_host)])
    (check-equal? (hash-ref h 'os_version_id) "26.04")
    (check-equal? (hash-ref h 'image_os) "ubuntu26")
    (check-equal? (hash-ref h 'image_version) "20261122.9.1")
    (check-equal? (hash-ref h 'cores) 4)
    (check-equal? (hash-ref h 'cpu_class_regex) "AMD EPYC")
    (check-not-false (member "AMD EPYC 7763" (hash-ref h 'observed_production_cpu_models)))
    (check-equal? (hash-ref h 'racket_exe_sha256)
                  "7674f17865fa795b21641828d839e689e01a2f17bbdd2b7d71cc59cceafa873c")))

(test-case "original pilot workload remains untouched"
  (check-equal? (sha256-hex (file->bytes original-workload-path))
                "f06a19df9aef5840ad5314715ddde8f3f866ed8989980ae9ce4019f92e118bb4"))

(test-case "pre-registration names the budget, host gate and no-retry rule"
  (for ([needle (in-list '("CL5-P2F-REV1"
                           "dd5c9995fc13bbc1253995768b2d78c4531e0a60"
                           "20261122.9.1"
                           "7674f17865fa795b21641828d839e689e01a2f17bbdd2b7d71cc59cceafa873c"
                           "Validate measured host"
                           "AMD EPYC 7763"
                           "must not be retried"))])
    (check-true (string-contains? design needle) needle)))

(test-case "deployed workflow validates host before materialising the workload"
  (check-true (string-contains? workflow "Validate measured host before measurement"))
  (check-true (string-contains? workflow "Materialise the frozen workload"))
  (check-true (< (string-contains* workflow "Validate measured host before measurement")
                 (string-contains* workflow "Materialise the frozen workload")))
  (check-true (string-contains? workflow "budget-id"))
  (check-true (string-contains? workflow "prior-failed-run-id"))
  (check-true (string-contains? workflow "prior-failure-signature"))
  (check-true (string-contains? workflow "test \"$BUDGET_ID\" = 'CL5-P2F-REV1'"))
  (check-true (string-contains?
               workflow
               "racket scripts/run-tests/runner.rkt --jobs 4 --json-out cl5-p2f/run-$i.json"))
  (check-true (string-contains? workflow "--jobs 4 --json-out \"cl5-p2f/run-$i.json\""))
  (check-true (string-contains? workflow "cl5-p2f-pilot-rev1")))

(test-case "workflow tree pin, digest and trigger stay aligned with the frozen workload"
  (check-equal? (workflow-allowed-paths) (hash-ref workload 'allowed_tree_diff_paths))
  (check-equal? (sha256-hex (file->bytes workload-path)) workload-digest)
  (check-true (string-contains? workflow workload-digest))
  (check-true (string-contains? workflow "timeout-minutes: 120"))
  (define cpu-regex (hash-ref (hash-ref workload 'expected_host) 'cpu_class_regex))
  (check-true (string-contains? workflow (format "*'~a '*" cpu-regex)))
  (check-true (string-contains? workflow "on:\n  workflow_dispatch:"))
  (check-false (regexp-match? #px"(?m:^  (push|pull_request):)" workflow)))
