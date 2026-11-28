#lang racket

(require json
         rackunit
         racket/file
         racket/runtime-path
         racket/string
         "../scripts/cl5/contract-escalation.rkt"
         "../scripts/cl5/paired-pilot-selection.rkt")

(define-runtime-path repo-root "..")
(define workload-path (build-path repo-root "docs" "reports" "CL5-W3-PAIRED-PILOT-WORKLOAD.json"))
(define design-path (build-path repo-root "docs" "reports" "CL5-W3-PAIRED-PILOT-DESIGN.md"))
(define workflow-path (build-path repo-root ".github" "workflows" "cl5-w3-paired-pilot.yml"))
(define workload (call-with-input-file workload-path read-json))
(define design (file->string design-path))
(define workflow (file->string workflow-path))

(define (string-index haystack needle)
  (let loop ([i 0])
    (cond
      [(> i (- (string-length haystack) (string-length needle))) #f]
      [(string=? (substring haystack i (+ i (string-length needle))) needle) i]
      [else (loop (add1 i))])))

(test-case "paired workload freezes one real decision and the reduced-surface projection"
  (check-equal? (hash-ref workload 'schema) "cl5-w3-paired-pilot/1")
  (check-equal? (hash-ref workload 'pinned_test_tree_sha) "ecb92a5e846a48ee710d9e5b9bed9ca0b0631d5b")
  (define c (hash-ref workload 'case))
  (check-equal? (hash-ref c 'changed_files) '("extensions/gsd/wave-status.rkt"))
  (check-equal? (hash-ref c 'expected_verdict) "escalate")
  (check-equal? (hash-ref c 'expected_escalations) '("no-consumer-detector"))
  (define comprehensive (hash-ref workload 'comprehensive_reduced_files))
  (define selected (hash-ref workload 'selected_projected_files))
  (check-equal? (length comprehensive) 8)
  (check-equal? (length selected) 2)
  (check-true (andmap (lambda (f) (and (member f comprehensive) #t)) selected))
  (check-equal? (hash-ref (hash-ref workload 'schedule) 'order)
                '("selected-comprehensive" "comprehensive-selected" "selected-comprehensive"))
  (check-equal? (hash-ref (hash-ref workload 'command) 'jobs) 4))

(test-case "contract decision digest and projection are re-derivable"
  (define decision
    (evaluate-contract-changes repo-root (hash-ref (hash-ref workload 'case) 'changed_files)))
  (check-equal? (decision-digest decision) (hash-ref (hash-ref workload 'case) 'decision_sha256))
  (check-equal? (project-selection (hash-ref decision 'required)
                                   (hash-ref workload 'comprehensive_reduced_files))
                (hash-ref workload 'selected_projected_files)))

(test-case "pre-registration rejects cross-run timing equivalence and campaign claims"
  (for ([needle (in-list '("methodological pilot only" "production-equivalent"
                                                       "No threshold is applied to `S`"
                                                       "Not authorized: P2-F retry"
                                                       "40 % claim"))])
    (check-true (string-contains? design needle) needle))
  (check-false (string-contains? design "A4 comparability")))

(test-case "deployed workflow validates host before running tests and has no repeat"
  (define validate (string-index workflow "Validate measured host before measurement"))
  (define materialise (string-index workflow "Materialise the frozen reduced surface"))
  (define runner (string-index workflow "racket scripts/run-tests/runner.rkt"))
  (check-not-false validate)
  (check-not-false materialise)
  (check-not-false runner)
  (check-true (< validate materialise runner))
  (check-true (string-contains? workflow "on:\n  workflow_dispatch:"))
  (check-false (regexp-match? #px"(?m:^  (push|pull_request):)" workflow))
  (check-true (string-contains? workflow "--jobs 4"))
  (for ([needle (in-list '("selected warm0" "comprehensive warm0"
                                            "selected pair1"
                                            "comprehensive pair1"
                                            "comprehensive pair2"
                                            "selected pair2"
                                            "selected pair3"
                                            "comprehensive pair3"))])
    (check-true (string-contains? workflow needle) needle))
  (check-false (regexp-match? #px"(?i:retry)" workflow)))
