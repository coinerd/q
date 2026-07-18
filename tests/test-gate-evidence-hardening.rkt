#lang racket

;; F-15 #8753: Gate evidence and release governance hardening tests.
;; Red-first: strict gate evidence must reject wrong SHA, non-zero
;; failed/timed-out, zero test count, stale time.

(require rackunit
         racket/file
         racket/port
         racket/string
         racket/runtime-path)

(define-runtime-path script-path "../scripts/lint-release-readiness.rkt")

(test-case "validate-gate-evidence-entry: rejects wrong git_sha"
  (define validate (dynamic-require script-path 'validate-gate-evidence-entry))
  (define-values (pass? detail)
    (validate (hasheq 'version
                      "0.99.51"
                      'git_sha
                      "abc123"
                      'timestamp
                      (current-seconds)
                      'parsed_test_count
                      10
                      'failed
                      0
                      'timed_out
                      0)
              "0.99.51"
              "def456"
              (current-seconds)))
  (check-false pass?)
  (check-true (string-contains? detail "sha") detail))

(test-case "validate-gate-evidence-entry: rejects non-zero failed"
  (define validate (dynamic-require script-path 'validate-gate-evidence-entry))
  (define now (current-seconds))
  (define-values (pass? detail)
    (validate (hasheq 'version
                      "0.99.51"
                      'git_sha
                      "abc123"
                      'timestamp
                      now
                      'parsed_test_count
                      10
                      'failed
                      1
                      'timed_out
                      0)
              "0.99.51"
              "abc123"
              now))
  (check-false pass?)
  (check-true (string-contains? detail "fail") detail))

(test-case "validate-gate-evidence-entry: rejects non-zero timed_out"
  (define validate (dynamic-require script-path 'validate-gate-evidence-entry))
  (define now (current-seconds))
  (define-values (pass? detail)
    (validate (hasheq 'version
                      "0.99.51"
                      'git_sha
                      "abc123"
                      'timestamp
                      now
                      'parsed_test_count
                      10
                      'failed
                      0
                      'timed_out
                      1)
              "0.99.51"
              "abc123"
              now))
  (check-false pass?)
  (check-true (or (string-contains? detail "timeout") (string-contains? detail "timed")) detail))

(test-case "validate-gate-evidence-entry: rejects zero parsed_test_count"
  (define validate (dynamic-require script-path 'validate-gate-evidence-entry))
  (define now (current-seconds))
  (define-values (pass? detail)
    (validate (hasheq 'version
                      "0.99.51"
                      'git_sha
                      "abc123"
                      'timestamp
                      now
                      'parsed_test_count
                      0
                      'failed
                      0
                      'timed_out
                      0)
              "0.99.51"
              "abc123"
              now))
  (check-false pass?)
  (check-true (string-contains? detail "zero") detail))

(test-case "validate-gate-evidence-entry: rejects stale timestamp"
  (define validate (dynamic-require script-path 'validate-gate-evidence-entry))
  (define old-time (- (current-seconds) 8000))
  (define-values (pass? detail)
    (validate (hasheq 'version
                      "0.99.51"
                      'git_sha
                      "abc123"
                      'timestamp
                      old-time
                      'parsed_test_count
                      10
                      'failed
                      0
                      'timed_out
                      0)
              "0.99.51"
              "abc123"
              (current-seconds)))
  (check-false pass?)
  (check-true (string-contains? detail "stale") detail))

(test-case "validate-gate-evidence-entry: accepts fully valid evidence"
  (define validate (dynamic-require script-path 'validate-gate-evidence-entry))
  (define now (current-seconds))
  (define-values (pass? detail)
    (validate (hasheq 'version
                      "0.99.51"
                      'git_sha
                      "abc123"
                      'timestamp
                      now
                      'parsed_test_count
                      100
                      'failed
                      0
                      'timed_out
                      0)
              "0.99.51"
              "abc123"
              now))
  (check-true pass? detail))

(test-case "validate-gate-evidence-entry: rejects wrong version"
  (define validate (dynamic-require script-path 'validate-gate-evidence-entry))
  (define now (current-seconds))
  (define-values (pass? detail)
    (validate (hasheq 'version
                      "0.0.1"
                      'git_sha
                      "abc123"
                      'timestamp
                      now
                      'parsed_test_count
                      10
                      'failed
                      0
                      'timed_out
                      0)
              "0.99.51"
              "abc123"
              now))
  (check-false pass?)
  (check-true (string-contains? detail "version") detail))
