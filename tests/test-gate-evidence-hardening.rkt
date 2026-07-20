#lang racket

;; F-15 #8753: Gate evidence and release governance hardening tests.
;; Red-first: strict gate evidence must reject wrong SHA, non-zero
;; failed/timed-out, zero test count, stale time.

(require rackunit
         racket/file
         racket/port
         racket/string
         racket/runtime-path)

(define full-sha "abcdef0123456789abcdef0123456789abcdef01")
(define other-sha "fedcba9876543210fedcba9876543210fedcba98")

(define-runtime-path script-path "../scripts/lint-release-readiness.rkt")

(test-case "validate-gate-evidence-entry: rejects wrong git_sha"
  (define validate (dynamic-require script-path 'validate-gate-evidence-entry))
  (define-values (pass? detail)
    (validate (hasheq 'version
                      "0.99.51"
                      'git_sha
                      other-sha
                      'timestamp
                      (current-seconds)
                      'parsed_test_count
                      10
                      'failed
                      0
                      'timed_out
                      0)
              "0.99.51"
              full-sha
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
                      full-sha
                      'timestamp
                      now
                      'parsed_test_count
                      10
                      'failed
                      1
                      'timed_out
                      0)
              "0.99.51"
              full-sha
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
                      full-sha
                      'timestamp
                      now
                      'parsed_test_count
                      10
                      'failed
                      0
                      'timed_out
                      1)
              "0.99.51"
              full-sha
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
                      full-sha
                      'timestamp
                      now
                      'parsed_test_count
                      0
                      'failed
                      0
                      'timed_out
                      0)
              "0.99.51"
              full-sha
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
                      full-sha
                      'timestamp
                      old-time
                      'parsed_test_count
                      10
                      'failed
                      0
                      'timed_out
                      0)
              "0.99.51"
              full-sha
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
                      full-sha
                      'timestamp
                      now
                      'parsed_test_count
                      100
                      'failed
                      0
                      'timed_out
                      0)
              "0.99.51"
              full-sha
              now))
  (check-true pass? detail))

(test-case "validate-gate-evidence-entry: rejects short SHA (< 40 chars)"
  (define validate (dynamic-require script-path 'validate-gate-evidence-entry))
  (define now (current-seconds))
  (define-values (pass? detail)
    (validate (hasheq 'version
                      "0.99.51"
                      'git_sha
                      "shortsha"
                      'timestamp
                      now
                      'parsed_test_count
                      10
                      'failed
                      0
                      'timed_out
                      0)
              "0.99.51"
              full-sha
              now))
  (check-false pass?)
  (check-true (string-contains? detail "40") detail))

(test-case "validate-gate-evidence-entry: rejects future timestamp"
  (define validate (dynamic-require script-path 'validate-gate-evidence-entry))
  (define future-time (+ (current-seconds) 600))
  (define-values (pass? detail)
    (validate (hasheq 'version
                      "0.99.51"
                      'git_sha
                      full-sha
                      'timestamp
                      future-time
                      'parsed_test_count
                      10
                      'failed
                      0
                      'timed_out
                      0)
              "0.99.51"
              full-sha
              (current-seconds)))
  (check-false pass?)
  (check-true (string-contains? detail "future") detail))

(test-case "validate-gate-evidence-entry: rejects unknown sha"
  (define validate (dynamic-require script-path 'validate-gate-evidence-entry))
  (define now (current-seconds))
  (define-values (pass? detail)
    (validate (hasheq 'version
                      "0.99.51"
                      'git_sha
                      "unknown"
                      'timestamp
                      now
                      'parsed_test_count
                      10
                      'failed
                      0
                      'timed_out
                      0)
              "0.99.51"
              full-sha
              now))
  (check-false pass?)
  (check-true (string-contains? detail "unknown") detail))

(test-case "validate-gate-evidence-entry: rejects wrong version"
  (define validate (dynamic-require script-path 'validate-gate-evidence-entry))
  (define now (current-seconds))
  (define-values (pass? detail)
    (validate (hasheq 'version
                      "0.0.1"
                      'git_sha
                      full-sha
                      'timestamp
                      now
                      'parsed_test_count
                      10
                      'failed
                      0
                      'timed_out
                      0)
              "0.99.51"
              full-sha
              now))
  (check-false pass?)
  (check-true (string-contains? detail "version") detail))
