#lang racket/base

;; @speed fast
;; @suite default
;; @isolation temp-dir

(require rackunit
         racket/file
         racket/runtime-path
         racket/string
         "../scripts/gsd-wave-gate.rkt")

(define-runtime-path ci-workflow "../.github/workflows/ci.yml")

(define valid-sha "0123456789abcdef0123456789abcdef01234567")

(define (valid-evidence)
  (hash 'schema-version
        1
        'milestone
        837
        'wave
        "W0"
        'issue
        8730
        'status
        "ready-for-merge"
        'implementation-sha
        valid-sha
        'red-first
        (hash 'command
              "raco test tests/test-gsd-governance-workflow.rkt"
              'failure
              "missing evidence accepted")
        'validation
        (hash 'path ".planning/VALIDATION-v0.99.51-v0.99.50-AUDIT-REMEDIATION.md" 'status "current")
        'review
        (hash 'reviewer
              "independent-pi-reviewer"
              'verdict
              "APPROVED"
              'artifact
              "docs/reports/gsd-wave-reviews/v0.99.51-w0.md"
              'reviewed-sha
              valid-sha)
        'required-checks
        '("lint" "security" "inter-wave-gate" "gsd-governance")))

(test-case "complete wave evidence passes"
  (check-true (wave-evidence-result-passed? (validate-wave-evidence (valid-evidence)))))

(test-case "non-hash evidence fails closed"
  (check-false (wave-evidence-result-passed? (validate-wave-evidence '()))))

(for ([required-key (in-list '(schema-version milestone
                                              wave
                                              issue
                                              status
                                              implementation-sha
                                              red-first
                                              validation
                                              review
                                              required-checks))])
  (test-case (format "missing ~a fails closed" required-key)
    (define result (validate-wave-evidence (hash-remove (valid-evidence) required-key)))
    (check-false (wave-evidence-result-passed? result))))

(test-case "Not started validation status fails closed"
  (define evidence
    (hash-set (valid-evidence) 'validation (hash 'path "validation.md" 'status "Not started")))
  (check-false (wave-evidence-result-passed? (validate-wave-evidence evidence))))

(test-case "missing independent review artifact fails closed"
  (define evidence
    (hash-set (valid-evidence)
              'review
              (hash 'reviewer "reviewer" 'verdict "APPROVED" 'reviewed-sha valid-sha)))
  (check-false (wave-evidence-result-passed? (validate-wave-evidence evidence))))

(test-case "non-APPROVED review fails closed"
  (define review (hash-set (hash-ref (valid-evidence) 'review) 'verdict "REQUEST_CHANGES"))
  (check-false (wave-evidence-result-passed?
                (validate-wave-evidence (hash-set (valid-evidence) 'review review)))))

(test-case "review bound to stale implementation SHA fails closed"
  (define review
    (hash-set (hash-ref (valid-evidence) 'review)
              'reviewed-sha
              "ffffffffffffffffffffffffffffffffffffffff"))
  (check-false (wave-evidence-result-passed?
                (validate-wave-evidence (hash-set (valid-evidence) 'review review)))))

(test-case "empty required-check set fails closed"
  (check-false (wave-evidence-result-passed?
                (validate-wave-evidence (hash-set (valid-evidence) 'required-checks '())))))

(test-case "malformed SHA fails closed"
  (check-false (wave-evidence-result-passed?
                (validate-wave-evidence (hash-set (valid-evidence) 'implementation-sha "deadbeef")))))

(test-case "evidence file round-trips"
  (define path (make-temporary-file "wave-evidence-~a.rktd"))
  (call-with-output-file path (lambda (out) (write (valid-evidence) out)) #:exists 'replace)
  (check-true (wave-evidence-result-passed? (validate-wave-evidence-file path)))
  (delete-file path))

(test-case "missing evidence file fails closed"
  (define path (make-temporary-file "wave-evidence-missing-~a.rktd"))
  (delete-file path)
  (check-false (wave-evidence-result-passed? (validate-wave-evidence-file path))))

(test-case "malformed evidence file fails closed"
  (define path (make-temporary-file "wave-evidence-bad-~a.rktd"))
  (call-with-output-file path (lambda (out) (display "(" out)) #:exists 'replace)
  (check-false (wave-evidence-result-passed? (validate-wave-evidence-file path)))
  (delete-file path))

(test-case "CI defines required gsd-governance PR check"
  (define workflow (file->string ci-workflow))
  (check-true (string-contains? workflow "  gsd-governance:"))
  (check-true (string-contains? workflow "scripts/gsd-wave-gate.rkt"))
  (check-true (string-contains? workflow "docs/reports/gsd-wave-evidence")))
