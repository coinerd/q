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
(define-runtime-path required-check-policy "../scripts/required-pr-checks.policy")

(define valid-sha "0123456789abcdef0123456789abcdef01234567")
(define valid-digest "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa")

(define required-checks
  '("lint" "lint-alignment"
           "security"
           "release-dry-run"
           "inter-wave-gate"
           "workflows"
           "smoke (ubuntu-latest)"
           "smoke (macos-latest)"
           "test (ubuntu-latest, 8.10)"
           "test (ubuntu-latest, 8.11)"
           "test (macos-latest, 8.10)"
           "gsd-governance"))

(define (write-datum path datum)
  (make-parent-directory* path)
  (call-with-output-file path (lambda (out) (write datum out)) #:exists 'replace))

(define (valid-review)
  (hash 'reviewer
        "independent-pi-reviewer"
        'verdict
        "APPROVED"
        'reviewed-sha
        valid-sha
        'content-digest
        valid-digest
        'timestamp
        "2026-07-15T12:00:00Z"
        'scope
        "W0 F-10 F-13"
        'report
        "Independent review found no blockers."))

(define (valid-validation)
  (hash 'status
        "current"
        'milestone
        837
        'wave
        "W0"
        'issue
        8730
        'branch
        "feature/v09951-w0-gsd-merge-enforcement"
        'implementation-sha
        valid-sha
        'content-digest
        valid-digest
        'red-first
        (hash 'command
              "raco test tests/test-gsd-governance-workflow.rkt"
              'failure
              "missing evidence accepted")
        'focused-tests
        (hash 'command "raco test governance-tests" 'result "passed" 'tests 132)
        'format-compile
        (hash 'result "passed")
        'lint
        (hash 'result "passed")
        'fast
        (hash 'result "passed" 'files 1021 'tests 15159)
        'review-artifact
        "reviews/w0.rktd"
        'remaining-items
        '()
        'planning-sync
        "current"))

(define (valid-evidence)
  (hash 'schema-version
        2
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
        'content-digest
        valid-digest
        'review-artifact
        "reviews/w0.rktd"
        'validation-artifact
        "validation/w0.rktd"
        'required-checks
        required-checks))

(define (with-fixture proc)
  (define root (make-temporary-file "gsd-wave-gate-~a" 'directory))
  (dynamic-wind void
                (lambda ()
                  (write-datum (build-path root "policy.rktd") required-checks)
                  (write-datum (build-path root "reviews" "w0.rktd") (valid-review))
                  (write-datum (build-path root "validation" "w0.rktd") (valid-validation))
                  (write-datum (build-path root "evidence.rktd") (valid-evidence))
                  (proc root))
                (lambda () (delete-directory/files root))))

(define (validate-in root [evidence (valid-evidence)] #:digest [digest valid-digest])
  (validate-wave-evidence evidence
                          #:root root
                          #:actual-content-digest digest
                          #:policy (build-path root "policy.rktd")))

(test-case "complete authentic wave evidence passes"
  (with-fixture (lambda (root) (check-true (wave-evidence-result-passed? (validate-in root))))))

(test-case "non-hash evidence fails closed"
  (with-fixture (lambda (root) (check-false (wave-evidence-result-passed? (validate-in root '()))))))

(for ([required-key (in-list '(schema-version milestone
                                              wave
                                              issue
                                              status
                                              implementation-sha
                                              content-digest
                                              review-artifact
                                              validation-artifact
                                              required-checks))])
  (test-case (format "missing ~a fails closed" required-key)
    (with-fixture (lambda (root)
                    (check-false (wave-evidence-result-passed?
                                  (validate-in root (hash-remove (valid-evidence) required-key))))))))

(test-case "fabricated missing review path fails closed"
  (with-fixture (lambda (root)
                  (define evidence
                    (hash-set (valid-evidence) 'review-artifact "reviews/missing.rktd"))
                  (check-false (wave-evidence-result-passed? (validate-in root evidence))))))

(test-case "non-APPROVED retained review fails closed"
  (with-fixture (lambda (root)
                  (write-datum (build-path root "reviews" "w0.rktd")
                               (hash-set (valid-review) 'verdict "REQUEST_CHANGES"))
                  (check-false (wave-evidence-result-passed? (validate-in root))))))

(test-case "review bound to stale implementation SHA fails closed"
  (with-fixture
   (lambda (root)
     (write-datum (build-path root "reviews" "w0.rktd")
                  (hash-set (valid-review) 'reviewed-sha "ffffffffffffffffffffffffffffffffffffffff"))
     (check-false (wave-evidence-result-passed? (validate-in root))))))

(test-case "actual changed-content digest mismatch fails closed"
  (with-fixture
   (lambda (root)
     (check-false
      (wave-evidence-result-passed?
       (validate-in root
                    #:digest "bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"))))))

(test-case "missing validation field fails closed"
  (with-fixture (lambda (root)
                  (write-datum (build-path root "validation" "w0.rktd")
                               (hash-remove (valid-validation) 'planning-sync))
                  (check-false (wave-evidence-result-passed? (validate-in root))))))

(test-case "Not started validation status fails closed"
  (with-fixture (lambda (root)
                  (write-datum (build-path root "validation" "w0.rktd")
                               (hash-set (valid-validation) 'status "Not started"))
                  (check-false (wave-evidence-result-passed? (validate-in root))))))

(test-case "incomplete required-check inventory fails closed"
  (with-fixture (lambda (root)
                  (define evidence (hash-set (valid-evidence) 'required-checks '("lint")))
                  (check-false (wave-evidence-result-passed? (validate-in root evidence))))))

(test-case "evidence file round-trips with authentic artifacts"
  (with-fixture (lambda (root)
                  (check-true (wave-evidence-result-passed?
                               (validate-wave-evidence-file (build-path root "evidence.rktd")
                                                            #:root root
                                                            #:actual-content-digest valid-digest
                                                            #:policy
                                                            (build-path root "policy.rktd")))))))

(test-case "missing evidence file fails closed"
  (with-fixture (lambda (root)
                  (check-false (wave-evidence-result-passed?
                                (validate-wave-evidence-file (build-path root "missing.rktd")
                                                             #:root root
                                                             #:actual-content-digest valid-digest
                                                             #:policy
                                                             (build-path root "policy.rktd")))))))

(test-case "malformed evidence file fails closed"
  (with-fixture (lambda (root)
                  (call-with-output-file (build-path root "evidence.rktd")
                                         (lambda (out) (display "(" out))
                                         #:exists 'replace)
                  (check-false (wave-evidence-result-passed?
                                (validate-wave-evidence-file (build-path root "evidence.rktd")
                                                             #:root root
                                                             #:actual-content-digest valid-digest
                                                             #:policy
                                                             (build-path root "policy.rktd")))))))

(test-case "repository required-check policy includes governance and full matrix"
  (define policy (call-with-input-file required-check-policy read))
  (check-equal? policy required-checks))

(test-case "CI binds PR and main-push evidence to the changed diff"
  (define workflow (file->string ci-workflow))
  (check-true (string-contains? workflow "  gsd-governance:"))
  (check-true (string-contains? workflow "github.event.pull_request.head.sha"))
  (check-true (string-contains? workflow "github.event.before"))
  (check-true (string-contains? workflow "scripts/gsd-wave-gate.rkt"))
  (check-true (string-contains? workflow "docs/reports/gsd-wave-evidence"))
  (check-false (string-contains? workflow "sort | tail -1")))
