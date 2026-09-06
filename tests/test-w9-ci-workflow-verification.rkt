#lang racket

;; @speed fast
;; @suite ci
;; @boundary integration
;; tests/test-w9-ci-workflow-verification.rkt
;; v0.99.40 W9 (#8526): PR CI and controlled release workflow verification
;;
;; This wave verifies that the remediation works in GitHub, not just locally:
;; 1. CI workflows reference setup-racket composite action
;; 2. release-repair.yml is properly configured for dry-run safety
;; 3. CI workflow structure has required jobs
;; 4. Release workflow has proper gate structure
;; 5. Evidence structure is valid for audit recording

(require rackunit
         racket/string
         racket/match
         racket/file)

;; ---------------------------------------------------------------------------
;; Pure validation logic (no I/O)
;; ---------------------------------------------------------------------------

;; Validate a CI job name is present in the expected list.
(define (validate-ci-job-present job-name expected-jobs)
  (if (member job-name expected-jobs)
      (list 'found job-name)
      (list 'missing job-name)))

;; Validate all required CI jobs are present.
(define (validate-required-ci-jobs actual-jobs required-jobs)
  (define results
    (for/list ([job (in-list required-jobs)])
      (validate-ci-job-present job actual-jobs)))
  (define missing (filter (lambda (r) (eq? (car r) 'missing)) results))
  (if (null? missing)
      (list 'all-present)
      (list 'incomplete (map cadr missing))))

;; Validate setup-racket is referenced in workflow content.
(define (validate-setup-racket-ref workflow-content)
  (if (string-contains? workflow-content "setup-racket")
      (list 'referenced)
      (list 'not-referenced)))

;; Validate release-repair dry-run safety defaults.
(define (validate-repair-default-mode workflow-content)
  ;; Check that default is dry-run
  (if (and (string-contains? workflow-content "default: dry-run")
           (string-contains? workflow-content "mode"))
      (list 'safe-default)
      (list 'unsafe-default)))

;; Validate that a mode choice exists with the three expected options.
(define (validate-repair-mode-options workflow-content)
  (define has-dry-run (string-contains? workflow-content "dry-run"))
  (define has-publish (string-contains? workflow-content "publish"))
  (define has-repair-assets (string-contains? workflow-content "repair-assets"))
  (if (and has-dry-run has-publish has-repair-assets)
      (list 'all-modes-present)
      (list 'missing-modes
            (filter values
                    (list (and (not has-dry-run) "dry-run")
                          (and (not has-publish) "publish")
                          (and (not has-repair-assets) "repair-assets"))))))

;; Validate release.yml uses tag-publish context.
(define (validate-release-context workflow-content)
  (if (string-contains? workflow-content "tag-publish")
      (list 'correct-context)
      (list 'wrong-context)))

;; Validate release asset verification step exists.
(define (validate-asset-verification workflow-content)
  (if (or (string-contains? workflow-content "Verify") (string-contains? workflow-content "asset"))
      (list 'has-verification)
      (list 'no-verification)))

;; Build evidence record structure.
(define (build-evidence-record pr-number ci-run-id repair-run-id setup-racket-ok lint-ok)
  (hash 'pr-number
        pr-number
        'ci-run-id
        ci-run-id
        'repair-run-id
        repair-run-id
        'setup-racket-success
        setup-racket-ok
        'lint-all-pass
        lint-ok
        'timestamp
        (current-seconds)))

;; Validate evidence record has all required fields.
(define (validate-evidence-record record)
  (define required-keys '(pr-number ci-run-id repair-run-id setup-racket-success lint-all-pass))
  (define missing (filter (lambda (k) (not (hash-has-key? record k))) required-keys))
  (if (null? missing)
      (list 'complete)
      (list 'incomplete missing)))

;; ---------------------------------------------------------------------------
;; Tests
;; ---------------------------------------------------------------------------

(define-test-suite
 w9-ci-verification
 ;; ── CI workflow structure tests ──
 (test-case "ci.yml has setup-racket composite action reference"
   (check-true (file-exists? "../.github/workflows/ci.yml"))
   (define content (file->string "../.github/workflows/ci.yml"))
   (check-equal? (validate-setup-racket-ref content) '(referenced)))
 (test-case "release.yml has setup-racket composite action reference"
   (check-true (file-exists? "../.github/workflows/release.yml"))
   (define content (file->string "../.github/workflows/release.yml"))
   (check-equal? (validate-setup-racket-ref content) '(referenced)))
 (test-case "release-repair.yml has setup-racket composite action reference"
   (check-true (file-exists? "../.github/workflows/release-repair.yml"))
   (define content (file->string "../.github/workflows/release-repair.yml"))
   (check-equal? (validate-setup-racket-ref content) '(referenced)))
 ;; ── CI job structure tests ──
 (test-case "ci.yml has required CI jobs"
   (define content (file->string "../.github/workflows/ci.yml"))
   (define required-jobs '("lint:" "test:" "security:" "smoke:" "workflows:" "release-readiness:"))
   (for ([job (in-list required-jobs)])
     (check-true (string-contains? content job) (format "ci.yml should contain job ~a" job))))
 (test-case "required CI job validation logic — all present"
   (define result (validate-required-ci-jobs '("lint" "test" "smoke") '("lint" "test")))
   (check-equal? result '(all-present)))
 (test-case "required CI job validation logic — incomplete"
   (define result (validate-required-ci-jobs '("lint") '("lint" "test" "smoke")))
   (check-equal? result '(incomplete ("test" "smoke"))))
 ;; ── release-repair.yml safety tests ──
 (test-case "release-repair.yml defaults to dry-run"
   (define content (file->string "../.github/workflows/release-repair.yml"))
   (check-equal? (validate-repair-default-mode content) '(safe-default)))
 (test-case "release-repair.yml mode options — only dry-run"
   (define content (file->string "../.github/workflows/release-repair.yml"))
   (check-true (string-contains? content "dry-run") "must contain dry-run")
   ;; Must NOT have publish or repair-assets mode options
   (check-false (string-contains? content "- publish") "must NOT have publish mode option")
   (check-false (string-contains? content "- repair-assets")
                "must NOT have repair-assets mode option"))
 ;; ── release.yml correctness tests ──
 (test-case "release.yml uses tag-publish context"
   (define content (file->string "../.github/workflows/release.yml"))
   (check-equal? (validate-release-context content) '(correct-context)))
 (test-case "release.yml has asset verification"
   (define content (file->string "../.github/workflows/release.yml"))
   (check-equal? (validate-asset-verification content) '(has-verification)))
 ;; ── Evidence record structure tests ──
 (test-case "evidence record — complete"
   (define record (build-evidence-record 8537 5610 5611 #t #t))
   (check-equal? (validate-evidence-record record) '(complete)))
 (test-case "evidence record — incomplete (missing ci-run-id)"
   (define record (make-hash))
   (hash-set! record 'pr-number 100)
   (hash-set! record 'repair-run-id 200)
   (hash-set! record 'setup-racket-success #t)
   (hash-set! record 'lint-all-pass #t)
   (check-equal? (validate-evidence-record record) '(incomplete (ci-run-id))))
 (test-case "evidence record contains timestamp"
   (define record (build-evidence-record 1 2 3 #t #t))
   (check-true (hash-has-key? record 'timestamp)))
 ;; ── CI dependency verification ──
 (test-case "setup-racket composite action exists"
   (check-true (file-exists? (build-path ".." ".github" "actions" "setup-racket" "action.yml"))))
 (test-case "ci-package-setup.rkt exists from v0.99.39"
   (check-true (file-exists? (build-path ".." "scripts" "ci-package-setup.rkt"))))
 (test-case "release-repair.rkt script exists from W7"
   (check-true (file-exists? (build-path ".." "scripts" "release-repair.rkt"))))
  (test-case "release-dry-run.rkt script exists from W3"
    (check-true (file-exists? (build-path ".." "scripts" "release-dry-run.rkt"))))
  ;; ── W0 v1.00.26: required-check graph characterization (integration layer) ──
  ;; Pins, as literal data compiled into this module, the same graph that
  ;; tests/test-ci-runtime-contract.rkt pins against the live policy file and
  ;; the checksummed snapshot: the protected-main gate context list, the
  ;; aggregate needs edges, and fail-closed gate verdict semantics.
  (test-case "w0: required-check graph characterization"
    (define W0-REQUIRED-CHECKS
      '("lint" "security" "release-dry-run" "smoke (ubuntu-latest)"
        "test (0)" "test (1)" "test (2)" "test-aggregate" "test-platform"
        "workflows (0)" "workflows (1)" "workflows-aggregate"))
    (define W0-AGGREGATE-NEEDS
      '(("test-aggregate" ("test (0)" "test (1)" "test (2)" "test-platform"))
        ("workflows-aggregate" ("workflows (0)" "workflows (1)"))))
    (define (w0-verdict required observed)
      (define missing (filter (lambda (n) (not (member n observed))) required))
      (if (null? missing) 'pass (list 'fail missing)))
    (test-case "gate requires the pinned twelve contexts, and only those"
      (check-equal? (length W0-REQUIRED-CHECKS) 12)
      (check-true (andmap (lambda (n) (member n W0-REQUIRED-CHECKS))
                          '("lint" "security" "release-dry-run" "test-aggregate"
                            "test-platform" "workflows-aggregate")))
      (check-false (member "fast-env" W0-REQUIRED-CHECKS))
      (check-false (member "prepared-env-report" W0-REQUIRED-CHECKS))
      (check-false (member "shard-plan-report" W0-REQUIRED-CHECKS))
      (check-false (member "release-readiness" W0-REQUIRED-CHECKS)))
    (test-case "every aggregate job needs its shard jobs"
      (for-each
       (lambda (entry)
         (check-true (member (car entry) W0-REQUIRED-CHECKS)
                     (format "aggregate ~a must itself be required" (car entry)))
         (for-each
          (lambda (shard)
            (check-true (member shard W0-REQUIRED-CHECKS)
                        (format "~a shard ~a must be required" (car entry) shard)))
          (cadr entry)))
       W0-AGGREGATE-NEEDS))
    (test-case "missing any required check fails the gate (fail-closed)"
      (check-equal? (w0-verdict W0-REQUIRED-CHECKS W0-REQUIRED-CHECKS) 'pass)
      (for-each
       (lambda (dropped)
         (check-equal? (w0-verdict W0-REQUIRED-CHECKS (remove dropped W0-REQUIRED-CHECKS))
                       (list 'fail (list dropped))
                       (format "gate must fail closed when ~a is missing" dropped)))
       W0-REQUIRED-CHECKS))))

(module+ test
  (require rackunit/text-ui)
  (run-tests w9-ci-verification))
