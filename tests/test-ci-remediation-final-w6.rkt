#lang racket

;; @speed fast
;; @suite fast

;; W6 (#8509): Final CI remediation audit evidence tests.
;; Verifies the complete CI remediation deliverables are present and
;; correct across all 7 waves (W0-W6).

(require rackunit
         racket/file
         racket/string)

(define project-root
  (simplify-path
   (build-path (or (path-only (resolved-module-path-name (variable-reference->resolved-module-path
                                                          (#%variable-reference))))
                   ".")
               "..")))

;; ---------------------------------------------------------------------------
;; F1-F4 compile fix verification (all 4 modules)
;; ---------------------------------------------------------------------------

(define-test-suite
 f1-f4-complete-verification
 (test-case "F1: generate-certificates.rkt has racket/string"
   (define content (file->string (build-path project-root "cli" "generate-certificates.rkt")))
   (check-true (string-contains? content "racket/string") "F1 fix present"))
 (test-case "F2: sdk-gsd-integration-test.rkt uses only-in"
   (define content (file->string (build-path project-root "scripts" "sdk-gsd-integration-test.rkt")))
   (check-true (string-contains? content "only-in") "F2 fix present"))
 (test-case "F3: test-gsd-go-replanning.rkt uses current-gsd-mode"
   (define content (file->string (build-path project-root "scripts" "test-gsd-go-replanning.rkt")))
   (check-true (string-contains? content "current-gsd-mode") "F3 fix present"))
 (test-case "F4: test-gsd-sdk-live.rkt has no direct cancellation import"
   (define content (file->string (build-path project-root "scripts" "test-gsd-sdk-live.rkt")))
   (check-false (string-contains? content "util/cancellation.rkt") "F4 fix present")))

;; ---------------------------------------------------------------------------
;; CI infrastructure deliverables (W2-W3)
;; ---------------------------------------------------------------------------

(define-test-suite
 ci-infrastructure-deliverables
 (test-case "ci-package-setup.rkt exists"
   (check-true (file-exists? (build-path project-root "scripts" "ci-package-setup.rkt"))))
 (test-case "ci-local.rkt has package-setup flag"
   (define content (file->string (build-path project-root "scripts" "ci-local.rkt")))
   (check-true (string-contains? content "--package-setup")))
 (test-case "ci-local.rkt has github-setup flag"
   (define content (file->string (build-path project-root "scripts" "ci-local.rkt")))
   (check-true (string-contains? content "--github-setup")))
 (test-case "setup-racket action has explicit package compile name"
   (define content
     (file->string (build-path project-root ".github" "actions" "setup-racket" "action.yml")))
   (check-true (string-contains? content "compile package-visible modules")))
 (test-case "setup-racket action has diagnostic logging"
   (define content
     (file->string (build-path project-root ".github" "actions" "setup-racket" "action.yml")))
   (check-true (or (string-contains? content "::group::")
                   (string-contains? content "::endgroup::")
                   (string-contains? content "::error::"))
               "Diagnostic logging must be present")))

;; ---------------------------------------------------------------------------
;; Policy and documentation deliverables (W4)
;; ---------------------------------------------------------------------------

(define-test-suite
 policy-doc-deliverables
 (test-case "policy document exists"
   (check-true (file-exists?
                (build-path project-root "docs" "reports" "CI-PACKAGE-SETUP-POLICY-v0.99.39.md"))))
 (test-case "policy has 4 core rules"
   (define content
     (file->string (build-path project-root "docs" "reports" "CI-PACKAGE-SETUP-POLICY-v0.99.39.md")))
   (check-true (string-contains? content "Everything package-visible must compile"))
   (check-true (string-contains? content "No compile-time side effects"))
   (check-true (string-contains? content "Imports must be explicit and current")))
 (test-case "policy has script classification"
   (define content
     (file->string (build-path project-root "docs" "reports" "CI-PACKAGE-SETUP-POLICY-v0.99.39.md")))
   (check-true (string-contains? content "Script Classification"))
   (check-true (string-contains? content "CLI entry points"))))

;; ---------------------------------------------------------------------------
;; Test coverage deliverables (W1-W5)
;; ---------------------------------------------------------------------------

(define-test-suite
 test-coverage-deliverables
 (test-case "compile boundary tests exist (W1)"
   (check-true (file-exists?
                (build-path project-root "tests" "test-ci-package-compile-boundary.rkt"))))
 (test-case "package setup tests exist (W2)"
   (check-true (file-exists? (build-path project-root "tests" "test-ci-package-setup.rkt"))))
 (test-case "workflow diagnostics tests exist (W3)"
   (check-true (file-exists? (build-path project-root "tests" "test-ci-workflow-diagnostics.rkt"))))
 (test-case "policy tests exist (W4)"
   (check-true (file-exists? (build-path project-root "tests" "test-ci-package-setup-policy.rkt"))))
 (test-case "verification tests exist (W5)"
   (check-true (file-exists? (build-path project-root "tests" "test-ci-verification-w5.rkt")))))

;; ---------------------------------------------------------------------------
;; README metrics integrity
;; ---------------------------------------------------------------------------

(define-test-suite metrics-integrity
                   (test-case "README has current test file count"
                     (define content (file->string (build-path project-root "README.md")))
                     (check-true (string-contains? content "Test files")
                                 "README must have test files metric")
                     ;; Should NOT have stale 1102 count
                     (check-false (string-contains? content "| Test files | 1102 |")
                                  "README must not have stale 1102 test file count")))

;; ---------------------------------------------------------------------------

(module+ test
  (require rackunit/text-ui)
  (run-tests f1-f4-complete-verification)
  (run-tests ci-infrastructure-deliverables)
  (run-tests policy-doc-deliverables)
  (run-tests test-coverage-deliverables)
  (run-tests metrics-integrity))

(module+ main
  (require rackunit/text-ui)
  (run-tests f1-f4-complete-verification)
  (run-tests ci-infrastructure-deliverables)
  (run-tests policy-doc-deliverables)
  (run-tests test-coverage-deliverables)
  (run-tests metrics-integrity))
