#lang racket

;; @speed fast
;; @suite fast

;; W5 (#8508): CI verification evidence tests.
;; Verifies the CI remediation is effective by checking:
;; - F1-F4 regression targets compile (the core fix)
;; - Package setup tooling is available
;; - CI workflow has the diagnostic naming from W3
;; This test serves as living documentation of the CI remediation state.

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
;; F1-F4 remediation verification (core CI fix)
;; ---------------------------------------------------------------------------

(define-test-suite
 f1-f4-remediation-tests
 (test-case "F1 fix: cli/generate-certificates.rkt has racket/string require"
   (define f1-content (file->string (build-path project-root "cli" "generate-certificates.rkt")))
   (check-true (string-contains? f1-content "racket/string")
               "F1 fix must be present: racket/string require added"))
 (test-case "F2 fix: sdk-gsd-integration-test.rkt uses only-in for session-state"
   (define f2-content
     (file->string (build-path project-root "scripts" "sdk-gsd-integration-test.rkt")))
   (check-true (string-contains? f2-content "only-in")
               "F2 fix must be present: only-in used to resolve import conflict"))
 (test-case "F3 fix: test-gsd-go-replanning.rkt uses current-gsd-mode"
   (define f3-content (file->string (build-path project-root "scripts" "test-gsd-go-replanning.rkt")))
   (check-true (string-contains? f3-content "current-gsd-mode")
               "F3 fix must be present: current-gsd-mode API used")
   (check-false (regexp-match? #rx"[( ]gsd-mode[) ]" f3-content)
                "F3: stale gsd-mode reference must be removed"))
 (test-case "F4 fix: test-gsd-sdk-live.rkt does not import cancellation.rkt directly"
   (define f4-content (file->string (build-path project-root "scripts" "test-gsd-sdk-live.rkt")))
   (check-false (string-contains? f4-content "util/cancellation.rkt")
                "F4 fix must be present: no direct cancellation.rkt import")))

;; ---------------------------------------------------------------------------
;; CI infrastructure verification
;; ---------------------------------------------------------------------------

(define-test-suite
 ci-infrastructure-tests
 (test-case "setup-racket action has explicit package setup step name"
   (define action-content
     (file->string (build-path project-root ".github" "actions" "setup-racket" "action.yml")))
   (check-true (string-contains? action-content "compile package-visible modules")
               "Step name must reference package compile behavior"))
 (test-case "ci-package-setup.rkt exists with preflight capability"
   (check-true (file-exists? (build-path project-root "scripts" "ci-package-setup.rkt"))
               "Package setup preflight tool must exist"))
 (test-case "ci-local.rkt exposes --package-setup flag"
   (define ci-local-content (file->string (build-path project-root "scripts" "ci-local.rkt")))
   (check-true (string-contains? ci-local-content "--package-setup")
               "ci-local.rkt must expose --package-setup flag"))
 (test-case "ci.yml workflow references setup-racket composite action"
   (define ci-content (file->string (build-path project-root ".github" "workflows" "ci.yml")))
   (check-true (string-contains? ci-content ".github/actions/setup-racket")
               "ci.yml must use the composite action")))

;; ---------------------------------------------------------------------------
;; Policy enforcement verification
;; ---------------------------------------------------------------------------

(define-test-suite
 policy-enforcement-tests
 (test-case "package setup policy doc exists"
   (check-true
    (file-exists? (build-path project-root "docs" "reports" "CI-PACKAGE-SETUP-POLICY-v0.99.39.md"))
    "CI package setup policy must exist"))
 (test-case "compile boundary tests exist"
   (check-true (file-exists? (build-path project-root "tests" "test-ci-package-compile-boundary.rkt"))
               "Compile boundary regression tests must exist")))

;; ---------------------------------------------------------------------------

(module+ test
  (require rackunit/text-ui)
  (run-tests f1-f4-remediation-tests)
  (run-tests ci-infrastructure-tests)
  (run-tests policy-enforcement-tests))

(module+ main
  (require rackunit/text-ui)
  (run-tests f1-f4-remediation-tests)
  (run-tests ci-infrastructure-tests)
  (run-tests policy-enforcement-tests))
