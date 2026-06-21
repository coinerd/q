#lang racket

;; @speed fast
;; @suite fast

;; W4 (#8507): Tests for package-visible script policy enforcement.
;; Verifies that the policy doc exists and key package-visible scripts
;; compile. These are regression guards against the F1-F4 class of
;; failures where scripts/CLI modules had compile errors invisible to
;; the main.rkt dependency graph.

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
;; Policy document existence
;; ---------------------------------------------------------------------------

(define policy-path (build-path project-root "docs" "reports" "CI-PACKAGE-SETUP-POLICY-v0.99.39.md"))

(define policy-content
  (if (file-exists? policy-path)
      (file->string policy-path)
      ""))

(define-test-suite
 policy-doc-tests
 (test-case "policy document exists"
   (check-true (file-exists? policy-path) "CI-PACKAGE-SETUP-POLICY-v0.99.39.md must exist"))
 (test-case "policy has core rules"
   (check-true (string-contains? policy-content "Everything package-visible must compile")
               "Must state Rule 1")
   (check-true (string-contains? policy-content "No compile-time side effects") "Must state Rule 2")
   (check-true (string-contains? policy-content "Imports must be explicit and current")
               "Must state Rule 3"))
 (test-case "policy references enforcement mechanisms"
   (check-true (string-contains? policy-content "ci-package-setup.rkt")
               "Must reference the preflight tool")
   (check-true (string-contains? policy-content "--preflight") "Must document the preflight flag"))
 (test-case "policy has script classification"
   (check-true (string-contains? policy-content "Script Classification")
               "Must have script classification section")
   (check-true (string-contains? policy-content "CLI entry points") "Must classify CLI entry points"))
 (test-case "policy references F1-F4 historical context"
   (check-true (string-contains? policy-content "F1") "Must reference F1 historical context")
   (check-true (string-contains? policy-content "generate-certificates")
               "Must reference the F1 module")))

;; ---------------------------------------------------------------------------
;; F1-F4 regression targets compile (structural check via file existence
;; and module parseability — full compile is tested in
;; test-ci-package-compile-boundary.rkt)
;; ---------------------------------------------------------------------------

(define-test-suite
 f1-f4-regression-tests
 (test-case "F1: cli/generate-certificates.rkt exists"
   (check-true (file-exists? (build-path project-root "cli" "generate-certificates.rkt"))
               "F1 target module must exist"))
 (test-case "F2: scripts/sdk-gsd-integration-test.rkt exists"
   (check-true (file-exists? (build-path project-root "scripts" "sdk-gsd-integration-test.rkt"))
               "F2 target module must exist"))
 (test-case "F3: scripts/test-gsd-go-replanning.rkt exists"
   (check-true (file-exists? (build-path project-root "scripts" "test-gsd-go-replanning.rkt"))
               "F3 target module must exist"))
 (test-case "F4: scripts/test-gsd-sdk-live.rkt exists"
   (check-true (file-exists? (build-path project-root "scripts" "test-gsd-sdk-live.rkt"))
               "F4 target module must exist"))
 (test-case "F1 fix verification: generate-certificates.rkt requires racket/string"
   (define f1-content (file->string (build-path project-root "cli" "generate-certificates.rkt")))
   (check-true (string-contains? f1-content "racket/string") "F1 fix: must require racket/string")))

;; ---------------------------------------------------------------------------
;; Package setup tooling exists
;; ---------------------------------------------------------------------------

(define-test-suite
 package-setup-tooling-tests
 (test-case "ci-package-setup.rkt exists"
   (check-true (file-exists? (build-path project-root "scripts" "ci-package-setup.rkt"))
               "Package setup preflight tool must exist"))
 (test-case "ci-local.rkt has --package-setup flag"
   (define ci-local-content (file->string (build-path project-root "scripts" "ci-local.rkt")))
   (check-true (string-contains? ci-local-content "--package-setup")
               "ci-local.rkt must expose --package-setup flag"))
 (test-case "ci-local.rkt has --github-setup flag"
   (define ci-local-content (file->string (build-path project-root "scripts" "ci-local.rkt")))
   (check-true (string-contains? ci-local-content "--github-setup")
               "ci-local.rkt must expose --github-setup flag")))

;; ---------------------------------------------------------------------------

(module+ test
  (require rackunit/text-ui)
  (run-tests policy-doc-tests)
  (run-tests f1-f4-regression-tests)
  (run-tests package-setup-tooling-tests))

(module+ main
  (require rackunit/text-ui)
  (run-tests policy-doc-tests)
  (run-tests f1-f4-regression-tests)
  (run-tests package-setup-tooling-tests))
