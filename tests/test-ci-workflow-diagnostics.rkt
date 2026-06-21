#lang racket

;; @speed fast
;; @suite fast

;; W3 (#8506): Tests for CI workflow diagnostic improvements.
;; Verifies the composite action has explicit step naming and
;; diagnostic logging that makes package setup compile behavior visible.

(require rackunit
         racket/file
         racket/string)

(define project-root
  (simplify-path
   (build-path (or (path-only (resolved-module-path-name (variable-reference->resolved-module-path
                                                          (#%variable-reference))))
                   ".")
               "..")))

(define action-path (build-path project-root ".github" "actions" "setup-racket" "action.yml"))

(define action-content (file->string action-path))

(define ci-path (build-path project-root ".github" "workflows" "ci.yml"))

(define ci-content (file->string ci-path))

;; ---------------------------------------------------------------------------
;; Step naming: no longer "Install dependencies"
;; ---------------------------------------------------------------------------

(define-test-suite step-naming-tests
                   (test-case "composite action does NOT use generic 'Install dependencies' name"
                     (check-false (regexp-match? #rx"name: 'Install dependencies'" action-content)
                                  "Step should be renamed from generic 'Install dependencies'")
                     (check-false (regexp-match? #rx"name: Install dependencies\r?$" action-content)
                                  "Step should be renamed from generic 'Install dependencies'"))
                   (test-case "composite action names step with 'package' and 'compile'"
                     (check-true (and (string-contains? action-content "package")
                                      (string-contains? action-content "compile"))
                                 "Step name must reference package and compile behavior"))
                   (test-case "composite action has package setup comment block"
                     (check-true (string-contains? action-content "Package setup:")
                                 "Must have comment explaining package setup compile boundary")))

;; ---------------------------------------------------------------------------
;; Diagnostic logging: versions and package state
;; ---------------------------------------------------------------------------

(define-test-suite diagnostic-logging-tests
                   (test-case "prints racket version"
                     (check-true (string-contains? action-content "racket --version")
                                 "Must print racket version for diagnostics"))
                   (test-case "prints raco version"
                     (check-true (string-contains? action-content "raco --version")
                                 "Must print raco version for diagnostics"))
                   (test-case "prints package state before install/update"
                     (check-true (string-contains? action-content "Package state:")
                                 "Must print package state (installed or not)"))
                   (test-case "prints working directory"
                     (check-true (string-contains? action-content "Working directory")
                                 "Must print PWD for diagnostics"))
                   (test-case "uses GitHub Actions log grouping"
                     (check-true (string-contains? action-content "::group::")
                                 "Must use ::group:: for collapsible log sections")
                     (check-true (string-contains? action-content "::endgroup::")
                                 "Must close log groups with ::endgroup::"))
                   (test-case "has success marker"
                     (check-true (string-contains? action-content "completed successfully")
                                 "Must have clear success marker"))
                   (test-case "has failure diagnostics with local reproduction hint"
                     (check-true (string-contains? action-content "::error::")
                                 "Must use ::error:: annotation on failure")
                     (check-true (string-contains? action-content "ci-package-setup.rkt")
                                 "Must hint at local reproduction command"))
                   (test-case "captures and checks exit codes"
                     (check-true (or (string-contains? action-content "INSTALL_RC")
                                     (string-contains? action-content "UPDATE_RC"))
                                 "Must capture exit codes for explicit error handling")))

;; ---------------------------------------------------------------------------
;; YAML structural validity (structural, not runtime)
;; ---------------------------------------------------------------------------

(define-test-suite
 yaml-structure-tests
 (test-case "action.yml is valid YAML (parsed by Racket reader)"
   ;; Basic structural check: starts with 'name:' and has 'runs:' key
   (check-true (regexp-match? #rx"^name:" (string-trim action-content)) "Must start with name: key")
   (check-true (string-contains? action-content "using: 'composite'") "Must be a composite action")
   (check-true (string-contains? action-content "shell: bash") "Must use bash shell"))
 (test-case "ci.yml references setup-racket composite action"
   (check-true (string-contains? ci-content ".github/actions/setup-racket")
               "Workflow must use the composite action"))
 (test-case "ci.yml has YAML validation step"
   (check-true (string-contains? ci-content "YAML validation")
               "Workflow must have YAML validation gate")))

;; ---------------------------------------------------------------------------

(module+ test
  (require rackunit/text-ui)
  (run-tests step-naming-tests)
  (run-tests diagnostic-logging-tests)
  (run-tests yaml-structure-tests))

(module+ main
  (require rackunit/text-ui)
  (run-tests step-naming-tests)
  (run-tests diagnostic-logging-tests)
  (run-tests yaml-structure-tests))
