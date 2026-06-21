#lang racket

;; @speed fast
;; @suite fast

;; W2 (#8505): Tests for scripts/ci-package-setup.rkt
;; Verifies pure functions: command construction, failure classification,
;; module discovery. Does NOT run actual raco pkg install.

(require rackunit
         racket/file
         racket/runtime-path)

;; Resolve project root relative to this test file.
;; raco test runs from the test file's directory (tests/).
(define project-root
  (simplify-path
   (build-path (or (path-only (resolved-module-path-name (variable-reference->resolved-module-path
                                                          (#%variable-reference))))
                   ".")
               "..")))

(require "../scripts/ci-package-setup.rkt")

;; ---------------------------------------------------------------------------
;; build-package-setup-command
;; ---------------------------------------------------------------------------

(define-test-suite
 build-command-tests
 (test-case "builds command with PLTUSERHOME isolation"
   (define cmd (build-package-setup-command "/path/to/project"))
   (check-pred (lambda (s) (string-contains? s "PLTUSERHOME")) cmd)
   (check-pred (lambda (s) (string-contains? s "raco pkg install")) cmd)
   (check-pred (lambda (s) (string-contains? s "--auto")) cmd)
   (check-pred (lambda (s) (string-contains? s "--batch")) cmd)
   (check-pred (lambda (s) (string-contains? s "/path/to/project")) cmd))
 (test-case "uses provided PLTUSERHOME"
   (define cmd (build-package-setup-command "/proj" #:plt-user-home "/tmp/custom-home"))
   (check-pred (lambda (s) (string-contains? s "PLTUSERHOME=/tmp/custom-home")) cmd))
 (test-case "uses default home when none provided"
   (define cmd (build-package-setup-command "/proj"))
   (check-pred (lambda (s) (string-contains? s "PLTUSERHOME=")) cmd)))

;; ---------------------------------------------------------------------------
;; classify-failure
;; ---------------------------------------------------------------------------

(define-test-suite classify-tests
                   (test-case "classifies dependency install failure"
                     (check-equal? (classify-failure "cannot find package foo") 'dependency-install))
                   (test-case "classifies unbound identifier as compile failure"
                     (check-equal? (classify-failure "foo: unbound identifier") 'compile-failure))
                   (test-case "classifies duplicate import as compile failure"
                     (check-equal? (classify-failure "identifier already required: foo")
                                   'compile-failure))
                   (test-case "classifies cannot open module as compile failure"
                     (check-equal? (classify-failure "cannot open module file") 'compile-failure))
                   (test-case "classifies cache failure"
                     (check-equal? (classify-failure "cache directory corrupted") 'cache-path))
                   (test-case "classifies path failure"
                     (check-equal? (classify-failure "no such file or directory") 'cache-path))
                   (test-case "classifies unknown failure"
                     (check-equal? (classify-failure "something else happened") 'unknown)))

;; ---------------------------------------------------------------------------
;; find-package-visible-modules
;; ---------------------------------------------------------------------------

(define-test-suite
 module-discovery-tests
 (test-case "finds non-test .rkt files in project root"
   (define modules (find-package-visible-modules project-root))
   (check-pred (lambda (lst) (> (length lst) 50)) modules "expected many modules"))
 (test-case "excludes compiled/ directories"
   (define modules (find-package-visible-modules project-root))
   (check-false (ormap (lambda (m) (string-contains? m "/compiled/")) modules)))
 (test-case "excludes tests/ directory"
   (define modules (find-package-visible-modules project-root))
   (check-false (ormap (lambda (m) (string-contains? m "/tests/")) modules)))
 (test-case "includes cli/generate-certificates.rkt (F1 regression target)"
   (define modules (find-package-visible-modules project-root))
   (check-true (ormap (lambda (m) (string-contains? m "generate-certificates")) modules)
               "cli/generate-certificates.rkt should be package-visible"))
 (test-case "includes scripts that previously failed compile"
   (define modules (find-package-visible-modules project-root))
   (check-true (ormap (lambda (m) (string-contains? m "sdk-gsd-integration-test")) modules))
   (check-true (ormap (lambda (m) (string-contains? m "test-gsd-go-replanning")) modules))
   (check-true (ormap (lambda (m) (string-contains? m "test-gsd-sdk-live")) modules)))
 (test-case "returns sorted list"
   (define modules (find-package-visible-modules project-root))
   (check-equal? modules (sort modules string<?))))

;; ---------------------------------------------------------------------------
;; Quick mode parity warning (structural check)
;; ---------------------------------------------------------------------------

(define-test-suite quick-mode-parity-tests
                   (test-case "ci-local.rkt warns about --quick not having package parity"
                     (define ci-local-path (build-path project-root "scripts" "ci-local.rkt"))
                     (define ci-local-content (file->string ci-local-path))
                     (check-true (string-contains? ci-local-content
                                                   "does NOT include package setup parity")
                                 "ci-local must warn that --quick lacks package parity"))
                   (test-case "ci-local.rkt supports --package-setup flag"
                     (define ci-local-path (build-path project-root "scripts" "ci-local.rkt"))
                     (define ci-local-content (file->string ci-local-path))
                     (check-true (string-contains? ci-local-content "--package-setup")
                                 "ci-local must support --package-setup flag"))
                   (test-case "ci-local.rkt supports --github-setup flag"
                     (define ci-local-path (build-path project-root "scripts" "ci-local.rkt"))
                     (define ci-local-content (file->string ci-local-path))
                     (check-true (string-contains? ci-local-content "--github-setup")
                                 "ci-local must support --github-setup flag")))

;; ---------------------------------------------------------------------------

(module+ test
  (require rackunit/text-ui)
  (run-tests build-command-tests)
  (run-tests classify-tests)
  (run-tests module-discovery-tests)
  (run-tests quick-mode-parity-tests))

(module+ main
  (require rackunit/text-ui)
  (run-tests build-command-tests)
  (run-tests classify-tests)
  (run-tests module-discovery-tests)
  (run-tests quick-mode-parity-tests))
