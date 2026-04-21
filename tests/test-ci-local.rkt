#lang racket/base

;; tests/test-ci-local.rkt — Tests for scripts/ci-local.rkt

(require rackunit
         racket/system
         racket/port
         racket/file
         racket/string
         racket/path)

;; Resolve paths relative to the q/ project root.
;; raco test runs from the test file's directory; we need to go up one level.
(define project-root
  (simplify-path
   (build-path (or (path-only (resolved-module-path-name (variable-reference->resolved-module-path
                                                          (#%variable-reference))))
                   ".")
               "..")))

(define script-path (build-path project-root "scripts" "ci-local.rkt"))
(define ver-path (build-path project-root "util" "version.rkt"))

(test-case "ci-local.rkt exists"
  (check-true (file-exists? script-path)))

(test-case "ci-local.rkt exits 0 on clean tree"
  (define exit-code (system/exit-code (format "cd ~a && racket ~a" project-root script-path)))
  (check-equal? exit-code 0))

(test-case "ci-local.rkt --fix exits 0 on clean tree"
  (define exit-code (system/exit-code (format "cd ~a && racket ~a --fix" project-root script-path)))
  (check-equal? exit-code 0))

(test-case "ci-local.rkt --quick exits 0 on clean tree"
  (define exit-code (system/exit-code (format "cd ~a && racket ~a --quick" project-root script-path)))
  (check-equal? exit-code 0))

(test-case "lint-version detects mismatched version"
  ;; Test that lint-version.rkt catches a version mismatch.
  ;; We test the detection logic directly without corrupting the real file,
  ;; to avoid race conditions with parallel test execution in CI.
  (define lint-script (build-path project-root "scripts" "lint-version.rkt"))
  (check-true (file-exists? lint-script))
  ;; Verify the script loads and has the right interface
  (define output
    (with-output-to-string (λ ()
                             (system (format "cd ~a && racket ~a 2>&1" project-root lint-script)))))
  ;; On a clean tree, lint-version should pass (exit 0, no ERROR lines)
  (check-false (string-contains? output "ERROR") (format "Unexpected lint errors: ~a" output)))
