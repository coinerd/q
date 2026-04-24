#lang racket

;; tests/helpers/ci-detection.rkt — Shared CI detection for test files.
;;
;; Tests that reference files outside the q/ git repo (e.g. .pi/skills/,
;; .planning/, gh_helpers.py) must guard against CI where those files
;; don't exist. Use `skip-on-ci` or `on-ci?` to guard such references.
;;
;; Convention: tests must not assume files outside q/ git repo exist.

(provide on-ci?
         skip-on-ci
         project-root)

(require rackunit
         racket/file
         racket/path)

;; Project root is three levels up from this file:
;; tests/helpers/ -> tests/ -> q/ -> repo root
(define project-root
  (simplify-path
   (build-path (path-only (resolved-module-path-name (variable-reference->resolved-module-path
                                                      (#%variable-reference))))
               ".."
               ".."
               "..")))

;; CI detection: .pi/skills/ exists in local dev but not on CI.
(define on-ci? (not (directory-exists? (build-path project-root ".pi" "skills"))))

;; Macro to skip a test on CI (runs as trivial pass instead)
(define-syntax-rule (skip-on-ci test-name body ...)
  (if on-ci?
      (test-case test-name
        (check-true #t "skipped: CI environment"))
      (test-case test-name
        body ...)))
