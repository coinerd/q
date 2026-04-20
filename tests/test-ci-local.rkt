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
  (simplify-path (build-path (or (path-only (resolved-module-path-name
                                              (variable-reference->resolved-module-path
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

(test-case "ci-local.rkt exits 1 on version mismatch"
  (define backup (file->string ver-path))
  ;; Corrupt version
  (define corrupted (string-replace backup "\"0.12.0\"" "\"99.99.98\""))
  (call-with-output-file ver-path (λ (out) (display corrupted out)) #:exists 'replace)
  (define exit-code (system/exit-code (format "cd ~a && racket ~a" project-root script-path)))
  ;; Restore
  (call-with-output-file ver-path (λ (out) (display backup out)) #:exists 'replace)
  (check-equal? exit-code 1))
