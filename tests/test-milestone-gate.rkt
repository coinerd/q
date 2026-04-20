#lang racket/base

;; tests/test-milestone-gate.rkt — Tests for scripts/milestone-gate.rkt

(require rackunit
         racket/file
         racket/path
         racket/string
         racket/port
         racket/system)

(define project-root
  (simplify-path (build-path (or (path-only (resolved-module-path-name
                                              (variable-reference->resolved-module-path
                                               (#%variable-reference))))
                                 ".")
                             "..")))

(define script (build-path project-root "scripts" "milestone-gate.rkt"))

(test-case "milestone-gate.rkt exists"
  (check-true (file-exists? script)))

(test-case "--help exits 0"
  (define exit-code (system/exit-code (format "cd ~a && racket ~a --help" project-root script)))
  (check-equal? exit-code 0))
