#lang racket/base

;; @speed fast  ;; @suite security

;; BOUNDARY: integration

;; tests/test-safe-mode-defense-depth.rkt
;; Test defense-in-depth for #reader/#lang injection prevention

(require rackunit
         (only-in "../extensions/racket-tooling/rewrite.rkt" handle-racket-codemod)
         (only-in "../extensions/racket-tooling/formatting.rkt" handle-racket-edit))

;; ============================================================
;; Tests for #reader/#lang injection prevention
;; ============================================================

(test-case "handle-racket-codemod: rejects #reader in pattern"
  (define args (hasheq 'file "dummy.rkt" 'pattern "#reader(/racket/date date)" 'template "(void)"))
  (check-exn exn:fail? (lambda () (handle-racket-codemod args))))

(test-case "handle-racket-codemod: rejects #lang in pattern"
  (define args (hasheq 'file "dummy.rkt" 'pattern "#lang racket\n(+ 1 2)" 'template "(void)"))
  (check-exn exn:fail? (lambda () (handle-racket-codemod args))))

(test-case "handle-racket-edit: rejects #reader in pattern"
  (define args
    (hasheq 'file
            "dummy.rkt"
            'mode
            "replace"
            'pattern
            "#reader(/racket/date date)"
            'oldText
            "(+ 1 2)"))
  (check-exn exn:fail? (lambda () (handle-racket-edit args))))

(test-case "handle-racket-edit: rejects #lang in template"
  (define args
    (hasheq 'file "dummy.rkt" 'mode "replace" 'oldText "(+ 1 2)" 'newText "#lang racket\n(+ 1 2)"))
  (check-exn exn:fail? (lambda () (handle-racket-edit args))))

;; ============================================================
;; All tests
;; ============================================================

(provide (all-defined-out))
