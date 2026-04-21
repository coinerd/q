#lang racket/base

;; tests/test-bump-version.rkt — Tests for scripts/bump-version.rkt

(require rackunit
         racket/file
         racket/string
         racket/system
         racket/path
         racket/port)

(define project-root
  (simplify-path
   (build-path (or (path-only (resolved-module-path-name (variable-reference->resolved-module-path
                                                          (#%variable-reference))))
                   ".")
               "..")))

(define script (build-path project-root "scripts" "bump-version.rkt"))

(test-case "bump-version.rkt exists"
  (check-true (file-exists? script)))

(test-case "dry-run shows surfaces without modifying"
  (define output
    (with-output-to-string
     (λ () (system (format "cd ~a && racket ~a 99.99.99 --dry-run 2>&1" project-root script)))))
  (check-not-false (string-contains? output "version.rkt"))
  (check-not-false (string-contains? output "info.rkt"))
  (check-not-false (string-contains? output "README.md"))
  ;; Verify version was NOT actually changed
  (define ver-content (file->string (build-path project-root "util" "version.rkt")))
  (define current-version
    (let ([m (regexp-match #rx"\"([0-9]+[.][0-9]+[.][0-9]+)\"" ver-content)]) (and m (cadr m))))
  (check-not-false (and current-version
                        (string-contains? ver-content (format "\"~a\"" current-version)))
                   "Version should be unchanged after dry-run"))

(test-case "dry-run reports 10+ surfaces"
  (define output
    (with-output-to-string
     (λ () (system (format "cd ~a && racket ~a 99.99.99 --dry-run 2>&1" project-root script)))))
  ;; Count "Would update" lines
  (define matches (regexp-match* #rx"Would update" output))
  (check-true (>= (length matches) 8) (format "Expected 8+ surfaces, got ~a" (length matches))))
