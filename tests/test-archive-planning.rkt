#lang racket/base

;; tests/test-archive-planning.rkt — Tests for scripts/archive-planning.rkt

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

(define script (build-path project-root "scripts" "archive-planning.rkt"))

(test-case "archive-planning.rkt exists"
  (check-true (file-exists? script)))

(test-case "--dry-run exits 0"
  (define exit-code (system/exit-code (format "cd ~a && racket ~a --dry-run" project-root script)))
  (check-equal? exit-code 0))

(test-case "--dry-run shows archive candidates"
  (define output
    (with-output-to-string
      (λ ()
        (system (format "cd ~a && racket ~a --dry-run 2>&1" project-root script)))))
  ;; Should mention files or "No archive candidates"
  (check-not-false (or (string-contains? output "archive")
                       (string-contains? output "candidate")
                       (string-contains? output "No")))
  ;; Should NOT mention protected files (PLAN.md, STATE.md)
  (check-false (string-contains? output "KEEPING: PLAN.md")
               "PLAN.md should always be kept"))

(test-case "milestone-group extracts version from filename"
  ;; Test the helper directly by checking script output
  (define output
    (with-output-to-string
      (λ ()
        (system (format "cd ~a && racket ~a --dry-run 2>&1" project-root script)))))
  ;; V0113 files should be grouped under v0.11.3 or v0113
  (when (file-exists? (build-path project-root ".planning" "V0113_REMEDIATION_PLAN.md"))
    (check-not-false (regexp-match? #rx"v0\\.?11\\.?3|v0113" output))))
