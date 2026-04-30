#lang racket

;; tests/test-audit-script.rkt — Tests for scripts/audit-project.rkt (RA-4)

(require rackunit
         rackunit/text-ui
         racket/string
         racket/file
         racket/path
         racket/port)

;; Resolve q-dir: go up from tests/ directory
(define q-dir
  (let* ([test-dir (current-directory)]
         [parent (simplify-path (build-path test-dir ".."))])
    (if (directory-exists? (build-path parent "runtime"))
        parent
        (simplify-path (build-path parent "q")))))

(define audit-script (build-path q-dir "scripts" "audit-project.rkt"))

(define (run-audit-stdout)
  (define-values (sp out in err)
    (parameterize ([current-directory q-dir])
      (subprocess #f #f #f (find-executable-path "racket") (path->string audit-script) "--stdout")))
  (define output (port->string out))
  (close-input-port out)
  (close-input-port err)
  (close-output-port in)
  (subprocess-wait sp)
  output)

(define audit-tests
  (test-suite "Audit Script Tests"

    (test-case "audit script exists and is readable"
      (check-true (file-exists? audit-script)))

    (test-case "audit --stdout produces valid markdown report"
      (define output (run-audit-stdout))
      (check-true (string-contains? output "# Q Project Audit Report"))
      (check-true (string-contains? output "## Module Inventory"))
      (check-true (string-contains? output "## Findings"))
      (check-true (string-contains? output "## Summary")))

    (test-case "audit report contains module count"
      (define output (run-audit-stdout))
      (check-true (string-contains? output "Total modules"))
      (check-true (regexp-match? #rx"Total modules.*: [0-9]+" output)))

    (test-case "audit report includes layer breakdown"
      (define output (run-audit-stdout))
      (check-true (string-contains? output "runtime"))
      (check-true (string-contains? output "tests")))))

(run-tests audit-tests)
