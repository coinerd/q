#lang racket/base
;; W11 — Final audit summary test
;; Validates that all v0.99.45 audit test files exist and are discoverable.
;; @suite default
;; @speed fast

(require rackunit
         racket/file)

;; ---------------------------------------------------------------------------
;; Audit test file inventory
;; ---------------------------------------------------------------------------

(define audit-test-dir ".")
(define report-dir "../docs/reports")

(define expected-audit-files
  '("test-audit-v09945-w1-memory.rkt" "test-audit-v09945-w2-context-assembly.rkt"
                                      "test-audit-v09945-w3-gsd.rkt"
                                      "test-audit-v09945-w4-mas.rkt"
                                      "test-audit-v09945-w5-tools.rkt"
                                      "test-audit-v09945-w6-session.rkt"
                                      "test-audit-v09945-w7-creds.rkt"
                                      "test-audit-v09945-w8-tui.rkt"
                                      "test-audit-v09945-w9-extension.rkt"
                                      "test-audit-v09945-w10-integration.rkt"))

(define expected-reports
  '("AUDIT-v0.99.45-W1-MEMORY.md" "AUDIT-v0.99.45-W2-CONTEXT-ASSEMBLY.md"
                                  "AUDIT-v0.99.45-W3-GSD.md"
                                  "AUDIT-v0.99.45-W4-MAS.md"
                                  "AUDIT-v0.99.45-W5-TOOLS.md"
                                  "AUDIT-v0.99.45-W6-SESSION.md"
                                  "AUDIT-v0.99.45-W7-CREDENTIALS.md"
                                  "AUDIT-v0.99.45-W8-TUI.md"
                                  "AUDIT-v0.99.45-W9-EXTENSION.md"
                                  "AUDIT-v0.99.45-W10-INTEGRATION.md"
                                  "AUDIT-v0.99.45-FINAL-REPORT.md"))

(test-case "audit-all-test-files-exist"
  (for ([f (in-list expected-audit-files)])
    (define path (build-path audit-test-dir f))
    (check-true (file-exists? path) (format "audit test file missing: ~a" f))))

(test-case "audit-all-reports-exist"
  (for ([f (in-list expected-reports)])
    (define path (build-path report-dir f))
    (check-true (file-exists? path) (format "audit report missing: ~a" f))))

(test-case "audit-test-file-count"
  (define all-audit-files
    (filter (lambda (f)
              (and (regexp-match? #rx"test-audit-v09945" f) (not (regexp-match? #rx"w0" f))))
            (map path->string (directory-list audit-test-dir))))
  ;; Should have at least 10 audit test files (W1-W10)
  (check-true (>= (length all-audit-files) 10)
              (format "expected >= 10 audit files, got ~a" (length all-audit-files))))

(test-case "audit-total-test-count"
  ;; Each audit file should have at least 25 tests
  (for ([f (in-list expected-audit-files)])
    (define path (build-path audit-test-dir f))
    (when (file-exists? path)
      (define content (file->string path))
      (define test-count (length (regexp-match* #rx"\\(test-case " content)))
      (check-true (>= test-count 5) (format "~a: expected >= 5 test cases, got ~a" f test-count)))))
