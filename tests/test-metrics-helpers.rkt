#lang racket/base

;; @speed fast
;; @suite fast
;; BOUNDARY: pure

;; W5 (#8479): Unit tests for scripts/metrics-helpers.rkt
;;
;; Tests the pure computation extracted from metrics.rkt:
;;   compute-marker-sync, compute-table-sync, compute-prose-sync,
;;   compute-all-sync, lint-table-values, lint-prose-values,
;;   extract-table-section.

(require rackunit
         rackunit/text-ui
         racket/string
         "../scripts/metrics-helpers.rkt")

;; ============================================================
;; §1: compute-marker-sync
;; ============================================================

(define-test-suite
 marker-sync-tests
 (test-case "replaces known markers"
   (define content "Suite (<!-- METRICS: test-files --> files)")
   (define result (compute-marker-sync content (hash "test-files" "999")))
   (check-equal? result "Suite (999 files)"))
 (test-case "replaces multiple markers"
   (define content "<!-- METRICS: test-files --> and <!-- METRICS: source-modules -->")
   (define result (compute-marker-sync content (hash "test-files" "100" "source-modules" "50")))
   (check-equal? result "100 and 50"))
 (test-case "preserves unknown markers with annotation"
   (define content "<!-- METRICS: unknown-key -->")
   (define result (compute-marker-sync content (hash "test-files" "100")))
   (check-true (string-contains? result "unknown-key")))
 (test-case "no markers returns content unchanged"
   (define content "No markers here.")
   (check-equal? (compute-marker-sync content (hash)) content))
 (test-case "empty content returns empty"
   (check-equal? (compute-marker-sync "" (hash "test-files" "1")) ""))
 (test-case "handles all 5 metric keys"
   (define content
     (string-append "<!-- METRICS: test-files -->\n"
                    "<!-- METRICS: source-modules -->\n"
                    "<!-- METRICS: source-lines -->\n"
                    "<!-- METRICS: test-lines -->\n"
                    "<!-- METRICS: test-assertions -->"))
   (define metrics
     (hash "test-files"
           "100"
           "source-modules"
           "200"
           "source-lines"
           "300"
           "test-lines"
           "400"
           "test-assertions"
           "500"))
   (define result (compute-marker-sync content metrics))
   (check-equal? result "100\n200\n300\n400\n500")))

;; ============================================================
;; §2: compute-table-sync
;; ============================================================

(define-test-suite
 table-sync-tests
 (test-case "replaces table row value"
   (define content "| Source modules | 100 |")
   (define result (compute-table-sync content '(("Source modules" . "999"))))
   (check-equal? result "| Source modules | 999 |"))
 (test-case "replaces multiple rows"
   (define content "| Source modules | 100 |\n| Test files | 50 |")
   (define result (compute-table-sync content '(("Source modules" . "200") ("Test files" . "60"))))
   (check-equal? result "| Source modules | 200 |\n| Test files | 60 |"))
 (test-case "handles comma-formatted values"
   (define content "| Source lines | 10,000 |")
   (define result (compute-table-sync content '(("Source lines" . "9999"))))
   (check-equal? result "| Source lines | 9999 |"))
 (test-case "preserves non-matching rows"
   (define content "| Other metric | 42 |")
   (define result (compute-table-sync content '(("Source modules" . "100"))))
   (check-equal? result content))
 (test-case "preserves surrounding text"
   (define content "Before\n| Source modules | 100 |\nAfter")
   (define result (compute-table-sync content '(("Source modules" . "200"))))
   (check-true (string-contains? result "Before"))
   (check-true (string-contains? result "After"))))

;; ============================================================
;; §3: compute-prose-sync
;; ============================================================

(define-test-suite prose-sync-tests
                   (test-case "replaces test file prose count"
                     (define content "Full test suite (100 files)")
                     (define result (compute-prose-sync content "999" "200"))
                     (check-equal? result "Full test suite (999 files)"))
                   (test-case "replaces source module prose count"
                     (define content "500 source modules")
                     (define result (compute-prose-sync content "100" "999"))
                     (check-equal? result "999 source modules"))
                   (test-case "replaces both patterns"
                     (define content "Full test suite (100 files) and 500 source modules")
                     (define result (compute-prose-sync content "200" "600"))
                     (check-true (string-contains? result "200 files"))
                     (check-true (string-contains? result "600 source modules")))
                   (test-case "no patterns returns unchanged"
                     (define content "No patterns here.")
                     (check-equal? (compute-prose-sync content "100" "200") content)))

;; ============================================================
;; §4: compute-all-sync
;; ============================================================

(define-test-suite all-sync-tests
                   (test-case "applies markers, table, and prose in sequence"
                     (define content
                       (string-append "<!-- METRICS: test-files -->\n"
                                      "| Test files | 100 |\n"
                                      "Full test suite (50 files)"))
                     (define metrics-map (hash "test-files" "999" "source-modules" "500"))
                     (define computed '(("Test files" . "999")))
                     (define result (compute-all-sync content metrics-map computed))
                     (check-true (string-contains? result "999\n| Test files | 999 |"))
                     (check-true (string-contains? result "999 files)"))))

;; ============================================================
;; §5: extract-table-section
;; ============================================================

(define-test-suite section-extraction-tests
                   (test-case "extracts lines from ## Test Suite section"
                     (define content
                       (string-append "## Intro\n"
                                      "Some text\n"
                                      "## Test Suite\n"
                                      "| Metric | Value |\n"
                                      "| Source modules | 100 |\n"
                                      "## Next Section\n"
                                      "More text"))
                     (define lines (extract-table-section content))
                     (check-true (pair? lines))
                     (check-not-false (member "| Metric | Value |" lines) "header in section")
                     (check-not-false (member "| Source modules | 100 |" lines) "data in section")
                     (check-false (member "Some text" lines) "pre-section excluded")
                     (check-false (member "More text" lines) "post-section excluded"))
                   (test-case "empty section returns empty list"
                     (define content "## Other Section\nNo test suite")
                     (check-equal? (extract-table-section content) '()))
                   (test-case "section at end of content"
                     (define content "## Test Suite\n| Source modules | 42 |")
                     (define lines (extract-table-section content))
                     (check-equal? (length lines) 1)))

;; ============================================================
;; §6: lint-table-values
;; ============================================================

(define-test-suite table-lint-tests
                   (test-case "all matching values returns no errors"
                     (define content
                       (string-append "## Test Suite\n"
                                      "| Metric | Value |\n"
                                      "|--------|-------|\n"
                                      "| Source modules | 695 |\n"
                                      "| Test files | 1094 |"))
                     (define metrics '(("Source modules" . "695") ("Test files" . "1094")))
                     (check-equal? (lint-table-values content metrics) '()))
                   (test-case "mismatched value returns error"
                     (define content (string-append "## Test Suite\n" "| Source modules | 600 |"))
                     (define metrics '(("Source modules" . "695")))
                     (define errs (lint-table-values content metrics))
                     (check-true (pair? errs))
                     (check-true (string-contains? (car errs) "Source modules")))
                   (test-case "missing metric returns error"
                     (define content "## Test Suite\n| Other | 42 |")
                     (define metrics '(("Source modules" . "695")))
                     (define errs (lint-table-values content metrics))
                     (check-true (pair? errs))
                     (check-true (string-contains? (car errs) "not found")))
                   (test-case "ignores non-numeric values"
                     (define content
                       (string-append "## Test Suite\n"
                                      "| Tests passing | — (use --tests) |\n"
                                      "| Source modules | 695 |"))
                     (define metrics '(("Source modules" . "695")))
                     (check-equal? (lint-table-values content metrics) '()))
                   (test-case "comma-formatted values compared correctly"
                     (define content (string-append "## Test Suite\n" "| Source lines | 109,851 |"))
                     (define metrics '(("Source lines" . "109851")))
                     (check-equal? (lint-table-values content metrics) '())))

;; ============================================================
;; §7: lint-prose-values
;; ============================================================

(define-test-suite prose-lint-tests
                   (test-case "matching test-file prose returns no errors"
                     (define content "Full test suite (1094 files)")
                     (check-equal? (lint-prose-values content "1094" "695") '()))
                   (test-case "mismatched test-file prose returns error"
                     (define content "Full test suite (500 files)")
                     (define errs (lint-prose-values content "1094" "695"))
                     (check-true (pair? errs))
                     (check-true (string-contains? (car errs) "MISMATCH")))
                   (test-case "matching source-modules prose returns no errors"
                     (define content "695 source modules")
                     (check-equal? (lint-prose-values content "1094" "695") '()))
                   (test-case "mismatched source-modules prose returns error"
                     (define content "600 source modules")
                     (define errs (lint-prose-values content "1094" "695"))
                     (check-true (pair? errs))
                     (check-true (string-contains? (car errs) "MISMATCH")))
                   (test-case "no patterns returns empty errors"
                     (define content "No patterns here.")
                     (check-equal? (lint-prose-values content "100" "200") '()))
                   (test-case "multiple errors collected"
                     (define content "Full test suite (100 files) and 200 source modules")
                     (define errs (lint-prose-values content "999" "888"))
                     ;; 3 errors: two test-file patterns match + one source-modules
                     (check-true (>= (length errs) 2)
                                 (format "Expected ≥2 errors, got ~a" (length errs)))))

;; ============================================================
;; All tests
;; ============================================================

(define-test-suite all-metrics-helpers-tests
                   marker-sync-tests
                   table-sync-tests
                   prose-sync-tests
                   all-sync-tests
                   section-extraction-tests
                   table-lint-tests
                   prose-lint-tests)

(module+ test
  (run-tests all-metrics-helpers-tests))
