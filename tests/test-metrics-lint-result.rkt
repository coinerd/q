#lang racket/base

;; @speed fast
;; @suite default

;; BOUNDARY: unit

;; tests/test-metrics-lint-result.rkt — Tests for metrics lint result types
;;
;; W4 (#8444): Expected-failure/result-boundary pilot.
;; Tests the pure check-metrics function and result-type dispatch.

(require rackunit
         racket/list
         "../scripts/metrics-lint-result.rkt")

;; ---------------------------------------------------------------------------
;; Result type tests
;; ---------------------------------------------------------------------------

(test-case "lint-ok struct has metric-name field"
  (define r (lint-ok "Source modules"))
  (check-equal? (lint-ok-metric-name r) "Source modules")
  (check-true (lint-ok? r))
  (check-equal? (lint-result-kind r) 'ok))

(test-case "lint-mismatch struct has all fields"
  (define r (lint-mismatch "Test files" "150" "149"))
  (check-equal? (lint-mismatch-metric-name r) "Test files")
  (check-equal? (lint-mismatch-expected r) "150")
  (check-equal? (lint-mismatch-found r) "149")
  (check-equal? (lint-result-kind r) 'mismatch))

(test-case "lint-metric-not-found struct"
  (define r (lint-metric-not-found "Test assertions"))
  (check-equal? (lint-metric-not-found-metric-name r) "Test assertions")
  (check-equal? (lint-result-kind r) 'not-found))

(test-case "lint-file-error struct"
  (define r (lint-file-error "README.md" "file not found"))
  (check-equal? (lint-file-error-path r) "README.md")
  (check-equal? (lint-file-error-message r) "file not found")
  (check-equal? (lint-result-kind r) 'file-error))

(test-case "lint-result? predicate covers all variants"
  (check-true (lint-result? (lint-ok "x")))
  (check-true (lint-result? (lint-mismatch "x" "1" "2")))
  (check-true (lint-result? (lint-metric-not-found "x")))
  (check-true (lint-result? (lint-file-error "x" "y")))
  (check-false (lint-result? "not a result"))
  (check-false (lint-result? 42)))

;; ---------------------------------------------------------------------------
;; Format tests
;; ---------------------------------------------------------------------------

(test-case "format-lint-result: ok"
  (check-equal? (format-lint-result (lint-ok "Source modules")) "OK: Source modules"))

(test-case "format-lint-result: mismatch"
  (check-equal? (format-lint-result (lint-mismatch "Test files" "150" "149"))
                "MISMATCH: Test files: expected 150, found 149"))

(test-case "format-lint-result: not-found"
  (check-equal? (format-lint-result (lint-metric-not-found "Test assertions"))
                "NOT FOUND: Test assertions not in README table"))

;; ---------------------------------------------------------------------------
;; Exit code tests
;; ---------------------------------------------------------------------------

(test-case "lint-results-exit-code: all ok → 0"
  (check-equal? (lint-results-exit-code (list (lint-ok "a") (lint-ok "b"))) 0))

(test-case "lint-results-exit-code: any failure → 1"
  (check-equal? (lint-results-exit-code (list (lint-ok "a") (lint-mismatch "b" "1" "2"))) 1))

(test-case "lint-results-exit-code: empty list → 0"
  (check-equal? (lint-results-exit-code '()) 0))

;; ---------------------------------------------------------------------------
;; check-metrics pure function tests
;; ---------------------------------------------------------------------------

(test-case "check-metrics: all match → all lint-ok"
  (define readme (hash "Source modules" "100" "Test files" "50"))
  (define computed '(("Source modules" . "100") ("Test files" . "50")))
  (define results (check-metrics readme computed))
  (check-equal? (length results) 2)
  (check-true (andmap lint-ok? results))
  (check-equal? (lint-results-exit-code results) 0))

(test-case "check-metrics: mismatch detected"
  (define readme (hash "Source modules" "100"))
  (define computed '(("Source modules" . "99")))
  (define results (check-metrics readme computed))
  (check-equal? (length results) 1)
  (define r (first results))
  (check-equal? (lint-result-kind r) 'mismatch)
  (check-equal? (lint-mismatch-expected r) "99")
  (check-equal? (lint-mismatch-found r) "100"))

(test-case "check-metrics: missing metric detected"
  (define readme (hash "Source modules" "100"))
  (define computed '(("Source modules" . "100") ("Test files" . "50")))
  (define results (check-metrics readme computed))
  (check-equal? (length results) 2)
  (define missing (findf lint-metric-not-found? results))
  (check-not-false missing)
  (check-equal? (lint-metric-not-found-metric-name missing) "Test files"))

(test-case "check-metrics: empty inputs → empty results"
  (define results (check-metrics (hash) '()))
  (check-equal? results '()))

(test-case "check-metrics: empty readme with metrics → all not-found"
  (define computed '(("A" . "1") ("B" . "2")))
  (define results (check-metrics (hash) computed))
  (check-equal? (length results) 2)
  (check-true (andmap lint-metric-not-found? results)))

(test-case "check-metrics: mixed results"
  (define readme (hash "A" "1" "B" "2"))
  (define computed '(("A" . "1") ("B" . "3") ("C" . "4")))
  (define results (check-metrics readme computed))
  (check-equal? (length results) 3)
  (check-equal? (lint-result-kind (first results)) 'ok)
  (check-equal? (lint-result-kind (second results)) 'mismatch)
  (check-equal? (lint-result-kind (third results)) 'not-found)
  (check-equal? (lint-results-exit-code results) 1))
