#lang racket/base

;; tests/test-claim-verifier.rkt — Unit tests for claim-verifier.rkt
;;
;; W1 (#8683): Claim verification tool (GAP-1).
;;
;; Tests pure functions for counting test-cases and check assertions,
;; extracting claimed counts from markdown report text, and verifying
;; that claimed counts match actual counts.

(require rackunit
         "../scripts/claim-verifier.rkt")

;; ---------------------------------------------------------------------------
;; count-test-cases-in-text
;; ---------------------------------------------------------------------------

(test-case "count-test-cases-in-text counts (test-case forms"
  (define text
    "(test-case \"simple\"\n  (check-equal? 1 1))\n(test-case \"another\"\n  (check-true #t))")
  (check-equal? (count-test-cases-in-text text) 2))

(test-case "count-test-cases-in-text returns 0 for empty text"
  (check-equal? (count-test-cases-in-text "") 0))

(test-case "count-test-cases-in-text ignores test-case substring inside strings"
  ;; The word "test-case" inside a string should not be counted.
  (define text "(check-equal? \"test-case is here\" 1)")
  (check-equal? (count-test-cases-in-text text) 0))

(test-case "count-test-cases-in-text counts nested test-case forms"
  (define text "(test-case \"outer\"\n  (test-case \"inner\"\n    (check-true #t)))")
  (check-equal? (count-test-cases-in-text text) 2))

;; ---------------------------------------------------------------------------
;; count-check-assertions-in-text
;; ---------------------------------------------------------------------------

(test-case "count-check-assertions-in-text counts check- forms"
  (define text
    "(test-case \"simple\"\n  (check-equal? 1 1)\n  (check-true #t)\n  (check-not-false #f))")
  (check-equal? (count-check-assertions-in-text text) 3))

(test-case "count-check-assertions-in-text returns 0 for empty text"
  (check-equal? (count-check-assertions-in-text "") 0))

(test-case "count-check-assertions-in-text ignores check- in strings"
  ;; No real (check- call — the word appears only inside a string.
  (define text "The doc mentions check-equal? and check-true but no real calls.")
  (check-equal? (count-check-assertions-in-text text) 0))

;; ---------------------------------------------------------------------------
;; extract-claimed-counts
;; ---------------------------------------------------------------------------

(test-case "extract-claimed-counts finds 'N tests' pattern"
  (define text "The smoke gate was green with 286 tests passing.")
  (define claims (extract-claimed-counts text))
  (check-not-false (assoc 'tests claims))
  (check-equal? (cdr (assoc 'tests claims)) 286))

(test-case "extract-claimed-counts finds 'N test files' pattern"
  (define text "663 tests across 11 test files, containing 1626 check assertions.")
  (define claims (extract-claimed-counts text))
  (check-not-false (assoc 'test-files claims))
  (check-equal? (cdr (assoc 'test-files claims)) 11))

(test-case "extract-claimed-counts finds 'N check assertions' pattern"
  (define text "containing 1,626 check assertions")
  (define claims (extract-claimed-counts text))
  (check-not-false (assoc 'check-assertions claims))
  (check-equal? (cdr (assoc 'check-assertions claims)) 1626))

(test-case "extract-claimed-counts finds 'N test-cases' pattern"
  (define text "Added 50 test-cases in this wave.")
  (define claims (extract-claimed-counts text))
  (check-not-false (assoc 'test-cases claims))
  (check-equal? (cdr (assoc 'test-cases claims)) 50))

(test-case "extract-claimed-counts handles comma-separated numbers"
  (define text "The audit added 1,500 check assertions.")
  (define claims (extract-claimed-counts text))
  (check-not-false (assoc 'check-assertions claims))
  (check-equal? (cdr (assoc 'check-assertions claims)) 1500))

(test-case "extract-claimed-counts returns empty for no claims"
  (define text "This is a normal paragraph with no test count claims.")
  (check-equal? (extract-claimed-counts text) '()))

;; ---------------------------------------------------------------------------
;; extract-test-file-refs
;; ---------------------------------------------------------------------------

(test-case "extract-test-file-refs finds test-*.rkt references"
  (define text "See tests/test-foo.rkt and tests/test-bar.rkt for details.")
  (define refs (extract-test-file-refs text))
  (check-not-false (member "test-foo.rkt" refs))
  (check-not-false (member "test-bar.rkt" refs)))

(test-case "extract-test-file-refs returns empty for no references"
  (define text "No test file references here.")
  (check-equal? (extract-test-file-refs text) '()))

(test-case "extract-test-file-refs handles relative paths"
  (define text "Modified test-baz.rkt in the tests directory.")
  (define refs (extract-test-file-refs text))
  (check-not-false (member "test-baz.rkt" refs)))

;; ---------------------------------------------------------------------------
;; verify-claims
;; ---------------------------------------------------------------------------

(test-case "verify-claims returns matched?=#t when counts agree"
  (define claims '((tests . 2)))
  (define actual '((tests . 2)))
  (define results (verify-claims claims actual))
  (check-true (andmap claim-result-matched? results)))

(test-case "verify-claims returns matched?=#f when counts disagree"
  (define claims '((tests . 100)))
  (define actual '())
  (define results (verify-claims claims actual))
  ;; actual defaults to empty, so 100 vs 0 should not match
  (check-false (andmap claim-result-matched? results)))

(test-case "verify-claims constructs claim-result with pattern, claimed, actual"
  (define claims '((tests . 100)))
  (define actual '((tests . 99)))
  (define results (verify-claims claims actual))
  (check-equal? (length results) 1)
  (define r (car results))
  (check-equal? (claim-result-pattern r) 'tests)
  (check-equal? (claim-result-claimed r) 100)
  (check-equal? (claim-result-actual r) 99)
  (check-false (claim-result-matched? r)))

(test-case "verify-claims handles multiple claim types"
  (define claims '((tests . 2) (check-assertions . 5) (test-files . 1)))
  (define actual '((tests . 2) (check-assertions . 5) (test-files . 1)))
  (define results (verify-claims claims actual))
  (check-equal? (length results) 3)
  (check-true (andmap claim-result-matched? results)))

;; ---------------------------------------------------------------------------
;; claim-result struct
;; ---------------------------------------------------------------------------

(test-case "claim-result struct is transparent"
  (define r (claim-result 'tests 10 10 #t))
  (check-equal? (claim-result-pattern r) 'tests)
  (check-equal? (claim-result-claimed r) 10)
  (check-equal? (claim-result-actual r) 10)
  (check-true (claim-result-matched? r)))

;; ---------------------------------------------------------------------------
;; parse-claim-verifier-args
;; ---------------------------------------------------------------------------

(test-case "parse-claim-verifier-args with no args returns count mode"
  (define opts (parse-claim-verifier-args '()))
  (check-equal? (hash-ref opts 'mode) 'count))

(test-case "parse-claim-verifier-args with --check sets check mode"
  (define opts (parse-claim-verifier-args '("--check" "report.md")))
  (check-equal? (hash-ref opts 'mode) 'check)
  (check-equal? (hash-ref opts 'file) "report.md"))

(test-case "parse-claim-verifier-args with --milestone sets milestone mode"
  (define opts (parse-claim-verifier-args '("--milestone" "834")))
  (check-equal? (hash-ref opts 'mode) 'milestone)
  (check-equal? (hash-ref opts 'milestone-number) 834))
