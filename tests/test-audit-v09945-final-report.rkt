#lang racket

;; @speed fast
(require rackunit
         racket/runtime-path
         racket/string)

(define-runtime-path report-path "../docs/reports/AUDIT-v0.99.45-FINAL-REPORT.md")

(define report-text (file->string report-path))

(define (contains? needle)
  (string-contains? report-text needle))

(test-case "v0.99.45 final report uses corrected audit truth"
  (check-false (contains? "782 tests"))
  (check-true (contains? "663 tests"))
  (check-false (contains? "5 low-severity issues"))
  (check-true (contains? "6 low-severity issues"))
  (check-true (contains? "v0.99.45 was audit-only"))
  (check-true (contains? "no v0.99.45 tag or GitHub Release"))
  (check-true (contains? "#8666–#8671"))
  (check-true (contains? "v0.99.46")))

(test-case "v0.99.45 final report distinguishes smoke gate from audit suite"
  (check-true (contains? "Smoke gate"))
  (check-true (contains? "did not include the v0.99.45 audit tests"))
  (check-true (contains? "fast/default suite"))
  (check-true (contains? "663 test cases"))
  (check-true (contains? "1,626 check assertions")))
