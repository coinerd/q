#lang racket

;; @speed fast
;; @suite default

;; BOUNDARY: unit

;; tests/test-run-tests-reporting-truthfulness.rkt
;;
;; Tests for the test-runner reporting truthfulness fixes (W3, #8283).
;; Verifies that compute-verdict and print-summary honestly report
;; PASS/FAIL/INCOMPLETE/INCONCLUSIVE instead of silently masking timeouts
;; and zero-test files.

(require rackunit
         rackunit/text-ui
         racket/port
         (only-in "../scripts/run-tests/parse.rkt" make-test-file-result)
         (only-in "../scripts/run-tests/reporting.rkt"
                  compute-verdict
                  summary-exit-code
                  print-summary
                  format-verdict-line))

;; Helpers for constructing results
(define (make-passed path n)
  (make-test-file-result path 0 #"" #"" 100 n 0 n))

(define (make-failed path n)
  (make-test-file-result path 1 #"" #"" 100 (- n 1) 1 n))

(define (make-timeout path)
  (make-test-file-result path 2 #"" #"" 5000 0 0 0))

(define (make-zero-test path)
  (make-test-file-result path 0 #"" #"" 50 0 0 0))

(define-test-suite
 compute-verdict-tests
 (test-case "all-passing results with tests → 'pass"
   (define results (list (make-passed "test-a.rkt" 10) (make-passed "test-b.rkt" 5)))
   (check-equal? (compute-verdict results) 'pass))
 (test-case "any failure → 'fail"
   (define results (list (make-passed "test-a.rkt" 10) (make-failed "test-b.rkt" 5)))
   (check-equal? (compute-verdict results) 'fail))
 (test-case "timeout without failures → 'incomplete"
   (define results (list (make-passed "test-a.rkt" 10) (make-timeout "test-slow.rkt")))
   (check-equal? (compute-verdict results) 'incomplete))
 (test-case "both failure and timeout → 'fail (fail takes priority)"
   (define results (list (make-failed "test-a.rkt" 5) (make-timeout "test-b.rkt")))
   (check-equal? (compute-verdict results) 'fail))
 (test-case "zero tests across all files → 'inconclusive"
   (define results (list (make-zero-test "test-a.rkt") (make-zero-test "test-b.rkt")))
   (check-equal? (compute-verdict results) 'inconclusive))
 (test-case "mixed zero-test and passing → 'pass (at least some tests ran)"
   (define results (list (make-zero-test "test-a.rkt") (make-passed "test-b.rkt" 3)))
   (check-equal? (compute-verdict results) 'pass))
 (test-case "empty results → 'inconclusive"
   (check-equal? (compute-verdict '()) 'inconclusive)))

(define-test-suite summary-exit-code-tests
                   (test-case "no failures, no timeouts → 0 (pass)"
                     (check-equal? (summary-exit-code 0 0) 0))
                   (test-case "failures only → 1"
                     (check-equal? (summary-exit-code 3 0) 1))
                   (test-case "timeouts only → 2"
                     (check-equal? (summary-exit-code 0 2) 2))
                   (test-case "both failures and timeouts → 3"
                     (check-equal? (summary-exit-code 2 1) 3)))

(define-test-suite format-verdict-line-tests
                   (test-case "pass verdict"
                     (check-true (string-contains? (format-verdict-line 'pass 0) "PASS")))
                   (test-case "fail verdict"
                     (check-true (string-contains? (format-verdict-line 'fail 0) "FAIL")))
                   (test-case "incomplete verdict includes timeout count"
                     (define line (format-verdict-line 'incomplete 3))
                     (check-true (string-contains? line "INCOMPLETE"))
                     (check-true (string-contains? line "3")))
                   (test-case "inconclusive verdict"
                     (check-true (string-contains? (format-verdict-line 'inconclusive 0)
                                                   "INCONCLUSIVE"))))

(define-test-suite
 print-summary-verdict-tests
 (test-case "summary includes VERDICT line for passing results"
   (define out (open-output-string))
   (parameterize ([current-output-port out])
     (print-summary (list (make-passed "test-a.rkt" 5)) 100))
   (define output (get-output-string out))
   (check-true (string-contains? output "VERDICT"))
   (check-true (string-contains? output "PASS")))
 (test-case "summary includes FAIL verdict for failing results"
   (define out (open-output-string))
   (parameterize ([current-output-port out])
     (print-summary (list (make-failed "test-a.rkt" 5)) 100))
   (define output (get-output-string out))
   (check-true (string-contains? output "VERDICT"))
   (check-true (string-contains? output "FAIL")))
 (test-case "summary includes INCOMPLETE verdict for timeout results"
   (define out (open-output-string))
   (parameterize ([current-output-port out])
     (print-summary (list (make-passed "test-a.rkt" 5) (make-timeout "test-slow.rkt")) 100))
   (define output (get-output-string out))
   (check-true (string-contains? output "VERDICT"))
   (check-true (string-contains? output "INCOMPLETE"))
   (check-true (string-contains? output "timeout")))
 (test-case "summary warns about zero-test files"
   (define out (open-output-string))
   (parameterize ([current-output-port out])
     (print-summary (list (make-passed "test-a.rkt" 5) (make-zero-test "test-empty.rkt")) 100))
   (define output (get-output-string out))
   (check-true (string-contains? output "zero parsed tests"))
   ;; Should still pass since some tests ran
   (check-true (string-contains? output "PASS"))))

(run-tests (make-test-suite "run-tests reporting truthfulness"
                            (list compute-verdict-tests
                                  summary-exit-code-tests
                                  format-verdict-line-tests
                                  print-summary-verdict-tests)))
