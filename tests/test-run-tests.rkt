#lang racket

;; Tests for the rewritten run-tests.rkt test runner.
;; Wave 13 Subs 13.1–13.4: struct, parsing, running, summary functions

(require rackunit
         racket/port
         racket/string
         racket/file
         racket/path)

;; ---------------------------------------------------------------------------
;; Require the runner module and get all exports
;; ---------------------------------------------------------------------------

;; find-system-path 'orig-dir gives the CWD when racket was invoked
;; This works under both `racket file.rkt` and `raco test -t file.rkt`
(define runner-path
  (simplify-path (build-path (find-system-path 'orig-dir) "scripts" "run-tests.rkt")))

;; Load module once, cache bindings
(define runner-loaded? (box #f))
(define runner-cache (make-hash))

(define (runner-ref sym)
  (unless (unbox runner-loaded?)
    (dynamic-require runner-path #f)
    (set-box! runner-loaded? #t))
  (hash-ref! runner-cache sym (lambda () (dynamic-require runner-path sym))))

;; ---------------------------------------------------------------------------
;; 13.1: test-file-result struct
;; ---------------------------------------------------------------------------

(test-case "test-file-result: construction and access"
  (define tfr (runner-ref 'make-test-file-result))
  (define result (tfr "tests/test-foo.rkt" 0 #"5 tests passed\n" #"" 1234 5 0 5))
  (check-equal? ((runner-ref 'test-file-result-path) result) "tests/test-foo.rkt")
  (check-equal? ((runner-ref 'test-file-result-exit-code) result) 0)
  (check-equal? ((runner-ref 'test-file-result-stdout-bytes) result) #"5 tests passed\n")
  (check-equal? ((runner-ref 'test-file-result-stderr-bytes) result) #"")
  (check-equal? ((runner-ref 'test-file-result-elapsed-ms) result) 1234)
  (check-equal? ((runner-ref 'test-file-result-passed) result) 5)
  (check-equal? ((runner-ref 'test-file-result-failed) result) 0)
  (check-equal? ((runner-ref 'test-file-result-total) result) 5))

(test-case "test-file-result: failed result"
  (define tfr (runner-ref 'make-test-file-result))
  (define result (tfr "tests/test-bar.rkt" 1 #"1 test passed\n1 test failed\n" #"" 5678 1 1 2))
  (check-equal? ((runner-ref 'test-file-result-exit-code) result) 1)
  (check-equal? ((runner-ref 'test-file-result-passed) result) 1)
  (check-equal? ((runner-ref 'test-file-result-failed) result) 1)
  (check-equal? ((runner-ref 'test-file-result-total) result) 2))

(test-case "test-file-result: timeout result (exit-code 2)"
  (define tfr (runner-ref 'make-test-file-result))
  (define result (tfr "tests/test-slow.rkt" 2 #"" #"timeout\n" 60000 0 0 0))
  (check-equal? ((runner-ref 'test-file-result-exit-code) result) 2)
  (check-equal? ((runner-ref 'test-file-result-elapsed-ms) result) 60000))

;; ---------------------------------------------------------------------------
;; 13.1: parse-raco-output
;; ---------------------------------------------------------------------------

(test-case "parse-raco-output: all pass"
  (define parse (runner-ref 'parse-raco-output))
  (define-values (passed failed total) (parse #"raco test: test-foo.rkt\n5 tests passed\n"))
  (check-equal? passed 5)
  (check-equal? failed 0)
  (check-equal? total 5))

(test-case "parse-raco-output: pass and fail"
  (define parse (runner-ref 'parse-raco-output))
  (define-values (passed failed total) (parse #"3 tests passed\n2 tests failed\n"))
  (check-equal? passed 3)
  (check-equal? failed 2)
  (check-equal? total 5))

(test-case "parse-raco-output: zero tests"
  (define parse (runner-ref 'parse-raco-output))
  (define-values (passed failed total) (parse #"0 tests passed\n"))
  (check-equal? passed 0)
  (check-equal? failed 0)
  (check-equal? total 0))

(test-case "parse-raco-output: no recognizable output returns zeros"
  (define parse (runner-ref 'parse-raco-output))
  (define-values (passed failed total) (parse #"some random output\n"))
  (check-equal? passed 0)
  (check-equal? failed 0)
  (check-equal? total 0))

(test-case "parse-raco-output: singular 'test passed' / 'test failed'"
  (define parse (runner-ref 'parse-raco-output))
  (define-values (passed failed total) (parse #"1 test passed\n1 test failed\n"))
  (check-equal? passed 1)
  (check-equal? failed 1)
  (check-equal? total 2))

(test-case "parse-raco-output: multiple failures output"
  (define parse (runner-ref 'parse-raco-output))
  (define-values (passed failed total) (parse #"3 tests passed\n2 tests failed\n"))
  (check-equal? passed 3)
  (check-equal? failed 2)
  (check-equal? total 5))

;; ---------------------------------------------------------------------------
;; 13.1: extract-failure-lines
;; ---------------------------------------------------------------------------

(test-case "extract-failure-lines: no failures returns empty"
  (define extract (runner-ref 'extract-failure-lines))
  (check-equal? (extract #"5 tests passed\n") '()))

(test-case "extract-failure-lines: extracts failure block"
  (define extract (runner-ref 'extract-failure-lines))
  (define output
    (bytes-append #"raco test: test-bar.rkt\n"
                  #"-------------------- FAILURE --------------------\n"
                  #"name:       check-equal?\n"
                  #"location:   test-bar.rkt:42:15\n"
                  #"actual:     1\n"
                  #"expected:   2\n"
                  #"----------------------------------------\n"
                  #"1 test passed\n"
                  #"1 test failed\n"))
  (define lines (extract output))
  (check-true (>= (length lines) 4))
  (check-not-false (member "name:       check-equal?" lines))
  (check-not-false (member "actual:     1" lines))
  (check-not-false (member "expected:   2" lines)))

(test-case "extract-failure-lines: multiple failure blocks"
  (define extract (runner-ref 'extract-failure-lines))
  (define output
    (bytes-append #"-------------------- FAILURE --------------------\n"
                  #"name:       check-equal?\n"
                  #"actual:     a\n"
                  #"expected:   b\n"
                  #"----------------------------------------\n"
                  #"-------------------- FAILURE --------------------\n"
                  #"name:       check-true\n"
                  #"actual:     #f\n"
                  #"expected:   #t\n"
                  #"----------------------------------------\n"
                  #"3 tests passed\n2 tests failed\n"))
  (define lines (extract output))
  (check-true (>= (length lines) 8))
  (check-not-false (member "name:       check-equal?" lines))
  (check-not-false (member "name:       check-true" lines)))

;; ---------------------------------------------------------------------------
;; 13.4: format-duration
;; ---------------------------------------------------------------------------

(test-case "format-duration: zero ms"
  (define fmt (runner-ref 'format-duration))
  (check-equal? (fmt 0) "0s"))

(test-case "format-duration: seconds"
  (define fmt (runner-ref 'format-duration))
  (check-equal? (fmt 1500) "1.5s"))

(test-case "format-duration: minutes"
  (define fmt (runner-ref 'format-duration))
  (check-equal? (fmt 70000) "1m 10.0s"))

(test-case "format-duration: hours"
  (define fmt (runner-ref 'format-duration))
  (check-equal? (fmt 3723000) "1h 2m 3.0s"))

;; ---------------------------------------------------------------------------
;; 13.4: summary-exit-code
;; ---------------------------------------------------------------------------

(test-case "summary-exit-code: all pass"
  (define sec (runner-ref 'summary-exit-code))
  (check-equal? (sec 0 0) 0))

(test-case "summary-exit-code: failures"
  (define sec (runner-ref 'summary-exit-code))
  (check-equal? (sec 3 0) 1))

(test-case "summary-exit-code: timeouts only"
  (define sec (runner-ref 'summary-exit-code))
  (check-equal? (sec 0 2) 2))

(test-case "summary-exit-code: failures and timeouts"
  (define sec (runner-ref 'summary-exit-code))
  (check-equal? (sec 3 2) 3))

;; ---------------------------------------------------------------------------
;; 13.2: run-single-file (uses actual raco test on a real test file)
;; ---------------------------------------------------------------------------

(test-case "run-single-file: passing test file"
  (define run (runner-ref 'run-single-file))
  (define result (run "tests/test-version.rkt" #:timeout 60000))
  (check-equal? ((runner-ref 'test-file-result-exit-code) result) 0)
  (check-true (>= ((runner-ref 'test-file-result-passed) result) 1))
  (check-equal? ((runner-ref 'test-file-result-failed) result) 0))

(test-case "run-single-file: nonexistent file returns error"
  (define run (runner-ref 'run-single-file))
  (define result (run "tests/nonexistent-test-file-xyz.rkt" #:timeout 10000))
  (check-not-equal? ((runner-ref 'test-file-result-exit-code) result) 0))

;; ---------------------------------------------------------------------------
;; 13.3: collect-test-files (suite filtering)
;; ---------------------------------------------------------------------------

(test-case "collect-test-files: all suite returns tests/ path"
  (define collect (runner-ref 'collect-test-files))
  (define files (collect 'all))
  (check-not-false (member "tests/" files)))

(test-case "collect-test-files: smoke suite returns list of files"
  (define collect (runner-ref 'collect-test-files))
  (define files (collect 'smoke))
  (check-true (list? files))
  (check-true (> (length files) 0))
  (for ([f (in-list files)])
    (check-false (string-contains? f "/workflows/"))
    (check-false (string-contains? f "/interfaces/"))))

(test-case "collect-test-files: explicit files override suite"
  (define collect (runner-ref 'collect-test-files))
  (define files (collect 'all #:extra-files '("tests/test-version.rkt")))
  (check-equal? files '("tests/test-version.rkt")))
