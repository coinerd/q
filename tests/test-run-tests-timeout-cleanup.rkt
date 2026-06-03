#lang racket/base

;; BOUNDARY: unit
;; @suite runtime
;; @boundary unit
;; @speed fast
;; @mutates env,cwd
;; tests/test-run-tests-timeout-cleanup.rkt — Timeout cleanup and output bounds tests
;;
;; Tests for:
;; - Unique failure log names (no basename collision)
;; - Output cap prevents memory blowup
;; - Process cleanup characterization

(require rackunit
         racket/file
         racket/string
         racket/path
         "../scripts/run-tests.rkt")

;; ---------------------------------------------------------------------------
;; Unique failure log names
;; ---------------------------------------------------------------------------

(test-case "failure log name includes path hash for uniqueness"
  (define name1 (make-unique-log-name "tests/test-foo.rkt"))
  (define name2 (make-unique-log-name "other/test-foo.rkt"))
  ;; Same basename but different full paths should produce different log names
  (check-not-equal? name1 name2 "Different paths with same basename must produce unique log names"))

(test-case "failure log name preserves basename readability"
  (define name (make-unique-log-name "tests/test-provider-scenarios.rkt"))
  (check-true (string-contains? name "test-provider-scenarios")
              "Log name should contain readable basename"))

(test-case "failure log name handles special characters"
  (define name (make-unique-log-name "tests/weird name (v2).rkt"))
  (check-false (string-contains? name "(") "Special characters should be sanitized"))

;; ---------------------------------------------------------------------------
;; Output truncation
;; ---------------------------------------------------------------------------

(test-case "truncate-output preserves head and tail"
  (define output
    (apply string-append
           (for/list ([i (in-range 1000)])
             (format "line ~a\n" i))))
  (define result (truncate-test-output output 500))
  (check-true (string-contains? result "line 0") "Head should be preserved")
  (check-true (string-contains? result "line 999") "Tail should be preserved")
  (check-true (string-contains? result "... truncated") "Truncation marker should be present"))

(test-case "truncate-output is no-op for short output"
  (define output "short output\n")
  (define result (truncate-test-output output 1000))
  (check-equal? result output "Short output should not be truncated"))

(test-case "truncate-output handles empty string"
  (define result (truncate-test-output "" 100))
  (check-equal? result ""))

;; ---------------------------------------------------------------------------
;; Process cleanup: subprocess result structure
;; ---------------------------------------------------------------------------

(test-case "test-file-result has stdout/stderr bytes"
  (define result (make-test-file-result "tests/fake.rkt" 0 #"ok" #"" 100 1 0 1))
  (check-equal? (test-file-result-path result) "tests/fake.rkt")
  (check-equal? (test-file-result-exit-code result) 0)
  (check-equal? (test-file-result-stdout-bytes result) #"ok")
  (check-equal? (test-file-result-stderr-bytes result) #""))

;; ---------------------------------------------------------------------------
;; save-failure-logs uses make-unique-log-name (T1)
;; ---------------------------------------------------------------------------

(test-case "save-failure-logs writes unique log files"
  (define fake-result (make-test-file-result "tests/test-foo.rkt" 1 #"failed" #"error" 50 0 1 1))
  (save-failure-logs (list fake-result))
  ;; The log path should use make-unique-log-name (with path hash)
  (define expected-name (make-unique-log-name "tests/test-foo.rkt"))
  (define log-path (build-path "/tmp" expected-name))
  (check-true (file-exists? log-path) (format "Log file should exist at ~a" log-path))
  ;; Cleanup
  (when (file-exists? log-path)
    (delete-file log-path)))
