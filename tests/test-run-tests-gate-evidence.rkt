#lang racket

;; BOUNDARY: unit
;; Tests for gate evidence v2 in run-tests.rkt (v0.83.1 W1)

(require rackunit
         racket/port
         racket/file
         racket/runtime-path)

(define-runtime-path runner-path "../scripts/run-tests.rkt")

(define runner-loaded? (box #f))
(define runner-cache (make-hash))

(define (runner-ref sym)
  (unless (unbox runner-loaded?)
    (dynamic-require runner-path #f)
    (set-box! runner-loaded? #t))
  (hash-ref! runner-cache sym (lambda () (dynamic-require runner-path sym))))

;; ---------------------------------------------------------------------------
;; Unit tests: record-gate-evidence! guards
;; ---------------------------------------------------------------------------

(test-case "record-gate-evidence!: rejects empty results"
  (define rge (runner-ref 'record-gate-evidence!))
  (check-exn exn:fail?
             (lambda () (rge "test-suite" #:results '() #:args '() #:jobs 1 #:file-count 1))))

(test-case "record-gate-evidence!: rejects zero parsed tests"
  (define rge (runner-ref 'record-gate-evidence!))
  (define make-tfr (runner-ref 'make-test-file-result))
  (define result (make-tfr "tests/fake.rkt" 0 #"" #"" 100 0 0 0))
  (check-exn exn:fail?
             (lambda ()
               (rge "test-suite" #:results (list result) #:args '() #:jobs 1 #:file-count 1))))

(test-case "record-gate-evidence!: rejects results with failures"
  (define rge (runner-ref 'record-gate-evidence!))
  (define make-tfr (runner-ref 'make-test-file-result))
  (define result (make-tfr "tests/fake.rkt" 1 #"1 failed\n" #"" 100 0 1 1))
  (check-exn exn:fail?
             (lambda ()
               (rge "test-suite" #:results (list result) #:args '() #:jobs 1 #:file-count 1))))

(test-case "record-gate-evidence!: writes valid JSON for passing results"
  (define rge (runner-ref 'record-gate-evidence!))
  (define make-tfr (runner-ref 'make-test-file-result))
  (define result (make-tfr "tests/fake.rkt" 0 #"5 tests passed\n" #"" 500 5 0 5))
  ;; Use the project .gate-evidence dir (version.rkt is project-relative)
  (define evid-dir (build-path (current-directory) ".gate-evidence"))
  (define test-label "_test-gate-v2")
  (rge test-label
       #:results (list result)
       #:args '("--suite" "test")
       #:jobs 2
       #:timeout 60
       #:repeat 1
       #:file-count 1
       #:inventory-hash "abc123")
  (define evidence-file (build-path evid-dir (format "~a.json" test-label)))
  (check-true (file-exists? evidence-file))
  (define content (file->string evidence-file))
  ;; Verify required fields
  (check-not-false (regexp-match? #rx"\"version\":" content))
  (check-not-false (regexp-match? #rx"\"git_sha\":" content))
  (check-not-false (regexp-match? #rx"\"suite\":" content) content)
  (check-not-false (regexp-match? #rx"\"selected_file_count\": 1" content))
  (check-not-false (regexp-match? #rx"\"parsed_test_count\": 5" content))
  (check-not-false (regexp-match? #rx"\"passed\": 5" content))
  (check-not-false (regexp-match? #rx"\"failed\": 0" content))
  (check-not-false (regexp-match? #rx"\"inventory_hash\":" content))
  (check-not-false (regexp-match? #rx"\"timestamp\":" content))
  ;; Cleanup test evidence
  (delete-file evidence-file))
