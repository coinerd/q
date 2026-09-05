#lang racket

;; @speed slow
;; @suite default
;; @boundary integration

;; BOUNDARY: unit
;; Tests for gate evidence v2 in run-tests.rkt (v0.83.1 W1)

(require rackunit
         racket/port
         racket/file
         racket/runtime-path
         "../util/version.rkt")

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
  ;; W3: explicit shard identity (schema migration — every evidence
  ;; file must state whether it is a full-suite run or a partial shard).
  (check-not-false (regexp-match? #rx"\"shard_index\": 0" content))
  (check-not-false (regexp-match? #rx"\"shard_total\": 1" content))
  ;; Effective args preserved verbatim (shard/unsharded runs distinguishable).
  (check-not-false (regexp-match? #rx"\"--suite\"" content))
  ;; Cleanup test evidence
  (delete-file evidence-file))

;; ---------------------------------------------------------------------------
;; W3: truthful gate evidence — shard refusal, complete-run proof,
;; explicit shard identity, fail-closed validation.
;; ---------------------------------------------------------------------------

;; validate-gate-evidence! and gate-evidence-refusal are not re-exported by
;; the run-tests.rkt facade; require the module directly.
(define gate-evidence-module "../scripts/run-tests/gate-evidence.rkt")

(define (gate-evidence-ref sym)
  (dynamic-require gate-evidence-module sym))

(test-case "gate-evidence-refusal: unsharded allowed; shards and invalid configs refused"
  (define refusal (gate-evidence-ref 'gate-evidence-refusal))
  (check-false (refusal 0 1))
  (check-not-false (refusal 0 2))
  (check-not-false (refusal 1 3))
  (check-not-false (refusal 2 2))
  (check-not-false (refusal 0 0))
  (check-not-false (refusal #f 1)))

(test-case "record-gate-evidence!: refuses sharded runs without writing any evidence"
  (define rge (runner-ref 'record-gate-evidence!))
  (define make-tfr (runner-ref 'make-test-file-result))
  (define result (make-tfr "tests/fake.rkt" 0 #"5 tests passed\n" #"" 500 5 0 5))
  (define evid-dir (build-path (current-directory) ".gate-evidence"))
  (define test-label "_test-gate-shard")
  (check-exn exn:fail?
             (lambda ()
               (rge test-label
                    #:results (list result)
                    #:args '("--shard-index" "1" "--shard-total" "3")
                    #:jobs 2
                    #:timeout 60
                    #:repeat 1
                    #:file-count 1
                    #:shard-index 1
                    #:shard-total 3)))
  ;; A sharded run must never create or overwrite ANY suite evidence file,
  ;; shard-labelled or not.
  (check-false (file-exists? (build-path evid-dir (format "~a.json" test-label))))
  (check-false (file-exists? (build-path evid-dir (format "~a.shard-1-of-3.json" test-label)))))

(test-case "record-gate-evidence!: refuses incomplete run (missing RUN-SUMMARY) — no PASS evidence"
  (define rge (runner-ref 'record-gate-evidence!))
  (define make-tfr (runner-ref 'make-test-file-result))
  (define result (make-tfr "tests/fake.rkt" 0 #"5 tests passed\n" #"" 500 5 0 5))
  (define evid-dir (build-path (current-directory) ".gate-evidence"))
  (define test-label "_test-gate-incomplete")
  ;; 1 result for 2 selected files: the run was interrupted before the
  ;; summary covered the whole suite — recording must refuse, not PASS.
  (check-exn exn:fail?
             (lambda ()
               (rge test-label
                    #:results (list result)
                    #:args '()
                    #:jobs 1
                    #:file-count 2
                    #:inventory-hash "abc123")))
  (check-false (file-exists? (build-path evid-dir (format "~a.json" test-label)))))

(test-case "validate-gate-evidence!: rejects evidence without explicit shard identity (fail closed)"
  (define validate (gate-evidence-ref 'validate-gate-evidence!))
  (check-exn exn:fail?
             (lambda ()
               (validate (hasheq 'version
                                 q-version
                                 'git-sha
                                 "0123456789abcdef0123456789abcdef01234567"
                                 'parsed-test-count
                                 5
                                 'passed
                                 5
                                 'failed
                                 0
                                 'timed-out
                                 0
                                 'timestamp
                                 (current-seconds))))))

(test-case "validate-gate-evidence!: rejects partial shard evidence as full-suite gate"
  (define validate (gate-evidence-ref 'validate-gate-evidence!))
  (check-exn exn:fail?
             (lambda ()
               (validate (hasheq 'version
                                 q-version
                                 'git-sha
                                 "0123456789abcdef0123456789abcdef01234567"
                                 'shard-index
                                 1
                                 'shard-total
                                 3
                                 'parsed-test-count
                                 5
                                 'passed
                                 5
                                 'failed
                                 0
                                 'timed-out
                                 0
                                 'timestamp
                                 (current-seconds))))))

(test-case "validate-gate-evidence!: accepts explicit full-suite evidence (shard 0/1)"
  (define validate (gate-evidence-ref 'validate-gate-evidence!))
  (check-not-exn (lambda ()
                   (validate (hasheq 'version
                                     q-version
                                     'git-sha
                                     "0123456789abcdef0123456789abcdef01234567"
                                     'shard-index
                                     0
                                     'shard-total
                                     1
                                     'parsed-test-count
                                     5
                                     'passed
                                     5
                                     'failed
                                     0
                                     'timed-out
                                     0
                                     'timestamp
                                     (current-seconds))))))
