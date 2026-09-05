#lang racket

;; @speed fast
;; @suite default
;; @boundary unit

;; BOUNDARY: unit

;; tests/test-run-tests-reporting-truthfulness.rkt
;;
;; Tests for the test-runner reporting truthfulness fixes (W3, #8283).
;; Verifies that compute-verdict and print-summary honestly report
;; PASS/FAIL/INCOMPLETE/INCONCLUSIVE instead of silently masking timeouts
;; and zero-test files.
;;
;; v1.00.24 W7: adds additive execution-mode telemetry tests —
;; requested_execution_mode, effective_execution_mode, and
;; grouped_fallback_reason must always be present in per-file JSON,
;; must default sanely for pre-W7 construction sites, and old evidence
;; JSON (without the new keys) must remain parseable.

(require rackunit
         rackunit/text-ui
         racket/port
         (only-in "../scripts/run-tests/parse.rkt" make-test-file-result test-result->jsexpr)
         (only-in "../scripts/run-tests/reporting.rkt"
                  compute-verdict
                  summary-exit-code
                  print-summary
                  format-verdict-line
                  test-result->ledger-jsexpr)
         json)

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

(define-test-suite
 ledger-jsexpr-escalation-tests
 (test-case "expired quarantine flags per-file JSON as escalating failure"
   ;; W8: an entry past expires_on must surface escalate=#t in the per-file
   ;; record so shard JSON evidence cannot read as a tolerated known failure.
   (define expired-entry
     (hasheq 'file
             "tests/legacy-fail.rkt"
             'category
             "ASSERTION_FAILURE"
             'owner
             "runtime"
             'first_seen
             "1.0.0"
             'release_blocking
             #f
             'issue
             "#9502"
             'notes
             "stale quarantine"
             'expires_on
             "2020-01-01"))
   (define active-entry
     (hasheq 'file
             "tests/active-flake.rkt"
             'category
             "ASSERTION_FAILURE"
             'owner
             "runtime"
             'first_seen
             "1.0.0"
             'release_blocking
             #f
             'issue
             "#9501"
             'notes
             "active flake quarantine"
             'expires_on
             "2999-12-31"))
   (define ledger (list active-entry expired-entry))
   (define expired-record (test-result->ledger-jsexpr (make-failed "tests/legacy-fail.rkt" 3) ledger))
   (check-true (hash-ref expired-record 'known_failure))
   (check-true (hash-ref expired-record 'escalate))
   (check-true (hash-ref expired-record 'quarantine_expired))
   (check-equal? (hash-ref expired-record 'expires_on) "2020-01-01")
   ;; An unexpired quarantine stays tolerated: no escalation flags.
   (define active-record (test-result->ledger-jsexpr (make-failed "tests/active-flake.rkt" 3) ledger))
   (check-true (hash-ref active-record 'known_failure))
   (check-false (hash-ref active-record 'escalate #f))
   (check-false (hash-ref active-record 'quarantine_expired #f))
   ;; Unlisted failures carry known_failure=#f only.
   (define unknown-record
     (test-result->ledger-jsexpr (make-failed "tests/brand-new-fail.rkt" 3) ledger))
   (check-false (hash-ref unknown-record 'known_failure))))

;; ---------------------------------------------------------------------------
;; v1.00.24 W7: additive per-file execution-mode telemetry.
;; ---------------------------------------------------------------------------

;; Stable named fallback reasons. When grouped mode is requested but a file
;; executes in subprocess, grouped_fallback_reason must name exactly why.
(define known-fallback-reasons
  (list "module-test-missing" "side-effect-unsafe" "worker-incompatible" "unknown"))

(define (effective-field r)
  (hash-ref (test-result->jsexpr r) 'effective_execution_mode))
(define (requested-field r)
  (hash-ref (test-result->jsexpr r) 'requested_execution_mode))
(define (fallback-field r)
  (hash-ref (test-result->jsexpr r) 'grouped_fallback_reason))

(define-test-suite
 execution-mode-telemetry-tests
 (test-case "pre-W7 construction still yields all three additive keys"
   (define r (make-passed "test-a.rkt" 5))
   (define j (test-result->jsexpr r))
   (check-true (hash-has-key? j 'requested_execution_mode))
   (check-true (hash-has-key? j 'effective_execution_mode))
   (check-true (hash-has-key? j 'grouped_fallback_reason))
   (check-equal? (requested-field r) "subprocess")
   (check-equal? (effective-field r) "subprocess")
   (check-false (fallback-field r)))
 (test-case "grouped request honored → effective grouped, no fallback reason"
   (define r
     (make-test-file-result "test-a.rkt" 0 #"" #"" 100 1 0 1 #:requested-execution-mode "grouped"))
   (check-equal? (requested-field r) "grouped")
   (check-equal? (effective-field r) "grouped")
   (check-false (fallback-field r)))
 (test-case "grouped request that ran subprocess names a stable fallback reason"
   (define r
     (make-test-file-result "test-a.rkt"
                            0
                            #""
                            #""
                            100
                            1
                            0
                            1
                            #:requested-execution-mode "grouped"
                            #:grouped-fallback-reason 'module-test-missing))
   (check-equal? (requested-field r) "grouped")
   (check-equal? (effective-field r) "subprocess")
   (define reason (fallback-field r))
   (check-true (string? reason))
   (check-not-false (member reason known-fallback-reasons)
                    "fallback reason must be from the stable named set"))
 (test-case "fallback reason is a JSON string, not a symbol serialization artifact"
   (define r
     (make-test-file-result "test-a.rkt"
                            0
                            #""
                            #""
                            100
                            1
                            0
                            1
                            #:requested-execution-mode "grouped"
                            #:grouped-fallback-reason 'side-effect-unsafe))
   (check-equal? (fallback-field r) "side-effect-unsafe"))
 (test-case "effective mode never claims grouped while a fallback reason is present"
   (define r
     (make-test-file-result "test-a.rkt"
                            0
                            #""
                            #""
                            100
                            1
                            0
                            1
                            #:requested-execution-mode "grouped"
                            #:grouped-fallback-reason 'side-effect-unsafe))
   (check-not-equal? (effective-field r) "grouped"))
 (test-case "telemetry survives a JSON round-trip"
   (define r
     (make-test-file-result "test-a.rkt"
                            0
                            #""
                            #""
                            100
                            1
                            0
                            1
                            #:requested-execution-mode "grouped"
                            #:grouped-fallback-reason 'module-test-missing))
   (define round-tripped
     (read-json (open-input-string (with-output-to-string (lambda ()
                                                            (write-json (test-result->jsexpr r)))))))
   (check-equal? (hash-ref round-tripped 'requested_execution_mode) "grouped")
   (check-equal? (hash-ref round-tripped 'effective_execution_mode) "subprocess")
   (check-equal? (hash-ref round-tripped 'grouped_fallback_reason) "module-test-missing")))

(define-test-suite
 old-evidence-compat-tests
 (test-case "old evidence JSON without additive keys remains parseable"
   ;; Pre-W7 evidence shape: no requested/effective/fallback keys.
   (define old-json
     "{\"path\":\"tests/old.rkt\",\"category\":\"PASS\",\"exit_code\":0,\"elapsed_ms\":10,\"passed\":1,\"failed\":0,\"total\":1,\"output\":\"ok\"}")
   (define parsed (read-json (open-input-string old-json)))
   (check-equal? (hash-ref parsed 'path) "tests/old.rkt")
   ;; Additive consumers must use defaults, never assume presence.
   (check-false (hash-ref parsed 'requested_execution_mode #f))
   (check-false (hash-ref parsed 'effective_execution_mode #f))
   (check-false (hash-ref parsed 'grouped_fallback_reason #f)))
 (test-case "old run summary shape still parses alongside new per-file entries"
   (define old-run
     "{\"execution_mode\":\"subprocess\",\"file_count\":1,\"pass\":1,\"fail\":0,\"timeout\":0,\"skip\":0}")
   (define parsed (read-json (open-input-string old-run)))
   (check-equal? (hash-ref parsed 'execution_mode) "subprocess")
   (check-equal? (hash-ref parsed 'file_count) 1)))

(run-tests (make-test-suite "run-tests reporting truthfulness"
                            (list compute-verdict-tests
                                  summary-exit-code-tests
                                  format-verdict-line-tests
                                  print-summary-verdict-tests
                                  ledger-jsexpr-escalation-tests
                                  execution-mode-telemetry-tests
                                  old-evidence-compat-tests)))
