#lang racket/base
;; v1.00.24 W7: grouped-mode equivalence characterization.
;;
;; Contract under test (wave W7, no defaults changed, no eligibility broadened):
;; 1. A grouped request for an eligible module+ test file runs IN-PROCESS with
;;    requested-execution-mode "grouped", no fallback reason, and the SAME
;;    verdict / exit code / parsed counts / captured output as subprocess mode.
;; 2. Every ineligible file gets a NAMED fallback reason
;;    (missing-module-plus-test-form | declared-mutation |
;;     declared-process-isolation), executes subprocess, and is NOT counted
;;    as grouped (effective_execution_mode = "subprocess").
;; 3. Timeout, explicit exit, failure, sequential two-file execution and
;;    repeatability behave identically in both planes.
;; 4. JSON serialization is ADDITIVE: requested_execution_mode,
;;    effective_execution_mode, grouped_fallback_reason; legacy results
;;    (no mode fields) keep serializing with "subprocess"/null defaults.

(module+ test
  (require rackunit
           rackunit/text-ui
           racket/format
           racket/string
           "../scripts/run-tests/parse.rkt"
           "../scripts/run-tests/runner.rkt")

  (define FX "tests/fixtures/grouped-mode")
  (define (fx name)
    (string-append FX "/" name))

  (define f01 (fx "f01-eligible-pass.rkt"))
  (define f02 (fx "f02-output.rkt"))
  (define f03 (fx "f03-fail.rkt"))
  (define f04 (fx "f04-exit.rkt"))
  (define f05 (fx "f05-toplevel.rkt"))
  (define f06 (fx "f06-main-only.rkt"))
  (define f07 (fx "f07-declared-mutator.rkt"))
  (define f08 (fx "f08-isolation.rkt"))
  (define f09 (fx "f09-timeout.rkt"))
  (define f10 (fx "f10-undeclared-mutator.rkt"))

  ;; ------------------------------------------------------------------
  ;; Helpers
  ;; ------------------------------------------------------------------

  ;; Everything that may NOT differ between subprocess and grouped planes.
  ;; (Wall clock may differ; verdicts may not.)
  (define (comparison-key r)
    (list (classify-test-result r)
          (test-file-result-exit-code r)
          (test-file-result-passed r)
          (test-file-result-failed r)
          (test-file-result-total r)))

  (define (grouped-run path #:timeout [timeout 120000])
    (run-single-file path #:timeout timeout #:mode 'grouped))

  (define (subprocess-run path #:timeout [timeout 120000])
    (run-single-file path #:timeout timeout #:mode 'subprocess))

  (define suite
    (test-suite "grouped-mode equivalence characterization"

      ;; --------------------------------------------------------------
      ;; 1. Additive JSON schema (reporting truthfulness, W7 step 1)
      ;; --------------------------------------------------------------

      (test-case "json: grouped success result carries additive per-file fields"
        (define r (grouped-run f01))
        (define js (test-result->jsexpr r))
        (check-true (hash-has-key? js 'requested_execution_mode))
        (check-true (hash-has-key? js 'effective_execution_mode))
        (check-true (hash-has-key? js 'grouped_fallback_reason))
        (check-equal? (hash-ref js 'requested_execution_mode) "grouped")
        (check-equal? (hash-ref js 'effective_execution_mode) "grouped")
        (check-false (hash-ref js 'grouped_fallback_reason)))

      (test-case "json: fallback result names its reason and is not counted grouped"
        (define r (grouped-run f05))
        (define js (test-result->jsexpr r))
        (check-equal? (hash-ref js 'requested_execution_mode) "grouped")
        (check-equal? (hash-ref js 'effective_execution_mode) "subprocess")
        (check-equal? (hash-ref js 'grouped_fallback_reason) "missing-module-plus-test-form"))

      (test-case "json: legacy 8-field result still serializes (old evidence readable)"
        (define legacy (make-test-file-result "legacy/path.rkt" 0 #"" #"" 0 0 0 0))
        (define js (test-result->jsexpr legacy))
        (check-equal? (hash-ref js 'requested_execution_mode) "subprocess")
        (check-equal? (hash-ref js 'effective_execution_mode) "subprocess")
        (check-false (hash-ref js 'grouped_fallback_reason))
        ;; pre-existing keys untouched
        (check-equal? (hash-ref js 'exit_code) 0)
        (check-equal? (hash-ref js 'total) 0))

      ;; --------------------------------------------------------------
      ;; 2. Eligible fixture: grouped == subprocess, counted as grouped
      ;; --------------------------------------------------------------

      (test-case "eligible pass: grouped runs in-process with parity"
        (define g (grouped-run f01))
        (define s (subprocess-run f01))
        (check-equal? (test-file-result-requested-execution-mode g) "grouped")
        (check-false (test-file-result-grouped-fallback-reason g))
        (check-equal? (hash-ref (test-result->jsexpr g) 'effective_execution_mode) "grouped")
        (check-equal? (comparison-key g)
                      (comparison-key s)
                      "eligible pass verdict/counts must match across planes")
        (check-equal? (test-file-result-exit-code g) 0)
        ;; cache records the file as grouped-in-process
        (check-equal? (execution-mode-of (test-file-result-path g)) 'grouped-in-process))

      (test-case "eligible repeatability: three grouped runs are identical"
        (define g1 (grouped-run f01))
        (define g2 (grouped-run f01))
        (define g3 (grouped-run f01))
        (check-equal? (comparison-key g1) (comparison-key g2))
        (check-equal? (comparison-key g2) (comparison-key g3)))

      (test-case "sequential eligible files in one grouped process"
        (define g-a (grouped-run f01))
        (define g-b (grouped-run f02))
        (define g-a2 (grouped-run f01))
        (check-equal? (test-file-result-exit-code g-a) 0)
        (check-equal? (test-file-result-exit-code g-b) 0)
        (check-equal? (comparison-key g-a)
                      (comparison-key g-a2)
                      "first file unchanged after second file ran in same process"))

      ;; --------------------------------------------------------------
      ;; 3. stdout/stderr capture parity
      ;; --------------------------------------------------------------

      (test-case "output capture: stdout and stderr lines survive both planes"
        (define g (grouped-run f02))
        (define s (subprocess-run f02))
        (check-true (string-contains? (bytes->string/utf-8 (test-file-result-stdout-bytes g) #\uFFFD)
                                      "F02-OUT-LINE-1"))
        (check-true (string-contains? (bytes->string/utf-8 (test-file-result-stderr-bytes g) #\uFFFD)
                                      "F02-ERR-LINE-1"))
        (check-true (string-contains? (bytes->string/utf-8 (test-file-result-stdout-bytes s) #\uFFFD)
                                      "F02-OUT-LINE-1"))
        (check-true (string-contains? (bytes->string/utf-8 (test-file-result-stderr-bytes s) #\uFFFD)
                                      "F02-ERR-LINE-1")))

      ;; --------------------------------------------------------------
      ;; 4. Failure and explicit-exit parity
      ;; --------------------------------------------------------------

      (test-case "failing check: same failure classification in both planes"
        (define g (grouped-run f03))
        (define s (subprocess-run f03))
        (check-equal? (comparison-key g) (comparison-key s))
        (check-equal? (test-file-result-failed g) 1)
        (check-equal? (test-file-result-passed g) 1))

      (test-case "explicit exit 0: same verdict in both planes"
        (define g (grouped-run f04))
        (define s (subprocess-run f04))
        (check-equal? (comparison-key g) (comparison-key s))
        (check-equal? (test-file-result-exit-code g) 0))

      ;; --------------------------------------------------------------
      ;; 5. Named fallbacks; never counted as grouped
      ;; --------------------------------------------------------------

      (test-case "fallback: missing module+ test (top-level only)"
        (define g (grouped-run f05))
        (define s (subprocess-run f05))
        (check-equal? (test-file-result-requested-execution-mode g) "grouped")
        (check-equal? (test-file-result-grouped-fallback-reason g) 'missing-module-plus-test-form)
        (check-equal? (hash-ref (test-result->jsexpr g) 'effective_execution_mode) "subprocess")
        (check-equal? (execution-mode-of (test-file-result-path g)) 'subprocess)
        (check-equal? (comparison-key g) (comparison-key s)))

      (test-case "fallback: missing module+ test (module+ main only)"
        (define g (grouped-run f06))
        (check-equal? (test-file-result-grouped-fallback-reason g) 'missing-module-plus-test-form)
        (check-equal? (hash-ref (test-result->jsexpr g) 'effective_execution_mode) "subprocess"))

      (test-case "fallback: declared mutation"
        (define g (grouped-run f07))
        (define s (subprocess-run f07))
        (check-equal? (test-file-result-grouped-fallback-reason g) 'declared-mutation)
        (check-equal? (hash-ref (test-result->jsexpr g) 'effective_execution_mode) "subprocess")
        (check-equal? (comparison-key g) (comparison-key s)))

      (test-case "fallback: declared process isolation"
        (define g (grouped-run f08))
        (define s (subprocess-run f08))
        (check-equal? (test-file-result-grouped-fallback-reason g) 'declared-process-isolation)
        (check-equal? (hash-ref (test-result->jsexpr g) 'effective_execution_mode) "subprocess")
        (check-equal? (comparison-key g) (comparison-key s)))

      ;; --------------------------------------------------------------
      ;; 6. Timeout parity (short timeout, both planes)
      ;; --------------------------------------------------------------

      (test-case "timeout: same classification with short timeout in both planes"
        (define g (grouped-run f09 #:timeout 400))
        (define s (subprocess-run f09 #:timeout 400))
        (check-equal? (test-file-result-exit-code g) 2)
        (check-equal? (test-file-result-exit-code s) 2)
        (check-equal? (classify-test-result g) (classify-test-result s)))

      ;; --------------------------------------------------------------
      ;; 7. Undeclared mutation: documented leak boundary (f10)
      ;; --------------------------------------------------------------

      (test-case "undeclared mutator runs grouped and leaks env (documented boundary)"
        (define prior (getenv "W7_F10_UNDECLARED"))
        (define g (grouped-run f10))
        (check-false (test-file-result-grouped-fallback-reason g)
                     "gates cannot detect undeclared mutation: file is eligible")
        (check-equal? (hash-ref (test-result->jsexpr g) 'effective_execution_mode) "grouped")
        (check-equal? (getenv "W7_F10_UNDECLARED")
                      "leaked"
                      "host env leaked into by in-process grouped run")
        ;; restore host environment (cannot truly unset via putenv)
        (if prior
            (putenv "W7_F10_UNDECLARED" prior)
            (putenv "W7_F10_UNDECLARED" "")))

      ;; --------------------------------------------------------------
      ;; 8. Full matrix: repeated two-plane equivalence sweep
      ;; --------------------------------------------------------------

      (test-case "matrix: subprocess vs grouped equivalence holds on repeat"
        (define paths (list f01 f02 f03 f04 f05 f06 f07 f08))
        (for ([round '(1 2)])
          (for ([p paths])
            (define g (grouped-run p))
            (define s (subprocess-run p))
            (check-equal? (comparison-key g)
                          (comparison-key s)
                          (format "round ~a plane mismatch for ~a" round p)))))))

  (run-tests suite))
