#lang racket/base

;; q/scripts/run-tests/cli.rkt — CLI argument parsing
;;
;; Usage, parse-args, validate-args!, known-suites.
;; Extracted from run-tests.rkt (v0.96.16, AX1-2).
;; STABILITY: internal

(require racket/match
         racket/string
         (only-in racket/future processor-count))

(provide usage
         parse-args
         validate-args!
         known-suites
         known-modes)

(define (usage)
  (displayln "Usage: racket scripts/run-tests.rkt [OPTIONS] [TEST-FILES ...]")
  (newline)
  (displayln "Options:")
  (displayln "  --jobs N          Number of parallel jobs (default: processor-count)")
  (displayln "  --sequential      Run tests sequentially (jobs=1)")
  (displayln "  --timeout SECS    Per-file timeout in seconds")
  (displayln "  --mode <name>     Execution mode: auto (default), subprocess, in-process, grouped")
  (displayln
   "  --suite <name>    Run test suite: all/broad (default all), fast, unit-fast, slow, tui, smoke, security, arch, runtime, extensions, workflows")
  (displayln "  --strict          Enable strict zero-test detection (default: on)")
  (displayln "  --repeat N        Run suite N times (exit 1 if any run fails)")
  (displayln "  --record-gate-evidence  Write .gate-evidence/<suite>.passed on success")
  (displayln "  --inventory             Print inventory report (selected/excluded files) and exit")
  (displayln "  --diagnose-overhead     Measure Racket/raco per-file startup overhead and exit")
  (displayln "  --json-out PATH         Write structured per-file JSON results")
  (displayln
   "  --ledger PATH           Read known-failure ledger JSON and report known/new/resolved failures")
  (displayln "  --help            Show this help message")
  (newline)
  (displayln "Suites:")
  (displayln "  all     Entire tests/ directory (per-file spawn)")
  (displayln "  broad   Alias for all discoverable tests (per-file spawn)")
  (displayln "  fast    All tests except slow patterns (per-file spawn)")
  (displayln "  unit-fast  Fast unit tests eligible for in-process/grouped execution")
  (displayln "  slow    Only sandbox/subprocess tests")
  (displayln "  tui     Files in tests/tui/")
  (displayln "  smoke   Fast minus workflows/, interfaces/, and provider tests")
  (displayln "  security  All security/permission/sandbox/safe-mode tests")
  (displayln "  arch    Architecture boundary/fitness tests")
  (displayln "  runtime Runtime/session/compaction/iteration tests")
  (displayln "  extensions Extension/GSD/hook tests")
  (displayln "  workflows All tests/workflows/ including fixture self-tests (integration-level)"))

(define known-suites
  '(all broad fast unit-fast slow smoke tui security arch runtime extensions workflows))
(define known-modes '(auto subprocess in-process grouped))

(define (parse-args args)
  (let loop ([rest args]
             [jobs (processor-count)]
             [sequential? #f]
             [timeout #f]
             [strict? #t]
             [suite 'all]
             [extra '()]
             [repeat 1]
             [record-gate? #f]
             [inventory? #f]
             [diagnose-overhead? #f]
             [mode 'auto]
             [json-out #f]
             [ledger #f])
    (match rest
      ['()
       (values jobs
               sequential?
               timeout
               strict?
               suite
               (reverse extra)
               repeat
               record-gate?
               inventory?
               diagnose-overhead?
               mode
               json-out
               ledger)]
      [(list "--help" _ ...)
       (usage)
       (exit 0)]
      [(list "--strict" rest ...)
       (loop rest
             jobs
             sequential?
             timeout
             #t
             suite
             extra
             repeat
             record-gate?
             inventory?
             diagnose-overhead?
             mode
             json-out
             ledger)]
      [(list "--jobs" n rest ...)
       (loop rest
             (string->number n)
             sequential?
             timeout
             strict?
             suite
             extra
             repeat
             record-gate?
             inventory?
             diagnose-overhead?
             mode
             json-out
             ledger)]
      [(list "--sequential" rest ...)
       (loop rest
             1
             #t
             timeout
             strict?
             suite
             extra
             repeat
             record-gate?
             inventory?
             diagnose-overhead?
             mode
             json-out
             ledger)]
      [(list "--timeout" secs rest ...)
       (loop rest
             jobs
             sequential?
             (string->number secs)
             strict?
             suite
             extra
             repeat
             record-gate?
             inventory?
             diagnose-overhead?
             mode
             json-out
             ledger)]
      [(list "--mode" name rest ...)
       (loop rest
             jobs
             sequential?
             timeout
             strict?
             suite
             extra
             repeat
             record-gate?
             inventory?
             diagnose-overhead?
             (string->symbol name)
             json-out
             ledger)]
      [(list "--suite" name rest ...)
       (loop rest
             jobs
             sequential?
             timeout
             strict?
             (string->symbol name)
             extra
             repeat
             record-gate?
             inventory?
             diagnose-overhead?
             mode
             json-out
             ledger)]
      [(list "--repeat" n rest ...)
       (loop rest
             jobs
             sequential?
             timeout
             strict?
             suite
             extra
             (string->number n)
             record-gate?
             inventory?
             diagnose-overhead?
             mode
             json-out
             ledger)]
      [(list "--record-gate-evidence" rest ...)
       (loop rest
             jobs
             sequential?
             timeout
             strict?
             suite
             extra
             repeat
             #t
             inventory?
             diagnose-overhead?
             mode
             json-out
             ledger)]
      [(list "--inventory" rest ...)
       (loop rest
             jobs
             sequential?
             timeout
             strict?
             suite
             extra
             repeat
             record-gate?
             #t
             diagnose-overhead?
             mode
             json-out
             ledger)]
      [(list "--diagnose-overhead" rest ...)
       (loop rest
             jobs
             sequential?
             timeout
             strict?
             suite
             extra
             repeat
             record-gate?
             inventory?
             #t
             mode
             json-out
             ledger)]
      [(list "--json-out" path rest ...)
       (loop rest
             jobs
             sequential?
             timeout
             strict?
             suite
             extra
             repeat
             record-gate?
             inventory?
             diagnose-overhead?
             mode
             path
             ledger)]
      [(list "--ledger" path rest ...)
       (loop rest
             jobs
             sequential?
             timeout
             strict?
             suite
             extra
             repeat
             record-gate?
             inventory?
             diagnose-overhead?
             mode
             json-out
             path)]
      [(list (regexp #rx"^--") rest ...)
       (eprintf "run-tests: unknown flag: ~a~n" (car rest))
       (usage)
       (exit 2)]
      [(list arg rest ...)
       (loop rest
             jobs
             sequential?
             timeout
             strict?
             suite
             (cons arg extra)
             repeat
             record-gate?
             inventory?
             diagnose-overhead?
             mode
             json-out
             ledger)])))

(define (validate-args! jobs
                        sequential?
                        timeout
                        strict?
                        suite
                        extra
                        repeat
                        record-gate?
                        inventory?
                        diagnose-overhead?
                        mode
                        json-out
                        ledger)
  (unless (memq suite known-suites)
    (raise-user-error 'run-tests
                      "unknown suite: ~a (valid: ~a)"
                      suite
                      (string-join (map symbol->string known-suites) ", ")))
  (when (or (not jobs) (not (integer? jobs)) (<= jobs 0))
    (raise-user-error 'run-tests "--jobs must be a positive integer, got: ~a" jobs))
  (when (or (not repeat) (not (integer? repeat)) (<= repeat 0))
    (raise-user-error 'run-tests "--repeat must be a positive integer, got: ~a" repeat))
  (when (and timeout (or (not (number? timeout)) (<= timeout 0)))
    (raise-user-error 'run-tests "--timeout must be a positive number, got: ~a" timeout))
  (unless (memq mode known-modes)
    (raise-user-error 'run-tests
                      "unknown mode: ~a (valid: ~a)"
                      mode
                      (string-join (map symbol->string known-modes) ", ")))
  (when (and json-out (not (string? json-out)))
    (raise-user-error 'run-tests "--json-out must be a path string, got: ~a" json-out))
  (when (and ledger (not (string? ledger)))
    (raise-user-error 'run-tests "--ledger must be a path string, got: ~a" ledger))
  (values jobs
          sequential?
          timeout
          strict?
          suite
          extra
          repeat
          record-gate?
          inventory?
          mode
          json-out
          ledger))
