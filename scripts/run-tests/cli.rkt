#lang racket/base

;; q/scripts/run-tests/cli.rkt — CLI argument parsing
;;
;; Usage, parse-args, validate-args!, known-suites.
;; Extracted from run-tests.rkt (v0.96.16, AX1-2).
;; STABILITY: internal

(require racket/match
         racket/string
         (only-in racket/future processor-count)
         (only-in "profiles.rkt" known-profiles))

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
  (displayln "  --profile NAME          Environment profile: local, vps, ci, headless, full")
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
             [ledger #f]
             [profile 'local])
    (define (continue rest
                      #:jobs [jobs* jobs]
                      #:sequential? [sequential?* sequential?]
                      #:timeout [timeout* timeout]
                      #:strict? [strict?* strict?]
                      #:suite [suite* suite]
                      #:extra [extra* extra]
                      #:repeat [repeat* repeat]
                      #:record-gate? [record-gate?* record-gate?]
                      #:inventory? [inventory?* inventory?]
                      #:diagnose-overhead? [diagnose-overhead?* diagnose-overhead?]
                      #:mode [mode* mode]
                      #:json-out [json-out* json-out]
                      #:ledger [ledger* ledger]
                      #:profile [profile* profile])
      (loop rest
            jobs*
            sequential?*
            timeout*
            strict?*
            suite*
            extra*
            repeat*
            record-gate?*
            inventory?*
            diagnose-overhead?*
            mode*
            json-out*
            ledger*
            profile*))
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
               ledger
               profile)]
      [(list "--help" _ ...)
       (usage)
       (exit 0)]
      [(list "--strict" rest ...) (continue rest #:strict? #t)]
      [(list "--jobs" n rest ...) (continue rest #:jobs (string->number n))]
      [(list "--sequential" rest ...) (continue rest #:jobs 1 #:sequential? #t)]
      [(list "--timeout" secs rest ...) (continue rest #:timeout (string->number secs))]
      [(list "--mode" name rest ...) (continue rest #:mode (string->symbol name))]
      [(list "--suite" name rest ...) (continue rest #:suite (string->symbol name))]
      [(list "--repeat" n rest ...) (continue rest #:repeat (string->number n))]
      [(list "--record-gate-evidence" rest ...) (continue rest #:record-gate? #t)]
      [(list "--inventory" rest ...) (continue rest #:inventory? #t)]
      [(list "--diagnose-overhead" rest ...) (continue rest #:diagnose-overhead? #t)]
      [(list "--json-out" path rest ...) (continue rest #:json-out path)]
      [(list "--ledger" path rest ...) (continue rest #:ledger path)]
      [(list "--profile" name rest ...) (continue rest #:profile (string->symbol name))]
      [(list flag rest ...)
       #:when (regexp-match? #rx"^--" flag)
       (eprintf "run-tests: unknown flag: ~a~n" flag)
       (usage)
       (exit 2)]
      [(list arg rest ...) (continue rest #:extra (cons arg extra))])))

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
                        ledger
                        profile)
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
  (unless (memq profile known-profiles)
    (raise-user-error 'run-tests
                      "unknown profile: ~a (valid: ~a)"
                      profile
                      (string-join (map symbol->string known-profiles) ", ")))
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
          ledger
          profile))
