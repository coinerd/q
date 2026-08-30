#lang racket/base

;; q/scripts/run-tests/cli.rkt — CLI argument parsing
;;
;; Usage, parse-args, validate-args!, known-suites.
;; Extracted from run-tests.rkt (v0.96.16, AX1-2).
;; STABILITY: internal

(require racket/match
         racket/string
         (only-in racket/future processor-count)
         (only-in "profiles.rkt" known-profiles)
         (only-in "scheduler-order.rkt"
                  known-orderings
                  default-ordering))

(provide usage
         parse-args
         validate-args!
         known-suites
         known-modes
         known-schedulers
         known-orderings
         default-ordering)

(define (usage)
  (displayln "Usage: racket scripts/run-tests.rkt [OPTIONS] [TEST-FILES ...]")
  (newline)
  (displayln "Options:")
  (displayln "  --jobs N          Number of parallel jobs (default: processor-count)")
  (displayln "  --sequential      Run tests sequentially (jobs=1)")
  (displayln "  --timeout SECS    Per-file timeout in seconds")
  (displayln "  --mode <name>     Execution mode: auto (default), subprocess, in-process, grouped")
  (displayln "  --scheduler <name>  Scheduler: batch (default, fixed-batch barrier) or")
  (displayln "                    queue (bounded work-conserving worker pool)")
  (displayln "  --ordering <name>  File ordering: fifo (default, deterministic input order)")
  (displayln "                    or lpt (longest-processing-time-first using --durations)")
  (displayln "                    evidence; falls back to fifo with a named reason when")
  (displayln "                    duration evidence is missing/stale/malformed/wrong-inventory)")
  (displayln "  --suite <name>    Run test suite: all/broad (default all), fast,")
  (displayln "                    unit-fast, slow, tui, smoke, release-smoke,")
  (displayln "                    security, arch, runtime, extensions, workflows, platform")
  (displayln "  --strict          Enable strict zero-test detection (default: on)")
  (displayln "  --repeat N        Run suite N times (exit 1 if any run fails)")
  (displayln "  --record-gate-evidence  Write .gate-evidence/<suite>.passed on success")
  (displayln "  --inventory             Print inventory report (selected/excluded files) and exit")
  (displayln "  --diagnose-overhead     Measure Racket/raco per-file startup overhead and exit")
  (displayln
   "  --lint-metadata         Lint test metadata against schema v1 (enforced: invalid tags fail; missing tags warn) and exit")
  (displayln "  --json-out PATH         Write structured per-file JSON results")
  (displayln
   "  --ledger PATH           Read known-failure ledger JSON and report known/new/resolved failures")
  (displayln "  --changed-base REF     Change-impact selection base ref (git merge-base style)")
  (displayln "  --changed-head REF     Change-impact head ref (default: HEAD)")
  (displayln
   "  --explain              Print the reasoned impact selection (requires --changed-base) and exit")
  (displayln "  --impact-dry-run       Print impact selection JSON, execute no tests, exit 0")
  (displayln
   "  --prioritize impact    Deterministic prioritization of the selected set (never changes selection)")
  (displayln
   "  --failure-history PATH Retained CI JSON artifact(s) used by --prioritize impact (decay-weighted)")
  (displayln
   "  --generate-covers-manifest  Regenerate tests/.coverage-manifest.json from @covers metadata and exit")
  (displayln "  --profile NAME          Environment profile: local, vps, ci, headless, full")
  (displayln "  --shard-index N         Select shard N of M (for parallel CI sharding)")
  (displayln "  --shard-total M         Total number of shards (default: 1, no sharding)")
  (displayln "  --shard-plan <mode>     Duration-aware shard planning: report (print plan +")
  (displayln "                          predicted durations, change nothing, exit 0) or")
  (displayln "                          active (consume the plan instead of round-robin)")
  (displayln "  --durations PATH        Duration snapshot for --shard-plan: a retained CI JSON")
  (displayln "                          artifact or a directory of *.json artifacts (W0 schema)")
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
  (displayln
   "  release-smoke  Post-release artifact verification (deterministic, no browser/network)")
  (displayln "  security  All security/permission/sandbox/safe-mode tests")
  (displayln "  arch    Architecture boundary/fitness tests")
  (displayln "  runtime Runtime/session/compaction/iteration tests")
  (displayln "  extensions Extension/GSD/hook tests")
  (displayln "  workflows All tests/workflows/ including fixture self-tests (integration-level)")
  (displayln "  platform Curated platform-cross subset for macOS verification"))

(define known-suites
  '(all broad
        fast
        unit-fast
        slow
        smoke
        release-smoke
        tui
        security
        arch
        runtime
        extensions
        workflows
        platform))
(define known-modes '(auto subprocess in-process grouped))
(define known-schedulers '(batch queue))

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
             [scheduler 'batch]
             [json-out #f]
             [ledger #f]
             [profile 'local]
             [lint-metadata? #f]
             [changed-base #f]
             [changed-head "HEAD"]
             [explain? #f]
             [impact-dry-run? #f]
             [prioritize #f]
             [failure-history #f]
             [generate-covers-manifest? #f]
             [shard-plan #f]
             [durations #f]
             [ordering #f])
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
                      #:scheduler [scheduler* scheduler]
                      #:json-out [json-out* json-out]
                      #:ledger [ledger* ledger]
                      #:profile [profile* profile]
                      #:lint-metadata? [lint-metadata?* lint-metadata?]
                      #:changed-base [changed-base* changed-base]
                      #:changed-head [changed-head* changed-head]
                      #:explain? [explain?* explain?]
                      #:impact-dry-run? [impact-dry-run?* impact-dry-run?]
                      #:prioritize [prioritize* prioritize]
                      #:failure-history [failure-history* failure-history]
                      #:generate-covers-manifest?
                      [generate-covers-manifest?* generate-covers-manifest?]
                      #:shard-plan [shard-plan* shard-plan]
                      #:durations [durations* durations]
                      #:ordering [ordering* ordering])
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
            scheduler*
            json-out*
            ledger*
            profile*
            lint-metadata?*
            changed-base*
            changed-head*
            explain?*
            impact-dry-run?*
            prioritize*
            failure-history*
            generate-covers-manifest?*
            shard-plan*
            durations*
            ordering*))
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
               scheduler
               json-out
               ledger
               profile
               lint-metadata?
               changed-base
               changed-head
               explain?
               impact-dry-run?
               prioritize
               failure-history
               generate-covers-manifest?
               shard-plan
               durations
               ordering)]
      [(list "--help" _ ...)
       (usage)
       (exit 0)]
      [(list "--strict" rest ...) (continue rest #:strict? #t)]
      [(list "--jobs" n rest ...) (continue rest #:jobs (string->number n))]
      [(list "--sequential" rest ...) (continue rest #:jobs 1 #:sequential? #t)]
      [(list "--timeout" secs rest ...) (continue rest #:timeout (string->number secs))]
      [(list "--mode" name rest ...) (continue rest #:mode (string->symbol name))]
      [(list "--scheduler" name rest ...)
       (define sch (string->symbol name))
       (unless (memq sch known-schedulers)
         (eprintf "run-tests: invalid --scheduler value ~s (valid: batch, queue)~n" name)
         (usage)
         (exit 2))
       (continue rest #:scheduler sch)]
      [(list "--suite" name rest ...) (continue rest #:suite (string->symbol name))]
      [(list "--repeat" n rest ...) (continue rest #:repeat (string->number n))]
      [(list "--record-gate-evidence" rest ...) (continue rest #:record-gate? #t)]
      [(list "--inventory" rest ...) (continue rest #:inventory? #t)]
      [(list "--diagnose-overhead" rest ...) (continue rest #:diagnose-overhead? #t)]
      [(list "--lint-metadata" rest ...) (continue rest #:lint-metadata? #t)]
      [(list "--json-out" path rest ...) (continue rest #:json-out path)]
      [(list "--ledger" path rest ...) (continue rest #:ledger path)]
      [(list "--profile" name rest ...) (continue rest #:profile (string->symbol name))]
      [(list "--changed-base" ref rest ...) (continue rest #:changed-base ref)]
      [(list "--changed-head" ref rest ...) (continue rest #:changed-head ref)]
      [(list "--explain" rest ...) (continue rest #:explain? #t)]
      [(list "--impact-dry-run" rest ...) (continue rest #:impact-dry-run? #t)]
      [(list "--prioritize" name rest ...) (continue rest #:prioritize name)]
      [(list "--failure-history" path rest ...) (continue rest #:failure-history path)]
      [(list "--generate-covers-manifest" rest ...) (continue rest #:generate-covers-manifest? #t)]
      [(list "--shard-plan" mode* rest ...) (continue rest #:shard-plan mode*)]
      [(list "--durations" path rest ...) (continue rest #:durations path)]
      [(list "--ordering" name rest ...)
       (define ord (string->symbol name))
       (unless (memq ord known-orderings)
         (eprintf "run-tests: invalid --ordering value ~s (valid: ~a)~n"
                  name
                  (string-join (map symbol->string known-orderings) ", "))
         (usage)
         (exit 2))
       (continue rest #:ordering ord)]
      [(list "--shard-plan" rest ...)
       (eprintf "run-tests: --shard-plan requires a mode (report|active)~n")
       (usage)
       (exit 2)]
      [(list "--durations" rest ...)
       (eprintf "run-tests: --durations requires a path~n")
       (usage)
       (exit 2)]
      [(list "--ordering" rest ...)
       (eprintf "run-tests: --ordering requires a mode (fifo|lpt)~n")
       (usage)
       (exit 2)]
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
                        scheduler
                        json-out
                        ledger
                        profile
                        lint-metadata?
                        changed-base
                        changed-head
                        explain?
                        impact-dry-run?
                        prioritize
                        failure-history
                        generate-covers-manifest?
                        shard-plan
                        durations
                        ordering)
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
  (unless (memq scheduler known-schedulers)
    (raise-user-error 'run-tests
                      "unknown scheduler: ~a (valid: ~a)"
                      scheduler
                      (string-join (map symbol->string known-schedulers) ", ")))
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
  (when (and (or explain? impact-dry-run?) (not changed-base))
    (raise-user-error 'run-tests "--explain/--impact-dry-run require --changed-base <ref>"))
  (when (and prioritize (not (equal? prioritize "impact")))
    (raise-user-error 'run-tests "unknown --prioritize mode: ~a (valid: impact)" prioritize))
  (when (and prioritize (not changed-base))
    (raise-user-error 'run-tests "--prioritize requires --changed-base"))
  (when (and failure-history (not prioritize))
    (raise-user-error 'run-tests "--failure-history requires --prioritize impact"))
  (when (and changed-base (string? changed-base) (equal? changed-base ""))
    (raise-user-error 'run-tests "--changed-base must be non-empty"))
  (when (and changed-head (string? changed-head) (equal? changed-head ""))
    (raise-user-error 'run-tests "--changed-head must be non-empty"))
  (when (and shard-plan (not (member shard-plan '("report" "active"))))
    (raise-user-error 'run-tests "unknown --shard-plan mode: ~a (valid: report, active)" shard-plan))
  (when (and durations (not (string? durations)))
    (raise-user-error 'run-tests "--durations must be a path string, got: ~a" durations))
  (when (and ordering (not (memq ordering known-orderings)))
    (raise-user-error 'run-tests
                      "unknown ordering: ~a (valid: ~a)"
                      ordering
                      (string-join (map symbol->string known-orderings) ", ")))
  ;; NOTE: `--ordering lpt` WITHOUT --durations is legal.  Missing duration
  ;; evidence is a named fallback (missing-snapshot → FIFO) at ordering
  ;; preparation time, never a hard CLI error — "fail safely to
  ;; deterministic FIFO" is part of the ordering contract (v1.00.23 W3).
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
          scheduler
          json-out
          ledger
          profile
          lint-metadata?
          changed-base
          changed-head
          explain?
          impact-dry-run?
          prioritize
          failure-history
          generate-covers-manifest?
          shard-plan
          durations
          ordering))
