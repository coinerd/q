#lang racket/base

;; run-tests.rkt - Parallel test runner. Thin facade (v0.99.43 W0c) re-exporting
;; the full public surface from run-tests/ sub-modules: classify.rkt (metadata,
;; classifiers, discovery), parse.rkt (output parsing, result struct),
;; reporting.rkt (summaries, failure logs), ledger.rkt, cli.rkt, profiles.rkt,
;; gate-evidence.rkt, inventory.rkt, and runner.rkt (execution orchestration,
;; including the overhead diagnostics/telemetry formerly in overhead.rkt).
;;
;; Phase D step 1: this file is also the command-line entry point, as a thin
;; COMPATIBILITY ADAPTER over the shared `./qt full` CI engine. Its argv is
;; translated 1:1 into a `qt-ci-spec` and resolved/executed through the same
;; `resolve-qt-ci-plan`/`run-qt-ci-plan` pair `./qt` uses, so CI and local
;; callers share ONE implementation of selection, execution and reporting.
;; The protected check name, the stdout summary (reporting.rkt's per-file
;; results, TEST SUMMARY block, RUN-SUMMARY record, ledger summary), the JSON
;; schema and the exit codes are unchanged; what stays here is the CLI contract
;; itself (cli.rkt parsing/validation) plus the compat presentation lines and
;; the view-only/impact modes that are not part of the CI surface.

;; run-tests.rkt - Parallel test runner. Thin facade (v0.99.43 W0c) re-exporting
;; the full public surface from run-tests/ sub-modules: classify.rkt (metadata,
;; classifiers, discovery, environment profile policy), reporting.rkt (result
;; parsing + classification, summaries, failure logs, known-failure ledger,
;; gate evidence), inventory.rkt, shard-plan.rkt and runner.rkt (execution
;; orchestration + the CLI grammar).
;;
;; Phase D step 1: this file is also the command-line entry point, as a thin
;; COMPATIBILITY ADAPTER over the shared `./qt full` CI engine. Its argv is
;; translated 1:1 into a `qt-ci-spec` and resolved/executed through the same
;; `resolve-qt-ci-plan`/`run-qt-ci-plan` pair `./qt` uses, so CI and local
;; callers share ONE implementation of selection, execution and reporting.
;; The protected check name, the stdout summary (reporting.rkt's per-file
;; results, TEST SUMMARY block, RUN-SUMMARY record, ledger summary), the JSON
;; schema and the exit codes are unchanged; what stays here is the CLI contract
;; itself (runner.rkt's parsing/validation) plus the compat presentation lines
;; and the view-only/impact modes that are not part of the CI surface.

(require racket/match
         racket/format
         racket/date
         racket/file
         racket/list
         json
         "run-tests/classify.rkt"
         "run-tests/reporting.rkt"
         "run-tests/inventory.rkt"
         "run-tests/shard-plan.rkt"
         "testing/changed-selection.rkt"
         "run-tests/runner.rkt"
         "testing/full-plan.rkt")

(provide test-file-result
         test-file-result?
         test-file-result-path
         test-file-result-exit-code
         test-file-result-stdout-bytes
         test-file-result-stderr-bytes
         test-file-result-elapsed-ms
         test-file-result-passed
         test-file-result-failed
         test-file-result-total
         make-test-file-result
         parse-raco-output
         normalize-counts
         classify-test-result
         test-result->jsexpr
         extract-failure-lines
         run-single-file
         run-all-files
         run-suite-once
         collect-test-files
         mutating-file?
         mutating-patterns
         platform-file?
         shard-files
         repo-surface-files
         print-summary
         save-failure-logs
         format-duration
         summary-exit-code
         compute-verdict
         write-json-results!
         print-ledger-summary
         load-known-failure-ledger
         summarize-ledger-results
         ledger-summary-counts
         ledger-entry-matches-result?
         ;; W8 quarantine expiry (ledger schema `expires_on`):
         ledger-entry-expired?
         today-iso8601
         valid-expires-on?
         ;; W8 definitive status reporting (pass/fail/timeout/skip kept
         ;; distinct in JSON + summary records):
         result-status
         known-profiles
         profile-unavailable-requirements
         profile-skips-test?
         skipped-requirements
         make-skipped-result
         skipped-result-exit-code
         bytes->string*
         clean-stale-bytecode!
         file-has-rackunit-tests?
         parse-args
         arch-file?
         runtime-file?
         extensions-file?
         workflows-file?
         unit-fast-file?
         slow-file?
         tui-file?
         validate-args!
         known-suites
         known-modes
         record-gate-evidence!
         get-file-metadata
         clear-metadata-cache!
         make-unique-log-name
         truncate-test-output
         print-inventory
         classify-exclusion-reason
         detect-high-risk-flags
         compute-inventory-hash
         ;; Phase D step 1: the compatibility adapter over the shared qt CI
         ;; engine. Published so tests can pin the argv translation table and
         ;; prove both front-ends resolve the SAME selection.
         (struct-out compat-args)
         parse-compat-argv
         compat-args->ci-spec
         compat-main)

;; ============================================================
;; Phase D step 1 — compatibility adapter
;; ============================================================
;; Everything below is the CLI contract of `racket scripts/run-tests.rkt`.
;; Nothing here selects, executes or reports tests: selection/sharding is
;; `resolve-qt-ci-plan`, execution + repeat + JSON + gate evidence is
;; `run-qt-ci-plan`, and the summary is printed by runner.rkt's
;; `run-suite-once` through reporting.rkt, i.e. the exact code `./qt full` runs.

;; Parsed compat CLI values, one field per documented flag. `shard-index`,
;; `shard-total` and `argv` are kept beside the cli.rkt values because cli.rkt
;; deliberately does not parse the shard flags (they are orthogonal to every
;; mode, including the view-only ones) and `argv` is gate-evidence provenance.
(struct compat-args
        (jobs sequential?
              timeout
              strict?
              suite
              files
              repeat
              record-gate?
              inventory?
              mode
              json-out
              ledger-path
              profile
              lint-metadata?
              changed-base
              changed-head
              explain?
              impact-dry-run?
              prioritize
              failure-history
              covers-manifest?
              shard-plan
              durations
              shard-index
              shard-total
              argv)
  #:transparent)

;; Same shard-flag extraction as the runner has always done: `--shard-index N`
;; and `--shard-total M` are consumed before the rest of the grammar and their
;; values are validated by `shard-files`/`validate-qt-ci-spec!`.
(define (compat-extract-shard-args argv)
  (define index (box 0))
  (define total (box 1))
  (define filtered
    (let loop ([rest argv])
      (match rest
        ['() '()]
        [(list (== "--shard-index") n more ...)
         (set-box! index (string->number n))
         (loop more)]
        [(list (== "--shard-total") n more ...)
         (set-box! total (string->number n))
         (loop more)]
        [(list elem more ...) (cons elem (loop more))])))
  (values (unbox index) (unbox total) filtered))

;; runner.rkt stays the compat CLI's parser and validator: its messages, its
;; `usage` output and its exit codes (`--help` 0, unknown flag 2, invalid
;; enumerated value 2) are the protected command-line contract.
(define (parse-compat-argv argv)
  (define-values (shard-index shard-total filtered) (compat-extract-shard-args argv))
  (define-values (jobs
                  sequential?
                  timeout
                  strict?
                  suite
                  files
                  repeat
                  record-gate?
                  inventory?
                  mode
                  json-out
                  ledger-path
                  profile
                  lint-metadata?
                  changed-base
                  changed-head
                  explain?
                  impact-dry-run?
                  prioritize
                  failure-history
                  covers-manifest?
                  shard-plan
                  durations)
    (parse-args filtered))
  (define-values (jobs*
                  sequential?*
                  timeout*
                  strict*
                  suite*
                  files*
                  repeat*
                  record-gate*
                  inventory*
                  mode*
                  json-out*
                  ledger*
                  profile*
                  lint*
                  changed-base*
                  changed-head*
                  explain*
                  impact-dry-run*
                  prioritize*
                  failure-history*
                  covers*
                  shard-plan*
                  durations*)
    (validate-args! jobs
                    sequential?
                    timeout
                    strict?
                    suite
                    files
                    repeat
                    record-gate?
                    inventory?
                    mode
                    json-out
                    ledger-path
                    profile
                    lint-metadata?
                    changed-base
                    changed-head
                    explain?
                    impact-dry-run?
                    prioritize
                    failure-history
                    covers-manifest?
                    shard-plan
                    durations))
  (compat-args jobs*
               sequential?*
               timeout*
               strict*
               suite*
               files*
               repeat*
               record-gate*
               inventory*
               mode*
               json-out*
               ledger*
               profile*
               lint*
               changed-base*
               changed-head*
               explain*
               impact-dry-run*
               prioritize*
               failure-history*
               covers*
               shard-plan*
               durations*
               shard-index
               shard-total
               argv))

;; The 1:1 argument translation table (`./qt full` spells the same values as
;; named CI flags; the compat CLI has positional files and a few legacy flags):
;;
;;   --suite NAME            -> spec suite      (dropped when an explicit file
;;                                               list replaces the selection;
;;                                               the report label is carried by
;;                                               `qt-ci-compat-options`, exactly
;;                                               like the runner's suite-label)
;;   positional TEST-FILES   -> spec files
;;   --shard-index N         -> spec shard-index
;;   --shard-total M         -> spec shard-total
;;   --timeout SECS          -> spec timeout
;;   --profile NAME          -> spec profile
;;   --ledger PATH           -> spec ledger-path
;;   --json-out PATH         -> spec json-out
;;   --record-gate-evidence  -> spec record-gate?
;;   --repeat N              -> spec repeat
;;   --mode NAME             -> spec mode
;;   --shard-plan active     -> spec shard-plan
;;   --durations PATH        -> spec durations
;;   argv                    -> spec argv (gate-evidence provenance)
;;   --jobs N / --sequential -> execution width (not a spec field; passed to the
;;                              engine exactly like `./qt full --jobs`)
;;   --strict                -> `qt-ci-compat-options` strict? (the spec has no
;;                              strict field: qt derives it from
;;                              STRICT_TEST_RUNNER)
;;
;; `files` is an explicit override for the impact modes, whose selection is not
;; part of the CI surface (see `compat-main`).
(define (compat-args->ci-spec a [files (compat-args-files a)])
  (qt-ci-spec (and (null? files) (compat-args-suite a))
              files
              (compat-args-shard-index a)
              (compat-args-shard-total a)
              (compat-args-timeout a)
              (compat-args-profile a)
              (compat-args-ledger-path a)
              (compat-args-json-out a)
              (compat-args-record-gate? a)
              (compat-args-repeat a)
              (compat-args-mode a)
              (compat-args-shard-plan a)
              (compat-args-durations a)
              (compat-args-argv a)))

;; Read q-version from util/version.rkt without loading the module.
(define (compat-runner-version)
  (with-handlers ([exn:fail? (lambda (_) "unknown")])
    (define m
      (regexp-match #rx"q-version[ \t]+\"([^\"]+)\""
                    (file->string (build-path base-dir "util" "version.rkt"))))
    (if m
        (cadr m)
        "unknown")))

;; The profile-aware duration plan callback, spelled exactly like the runner's.
(define (compat-profile-skips? profile)
  (lambda (f) (profile-skips-test? profile (hash-ref (get-file-metadata f) 'requires '()))))

;; The compat presentation of an `--prioritize impact` run: the first failing
;; selected test, with its selection and priority reason (runner behaviour).
(define (compat-print-first-failure results prioritize-payload)
  (when (pair? results)
    (define first-fail
      (for/or ([r (in-list results)])
        (and (not (zero? (test-file-result-exit-code r))) r)))
    (when first-fail
      (define f (test-file-result-path first-fail))
      (printf "~n;; ── first failing test (prioritized order) ──~n")
      (for ([e (in-list (append (hash-ref prioritize-payload 'serial '())
                                (hash-ref prioritize-payload 'parallel '())))]
            #:when (equal? (hash-ref e 'file) f))
        (printf ";;   file: ~a~n;;   tier: ~a  priority: ~a  selected-because: ~a~n"
                f
                (hash-ref e 'tier)
                (hash-ref e 'priority-reason)
                (hash-ref e 'selection-reason-code))))))

;; ============================================================
;; Entry point
;; ============================================================
;; The whole compat run happens with the qt presentation switched OFF: no
;; `;; qt:` progress lines and no `extra.qt` marker, so this command's stdout and
;; JSON stay exactly the runner's own.
(define (compat-main argv)
  (parameterize ([qt-ci-presentation #f])
    (compat-main/presentation argv)))

(define (compat-main/presentation argv)
  (define a (parse-compat-argv argv))
  (define files (compat-args-files a))
  (define suite (compat-args-suite a))
  (define shard-index (compat-args-shard-index a))
  (define shard-total (compat-args-shard-total a))
  (define repeat (compat-args-repeat a))
  (define profile (compat-args-profile a))
  ;; v1.00.24 W3: a shard never records gate evidence (it would claim a
  ;; full-suite PASS). Refuse before any work; the engine re-checks.
  (when (compat-args-record-gate? a)
    (define refusal (gate-evidence-refusal shard-index shard-total))
    (when refusal
      (eprintf ";; run-tests: ~a~n" refusal)
      (exit 2)))
  ;; ── view-only modes (never execute a test) ───────────────────
  (when (compat-args-lint-metadata? a)
    (define summary
      (print-lint-report (if (pair? files)
                             (map normalize-test-path files)
                             (collect-test-files suite))))
    (exit (if (> (hash-ref summary 'invalid_count 0) 0) 1 0)))
  (when (compat-args-covers-manifest? a)
    (define written (write-covers-manifest! base-dir (compat-runner-version)))
    (define entries (hash-ref written 'entries '()))
    (printf ";; run-tests: @covers manifest written → tests/.coverage-manifest.json~n")
    (printf ";; run-tests: entries=~a manual-review=~a runner-version=~a~n"
            (length entries)
            (count (lambda (e) (equal? (hash-ref e 'source #f) "manual-review")) entries)
            (hash-ref written 'runner_version))
    (exit 0))
  ;; ── change-impact selection (not part of the CI surface) ────
  ;; With --changed-base the impact selection REPLACES the static suite list
  ;; (escalations run their declared fallback suites); explicit files stay
  ;; honored. The resulting file list is then executed by the SAME engine.
  (define impact-files #f)
  (define impact-selection #f)
  (define impact-changed '())
  (define prioritize-payload #f)
  (define changed-base (compat-args-changed-base a))
  (when changed-base
    (define-values (sel-files sel changed)
      (run-impact-selection! changed-base
                             (or (compat-args-changed-head a) "HEAD")
                             #:root base-dir
                             #:collect collect-test-files))
    (set! impact-selection sel)
    (set! impact-changed changed)
    (when (compat-args-explain? a)
      (print-impact-explain sel #:base changed-base #:head (or (compat-args-changed-head a) "HEAD"))
      (when (compat-args-json-out a)
        (call-with-output-file (compat-args-json-out a)
                               #:exists 'truncate/replace
                               (lambda (out)
                                 (write-json (hasheq 'explain #t 'selection (selection->jsexpr sel))
                                             out)
                                 (newline out))))
      (exit 0))
    (when (hash-ref sel 'doc-only?)
      (printf ";; run-tests: doc-only change — zero-source-change no-op~n")
      (when (compat-args-json-out a)
        (call-with-output-file (compat-args-json-out a)
                               #:exists 'truncate/replace
                               (lambda (out)
                                 (write-json (hasheq 'doc_only #t 'selection (selection->jsexpr sel))
                                             out)
                                 (newline out))))
      (exit 0))
    (define explicit (map normalize-test-path files))
    (define merged (remove-duplicates (append explicit sel-files)))
    (unless (pair? merged)
      (printf ";; run-tests: ERROR impact selection is empty — refusing to run zero tests~n")
      (when (compat-args-json-out a)
        (call-with-output-file
         (compat-args-json-out a)
         #:exists 'truncate/replace
         (lambda (out)
           (write-json (hasheq 'error 'empty-selection 'selection (selection->jsexpr sel)) out)
           (newline out))))
      (exit 3))
    (when (equal? (compat-args-prioritize a) "impact")
      (define-values (hist-weights hist-status)
        (load-failure-history (compat-args-failure-history a)))
      (printf ";; run-tests: prioritize=impact history=~a (neutral path order when absent/corrupt)~n"
              hist-status)
      (define boundaries
        (for/hash ([f (in-list merged)]
                   #:when (hash-ref (get-file-metadata f) 'boundary #f))
          (values f (hash-ref (get-file-metadata f) 'boundary))))
      (define ctx (make-prioritize-ctx explicit (hash-ref sel 'selected '()) hist-weights boundaries))
      (define serial-part (filter mutating-file? merged))
      (define parallel-part (filter (lambda (f) (not (mutating-file? f))) merged))
      (define-values (s-files s-entries) (prioritize-partition serial-part ctx))
      (define-values (p-files p-entries) (prioritize-partition parallel-part ctx))
      (set! impact-files (append s-files p-files))
      (set! prioritize-payload (partition-entries->jsexpr s-entries p-entries hist-status)))
    (unless impact-files
      (set! impact-files merged))
    (when (compat-args-impact-dry-run? a)
      (printf ";; run-tests: impact dry run — executing nothing~n")
      (define plan
        (hasheq 'plan
                impact-files
                'selection
                (selection->jsexpr sel)
                'prioritization
                (or prioritize-payload (hasheq 'prioritized #f))))
      (if (compat-args-json-out a)
          (call-with-output-file (compat-args-json-out a)
                                 #:exists 'truncate/replace
                                 (lambda (out)
                                   (write-json plan out)
                                   (newline out)))
          (begin
            (write-json plan)
            (newline)))
      (exit 0)))
  ;; ── shared selection ────────────────────────────────────────
  (define cleaned-dirs (clean-stale-bytecode! (current-directory)))
  (when (> cleaned-dirs 0)
    (printf ";; run-tests: cleaned ~a stale compiled/ director~a~n"
            cleaned-dirs
            (if (= cleaned-dirs 1) "y" "ies")))
  (define spec (compat-args->ci-spec a (or impact-files files)))
  (define plan (resolve-qt-ci-plan #f spec #:contract 'compat))
  (define rels (qt-ci-plan-rels plan))
  (define suite-label
    (cond
      [impact-files "impact"]
      [else (symbol->string suite)]))
  ;; ── plan-only mode (`--shard-plan active`) ─────────────────
  (define plan-mode (compat-args-shard-plan a))
  (when (and (equal? plan-mode "active") (not (> shard-total 1)))
    (printf ";; run-tests: --shard-plan ~a ignored — --shard-total=~a (unsharded)~n"
            plan-mode
            shard-total))
  (when (and (equal? plan-mode "active") (> shard-total 1))
    (define-values (dur dur-status) (load-duration-snapshot (compat-args-durations a)))
    (printf ";; run-tests: shard-plan=~a durations=~a status=~a known=~a~n"
            plan-mode
            (or (compat-args-durations a) "<none>")
            dur-status
            (hash-count dur))
    (unless (eq? dur-status 'ok)
      (printf ";; run-tests: unmeasured files use the conservative p95 default duration~n"))
    (define duration-plan
      (build-shard-plan/safe rels
                             shard-total
                             #:durations dur
                             #:profile-skips? (compat-profile-skips? profile)
                             #:duration-source (compat-args-durations a)))
    (printf ";; run-tests: duration-aware plan (~a) replaces round-robin for shard ~a/~a~n"
            (shard-plan-mode duration-plan)
            shard-index
            shard-total))
  (define run-files (qt-ci-run-files plan))
  (unless (equal? plan-mode "active")
    (when (> shard-total 1)
      (printf ";; run-tests: sharding ~a files — shard ~a/~a~n"
              (length rels)
              shard-index
              shard-total)))
  (when (compat-args-inventory? a)
    (if (pair? run-files)
        (begin
          (print-inventory suite run-files)
          (exit 0))
        (begin
          (displayln "No test files matched the selected suite.")
          (exit 1))))
  (unless (pair? run-files)
    (displayln "No test files matched the selected suite.")
    (exit 1))
  ;; ── execution (the shared engine) ───────────────────────────
  (define phases (make-hasheq))
  (hash-set! phases 'selection_end_ms (current-inexact-milliseconds))
  (hash-set! phases 'execution_start_ms (current-inexact-milliseconds))
  (printf ";; run-tests: suite=~a files=~a jobs=~a sequential=~a repeat=~a mode=~a profile=~a~n"
          suite-label
          (length run-files)
          (compat-args-jobs a)
          (compat-args-sequential? a)
          repeat
          (qt-effective-mode (compat-args-mode a) suite-label)
          profile)
  (newline)
  (when (> repeat 1)
    (printf ";; run-tests: running suite ~a time~a for confidence gate~n"
            repeat
            (if (= repeat 1) "" "s")))
  (define options
    (qt-ci-compat-options suite-label
                          (compat-args-strict? a)
                          phases
                          (lambda (run-num repeat-total)
                            (when (> repeat-total 1)
                              (printf "~n;; ── Run ~a/~a ──~n" run-num repeat-total)))))
  (define outcome (run-qt-ci-plan plan (compat-args-jobs a) #:options options))
  (define results (qt-ci-outcome-results outcome))
  ;; W5/W6 post-run evidence: embed the impact selection in the results JSON
  ;; and surface the first failing selected test on prioritized runs.
  (when (and changed-base (compat-args-json-out a) impact-selection)
    (embed-impact-in-results! (compat-args-json-out a)
                              impact-selection
                              prioritize-payload
                              impact-changed))
  (when prioritize-payload
    (compat-print-first-failure results prioritize-payload))
  (exit (qt-ci-outcome-exit-code outcome)))

(module+ main
  (compat-main (vector->list (current-command-line-arguments))))
