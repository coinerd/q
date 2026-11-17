#lang racket/base

;; run-tests/runner.rkt — Test execution orchestration for run-tests.rkt
;;
;; Owns the runner machinery: subprocess execution of individual test files,
;; the shared work-conserving scheduler cutover (scripts/testing/scheduler.rkt,
;; final plan §2 — the fixed-batch barrier `run-all-files/batch` is deleted),
;; suite-level orchestration (run-single-file, run-all-files, run-suite-once),
;; the command-line entry point (main) with its direct-invocation guard, and
;; the CLI grammar (parse-args / validate-args! / usage, folded in from the
;; former cli.rkt). Extracted from run-tests.rkt (v0.99.43 W0c).
;; STABILITY: internal — the public surface is re-exported by run-tests.rkt.

(require racket/string
         racket/match
         racket/math
         racket/path
         racket/file
         racket/system
         racket/port
         racket/list
         racket/future
         racket/exn
         json
         (only-in racket/format ~r)
         (only-in racket/date date->string current-date date-display-format)
         (only-in "../testing/scheduler.rkt" sched-task run-scheduled)
         (only-in "classify.rkt"
                  base-dir
                  known-profiles
                  normalize-test-path
                  collect-test-files
                  get-file-metadata
                  mutating-file?
                  declared-mutation?
                  repository-exclusive-file?
                  co-runs-under-exclusive-file?
                  repo-surface-files
                  clean-stale-bytecode!
                  file-has-rackunit-tests?
                  shard-files
                  print-lint-report)
         (only-in "reporting.rkt"
                  test-file-result
                  test-file-result?
                  test-file-result-path
                  test-file-result-exit-code
                  test-file-result-total
                  test-file-result-elapsed-ms
                  parse-raco-output
                  normalize-counts
                  effective-exit-code
                  classify-test-result
                  print-summary
                  save-failure-logs
                  summary-exit-code
                  write-json-results!
                  result-status
                  print-ledger-summary
                  print-run-summary-record
                  load-known-failure-ledger
                  record-gate-evidence!
                  gate-evidence-refusal)
         (only-in "classify.rkt" profile-skips-test?)
         (only-in "reporting.rkt" make-skipped-result skipped-result-exit-code)
         (only-in "shard-plan.rkt"
                  build-shard-plan/safe
                  plan-shard-files
                  load-duration-snapshot
                  shard-plan-mode)
         (only-in "inventory.rkt" print-inventory compute-inventory-hash)
         (only-in "../testing/changed-selection.rkt"
                  run-impact-selection!
                  print-impact-explain
                  selection->jsexpr
                  write-covers-manifest!
                  load-failure-history
                  make-prioritize-ctx
                  prioritize-partition
                  partition-entries->jsexpr
                  embed-impact-in-results!)
         (only-in "../testing/runner-cli.rkt"
                  usage
                  known-suites
                  known-modes
                  parse-args
                  validate-args!))

(provide run-single-file
         run-all-files
         run-suite-once
         prepared-environment-state
         parse-args
         validate-args!
         usage
         known-suites
         known-modes
         ;; Compiled-root child argv helpers (folded from compiled-root-child.rkt;
         ;; published so the fail-closed contract stays directly testable).
         compiled-root-child-prefix
         compiled-root-raco-child-args
         compiled-root-bare-child-args
         main)

;; ── W0 fast-gate budget instrumentation (schema-additive) ─────────────
;; Wall clock at module load: the retained per-job JSON can then attribute
;; time inside the fast gate (runner boot → selection → compile/require
;; warmup → execution → summary) instead of one undifferentiated number.
(define runner-start-ms (current-inexact-milliseconds))

;; ----------------------------------------------------------------------
;; Compiled-root child inheritance (folded verbatim from the former
;; compiled-root-child.rkt, v1.00.32 W2 repair — trial c01 window-2, run
;; 41271542715).
;;
;; The launcher (ci/prepared-environment/compiled-root.rkt `launch`)
;; exports Q_COMPILED_ROOT_FLAGS_FILE (plus the telemetry directory the
;; bootstrap reads) into the shard child. These functions re-validate those
;; flags with the SAME shape rule the launcher applied and build the argv
;; that injects the identical bootstrap into the runner's per-file racket
;; children:
;;
;;   rackunit files:
;;     racket <bootstrap…> -N raco -l- raco/main.rkt test -t <file>
;;     (the raco launcher binary is literally
;;      `exec racket -N raco -l- raco/main.rkt "$@"`; probe-verified
;;      byte-identical discovery output and exit codes)
;;
;;   other files:
;;     racket <bootstrap…> -N <file> -t- <file>
;;     (the unified bare-child entry, W1a round 5 / trial
;;     trial-2026-11-01-smoke-3 run 41303344486: the same mechanism as
;;     the proven raco dispatch `racket -N raco -l- raco/main.rkt …`,
;;     here in the file-path grammar because -l rejects absolute file
;;     paths. The former -e expression entry left find-system-path
;;     'run-file at the interpreter and could never route a command
;;     line; -t- loads the file as a module AND as the run-file — plain
;;     `racket <file>` semantics: module body, then the 'main submodule
;;     when declared, empty child argv)
;;
;; Without those test files' bootstrap the children inherit neither
;; current-compiled-file-roots nor the TOCTOU guard, so no test load could
;; ever be root-served and the launcher record could never report a real
;; hit (zero-root-hits).
;;
;; Fail-closed: when the env var is absent, the file unreadable, or the
;; flags fail validation, the prefix is #f and every invocation stays
;; byte-identical to the legacy path (the launcher has already selected
;; the one eager fallback in that situation). A malformed flags file is
;; reported loudly on stderr, never silently ignored.

;; The validated flags payload: (list 'compiled-root-flags root map
;; checkout (list launch-args…)) — same shape the launcher validated.
(define (compiled-root-valid-flags? v)
  (and (list? v)
       (= 5 (length v))
       (eq? (list-ref v 0) 'compiled-root-flags)
       (string? (list-ref v 1))
       (string? (list-ref v 2))
       (string? (list-ref v 3))
       (and (list? (list-ref v 4)) (andmap string? (list-ref v 4)))))

;; Cache keyed on the flags-file path: the runner spawns one child per
;; test file, so getenv+read must not repeat per child. The path key makes
;; the cache transparently recompute when the environment changes (tests).
(define compiled-root-prefix-cache (cons #f #f)) ; (cons key prefix|#f)

(define (compiled-root-compute-prefix)
  (define ff (getenv "Q_COMPILED_ROOT_FLAGS_FILE"))
  (cond
    [(not (and ff (non-empty-string? ff))) #f]
    [else
     (define flags
       (with-handlers ([exn:fail? (lambda (e)
                                    (eprintf "run-tests: compiled-root flags unreadable (~a): ~a\n"
                                             ff
                                             (exn-message e))
                                    #f)])
         (file->value (path->complete-path (string->path ff)))))
     (cond
       [(compiled-root-valid-flags? flags) (list-ref flags 4)]
       [else
        (eprintf "run-tests: compiled-root flags malformed (~a); running children unmapped\n" ff)
        #f])]))

;; compiled-root-child-prefix
;;   The bootstrap argv prefix (a list of racket command-line strings) for
;;   the runner's racket children, or #f when root-on child inheritance is
;;   not active.
(define (compiled-root-child-prefix)
  (define ff (getenv "Q_COMPILED_ROOT_FLAGS_FILE"))
  (define key (and ff (string? ff) ff))
  (if (equal? key (car compiled-root-prefix-cache))
      (cdr compiled-root-prefix-cache)
      (let ([p (compiled-root-compute-prefix)])
        (set! compiled-root-prefix-cache (cons key p))
        p)))

;; compiled-root-raco-child-args
;;   Full argv (after the racket binary) for a rackunit test file child
;;   under root-on, or #f to keep the legacy `raco test -t` invocation.
(define (compiled-root-raco-child-args resolved-path)
  (define prefix (compiled-root-child-prefix))
  (and prefix
       (append prefix
               (list "-N" "raco" "-l-" "raco/main.rkt" "test" "-t" (path->string resolved-path)))))

;; compiled-root-bare-child-args
;;   Full argv (after the racket binary) for a non-rackunit test file
;;   child under root-on, or #f to keep the legacy `racket <file>`
;;   invocation. The -N/-t- entry preserves `racket <file>` semantics
;;   exactly (body + declared 'main submodule, empty child argv) AND
;;   keeps the file as the run-file (the unified entry, see the
;;   launcher's compiled-root-script-entry-argument, W1a round 5).
(define (compiled-root-bare-child-args resolved-path)
  (define prefix (compiled-root-child-prefix))
  (and prefix (let ([p (path->string resolved-path)]) (append prefix (list "-N" p "-t-" p)))))

;; ----------------------------------------------------------------------
;; Body extracted verbatim from run-tests.rkt (v0.99.43 W0c).
;; ----------------------------------------------------------------------

(define (build-result-from-process test-path stdout-out stderr-out ctrl timeout elapsed)
  (cond
    [timeout
     (define wait-thread (thread (lambda () (ctrl 'wait))))
     (define timed-out? (not (sync/timeout (/ timeout 1000.0) wait-thread)))
     (cond
       [timed-out?
        (ctrl 'kill)
        (unless (sync/timeout 5.0 wait-thread)
          (kill-thread wait-thread))
        (test-file-result test-path
                          2
                          (string->bytes/utf-8 (get-output-string stdout-out))
                          (string->bytes/utf-8 (get-output-string stderr-out))
                          (elapsed)
                          0
                          0
                          0)]
       [else
        (define exit-code (ctrl 'exit-code))
        (define stdout-bytes (string->bytes/utf-8 (get-output-string stdout-out)))
        (define stderr-bytes (string->bytes/utf-8 (get-output-string stderr-out)))
        (define merged-bytes (bytes-append stdout-bytes #"\n" stderr-bytes))
        (define-values (parsed-passed parsed-failed parsed-total) (parse-raco-output merged-bytes))
        (define-values (passed failed total)
          (normalize-counts exit-code parsed-passed parsed-failed parsed-total))
        (test-file-result test-path
                          (effective-exit-code exit-code failed)
                          stdout-bytes
                          stderr-bytes
                          (elapsed)
                          passed
                          failed
                          total)])]
    [else
     (ctrl 'wait)
     (define exit-code (ctrl 'exit-code))
     (define stdout-bytes (string->bytes/utf-8 (get-output-string stdout-out)))
     (define stderr-bytes (string->bytes/utf-8 (get-output-string stderr-out)))
     (define merged-bytes (bytes-append stdout-bytes #"\n" stderr-bytes))
     (define-values (parsed-passed parsed-failed parsed-total) (parse-raco-output merged-bytes))
     (define-values (passed failed total)
       (normalize-counts exit-code parsed-passed parsed-failed parsed-total))
     (test-file-result test-path
                       (effective-exit-code exit-code failed)
                       stdout-bytes
                       stderr-bytes
                       (elapsed)
                       passed
                       failed
                       total)]))

(define (resolve-test-path test-path)
  (define p
    (if (path? test-path)
        test-path
        (string->path test-path)))
  (if (absolute-path? p)
      p
      (simplify-path (build-path base-dir p))))

;; Resource classes for the shared scheduler (final plan §2, rev 3) live in
;; classify.rkt next to the metadata they read: `repository-exclusive-file?`
;; (mutation owners) and `co-runs-under-exclusive-file?` (declared
;; temp-dir/sandboxed-home isolation). One derivation, two consumers.

(define (file-timeout-ms test-path timeout)
  (let ([meta-timeout (hash-ref (get-file-metadata test-path) 'timeout #f)])
    (if meta-timeout
        (* meta-timeout 1000)
        (or timeout 120000))))

;; The only surviving execution strategy is one subprocess per test file, so
;; per-file execution mode is a constant recorded through this one accessor
;; (the JSON report keeps its `execution_mode` field with the same meaning).
(define per-file-execution-mode 'subprocess)

(define (mutates-true? raw)
  (and raw (not (member (string-downcase (string-trim raw)) '("" "none" "false" "no" "0" "off")))))

;; Line-anchored: "(module+ test" mentioned inside a comment (e.g. W3
;; reconciliation notes) must not make a file count as a submodule test file —
;; requiring (submod _ test) on such files crashed with "unknown module" and
;; produced zero-parsed results in the retired in-process executor.
(define (module-plus-test-file? path)
  (and (file-exists? path)
       (regexp-match? #px"(?m:^\\s*\\(module\\+\\s+test\\b)" (file->string path))))

(define (run-single-file/subprocess test-path #:timeout [timeout #f])
  (define resolved-path (resolve-test-path test-path))
  (define file-timeout (file-timeout-ms test-path timeout))
  (define t0 (current-inexact-milliseconds))
  (define elapsed (lambda () (exact-round (- (current-inexact-milliseconds) t0))))
  (define stdout-out (open-output-string))
  (define stderr-out (open-output-string))
  ;; v1.00.32 W2 repair (trial c01 window-2, run 41271542715): under a
  ;; root-on launch the runner's per-file children must inherit the
  ;; compiled-root activation — they are separate processes and inherit
  ;; neither current-compiled-file-roots nor the TOCTOU guard, so no test
  ;; load could ever be root-served (zero-root-hits). The launcher exports
  ;; Q_COMPILED_ROOT_FLAGS_FILE; compiled-root-child re-validates it and
  ;; yields the identical bootstrap argv. When inactive (#f) every
  ;; invocation below stays byte-identical to the legacy path.
  (define-values (ctrl)
    (if (file-has-rackunit-tests? resolved-path)
        (let ([cr-args (compiled-root-raco-child-args resolved-path)])
          (if cr-args
              (let ([racket-bin (find-executable-path "racket")])
                ;; The raco launcher binary is literally `exec racket -N
                ;; raco -l- raco/main.rkt "$@"`: identical discovery
                ;; output and exit codes, with the bootstrap applied.
                (define-values (sp2 out-in2 pid2 err-in2 ctrl2)
                  (apply values (apply process*/ports stdout-out #f stderr-out racket-bin cr-args)))
                (when out-in2
                  (close-output-port out-in2))
                ctrl2)
              (let ([raco-bin (find-executable-path "raco")])
                (define-values (sp2 out-in2 pid2 err-in2 ctrl2)
                  (apply
                   values
                   (process*/ports stdout-out #f stderr-out raco-bin "test" "-t" resolved-path)))
                (when out-in2
                  (close-output-port out-in2))
                ctrl2)))
        (let ([cr-args (compiled-root-bare-child-args resolved-path)]
              [racket-bin (find-executable-path "racket")])
          (define-values (sp out-in pid err-in ctrl)
            (apply values
                   (if cr-args
                       (apply process*/ports stdout-out #f stderr-out racket-bin cr-args)
                       (process*/ports stdout-out #f stderr-out racket-bin resolved-path))))
          (when out-in
            (close-output-port out-in))
          ctrl)))
  ;; Bound to a local (not a tail call) so the worker's continuation still names
  ;; `run-single-file/subprocess` while the child process is waited on — the
  ;; batch-worker-death contract test identifies the blocked worker by that
  ;; frame.
  (define result
    (build-result-from-process test-path stdout-out stderr-out ctrl file-timeout elapsed))
  result)

(define (run-single-file test-path #:timeout [timeout #f])
  (run-single-file/subprocess test-path #:timeout timeout))

;; Scheduler ownership prevents a dead coordinator or worker from leaving
;; registered subprocess groups running after the run ends.
(define (call-with-scheduler-ownership thunk)
  (define cust (make-custodian))
  (define owner (current-thread))
  (define watcher
    (thread (lambda ()
              (sync owner)
              (custodian-shutdown-all cust))))
  (dynamic-wind void
                (lambda ()
                  (parameterize ([current-custodian cust]
                                 [current-subprocess-custodian-mode 'kill]
                                 [subprocess-group-enabled #t])
                    (thunk)))
                (lambda ()
                  (custodian-shutdown-all cust)
                  (kill-thread watcher))))

(define (validate-result-multiset! files results)
  (define unmatched
    (for/fold ([remaining files]) ([result (in-list results)])
      (unless (and (test-file-result? result)
                   (member (test-file-result-path result) remaining equal?))
        (error 'run-all-files "unexpected or duplicate worker result"))
      (remove (test-file-result-path result) remaining equal?)))
  (unless (null? unmatched)
    (error 'run-all-files "missing worker results for ~a" unmatched)))

;; The ONE scheduling path: the shared work-conserving queue
;; (scripts/testing/scheduler.rkt). Mutation owners get `repository-exclusive`
;; and dispatch first (priority 0); declared temp-dir/sandboxed-home isolation
;; co-runs while the lock is held; everything else is eligible once the
;; mutation set drains. `#:first-batch-ms-box` receives the single-queue
;; dispatch latency (run start → first completion) — the redefined, kept
;; `first_batch_ms` telemetry field (final plan §2, rev 3).
(define (run-all-files files jobs timeout #:first-batch-ms-box [first-batch-ms-box #f])
  (define results
    (call-with-scheduler-ownership
     (lambda ()
       (run-scheduled
        (for/list ([f (in-list files)]
                   [i (in-naturals)])
          (sched-task
           (format "~a" f)
           ;; The adapter returns a result for every ordinary failure
           ;; (subprocess exit, timeout, child error, an out-of-contract
           ;; path) — the deleted batch barrier's per-file handler, kept
           ;; verbatim. A raise that escapes is unexpected worker
           ;; termination and the scheduler fails the run (fail closed).
           ;; The raw `f` stays the result identity (the scheduler's task id
           ;; is an internal string); run-single-file resolves it.
           (lambda (_id file-timeout-ms)
             (with-handlers
                 ([exn:fail?
                   (lambda (e)
                     (test-file-result f 1 #"" (string->bytes/utf-8 (exn-message e)) 0 0 0 0))])
               ;; Per-file progress marker, kept from the deleted batch
               ;; barrier: S skipped / . pass / T timeout / F fail.
               (define result (run-single-file f #:timeout file-timeout-ms))
               (define exit-code (test-file-result-exit-code result))
               (cond
                 [(eq? (classify-test-result result) 'SKIPPED_BY_PROFILE) (display "S")]
                 [(= exit-code 0) (display ".")]
                 [(= exit-code 2) (display "T")]
                 [else (display "F")])
               (flush-output)
               result))
           ;; `timeout` arrives in milliseconds from run-suite-once and is
           ;; passed through opaquely to the adapter, exactly like the batch
           ;; barrier passed it to run-single-file.
           (or timeout 120000)
           (repository-exclusive-file? f)
           (co-runs-under-exclusive-file? f)
           (if (repository-exclusive-file? f) 0 1)
           i))
        jobs
        #:late-ms-box first-batch-ms-box))))
  ;; Every scheduled occurrence must have exactly one result. A worker that
  ;; dies without publishing must never make the remaining files look green.
  (validate-result-multiset! files results)
  results)

;; W1 action 3: prepared-environment CI evidence, passed through an explicit
;; environment/input contract (the CI workflow exports these after the
;; guarded restore; see .github/actions/setup-racket/action.yml + ci.yml).
;; Local runs never invent `restored` — the default is `unavailable` with
;; 'null restore/fallback timings.
(define (prepared-environment-state)
  (define raw-result (getenv "Q_PREPARED_ENV_STATE"))
  ;; Backward-compatible alias for any older CI call site that exported
  ;; the same normalized vocabulary under the pre-W1 name.
  (define legacy-result (getenv "Q_PREPARED_ENV_RESULT"))
  (define result
    (if (and raw-result (member raw-result '("restored" "rebuilt")))
        raw-result
        (if (and legacy-result (member legacy-result '("restored" "rebuilt")))
            legacy-result
            "unavailable")))
  (define (env-ms name)
    (define v (getenv name))
    (if (and v (regexp-match? #px"^[0-9]+$" v))
        (string->number v)
        'null))
  (hasheq 'result
          result
          'restore_ms
          ;; Known whenever the guarded restore step ran (success or
          ;; failure); 'null when the prepared path was not in play.
          (env-ms "Q_PREPARED_ENV_RESTORE_MS")
          'fallback_ms
          (if (string=? result "rebuilt")
              (env-ms "Q_PREPARED_ENV_FALLBACK_MS")
              'null)))

;; `auto` and `subprocess` are the only surviving modes and both execute one
;; subprocess per test file: the grouped/in-process executor is deleted, so no
;; suite maps to a second execution strategy any more.
(define (effective-mode requested-mode _suite-label)
  (if (eq? requested-mode 'auto) 'subprocess requested-mode))

;; Read q-version from util/version.rkt without loading the module
;; (keeps runner startup free of contract instantiation).
(define (detect-runner-version)
  (define version-path (build-path base-dir "util" "version.rkt"))
  (with-handlers ([exn:fail? (lambda (_) "unknown")])
    (define m (regexp-match #rx"q-version[ \t]+\"([^\"]+)\"" (file->string version-path)))
    (if m
        (cadr m)
        "unknown")))

(define (surface-kind path)
  (when (link-exists? path)
    (error (quote run-suite-once) "unsafe surface symlink: ~a" path))
  (define kind (file-or-directory-type path #f))
  (when (and kind (not (eq? kind (quote file))))
    (error (quote run-suite-once) "unsafe surface type: ~a" path))
  (when kind
    (define stat (file-or-directory-stat path #f))
    (unless (= (bitwise-and (hash-ref stat (quote mode)) #o170000) #o100000)
      (error (quote run-suite-once) "unsafe non-regular surface: ~a" path))
    (when (> (hash-ref stat (quote hardlink-count)) 1)
      (error (quote run-suite-once) "unsafe surface hardlink: ~a" path)))
  kind)

;; Managed surfaces are DETECTED, never rewritten: restoring saved bytes (or a
;; git checkout) would destroy pre-existing or concurrent user edits. A test
;; that mutates them is a defect and fails the suite; mutation coverage belongs
;; in temporary fixtures.
(define (surface-state root name)
  (define path (build-path root name))
  (if (surface-kind path)
      (cons (file->bytes path) (file-or-directory-permissions path (quote bits)))
      (cons #f #f)))

;; Reports (does not raise) so the caller can finish printing evidence and
;; still fail closed afterwards.
(define (call-with-surface-guard root thunk)
  (define before
    (for/list ([name (in-list repo-surface-files)])
      (cons name (surface-state root name))))
  (define violations (box '()))
  (dynamic-wind void
                thunk
                (lambda ()
                  (set-box! violations
                            (for/list ([entry (in-list before)]
                                       #:unless (equal? (cdr entry) (surface-state root (car entry))))
                              (car entry)))))
  (unbox violations))

(define (run-suite-once suite-files
                        jobs
                        timeout-ms
                        strict?
                        suite-label
                        repeat-num
                        repeat-total
                        mode
                        json-out
                        ledger
                        profile
                        #:shard [shard #f]
                        #:phases [phases (hasheq)])
  (define t0 (current-inexact-milliseconds))
  (define exec-start-ms (hash-ref phases 'execution_start_ms #f))
  (define selection-end-ms (hash-ref phases 'selection_end_ms #f))
  ;; Single-queue dispatch-latency telemetry (final plan §2, rev 3 "Keep (do
  ;; not silently drop)"): first_batch_ms is run start → first completion for
  ;; the ONE shared queue; the legacy serial/parallel pair collapses to the
  ;; same value plus a constant 0. The keys stay in the schema for output
  ;; compatibility; nothing in scripts/ci/* or the claim schema reads them.
  (define serial-first-batch-ms-box (box #f))
  (define parallel-first-batch-ms-box (box 0))
  (define-values (skipped-files runnable-files)
    (partition (lambda (f)
                 (profile-skips-test? profile (hash-ref (get-file-metadata f) 'requires '())))
               suite-files))
  (define skipped-results
    (for/list ([f (in-list skipped-files)])
      (make-skipped-result f profile (hash-ref (get-file-metadata f) 'requires '()))))
  (when (pair? skipped-files)
    (printf ";; run-tests: profile=~a skipped ~a file~a by @requires metadata~n"
            profile
            (length skipped-files)
            (if (= (length skipped-files) 1) "" "s")))
  ;; ONE scheduling call through the shared work-conserving queue: mutation
  ;; owners take repository-exclusive and dispatch first; declared
  ;; temp-dir/sandboxed-home isolation co-runs while the lock is held;
  ;; everything else is eligible once the mutation set drains. Results are
  ;; sorted back into suite-file order below — never a fixed serial/parallel
  ;; partition barrier.
  (define mutation-owners (filter repository-exclusive-file? runnable-files))
  (when (pair? mutation-owners)
    (printf
     (string-append ";; run-tests: ~a mutation-owning file~a hold repository-exclusive "
                    "(dispatched first; only declared temp-dir/sandboxed-home isolation co-runs)~n")
     (length mutation-owners)
     (if (= (length mutation-owners) 1) "" "s")))
  ;; One guard around the single execution path (sequential and queued): a
  ;; test that rewrites a managed surface fails the suite instead of being
  ;; silently reverted.
  (define results-box (box '()))
  (define surface-violations
    (call-with-surface-guard base-dir
                             (lambda ()
                               (set-box! results-box
                                         (run-all-files runnable-files
                                                        jobs
                                                        timeout-ms
                                                        #:first-batch-ms-box
                                                        serial-first-batch-ms-box)))))
  (define queue-results (unbox results-box))
  (define file-order
    (for/hash ([f (in-list suite-files)]
               [i (in-naturals)])
      (values f i)))
  (define results
    (sort (append skipped-results queue-results)
          <
          #:key (lambda (r) (hash-ref file-order (test-file-result-path r) 0))))
  ;; The suite boundary must also account for skipped files.
  (validate-result-multiset! suite-files results)
  (define total-elapsed (exact-round (- (current-inexact-milliseconds) t0)))
  (define exec-end-ms (current-inexact-milliseconds))
  (define runner-version (detect-runner-version))
  ;; Actual per-file execution mode, keyed by the raw result-path string (the
  ;; key form reporting.rkt's file-execution-mode lookup uses). Skipped files
  ;; never ran, so they carry no mode entry.
  (define actual-mode-hash
    (and json-out
         (for/hash ([r (in-list results)]
                    #:unless (eq? (classify-test-result r) 'SKIPPED_BY_PROFILE))
           (define p (test-file-result-path r))
           (values (if (path? p)
                       (path->string p)
                       p)
                   per-file-execution-mode))))
  (print-summary results total-elapsed)
  (print-run-summary-record results
                            #:suite suite-label
                            #:profile profile
                            #:shard shard
                            #:mode mode
                            #:elapsed-ms total-elapsed
                            #:runner-version runner-version)
  (when json-out
    (write-json-results! json-out
                         results
                         #:suite (string->symbol suite-label)
                         #:mode mode
                         #:elapsed-ms total-elapsed
                         #:ledger ledger
                         #:profile profile
                         #:shard shard
                         #:runner-version runner-version
                         #:extra
                         (let ([m (make-hasheq)])
                           (hash-set! m 'runner_start_ms (exact-round runner-start-ms))
                           (hash-set! m 'execution_end_ms (exact-round exec-end-ms))
                           (when exec-start-ms
                             (hash-set! m 'execution_start_ms (exact-round exec-start-ms)))
                           (when selection-end-ms
                             (hash-set! m 'selection_end_ms (exact-round selection-end-ms)))
                           ;; Single-queue dispatch latency (run start → first completion).
                           ;; The serial/parallel pair is kept for schema compatibility:
                           ;; serial carries the queue's latency, parallel is the constant 0.
                           (when (unbox serial-first-batch-ms-box)
                             (hash-set! m 'serial_first_batch_ms (unbox serial-first-batch-ms-box)))
                           (hash-set! m 'parallel_first_batch_ms (unbox parallel-first-batch-ms-box))
                           (when (unbox serial-first-batch-ms-box)
                             ;; Legacy field: now the one queue's dispatch latency.
                             (hash-set! m 'first_batch_ms (unbox serial-first-batch-ms-box)))
                           (hash-set! m 'prepared_environment (prepared-environment-state))
                           m)
                         #:actual-modes actual-mode-hash))
  (when ledger
    (print-ledger-summary ledger results))
  (save-failure-logs results #:profile profile #:mode mode)
  (define failed-files
    (count (lambda (r)
             (and (not (eq? (classify-test-result r) 'SKIPPED_BY_PROFILE))
                  (not (= (test-file-result-exit-code r) 0))
                  (not (= (test-file-result-exit-code r) 2))))
           results))
  (define timeout-files
    (count (lambda (r)
             (and (not (eq? (classify-test-result r) 'SKIPPED_BY_PROFILE))
                  (= (test-file-result-exit-code r) 2)))
           results))
  (when strict?
    (define suspicious
      (filter (lambda (r) (and (= (test-file-result-exit-code r) 0) (= (test-file-result-total r) 0)))
              results))
    (when (pair? suspicious)
      (newline)
      (if (> repeat-total 1)
          (printf "⛔ STRICT MODE (run ~a/~a): files with zero parsed tests:\n"
                  repeat-num
                  repeat-total)
          (displayln "⛔ STRICT MODE: files with zero parsed tests:"))
      (for ([s (in-list suspicious)])
        (printf "  ~a (exit=0 but no rackunit output parsed)~n" (test-file-result-path s)))
      (exit 4)))
  (define exit-code (summary-exit-code failed-files timeout-files))
  (when (pair? surface-violations)
    ;; Evidence above is already printed/persisted; now fail closed without
    ;; touching the files. The snapshot diff cannot distinguish a test
    ;; mutation from a concurrent edit, so it is never auto-reverted.
    (error (quote run-suite-once)
           (string-append "managed surface(s) changed during this run: "
                          (string-join surface-violations ", ")
                          "; mutation coverage belongs in temporary fixtures "
                          "(the runner never restores user files)")))
  (when (and (zero? exit-code) (> repeat-total 1))
    (printf ";; Run ~a/~a: PASS~n" repeat-num repeat-total))
  (values exit-code results))

(define (extract-shard-args! args)
  ;; Extract --shard-index and --shard-total from args, returning
  ;; (values shard-index shard-total filtered-args).
  ;; Defaults: shard-index=0, shard-total=1 (no sharding).
  (define shard-index (box 0))
  (define shard-total (box 1))
  (define filtered
    (let loop ([rest (vector->list args)])
      (match rest
        ['() '()]
        [(list (== "--shard-index") n rest ...)
         (set-box! shard-index (string->number n))
         (loop rest)]
        [(list (== "--shard-total") n rest ...)
         (set-box! shard-total (string->number n))
         (loop rest)]
        [(list elem rest ...) (cons elem (loop rest))])))
  (values (unbox shard-index) (unbox shard-total) (list->vector filtered)))

(define (main args)
  (define-values (shard-index shard-total filtered-args) (extract-shard-args! (list->vector args)))
  (define-values (jobs
                  sequential?
                  timeout
                  strict?
                  suite
                  extra-files
                  repeat
                  record-gate?
                  inventory?
                  requested-mode
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
                  generate-covers-manifest?
                  shard-plan
                  durations)
    (parse-args (vector->list filtered-args)))
  (validate-args! jobs
                  sequential?
                  timeout
                  strict?
                  suite
                  extra-files
                  repeat
                  record-gate?
                  inventory?
                  requested-mode
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
                  generate-covers-manifest?
                  shard-plan
                  durations)
  ;; v1.00.24 W3: sharded runs never record gate evidence. A shard covers
  ;; only part of the suite, so recording would emit (or overwrite) evidence
  ;; that claims to be a full-suite PASS. Fail closed before doing any work.
  (when record-gate?
    (define refusal (gate-evidence-refusal shard-index shard-total))
    (when refusal
      (eprintf ";; run-tests: ~a~n" refusal)
      (exit 2)))
  (when lint-metadata?
    (define summary
      (print-lint-report (if (pair? extra-files)
                             (map normalize-test-path extra-files)
                             (collect-test-files suite))))
    ;; W3: the schema-v1 lint is enforced. Invalid tags/unknown tags/malformed
    ;; values are errors and fail the invocation (exit 1). Missing required
    ;; tags remain warnings — they do not fail the lint.
    (exit (if (> (hash-ref summary 'invalid_count 0) 0) 1 0)))
  ;; ── @covers manifest regeneration (W4 action 2) ──────────────────────
  (when generate-covers-manifest?
    (define written (write-covers-manifest! base-dir (detect-runner-version)))
    (define entries (hash-ref written 'entries '()))
    (printf ";; run-tests: @covers manifest written → tests/.coverage-manifest.json~n")
    (printf ";; run-tests: entries=~a manual-review=~a runner-version=~a~n"
            (length entries)
            (count (lambda (e) (equal? (hash-ref e 'source #f) "manual-review")) entries)
            (hash-ref written 'runner_version))
    (exit 0))
  ;; ── Impact selection (W4) + deterministic prioritization (W6) ────────
  ;; With --changed-base the impact selection REPLACES the static suite
  ;; list (escalations run their declared fallback suites). Explicit
  ;; extra files stay honored: they are also the top `explicit` tier
  ;; under --prioritize impact (the L0 current-test loop).
  (define impact-plan (box #f))
  (define impact-selection #f)
  (define impact-changed '())
  (define prioritize-payload #f)
  (when changed-base
    (define-values (sel-files sel changed)
      (run-impact-selection! changed-base
                             (or changed-head "HEAD")
                             #:root base-dir
                             #:collect collect-test-files))
    (set! impact-selection sel)
    (set! impact-changed changed)
    (when explain?
      (print-impact-explain sel #:base changed-base #:head (or changed-head "HEAD"))
      ;; --explain is a VIEW (help text: "print ... and exit"): it never
      ;; executes tests, not even fallback suites. JSON evidence still
      ;; lands in --json-out when given.
      (when json-out
        (call-with-output-file json-out
                               #:exists 'truncate/replace
                               (lambda (out)
                                 (write-json (hasheq 'explain #t 'selection (selection->jsexpr sel))
                                             out)
                                 (newline out))))
      (exit 0))
    ;; Doc-only changes are an explicit zero-source-change no-op with JSON
    ;; evidence — never a silent green with zero tests.
    (when (hash-ref sel 'doc-only?)
      (printf ";; run-tests: doc-only change — zero-source-change no-op~n")
      (when json-out
        (call-with-output-file json-out
                               #:exists 'truncate/replace
                               (lambda (out)
                                 (write-json (hasheq 'doc_only #t 'selection (selection->jsexpr sel))
                                             out)
                                 (newline out))))
      (exit 0))
    (define explicit (map normalize-test-path extra-files))
    (define merged (remove-duplicates (append explicit sel-files)))
    ;; Empty selection with a non-doc-only change is an ERROR (exit 3 with
    ;; the reasoned selection JSON), never a silent pass.
    (unless (pair? merged)
      (printf ";; run-tests: ERROR impact selection is empty — refusing to run zero tests~n")
      (when json-out
        (call-with-output-file
         json-out
         #:exists 'truncate/replace
         (lambda (out)
           (write-json (hasheq 'error 'empty-selection 'selection (selection->jsexpr sel)) out)
           (newline out))))
      (exit 3))
    ;; Deterministic prioritization (W6 action 1): ordering ONLY — the
    ;; selected set is never altered. Serial (mutation-sensitive) and
    ;; parallel partitions are prioritized independently; serialization
    ;; semantics are preserved exactly.
    (when (equal? prioritize "impact")
      (define-values (hist-weights hist-status) (load-failure-history failure-history))
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
      (set! merged (append s-files p-files))
      (set! prioritize-payload (partition-entries->jsexpr s-entries p-entries hist-status)))
    ;; Dry run: print the machine-readable plan, execute nothing, exit 0.
    (when impact-dry-run?
      (printf ";; run-tests: impact dry run — executing nothing~n")
      (define plan
        (hasheq 'plan
                merged
                'selection
                (selection->jsexpr sel)
                'prioritization
                (or prioritize-payload (hasheq 'prioritized #f))))
      (if json-out
          (call-with-output-file json-out
                                 #:exists 'truncate/replace
                                 (lambda (out)
                                   (write-json plan out)
                                   (newline out)))
          (begin
            (write-json plan)
            (newline)))
      (exit 0))
    (set-box! impact-plan merged))
  (define cleaned-dirs (clean-stale-bytecode! (current-directory)))
  (when (> cleaned-dirs 0)
    (printf ";; run-tests: cleaned ~a stale compiled/ director~a~n"
            cleaned-dirs
            (if (= cleaned-dirs 1) "y" "ies")))
  (define all-suite-files
    (cond
      [(unbox impact-plan) (unbox impact-plan)]
      [(pair? extra-files) (map normalize-test-path extra-files)]
      [else (collect-test-files suite)]))
  ;; ── Duration-aware shard planning (`--shard-plan active`) ─────────────
  ;; `active` consumes the duration-aware plan: each shard runs exactly its
  ;; planned file list instead of the round-robin slice. Execution semantics
  ;; (one subprocess per file, serial-ahead for mutation-sensitive files) are
  ;; unchanged — only the shard assignment differs. The `report` and `measure`
  ;; experiment modes are deleted (§11.3).
  (define plan-mode (and (equal? shard-plan "active") 'active))
  (define active-plan (box #f))
  ;; Planning without sharding is a no-op (there is only one shard); state it
  ;; explicitly so CI logs never hide the effective configuration.
  (when (and plan-mode (not (> shard-total 1)))
    (printf ";; run-tests: --shard-plan ~a ignored — --shard-total=~a (unsharded)~n"
            shard-plan
            shard-total))
  (when (and plan-mode (> shard-total 1))
    (define dur-source (and (string? durations) (non-empty-string? durations) durations))
    (define-values (dur dur-status) (load-duration-snapshot dur-source))
    (printf ";; run-tests: shard-plan=~a durations=~a status=~a known=~a~n"
            shard-plan
            (or dur-source "<none>")
            dur-status
            (hash-count dur))
    (unless (eq? dur-status 'ok)
      (printf ";; run-tests: unmeasured files use the conservative p95 default duration~n"))
    (define plan
      (build-shard-plan/safe
       all-suite-files
       shard-total
       #:durations dur
       #:profile-skips?
       (lambda (f) (profile-skips-test? profile (hash-ref (get-file-metadata f) 'requires '())))
       #:duration-source dur-source))
    (printf ";; run-tests: duration-aware plan (~a) replaces round-robin for shard ~a/~a~n"
            (shard-plan-mode plan)
            shard-index
            shard-total)
    (set-box! active-plan plan))
  (define suite-files
    (cond
      [(unbox active-plan) (plan-shard-files (unbox active-plan) shard-index)]
      [(> shard-total 1)
       (begin
         (printf ";; run-tests: sharding ~a files — shard ~a/~a~n"
                 (length all-suite-files)
                 shard-index
                 shard-total)
         (shard-files all-suite-files shard-index shard-total))]
      [else all-suite-files]))
  (when inventory?
    (if (pair? suite-files)
        (begin
          (print-inventory suite suite-files)
          (exit 0))
        (begin
          (displayln "No test files matched the selected suite.")
          (exit 1))))
  (unless (pair? suite-files)
    (displayln "No test files matched the selected suite.")
    (exit 1))
  (define timeout-ms (and timeout (* timeout 1000)))
  (define suite-label
    (if changed-base
        "impact"
        (symbol->string suite)))
  (define mode (effective-mode requested-mode suite-label))
  (define ledger (and ledger-path (load-known-failure-ledger ledger-path)))
  (define n-files (length suite-files))
  (printf ";; run-tests: suite=~a files=~a jobs=~a sequential=~a repeat=~a mode=~a profile=~a~n"
          suite-label
          n-files
          jobs
          sequential?
          repeat
          mode
          profile)
  (newline)
  (when (> repeat 1)
    (printf ";; run-tests: running suite ~a time~a for confidence gate~n"
            repeat
            (if (= repeat 1) "" "s")))
  (define last-results (box '()))
  ;; W0 fast-gate phases: selection finished and execution starts at the
  ;; repeat loop; run-suite-once records runner_start/execution_end itself.
  (define phases (make-hasheq))
  (hash-set! phases 'selection_end_ms (current-inexact-milliseconds))
  (hash-set! phases 'execution_start_ms (current-inexact-milliseconds))
  ;; W5/W6 post-run evidence: embed selection + prioritization + changed
  ;; files into the results JSON, and surface the FIRST failing selected
  ;; test (with its selection and priority reason) on prioritized runs.
  ;; The summary is a pointer, not the source of truth — full results and
  ;; failure logs remain available as before.
  (define (emit-impact-evidence!)
    (when (and changed-base json-out impact-selection)
      (embed-impact-in-results! json-out impact-selection prioritize-payload impact-changed))
    (when (and (equal? prioritize "impact") prioritize-payload (pair? (unbox last-results)))
      (define first-fail
        (for/or ([r (in-list (unbox last-results))])
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
  (for ([run-num (in-range 1 (add1 repeat))])
    (when (> repeat 1)
      (printf "~n;; ── Run ~a/~a ──~n" run-num repeat))
    (define-values (exit-code results)
      (run-suite-once suite-files
                      jobs
                      timeout-ms
                      strict?
                      suite-label
                      run-num
                      repeat
                      mode
                      json-out
                      ledger
                      profile
                      #:shard (and (> shard-total 1) (cons shard-index shard-total))
                      #:phases phases))
    (set-box! last-results results)
    (unless (zero? exit-code)
      (emit-impact-evidence!)
      (exit exit-code)))
  (emit-impact-evidence!)
  (when record-gate?
    (define inv-hash (compute-inventory-hash suite-files))
    (record-gate-evidence! suite-label
                           #:results (unbox last-results)
                           #:args args
                           #:jobs jobs
                           #:timeout timeout
                           #:repeat repeat
                           #:file-count (length suite-files)
                           #:inventory-hash inv-hash
                           #:shard-index shard-index
                           #:shard-total shard-total))
  (exit 0))

;; ----------------------------------------------------------------------
;; CLI grammar lives in scripts/testing/runner-cli.rkt (plan §11.3 size
;; clause): usage, known-suites, known-modes, parse-args, validate-args! are
;; required below and re-exported unchanged, so the adapter CLI and every
;; `runner-ref 'parse-args` caller keep working.
;; ----------------------------------------------------------------------

;; Direct-invocation guard (Phase D step 1).
;; `scripts/run-tests.rkt` is now a compatibility adapter that owns the
;; command-line entry point and delegates selection/execution/reporting to the
;; shared CI engine in `scripts/testing/full-plan.rkt`. runner.rkt therefore
;; autoruns only when IT is the module that was actually run (`racket
;; scripts/run-tests/runner.rkt`), never when the facade loads it for its
;; re-exports. Matching is on paths, not on a file name, so a copy of
;; runner.rkt still autoruns and a copy of the facade still runs the adapter.
(define invoked-directly?
  (let ([run-file (find-system-path 'run-file)]
        [source (variable-reference->module-source (#%variable-reference))])
    (and (path? run-file)
         (path? source)
         (equal? (simplify-path (path->complete-path run-file))
                 (simplify-path (path->complete-path source))))))
(when invoked-directly?
  (main (vector->list (current-command-line-arguments))))
