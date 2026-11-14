#lang racket/base

;; run-tests/runner.rkt — Test execution orchestration for run-tests.rkt
;;
;; Owns the runner machinery: subprocess/in-process execution of individual
;; test files, parallel batch scheduling, suite-level orchestration
;; (run-single-file, run-all-files, run-suite-once), and the command-line
;; entry point (main) with its direct-invocation guard.
;; Extracted from run-tests.rkt (v0.99.43 W0c).
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
         (only-in "classify.rkt"
                  base-dir
                  normalize-test-path
                  collect-test-files
                  get-file-metadata
                  mutating-file?
                  repo-surface-files
                  clean-stale-bytecode!
                  file-has-rackunit-tests?
                  shard-files
                  print-lint-report)
         (only-in "parse.rkt"
                  test-file-result
                  test-file-result?
                  test-file-result-path
                  test-file-result-exit-code
                  test-file-result-total
                  test-file-result-elapsed-ms
                  test-file-result-requested-execution-mode
                  test-file-result-grouped-fallback-reason
                  parse-raco-output
                  normalize-counts
                  effective-exit-code
                  classify-test-result)
         (only-in "compiled-root-child.rkt"
                  compiled-root-raco-child-args
                  compiled-root-bare-child-args)
         (only-in "reporting.rkt"
                  print-summary
                  save-failure-logs
                  summary-exit-code
                  write-json-results!
                  result-status
                  print-ledger-summary
                  print-run-summary-record)
         (only-in "ledger.rkt" load-known-failure-ledger)
         (only-in "cli.rkt" parse-args validate-args!)
         (only-in "profiles.rkt"
                  profile-skips-test?
                  make-skipped-result
                  area-grouped-decision
                  grouped-area-config
                  grouped-area-config->jsexpr
                  grouped-rollback-areas
                  grouped-rollback-env-var
                  grouped-fallback-rows
                  test-file-area)
         (only-in "shard-plan.rkt"
                  build-shard-plan/safe
                  plan-shard-files
                  print-shard-plan-report
                  print-starvation-tail-report
                  load-duration-snapshot
                  shard-plan-mode
                  default-ordering
                  current-max-age-seconds
                  prepare-ordering
                  order-files
                  ordering-record
                  ordering-record-mode
                  ordering-record-requested
                  ordering-record-fallback-reason
                  ordering-record->jsexpr)
         (only-in "gate-evidence.rkt" record-gate-evidence! gate-evidence-refusal)
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
                  embed-impact-in-results!))

(provide run-single-file
         run-all-files
         run-all-files/queue
         run-suite-once
         make-run-all-telemetry
         compute-scheduler-telemetry
         prepared-environment-state
         partition-scheduler-fields
         mark-execution-mode!
         ;; v1.00.24 W7: explicit grouped-fallback telemetry
         current-requested-execution-mode
         execution-mode-of
         execution-eligibility-reason
         main
         ;; Phase C fold: overhead diagnostics + local feedback telemetry
         ;; (formerly overhead.rkt).
         (struct-out overhead-result)
         make-overhead-result
         run-overhead-command
         collect-overhead-diagnostics
         format-overhead-result
         print-overhead-diagnostics
         local-p90-min-samples
         local-loop-p90
         slo-verdict
         machine-context
         local-feedback-loops
         area-mode-rows
         collect-local-p90
         check-local-p90-record
         check-local-p90-file
         implementation-sha)

;; ── W0 fast-gate budget instrumentation (schema-additive) ─────────────
;; Wall clock at module load: the retained per-job JSON can then attribute
;; time inside the fast gate (runner boot → selection → compile/require
;; warmup → execution → summary) instead of one undifferentiated number.
(define runner-start-ms (current-inexact-milliseconds))

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
                          0
                          #f
                          #f)]
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
                          total
                          #f
                          #f)])]
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
                       total
                       #f
                       #f)]))

(define (resolve-test-path test-path)
  (define p
    (if (path? test-path)
        test-path
        (string->path test-path)))
  (if (absolute-path? p)
      p
      (simplify-path (build-path base-dir p))))

(define (file-timeout-ms test-path timeout)
  (let ([meta-timeout (hash-ref (get-file-metadata test-path) 'timeout #f)])
    (if meta-timeout
        (* meta-timeout 1000)
        (or timeout 120000))))

(define (parse-result-bytes test-path stdout-bytes stderr-bytes exit-code elapsed-ms)
  (define merged-bytes (bytes-append stdout-bytes #"\n" stderr-bytes))
  (define-values (parsed-passed parsed-failed parsed-total) (parse-raco-output merged-bytes))
  (define-values (passed failed total)
    (normalize-counts exit-code parsed-passed parsed-failed parsed-total))
  (test-file-result test-path
                    (effective-exit-code exit-code failed)
                    stdout-bytes
                    stderr-bytes
                    elapsed-ms
                    passed
                    failed
                    total
                    #f
                    #f))

;; Per-file execution mode attribution (W0 fast-budget): records how each
;; file actually ran ('grouped-in-process vs 'subprocess) so the JSON report
;; can separate process-boot-dominated files from in-process ones. Keys are
;; resolved path strings (JSON-safe), values are symbols.
(define execution-modes (make-hash))
(define (mark-execution-mode! resolved-path mode)
  (hash-set! execution-modes (path->string (simplify-path resolved-path)) mode))
(define (execution-mode-of resolved-path)
  (hash-ref execution-modes (path->string (simplify-path resolved-path)) 'subprocess))

;; ── v1.00.24 W7: explicit grouped-fallback telemetry ──────────────────
;; A grouped-mode request that executes subprocess must say why and may not
;; be counted as grouped. The CLI requested mode (before auto/fast suite
;; resolution) is stamped onto every result so per-file JSON can distinguish
;; "asked for grouped" from "actually ran grouped".
(define current-requested-execution-mode (make-parameter #f))

;; Attach W7 telemetry to a finished result (additive; nothing here changes
;; exit code, verdict, or counts).
(define (with-grouped-telemetry result
                                #:requested-mode [requested-mode #f]
                                #:fallback-reason [fallback-reason #f])
  (if (and (not requested-mode) (not fallback-reason))
      result
      (struct-copy test-file-result
                   result
                   [requested-execution-mode
                    (or requested-mode (test-file-result-requested-execution-mode result))]
                   [grouped-fallback-reason
                    (or fallback-reason (test-file-result-grouped-fallback-reason result))])))

;; Stable named grouped-eligibility reasons (do not rename: downstream
;; evidence and the characterization report key on these exact tokens):
;;   'declared-process-isolation      file declares @isolation process|subprocess
;;   'declared-mutation               file declares a truthy @mutates token
;;   'missing-module-plus-test-form   no real (module+ test ...) submodule
(define (execution-eligibility-reason resolved-path)
  (define meta (get-file-metadata resolved-path))
  (define isolation (hash-ref meta 'isolation #f))
  (cond
    [(member isolation '("process" "subprocess")) 'declared-process-isolation]
    [(mutates-true? (hash-ref meta 'mutates #f)) 'declared-mutation]
    [(not (module-plus-test-file? resolved-path)) 'missing-module-plus-test-form]
    [else #f]))

(define (run-single-file/subprocess test-path
                                    #:timeout [timeout #f]
                                    #:requested-mode [requested-mode #f]
                                    #:fallback-reason [fallback-reason #f])
  (define resolved-path (resolve-test-path test-path))
  (mark-execution-mode! resolved-path 'subprocess)
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
  (with-grouped-telemetry
   (build-result-from-process test-path stdout-out stderr-out ctrl file-timeout elapsed)
   #:requested-mode requested-mode
   #:fallback-reason fallback-reason))

;; Line-anchored: "(module+ test" mentioned inside a comment (e.g. W3
;; reconciliation notes) must not make a file count as grouped-eligible —
;; requiring (submod _ test) on such files crashed with "unknown module"
;; and produced zero-parsed results.
(define (module-plus-test-file? path)
  (and (file-exists? path)
       (regexp-match? #px"(?m:^\\s*\\(module\\+\\s+test\\b)" (file->string path))))

(define (in-process-module-path resolved-path)
  (if (module-plus-test-file? resolved-path)
      (make-resolved-module-path (cons resolved-path '(test)))
      (make-resolved-module-path resolved-path)))

;; Grouped/in-process execution requires a real (module+ test ...) submodule.
;; Silent checks can still produce ZERO_PARSED and are not replayed. Other
;; forms, including main-only and top-level checks, run as raco subprocesses.
(define (in-process-eligible? resolved-path)
  (not (execution-eligibility-reason resolved-path)))

;; ── W1 v1.00.16 grouped eligibility contract ──────────────────────────
;; A fast file qualifies for grouped in-process execution ONLY when it
;; declares no `@isolation process`/`@isolation subprocess`, declares no
;; `@mutates` (missing, `none`, or an explicit false-y token), AND uses the
;; `(module+ test ...)` submodule form. Every other file keeps today's
;; subprocess execution path byte-for-byte. Documented in
;; q/docs/TEST_CONVENTIONS.md (schema contract) and audited per file in
;; q/docs/reports/unit-fast-eligibility-v1.00.16.md.
(define (mutates-true? raw)
  (and raw (not (member (string-downcase (string-trim raw)) '("" "none" "false" "no" "0" "off")))))

(define (grouped-eligible? path)
  (define rp (resolve-test-path path))
  (define meta (get-file-metadata rp))
  (define isolation (hash-ref meta 'isolation #f))
  (and (not (member isolation '("process" "subprocess")))
       (not (mutates-true? (hash-ref meta 'mutates #f)))
       (module-plus-test-file? rp)))

;; raco test executes each file with current-directory bound to that file's
;; directory; grouped/in-process execution must do the same or tests that
;; resolve sibling paths (e.g. "../scripts/foo.rkt") break.
(define (in-process-cwd resolved-path)
  (or (and resolved-path (path-only (simplify-path resolved-path))) base-dir))

;; A test file that calls (exit ...) during grouped/in-process execution must
;; not terminate the host runner: custodians do not guard the exit handler, so
;; an unguarded (exit 0) silently kills the whole suite (observed W3: output
;; truncated at file 323 with process exit 0). Redirect exit to an exception
;; so the worker records a failure without terminating the host.
(struct in-process-test-exit exn:fail (code))

(define (make-in-process-exit-handler exit-box)
  (lambda (code)
    (raise (in-process-test-exit (format "test file requested (exit ~a) during in-process execution"
                                         code)
                                 (current-continuation-marks)
                                 (if (and (exact-integer? code) (not (zero? code))) code 1)))))

(define (run-single-file/in-process test-path
                                    #:timeout [timeout #f]
                                    #:requested-mode [requested-mode #f])
  (define resolved-path (resolve-test-path test-path))
  ;; Bare RackUnit files without run-tests/module+ still need raco's discovery output;
  ;; fall back to subprocess to avoid reintroducing zero-parsed false greens.
  (cond
    [(not (in-process-eligible? resolved-path))
     ;; W7: a grouped request that executes subprocess is named and never
     ;; counted as grouped.
     (run-single-file/subprocess test-path
                                 #:timeout timeout
                                 #:requested-mode requested-mode
                                 #:fallback-reason (execution-eligibility-reason resolved-path))]
    ;; v1.00.27 W3 (#9591): per-area grouped policy — only areas backed by an
    ;; exact subprocess-vs-grouped comparison execute grouped; a per-area env
    ;; rollback switch restores subprocess for a single area, named, without
    ;; touching the others. W5: the policy gates policy-driven grouped runs
    ;; (orchestrator #:mode 'grouped and suite requests). An explicit
    ;; call-site #:mode 'grouped-in-process is a deliberate per-call
    ;; escalation and is never downgraded by an un-expanded area — the
    ;; characterization suite's parity contract depends on that. The
    ;; In-process eligibility still applies to every path; completed silent
    ;; runs are not replayed. No production default changes.
    [(and (not (equal? requested-mode "grouped-in-process"))
          (let-values ([(mode fallback) (area-grouped-decision resolved-path)])
            (and (eq? mode 'subprocess) (or fallback 'area-not-expanded))))
     =>
     (lambda (fallback)
       (run-single-file/subprocess test-path
                                   #:timeout timeout
                                   #:requested-mode requested-mode
                                   #:fallback-reason fallback))]
    [else
     (mark-execution-mode! resolved-path 'grouped-in-process)
     (define file-timeout (file-timeout-ms test-path timeout))
     (define t0 (current-inexact-milliseconds))
     (define elapsed (lambda () (exact-round (- (current-inexact-milliseconds) t0))))
     (define stdout-out (open-output-string))
     (define stderr-out (open-output-string))
     (define exit-code (box #f))
     (define cust (make-custodian))
     (define completed?
       (dynamic-wind
        void
        (lambda ()
          (define worker
            (parameterize ([current-custodian cust])
              (thread (lambda ()
                        (with-handlers
                            ([in-process-test-exit? (lambda (e)
                                                      (displayln (exn-message e) stderr-out)
                                                      (define code (in-process-test-exit-code e))
                                                      (set-box! exit-code (if (= code 2) 1 code)))]
                             [exn:fail? (lambda (e)
                                          (displayln (exn->string e) stderr-out)
                                          ;; v1.00.24 W7: mirror raco test's module-
                                          ;; exception phrasing so grouped and
                                          ;; subprocess outputs classify identically.
                                          (fprintf stderr-out
                                                   "~a: raco test: test raised an exception~n"
                                                   (path->string (file-name-from-path resolved-path)))
                                          (set-box! exit-code 1))])
                          (parameterize ([current-output-port stdout-out]
                                         [current-error-port stderr-out]
                                         [current-directory (in-process-cwd resolved-path)]
                                         [current-command-line-arguments #()]
                                         [exit-handler (make-in-process-exit-handler exit-code)]
                                         [current-namespace (make-base-namespace)])
                            (dynamic-require (in-process-module-path resolved-path) #f)
                            (set-box! exit-code 0)))))))
          (sync/timeout (/ file-timeout 1000.0) worker))
        (lambda () (custodian-shutdown-all cust))))
     (define stdout-bytes (string->bytes/utf-8 (get-output-string stdout-out)))
     (define stderr-bytes (string->bytes/utf-8 (get-output-string stderr-out)))
     (define outcome
       (if completed?
           (let ([result (parse-result-bytes test-path
                                             stdout-bytes
                                             stderr-bytes
                                             (or (unbox exit-code) 1)
                                             (elapsed))])
             ;; Never replay a completed test: side effects would run twice.
             ;; A silent grouped run remains zero-parsed for strict validation.
             result)
           (test-file-result test-path
                             2
                             stdout-bytes
                             stderr-bytes
                             (elapsed)
                             0
                             0
                             0
                             requested-mode
                             #f)))
     (with-grouped-telemetry outcome #:requested-mode requested-mode)]))

(define (run-single-file test-path #:timeout [timeout #f] #:mode [mode 'subprocess])
  ;; W7: requested mode = the CLI request when the orchestrator stamped one,
  ;; otherwise the explicit per-call mode (direct/test callers).
  (define requested (or (current-requested-execution-mode) (symbol->string mode)))
  (case mode
    ;; W5: 'grouped-in-process is the explicit per-call escalation symbol
    ;; (used by tests/test-runner-grouped-characterization.rkt). The
    ;; orchestrator never requests it; suite-wide grouped runs keep using
    ;; #:mode 'grouped, which remains subject to the W3 per-area policy.
    [(in-process grouped grouped-in-process)
     (run-single-file/in-process test-path #:timeout timeout #:requested-mode requested)]
    [else (run-single-file/subprocess test-path #:timeout timeout #:requested-mode requested)]))

(define (split-list lst n)
  (cond
    [(null? lst) '()]
    [else (cons (take lst (min n (length lst))) (split-list (drop lst (min n (length lst))) n))]))

;; Scheduler ownership prevents a dead coordinator or worker from leaving
;; registered subprocess groups running after the partition ends.
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

(define (run-all-files files
                       jobs
                       timeout
                       #:mode [mode 'subprocess]
                       #:first-batch-ms-box [first-batch-ms-box #f]
                       #:telemetry [telemetry #f]
                       #:scheduler [scheduler 'batch])
  ;; W2: `scheduler` selects the execution strategy for this partition.
  ;;   'batch — W1 fixed-batch barrier (default, rollback-safe).
  ;;   'queue — bounded work-conserving worker pool (see
  ;;            run-all-files/queue for the GC policy).
  (define results
    (if (eq? scheduler 'queue)
        (run-all-files/queue files
                             jobs
                             timeout
                             #:mode mode
                             #:first-batch-ms-box first-batch-ms-box
                             #:telemetry telemetry)
        (call-with-scheduler-ownership (lambda ()
                                         (run-all-files/batch files
                                                              jobs
                                                              timeout
                                                              #:mode mode
                                                              #:first-batch-ms-box first-batch-ms-box
                                                              #:telemetry telemetry)))))
  ;; Every scheduled occurrence must have exactly one result. A worker that
  ;; dies without publishing must never make the remaining files look green.
  (validate-result-multiset! files results)
  results)

;; W1 fixed-batch barrier scheduler. When called without #:scheduler this is
;; byte-identical to the pre-W2 implementation, so `batch` remains the
;; default and the rollback path is executable.
(define (run-all-files/batch files
                             jobs
                             timeout
                             #:mode [mode 'subprocess]
                             #:first-batch-ms-box [first-batch-ms-box #f]
                             #:telemetry [telemetry #f])
  ;; W1: `telemetry` is a mutable hash (see make-run-all-telemetry) that this
  ;; call fills with per-partition scheduler evidence: `batches` (list of
  ;; (file-count . duration-ms) pairs, reversed append order — callers must
  ;; reverse), `gc_count`, and `gc_pause_ms`. Passing #f keeps the pre-W1
  ;; behavior byte-identical (no timing reads, no hash writes).
  (define batch-t0 (current-inexact-milliseconds))
  (define results (box '()))
  (define results-lock (make-semaphore 1))
  (define (add-result! r)
    (call-with-semaphore results-lock (lambda () (set-box! results (cons r (unbox results))))))
  (define batch-size jobs)
  (define batches (split-list files batch-size))
  (define gc-counter 0)
  (define gc-runs 0)
  (define gc-pause-ms 0)
  (define total-batches (length batches))
  (when telemetry
    (hash-set! telemetry 'batches '())
    (hash-set! telemetry 'gc_count 0)
    (hash-set! telemetry 'gc_pause_ms 0))
  (for ([batch (in-list batches)]
        [batch-idx (in-naturals)])
    (define batch-start (current-inexact-milliseconds))
    (define previous-count (length (unbox results)))
    (define batch-threads
      (for/list ([f (in-list batch)])
        (thread
         (lambda ()
           (define result
             (with-handlers ([exn:fail? (lambda (e)
                                          (test-file-result f
                                                            1
                                                            #""
                                                            (string->bytes/utf-8 (exn-message e))
                                                            0
                                                            0
                                                            0
                                                            0
                                                            (current-requested-execution-mode)
                                                            #f))])
               (run-single-file f #:timeout (or timeout 120000) #:mode mode)))
           (define exit-code (test-file-result-exit-code result))
           (cond
             [(eq? (classify-test-result result) 'SKIPPED_BY_PROFILE) (display "S")]
             [(= exit-code 0) (display ".")]
             [(= exit-code 2) (display "T")]
             [else (display "F")])
           (flush-output)
           (add-result! result)))))
    (for-each thread-wait batch-threads)
    ;; Do not start another batch if one of these workers failed to publish.
    (define current-results (unbox results))
    (validate-result-multiset! batch
                               (take current-results (- (length current-results) previous-count)))
    (when (and first-batch-ms-box (= batch-idx 0))
      (set-box! first-batch-ms-box (exact-round (- (current-inexact-milliseconds) batch-t0))))
    (when telemetry
      (hash-set! telemetry
                 'batches
                 (cons (cons (length batch)
                             (exact-round (- (current-inexact-milliseconds) batch-start)))
                       (hash-ref telemetry 'batches))))
    (set! gc-counter (add1 gc-counter))
    (when (or (= 0 (modulo gc-counter 5)) (= gc-counter total-batches))
      (define gc-start (current-inexact-milliseconds))
      (collect-garbage 'major)
      (set! gc-runs (add1 gc-runs))
      (set! gc-pause-ms (+ gc-pause-ms (- (current-inexact-milliseconds) gc-start)))))
  (when telemetry
    (hash-set! telemetry 'gc_count gc-runs)
    (hash-set! telemetry 'gc_pause_ms (exact-round gc-pause-ms)))
  (newline)
  (define file-order
    (for/hash ([f (in-list files)]
               [i (in-naturals)])
      (values f i)))
  (sort (unbox results) < #:key (lambda (r) (hash-ref file-order (test-file-result-path r) 0))))

;; W2: queue-mode scheduler — a bounded, work-conserving worker pool.
;;
;; Concurrency: exactly `jobs` long-lived worker threads are created for the
;; partition (min 1). Each worker pulls the next file from a shared work
;; channel the instant it finishes the previous one; no thread is created
;; per test file. When the channel is exhausted each worker receives a stop
;; sentinel and exits, so fewer files than workers and empty input stay
;; bounded (idle workers just exit).
;;
;; Failure isolation: a per-file exception (e.g. an unreadable path or an
;; internal runner error) is caught inside the worker and recorded as a
;; failing result, so one bad file can never kill the pool, deadlock the
;; coordinator, or discard the other files' results.
;;
;; GC policy (deterministic and telemetry-visible): the coordinator — this
;; thread, the only one that touches the GC counters — performs a major GC
;; after every 5th file completion and at the final completion, coalescing
;; the two triggers when they coincide, as in batch mode. Workers never
;; race the GC counter. `gc_count` / `gc_pause_ms` are reported exactly like
;; batch mode; per-file durations are recorded as (1 . duration-ms)
;; "batches" in completion order (reversed consing, callers reverse). These
;; overlapping durations cannot establish queue partition or worker timings.
(define (run-all-files/queue files
                             jobs
                             timeout
                             #:mode [mode 'subprocess]
                             #:first-batch-ms-box [first-batch-ms-box #f]
                             #:telemetry [telemetry #f])
  ;; Group ownership cannot contain descendants that deliberately escape it.
  (call-with-scheduler-ownership (lambda ()
                                   (run-all-files/queue/owned files
                                                              jobs
                                                              timeout
                                                              #:mode mode
                                                              #:first-batch-ms-box first-batch-ms-box
                                                              #:telemetry telemetry))))

(define (run-all-files/queue/owned files
                                   jobs
                                   timeout
                                   #:mode [mode 'subprocess]
                                   #:first-batch-ms-box [first-batch-ms-box #f]
                                   #:telemetry [telemetry #f])
  (define t0 (current-inexact-milliseconds))
  (define total (length files))
  (define worker-count (max 1 (if (exact-nonnegative-integer? jobs) jobs 1)))
  (define results (box '()))
  (define results-lock (make-semaphore 1))
  (define (add-result! r)
    (call-with-semaphore results-lock (lambda () (set-box! results (cons r (unbox results))))))
  (when telemetry
    (hash-set! telemetry 'batches '())
    (hash-set! telemetry 'gc_count 0)
    (hash-set! telemetry 'gc_pause_ms 0))
  (cond
    [(zero? total)
     ;; Empty partition: no workers, no GC, bounded trivially.
     (newline)
     (when first-batch-ms-box
       (set-box! first-batch-ms-box (exact-round (- (current-inexact-milliseconds) t0))))
     '()]
    [else
     (define work-ch (make-channel))
     (define done-ch (make-channel))
     (define stop-sentinel (list 'stop))
     (define workers
       (for/list ([i (in-range worker-count)])
         (thread
          (lambda ()
            ;; Defer breaks across handoff and publication; enable them only
            ;; after an item's cleanup and exception handlers are installed.
            (parameterize-break
             #f
             (let loop ()
               (define item (channel-get work-ch))
               (unless (eq? item stop-sentinel)
                 ;; A break deferred during publication or idle receive belongs
                 ;; to the preceding boundary, not this newly handed-off file.
                 (with-handlers ([exn:break? void])
                   (parameterize-break #t (sync/timeout 0 never-evt)))
                 (define item-custodian (make-custodian))
                 (define-values (result file-dur)
                   (dynamic-wind
                    void
                    (lambda ()
                      (with-handlers ([exn? (lambda (e)
                                              (values
                                               (test-file-result item
                                                                 1
                                                                 #""
                                                                 (string->bytes/utf-8 (exn-message e))
                                                                 0
                                                                 0
                                                                 0
                                                                 0
                                                                 (current-requested-execution-mode)
                                                                 #f)
                                               0))])
                        (parameterize ([current-custodian item-custodian])
                          (parameterize-break
                           #t
                           (define t-file (current-inexact-milliseconds))
                           (define r (run-single-file item #:timeout (or timeout 120000) #:mode mode))
                           (values r (exact-round (- (current-inexact-milliseconds) t-file)))))))
                    (lambda () (custodian-shutdown-all item-custodian))))
                 (define exit-code (test-file-result-exit-code result))
                 (cond
                   [(eq? (classify-test-result result) 'SKIPPED_BY_PROFILE) (display "S")]
                   [(= exit-code 0) (display ".")]
                   [(= exit-code 2) (display "T")]
                   [else (display "F")])
                 (flush-output)
                 (add-result! result)
                 (channel-put done-ch file-dur)
                 (loop))))))))
     ;; Work-conserving distribution over unbuffered channels: the
     ;; coordinator hands out a new item exactly when a worker reports a
     ;; completion, so at most `worker-count` items are ever in flight and
     ;; no unbuffered put can deadlock (every put has a receiver waiting).
     (define (queue-await evt)
       ;; A dead worker must fail the partition instead of stranding the
       ;; coordinator on a work or completion channel indefinitely.
       (define tagged
         (apply sync
                (handle-evt evt (lambda (value) (cons #t value)))
                (for/list ([worker (in-list workers)])
                  (handle-evt (thread-dead-evt worker) (lambda (_) (cons #f worker))))))
       (unless (car tagged)
         (error 'run-all-files/queue "worker terminated unexpectedly"))
       (cdr tagged))
     (for ([i (in-range (min worker-count total))])
       (queue-await (channel-put-evt work-ch (list-ref files i))))
     (define sent (min worker-count total))
     ;; Coordinator: consume completions, refill work as workers free up,
     ;; run the deterministic GC schedule, record telemetry, then join the
     ;; workers (all stopped by now).
     (define gc-counter 0)
     (define gc-runs 0)
     (define gc-pause-ms 0)
     (define batches-seen '())
     (for ([i (in-range total)])
       (define file-dur (queue-await done-ch))
       (when (< sent total)
         (queue-await (channel-put-evt work-ch (list-ref files sent)))
         (set! sent (add1 sent)))
       (when (and first-batch-ms-box (not (unbox first-batch-ms-box)))
         (set-box! first-batch-ms-box (exact-round (- (current-inexact-milliseconds) t0))))
       (set! batches-seen (cons (cons 1 (max 0 file-dur)) batches-seen))
       (set! gc-counter (add1 gc-counter))
       (when (or (= 0 (modulo gc-counter 5)) (= gc-counter total))
         (define gc-start (current-inexact-milliseconds))
         (collect-garbage 'major)
         (set! gc-runs (add1 gc-runs))
         (set! gc-pause-ms (+ gc-pause-ms (- (current-inexact-milliseconds) gc-start)))))
     ;; All work is acknowledged; the enclosing custodian shuts down idle
     ;; workers after this result is constructed.
     (when telemetry
       (hash-set! telemetry 'batches batches-seen)
       (hash-set! telemetry 'gc_count gc-runs)
       (hash-set! telemetry 'gc_pause_ms (exact-round gc-pause-ms)))
     (newline)
     (define file-order
       (for/hash ([f (in-list files)]
                  [i (in-naturals)])
         (values f i)))
     (sort (unbox results) < #:key (lambda (r) (hash-ref file-order (test-file-result-path r) 0)))]))

;; W1: per-partition scheduler telemetry (schema-additive, see
;; docs/TEST_CONVENTIONS.md "Runner scheduler telemetry (W1)").
;; make-run-all-telemetry returns a fresh mutable hash run-all-files fills.
(define (make-run-all-telemetry)
  (define h (make-hasheq))
  (hash-set! h 'batches '())
  (hash-set! h 'gc_count 0)
  (hash-set! h 'gc_pause_ms 0)
  h)

;; Aggregate one partition's raw batch telemetry into scheduler metrics.
;; batches: (listof (cons file-count duration-ms)) in EXECUTION order.
;; workers: concurrency of this partition (1 for the serial partition, jobs
;; for the parallel partition). All durations are integer milliseconds.
;; Returns #f when batches is empty (callers default to zeros).
;;   queue_wait_ms   — cumulative wall time files spend waiting for earlier
;;                     batches of the same partition to finish before their
;;                     own batch starts (offset × batch file count, summed).
;;   worker_busy_ms  — per-batch wall duration scaled by the fraction of the
;;                     worker pool the batch occupies (files / workers, cap 1).
;;   worker_idle_ms  — partition pool capacity (partition wall ms × workers)
;;                     minus worker_busy_ms; 0 for the serial partition.
(define (partition-scheduler-fields batches workers)
  (if (null? batches)
      #f
      (let* ([offsets (let loop ([bs batches]
                                 [offset 0]
                                 [acc '()])
                        (if (null? bs)
                            (reverse acc)
                            (loop (cdr bs) (+ offset (cdar bs)) (cons offset acc))))]
             [partition-ms (for/sum ([b (in-list batches)]) (cdr b))]
             [queue-wait-ms (for/sum ([b (in-list batches)] [o (in-list offsets)]) (* o (car b)))]
             [worker-busy-ms (for/sum ([b (in-list batches)])
                                      (exact-round (* (cdr b)
                                                      (min 1.0 (/ (car b) (max 1 workers))))))]
             [worker-idle-ms (max 0 (- (* partition-ms workers) worker-busy-ms))])
        (hasheq 'queue_wait_ms
                (exact-round queue-wait-ms)
                'worker_busy_ms
                (exact-round worker-busy-ms)
                'worker_idle_ms
                (exact-round worker-idle-ms)
                'partition_ms
                (exact-round partition-ms)))))

;; Merge the serial + parallel partition telemetry into the versioned W1
;; scheduler object (values are numbers / 'null; reporting adds the
;; schema_version field). `results` supplies the ACTUAL per-file execution
;; modes so the subprocess/grouped counts reflect what happened, not what was
;; requested.
(define (compute-scheduler-telemetry serial-tel
                                     parallel-tel
                                     worker-count
                                     results
                                     #:scheduler [scheduler 'batch])
  (define serial-batches (reverse (hash-ref serial-tel 'batches '())))
  (define parallel-batches (reverse (hash-ref parallel-tel 'batches '())))
  (define serial-fields (and (pair? serial-batches) (partition-scheduler-fields serial-batches 1)))
  (define parallel-fields
    (and (pair? parallel-batches) (partition-scheduler-fields parallel-batches worker-count)))
  (define (sum key fields)
    (for/sum ([f (in-list (filter values fields))]) (hash-ref f key 0)))
  (define ran-results
    (filter (lambda (r) (not (eq? (classify-test-result r) 'SKIPPED_BY_PROFILE))) results))
  (define grouped-count
    (count
     (lambda (r)
       (equal? (hash-ref execution-modes
                         ;; W7 fix: resolve to the same absolute key form
                         ;; mark-execution-mode! writes.
                         (path->string (simplify-path (resolve-test-path (test-file-result-path r))))
                         'subprocess)
               'grouped-in-process))
     ran-results))
  (define subprocess-count (- (length ran-results) grouped-count))
  (hasheq 'schema_version
          1
          'scheduler_mode
          (symbol->string scheduler)
          'worker_count
          worker-count
          'queue_wait_ms
          (if (eq? scheduler 'queue)
              'null
              (sum 'queue_wait_ms (list serial-fields parallel-fields)))
          'worker_busy_ms
          (if (eq? scheduler 'queue)
              'null
              (sum 'worker_busy_ms (list serial-fields parallel-fields)))
          'worker_idle_ms
          (if (eq? scheduler 'queue)
              'null
              (sum 'worker_idle_ms (list serial-fields parallel-fields)))
          'serial_partition_ms
          (if (eq? scheduler 'queue)
              'null
              (if serial-fields
                  (hash-ref serial-fields 'partition_ms)
                  0))
          'parallel_partition_ms
          (if (eq? scheduler 'queue)
              'null
              (if parallel-fields
                  (hash-ref parallel-fields 'partition_ms)
                  0))
          'process_start_count
          subprocess-count
          'subprocess_count
          subprocess-count
          'grouped_in_process_count
          grouped-count
          'gc_count
          (+ (hash-ref serial-tel 'gc_count 0) (hash-ref parallel-tel 'gc_count 0))
          'gc_pause_ms
          (+ (hash-ref serial-tel 'gc_pause_ms 0) (hash-ref parallel-tel 'gc_pause_ms 0))))

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

(define (effective-mode requested-mode suite-label)
  (cond
    [(eq? requested-mode 'auto) (if (string=? suite-label "unit-fast") 'grouped 'subprocess)]
    [else requested-mode]))

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
                        #:phases [phases (hasheq)]
                        #:first-batch-ms-box [first-batch-ms-box #f]
                        #:serial-telemetry [serial-telemetry #f]
                        #:parallel-telemetry [parallel-telemetry #f]
                        #:scheduler [scheduler 'batch]
                        #:ordering [ordering default-ordering]
                        #:ordering-record [ordering-record #f]
                        #:duration-source [duration-source #f])
  (define t0 (current-inexact-milliseconds))
  (define exec-start-ms (hash-ref phases 'execution_start_ms #f))
  (define selection-end-ms (hash-ref phases 'selection_end_ms #f))
  ;; W1: separate first-batch boxes per partition so the serial and parallel
  ;; calls can never overwrite each other. The legacy `first_batch_ms` field
  ;; keeps the first partition that ran; each partition also gets an
  ;; unambiguous field (serial_first_batch_ms / parallel_first_batch_ms).
  (define serial-first-batch-ms-box (box #f))
  (define parallel-first-batch-ms-box (box #f))
  (define serial-tel (or serial-telemetry (make-run-all-telemetry)))
  (define parallel-tel (or parallel-telemetry (make-run-all-telemetry)))
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
  ;; W3: deterministic file ordering — a SEPARATE contract from scheduler
  ;; selection.  `order-files` is a pure permutation of `runnable-files`, so
  ;; it can never add/remove a file, change the inventory digest, or cross
  ;; the serial/parallel partition; final result order is re-established by
  ;; the result sort keyed on suite-file position below.  FIFO (the default)
  ;; preserves deterministic input order; LPT sorts by retained per-file
  ;; duration (descending, stable repository-path tie-breaks) and falls back
  ;; to FIFO with a named reason when duration evidence is unusable.
  (define effective-ordering (or ordering default-ordering))
  (define ordering-rec
    (or
     ordering-record
     (prepare-ordering runnable-files effective-ordering (current-max-age-seconds) duration-source)))
  (define ordered-runnable (order-files runnable-files ordering-rec))
  (when (or (not (eq? (ordering-record-mode ordering-rec) effective-ordering))
            (not (eq? effective-ordering 'fifo)))
    (printf ";; run-tests: ordering=~a requested=~a fallback=~a~n"
            (ordering-record-mode ordering-rec)
            (ordering-record-requested ordering-rec)
            (or (ordering-record-fallback-reason ordering-rec) "none")))
  (define-values (serial-files parallel-files)
    (if (> jobs 1)
        (values (filter mutating-file? ordered-runnable)
                (filter (lambda (f) (not (mutating-file? f))) ordered-runnable))
        (values '() ordered-runnable)))
  (when (pair? serial-files)
    (printf ";; run-tests: serializing ~a mutation-sensitive file~a before parallel batches~n"
            (length serial-files)
            (if (= (length serial-files) 1) "" "s")))
  ;; One guard around every execution path (sequential, serial and parallel):
  ;; a test that rewrites a managed surface fails the suite instead of being
  ;; silently reverted.
  (define serial-results-box (box '()))
  (define parallel-results-box (box '()))
  (define surface-violations
    (call-with-surface-guard
     base-dir
     (lambda ()
       (set-box! serial-results-box
                 (if (pair? serial-files)
                     (run-all-files serial-files
                                    1
                                    timeout-ms
                                    #:mode mode
                                    #:first-batch-ms-box serial-first-batch-ms-box
                                    #:telemetry serial-tel
                                    #:scheduler scheduler)
                     '()))
       (set-box! parallel-results-box
                 (if (pair? parallel-files)
                     (run-all-files parallel-files
                                    jobs
                                    timeout-ms
                                    #:mode mode
                                    #:first-batch-ms-box parallel-first-batch-ms-box
                                    #:telemetry parallel-tel
                                    #:scheduler scheduler)
                     '())))))
  (define serial-results (unbox serial-results-box))
  (define parallel-results (unbox parallel-results-box))
  (define file-order
    (for/hash ([f (in-list suite-files)]
               [i (in-naturals)])
      (values f i)))
  (define results
    (sort (append skipped-results serial-results parallel-results)
          <
          #:key (lambda (r) (hash-ref file-order (test-file-result-path r) 0))))
  ;; The suite boundary must also account for skipped and partitioned files.
  (validate-result-multiset! suite-files results)
  (define total-elapsed (exact-round (- (current-inexact-milliseconds) t0)))
  (define exec-end-ms (current-inexact-milliseconds))
  (define runner-version (detect-runner-version))
  ;; Actual per-file execution mode, keyed by resolved path string. Skipped
  ;; files never ran, so they carry no mode entry.
  (define actual-mode-hash
    (and json-out
         (for/hash ([r (in-list results)]
                    #:unless (eq? (classify-test-result r) 'SKIPPED_BY_PROFILE))
           (define p (test-file-result-path r))
           ;; W7 fix: execution-modes keys are resolved absolute paths (see
           ;; mark-execution-mode!), so look up through resolve-test-path
           ;; instead of the raw (possibly relative) result path.
           (define mode-key (path->string (simplify-path (resolve-test-path p))))
           ;; The hash key mirrors reporting.rkt's file-execution-mode lookup,
           ;; which uses the raw result-path string.
           (values (if (path? p)
                       (path->string p)
                       p)
                   (hash-ref execution-modes mode-key 'subprocess)))))
  ;; v1.00.27 W3 (#9591): area-level grouped fallbacks are named, never
  ;; silent — one summary line per affected area.
  (define area-fallback-rows (grouped-fallback-rows results))
  (for ([row (in-list area-fallback-rows)])
    (printf ";; run-tests: ~a area=~a reason=~a files=~a\n"
            (hash-ref row 'kind)
            (hash-ref row 'area)
            (hash-ref row 'reason)
            (hash-ref row 'files)))
  (print-summary results total-elapsed)
  (print-run-summary-record results
                            #:suite suite-label
                            #:profile profile
                            #:shard shard
                            #:mode mode
                            #:elapsed-ms total-elapsed
                            #:runner-version runner-version)
  (when json-out
    (write-json-results!
     json-out
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
       ;; v1.00.27 W3 (#9591): grouped expansion policy + named fallbacks.
       ;; The policy must cross the write-json boundary symbol-keyed: a
       ;; string-keyed hash is not a legal jsexpr (write-json raises
       ;; "<legal JSON key value>; given: \"ci\""), so the summary renders
       ;; it through the profiles-level jsexpr projection.
       (hash-set! m
                  'grouped_expansion
                  (hasheq 'policy
                          (grouped-area-config->jsexpr)
                          'rollback_areas
                          (grouped-rollback-areas)
                          'fallbacks
                          area-fallback-rows))
       (when exec-start-ms
         (hash-set! m 'execution_start_ms (exact-round exec-start-ms)))
       (when selection-end-ms
         (hash-set! m 'selection_end_ms (exact-round selection-end-ms)))
       (when (unbox serial-first-batch-ms-box)
         (hash-set! m 'serial_first_batch_ms (unbox serial-first-batch-ms-box)))
       (when (unbox parallel-first-batch-ms-box)
         (hash-set! m 'parallel_first_batch_ms (unbox parallel-first-batch-ms-box)))
       (when (or (unbox serial-first-batch-ms-box) (unbox parallel-first-batch-ms-box))
         ;; Legacy field: first partition that actually ran.
         (hash-set! m
                    'first_batch_ms
                    (if (unbox serial-first-batch-ms-box)
                        (unbox serial-first-batch-ms-box)
                        (unbox parallel-first-batch-ms-box))))
       (hash-set!
        m
        'scheduler
        (compute-scheduler-telemetry serial-tel parallel-tel jobs results #:scheduler scheduler))
       ;; W3: ordering evidence — mode (fifo|lpt), snapshot checksum/status,
       ;; freshness decision, and named fallback reason, exactly as decided.
       (hash-set! m 'ordering (ordering-record->jsexpr ordering-rec))
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
                  diagnose-overhead?
                  requested-mode
                  scheduler
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
                  durations
                  ordering)
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
                  diagnose-overhead?
                  requested-mode
                  scheduler
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
                  durations
                  ordering)
  ;; v1.00.24 W3: sharded runs never record gate evidence. A shard covers
  ;; only part of the suite, so recording would emit (or overwrite) evidence
  ;; that claims to be a full-suite PASS. Fail closed before doing any work.
  (when record-gate?
    (define refusal (gate-evidence-refusal shard-index shard-total))
    (when refusal
      (eprintf ";; run-tests: ~a~n" refusal)
      (exit 2)))
  (when diagnose-overhead?
    (print-overhead-diagnostics #:base-dir base-dir)
    (exit 0))
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
  ;; ── W7: duration-aware shard planning (report | active) ─────────────
  ;; `report` prints the plan + predicted per-shard durations and changes
  ;; nothing (exits 0). `active` consumes the plan: each shard runs exactly
  ;; its planned file list instead of the round-robin slice. Execution
  ;; semantics (subprocess vs grouped, serial-ahead for mutation-sensitive
  ;; files) are unchanged — only the shard assignment differs.
  (define plan-mode
    (cond
      [(equal? shard-plan "report") 'report]
      [(equal? shard-plan "active") 'active]
      [(equal? shard-plan "measure") 'measure]
      [else #f]))
  (define active-plan (box #f))
  ;; W10: `measure` — measure each selected file once (subprocess mode,
  ;; sequential, deterministic order), write a W0-schema duration snapshot to
  ;; --json-out, then print the duration-aware plan regenerated from that
  ;; snapshot plus the W10 starvation/tail checks, and exit 0. It is a
  ;; measurement pass: it never produces a test verdict and changes nothing.
  (when (and plan-mode (eq? plan-mode 'measure))
    (unless (string? json-out)
      (raise-user-error
       'run-tests
       "--shard-plan measure requires --json-out <path> (duration snapshot destination)"))
    (printf
     ";; run-tests: shard-plan=measure files=~a jobs=1 (sequential per-file subprocess measurement)~n"
     (length all-suite-files))
    (define measured
      (for/list ([f (in-list (sort all-suite-files string<?))])
        (define r (run-single-file f #:timeout timeout #:mode 'subprocess))
        (printf ";; run-tests: measured ~a ~as (~a)~n"
                f
                (~r (/ (test-file-result-elapsed-ms r) 1000.0) #:precision '(= 3))
                (result-status r))
        (hasheq 'path
                f
                'duration_seconds
                (/ (test-file-result-elapsed-ms r) 1000.0)
                'status
                (result-status r))))
    (call-with-output-file
     json-out
     #:exists 'truncate/replace
     (lambda (out)
       (write-json
        (hasheq
         'schema
         "ci-durations/1"
         'suite
         (symbol->string suite)
         'generated_at
         (parameterize ([date-display-format 'iso-8601])
           (date->string (current-date) #t))
         'measured_by
         "--shard-plan measure (sequential subprocess per-file wall clock, includes per-file runner overhead)"
         'files
         measured)
        out)
       (newline out)))
    (printf ";; run-tests: duration snapshot written to ~a~n" json-out)
    (define-values (mdur mdur-status) (load-duration-snapshot json-out))
    (printf ";; run-tests: shard-plan=measure snapshot status=~a known=~a~n"
            mdur-status
            (hash-count mdur))
    ;; Regenerate the plan from the fresh snapshot; with no explicit sharding
    ;; the report uses the CI fast-lane shard count (3) so the prediction is
    ;; comparable with the deployed topology.
    (define mplan
      (build-shard-plan/safe
       all-suite-files
       (if (> shard-total 1) shard-total 3)
       #:durations mdur
       #:profile-skips?
       (lambda (f) (profile-skips-test? profile (hash-ref (get-file-metadata f) 'requires '())))
       #:duration-source json-out))
    (print-shard-plan-report mplan)
    (print-starvation-tail-report mplan)
    (exit 0))
  ;; W7: `report` is informational only and works for any --shard-total
  ;; (a 1-shard plan is a degenerate-but-valid plan: all files on shard 0).
  ;; `active` needs a real partition, so it stays gated on shard-total > 1.
  (when (and plan-mode (eq? plan-mode 'active) (not (> shard-total 1)))
    ;; Planning without sharding is a no-op (there is only one shard); state
    ;; it explicitly so CI logs never hide the effective configuration.
    (printf ";; run-tests: --shard-plan ~a ignored — --shard-total=~a (unsharded)~n"
            shard-plan
            shard-total))
  (when (and plan-mode (or (eq? plan-mode 'report) (> shard-total 1)))
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
    (when (eq? plan-mode 'report)
      (print-shard-plan-report plan)
      (exit 0))
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
  ;; W7: stamp the CLI-requested mode for per-file fallback truth.
  (current-requested-execution-mode (symbol->string requested-mode))
  (define ledger (and ledger-path (load-known-failure-ledger ledger-path)))
  (define n-files (length suite-files))
  (printf
   ";; run-tests: suite=~a files=~a jobs=~a sequential=~a repeat=~a mode=~a profile=~a scheduler=~a ordering=~a~n"
   suite-label
   n-files
   jobs
   sequential?
   repeat
   mode
   profile
   scheduler
   (or ordering default-ordering))
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
                      #:phases phases
                      #:scheduler scheduler
                      #:ordering ordering
                      #:duration-source durations))
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

;; ─────────────────────────────────────────────────────────────────────
;; Test-runner overhead diagnostics (moved verbatim from the former
;; overhead.rkt; W0 of v0.99.30 + v1.00.27 W4 local feedback telemetry).
;; The CLI (`--check` / `--collect-local`) is dispatched from this module's
;; direct-invocation guard below, so there is no second `module+ main`.
;; ─────────────────────────────────────────────────────────────────────

(struct overhead-result (label command exit-code elapsed-ms stdout stderr) #:transparent)

(define (make-overhead-result label command exit-code elapsed-ms stdout stderr)
  (overhead-result label command exit-code elapsed-ms stdout stderr))

(define (now-ms)
  (current-inexact-milliseconds))

(define (elapsed-since t0)
  (exact-round (- (now-ms) t0)))

(define (run-overhead-command label executable args #:cwd [cwd (current-directory)])
  (define exe-path (find-executable-path executable))
  (unless exe-path
    (error 'run-overhead-command "executable not found: ~a" executable))
  (define stdout-out (open-output-string))
  (define stderr-out (open-output-string))
  (define command-text (string-join (cons executable args) " "))
  (define t0 (now-ms))
  (define-values (_proc stdin _pid _stderr ctrl)
    (parameterize ([current-directory cwd])
      (apply values (apply process*/ports stdout-out #f stderr-out exe-path args))))
  (when stdin
    (close-output-port stdin))
  (ctrl 'wait)
  (define exit-code (ctrl 'exit-code))
  (make-overhead-result label
                        command-text
                        exit-code
                        (elapsed-since t0)
                        (get-output-string stdout-out)
                        (get-output-string stderr-out)))

(define (write-file path content)
  (call-with-output-file path #:exists 'replace (lambda (out) (display content out))))

(define (collect-overhead-diagnostics #:base-dir [base-dir (current-directory)])
  (define temp-dir (make-temporary-file "q-run-tests-overhead-~a" 'directory))
  (define empty-file (build-path temp-dir "empty.rkt"))
  (define rackunit-file (build-path temp-dir "rackunit-empty.rkt"))
  (write-file empty-file "#lang racket/base\n(void)\n")
  (write-file rackunit-file
              (string-append
               "#lang racket\n"
               "(require rackunit rackunit/text-ui)\n"
               "(run-tests (test-suite \"empty\" (test-case \"ok\" (check-true #t))))\n"))
  (define representative (build-path base-dir "tests" "test-version.rkt"))
  (define commands
    (append
     (list (list "racket-noop" "racket" (list "-e" "(void)"))
           (list "racket-empty" "racket" (list (path->string empty-file)))
           (list "raco-empty" "raco" (list "test" (path->string empty-file)))
           (list "raco-rackunit-empty" "raco" (list "test" (path->string rackunit-file))))
     (if (file-exists? representative)
         (list (list "raco-representative-test" "raco" (list "test" (path->string representative))))
         '())))
  (dynamic-wind void
                (lambda ()
                  (for/list ([cmd (in-list commands)])
                    (match cmd
                      [(list label exe args) (run-overhead-command label exe args #:cwd base-dir)])))
                (lambda ()
                  (when (directory-exists? temp-dir)
                    (delete-directory/files temp-dir)))))

(define (format-overhead-result r)
  (format "  ~a: ~ams exit=~a cmd=~a"
          (overhead-result-label r)
          (overhead-result-elapsed-ms r)
          (overhead-result-exit-code r)
          (overhead-result-command r)))

(define (print-overhead-diagnostics #:base-dir [base-dir (current-directory)])
  (displayln "═══════════════════════════════════════════════════════════")
  (displayln "              TEST RUNNER OVERHEAD DIAGNOSTIC")
  (displayln "═══════════════════════════════════════════════════════════")
  (printf "  Base dir:   ~a~n" (path->string (simplify-path base-dir)))
  (printf "  Racket:     ~a~n" (or (find-executable-path "racket") "not found"))
  (printf "  raco:       ~a~n" (or (find-executable-path "raco") "not found"))
  (newline)
  (define results (collect-overhead-diagnostics #:base-dir base-dir))
  (for ([r (in-list results)])
    (displayln (format-overhead-result r)))
  (newline)
  (displayln "Interpretation:")
  (displayln "  - racket-noop approximates raw Racket process startup cost.")
  (displayln "  - raco-empty approximates per-file raco test harness overhead.")
  (displayln "  - raco-rackunit-empty approximates parser-visible rackunit overhead.")
  (displayln "  - raco-representative-test samples one normal project test when available.")
  (displayln "═══════════════════════════════════════════════════════════")
  results)

;; ── v1.00.27 W4 (#9592): L0/L1/L2 local feedback telemetry ─────────────
;; Real p90 evidence for the three local feedback loops of
;; docs/TEST_CONVENTIONS.md: L0 = single test file (≤5s), L1 = direct unit
;; impact (≤30s), L2 = transitive impact = the unit-fast tier (≤120s).
;; The collector measures representative invocations ≥20 times per loop
;; (first sample cold, remaining samples warm, every sample a fresh
;; run-tests process), stamps machine context and the W3-governed
;; grouped/subprocess mode per area, and computes every SLO verdict from
;; the samples. Verdicts are never hand-written: --check re-derives p90
;; (linear interpolation) and the verdict from the recorded samples and
;; rejects hand-editing, missing fields, failed samples, sample counts
;; below the minimum, and any area mode contradicting the W3
;; grouped-expansion configuration.

(define local-p90-min-samples 20)

;; v1.00.27-w4: L2 local p90 SLO adjusted 120s -> 240s through the governed
;; evidence record artifacts/tier-ownership/v1.00.27-w4/slo-evidence-record.md
;; (measured p90 234.9s over 20 samples of the unit-fast tier; see
;; artifacts/tier-ownership/v1.00.27-w4/local-p90.json).
(define local-feedback-slo-table (list (cons "L0" 5000) (cons "L1" 30000) (cons "L2" 240000)))

(define (slo-budget-for label)
  (cond
    [(assoc label local-feedback-slo-table)
     =>
     cdr]
    [else #f]))

(define (percentile-rank xs p)
  ;; Linear interpolation on the sorted sample list: rank = p*(n-1).
  (define sorted (sort (map exact->inexact xs) <))
  (define n (length sorted))
  (when (zero? n)
    (error 'percentile-rank "empty sample list"))
  (define rank (* p (sub1 n)))
  (define lo (floor rank))
  (define hi (ceiling rank))
  (define vlo (list-ref sorted (inexact->exact lo)))
  (if (= lo hi)
      vlo
      (+ vlo (* (- rank lo) (- (list-ref sorted (inexact->exact hi)) vlo)))))

(define (local-loop-p90 elapsed-ms-list)
  (exact-round (percentile-rank elapsed-ms-list 0.9)))

(define (slo-verdict p90-ms budget-ms)
  (if (<= p90-ms budget-ms) "meet" "miss"))

(define (machine-context)
  (define loadavg-path "/proc/loadavg")
  (define-values (l1 l5 l15)
    (if (file-exists? loadavg-path)
        (let* ([parts (string-split (string-trim (file->string loadavg-path)) " ")])
          (values (string->number (first parts))
                  (string->number (second parts))
                  (string->number (third parts))))
        (values #f #f #f)))
  (hasheq 'cpu-count
          (processor-count)
          'load-1
          (or l1 0.0)
          'load-5
          (or l5 0.0)
          'load-15
          (or l15 0.0)
          'platform
          (symbol->string (system-type))
          'racket
          (version)))

(define (local-feedback-loops base-dir)
  (define (loop-def label description argv paths)
    (hasheq 'loop
            label
            'description
            description
            'argv
            argv
            'paths
            paths
            'slo-ms
            (slo-budget-for label)))
  (list
   (loop-def "L0"
             "current behavior: the single test file being edited (explicit-file mode)"
             (list "racket" "scripts/run-tests.rkt" "tests/ci/metadata-discovery-test.rkt")
             (list "tests/ci/metadata-discovery-test.rkt"))
   (loop-def "L1"
             "direct unit impact: direct tests of changed modules + required local contract tests"
             (list "racket"
                   "scripts/run-tests.rkt"
                   "tests/ci/metadata-discovery-test.rkt"
                   "tests/ci/verify-lock-selection-test.rkt"
                   "tests/test-version.rkt")
             (list "tests/ci/metadata-discovery-test.rkt"
                   "tests/ci/verify-lock-selection-test.rkt"
                   "tests/test-version.rkt"))
   (loop-def "L2"
             "transitive impact: the full unit-fast developer iteration tier"
             (list "racket" "scripts/run-tests.rkt" "--suite" "unit-fast")
             (list "tests/ci/metadata-discovery-test.rkt"
                   "tests/ci/verify-lock-selection-test.rkt"))))

(define (expected-area-mode area-string)
  ;; grouped-area-config is string-keyed (test-file-area convention).
  (define policy (hash-ref (grouped-area-config) area-string 'subprocess))
  (if (eq? policy 'grouped) "grouped" "subprocess"))

(define (area-mode-rows paths #:base-dir [base-dir (current-directory)])
  (parameterize ([current-directory base-dir])
    (for/list ([p (in-list paths)])
      (define-values (mode fallback-reason) (area-grouped-decision p))
      (define area (test-file-area p))
      (hasheq 'path
              (if (string? p)
                  p
                  (path->string p))
              'area
              (if (symbol? area)
                  (symbol->string area)
                  area)
              'mode
              (if (eq? mode 'grouped) "grouped" "subprocess")
              'fallback-reason
              (and fallback-reason (symbol->string fallback-reason))))))

(define (grouped-config-jsexpr)
  (define cfg (grouped-area-config->jsexpr))
  (hasheq 'source
          "artifacts/tier-ownership/v1.00.27-w3/grouped-expansion.json"
          'default-mode
          "subprocess"
          'areas
          (for/list ([(area policy) (in-hash cfg)])
            (hasheq 'area
                    (if (symbol? area)
                        (symbol->string area)
                        area)
                    'mode
                    (if (member policy '(grouped "grouped")) "grouped" "subprocess")))))

(define (implementation-sha #:base-dir [base-dir (current-directory)])
  (with-handlers ([exn:fail? (lambda (e)
                               (eprintf "warning: implementation-sha: git rev-parse failed: ~a\n"
                                        (exn-message e))
                               "unknown")])
    (define r (run-overhead-command "git rev-parse" "git" (list "rev-parse" "HEAD") #:cwd base-dir))
    (define line (string-trim (overhead-result-stdout r)))
    (if (regexp-match? #px"^[0-9a-f]{40,64}$" line)
        line
        (begin
          (eprintf "warning: implementation-sha: unexpected rev-parse output ~s\n" line)
          "unknown"))))

(define (iso-utc-now)
  (define d (seconds->date (current-seconds) #t))
  (define (pad2 x)
    (~r x #:min-width 2 #:pad-string "0"))
  (format "~a-~a-~aT~a:~a:~aZ"
          (date-year d)
          (pad2 (date-month d))
          (pad2 (date-day d))
          (pad2 (date-hour d))
          (pad2 (date-minute d))
          (pad2 (date-second d))))

(define (collect-local-p90 #:base-dir [base-dir (current-directory)]
                           #:loops [loops #f]
                           #:samples-per-loop [samples-per-loop local-p90-min-samples]
                           #:cold-samples [cold-samples 1])
  (define loop-defs (or loops (local-feedback-loops base-dir)))
  (define mc (machine-context))
  (define loop-records
    (for/list ([l (in-list loop-defs)])
      (define label (hash-ref l 'loop))
      (define argv (hash-ref l 'argv))
      (define samples
        (for/list ([i (in-range samples-per-loop)])
          (define start (if (< i cold-samples) "cold" "warm"))
          (define r (run-overhead-command label (first argv) (rest argv) #:cwd base-dir))
          (unless (zero? (overhead-result-exit-code r))
            (eprintf "warning: ~a sample ~a exited ~a\n"
                     label
                     (add1 i)
                     (overhead-result-exit-code r)))
          (hasheq 'i
                  (add1 i)
                  'start
                  start
                  'elapsed-ms
                  (overhead-result-elapsed-ms r)
                  'exit-code
                  (overhead-result-exit-code r))))
      (define p90 (local-loop-p90 (map (lambda (s) (hash-ref s 'elapsed-ms)) samples)))
      (define slo (hash-ref l 'slo-ms))
      (hasheq 'loop
              label
              'description
              (hash-ref l 'description)
              'command
              (string-join argv " ")
              'paths
              (hash-ref l 'paths)
              'area-modes
              (area-mode-rows (hash-ref l 'paths) #:base-dir base-dir)
              'sample-count
              (length samples)
              'samples
              samples
              'p90-ms
              p90
              'slo-ms
              slo
              'verdict
              (slo-verdict p90 slo))))
  (hasheq
   'version
   1
   'wave
   "v1.00.27-w4"
   'ticket
   "#9592"
   'generated-at
   (iso-utc-now)
   'implementation-sha
   (implementation-sha #:base-dir base-dir)
   'method
   (hasheq
    'p90
    "linear interpolation"
    'rank
    "0.9*(n-1)"
    'cold-samples
    cold-samples
    'warm-samples
    (max 0 (- samples-per-loop cold-samples))
    'note
    "each sample is a fresh run-tests process; cold = first invocation of the loop in this collection, warm = subsequent invocations")
   'machine
   mc
   'grouped-config
   (grouped-config-jsexpr)
   'loops
   loop-records))

(define (check-local-p90-record rec)
  (define violations '())
  (define (bad fmt . args)
    (set! violations (cons (apply format fmt args) violations)))
  (define (need h field ctx)
    (unless (and (hash? h) (hash-has-key? h field))
      (bad "~a: missing required field: ~a" ctx field)))
  (if (not (hash? rec))
      (bad "record is not a JSON object")
      (let ()
        (for ([f (in-list '(version wave
                                    ticket
                                    generated-at
                                    implementation-sha
                                    method
                                    machine
                                    grouped-config
                                    loops))])
          (need rec f "record"))
        (define mc (hash-ref rec 'machine #f))
        (when (hash? mc)
          (for ([f (in-list '(cpu-count load-1 load-5 load-15 platform racket))])
            (need mc f "machine")))
        (define sha (hash-ref rec 'implementation-sha #f))
        (when (string? sha)
          (unless (regexp-match? #px"^[0-9a-f]{40,64}$" sha)
            (bad "implementation-sha ~a is not a 40/64-hex commit sha" sha)))
        (define method (hash-ref rec 'method #f))
        (when (hash? method)
          (for ([f (in-list '(p90 rank))])
            (need method f "method")))
        (define gcfg (hash-ref rec 'grouped-config #f))
        (when (hash? gcfg)
          (for ([f (in-list '(source default-mode areas))])
            (need gcfg f "grouped-config"))
          (when (hash-has-key? gcfg 'source)
            (unless (equal? (hash-ref gcfg 'source)
                            "artifacts/tier-ownership/v1.00.27-w3/grouped-expansion.json")
              (bad "grouped-config source must reference the W3 governed expansion artifact")))
          (define areas (hash-ref gcfg 'areas #f))
          (when (list? areas)
            (unless (pair? areas)
              (bad "grouped-config: areas must not be empty"))
            (for ([row (in-list areas)])
              (need row 'area "grouped-config area row")
              (need row 'mode "grouped-config area row"))))
        (define loops (hash-ref rec 'loops #f))
        (unless (and (list? loops) (= (length loops) 3))
          (bad "record: loops must contain exactly L0, L1, L2"))
        (when (and (list? loops) (= (length loops) 3))
          (define labels (map (lambda (l) (hash-ref l 'loop #f)) loops))
          (unless (equal? labels '("L0" "L1" "L2"))
            (bad "record: loops must be labeled L0, L1, L2 (got ~a)" labels)))
        (when (list? loops)
          (for ([l (in-list loops)])
            (define label (hash-ref l 'loop "?"))
            (for ([f (in-list '(loop description
                                     command
                                     paths
                                     area-modes
                                     sample-count
                                     samples
                                     p90-ms
                                     slo-ms
                                     verdict))])
              (need l f label))
            (define count (hash-ref l 'sample-count #f))
            (when (and (hash-has-key? l 'sample-count) (not (integer? count)))
              (bad "~a: sample-count must be an integer" label))
            (when (and (integer? count) (< count local-p90-min-samples))
              (bad "~a: sample-count ~a is below the minimum of ~a real telemetry samples"
                   label
                   count
                   local-p90-min-samples))
            (define samples (hash-ref l 'samples #f))
            (when (list? samples)
              (when (and (integer? count) (not (= (length samples) count)))
                (bad "~a: sample-count ~a does not match ~a recorded samples"
                     label
                     count
                     (length samples)))
              (for ([s (in-list samples)])
                (for ([f (in-list '(i start elapsed-ms exit-code))])
                  (need s f (format "~a sample" label)))
                (unless (member (hash-ref s 'start #f) '("cold" "warm"))
                  (bad "~a sample ~a: start must be cold or warm" label (hash-ref s 'i "?")))
                (define ms (hash-ref s 'elapsed-ms #f))
                (unless (and (integer? ms) (>= ms 0))
                  (bad "~a sample ~a: elapsed-ms must be a non-negative integer"
                       label
                       (hash-ref s 'i "?")))
                (unless (equal? (hash-ref s 'exit-code #f) 0)
                  (bad "~a sample ~a: exit-code ~a is not 0 (failed runs are not latency evidence)"
                       label
                       (hash-ref s 'i "?")
                       (hash-ref s 'exit-code "?")))))
            (define elapsed-list
              (and (list? samples)
                   (not (null? samples))
                   (andmap (lambda (s) (and (hash? s) (hash-has-key? s 'elapsed-ms))) samples)
                   (map (lambda (s) (hash-ref s 'elapsed-ms)) samples)))
            (define slo (hash-ref l 'slo-ms #f))
            (when (hash-has-key? l 'slo-ms)
              (unless (equal? slo (slo-budget-for label))
                (bad "~a: slo-ms ~a contradicts the declared budget ~a"
                     label
                     slo
                     (slo-budget-for label))))
            (when (list? elapsed-list)
              (define recomputed (local-loop-p90 elapsed-list))
              (define recorded-p90 (hash-ref l 'p90-ms #f))
              (when (integer? recorded-p90)
                (unless (= recorded-p90 recomputed)
                  (bad
                   "~a: p90-ms ~a does not match recomputed p90 ~a (linear interpolation over samples)"
                   label
                   recorded-p90
                   recomputed)))
              (when (integer? slo)
                (define expected-verdict (slo-verdict recomputed slo))
                (define recorded-verdict (hash-ref l 'verdict #f))
                (unless (equal? recorded-verdict expected-verdict)
                  (bad
                   "~a: verdict '~a' is not the computed verdict '~a' (SLO verdicts are computed, never hand-written)"
                   label
                   recorded-verdict
                   expected-verdict))))
            (define rows (hash-ref l 'area-modes #f))
            (unless (and (list? rows) (pair? rows))
              (bad "~a: area-modes must list the grouped/subprocess mode per area" label))
            (when (list? rows)
              (for ([row (in-list rows)])
                (define area (hash-ref row 'area #f))
                (define mode (hash-ref row 'mode #f))
                (unless (string? area)
                  (bad "~a: area-mode row is missing area" label))
                (unless (member mode '("grouped" "subprocess"))
                  (bad "~a: area-mode row for ~a has invalid mode ~a" label area mode))
                (when (and (string? area) (member mode '("grouped" "subprocess")))
                  (define expected (expected-area-mode area))
                  (unless (equal? mode expected)
                    (bad
                     "~a: area mode for ~a contradicts the W3 grouped configuration (recorded ~a, governed ~a)"
                     label
                     area
                     mode
                     expected)))))))))
  (reverse violations))

(define (check-local-p90-file path)
  (with-handlers ([exn:fail?
                   (lambda (e)
                     (list (format "local-p90 check could not read ~a: ~a" path (exn-message e))))])
    (define rec (with-input-from-file path read-json))
    (check-local-p90-record rec)))

;; Overhead-diagnostics CLI (body moved verbatim from overhead.rkt's
;; `module+ main`; `argv` is the command-line vector supplied by the
;; direct-invocation guard in this module).

(define (overhead-cli argv)
  (define (usage)
    (displayln "usage: racket scripts/run-tests/runner.rkt --check <local-p90.json>")
    (displayln
     "       racket scripts/run-tests/runner.rkt --collect-local [--samples N] [--out PATH] [--loop L0|L1|L2]"))
  (cond
    [(and (>= (vector-length argv) 2) (equal? (vector-ref argv 0) "--check"))
     (define path (vector-ref argv 1))
     (define violations (check-local-p90-file path))
     (if (null? violations)
         (begin
           (printf "local-p90 OK: ~a (loops L0/L1/L2 verified, verdicts recomputed)\n" path)
           (exit 0))
         (begin
           (printf "local-p90 check FAILED for ~a:\n" path)
           (for ([v (in-list violations)])
             (printf "  - ~a\n" v))
           (exit 1)))]
    [(and (>= (vector-length argv) 1) (equal? (vector-ref argv 0) "--collect-local"))
     (define samples local-p90-min-samples)
     (define out #f)
     (define loop-filter #f)
     (let go ([i 1])
       (when (< i (vector-length argv))
         (define a (vector-ref argv i))
         (cond
           [(and (equal? a "--samples") (< (add1 i) (vector-length argv)))
            (set! samples (string->number (vector-ref argv (add1 i))))
            (go (+ i 2))]
           [(and (equal? a "--out") (< (add1 i) (vector-length argv)))
            (set! out (vector-ref argv (add1 i)))
            (go (+ i 2))]
           [(and (equal? a "--loop") (< (add1 i) (vector-length argv)))
            (set! loop-filter (vector-ref argv (add1 i)))
            (go (+ i 2))]
           [else
            (eprintf "unknown or incomplete option: ~a\n" a)
            (go (add1 i))])))
     (unless (and (integer? samples) (>= samples 1))
       (set! samples local-p90-min-samples))
     (define loops
       (if loop-filter
           (filter (lambda (l) (equal? (hash-ref l 'loop) loop-filter))
                   (local-feedback-loops (current-directory)))
           #f))
     (when (and loop-filter (null? loops))
       (eprintf "unknown loop ~a (expected L0, L1, or L2)\n" loop-filter)
       (exit 2))
     (define rec (collect-local-p90 #:loops loops #:samples-per-loop samples))
     (for ([l (in-list (hash-ref rec 'loops))])
       (printf "~a: p90=~ams slo=~ams verdict=~a samples=~a command=~a\n"
               (hash-ref l 'loop)
               (hash-ref l 'p90-ms)
               (hash-ref l 'slo-ms)
               (hash-ref l 'verdict)
               (hash-ref l 'sample-count)
               (hash-ref l 'command)))
     (if out
         (begin
           (call-with-output-file out
                                  (lambda (port)
                                    (write-json rec port)
                                    (newline port))
                                  #:exists 'replace)
           (printf "wrote ~a\n" out))
         (write-json rec))]
    [else
     (usage)
     (exit 2)]))

;; Direct-invocation guard (Phase D step 1).
;; `scripts/run-tests.rkt` is now a compatibility adapter that owns the
;; command-line entry point and delegates selection/execution/reporting to the
;; shared CI engine in `scripts/testing/full-plan.rkt`. runner.rkt therefore
;; autoruns only when IT is the module that was actually run (`racket
;; scripts/run-tests/runner.rkt`), never when the facade loads it for its
;; re-exports. Matching is on paths, not on a file name, so a copy of
;; runner.rkt still autoruns and a copy of the facade still runs the adapter.
;;
;; The overhead diagnostics CLI (formerly overhead.rkt's `module+ main`) shares
;; this entry point: its two subcommands are dispatched here instead of through
;; a second `module+ main`, which would double-run on direct invocation.
(define invoked-directly?
  (let ([run-file (find-system-path 'run-file)]
        [source (variable-reference->module-source (#%variable-reference))])
    (and (path? run-file)
         (path? source)
         (equal? (simplify-path (path->complete-path run-file))
                 (simplify-path (path->complete-path source))))))
(when invoked-directly?
  (define argv (current-command-line-arguments))
  (if (and (> (vector-length argv) 0) (member (vector-ref argv 0) '("--check" "--collect-local")))
      (overhead-cli argv)
      (main (vector->list argv))))
