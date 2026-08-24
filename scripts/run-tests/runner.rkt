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
         json
         racket/future
         racket/exn
         (only-in "classify.rkt"
                  base-dir
                  normalize-test-path
                  collect-test-files
                  get-file-metadata
                  mutating-file?
                  restore-repo-surfaces!
                  clean-stale-bytecode!
                  file-has-rackunit-tests?
                  shard-files
                  print-lint-report)
         (only-in "parse.rkt"
                  test-file-result
                  test-file-result-path
                  test-file-result-exit-code
                  test-file-result-total
                  parse-raco-output
                  normalize-counts
                  effective-exit-code
                  classify-test-result)
         (only-in "reporting.rkt"
                  print-summary
                  save-failure-logs
                  summary-exit-code
                  write-json-results!
                  print-ledger-summary
                  print-run-summary-record)
         (only-in "ledger.rkt" load-known-failure-ledger)
         (only-in "cli.rkt" parse-args validate-args!)
         (only-in "profiles.rkt" profile-skips-test? make-skipped-result)
         (only-in "shard-plan.rkt"
                  build-shard-plan/safe
                  plan-shard-files
                  print-shard-plan-report
                  load-duration-snapshot
                  shard-plan-mode)
         (only-in "gate-evidence.rkt" record-gate-evidence!)
         (only-in "inventory.rkt" print-inventory compute-inventory-hash)
         (only-in "overhead.rkt" print-overhead-diagnostics)
         (only-in "impact.rkt"
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
         run-suite-once
         main)

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
                          0)]
       [else
        (define exit-code (ctrl 'exit-code))
        (define stdout-bytes (string->bytes/utf-8 (get-output-string stdout-out)))
        (define stderr-bytes (string->bytes/utf-8 (get-output-string stderr-out)))
        (define merged-bytes (bytes-append stdout-bytes stderr-bytes))
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
     (define merged-bytes (bytes-append stdout-bytes stderr-bytes))
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

(define (file-timeout-ms test-path timeout)
  (let ([meta-timeout (hash-ref (get-file-metadata test-path) 'timeout #f)])
    (if meta-timeout
        (* meta-timeout 1000)
        (or timeout 120000))))

(define (parse-result-bytes test-path stdout-bytes stderr-bytes exit-code elapsed-ms)
  (define merged-bytes (bytes-append stdout-bytes stderr-bytes))
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
                    total))

;; Per-file execution mode attribution (W0 fast-budget): records how each
;; file actually ran ('grouped-in-process vs 'subprocess) so the JSON report
;; can separate process-boot-dominated files from in-process ones. Keys are
;; resolved path strings (JSON-safe), values are symbols.
(define execution-modes (make-hash))
(define (mark-execution-mode! resolved-path mode)
  (hash-set! execution-modes (path->string (simplify-path resolved-path)) mode))
(define (execution-mode-of resolved-path)
  (hash-ref execution-modes (path->string (simplify-path resolved-path)) 'subprocess))

(define (run-single-file/subprocess test-path #:timeout [timeout #f])
  (define resolved-path (resolve-test-path test-path))
  (mark-execution-mode! resolved-path 'subprocess)
  (define file-timeout (file-timeout-ms test-path timeout))
  (define t0 (current-inexact-milliseconds))
  (define elapsed (lambda () (exact-round (- (current-inexact-milliseconds) t0))))
  (define stdout-out (open-output-string))
  (define stderr-out (open-output-string))
  (define-values (ctrl)
    (if (file-has-rackunit-tests? resolved-path)
        (let ([raco-bin (find-executable-path "raco")])
          (define-values (sp2 out-in2 pid2 err-in2 ctrl2)
            (apply values
                   (process*/ports stdout-out #f stderr-out raco-bin "test" "-t" resolved-path)))
          (when out-in2
            (close-output-port out-in2))
          ctrl2)
        (let ([racket-bin (find-executable-path "racket")])
          (define-values (sp out-in pid err-in ctrl)
            (apply values (process*/ports stdout-out #f stderr-out racket-bin resolved-path)))
          (when out-in
            (close-output-port out-in))
          ctrl)))
  (build-result-from-process test-path stdout-out stderr-out ctrl file-timeout elapsed))

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

;; Grouped/in-process execution requires a real (module+ test ...) submodule:
;; that is the only form whose dynamic-require both runs the tests and keeps
;; rackunit reporting intact. Anything else — module+ main suites, top-level
;; checks, comment-only mentions — runs as a raco subprocess (W3 policy).
(define (in-process-eligible? resolved-path)
  (module-plus-test-file? resolved-path))

;; raco test executes each file with current-directory bound to that file's
;; directory; grouped/in-process execution must do the same or tests that
;; resolve sibling paths (e.g. "../scripts/foo.rkt") break.
(define (in-process-cwd resolved-path)
  (or (and resolved-path (path-only (simplify-path resolved-path))) base-dir))

;; A test file that calls (exit ...) during grouped/in-process execution must
;; not terminate the host runner: custodians do not guard the exit handler, so
;; an unguarded (exit 0) silently kills the whole suite (observed W3: output
;; truncated at file 323 with process exit 0). Redirect exit to an exception
;; so the worker thread records the file's intended exit code instead.
(struct in-process-test-exit exn:fail (code))

(define (make-in-process-exit-handler exit-box)
  (lambda (code)
    (raise (in-process-test-exit (format "test file requested (exit ~a) during in-process execution"
                                         code)
                                 (current-continuation-marks)
                                 (if (and (number? code) (= code 0)) 0 1)))))

(define (run-single-file/in-process test-path #:timeout [timeout #f])
  (define resolved-path (resolve-test-path test-path))
  ;; Bare RackUnit files without run-tests/module+ still need raco's discovery output;
  ;; fall back to subprocess to avoid reintroducing zero-parsed false greens.
  (cond
    [(not (in-process-eligible? resolved-path))
     (run-single-file/subprocess test-path #:timeout timeout)]
    [else
     (mark-execution-mode! resolved-path 'grouped-in-process)
     (define file-timeout (file-timeout-ms test-path timeout))
     (define t0 (current-inexact-milliseconds))
     (define elapsed (lambda () (exact-round (- (current-inexact-milliseconds) t0))))
     (define stdout-out (open-output-string))
     (define stderr-out (open-output-string))
     (define exit-code (box #f))
     (define cust (make-custodian))
     (define worker
       (parameterize ([current-custodian cust])
         (thread (lambda ()
                   (with-handlers ([in-process-test-exit?
                                    (lambda (e) (set-box! exit-code (in-process-test-exit-code e)))]
                                   [exn:fail? (lambda (e)
                                                (displayln (exn->string e) stderr-out)
                                                (set-box! exit-code 1))])
                     (parameterize ([current-output-port stdout-out]
                                    [current-error-port stderr-out]
                                    [current-directory (in-process-cwd resolved-path)]
                                    [current-command-line-arguments #()]
                                    [exit-handler (make-in-process-exit-handler exit-code)]
                                    [current-namespace (make-base-namespace)])
                       (dynamic-require (in-process-module-path resolved-path) #f)
                       (set-box! exit-code 0)))))))
     (define completed? (sync/timeout (/ file-timeout 1000.0) worker))
     (unless completed?
       (custodian-shutdown-all cust))
     (define stdout-bytes (string->bytes/utf-8 (get-output-string stdout-out)))
     (define stderr-bytes (string->bytes/utf-8 (get-output-string stderr-out)))
     (if completed?
         (let ([result (parse-result-bytes test-path
                                           stdout-bytes
                                           stderr-bytes
                                           (or (unbox exit-code) 0)
                                           (elapsed))])
           (if (and (= (test-file-result-exit-code result) 0) (= (test-file-result-total result) 0))
               ;; W6: grouped loading runs the module body, but files whose
               ;; tests are bare test-case/check-* forms (no rackunit/text-ui
               ;; run-tests self-report) pass silently — dynamic-require
               ;; produces no per-check output. raco test's discovery wrapper
               ;; prints pass counts, so re-run such files as a subprocess
               ;; instead of surfacing a zero-parsed strict failure. This
               ;; reproduces the pre-grouped (subprocess) behavior exactly.
               (run-single-file/subprocess test-path #:timeout timeout)
               result))
         (test-file-result test-path 2 stdout-bytes stderr-bytes (elapsed) 0 0 0))]))

(define (run-single-file test-path #:timeout [timeout #f] #:mode [mode 'subprocess])
  (case mode
    [(in-process grouped) (run-single-file/in-process test-path #:timeout timeout)]
    [(auto subprocess) (run-single-file/subprocess test-path #:timeout timeout)]
    [else (run-single-file/subprocess test-path #:timeout timeout)]))

(define (split-list lst n)
  (cond
    [(null? lst) '()]
    [else (cons (take lst (min n (length lst))) (split-list (drop lst (min n (length lst))) n))]))

(define (run-all-files files
                       jobs
                       timeout
                       #:mode [mode 'subprocess]
                       #:first-batch-ms-box [first-batch-ms-box #f])
  (define batch-t0 (current-inexact-milliseconds))
  (define results (box '()))
  (define results-lock (make-semaphore 1))
  (define (add-result! r)
    (call-with-semaphore results-lock (lambda () (set-box! results (cons r (unbox results))))))
  (define batch-size jobs)
  (define batches (split-list files batch-size))
  (define gc-counter 0)
  (define total-batches (length batches))
  (for ([batch (in-list batches)]
        [batch-idx (in-naturals)])
    (define batch-threads
      (for/list ([f (in-list batch)])
        (thread (lambda ()
                  (define result (run-single-file f #:timeout (or timeout 120000) #:mode mode))
                  (define exit-code (test-file-result-exit-code result))
                  (cond
                    [(eq? (classify-test-result result) 'SKIPPED_BY_PROFILE) (display "S")]
                    [(= exit-code 0) (display ".")]
                    [(= exit-code 2) (display "T")]
                    [else (display "F")])
                  (flush-output)
                  (add-result! result)))))
    (for-each thread-wait batch-threads)
    (when (and first-batch-ms-box (= batch-idx 0))
      (set-box! first-batch-ms-box (exact-round (- (current-inexact-milliseconds) batch-t0))))
    (set! gc-counter (add1 gc-counter))
    (when (or (= 0 (modulo gc-counter 5)) (= gc-counter total-batches))
      (collect-garbage 'major)))
  (newline)
  (define file-order
    (for/hash ([f (in-list files)]
               [i (in-naturals)])
      (values f i)))
  (sort (unbox results) < #:key (lambda (r) (hash-ref file-order (test-file-result-path r) 0))))

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
                        #:first-batch-ms-box [first-batch-ms-box #f])
  (define t0 (current-inexact-milliseconds))
  (define exec-start-ms (hash-ref phases 'execution_start_ms #f))
  (define selection-end-ms (hash-ref phases 'selection_end_ms #f))
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
  (define-values (serial-files parallel-files)
    (if (> jobs 1)
        (values (filter mutating-file? runnable-files)
                (filter (lambda (f) (not (mutating-file? f))) runnable-files))
        (values '() runnable-files)))
  (when (pair? serial-files)
    (printf ";; run-tests: serializing ~a mutation-sensitive file~a before parallel batches~n"
            (length serial-files)
            (if (= (length serial-files) 1) "" "s")))
  (define serial-results
    (if (pair? serial-files)
        (run-all-files serial-files 1 timeout-ms #:mode mode #:first-batch-ms-box first-batch-ms-box)
        '()))
  (when (pair? serial-files)
    (restore-repo-surfaces! base-dir))
  (define parallel-results
    (if (pair? parallel-files)
        (run-all-files parallel-files
                       jobs
                       timeout-ms
                       #:mode mode
                       #:first-batch-ms-box first-batch-ms-box)
        '()))
  (define file-order
    (for/hash ([f (in-list suite-files)]
               [i (in-naturals)])
      (values f i)))
  (define results
    (sort (append skipped-results serial-results parallel-results)
          <
          #:key (lambda (r) (hash-ref file-order (test-file-result-path r) 0))))
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
           (define key
             (path->string (simplify-path (if (path? p)
                                              p
                                              (string->path p)))))
           (values key (hash-ref execution-modes key 'subprocess)))))
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
                         (and (or exec-start-ms selection-end-ms)
                              (let ([m (make-hasheq)])
                                (hash-set! m 'runner_start_ms (exact-round runner-start-ms))
                                (hash-set! m 'execution_end_ms (exact-round exec-end-ms))
                                (when exec-start-ms
                                  (hash-set! m 'execution_start_ms (exact-round exec-start-ms)))
                                (when selection-end-ms
                                  (hash-set! m 'selection_end_ms (exact-round selection-end-ms)))
                                (when (and first-batch-ms-box (unbox first-batch-ms-box))
                                  (hash-set! m 'first_batch_ms (unbox first-batch-ms-box)))
                                m))
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
                  diagnose-overhead?
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
      [else #f]))
  (define active-plan (box #f))
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
  (define first-batch-ms-box (box #f))
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
                      #:first-batch-ms-box first-batch-ms-box))
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
                           #:inventory-hash inv-hash))
  (exit 0))

(define invoked-directly?
  (let ([run-file (find-system-path 'run-file)])
    (and (path? run-file)
         (let ([base (file-name-from-path run-file)])
           (and base (equal? (path->string base) "run-tests.rkt"))))))
(when invoked-directly?
  (main (vector->list (current-command-line-arguments))))
