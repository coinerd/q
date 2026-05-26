#lang racket/base

;; run-tests.rkt - Parallel test runner with per-file result tracking.
;;
;; KEY FIX: Uses `racket <file>` directly instead of `raco test -t <file>`
;; to avoid 17-19s per-file startup overhead on some Racket installations.
;;
;; Key functions:
;;   make-test-file-result - construct a test-file-result
;;   parse-raco-output     - parse passed/failed counts from test stdout
;;   extract-failure-lines - extract FAILURE blocks from test output
;;   run-single-file       - run one test file via process*/ports
;;   collect-test-files    - gather test files by suite
;;   run-all-files         - parallel dispatcher with progress dots
;;   print-summary         - formatted summary table
;;   save-failure-logs     - write failure output to /tmp
;;   format-duration       - ms → human-readable
;;   summary-exit-code     - compute exit code from failures/timeouts

(require racket/string
         racket/match
         racket/math
         racket/path
         racket/file
         racket/system
         racket/port
         racket/list
         racket/future)

;; Safe bytes->string conversion (handles invalid UTF-8)
(define (bytes->string* bs)
  (with-handlers ([exn:fail? (lambda (e) (bytes->string/latin-1 bs))])
    (bytes->string/utf-8 bs)))

;; ---------------------------------------------------------------------------
;; Provide all public API for testing
;; ---------------------------------------------------------------------------

;; Struct
(provide (struct-out test-file-result)
         make-test-file-result
         ;; Parsing
         parse-raco-output
         normalize-counts
         extract-failure-lines
         ;; Running
         run-single-file
         run-all-files
         collect-test-files
         ;; Isolation helpers
         mutating-file?
         mutating-patterns
         repo-surface-files
         restore-repo-surfaces!
         ;; Output
         print-summary
         save-failure-logs
         format-duration
         summary-exit-code
         bytes->string*
         clean-stale-bytecode!
         file-has-rackunit-tests?
         parse-args
         ;; Shard classifiers (W14)
         arch-file?
         runtime-file?
         extensions-file?
         workflows-file?)

;; ---------------------------------------------------------------------------
;; Struct: test-file-result
;; ---------------------------------------------------------------------------

(struct test-file-result (path exit-code stdout-bytes stderr-bytes elapsed-ms passed failed total)
  #:transparent)

;; Constructor alias for cleaner test access
(define (make-test-file-result path
                               exit-code
                               stdout-bytes
                               stderr-bytes
                               elapsed-ms
                               passed
                               failed
                               total)
  (test-file-result path exit-code stdout-bytes stderr-bytes elapsed-ms passed failed total))

;; ---------------------------------------------------------------------------
;; Suite matching helpers
;; ---------------------------------------------------------------------------

(define slow-patterns
  '("sandbox" "subprocess"
              "integration"
              "benchmark"
              "workflow-"
              "e2e-"
              "ci-local"
              "metrics-readme"
              "bump-version"
              "examples-compile" ;; compiles all examples (~60s)
              "pre-commit" ;; runs full pre-commit pipeline (~120s)
              "racket-tooling" ;; spawns raco commands (~90s)
              "run-tests" ;; tests the test runner itself (spawns subprocesses)
              "audit-script" ;; spawns racket subprocesses (~30s)
              "test-doctor" ;; spawns raco make subprocesses (~20s)
              "check-deps" ;; modifies info.rkt in-place
              "self-hosting" ;; loads full runtime (slow under load)
              "tui-terminal" ;; TUI terminal I/O tests
              "sync-readme" ;; file I/O tests, slow under load
              ))

;; Path-based slow patterns — matched against the full path, not just basename.
;; Used for tests that are integration-level (need full runtime mock) or slow.
;; v0.59.4 characterization: workflow tests are integration-level but do NOT hang
;; with explicit timeouts (20 passed, 4 failed, 0 timeouts in 180s).
;; The /workflows/ exclusion from fast suite is intentional: they need full runtime
;; and are available via `--suite workflows`.
(define path-slow-patterns '("/workflows/"))

(define (slow-file? f)
  (define base (file-name-from-path f))
  (or (for/or ([p (in-list slow-patterns)])
        (and base (string-contains? (path->string base) p)))
      (for/or ([p (in-list path-slow-patterns)])
        (string-contains? f p))))

(define (tui-file? f)
  (define base (file-name-from-path f))
  (or (string-contains? f "/tui/")
      (string-contains? f "/interfaces/tui.rkt")
      (and base (string-prefix? (path->string base) "test-tui-"))))

;; Tests that mutate repo files or run self-healing linters should not run in
;; parallel with other tests, otherwise clean-tree assumptions can race.
;; Also includes tests that load the full extension system (which triggers
;; quarantine.rkt lock-file operations on the shared ~/.q/quarantine path).
(define mutating-patterns
  '("ci-local" "pre-commit"
               "check-deps"
               "sync-version"
               "sync-readme"
               "bump-version"
               "metrics-readme"
               "self-hosting"))

(define (mutating-file? f)
  (define base (file-name-from-path f))
  (for/or ([p (in-list mutating-patterns)])
    (and base (string-contains? (path->string base) p))))

(define (security-file? f)
  (define base (path->string (file-name-from-path f)))
  (or (string-contains? base "security")
      (string-contains? base "permission")
      (string-contains? base "safe-mode")
      (string-contains? base "sandbox")
      (string-contains? base "tool-bash")
      ;; Tag-based: files with ";; @suite security" in first 10 lines
      (file-has-suite-tag? f "security")))

(define (file-has-suite-tag? f tag)
  "Check if a test file has a ;; @suite <tag> comment in its first 10 lines."
  (with-handlers ([exn:fail? (lambda (_) #f)])
    (call-with-input-file f
                          (lambda (port)
                            (define target (string-append "@suite " tag))
                            (for/or ([_ (in-range 10)]
                                     #:break (eof-object? (peek-byte port)))
                              (define line (read-line port))
                              (and (string? line) (string-contains? line target)))))))

(define (smoke-excluded? f)
  (or (slow-file? f) (string-contains? f "/workflows/") (string-contains? f "/interfaces/")))

(define support-test-module-names
  '("event-simulator.rkt" "mock-tui-session.rkt" "state-assertions.rkt" "workflow-harness.rkt"))

(define (support-test-module? f)
  (define s
    (if (path? f)
        (path->string f)
        f))
  (define base (file-name-from-path s))
  (or (string-contains? s "/helpers/")
      (string-contains? s "/fixtures/")
      (and base (member (path->string base) support-test-module-names) #t)))

;; Base directory for resolving test paths.
;; The runner must work from both the monorepo root (`racket q/scripts/run-tests.rkt`)
;; and the q repo root (`cd q && racket scripts/run-tests.rkt`).  Resolve the q
;; root by probing the current/original directory, its parent, and a `q/` child.
(define (q-root-candidate? p)
  (and (directory-exists? (build-path p "tests"))
       (file-exists? (build-path p "scripts" "run-tests.rkt"))))

(define (resolve-base-dir orig)
  (define parent (simplify-path (build-path orig "..")))
  (define candidates
    ;; Prefer q/ children before the current directory so invoking from the
    ;; monorepo root does not accidentally select legacy top-level tests/.
    (list (simplify-path (build-path orig "q")) (simplify-path (build-path parent "q")) orig parent))
  (or (for/first ([candidate (in-list candidates)]
                  #:when (q-root-candidate? candidate))
        candidate)
      orig))

(define base-dir (resolve-base-dir (find-system-path 'orig-dir)))

(define (normalize-test-path f)
  (define s
    (if (path? f)
        (path->string f)
        f))
  (cond
    [(absolute-path? s) s]
    [(string-prefix? s "q/tests/") (substring s 2)]
    [(string-prefix? s "./q/tests/") (substring s 4)]
    [else s]))

(define (arch-file? f)
  (or (string-contains? f "arch-")
      (string-contains? f "boundary")
      (string-contains? f "fitness")
      (string-contains? f "hotspot")
      (file-has-suite-tag? f "arch")))

(define (runtime-file? f)
  (or (string-contains? f "runtime")
      (string-contains? f "session")
      (string-contains? f "compaction")
      (string-contains? f "iteration")
      (string-contains? f "turn-")
      (string-contains? f "tool-coord")
      (file-has-suite-tag? f "runtime")))

(define (extensions-file? f)
  (or (string-contains? f "extensions/")
      (string-contains? f "gsd-")
      (string-contains? f "define-extension")
      (string-contains? f "wave-executor")
      (string-contains? f "hook-")
      (file-has-suite-tag? f "extensions")))

(define (workflows-file? f)
  (and (string-contains? f "/workflows/") (not (string-contains? f "/fixtures/"))))

(define (collect-test-files suite #:extra-files [extra-files #f])
  (cond
    [(pair? extra-files) (map normalize-test-path extra-files)]
    [else
     (define all-files
       (for/list ([f (in-directory (build-path base-dir "tests"))]
                  #:when (and (file-exists? f)
                              (let ([s (path->string f)])
                                (and (string-suffix? s ".rkt")
                                     (not (string-contains? s "/compiled/"))
                                     (not (support-test-module? s))))))
         (path->string (find-relative-path base-dir f))))
     (case suite
       [(all) all-files]
       [(fast) (filter (lambda (f) (not (slow-file? f))) all-files)]
       [(slow) (filter slow-file? all-files)]
       [(tui) (filter tui-file? all-files)]
       [(smoke) (filter (lambda (f) (not (smoke-excluded? f))) all-files)]
       [(security) (filter security-file? all-files)]
       [(arch) (filter arch-file? all-files)]
       [(runtime) (filter runtime-file? all-files)]
       [(extensions) (filter extensions-file? all-files)]
       [(workflows) (filter workflows-file? all-files)]
       [else '("tests/")])]))

;; ---------------------------------------------------------------------------
;; parse-raco-output - extract passed/failed/total from test stdout
;; ---------------------------------------------------------------------------
;;
;; Parses two output formats:
;;
;; 1. `racket <file>` with rackunit/text-ui:
;;    "13 success(es) 0 failure(s) 0 error(s) 13 test(s) run"
;;
;; 2. Legacy `raco test` format (still supported):
;;    "5 tests passed\n2 tests failed"

(define (parse-raco-output stdout-bytes)
  (define output (bytes->string* stdout-bytes))
  (define lines (string-split output "\n"))

  ;; Collect all rackunit/text-ui result lines and sum them
  ;; NOTE: Avoids for/first — broken in some racket/base contexts for regexp-match
  (define all-matches
    (filter
     values
     (for/list ([line (in-list lines)])
       (regexp-match
        #px"([0-9]+) success\\(es\\) ([0-9]+) failure\\(s\\)(?: ([0-9]+) error\\(s\\))? ([0-9]+) test\\(s\\) run"
        line))))

  (cond
    [(pair? all-matches)
     (define passed (for/sum ([m (in-list all-matches)]) (string->number (cadr m))))
     (define failed
       (for/sum ([m (in-list all-matches)])
                (+ (string->number (caddr m))
                   (let ([errors (list-ref m 3)])
                     (if errors
                         (string->number errors)
                         0)))))
     (values passed failed (+ passed failed))]
    [else
     ;; Fall back to legacy raco test format
     (define passed
       (for/fold ([n 0]) ([line (in-list lines)])
         (define m (regexp-match #rx"([0-9]+) tests? passed" line))
         (if m
             (string->number (cadr m))
             n)))

     (define failed
       (for/fold ([n 0]) ([line (in-list lines)])
         (define m (regexp-match #rx"([0-9]+) tests? failed" line))
         (if m
             (string->number (cadr m))
             n)))

     (cond
       [(> (+ passed failed) 0) (values passed failed (+ passed failed))]
       [else
        ;; rackunit `run-test` prints result structs rather than summary counts.
        (define result-successes (length (regexp-match* #rx"#<test-success>" output)))
        (define result-failures
          (+ (length (regexp-match* #rx"#<test-failure>" output))
             (length (regexp-match* #rx"#<test-error>" output))))
        (values result-successes result-failures (+ result-successes result-failures))])]))

;; Normalize parsed counters against process exit status.
;; Parsed rackunit failures/errors are release-blocking even if a legacy test
;; file forgot to propagate `run-tests`' non-zero result to the process exit.
(define (normalize-counts exit-code passed failed total)
  (values passed failed total))

(define (effective-exit-code exit-code failed)
  (if (and (= exit-code 0) (> failed 0)) 1 exit-code))

;; ---------------------------------------------------------------------------
;; extract-failure-lines - pull FAILURE context blocks from output
;; ---------------------------------------------------------------------------

(define FAILURE-START #rx"^-+ FAILURE -+$")
(define FAILURE-END #rx"^-{20,}$")

(define (extract-failure-lines stdout-bytes)
  (define output (bytes->string* stdout-bytes))
  (define lines (string-split output "\n"))
  (define in-failure? (box #f))
  (for/list ([line (in-list lines)]
             #:when (cond
                      [(regexp-match? FAILURE-START line)
                       (set-box! in-failure? #t)
                       #t]
                      [(and (unbox in-failure?)
                            (regexp-match? FAILURE-END line)
                            (not (regexp-match? FAILURE-START line)))
                       (set-box! in-failure? #f)
                       #t]
                      [(unbox in-failure?) #t]
                      [else #f]))
    line))

;; ---------------------------------------------------------------------------
;; run-single-file - spawn process for one test file
;; ---------------------------------------------------------------------------

;; Detect files that define rackunit tests but never call run-tests.
;; These files produce zero output when run with `racket <file>`,
;; causing the parser to see 0 tests and the runner to false-green.
;;
;; Matches both:
;;   (test-case ...) — explicit test cases
;;   (check- ...)    — bare check forms (check-equal?, check-true, etc.)
;; but NOT files that have (run-tests ...) — those are self-contained.
(define (file-has-rackunit-tests? path)
  (and (file-exists? path)
       (let* ([content (file->string path)]
              [has-rackunit-forms? (or (regexp-match? #rx"\\(test-case" content)
                                       (regexp-match? #rx"\\(check-" content))]
              [module-plus-test? (regexp-match? #px"\\(module\\+\\s+test\\b" content)]
              [self-running? (regexp-match? #rx"\\(run-tests" content)])
         (and has-rackunit-forms? (or module-plus-test? (not self-running?))))))

;; Common result extraction from a running process.
;; Handles timeout logic and stdout/stderr parsing uniformly.
(define (build-result-from-process test-path stdout-out stderr-out ctrl timeout elapsed)
  (cond
    [timeout
     (define wait-thread (thread (lambda () (ctrl 'wait))))
     (define timed-out? (not (sync/timeout (/ timeout 1000.0) wait-thread)))
     (cond
       [timed-out?
        (ctrl 'kill)
        ;; Give the wait thread 5s to clean up after kill
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
        (define effective-code (effective-exit-code exit-code failed))
        (test-file-result test-path
                          effective-code
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
     (define effective-code (effective-exit-code exit-code failed))
     (test-file-result test-path
                       effective-code
                       stdout-bytes
                       stderr-bytes
                       (elapsed)
                       passed
                       failed
                       total)]))

(define (run-single-file test-path #:timeout [timeout #f])
  (define resolved-path
    (if (absolute-path? test-path)
        test-path
        (simplify-path (build-path base-dir test-path))))
  (define t0 (current-inexact-milliseconds))
  (define elapsed (lambda () (exact-round (- (current-inexact-milliseconds) t0))))

  ;; Use process*/ports to capture stdout+stderr into string ports.
  (define stdout-out (open-output-string))
  (define stderr-out (open-output-string))

  ;; Choose the right runner upfront to avoid a wasted fast-path + fallback
  ;; double-run for files that define rackunit tests but never call run-tests.
  (define-values (ctrl)
    (if (file-has-rackunit-tests? resolved-path)
        ;; Slow path: raco test discovers and runs registered test cases
        (let ([raco-bin (find-executable-path "raco")])
          (define-values (sp2 out-in2 pid2 err-in2 ctrl2)
            (apply values
                   (process*/ports stdout-out #f stderr-out raco-bin "test" "-t" resolved-path)))
          (when out-in2
            (close-output-port out-in2))
          ctrl2)
        ;; Fast path: racket <file> for files with run-tests or non-rackunit tests
        (let ([racket-bin (find-executable-path "racket")])
          (define-values (sp out-in pid err-in ctrl)
            (apply values (process*/ports stdout-out #f stderr-out racket-bin resolved-path)))
          (when out-in
            (close-output-port out-in))
          ctrl)))

  (build-result-from-process test-path stdout-out stderr-out ctrl timeout elapsed))

;; ---------------------------------------------------------------------------
;; run-all-files - parallel dispatcher with progress dots
;; ---------------------------------------------------------------------------

(define (run-all-files files jobs timeout)
  (define results (box '()))
  (define results-lock (make-semaphore 1))
  (define n (length files))

  (define (add-result! r)
    (call-with-semaphore results-lock (lambda () (set-box! results (cons r (unbox results))))))

  ;; Process files in batches of `jobs` to avoid creating hundreds of threads.
  ;; This prevents FD/thread exhaustion on large test suites.
  (define batch-size jobs)
  (define batches (split-list files batch-size))

  (for ([batch (in-list batches)]
        [batch-idx (in-naturals)])
    (define batch-threads
      (for/list ([f (in-list batch)])
        (thread (lambda ()
                  (define result (run-single-file f #:timeout (or timeout 120000)))
                  (define exit-code (test-file-result-exit-code result))
                  (cond
                    [(= exit-code 0) (display ".")]
                    [(= exit-code 2) (display "T")]
                    [else (display "F")])
                  (flush-output)
                  (add-result! result)))))
    ;; Wait for entire batch to complete before starting next
    (for-each thread-wait batch-threads)
    ;; Major GC after each batch to prevent port/FD accumulation
    (collect-garbage 'major))

  (newline)

  ;; Return results in original file order
  (define file-order
    (for/hash ([f (in-list files)]
               [i (in-naturals)])
      (values f i)))
  (sort (unbox results) < #:key (lambda (r) (hash-ref file-order (test-file-result-path r) 0))))

;; Helper: split list into chunks
(define (split-list lst n)
  (cond
    [(null? lst) '()]
    [else (cons (take lst (min n (length lst))) (split-list (drop lst (min n (length lst))) n))]))

;; ---------------------------------------------------------------------------
;; print-summary - formatted output
;; ---------------------------------------------------------------------------

(define (print-summary results total-start-ms)
  (define total-files (length results))
  (define passed-files (count (lambda (r) (= (test-file-result-exit-code r) 0)) results))
  (define failed-files
    (count (lambda (r)
             (and (not (= (test-file-result-exit-code r) 0))
                  (not (= (test-file-result-exit-code r) 2))))
           results))
  (define timeout-files (count (lambda (r) (= (test-file-result-exit-code r) 2)) results))
  (define total-passed (apply + (map test-file-result-passed results)))
  (define total-failed (apply + (map test-file-result-failed results)))
  (define total-tests (apply + (map test-file-result-total results)))

  (newline)
  (displayln "═══════════════════════════════════════════════════════════")
  (displayln "                    TEST SUMMARY")
  (displayln "═══════════════════════════════════════════════════════════")
  (printf "  Files:     ~a total, ~a passed, ~a failed, ~a timeouts~n"
          total-files
          passed-files
          failed-files
          timeout-files)
  (printf "  Tests:     ~a total, ~a passed, ~a failed~n" total-tests total-passed total-failed)
  ;; v0.45.8 (NF14): Diagnostic hint when no test output parsed
  (when (and (= total-tests 0) (= passed-files total-files))
    (displayln "  ⚠ No test results parsed — files may use non-standard output format"))
  (printf "  Elapsed:   ~a~n" (format-duration total-start-ms))
  (displayln "═══════════════════════════════════════════════════════════")

  ;; Print failure details
  (define failures (filter (lambda (r) (not (= (test-file-result-exit-code r) 0))) results))
  (when (pair? failures)
    (newline)
    (displayln "FAILURES:")
    (for ([f (in-list failures)])
      (define path (test-file-result-path f))
      (define ec (test-file-result-exit-code f))
      (printf "  ~a ~a (exit=~a, ~a passed, ~a failed, ~a)~n"
              (if (= ec 2) "⏱" "✗")
              path
              ec
              (test-file-result-passed f)
              (test-file-result-failed f)
              (format-duration (test-file-result-elapsed-ms f)))
      (define failure-lines (extract-failure-lines (test-file-result-stdout-bytes f)))
      (for ([line (in-list failure-lines)])
        (printf "    ~a~n" line)))))

;; ---------------------------------------------------------------------------
;; save-failure-logs - write full failure output to /tmp
;; ---------------------------------------------------------------------------

(define (save-failure-logs results)
  (define failures (filter (lambda (r) (not (= (test-file-result-exit-code r) 0))) results))
  (for ([f (in-list failures)])
    (define safe-name
      (let* ([base (file-name-from-path (test-file-result-path f))]
             [s (if base
                    (path->string base)
                    "unknown")])
        (regexp-replace* #rx"[^a-zA-Z0-9._-]" s "_")))
    (define log-path (build-path "/tmp" (string-append "q-test-fail-" safe-name ".log")))
    (call-with-output-file log-path
                           #:exists 'truncate/replace
                           (lambda (out)
                             (fprintf out
                                      "=== ~a (exit=~a, elapsed=~a) ===~n"
                                      (test-file-result-path f)
                                      (test-file-result-exit-code f)
                                      (format-duration (test-file-result-elapsed-ms f)))
                             (fprintf out "--- stdout ---~n")
                             (display (bytes->string* (test-file-result-stdout-bytes f)) out)
                             (fprintf out "~n--- stderr ---~n")
                             (display (bytes->string* (test-file-result-stderr-bytes f)) out)))))

;; ---------------------------------------------------------------------------
;; format-duration - ms → human-readable
;; ---------------------------------------------------------------------------

(define (format-duration ms)
  (define total-secs (/ ms 1000.0))
  (cond
    [(>= ms 3600000)
     (define hours (quotient ms 3600000))
     (define remaining (- ms (* hours 3600000)))
     (define mins (quotient remaining 60000))
     (define secs (/ (- remaining (* mins 60000)) 1000.0))
     (format "~ah ~am ~as" hours mins secs)]
    [(>= ms 60000)
     (define mins (quotient ms 60000))
     (define secs (/ (- ms (* mins 60000)) 1000.0))
     (format "~am ~as" mins secs)]
    [else (format "~as" total-secs)]))

;; ---------------------------------------------------------------------------
;; summary-exit-code
;; ---------------------------------------------------------------------------

(define (summary-exit-code failure-count timeout-count)
  (cond
    [(and (> failure-count 0) (> timeout-count 0)) 3]
    [(> failure-count 0) 1]
    [(> timeout-count 0) 2]
    [else 0]))

;; ---------------------------------------------------------------------------
;; CLI argument parsing
;; ---------------------------------------------------------------------------

(define (usage)
  (displayln "Usage: racket scripts/run-tests.rkt [OPTIONS] [TEST-FILES ...]")
  (newline)
  (displayln "Options:")
  (displayln "  --jobs N          Number of parallel jobs (default: processor-count)")
  (displayln "  --sequential      Run tests sequentially (jobs=1)")
  (displayln "  --timeout SECS    Per-file timeout in seconds")
  (displayln
   "  --suite <name>    Run test suite: all (default), fast, slow, tui, smoke, security, arch, runtime, extensions, workflows")
  (displayln "  --strict          Enable strict zero-test detection (default: on)")
  (displayln "  --repeat N        Run suite N times (exit 1 if any run fails)")
  (displayln "  --record-gate-evidence  Write .gate-evidence/<suite>.passed on success")
  (displayln "  --help            Show this help message")
  (newline)
  (displayln "Suites:")
  (displayln "  all     Entire tests/ directory (per-file spawn)")
  (displayln "  fast    All tests except slow patterns (per-file spawn)")
  (displayln "  slow    Only sandbox/subprocess tests")
  (displayln "  tui     Files in tests/tui/")
  (displayln "  smoke   Fast minus workflows/, interfaces/, and provider tests")
  (displayln "  security  All security/permission/sandbox/safe-mode tests")
  (displayln "  arch    Architecture boundary/fitness tests")
  (displayln "  runtime Runtime/session/compaction/iteration tests")
  (displayln "  extensions Extension/GSD/hook tests")
  (displayln "  workflows All tests/workflows/ excluding fixtures (integration-level)"))

(define (parse-args args)
  (let loop ([rest args]
             [jobs (processor-count)]
             [sequential? #f]
             [timeout #f]
             [strict? #t] ;; Always-on strict mode (W0: false-green sentinel)
             [suite 'all]
             [extra '()]
             [repeat 1]
             [record-gate? #f])
    (match rest
      ['() (values jobs sequential? timeout strict? suite (reverse extra) repeat record-gate?)]
      [(list "--help" _ ...)
       (usage)
       (exit 0)]
      [(list "--strict" rest ...)
       (loop rest jobs sequential? timeout #t suite extra repeat record-gate?)]
      [(list "--jobs" n rest ...)
       (loop rest (string->number n) sequential? timeout strict? suite extra repeat record-gate?)]
      [(list "--sequential" rest ...)
       (loop rest 1 #t timeout strict? suite extra repeat record-gate?)]
      [(list "--timeout" secs rest ...)
       (loop rest jobs sequential? (string->number secs) strict? suite extra repeat record-gate?)]
      [(list "--suite" name rest ...)
       (loop rest jobs sequential? timeout strict? (string->symbol name) extra repeat record-gate?)]
      [(list "--repeat" n rest ...)
       (loop rest jobs sequential? timeout strict? suite extra (string->number n) record-gate?)]
      [(list "--record-gate-evidence" rest ...)
       (loop rest jobs sequential? timeout strict? suite extra repeat #t)]
      [(list arg rest ...)
       (loop rest jobs sequential? timeout strict? suite (cons arg extra) repeat record-gate?)])))

;; ---------------------------------------------------------------------------
;; Pre-flight: stale bytecode cleanup
;; ---------------------------------------------------------------------------

(define (compiled-zo-source-candidates compiled-dir zo)
  (define parent (path-only compiled-dir))
  (define base-path (file-name-from-path zo))
  (define base
    (if base-path
        (path->string base-path)
        ""))
  (define stem (regexp-replace #rx"\\.zo$" base ""))
  (filter values
          (list (and (regexp-match? #rx"_rkt$" stem)
                     (build-path parent (regexp-replace #rx"_rkt$" stem ".rkt")))
                (and (regexp-match? #rx"_rktl$" stem)
                     (build-path parent (regexp-replace #rx"_rktl$" stem ".rktl")))
                (and (regexp-match? #rx"_scrbl$" stem)
                     (build-path parent (regexp-replace #rx"_scrbl$" stem ".scrbl")))
                (path-replace-extension (path-replace-suffix zo "") #".rkt")
                (path-replace-extension (path-replace-suffix zo "") #".rktl"))))

(define (stale-compiled-zo? compiled-dir zo)
  (and (file-exists? zo)
       (string-suffix? (path->string zo) ".zo")
       (let* ([candidates (compiled-zo-source-candidates compiled-dir zo)]
              [existing-sources (filter file-exists? candidates)])
         (or (null? existing-sources)
             (for/or ([src (in-list existing-sources)])
               (> (file-or-directory-modify-seconds src) (file-or-directory-modify-seconds zo)))))))

(define (clean-stale-bytecode! root)
  "Delete compiled/ directories with stale or orphan .zo files.
  This prevents instantiate-linklet mismatches after module moves or renames."
  (define cleaned 0)
  (for ([d (in-directory root)])
    (when (and (directory-exists? d) (equal? (path->string (file-name-from-path d)) "compiled"))
      (define zo-files (directory-list d #:build? #t))
      (define stale?
        (for/or ([zo (in-list zo-files)])
          (stale-compiled-zo? d zo)))
      (when stale?
        (delete-directory/files d)
        (set! cleaned (add1 cleaned)))))
  cleaned)

;; ---------------------------------------------------------------------------
;; Post-serial guard: restore repo surfaces after mutation-sensitive tests
;; ---------------------------------------------------------------------------

;; Key repo files that mutation-sensitive tests may modify (check-deps writes
;; info.rkt, pre-commit version-drift test writes info.rkt, etc.).  We restore
;; them from git before starting the parallel batch so parallel tests see a
;; clean tree.
(define repo-surface-files '("info.rkt" "README.md" "CHANGELOG.md"))

(define (restore-repo-surfaces! root)
  (for ([surface (in-list repo-surface-files)])
    (define path (build-path root surface))
    (when (file-exists? path)
      (define git-restore (format "cd ~a && git checkout -- ~a 2>/dev/null" root surface))
      (system git-restore))))

;; ---------------------------------------------------------------------------
;; Main entry point
;; ---------------------------------------------------------------------------

(define (run-suite-once suite-files jobs timeout-ms strict? suite-label repeat-num repeat-total)
  "Run the test suite once. Returns exit code or #f for success with more repeats."
  (define t0 (current-inexact-milliseconds))

  ;; Run mutation-sensitive files sequentially to avoid cross-test races in
  ;; parallel mode (e.g., ci-local/pre-commit touching README/info surfaces).
  (define-values (serial-files parallel-files)
    (if (> jobs 1)
        (values (filter mutating-file? suite-files)
                (filter (lambda (f) (not (mutating-file? f))) suite-files))
        (values '() suite-files)))

  (when (pair? serial-files)
    (printf ";; run-tests: serializing ~a mutation-sensitive file~a before parallel batches~n"
            (length serial-files)
            (if (= (length serial-files) 1) "" "s")))

  (define serial-results
    (if (pair? serial-files)
        (run-all-files serial-files 1 timeout-ms)
        '()))

  ;; Post-serial guard: restore repo surfaces that mutation-sensitive tests
  ;; may have modified (info.rkt, README.md).  This prevents contamination
  ;; of the parallel batch.
  (when (pair? serial-files)
    (restore-repo-surfaces! base-dir))

  (define parallel-results
    (if (pair? parallel-files)
        (run-all-files parallel-files jobs timeout-ms)
        '()))

  (define file-order
    (for/hash ([f (in-list suite-files)]
               [i (in-naturals)])
      (values f i)))

  (define results
    (sort (append serial-results parallel-results)
          <
          #:key (lambda (r) (hash-ref file-order (test-file-result-path r) 0))))

  (define total-elapsed (exact-round (- (current-inexact-milliseconds) t0)))

  (print-summary results total-elapsed)
  (save-failure-logs results)

  (define failed-files
    (count (lambda (r)
             (and (not (= (test-file-result-exit-code r) 0))
                  (not (= (test-file-result-exit-code r) 2))))
           results))
  (define timeout-files (count (lambda (r) (= (test-file-result-exit-code r) 2)) results))

  ;; Strict mode: flag files that exited 0 but parsed zero tests
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
  exit-code)

;; ---------------------------------------------------------------------------
;; Gate evidence recording
;; ---------------------------------------------------------------------------

(define (record-gate-evidence! suite-label)
  (define evid-dir (build-path (current-directory) ".gate-evidence"))
  (unless (directory-exists? evid-dir)
    (make-directory evid-dir))
  (define ver
    (let ([m (regexp-match #rx"define q-version \"([^\"]+)\"" (file->string "util/version.rkt"))])
      (or (and m (cadr m)) "unknown")))
  (define evidence-file (build-path evid-dir (format "~a.passed" suite-label)))
  (with-output-to-file evidence-file
                       (lambda () (printf "~a ~a~n" ver (current-seconds)))
                       #:exists 'truncate)
  (printf ";; gate evidence recorded: ~a (v~a)~n" suite-label ver))

(define (main args)
  (define-values (jobs sequential? timeout strict? suite extra-files repeat record-gate?)
    (parse-args args))

  ;; Pre-flight: clear stale bytecode to avoid linklet mismatches
  (define cleaned-dirs (clean-stale-bytecode! (current-directory)))
  (when (> cleaned-dirs 0)
    (printf ";; run-tests: cleaned ~a stale compiled/ director~a~n"
            cleaned-dirs
            (if (= cleaned-dirs 1) "y" "ies")))

  (define suite-files
    (if (pair? extra-files)
        (map normalize-test-path extra-files)
        (collect-test-files suite)))

  (unless (pair? suite-files)
    (displayln "No test files matched the selected suite.")
    (exit 1))

  ;; Per-file spawning with result tracking
  (define timeout-ms (and timeout (* timeout 1000)))
  (define suite-label (symbol->string suite))
  (define n-files (length suite-files))

  (printf ";; run-tests: suite=~a files=~a jobs=~a sequential=~a repeat=~a~n"
          suite-label
          n-files
          jobs
          sequential?
          repeat)
  (newline)

  (when (> repeat 1)
    (printf ";; run-tests: running suite ~a time~a for confidence gate~n"
            repeat
            (if (= repeat 1) "" "s")))

  ;; Run suite, optionally repeating N times
  (for ([run-num (in-range 1 (add1 repeat))])
    (when (> repeat 1)
      (printf "~n;; ── Run ~a/~a ──~n" run-num repeat))
    (define exit-code (run-suite-once suite-files jobs timeout-ms strict? suite-label run-num repeat))
    (unless (zero? exit-code)
      (exit exit-code)))

  ;; Record gate evidence if requested
  (when record-gate?
    (record-gate-evidence! suite-label))

  (exit 0))

;; Entry point — only auto-run when executed directly as a script.
;; When dynamic-require loads this module, it won't trigger main
;; because we check the exact filename (not just suffix).
(define invoked-directly?
  (let ([run-file (find-system-path (quote run-file))])
    (and (path? run-file)
         (let ([base (file-name-from-path run-file)])
           (and base (equal? (path->string base) "run-tests.rkt"))))))
(when invoked-directly?
  (main (vector->list (current-command-line-arguments))))
