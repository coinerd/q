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
         extract-failure-lines
         ;; Running
         run-single-file
         run-all-files
         collect-test-files
         ;; Output
         print-summary
         save-failure-logs
         format-duration
         summary-exit-code
         bytes->string*)

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

(define (slow-file? f)
  (define base (file-name-from-path f))
  (for/or ([p (in-list slow-patterns)])
    (and base (string-contains? (path->string base) p))))

(define (tui-file? f)
  (string-contains? f "/tui/"))

(define (smoke-excluded? f)
  (or (slow-file? f)
      (string-contains? f "/workflows/")
      (string-contains? f "/interfaces/")
      (let ([base (file-name-from-path f)])
        (and base (string-contains? (path->string base) "provider")))))

;; Base directory for resolving test paths.
;; orig-dir gives CWD when invoked directly, but under `raco test --process`
;; it may point at the test file's directory. We detect which case we're in:
;; if orig-dir has a tests/ subdirectory, it's the project root; otherwise
;; try the parent (handles orig-dir = q/tests/ or q/scripts/).
(define base-dir
  (let ([orig (find-system-path 'orig-dir)])
    (if (directory-exists? (build-path orig "tests"))
        orig
        (let ([parent (simplify-path (build-path orig ".."))])
          (if (directory-exists? (build-path parent "tests")) parent orig)))))

(define (collect-test-files suite #:extra-files [extra-files #f])
  (cond
    [(pair? extra-files) extra-files]
    [else
     (define all-files
       (for/list ([f (in-directory (build-path base-dir "tests"))]
                  #:when (and (file-exists? f)
                              (let ([s (path->string f)])
                                (and (string-suffix? s ".rkt")
                                     (not (string-contains? s "/compiled/"))))))
         (path->string (find-relative-path base-dir f))))
     (case suite
       [(all) all-files]
       [(fast) (filter (lambda (f) (not (slow-file? f))) all-files)]
       [(slow) (filter slow-file? all-files)]
       [(tui) (filter tui-file? all-files)]
       [(smoke) (filter (lambda (f) (not (smoke-excluded? f))) all-files)]
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
     (define failed (for/sum ([m (in-list all-matches)]) (string->number (caddr m))))
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

     (values passed failed (+ passed failed))]))

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

(define (run-single-file test-path #:timeout [timeout #f])
  (define resolved-path
    (if (absolute-path? test-path)
        test-path
        (simplify-path (build-path base-dir test-path))))
  (define stdout-out (open-output-bytes))
  (define stderr-out (open-output-bytes))
  ;; Use `raco test` per file (not `racket <file>`):
  ;; - runs (module+ test ...) submodules consistently
  ;; - returns non-zero on rackunit failures
  ;; - avoids false file-level failures from helper/entry modules
  (define raco-bin (find-executable-path "raco"))
  (define args (list raco-bin "test" resolved-path))

  (define t0 (current-inexact-milliseconds))

  (define proc-result (apply process*/ports stdout-out #f stderr-out args))
  (define sp (first proc-result))
  (define out-in (second proc-result))
  (define pid (third proc-result))
  (define err-in (fourth proc-result))
  (define ctrl (fifth proc-result))

  ;; Close ports we don't need
  (when out-in
    (close-output-port out-in))
  (when err-in
    (close-input-port err-in))

  ;; Wait for process with optional timeout
  (define elapsed (lambda () (exact-round (- (current-inexact-milliseconds) t0))))

  (cond
    [timeout
     (define wait-thread (thread (lambda () (ctrl 'wait))))
     (define timed-out? (not (sync/timeout (/ timeout 1000.0) wait-thread)))
     (cond
       [timed-out?
        (ctrl 'kill)
        (thread-wait wait-thread)
        (test-file-result test-path
                          2 ; timeout exit code
                          (get-output-bytes stdout-out)
                          (get-output-bytes stderr-out)
                          (elapsed)
                          0
                          0
                          0)]
       [else
        (define exit-code (ctrl 'exit-code))
        (define stdout-bytes (get-output-bytes stdout-out))
        (define stderr-bytes (get-output-bytes stderr-out))
        ;; v0.45.8 (NF14): Merge stdout+stderr — rackunit text-ui outputs to stderr
        (define merged-bytes (bytes-append stdout-bytes stderr-bytes))
        (define-values (passed failed total) (parse-raco-output merged-bytes))
        (test-file-result test-path
                          exit-code
                          stdout-bytes
                          stderr-bytes
                          (elapsed)
                          passed
                          failed
                          total)])]
    [else
     (ctrl 'wait)
     (define exit-code (ctrl 'exit-code))
     (define stdout-bytes (get-output-bytes stdout-out))
     (define stderr-bytes (get-output-bytes stderr-out))
     ;; v0.45.8 (NF14): Merge stdout+stderr — rackunit text-ui outputs to stderr
     (define merged-bytes (bytes-append stdout-bytes stderr-bytes))
     (define-values (passed failed total) (parse-raco-output merged-bytes))
     (test-file-result test-path exit-code stdout-bytes stderr-bytes (elapsed) passed failed total)]))

;; ---------------------------------------------------------------------------
;; run-all-files - parallel dispatcher with progress dots
;; ---------------------------------------------------------------------------

(define (run-all-files files jobs timeout)
  (define sema (make-semaphore jobs))
  (define results (box '()))
  (define results-lock (make-semaphore 1))

  (define (add-result! r)
    (call-with-semaphore results-lock (lambda () (set-box! results (cons r (unbox results))))))

  (define threads
    (for/list ([f (in-list files)])
      (thread (lambda ()
                (call-with-semaphore sema
                                     (lambda ()
                                       (define result
                                         (run-single-file f #:timeout (or timeout 120000)))
                                       (define exit-code (test-file-result-exit-code result))
                                       (cond
                                         [(= exit-code 0) (display ".")]
                                         [(= exit-code 2) (display "T")]
                                         [else (display "F")])
                                       (flush-output)
                                       (add-result! result)))))))

  ;; Wait for all threads
  (for-each thread-wait threads)
  (newline)

  ;; Return results in original file order
  (define file-order
    (for/hash ([f (in-list files)]
               [i (in-naturals)])
      (values f i)))
  (sort (unbox results) < #:key (lambda (r) (hash-ref file-order (test-file-result-path r) 0))))

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
  (displayln "  --suite <name>    Run test suite: all (default), fast, slow, tui, smoke")
  (displayln "  --help            Show this help message")
  (newline)
  (displayln "Suites:")
  (displayln "  all     Entire tests/ directory (per-file spawn)")
  (displayln "  fast    All tests except slow patterns (per-file spawn)")
  (displayln "  slow    Only sandbox/subprocess tests")
  (displayln "  tui     Files in tests/tui/")
  (displayln "  smoke   Fast minus workflows/, interfaces/, and provider tests"))

(define (parse-args args)
  (let loop ([rest args]
             [jobs (processor-count)]
             [sequential? #f]
             [timeout #f]
             [suite 'all]
             [extra '()])
    (match rest
      ['() (values jobs sequential? timeout suite (reverse extra))]
      [(list "--help" _ ...)
       (usage)
       (exit 0)]
      [(list "--jobs" n rest ...) (loop rest (string->number n) sequential? timeout suite extra)]
      [(list "--sequential" rest ...) (loop rest 1 #t timeout suite extra)]
      [(list "--timeout" secs rest ...)
       (loop rest jobs sequential? (string->number secs) suite extra)]
      [(list "--suite" name rest ...)
       (loop rest jobs sequential? timeout (string->symbol name) extra)]
      [(list arg rest ...) (loop rest jobs sequential? timeout suite (cons arg extra))])))

;; ---------------------------------------------------------------------------
;; Pre-flight: stale bytecode cleanup
;; ---------------------------------------------------------------------------

(define (clean-stale-bytecode! root)
  "Delete compiled/ directories when any .zo is older than its source .rkt.
  This prevents instantiate-linklet mismatches after module moves or renames."
  (define cleaned 0)
  (for ([d (in-directory root)])
    (when (and (directory-exists? d) (equal? (path->string (file-name-from-path d)) "compiled"))
      (define zo-files (directory-list d #:build? #t))
      (define stale?
        (for/or ([zo (in-list zo-files)])
          (and (file-exists? zo)
               (let* ([base (path-replace-suffix zo "")]
                      [rkt (path-replace-extension base #".rkt")]
                      [rktl (path-replace-extension base #".rktl")]
                      [src (or (and (file-exists? rkt) rkt) (and (file-exists? rktl) rktl) #f)])
                 (and src
                      (> (file-or-directory-modify-seconds src)
                         (file-or-directory-modify-seconds zo)))))))
      (when stale?
        (delete-directory/files d)
        (set! cleaned (add1 cleaned)))))
  cleaned)

;; ---------------------------------------------------------------------------
;; Main entry point
;; ---------------------------------------------------------------------------

(define (main args)
  (define-values (jobs sequential? timeout suite extra-files) (parse-args args))

  ;; Pre-flight: clear stale bytecode to avoid linklet mismatches
  (define cleaned-dirs (clean-stale-bytecode! (build-path (current-directory) "..")))
  (when (> cleaned-dirs 0)
    (printf ";; run-tests: cleaned ~a stale compiled/ director~a~n"
            cleaned-dirs
            (if (= cleaned-dirs 1) "y" "ies")))

  (define suite-files
    (if (pair? extra-files)
        extra-files
        (collect-test-files suite)))

  (unless (pair? suite-files)
    (displayln "No test files matched the selected suite.")
    (exit 1))

  ;; Per-file spawning with result tracking
  (define timeout-ms (and timeout (* timeout 1000)))
  (define suite-label (symbol->string suite))
  (define n-files (length suite-files))

  (printf ";; run-tests: suite=~a files=~a jobs=~a sequential=~a~n"
          suite-label
          n-files
          jobs
          sequential?)
  (newline)

  (define t0 (current-inexact-milliseconds))
  (define results (run-all-files suite-files jobs timeout-ms))
  (define total-elapsed (exact-round (- (current-inexact-milliseconds) t0)))

  (print-summary results total-elapsed)
  (save-failure-logs results)

  (define failed-files
    (count (lambda (r)
             (and (not (= (test-file-result-exit-code r) 0))
                  (not (= (test-file-result-exit-code r) 2))))
           results))
  (define timeout-files (count (lambda (r) (= (test-file-result-exit-code r) 2)) results))

  (exit (summary-exit-code failed-files timeout-files)))

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
