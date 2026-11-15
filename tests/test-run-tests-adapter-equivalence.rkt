#lang racket

;; @speed fast
;; @suite default
;; @boundary integration
;; @isolation process

;; tests/test-run-tests-adapter-equivalence.rkt
;;
;; Phase D step 1 contract: `scripts/run-tests.rkt` is a COMPATIBILITY ADAPTER
;; over the `./qt full` CI engine (scripts/testing/full-plan.rkt), not a second
;; test runner.
;;
;; Pinned here:
;;   1. the argv translation table — every CI-surface flag lands in exactly one
;;      `qt-ci-spec` field, and the compat-only values (strict?, execution
;;      width) stay compat values instead of becoming spec fields;
;;   2. ONE selection implementation — for the same flags both front-ends
;;      resolve the SAME root-relative selection and the SAME shard slice via
;;      `resolve-qt-ci-plan`; there is no second selection engine;
;;   3. ONE execution/reporting path — the facade's entry point calls
;;      `resolve-qt-ci-plan`/`run-qt-ci-plan` (the exact exports `./qt` calls)
;;      and never calls runner.rkt's `main`; the compatibility presentation
;;      switch is what keeps the compat stdout and JSON free of qt-only output;
;;   4. the compat CLI stays byte-compatible at a cheap scale for the eight
;;      matrix cases: sharded selection + `--json-out` (runner schema, no
;;      `extra.qt` marker), a positional file with `--mode subprocess`,
;;      `--timeout` forwarding, an invalid flag (same message and exit code),
;;      `--shard-plan report`, `--repeat 2`, `--record-gate-evidence` and
;;      `--ledger`.
;;
;; Scale note: selecting a suite in THIS repository parses every test file's
;; metadata (~20 s per process), so suite+shard equality is pinned against a
;; temporary fixture root and the execution cases use one real test file. The
;; repo-wide matrix (fast suite shard 0/40, `--suite security --timeout`, …) was
;; diffed byte-for-byte before/after the refactor and is recorded in the wave
;; report.
;;
;; These are top-level test-case forms (this repository's convention), so the
;; file fails as a plain `racket tests/…` run and under `raco test -t` alike.

(require rackunit
         racket/file
         racket/list
         racket/path
         racket/port
         racket/string
         json
         racket/runtime-path
         (only-in "../scripts/run-tests.rkt"
                  parse-compat-argv
                  compat-args?
                  compat-args-jobs
                  compat-args-sequential?
                  compat-args-strict?
                  compat-args->ci-spec)
         (only-in "../scripts/testing/full-plan.rkt"
                  qt-ci-spec
                  qt-ci-spec-suite
                  qt-ci-spec-files
                  qt-ci-spec-shard-index
                  qt-ci-spec-shard-total
                  qt-ci-spec-timeout
                  qt-ci-spec-profile
                  qt-ci-spec-ledger-path
                  qt-ci-spec-json-out
                  qt-ci-spec-record-gate?
                  qt-ci-spec-repeat
                  qt-ci-spec-mode
                  qt-ci-spec-shard-plan
                  qt-ci-spec-durations
                  qt-ci-spec-argv
                  resolve-qt-ci-plan
                  qt-ci-plan-rels
                  qt-ci-plan-shard-index
                  qt-ci-plan-shard-total
                  qt-ci-run-files
                  qt-ci-presentation)
         (only-in "../scripts/run-tests/reporting.rkt" validate-run-summary-json)
         (only-in "helpers/fixtures.rkt" with-temp-dir))

(define-runtime-path root "..")
(define-runtime-path facade-path "../scripts/run-tests.rkt")
(define-runtime-path runner-path "../scripts/run-tests/runner.rkt")
(define-runtime-path qt-path "../qt")

;; ── invocation ───────────────────────────────────────────────────────
;; Both pipes are read while the child runs, so neither can block the other.
(define (invoke-command timeout cwd . args)
  (define cust (make-custodian))
  (define proc #f)
  (dynamic-wind void
                (lambda ()
                  (parameterize ([current-custodian cust]
                                 [current-directory cwd]
                                 [subprocess-group-enabled #t])
                    (define-values (child out in err) (apply subprocess #f #f #f args))
                    (set! proc child)
                    (close-output-port in)
                    (define stdout #f)
                    (define stderr #f)
                    (define out-thread (thread (lambda () (set! stdout (port->string out)))))
                    (define err-thread (thread (lambda () (set! stderr (port->string err)))))
                    (define done
                      (thread (lambda ()
                                (subprocess-wait child)
                                (thread-wait out-thread)
                                (thread-wait err-thread))))
                    (unless (sync/timeout timeout done)
                      (error 'invoke-command "subprocess timed out after ~a seconds" timeout))
                    (values (subprocess-status child) stdout stderr)))
                (lambda ()
                  (when proc
                    (when (eq? (subprocess-status proc) 'running)
                      (subprocess-kill proc #t))
                    (subprocess-wait proc))
                  (custodian-shutdown-all cust))))

(define (invoke . args)
  (apply invoke-command 300 root (or (find-executable-path "racket") "racket") args))

;; ── helpers ──────────────────────────────────────────────────────────
(define (write-file path text)
  (call-with-output-file path #:exists 'truncate (lambda (out) (display text out))))

;; A fixture q-root containing only <root>/tests/<file>; both front-ends resolve
;; their selection against exactly this tree.
(define (with-fixture-root files proc)
  (with-temp-dir (lambda (dir)
                   (make-directory (build-path dir "tests"))
                   (for ([file (in-list files)])
                     (write-file (build-path dir "tests" (car file)) (cdr file)))
                   (proc dir))))

(define (stdout-lines out)
  (string-split out "\n"))

;; Presence test (not a predicate returning the line: this rackunit's
;; `check-true` requires an actual #t).
(define (line-with-prefix out prefix)
  (for/or ([line (in-list (stdout-lines out))]
           #:when (string-prefix? line prefix))
    #t))

(define (count-lines out text)
  (for/sum ([line (in-list (stdout-lines out))] #:when (string=? line text)) 1))

(define (run-summary doc)
  (hash-ref doc 'run_summary #f))

(define (json-extra doc)
  (hash-ref doc 'extra #f))

(define (read-json-file path)
  (call-with-input-file path read-json))

;; The qt CI spec for the same flags, spelled exactly like `./qt`'s
;; `parse-options` -> `qt-ci-spec` call.
(define (qt-spec #:suite [suite #f]
                 #:files [files '()]
                 #:shard-index [shard-index 0]
                 #:shard-total [shard-total 1]
                 #:timeout [timeout #f]
                 #:profile [profile 'local]
                 #:ledger [ledger #f]
                 #:json-out [json-out #f]
                 #:record-gate? [record-gate? #f]
                 #:repeat [repeat 1]
                 #:mode [mode 'auto]
                 #:shard-plan [shard-plan #f]
                 #:durations [durations #f]
                 #:argv [argv '("full")])
  (qt-ci-spec suite
              files
              shard-index
              shard-total
              timeout
              profile
              ledger
              json-out
              record-gate?
              repeat
              mode
              shard-plan
              durations
              argv))

(define passing-fixture
  (cons
   "pass.rkt"
   "#lang racket/base\n;; @speed fast\n;; @suite unit\n(require rackunit)\n(module+ test\n  (check-true #t))\n"))

(define second-fixture
  (cons
   "second.rkt"
   "#lang racket/base\n;; @speed fast\n;; @suite unit\n(require rackunit)\n(module+ test (check-true #t))\n"))

(define third-fixture
  (cons
   "third.rkt"
   "#lang racket/base\n;; @speed fast\n;; @suite unit\n(require rackunit)\n(module+ test (check-true #t))\n"))

(define slow-fixture
  (cons
   "slow.rkt"
   "#lang racket/base\n;; @speed slow\n;; @suite slow\n(require rackunit)\n(module+ test (check-true #t))\n"))

(define tui-fixture
  (cons
   "tui.rkt"
   "#lang racket/base\n;; @speed fast\n;; @suite tui\n(require rackunit)\n(module+ test (check-true #t))\n"))

;; ── 1. the translation table ─────────────────────────────────────────
(test-case "every CI-surface flag maps to exactly one qt-ci-spec field"
  (define argv
    (list "--suite"
          "fast"
          "--shard-index"
          "2"
          "--shard-total"
          "5"
          "--timeout"
          "90"
          "--profile"
          "ci"
          "--ledger"
          "ledger.json"
          "--json-out"
          "res.json"
          "--record-gate-evidence"
          "--repeat"
          "3"
          "--mode"
          "subprocess"
          "--shard-plan"
          "active"
          "--durations"
          "dur.json"
          "--jobs"
          "6"))
  (define a (parse-compat-argv argv))
  (define spec (compat-args->ci-spec a))
  (check-true (compat-args? a))
  (check-equal? (qt-ci-spec-suite spec) 'fast)
  (check-equal? (qt-ci-spec-files spec) '() "no positional files in this argv")
  (check-equal? (qt-ci-spec-shard-index spec) 2)
  (check-equal? (qt-ci-spec-shard-total spec) 5)
  (check-equal? (qt-ci-spec-timeout spec) 90)
  (check-equal? (qt-ci-spec-profile spec) 'ci)
  (check-equal? (qt-ci-spec-ledger-path spec) "ledger.json")
  (check-equal? (qt-ci-spec-json-out spec) "res.json")
  (check-true (qt-ci-spec-record-gate? spec))
  (check-equal? (qt-ci-spec-repeat spec) 3)
  (check-equal? (qt-ci-spec-mode spec) 'subprocess)
  (check-equal? (qt-ci-spec-shard-plan spec) "active")
  (check-equal? (qt-ci-spec-durations spec) "dur.json")
  ;; gate-evidence provenance stays the compat argv itself: the runner has
  ;; always recorded the argv it was invoked with.
  (check-equal? (qt-ci-spec-argv spec) argv)
  ;; execution width and strictness are NOT spec fields — the engine takes the
  ;; width like `./qt full --jobs`, and the compat CLI owns strictness (its
  ;; long-standing default is ON, while qt derives it from STRICT_TEST_RUNNER).
  (check-equal? (compat-args-jobs a) 6)
  (check-false (compat-args-sequential? a))
  (check-true (compat-args-strict? a))
  (check-equal? (compat-args-jobs (parse-compat-argv (list "--sequential"))) 1)
  (check-true (compat-args-sequential? (parse-compat-argv (list "--sequential")))))

(test-case "an explicit file list replaces the suite, exactly like the runner"
  (define with-file
    (compat-args->ci-spec (parse-compat-argv (list "--suite" "fast" "tests/pass.rkt"))))
  (check-equal? (qt-ci-spec-files with-file) '("tests/pass.rkt"))
  (check-equal? (qt-ci-spec-suite with-file)
                #f
                "the runner ignores --suite when positional files are given")
  (check-equal? (qt-ci-spec-suite (compat-args->ci-spec (parse-compat-argv (list "--suite" "fast"))))
                'fast))

;; ── 2. ONE selection implementation ──────────────────────────────────
(test-case "both front-ends resolve the same suite+shard selection"
  (with-fixture-root
   (list passing-fixture second-fixture third-fixture slow-fixture tui-fixture)
   (lambda (dir)
     (for* ([total (in-list '(1 2 3))]
            [index (in-range total)])
       (define compat-plan
         (resolve-qt-ci-plan dir
                             (compat-args->ci-spec (parse-compat-argv (list "--suite"
                                                                            "fast"
                                                                            "--shard-index"
                                                                            (number->string index)
                                                                            "--shard-total"
                                                                            (number->string total))))
                             #:contract 'compat))
       (define qt-plan
         (resolve-qt-ci-plan dir (qt-spec #:suite 'fast #:shard-index index #:shard-total total)))
       (check-equal? (qt-ci-plan-rels compat-plan)
                     (qt-ci-plan-rels qt-plan)
                     (format "unsharded selection: --suite fast --shard-total ~a" total))
       (check-equal? (qt-ci-run-files compat-plan)
                     (qt-ci-run-files qt-plan)
                     (format "shard slice: ~a of ~a" index total))
       (check-equal? (qt-ci-plan-shard-index compat-plan) (qt-ci-plan-shard-index qt-plan))
       (check-equal? (qt-ci-plan-shard-total compat-plan) (qt-ci-plan-shard-total qt-plan)))
     ;; the suite predicate is classify.rkt's own, not a second registry
     (check-equal? (qt-ci-plan-rels (resolve-qt-ci-plan dir
                                                        (compat-args->ci-spec
                                                         (parse-compat-argv (list "--suite" "fast")))
                                                        #:contract 'compat))
                   (qt-ci-plan-rels (resolve-qt-ci-plan dir (qt-spec #:suite 'fast)))))))

(test-case "both front-ends resolve the same explicit-file selection"
  (with-fixture-root
   (list passing-fixture second-fixture)
   (lambda (dir)
     (define compat-plan
       (resolve-qt-ci-plan dir
                           (compat-args->ci-spec (parse-compat-argv (list "tests/second.rkt")))
                           #:contract 'compat))
     (define qt-plan (resolve-qt-ci-plan dir (qt-spec #:files '("tests/second.rkt"))))
     (check-equal? (qt-ci-plan-rels compat-plan) '("tests/second.rkt"))
     (check-equal? (qt-ci-plan-rels compat-plan) (qt-ci-plan-rels qt-plan)))))

;; ── 3. no second execution engine ────────────────────────────────────
(test-case "the adapter delegates to the shared engine and owns no runner main"
  (define facade (file->string facade-path))
  (define runner (file->string runner-path))
  (define qt-source (file->string qt-path))
  ;; the facade's entry point calls the exact exports ./qt calls…
  (check-true (string-contains? facade "(resolve-qt-ci-plan #f spec #:contract 'compat)")
              "selection goes through the shared resolver")
  (check-true (string-contains? facade "(run-qt-ci-plan plan (compat-args-jobs a)")
              "execution goes through the shared engine")
  ;; …and it never reaches runner.rkt's own command-line main.
  (check-false (string-contains? facade "(main (vector->list")
               "the facade must not call runner.rkt's main")
  (check-false (regexp-match? #px"\\(main\\s" facade)
               "the facade must not dispatch to a runner main at all")
  ;; ./qt resolves and executes through the same two exports.
  (check-true (string-contains? qt-source "(resolve-qt-ci-plan #f spec)")
              "./qt must call the shared resolver")
  (check-true (string-contains? qt-source "(run-qt-ci-plan plan (cli-options-jobs opts))")
              "./qt must call the shared engine")
  ;; the compatibility presentation is switched off for the whole compat run,
  ;; which is what keeps the compat stdout and JSON free of qt-only output.
  (check-true (string-contains? facade "(parameterize ([qt-ci-presentation #f])"))
  ;; the autorun guard is path-based now, so loading the facade cannot launch
  ;; the runner's main: the compat entry point owns the CLI.
  (check-true (string-contains? runner "variable-reference->module-source"))
  (check-false (string-contains? runner "(equal? (path->string base) \"run-tests.rkt\")")
               "the facade must not be the runner's autorun trigger")
  ;; Requiring the facade (this file does) did not run a suite: the process is
  ;; still here and no summary was printed.
  (check-true (parameter? qt-ci-presentation)))

;; ── 4. the eight matrix cases at a cheap scale ───────────────────────
(test-case "case 1: sharded selection + --json-out writes the runner schema only"
  (with-temp-dir (lambda (dir)
                   (define out-path (build-path dir "results.json"))
                   (define-values (status out err)
                     (invoke "scripts/run-tests.rkt"
                             "--jobs"
                             "1"
                             "--json-out"
                             (path->string out-path)
                             "--shard-index"
                             "0"
                             "--shard-total"
                             "40"
                             "tests/test-version.rkt"))
                   (check-equal? status 0 (string-append out err))
                   ;; compat presentation: the runner's own lines, no qt ones
                   (check-true (line-with-prefix out ";; run-tests: sharding 1 files — shard 0/40"))
                   (check-false (line-with-prefix out ";; qt:"))
                   (check-false (line-with-prefix out "full passed:"))
                   (check-true (line-with-prefix out "                    TEST SUMMARY"))
                   (define doc (read-json-file out-path))
                   (check-equal? (validate-run-summary-json doc) '())
                   (check-equal? (hash-ref doc 'suite) "all")
                   (check-equal? (hash-ref (run-summary doc) 'shard) (hasheq 'index 0 'total 40))
                   (check-equal? (hash-ref (hash-ref doc 'summary) 'files_total) 1)
                   ;; the schema-additive qt marker belongs to ./qt only
                   (check-false (hash-has-key? (json-extra doc) 'qt)))))

(test-case "case 2: one positional file + --json-out + --mode subprocess"
  (with-temp-dir
   (lambda (dir)
     (define out-path (build-path dir "repro.json"))
     (define-values (status out err)
       (invoke "scripts/run-tests.rkt"
               "tests/test-version.rkt"
               "--json-out"
               (path->string out-path)
               "--mode"
               "subprocess"))
     (check-equal? status 0 (string-append out err))
     (check-true (line-with-prefix out "  Files:     1 total, 1 passed, 0 failed, 0 timeouts"))
     (define doc (read-json-file out-path))
     (check-equal? (validate-run-summary-json doc) '())
     (check-equal? (hash-ref (run-summary doc) 'execution_mode) "subprocess")
     (check-equal? (hash-ref (run-summary doc) 'suite) "all")
     (check-equal? (map (lambda (f) (hash-ref f 'path)) (hash-ref doc 'files))
                   '("tests/test-version.rkt")))))

(test-case "case 3: --timeout is accepted and forwarded"
  (define-values (status out err)
    (invoke "scripts/run-tests.rkt" "tests/test-version.rkt" "--timeout" "120" "--jobs" "1"))
  (check-equal? status 0 (string-append out err))
  (check-false (regexp-match? #rx"timed out" out))
  ;; and the value lands in the shared spec unchanged
  (check-equal?
   (qt-ci-spec-timeout (compat-args->ci-spec
                        (parse-compat-argv (list "--timeout" "120" "tests/test-version.rkt"))))
   120))

(test-case "case 4: an invalid flag fails identically (usage + exit 2)"
  (define-values (status out err) (invoke "scripts/run-tests.rkt" "--not-a-flag"))
  (check-equal? status 2)
  (check-equal? (string-trim err) "run-tests: unknown flag: --not-a-flag")
  (check-true (string-contains? out
                                "Usage: racket scripts/run-tests.rkt [OPTIONS] [TEST-FILES ...]")))

(test-case "case 5: the deleted plan modes are rejected by the surviving CLI"
  (for ([mode (in-list '("report" "measure"))])
    (define-values (status out err)
      (invoke "scripts/run-tests.rkt"
              "--shard-plan"
              mode
              "--shard-total"
              "2"
              "tests/test-version.rkt"))
    (check-equal? status 1 (string-append out err))
    (check-true (string-contains? err "unknown --shard-plan mode"))
    ;; a rejected flag never executes a test
    (check-false (line-with-prefix out "                    TEST SUMMARY"))))

(test-case "case 6: --repeat 2 runs twice and prints both summaries"
  (define-values (status out err)
    (invoke "scripts/run-tests.rkt" "--repeat" "2" "tests/test-version.rkt"))
  (check-equal? status 0 (string-append out err))
  (check-equal? (count-lines out "                    TEST SUMMARY") 2)
  (check-true (line-with-prefix out ";; ── Run 1/2 ──"))
  (check-true (line-with-prefix out ";; ── Run 2/2 ──"))
  (check-true (line-with-prefix out ";; Run 2/2: PASS")))

(test-case "case 7: --record-gate-evidence records the runner's evidence shape"
  (define evidence (build-path root ".gate-evidence" "all.json"))
  (define before (and (file-exists? evidence) (file->bytes evidence)))
  (dynamic-wind
   void
   (lambda ()
     (define-values (status out err)
       (invoke "scripts/run-tests.rkt" "--record-gate-evidence" "tests/test-version.rkt"))
     (check-equal? status 0 (string-append out err))
     (check-true (line-with-prefix out ";; gate evidence v2 recorded: all"))
     (check-true (file-exists? evidence))
     (define ev (read-json-file evidence))
     (check-equal? (hash-ref ev 'suite) "all")
     (check-equal? (hash-ref ev 'shard_total) 1)
     (check-equal? (hash-ref ev 'selected_file_count) 1)
     (check-true (positive? (hash-ref ev 'parsed_test_count)))
     ;; gate-evidence provenance is the compat argv, not a synthesized one
     (check-equal? (hash-ref ev 'args) '("--record-gate-evidence" "tests/test-version.rkt")))
   (lambda ()
     (cond
       [before
        (call-with-output-file evidence #:exists 'truncate (lambda (out) (write-bytes before out)))]
       [(file-exists? evidence) (delete-file evidence)]
       [else (void)]))))

(test-case "case 8: --ledger with a fixture ledger prints the ledger summary"
  (with-temp-dir
   (lambda (dir)
     (define ledger (build-path dir "ledger.json"))
     (write-file ledger "{\"entries\": []}\n")
     (define-values (status out err)
       (invoke "scripts/run-tests.rkt" "--ledger" (path->string ledger) "tests/test-version.rkt"))
     (check-equal? status 0 (string-append out err))
     (check-true (line-with-prefix out "KNOWN-FAILURE LEDGER"))
     (check-true (line-with-prefix out "  Known failures: 0"))
     (check-true (line-with-prefix out "  New failures: 0")))))
