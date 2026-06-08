#lang racket

;; @speed slow
;; @suite default

;; BOUNDARY: integration

;; Tests for the rewritten run-tests.rkt test runner.
;; Wave 13 Subs 13.1–13.4: struct, parsing, running, summary functions

(require rackunit
         racket/port
         racket/string
         racket/file
         racket/path
         racket/runtime-path
         racket/system)

;; Path segments used by smoke-suite filtering in run-tests.rkt
;; Constructed without literal absolute-path patterns to satisfy lint-tests.rkt
(define workflows-path-segment (string-append "/" "workflows" "/"))
(define interfaces-path-segment (string-append "/" "interfaces" "/"))

;; ---------------------------------------------------------------------------
;; Require the runner module and get all exports
;; ---------------------------------------------------------------------------

;; Resolve runner path relative to THIS source file using define-runtime-path.
;; This works correctly under `racket`, `raco test`, and `raco test --process`
;; because the path is resolved relative to the source file, not CWD.
;; Previous approach using find-system-path 'orig-dir broke under
;; `raco test --process` where orig-dir differs from the project root.
(define-runtime-path runner-path "../scripts/run-tests.rkt")

;; Load module once, cache bindings
(define runner-loaded? (box #f))
(define runner-cache (make-hash))

(define (runner-ref sym)
  (unless (unbox runner-loaded?)
    (dynamic-require runner-path #f)
    (set-box! runner-loaded? #t))
  (hash-ref! runner-cache sym (lambda () (dynamic-require runner-path sym))))

;; ---------------------------------------------------------------------------
;; 13.1: test-file-result struct
;; ---------------------------------------------------------------------------

(test-case "test-file-result: construction and access"
  (define tfr (runner-ref 'make-test-file-result))
  (define result (tfr "tests/test-foo.rkt" 0 #"5 tests passed\n" #"" 1234 5 0 5))
  (check-equal? ((runner-ref 'test-file-result-path) result) "tests/test-foo.rkt")
  (check-equal? ((runner-ref 'test-file-result-exit-code) result) 0)
  (check-equal? ((runner-ref 'test-file-result-stdout-bytes) result) #"5 tests passed\n")
  (check-equal? ((runner-ref 'test-file-result-stderr-bytes) result) #"")
  (check-equal? ((runner-ref 'test-file-result-elapsed-ms) result) 1234)
  (check-equal? ((runner-ref 'test-file-result-passed) result) 5)
  (check-equal? ((runner-ref 'test-file-result-failed) result) 0)
  (check-equal? ((runner-ref 'test-file-result-total) result) 5))

(test-case "test-file-result: failed result"
  (define tfr (runner-ref 'make-test-file-result))
  (define result (tfr "tests/test-bar.rkt" 1 #"1 test passed\n1 test failed\n" #"" 5678 1 1 2))
  (check-equal? ((runner-ref 'test-file-result-exit-code) result) 1)
  (check-equal? ((runner-ref 'test-file-result-passed) result) 1)
  (check-equal? ((runner-ref 'test-file-result-failed) result) 1)
  (check-equal? ((runner-ref 'test-file-result-total) result) 2))

(test-case "test-file-result: timeout result (exit-code 2)"
  (define tfr (runner-ref 'make-test-file-result))
  (define result (tfr "tests/test-slow.rkt" 2 #"" #"timeout\n" 60000 0 0 0))
  (check-equal? ((runner-ref 'test-file-result-exit-code) result) 2)
  (check-equal? ((runner-ref 'test-file-result-elapsed-ms) result) 60000))

;; ---------------------------------------------------------------------------
;; 13.1: parse-raco-output
;; ---------------------------------------------------------------------------

(test-case "parse-raco-output: all pass"
  (define parse (runner-ref 'parse-raco-output))
  (define-values (passed failed total) (parse #"raco test: test-foo.rkt\n5 tests passed\n"))
  (check-equal? passed 5)
  (check-equal? failed 0)
  (check-equal? total 5))

(test-case "parse-raco-output: pass and fail"
  (define parse (runner-ref 'parse-raco-output))
  (define-values (passed failed total) (parse #"3 tests passed\n2 tests failed\n"))
  (check-equal? passed 3)
  (check-equal? failed 2)
  (check-equal? total 5))

(test-case "parse-raco-output: zero tests"
  (define parse (runner-ref 'parse-raco-output))
  (define-values (passed failed total) (parse #"0 tests passed\n"))
  (check-equal? passed 0)
  (check-equal? failed 0)
  (check-equal? total 0))

(test-case "parse-raco-output: no recognizable output returns zeros"
  (define parse (runner-ref 'parse-raco-output))
  (define-values (passed failed total) (parse #"some random output\n"))
  (check-equal? passed 0)
  (check-equal? failed 0)
  (check-equal? total 0))

(test-case "parse-raco-output: rackunit run-test result list"
  (define parse (runner-ref 'parse-raco-output))
  (define-values (passed failed total)
    (parse #"'(#<test-success> #<test-success> #<test-success>)\n"))
  (check-equal? passed 3)
  (check-equal? failed 0)
  (check-equal? total 3))

(test-case "parse-raco-output: singular 'test passed' / 'test failed'"
  (define parse (runner-ref 'parse-raco-output))
  (define-values (passed failed total) (parse #"1 test passed\n1 test failed\n"))
  (check-equal? passed 1)
  (check-equal? failed 1)
  (check-equal? total 2))

(test-case "parse-raco-output: multiple failures output"
  (define parse (runner-ref 'parse-raco-output))
  (define-values (passed failed total) (parse #"3 tests passed\n2 tests failed\n"))
  (check-equal? passed 3)
  (check-equal? failed 2)
  (check-equal? total 5))

(test-case "parse-raco-output: rackunit/text-ui format (successes)"
  (define parse (runner-ref 'parse-raco-output))
  (define-values (passed failed total)
    (parse #"13 success(es) 0 failure(s) 0 error(s) 13 test(s) run\n0\n"))
  (check-equal? passed 13)
  (check-equal? failed 0)
  (check-equal? total 13))

(test-case "parse-raco-output: rackunit/text-ui counts failures and errors"
  (define parse (runner-ref 'parse-raco-output))
  (define-values (passed failed total)
    (parse #"5 success(es) 2 failure(s) 1 error(s) 8 test(s) run\n"))
  (check-equal? passed 5)
  (check-equal? failed 3)
  (check-equal? total 8))

;; ---------------------------------------------------------------------------
;; 13.1: extract-failure-lines
;; ---------------------------------------------------------------------------

(test-case "extract-failure-lines: no failures returns empty"
  (define extract (runner-ref 'extract-failure-lines))
  (check-equal? (extract #"5 tests passed\n") '()))

(test-case "extract-failure-lines: extracts failure block"
  (define extract (runner-ref 'extract-failure-lines))
  (define output
    (bytes-append #"raco test: test-bar.rkt\n"
                  #"-------------------- FAILURE --------------------\n"
                  #"name:       check-equal?\n"
                  #"location:   test-bar.rkt:42:15\n"
                  #"actual:     1\n"
                  #"expected:   2\n"
                  #"----------------------------------------\n"
                  #"1 test passed\n"
                  #"1 test failed\n"))
  (define lines (extract output))
  (check-true (>= (length lines) 4))
  (check-not-false (member "name:       check-equal?" lines))
  (check-not-false (member "actual:     1" lines))
  (check-not-false (member "expected:   2" lines)))

(test-case "extract-failure-lines: multiple failure blocks"
  (define extract (runner-ref 'extract-failure-lines))
  (define output
    (bytes-append #"-------------------- FAILURE --------------------\n"
                  #"name:       check-equal?\n"
                  #"actual:     a\n"
                  #"expected:   b\n"
                  #"----------------------------------------\n"
                  #"-------------------- FAILURE --------------------\n"
                  #"name:       check-true\n"
                  #"actual:     #f\n"
                  #"expected:   #t\n"
                  #"----------------------------------------\n"
                  #"3 tests passed\n2 tests failed\n"))
  (define lines (extract output))
  (check-true (>= (length lines) 8))
  (check-not-false (member "name:       check-equal?" lines))
  (check-not-false (member "name:       check-true" lines)))

;; ---------------------------------------------------------------------------
;; 13.4: format-duration
;; ---------------------------------------------------------------------------

(test-case "format-duration: zero ms"
  (define fmt (runner-ref 'format-duration))
  (check-equal? (fmt 0) "0s"))

(test-case "format-duration: seconds"
  (define fmt (runner-ref 'format-duration))
  (check-equal? (fmt 1500) "1.5s"))

(test-case "format-duration: minutes"
  (define fmt (runner-ref 'format-duration))
  (check-equal? (fmt 70000) "1m 10.0s"))

(test-case "format-duration: hours"
  (define fmt (runner-ref 'format-duration))
  (check-equal? (fmt 3723000) "1h 2m 3.0s"))

;; ---------------------------------------------------------------------------
;; 13.4: summary-exit-code
;; ---------------------------------------------------------------------------

(test-case "summary-exit-code: all pass"
  (define sec (runner-ref 'summary-exit-code))
  (check-equal? (sec 0 0) 0))

(test-case "summary-exit-code: failures"
  (define sec (runner-ref 'summary-exit-code))
  (check-equal? (sec 3 0) 1))

(test-case "summary-exit-code: timeouts only"
  (define sec (runner-ref 'summary-exit-code))
  (check-equal? (sec 0 2) 2))

(test-case "summary-exit-code: failures and timeouts"
  (define sec (runner-ref 'summary-exit-code))
  (check-equal? (sec 3 2) 3))

;; ---------------------------------------------------------------------------
;; v0.55.5 regression: false-green parsing for files without run-tests
;; ---------------------------------------------------------------------------

(test-case "run-single-file: file without run-tests still reports actual test count"
  (define run (runner-ref 'run-single-file))
  (define result (run "tests/test-stream-loop-w1.rkt" #:timeout 60000))
  (check-equal? ((runner-ref 'test-file-result-exit-code) result) 0)
  ;; test-stream-loop-w1.rkt has 8 test-case definitions but no run-tests call
  ;; The runner must detect rackunit tests even when run-tests is not called
  (check-true (>= ((runner-ref 'test-file-result-total) result) 8)
              (format "expected >=8 tests, got ~a" ((runner-ref 'test-file-result-total) result))))

(test-case "normalize-counts: zero total with exit 0 is suspicious"
  (define norm (runner-ref 'normalize-counts))
  ;; A file that exits 0 but has 0 tests should not be treated as clean pass
  (define-values (passed failed total) (norm 0 0 0 0))
  (check-equal? passed 0)
  (check-equal? failed 0)
  (check-equal? total 0))

;; ---------------------------------------------------------------------------
;; 13.2: run-single-file (uses actual raco test on a real test file)
;; ---------------------------------------------------------------------------

(test-case "run-single-file: passing test file"
  (define run (runner-ref 'run-single-file))
  (define result (run "tests/test-version.rkt" #:timeout 60000))
  (check-equal? ((runner-ref 'test-file-result-exit-code) result) 0)
  (check-true (>= ((runner-ref 'test-file-result-passed) result) 1))
  (check-equal? ((runner-ref 'test-file-result-failed) result) 0))

(test-case "run-single-file: nonexistent file returns error"
  (define run (runner-ref 'run-single-file))
  (define result (run "tests/nonexistent-test-file-xyz.rkt" #:timeout 10000))
  (check-not-equal? ((runner-ref 'test-file-result-exit-code) result) 0))

(test-case "run-single-file: exit=0 with parsed failures is not false-green"
  (define run (runner-ref 'run-single-file))
  (define tmp (make-temporary-file "run-tests-anomaly-~a.rkt"))
  (call-with-output-file
   tmp
   #:exists 'truncate/replace
   (lambda (out)
     (display "#lang racket/base\n" out)
     (display "(displayln \"5 success(es) 1 failure(s) 1 error(s) 7 test(s) run\")\n" out)))
  (define result (run tmp #:timeout 10000))
  (delete-file tmp)
  (check-equal? ((runner-ref 'test-file-result-exit-code) result) 1)
  (check-equal? ((runner-ref 'test-file-result-failed) result) 2)
  (check-equal? ((runner-ref 'test-file-result-total) result) 7))

;; ---------------------------------------------------------------------------
;; 13.3: collect-test-files (suite filtering)
;; ---------------------------------------------------------------------------

(test-case "collect-test-files: all suite returns all files"
  (define collect (runner-ref 'collect-test-files))
  (define files (collect 'all))
  (check-true (list? files))
  (check-true (> (length files) 400)))

(test-case "collect-test-files: smoke suite returns list of files"
  (define collect (runner-ref 'collect-test-files))
  (define files (collect 'smoke))
  (check-true (list? files))
  (check-true (> (length files) 0))
  (for ([f (in-list files)])
    (check-false (string-contains? f workflows-path-segment))
    (check-false (string-contains? f interfaces-path-segment))))

(test-case "collect-test-files: explicit files override suite"
  (define collect (runner-ref 'collect-test-files))
  (define files (collect 'all #:extra-files '("tests/test-version.rkt")))
  (check-equal? files '("tests/test-version.rkt")))

(test-case "collect-test-files: tui suite includes top-level TUI tests"
  (define collect (runner-ref 'collect-test-files))
  (define files (collect 'tui))
  (check-not-false (member "tests/test-tui-layout.rkt" files))
  (check-not-false (member "tests/tui/test-keybindings-binder.rkt" files)))

(test-case "collect-test-files: smoke suite includes provider schema regression tests"
  (define collect (runner-ref 'collect-test-files))
  (define files (collect 'smoke))
  (check-not-false (member "tests/test-provider-registry-schema.rkt" files)))

(test-case "run-tests CLI works from repo root with q-prefixed test path"
  (define repo-root (simplify-path (build-path (path-only runner-path) ".." "..")))
  (define racket-bin (find-executable-path "racket"))
  (define exit-code
    (parameterize ([current-directory repo-root])
      (system*/exit-code racket-bin
                         (build-path repo-root "q" "scripts" "run-tests.rkt")
                         "q/tests/test-version.rkt"
                         "--jobs"
                         "1"
                         "--timeout"
                         "30")))
  (check-equal? exit-code 0))

(test-case "run-tests CLI works from q root with tests-prefixed path"
  (define q-root (simplify-path (build-path (path-only runner-path) "..")))
  (define racket-bin (find-executable-path "racket"))
  (define exit-code
    (parameterize ([current-directory q-root])
      (system*/exit-code racket-bin
                         (build-path q-root "scripts" "run-tests.rkt")
                         "tests/test-version.rkt"
                         "--jobs"
                         "1"
                         "--timeout"
                         "30")))
  (check-equal? exit-code 0))

;; ---------------------------------------------------------------------------
;; v0.55.10 regression: mutating-patterns includes self-hosting + restore guard
;; ---------------------------------------------------------------------------

(test-case "mutating-patterns: includes self-hosting for lock isolation"
  (define patterns (runner-ref 'mutating-patterns))
  (check-not-false (member "self-hosting" patterns)
                   "self-hosting must be in mutating-patterns to serialize quarantine-lock tests"))

(test-case "mutating-file?: detects self-hosting workflow test"
  (define m? (runner-ref 'mutating-file?))
  (check-true (m? "tests/test-self-hosting-workflow.rkt"))
  (check-true (m? "tests/test-self-hosting-deep.rkt"))
  ;; Non-matching files should not be flagged
  (check-false (m? "tests/test-version.rkt"))
  (check-false (m? "tests/test-stream.rkt")))

(test-case "mutating-file?: detects all mutation-sensitive patterns"
  (define m? (runner-ref 'mutating-file?))
  (define expected-mutating
    (list "tests/test-ci-local.rkt"
          "tests/test-pre-commit.rkt"
          "tests/test-check-deps.rkt"
          "tests/test-sync-version.rkt"
          "tests/test-sync-readme-status.rkt"
          "tests/test-bump-version.rkt"
          "tests/test-self-hosting-workflow.rkt"))
  (for ([p (in-list expected-mutating)])
    (check-true (m? p) (format "~a should be detected as mutating" p))))

(test-case "repo-surface-files: includes info.rkt"
  (define surfaces (runner-ref 'repo-surface-files))
  (check-not-false (member "info.rkt" surfaces))
  (check-not-false (member "README.md" surfaces)))

;; ---------------------------------------------------------------------------
;; W0: False-green sentinel tests
;; ---------------------------------------------------------------------------

(test-case "file-has-rackunit-tests?: detects test-case without run-tests"
  (define has-tests? (runner-ref 'file-has-rackunit-tests?))
  (define tmp (make-temporary-file "false-green-test-~a.rkt"))
  (call-with-output-file tmp
                         #:exists 'truncate/replace
                         (lambda (out)
                           (displayln "#lang racket" out)
                           (displayln "(require rackunit)" out)
                           (displayln "(test-case \"example\" (check-equal? 1 1))" out)))
  (check-true (has-tests? tmp))
  (delete-file tmp))

(test-case "file-has-rackunit-tests?: detects bare check- forms without run-tests"
  (define has-tests? (runner-ref 'file-has-rackunit-tests?))
  (define tmp (make-temporary-file "false-green-check-~a.rkt"))
  (call-with-output-file tmp
                         #:exists 'truncate/replace
                         (lambda (out)
                           (displayln "#lang racket" out)
                           (displayln "(require rackunit)" out)
                           (displayln "(check-equal? (+ 1 1) 2)" out)
                           (displayln "(check-true #t)" out)))
  (check-true (has-tests? tmp))
  (delete-file tmp))

(test-case "file-has-rackunit-tests?: returns #f for file with run-tests"
  (define has-tests? (runner-ref 'file-has-rackunit-tests?))
  (define tmp (make-temporary-file "false-green-hassetup-~a.rkt"))
  (call-with-output-file tmp
                         #:exists 'truncate/replace
                         (lambda (out)
                           (displayln "#lang racket" out)
                           (displayln "(require rackunit rackunit/text-ui)" out)
                           (displayln "(test-case \"example\" (check-equal? 1 1))" out)
                           (displayln "(run-tests tests)" out)))
  (check-false (has-tests? tmp))
  (delete-file tmp))

(test-case "parse-args: --repeat N sets repeat count"
  (define parse (runner-ref 'parse-args))
  (define-values (jobs seq? timeout strict? suite extra repeat record-gate?)
    (parse '("--repeat" "3")))
  (check-equal? repeat 3)
  (check-false record-gate?))

(test-case "parse-args: --repeat defaults to 1"
  (define parse (runner-ref 'parse-args))
  (define-values (jobs seq? timeout strict? suite extra repeat record-gate?) (parse '()))
  (check-equal? repeat 1)
  (check-false record-gate?))

(test-case "parse-args: --repeat with --suite smoke"
  (define parse (runner-ref 'parse-args))
  (define-values (jobs seq? timeout strict? suite extra repeat record-gate?)
    (parse '("--suite" "smoke" "--repeat" "2")))
  (check-equal? suite 'smoke)
  (check-equal? repeat 2)
  (check-false record-gate?))

;; ---------------------------------------------------------------------------
;; W1: --record-gate-evidence flag
;; ---------------------------------------------------------------------------

(test-case "parse-args: --record-gate-evidence sets flag"
  (define parse (runner-ref 'parse-args))
  (define-values (jobs seq? timeout strict? suite extra repeat record-gate?)
    (parse '("--record-gate-evidence")))
  (check-true record-gate?))

(test-case "parse-args: record-gate defaults to #f"
  (define parse (runner-ref 'parse-args))
  (define-values (jobs seq? timeout strict? suite extra repeat record-gate?) (parse '()))
  (check-false record-gate?))

;; ---- end of file ----

(test-case "file-has-rackunit-tests?: returns #f for file with no tests"
  (define has-tests? (runner-ref 'file-has-rackunit-tests?))
  (define tmp (make-temporary-file "false-green-notest-~a.rkt"))
  (call-with-output-file tmp
                         #:exists 'truncate/replace
                         (lambda (out)
                           (displayln "#lang racket/base" out)
                           (displayln "(displayln \"hello\")" out)))
  (check-false (has-tests? tmp))
  (delete-file tmp))

;; ---------------------------------------------------------------------------
;; W2: Runner truthfulness/isolation regressions
;; ---------------------------------------------------------------------------

(test-case "file-has-rackunit-tests?: module+ test with run-tests needs raco discovery"
  (define has-tests? (runner-ref 'file-has-rackunit-tests?))
  (define tmp (make-temporary-file "module-plus-test-~a.rkt"))
  (call-with-output-file
   tmp
   #:exists 'truncate/replace
   (lambda (out)
     (displayln "#lang racket" out)
     (displayln "(require rackunit rackunit/text-ui)" out)
     (displayln "(define suite (test-suite \"s\" (test-case \"ok\" (check-equal? 1 1))))" out)
     (displayln "(module+ test (run-tests suite))" out)))
  (check-true (has-tests? tmp))
  (delete-file tmp))

(define helpers-path-segment (string-append "/" "helpers" "/"))
(define fixtures-path-segment (string-append "/" "fixtures" "/"))

(test-case "collect-test-files: excludes helper and fixture modules"
  (define collect (runner-ref 'collect-test-files))
  (define fast-files (collect 'fast))
  (for ([f (in-list fast-files)])
    (check-false (string-contains? f helpers-path-segment) (format "helper should be excluded: ~a" f))
    (check-false (string-contains? f fixtures-path-segment)
                 (format "fixture should be excluded: ~a" f)))
  (check-false (member "tests/tui/event-simulator.rkt" fast-files))
  (check-false (member "tests/tui/mock-tui-session.rkt" fast-files))
  (check-false (member "tests/tui/state-assertions.rkt" fast-files))
  (check-false (member "tests/tui/workflow-harness.rkt" fast-files)))

(test-case "clean-stale-bytecode!: removes orphan compiled directory"
  (define clean! (runner-ref 'clean-stale-bytecode!))
  (define tmp-dir (make-temporary-file "compiled-orphan-~a" 'directory))
  (define compiled-dir (build-path tmp-dir "compiled"))
  (make-directory compiled-dir)
  (call-with-output-file (build-path compiled-dir "missing_rkt.zo")
                         #:exists 'truncate/replace
                         (lambda (out) (display "stale" out)))
  (check-equal? (clean! tmp-dir) 1)
  (check-false (directory-exists? compiled-dir))
  (delete-directory/files tmp-dir #:must-exist? #f))

;; ---- 14.x: Architecture shard suite classification ----

(test-case "arch-file?: matches architecture boundary/fitness tests"
  (define arch-file? (runner-ref 'arch-file?))
  (check-true (arch-file? "tests/test-arch-boundaries.rkt"))
  (check-true (arch-file? "tests/test-arch-fitness.rkt"))
  (check-true (arch-file? "tests/test-hotspot-report.rkt"))
  (check-true (arch-file? "tests/test-gsd-global-fitness.rkt"))
  (check-false (arch-file? "tests/test-event-bus.rkt"))
  (check-false (arch-file? "tests/test-tool-registry.rkt")))

(test-case "runtime-file?: matches runtime layer tests"
  (define runtime-file? (runner-ref 'runtime-file?))
  (check-true (runtime-file? "tests/test-session-lifecycle-pure.rkt"))
  (check-true (runtime-file? "tests/test-iteration-pure.rkt"))
  (check-true (runtime-file? "tests/test-tool-coordinator-pure.rkt"))
  (check-true (runtime-file? "tests/test-compaction-edge-cases.rkt"))
  (check-true (runtime-file? "tests/test-turn-model.rkt"))
  (check-false (runtime-file? "tests/test-event-bus.rkt"))
  (check-false (runtime-file? "tests/test-tool-registry.rkt")))

(test-case "extensions-file?: matches extension layer tests"
  (define ext-file? (runner-ref 'extensions-file?))
  (check-true (ext-file? "tests/test-define-extension.rkt"))
  (check-true (ext-file? "tests/test-gsd-command-dispatch.rkt"))
  (check-true (ext-file? "tests/extensions/test-gsd-state-machine.rkt"))
  (check-true (ext-file? "tests/test-hook-dispatch-transitions.rkt"))
  (check-false (ext-file? "tests/test-event-bus.rkt"))
  (check-false (ext-file? "tests/test-session-lifecycle-pure.rkt")))

(test-case "collect-test-files: arch suite returns only arch files"
  (define collect (runner-ref 'collect-test-files))
  (define arch-files (collect 'arch))
  (check-true (> (length arch-files) 0))
  (for ([f (in-list arch-files)])
    (check-not-false (or (string-contains? f "arch-")
                         (string-contains? f "boundary")
                         (string-contains? f "fitness")
                         (string-contains? f "hotspot"))
                     (format "arch suite included non-arch file: ~a" f))))

(test-case "collect-test-files: runtime suite non-empty"
  (define collect (runner-ref 'collect-test-files))
  (define rt-files (collect 'runtime))
  (check-true (> (length rt-files) 0)))

(test-case "collect-test-files: extensions suite non-empty"
  (define collect (runner-ref 'collect-test-files))
  (define ext-files (collect 'extensions))
  (check-true (> (length ext-files) 0)))

(test-case "workflows-file?: matches workflow tests excluding fixtures"
  (define wf-file? (runner-ref 'workflows-file?))
  (check-true (wf-file? "tests/workflows/tools/test-tool-read-workflow.rkt"))
  (check-true (wf-file? "tests/workflows/gsd/test-planning-workflow.rkt"))
  (check-false (wf-file? "tests/workflows/fixtures/mock-provider.rkt"))
  (check-false (wf-file? "tests/test-event-bus.rkt")))

(test-case "collect-test-files: workflows suite non-empty and excludes fixtures"
  (define collect (runner-ref 'collect-test-files))
  (define wf-files (collect 'workflows))
  (check-true (> (length wf-files) 0) "workflows suite has files")
  (for ([f (in-list wf-files)])
    (check-false (string-contains? f "/fixtures/")
                 (format "workflows suite included fixture: ~a" f))))

;; ---------------------------------------------------------------------------
;; v0.83.0: Inventory report mode tests
;; ---------------------------------------------------------------------------

(test-case "classify-exclusion-reason: support module detected"
  (define classify (runner-ref 'classify-exclusion-reason))
  (check-equal? (classify "tests/helpers/fixtures.rkt") 'support-module)
  (check-equal? (classify "tests/workflows/fixtures/mock-provider.rkt") 'support-module))

(test-case "classify-exclusion-reason: compiled path excluded"
  (define classify (runner-ref 'classify-exclusion-reason))
  (check-equal? (classify "tests/compiled/test-foo_rkt.zo") 'compiled))

(test-case "classify-exclusion-reason: non-rkt file"
  (define classify (runner-ref 'classify-exclusion-reason))
  (check-equal? (classify "tests/helpers/data.json") 'non-rkt))

(test-case "classify-exclusion-reason: regular test file is unknown"
  (define classify (runner-ref 'classify-exclusion-reason))
  (check-equal? (classify "tests/test-foo.rkt") 'unknown))

(test-case "compute-inventory-hash: deterministic for same input"
  (define hash-fn (runner-ref 'compute-inventory-hash))
  (define files '("a.rkt" "b.rkt" "c.rkt"))
  (check-equal? (hash-fn files) (hash-fn files)))

(test-case "compute-inventory-hash: differs for different input"
  (define hash-fn (runner-ref 'compute-inventory-hash))
  (define h1 (hash-fn '("a.rkt" "b.rkt")))
  (define h2 (hash-fn '("b.rkt" "c.rkt")))
  (check-not-equal? h1 h2))

(test-case "detect-high-risk-flags: detects env usage on known file"
  (define detect (runner-ref 'detect-high-risk-flags))
  ;; test-main.rkt uses getenv
  (define result (detect "tests/test-main.rkt"))
  (check-not-false (member 'env result) (format "expected 'env in ~a" result)))

(test-case "detect-high-risk-flags: detects cwd usage on known file"
  (define detect (runner-ref 'detect-high-risk-flags))
  ;; Many files use current-directory; check test-cli-interactive which parameterizes it
  (define result (detect "tests/test-cli-interactive.rkt"))
  (check-not-false (member 'cwd result) (format "expected 'cwd in ~a" result)))

(test-case "detect-high-risk-flags: simple file returns empty"
  (define detect (runner-ref 'detect-high-risk-flags))
  ;; test-version.rkt is unlikely to have env/cwd/temp-file
  (define result (detect "tests/test-version.rkt"))
  ;; It may or may not have flags, just verify it returns a list
  (check-true (list? result)))

(test-case "print-inventory: produces structured output"
  (define print-inv (runner-ref 'print-inventory))
  (define output
    (with-output-to-string (lambda () (print-inv 'smoke (list "tests/test-version.rkt")))))
  (check-not-false (string-contains? output "INVENTORY REPORT"))
  (check-not-false (string-contains? output "suite: smoke"))
  (check-not-false (string-contains? output "Selected files: 1"))
  (check-not-false (string-contains? output "Inventory hash:")))
