#lang racket
;; @speed fast
;; @suite testing
;; @boundary integration
;; @timeout 300

;; Contract for the Phase D CI surface of `./qt full`:
;;   - `--json-out PATH` writes the SAME schema scripts/run-tests/reporting.rkt
;;     writes today (run_summary + files[]) and the document validates with
;;     reporting.rkt's `validate-run-summary-json`; qt adds only the
;;     schema-additive `extra.qt` selection marker;
;;   - `--suite NAME` resolves through classify.rkt's existing suite predicates
;;     (no second registry);
;;   - `--shard-index/--shard-total` uses classify.rkt's `shard-files`
;;     semantics (a partition never overlaps and covers the selection exactly);
;;   - `--timeout`, `--profile`, `--ledger`, `--record-gate-evidence`,
;;     `--repeat`, `--sequential`, `--mode`, `--scheduler`, `--ordering`,
;;     `--shard-plan`, `--durations` are forwarded to the existing runner code
;;     path instead of being reimplemented;
;;   - an unsupported value/flag/combination fails closed with a `qt: ...`
;;     message that NAMES it (never silently ignored);
;;   - `./qt full --file FILE...` is the ONLY explicit-reproduction spelling:
;;     labelled in the pass line and marked in the JSON, and positional files
;;     stay rejected;
;;   - `STRICT_TEST_RUNNER=1` plus zero executed tests is a failure;
;;   - plain `./qt full --explain` stays byte-identical.
;; Fixture roots are temporary directories OUTSIDE the repository, so the
;; launch-tree root resolution picks the fixture itself (cheap execution).

(require rackunit
         racket/file
         racket/list
         racket/path
         racket/string
         racket/runtime-path
         json
         "../scripts/run-tests/classify.rkt"
         "../scripts/run-tests/reporting.rkt"
         "../scripts/testing/full-plan.rkt")

(define-runtime-path qt "../qt")
(define-runtime-path root "..")

;; Read both pipes while the child runs; neither pipe may block the other.
(define (invoke-command timeout cwd env executable . args)
  (define cust (make-custodian))
  (define proc #f)
  (dynamic-wind void
                (lambda ()
                  (parameterize ([current-custodian cust]
                                 [current-directory cwd]
                                 [current-environment-variables env]
                                 [subprocess-group-enabled #t])
                    (define-values (child out in err) (apply subprocess #f #f #f executable args))
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
  (apply invoke-command
         300
         (current-directory)
         (current-environment-variables)
         (find-executable-path "racket")
         qt
         args))

(define (invoke-in cwd . args)
  (apply invoke-command
         300
         cwd
         (current-environment-variables)
         (find-executable-path "racket")
         qt
         args))

(define (invoke-in-with-env cwd name value . args)
  (define env (environment-variables-copy (current-environment-variables)))
  (environment-variables-set! env (string->bytes/utf-8 name) (string->bytes/utf-8 value))
  (apply invoke-command 300 cwd env (find-executable-path "racket") qt args))

;; Make the fixture a git repository with one commit, so the gate-evidence
;; writer can resolve a strict 40-char HEAD SHA (it never accepts "unknown").
(define (git-commit! dir)
  (define git (find-executable-path "git"))
  (parameterize ([current-directory dir]
                 [current-output-port (open-output-nowhere)]
                 [current-error-port (open-output-nowhere)])
    (system* git "init" "-q")
    (system* git "-c" "user.email=qt@example.invalid" "-c" "user.name=qt" "add" "-A")
    (system* git "-c" "user.email=qt@example.invalid" "-c" "user.name=qt" "commit" "-qm" "fixture")))

(define (write-file path text)
  (call-with-output-file path #:exists 'truncate (lambda (out) (display text out))))

;; Build a fixture repository root containing only <root>/tests/<file>.
(define (with-fixture-root files thunk)
  (define dir (make-temporary-file "qt-ci-~a" 'directory))
  (dynamic-wind void
                (lambda ()
                  (make-directory (build-path dir "tests"))
                  (for ([file (in-list files)])
                    (write-file (build-path dir "tests" (car file)) (cdr file)))
                  (thunk dir))
                (lambda () (delete-directory/files dir))))

(define (pass-line out)
  (for/first ([line (in-list (string-split out "\n"))]
              #:when (string-prefix? line "full passed:"))
    line))

(define (read-json-file path)
  (call-with-input-file path read-json))

(define (json-extra-qt doc)
  (define extra (hash-ref doc 'extra #f))
  (and (hash? extra) (hash-ref extra 'qt #f)))

(define passing-fixture
  (cons
   "pass.rkt"
   "#lang racket/base\n;; @speed fast\n;; @suite unit\n(require rackunit)\n(module+ test\n  (check-true #t)\n  (check-equal? 1 1))\n"))

(define second-fixture
  (cons
   "second.rkt"
   "#lang racket/base\n;; @speed fast\n;; @suite unit\n(require rackunit)\n(module+ test (check-true #t))\n"))

(define slow-fixture
  (cons
   "slow.rkt"
   "#lang racket/base\n;; @speed slow\n;; @suite slow\n(require rackunit)\n(module+ test (check-true #t))\n"))

(define tui-fixture
  (cons
   "tui.rkt"
   "#lang racket/base\n;; @speed fast\n;; @suite tui\n(require rackunit)\n(module+ test (check-true #t))\n"))

;; Skipped by the CI profile (@requires network): zero executed tests, exit 0.
(define skipped-fixture
  (cons
   "skipped.rkt"
   "#lang racket/base\n;; @speed fast\n;; @suite unit\n;; @requires network\n(require rackunit)\n(module+ test (check-true #t))\n"))

;; Loading this fixture creates tests/marker; it must never happen under
;; --explain or a plan-only mode.
(define marker-fixture
  (cons "marker.rkt"
        "#lang racket/base\n(call-with-output-file \"marker\" void)\n(raise \"executed\")\n"))

(module+ test
  (test-case "--json-out writes the runner schema plus the qt marker and validates"
    (if (full-execution-supported?)
        (with-fixture-root
         (list passing-fixture)
         (lambda (dir)
           (define out-path (build-path dir "results.json"))
           (define-values (status out err)
             (invoke-in dir "full" "--jobs" "1" "--json-out" (path->string out-path)))
           (check-equal? status 0 (string-append out err))
           (check-regexp-match qt-ci-pass-pattern (or (pass-line out) ""))
           (define doc (read-json-file out-path))
           (check-equal? (validate-run-summary-json doc) '())
           (check-true (hash? (hash-ref doc 'run_summary #f)))
           (check-equal? (length (hash-ref doc 'files '())) 1)
           (define marker (json-extra-qt doc))
           (check-true (hash? marker))
           (check-equal? (hash-ref marker 'selection_mode) "full-suite")
           (check-equal? (hash-ref marker 'reproduction) #f)
           (check-equal? (hash-ref marker 'full_coverage) #t)))
        (displayln "SKIP qt --json-out: Linux /proc or setsid unavailable")))
  (test-case "--file is labelled explicit reproduction in the pass line and the JSON"
    (if (full-execution-supported?)
        (with-fixture-root
         (list passing-fixture second-fixture)
         (lambda (dir)
           (define out-path (build-path dir "repro.json"))
           (define-values (status out err)
             (invoke-in dir
                        "full"
                        "--jobs"
                        "1"
                        "--file"
                        "tests/pass.rkt"
                        "--json-out"
                        (path->string out-path)))
           (check-equal? status 0 (string-append out err))
           (check-equal? (pass-line out)
                         (string-append
                          "full passed: 1 files, 2 tests (explicit reproduction only, not a PR pass; "
                          "explicit --file selection (1 files))"))
           (define doc (read-json-file out-path))
           (check-equal? (validate-run-summary-json doc) '())
           (check-equal? (length (hash-ref doc 'files '())) 1)
           (define marker (json-extra-qt doc))
           (check-equal? (hash-ref marker 'selection_mode) "explicit-reproduction")
           (check-true (hash-ref marker 'reproduction))
           (check-false (hash-ref marker 'full_coverage))
           (check-equal? (hash-ref marker 'files) '("tests/pass.rkt"))))
        (displayln "SKIP qt --file: Linux /proc or setsid unavailable")))
  (test-case "positional files are rejected and the error names the --file spelling"
    (for ([args (in-list (list (list "full" "tests/pass.rkt") (list "run" "tests/pass.rkt")))])
      (define-values (status out err) (apply invoke args))
      (check-not-equal? status 0)
      (check-regexp-match #rx"^qt: " err)
      (check-regexp-match #rx"--file FILE" err)))
  (test-case "unsupported flags and values fail closed with a naming qt: message"
    (for ([case
           (in-list
            (list
             (list (list "full" "--bogus") #rx"^qt: unsupported option: --bogus$")
             (list (list "full" "--suite" "nope")
                   #rx"^qt: (?:full-plan: )?unsupported --suite: nope \\(valid: ")
             (list (list "full" "--mode" "nope")
                   #rx"^qt: (?:full-plan: )?unsupported --mode: nope \\(valid: ")
             (list (list "full" "--profile" "nope")
                   #rx"^qt: (?:full-plan: )?unsupported --profile: nope ")
             (list (list "full" "--shard-plan" "nope")
                   #rx"^qt: (?:full-plan: )?unsupported --shard-plan: nope ")
             (list (list "full" "--shard-plan" "report")
                   #rx"^qt: (?:full-plan: )?unsupported --shard-plan: report ")
             (list (list "full" "--shard-plan" "measure")
                   #rx"^qt: (?:full-plan: )?unsupported --shard-plan: measure ")
             (list (list "full" "--shard-index" "2" "--shard-total" "2")
                   #rx"^qt: (?:full-plan: )?--shard-index must be an integer in \\[0, 2\\)")
             (list (list "full" "--repeat" "0")
                   #rx"^qt: (?:full-plan: )?--repeat must be a positive integer")
             (list (list "default" "--json-out" "x.json")
                   #rx"^qt: default does not accept CI-surface option --json-out")
             (list (list "quick" "--suite" "fast")
                   #rx"^qt: quick does not accept CI-surface option --suite")
             (list (list "full" "--suite" "fast" "--file" "tests/pass.rkt")
                   #rx"^qt: (?:full-plan: )?cannot combine --suite with --file")
             (list (list "full" "--file" "tests/pass.rkt" "--shard-total" "2")
                   #rx"^qt: (?:full-plan: )?--file \\(explicit reproduction\\) cannot be sharded")
             (list (list "full" "--changed" "--suite" "fast")
                   #rx"^qt: --changed cannot be combined with CI-surface options")
             (list (list "full" "--explain" "--json-out" "x.json")
                   #rx"^qt: --explain cannot be combined with --json-out")
             (list (list "full" "--file" "tests/missing.rkt")
                   #rx"^qt: (?:full-plan: )?--file is not an existing test file")))])
      (define-values (status out err) (apply invoke (car case)))
      (check-not-equal? status 0 (format "~a should fail" (car case)))
      (check-regexp-match (cadr case) (string-trim err))))
  (test-case "zero executed tests fail under STRICT_TEST_RUNNER=1"
    (if (full-execution-supported?)
        (with-fixture-root
         (list skipped-fixture)
         (lambda (dir)
           (define-values (status out err)
             (invoke-in-with-env dir "STRICT_TEST_RUNNER" "1" "full" "--jobs" "1" "--profile" "ci"))
           (check-not-equal? status 0)
           (check-regexp-match #rx"^qt: (?:full-plan: )?zero tests executed" err)
           (check-regexp-match #rx"STRICT_TEST_RUNNER=1" err)
           (check-false (regexp-match? #rx"full passed:" out))))
        (displayln "SKIP qt strict zero tests: Linux /proc or setsid unavailable")))
  (test-case "shard partitions use shard-files semantics: no overlap, exact cover"
    (define files '("tests/a.rkt" "tests/b.rkt" "tests/c.rkt" "tests/d.rkt" "tests/e.rkt"))
    (for ([total (in-range 1 5)])
      (define parts
        (for/list ([index (in-range total)])
          (qt-shard-partition files index total)))
      (define flat (apply append parts))
      (check-equal? (sort flat string<?) files "the shards must cover the selection exactly")
      (check-equal? (length flat) (length (remove-duplicates flat)) "shards must not overlap")
      (for ([part (in-list parts)])
        (check-equal? (length part) (length (remove-duplicates part))))))
  (test-case "--suite resolves through classify.rkt's own suite predicates"
    (with-fixture-root
     (list passing-fixture slow-fixture tui-fixture)
     (lambda (dir)
       (define universe (collect-test-files 'all #:root dir))
       (define (absolute rel)
         (path->string (simplify-path (build-path dir rel) #f)))
       (for ([suite (in-list '(fast tui slow all))])
         (define expected
           (sort (filter (lambda (rel)
                           (case suite
                             [(fast)
                              (and (not (slow-file? (absolute rel)))
                                   (not (tui-file? (absolute rel))))]
                             [(tui) (tui-file? (absolute rel))]
                             [(slow) (slow-file? (absolute rel))]
                             [else #t]))
                         universe)
                 string<?))
         (check-equal? (qt-suite-relative suite dir) expected (format "suite ~a selection" suite)))
       (check-equal? (qt-suite-relative 'fast dir) '("tests/pass.rkt")))))
  (test-case "--suite and --shard are forwarded to the runner and echoed in --explain"
    (with-fixture-root
     (list passing-fixture slow-fixture tui-fixture)
     (lambda (dir)
       (define-values (status out err) (invoke-in dir "full" "--suite" "fast" "--explain"))
       (check-equal? status 0 (string-append out err))
       (check-regexp-match #rx"suite: fast \\(classify\\.rkt's existing suite predicate" out)
       (check-regexp-match #rx"tests/pass\\.rkt" out)
       (check-false (regexp-match? #rx"tests/slow\\.rkt" out))
       (check-false (regexp-match? #rx"tests/tui\\.rkt" out))))
    (if (full-execution-supported?)
        (with-fixture-root
         (list passing-fixture second-fixture slow-fixture)
         (lambda (dir)
           (define out0 (build-path dir "shard0.json"))
           (define out1 (build-path dir "shard1.json"))
           (define-values (s0 out0-text e0)
             (invoke-in dir
                        "full"
                        "--suite"
                        "all"
                        "--jobs"
                        "1"
                        "--shard-index"
                        "0"
                        "--shard-total"
                        "2"
                        "--json-out"
                        (path->string out0)))
           (define-values (s1 out1-text e1)
             (invoke-in dir
                        "full"
                        "--suite"
                        "all"
                        "--jobs"
                        "1"
                        "--shard-index"
                        "1"
                        "--shard-total"
                        "2"
                        "--json-out"
                        (path->string out1)))
           (check-equal? s0 0 e0)
           (check-equal? s1 0 e1)
           (define files0
             (map (lambda (f) (hash-ref f 'path)) (hash-ref (read-json-file out0) 'files)))
           (define files1
             (map (lambda (f) (hash-ref f 'path)) (hash-ref (read-json-file out1) 'files)))
           (check-equal? (sort (append files0 files1) string<?) (qt-suite-relative 'all dir))
           (check-equal? (length (append files0 files1))
                         (length (remove-duplicates (append files0 files1))))
           (check-equal? (hash-ref (json-extra-qt (read-json-file out0)) 'shard_index) 0)
           (check-equal? (hash-ref (json-extra-qt (read-json-file out0)) 'shard_total) 2)))
        (displayln "SKIP qt sharded execution: Linux /proc or setsid unavailable")))
  (test-case "execution flags are accepted and forwarded in one run"
    (if (full-execution-supported?)
        (with-fixture-root
         (list passing-fixture)
         (lambda (dir)
           (define ledger (build-path dir "ledger.json"))
           (write-file ledger "{\"entries\": []}\n")
           (define out-path (build-path dir "flags.json"))
           (define-values (status out err)
             (invoke-in dir
                        "full"
                        "--jobs"
                        "1"
                        "--timeout"
                        "60"
                        "--profile"
                        "local"
                        "--mode"
                        "subprocess"
                        "--repeat"
                        "2"
                        "--sequential"
                        "--ledger"
                        (path->string ledger)
                        "--json-out"
                        (path->string out-path)))
           (check-equal? status 0 (string-append out err))
           (check-regexp-match #rx";; qt: shard-plan|full passed:" out)
           (check-equal?
            (pass-line out)
            "full passed: 1 files, 2 tests (full Racket suite only, not PR/CI verification; complete available Racket suite)")
           (check-regexp-match #rx"Run 2/2" out)
           (define doc (read-json-file out-path))
           (check-equal? (validate-run-summary-json doc) '())
           (check-equal? (hash-ref (hash-ref doc 'run_summary) 'suite) "full")
           (check-equal? (hash-ref (hash-ref doc 'run_summary) 'execution_mode) "subprocess")))
        (displayln "SKIP qt execution flags: Linux /proc or setsid unavailable")))
  (test-case "--shard-plan active partitions the selection without overlap"
    (with-fixture-root (list passing-fixture second-fixture slow-fixture)
                       (lambda (dir)
                         (define (spec-for index)
                           (qt-ci-spec #f '() index 2 #f 'local #f #f #f 1 'auto "active" #f '()))
                         (define plan0 (resolve-qt-ci-plan dir (spec-for 0)))
                         (define plan1 (resolve-qt-ci-plan dir (spec-for 1)))
                         (define files0 (qt-ci-run-files plan0))
                         (define files1 (qt-ci-run-files plan1))
                         (check-equal? (sort (append files0 files1) string<?) (qt-ci-plan-rels plan0))
                         (check-equal? (length (append files0 files1))
                                       (length (remove-duplicates (append files0 files1)))))))
  (test-case "the deleted plan modes are refused by qt before anything runs"
    (with-fixture-root (list marker-fixture)
                       (lambda (dir)
                         (define marker (build-path dir "tests" "marker"))
                         (for ([mode (in-list '("report" "measure"))])
                           (define-values (status out err)
                             (invoke-in dir "full" "--shard-plan" mode "--shard-total" "2"))
                           (check-not-equal? status 0)
                           (check-regexp-match #rx"unsupported --shard-plan" err)
                           (check-false (regexp-match? #rx"full passed:" out))
                           (check-false (file-exists? marker))))))
  (test-case "plain full --explain stays byte-identical (no CI-surface lines)"
    (with-fixture-root
     (list marker-fixture)
     (lambda (dir)
       (define-values (status out err) (invoke-in dir "full" "--explain"))
       (check-equal? status 0 (string-append out err))
       (check-equal? out (string-append (string-join (full-explain-lines dir) "\n") "\n"))
       (check-false (regexp-match? #rx"reproduction spelling" out))
       (check-false (regexp-match? #rx"coverage:" out))
       (check-false (file-exists? (build-path dir "tests" "marker"))))))
  (test-case "CI --explain documents the explicit reproduction spelling without executing"
    (with-fixture-root
     (list marker-fixture)
     (lambda (dir)
       (define-values (status out err) (invoke-in dir "full" "--file" "tests/marker.rkt" "--explain"))
       (check-equal? status 0 (string-append out err))
       (check-regexp-match #rx"reproduction spelling: \\./qt full --file FILE" out)
       (check-regexp-match #rx"label: explicit reproduction only, not a PR pass" out)
       (check-regexp-match #rx"coverage: explicit --file selection" out)
       (check-false (file-exists? (build-path dir "tests" "marker"))))))
  (test-case "--record-gate-evidence records through the runner's gate-evidence module"
    (if (full-execution-supported?)
        (with-fixture-root (list passing-fixture)
                           (lambda (dir)
                             ;; gate-evidence reads the version from <root>/util/version.rkt and a
                             ;; strict 40-char SHA from git, so the fixture needs both.
                             (make-directory (build-path dir "util"))
                             (write-file (build-path dir "util" "version.rkt")
                                         "(define q-version \"test\")\n")
                             (git-commit! dir)
                             (define-values (status out err)
                               (invoke-in dir "full" "--jobs" "1" "--record-gate-evidence"))
                             (check-equal? status 0 (string-append out err))
                             (define evidence (build-path dir ".gate-evidence" "full.json"))
                             (check-true (file-exists? evidence))
                             (define ev (read-json-file evidence))
                             (check-equal? (hash-ref ev 'suite) "full")
                             (check-equal? (hash-ref ev 'shard_total) 1)
                             (check-true (positive? (hash-ref ev 'parsed_test_count)))))
        (displayln "SKIP qt --record-gate-evidence: Linux /proc or setsid unavailable"))))
