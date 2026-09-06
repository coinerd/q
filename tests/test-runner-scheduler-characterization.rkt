#lang racket/base

;; @speed fast
;; @timeout 180
;; @boundary unit
;; @isolation process
;; W7.1 grouped-mode reclassification: this file drives the runner's own
;; run-all-files/run-single-file/run-suite-once in-process and pins
;; subprocess timeout-kill semantics, so true grouped isolation cannot be
;; proved; grouped requests fall back to subprocess with the named reason
;; `declared-process-isolation`.

;; BOUNDARY: unit
;; tests/test-runner-scheduler-characterization.rkt
;; Current scheduler release — characterize the scheduler contract. W2 flipped the W0 pins:
;; `--scheduler batch|queue` now exists (batch default; queue work-conserving).
;;
;; Pins:
;;  1. work-conserving queue: with jobs=2 the third file starts when either
;;     initial worker frees, before an unrelated long file completes; batch
;;     mode still reproduces the fixed-batch barrier (rollback path).
;;  2. result order == input file order (never completion order).
;;  3. per-file timeout: file-timeout kills the subprocess -> exit 2 / TIMEOUT;
;;     the meta @timeout directive (seconds) is honored.
;;  4. status characters: "." exit 0, "T" exit 2 (timeout), "S" SKIPPED_BY_PROFILE,
;;     "F" any other failure (runner.rkt:353-358).
;;  5. exception/result classification matrix (parse.rkt classify-test-result).
;;  6. serial/parallel ownership seam (runner.rkt:724-754): mutation-sensitive
;;     files run alone and FIRST, then the parallel partition, under both batch
;;     and queue schedulers; results still sort by input order.
;;  7. --scheduler CLI seam: batch and queue are accepted; invalid values exit 2
;;     with a named diagnostic; --help advertises the option.
;;  8. W3 (#9591) within-shard LPT hold: ordering default stays fifo (parse-args
;;     never reads FAST_SHARD_ORDERING), ci.yml carries no ordering lever,
;;     every unusable-evidence fallback is named, and consecutive ordering /
;;     shard-plan generations are byte-identical on identical inputs.

(require rackunit
         rackunit/text-ui
         racket/string
         racket/match
         racket/file
         racket/path
         racket/runtime-path
         racket/system
         (prefix-in p: racket/port)
         (only-in "../scripts/run-tests/scheduler-order.rkt"
                  default-ordering
                  default-max-age-seconds
                  known-orderings
                  prepare-ordering
                  order-files
                  ordering-record-mode
                  ordering-record-requested
                  ordering-record-fallback-reason
                  ordering-record-snapshot-checksum
                  ordering-record-snapshot-status
                  ordering-record->jsexpr)
         (only-in "../scripts/run-tests/shard-plan.rkt" build-shard-plan plan->jsexpr)
         "../scripts/run-tests.rkt")

(define-runtime-path here ".")
(define project-root (simplify-path (build-path here "..")))

(define (find-runner)
  (build-path project-root "scripts" "run-tests.rkt"))

;; ---------------------------------------------------------------------------
;; Fixture helpers
;; ---------------------------------------------------------------------------

(define (write-fixture! dir name content)
  (define p (build-path dir name))
  (call-with-output-file p #:exists 'replace (lambda (out) (display content out)))
  (path->string p))

(define (delete-dir/safe d)
  (when (directory-exists? d)
    (for ([f (in-list (directory-list d))])
      (delete-file (build-path d f)))
    (delete-directory d)))

(define (unique-tmp-path base)
  (path->string (build-path (find-system-path 'temp-dir)
                            (format "~a-~a-~a.txt" base (current-milliseconds) (random 1000000)))))

(define (capture-stdout thunk)
  (define out (open-output-string))
  (parameterize ([current-output-port out])
    (thunk)
    (get-output-string out)))

(define (run/capture cmd)
  (define out (open-output-string))
  (define exit-code
    (parameterize ([current-output-port out]
                   [current-error-port out])
      (system/exit-code cmd)))
  (cons (get-output-string out) exit-code))

;; ---------------------------------------------------------------------------
;; Fixture sources
;; ---------------------------------------------------------------------------

(define fixture-slow
  (string-append "#lang racket/base\n"
                 "(define slow-done (getenv \"W0_SCHED_SLOW_DONE\"))\n"
                 "(unless slow-done (error \"W0_SCHED_SLOW_DONE unset\"))\n"
                 ";; Deliberately exceed loaded subprocess startup variance.\n"
                 "(sleep 10)\n"
                 "(call-with-output-file slow-done\n"
                 "  #:exists 'append\n"
                 "  (lambda (o) (display \"done\" o)))\n"))

(define fixture-quick
  (string-append "#lang racket/base\n"
                 "(define quick-done (getenv \"W0_SCHED_QUICK_DONE\"))\n"
                 "(unless quick-done (error \"W0_SCHED_QUICK_DONE unset\"))\n"
                 "(call-with-output-file quick-done\n"
                 "  #:exists 'append\n"
                 "  (lambda (o) (display \"done\" o)))\n"))

(define fixture-probe
  (string-append "#lang racket/base\n"
                 "(define quick-done (getenv \"W0_SCHED_QUICK_DONE\"))\n"
                 "(define slow-done (getenv \"W0_SCHED_SLOW_DONE\"))\n"
                 "(define probe-out (getenv \"W0_SCHED_PROBE_OUT\"))\n"
                 "(unless (and quick-done slow-done probe-out)\n"
                 "  (error \"W0 probe env unset\"))\n"
                 "(define probe-content\n"
                 "  (if (and (file-exists? quick-done) (file-exists? slow-done))\n"
                 "      \"after-full-batch\"\n"
                 "      \"early\"))\n"
                 "(call-with-output-file probe-out\n"
                 "  #:exists 'replace\n"
                 "  (lambda (o) (display probe-content o)))\n"))

(define fixture-serial
  (string-append "#lang racket/base\n"
                 ";; @isolation process\n"
                 "(define serial-marker (getenv \"W0_SCHED_SERIAL_MARKER\"))\n"
                 "(unless serial-marker (error \"W0_SCHED_SERIAL_MARKER unset\"))\n"
                 "(call-with-output-file serial-marker\n"
                 "  #:exists 'append\n"
                 "  (lambda (o) (display \"serial-done\" o)))\n"))

(define fixture-par
  (string-append "#lang racket/base\n"
                 "(define serial-marker (getenv \"W0_SCHED_SERIAL_MARKER\"))\n"
                 "(define par-out (getenv \"W0_SCHED_PAR_OUT\"))\n"
                 "(unless (and serial-marker par-out)\n"
                 "  (error \"W0 par env unset\"))\n"
                 "(define par-content\n"
                 "  (if (file-exists? serial-marker) \"after-serial\" \"before-serial\"))\n"
                 "(call-with-output-file par-out\n"
                 "  #:exists 'append\n"
                 "  (lambda (o) (display par-content o)))\n"))

;; ---------------------------------------------------------------------------
;; Tests
;; ---------------------------------------------------------------------------

(define suite
  (test-suite "test-runner-scheduler-characterization"

    (test-case "W2 work-conserving queue vs fixed-batch rollback: jobs=2, third file starts when a worker frees"
      (define dir (make-temporary-file "w2-barrier-~a" 'directory))
      (define slow (write-fixture! dir "batch-slow.rkt" fixture-slow))
      (define quick (write-fixture! dir "batch-quick.rkt" fixture-quick))
      (define probe (write-fixture! dir "batch-probe.rkt" fixture-probe))
      (define (run-once scheduler)
        (define quick-done (unique-tmp-path "w2-sched-quick-done"))
        (define slow-done (unique-tmp-path "w2-sched-slow-done"))
        (define probe-out (unique-tmp-path "w2-sched-probe-out"))
        (putenv "W0_SCHED_QUICK_DONE" quick-done)
        (putenv "W0_SCHED_SLOW_DONE" slow-done)
        (putenv "W0_SCHED_PROBE_OUT" probe-out)
        (define results (run-all-files (list slow quick probe) 2 #f #:scheduler scheduler))
        (putenv "W0_SCHED_QUICK_DONE" "")
        (putenv "W0_SCHED_SLOW_DONE" "")
        (putenv "W0_SCHED_PROBE_OUT" "")
        (values results quick-done slow-done probe-out))
      (define-values (queue-results q-quick q-slow q-probe) (run-once 'queue))
      (check-equal? (map classify-test-result queue-results)
                    (list 'ZERO_PARSED 'ZERO_PARSED 'ZERO_PARSED)
                    "queue mode: all three fixtures exit 0 with zero parsed tests")
      (check-true (file-exists? q-quick) "queue mode: first-batch quick file completed")
      (check-true (file-exists? q-slow) "queue mode: first-batch slow file completed")
      (check-equal?
       (file->string q-probe)
       "early"
       "queue mode: probe starts when an initial worker frees, before the long file completes (work-conserving)")
      (define-values (batch-results b-quick b-slow b-probe) (run-once 'batch))
      (check-equal? (map classify-test-result batch-results)
                    (list 'ZERO_PARSED 'ZERO_PARSED 'ZERO_PARSED)
                    "batch mode: all three fixtures exit 0 with zero parsed tests")
      (check-true (file-exists? b-quick) "batch mode: first-batch quick file completed")
      (check-true (file-exists? b-slow) "batch mode: first-batch slow file completed")
      (check-equal?
       (file->string b-probe)
       "after-full-batch"
       "batch mode: probe waits for BOTH first-batch files (rollback path reproduces old scheduling)")
      (delete-dir/safe dir))

    (test-case "result order equals input file order, never completion order"
      (define dir (make-temporary-file "w0-order-~a" 'directory))
      (define aaa (write-fixture! dir "aaa-slow.rkt" "#lang racket/base\n(sleep 0.5)\n"))
      (define bbb (write-fixture! dir "bbb-fast.rkt" "#lang racket/base\n(define x 1)\n"))
      (define ccc (write-fixture! dir "ccc-med.rkt" "#lang racket/base\n(sleep 0.3)\n"))
      (define results (run-all-files (list aaa bbb ccc) 3 #f))
      (check-equal?
       (map test-file-result-path results)
       (list aaa bbb ccc)
       "single batch: completion order differs from input order, results sorted by input order")
      (check-equal? (map classify-test-result results) (list 'ZERO_PARSED 'ZERO_PARSED 'ZERO_PARSED))
      (delete-dir/safe dir))

    (test-case "per-file timeout: meta @timeout (seconds) kills subprocess -> exit 2 / TIMEOUT"
      (define dir (make-temporary-file "w0-timeout-~a" 'directory))
      (define f
        (write-fixture! dir "timeout-file.rkt" "#lang racket/base\n;; @timeout 1\n(sleep 30)\n"))
      (check-equal? (hash-ref (get-file-metadata f) 'timeout #f)
                    1
                    "meta @timeout 1 parses to 1 second")
      (define r (run-single-file f #:timeout #f))
      (check-equal? (classify-test-result r) 'TIMEOUT)
      (check-equal? (test-file-result-exit-code r) 2)
      (check-true (< (test-file-result-elapsed-ms r) 15000)
                  "1s meta timeout enforced, not the 120s default")
      (delete-dir/safe dir))

    (test-case "status characters: . / T / S / F per exit class (runner.rkt:353-358)"
      (define dir (make-temporary-file "w0-status-~a" 'directory))
      (define ok-file (write-fixture! dir "ok.rkt" "#lang racket/base\n(define x 1)\n"))
      (define timeout-file
        (write-fixture! dir "timeout.rkt" "#lang racket/base\n;; @timeout 1\n(sleep 30)\n"))
      (define skip-file
        (write-fixture! dir "skip.rkt" "#lang racket/base\n(module+ main (exit 5))\n"))
      (define fail-file (write-fixture! dir "fail.rkt" "#lang racket/base\n(error \"boom\")\n"))
      (check-true (string-contains? (capture-stdout (lambda () (run-all-files (list ok-file) 1 #f)))
                                    ".")
                  "exit 0 -> '.'")
      (check-true
       (string-contains? (capture-stdout (lambda () (run-all-files (list timeout-file) 1 #f))) "T")
       "exit 2 -> 'T'")
      (check-true (string-contains? (capture-stdout (lambda () (run-all-files (list skip-file) 1 #f)))
                                    "S")
                  "SKIPPED_BY_PROFILE -> 'S'")
      (check-true (string-contains? (capture-stdout (lambda () (run-all-files (list fail-file) 1 #f)))
                                    "F")
                  "any other failure -> 'F'")
      (delete-dir/safe dir))

    (test-case "classification matrix (parse.rkt classify-test-result)"
      (define dir (make-temporary-file "w0-classify-~a" 'directory))
      (define (mk name content)
        (write-fixture! dir name content))
      (define cases
        (list
         (list "rackunit-ok.rkt"
               "#lang racket/base\n(require rackunit)\n(module+ test (check-equal? 1 1))\n"
               'PASS
               0
               1
               0
               1)
         (list "rackunit-bad.rkt"
               "#lang racket/base\n(require rackunit)\n(module+ test (check-equal? 1 2))\n"
               'ASSERTION_FAILURE
               1
               0
               1
               1)
         (list "plain-ok.rkt" "#lang racket/base\n(define x 1)\n" 'ZERO_PARSED 0 0 0 0)
         (list "exit5.rkt" "#lang racket/base\n(module+ main (exit 5))\n" 'SKIPPED_BY_PROFILE 5 0 0 0)
         (list "break.rkt" "#lang racket/base\n(error \"user break\")\n" 'USER_BREAK 1 0 0 0)
         (list "env.rkt"
               "#lang racket/base\n(error \"missing environment variable W0_X\")\n"
               'ENVIRONMENT_MISSING
               1
               0
               0
               0)
         (list "syntax.rkt" "#lang racket/base\n(define ( \n" 'COMPILE_FAILURE 1 0 0 0)
         (list "modload.rkt"
               "#lang racket/base\n(require definitely/missing/w0-char/fixture)\n"
               'MODULE_LOAD_FAILURE
               1
               0
               0
               0)
         (list "boom.rkt" "#lang racket/base\n(error \"boom\")\n" 'UNKNOWN_FAILURE 1 0 0 0)))
      (for ([c (in-list cases)])
        (match-define (list name content expected exit-code passed failed total) c)
        ;; Parallel fast-suite load can delay process startup substantially;
        ;; keep the characterization bounded without a 10s scheduling flake.
        (define r (run-single-file (mk name content) #:timeout 30000))
        (check-equal? (classify-test-result r) expected (format "~a classifies as ~a" name expected))
        (check-equal? (test-file-result-exit-code r) exit-code (format "~a exit code" name))
        (check-equal? (test-file-result-passed r) passed (format "~a passed" name))
        (check-equal? (test-file-result-failed r) failed (format "~a failed" name))
        (check-equal? (test-file-result-total r) total (format "~a total" name)))
      (delete-dir/safe dir))

    (test-case "serial/parallel ownership seam holds under batch AND queue schedulers (runner.rkt:731-761)"
      (define dir (make-temporary-file "w2-seam-~a" 'directory))
      (define serial-a (write-fixture! dir "serial-a.rkt" fixture-serial))
      (define par-b (write-fixture! dir "par-b.rkt" fixture-par))
      (define par-c (write-fixture! dir "par-c.rkt" fixture-par))
      (for ([scheduler (in-list '(batch queue))])
        (define serial-marker (unique-tmp-path "w2-sched-serial-marker"))
        (define par-out (unique-tmp-path "w2-sched-par-out"))
        (putenv "W0_SCHED_SERIAL_MARKER" serial-marker)
        (putenv "W0_SCHED_PAR_OUT" par-out)
        (define-values (exit-code run-results)
          (run-suite-once (list serial-a par-b par-c)
                          2 ; jobs
                          #f ; timeout-ms (default 120s per file)
                          #f ; strict?
                          "w2-scheduler-characterization"
                          1
                          1 ; repeat-num / repeat-total
                          'subprocess
                          #f ; json-out
                          #f ; ledger
                          'local ; profile
                          #:shard #f
                          #:phases (hasheq)
                          #:scheduler scheduler))
        (putenv "W0_SCHED_SERIAL_MARKER" "")
        (putenv "W0_SCHED_PAR_OUT" "")
        (check-true (file-exists? serial-marker) (format "~a: serial-phase file completed" scheduler))
        (check-true
         (regexp-match? #rx"after-serial" (file->string par-out))
         (format
          "~a: parallel files observed the serial phase's completion -> serial runs before parallel"
          scheduler))
        (check-false (regexp-match? #rx"before-serial" (file->string par-out))
                     (format "~a: no parallel file started before the serial phase finished"
                             scheduler))
        (check-equal? (map test-file-result-path run-results)
                      (list serial-a par-b par-c)
                      (format "~a: run-suite-once results sorted by input order" scheduler)))
      (delete-dir/safe dir))

    (test-case "--scheduler CLI seam: batch and queue accepted, invalid exits 2 with a named diagnostic"
      (define dir (make-temporary-file "w2-cli-~a" 'directory))
      (define solo
        (write-fixture! dir
                        "solo.rkt"
                        "#lang racket/base\n(require rackunit)\n(module+ test (check-equal? 1 1))\n"))
      (define batch-res (run/capture (format "racket ~a ~a --scheduler batch" (find-runner) solo)))
      (check-equal? (cdr batch-res) 0 "--scheduler batch is accepted (exit 0)")
      (define queue-res (run/capture (format "racket ~a ~a --scheduler queue" (find-runner) solo)))
      (check-equal? (cdr queue-res) 0 "--scheduler queue is accepted (exit 0)")
      (define invalid-res (run/capture (format "racket ~a ~a --scheduler bogus" (find-runner) solo)))
      (check-equal? (cdr invalid-res) 2 "invalid --scheduler value exits 2")
      (check-true (regexp-match? #rx"--scheduler" (car invalid-res))
                  (format "invalid --scheduler diagnostic names the option; got: ~a"
                          (car invalid-res)))
      (define help-res (run/capture (format "racket ~a --help" (find-runner))))
      (check-equal? (cdr help-res) 0 "--help exits 0")
      (check-true (regexp-match? #rx"--scheduler" (car help-res))
                  "--help advertises the --scheduler option")
      (delete-dir/safe dir))

    ;; ── W3 (#9591) within-shard LPT ordering hold contract ──
    ;; W1 closed cohort C1 with (fast-LPT . hold), so W3 records the hold
    ;; instead of flipping the ordering lever for the required fast shards.
    ;; These pins make the hold observable and make any silent activation a
    ;; red run: the CLI default stays unset (runner applies fifo), the
    ;; repository variable never reaches parse-args, ci.yml carries no
    ;; ordering lever, every unusable-evidence fallback is NAMED, and
    ;; consecutive ordering/shard-plan generations are byte-identical on
    ;; identical inputs (determinism even for a future activation).

    (test-case "hold: ordering default stays unset (runner applies fifo) while the W1 fast-LPT hold stands"
      (check-equal? (parse-ordering '()) #f "unset CLI must not request an ordering")
      (check-equal? default-ordering 'fifo "runner default ordering stays fifo")
      (check-equal? known-orderings '(fifo lpt) "ordering vocabulary is unchanged")
      (define env-lpt (make-environment-variables))
      (environment-variables-set! env-lpt #"FAST_SHARD_ORDERING" #"lpt")
      (parameterize ([current-environment-variables env-lpt])
        (check-equal? (parse-ordering '()) #f "FAST_SHARD_ORDERING must not reach parse-args"))
      (define env-fifo (make-environment-variables))
      (environment-variables-set! env-fifo #"FAST_SHARD_ORDERING" #"fifo")
      (parameterize ([current-environment-variables env-fifo])
        (check-equal? (parse-ordering '()) #f "even the no-op value must not reach parse-args")))

    (test-case "hold: manual --ordering override still selects the requested ordering"
      (check-equal? (parse-ordering '("--ordering" "lpt")) 'lpt)
      (check-equal? (parse-ordering '("--ordering" "fifo")) 'fifo))

    (test-case "--ordering CLI seam: fifo and lpt accepted, invalid exits 2 with a named diagnostic"
      (define dir (make-temporary-file "w3-ord-cli-~a" 'directory))
      (define solo
        (write-fixture! dir
                        "solo.rkt"
                        "#lang racket/base\n(require rackunit)\n(module+ test (check-equal? 1 1))\n"))
      (check-equal? (cdr (run/capture (format "racket ~a ~a --ordering fifo" (find-runner) solo)))
                    0
                    "--ordering fifo is accepted (exit 0)")
      (check-equal? (cdr (run/capture (format "racket ~a ~a --ordering lpt" (find-runner) solo)))
                    0
                    "--ordering lpt is accepted (exit 0)")
      (define invalid-res (run/capture (format "racket ~a ~a --ordering bogus" (find-runner) solo)))
      (check-equal? (cdr invalid-res) 2 "invalid --ordering value exits 2")
      (check-true (regexp-match? #rx"--ordering" (car invalid-res))
                  (format "invalid --ordering diagnostic names the option; got: ~a"
                          (car invalid-res)))
      (delete-dir/safe dir))

    (test-case "hold: ci.yml carries no ordering lever for the required fast shards"
      (define ci (file->string (build-path project-root ".github" "workflows" "ci.yml")))
      (check-false (regexp-match? #rx"--ordering" ci)
                   "required CI must not pass --ordering while the hold stands")
      (check-false
       (regexp-match? #rx"FAST_SHARD_ORDERING" ci)
       "required CI must not reference an ordering repository variable while the hold stands"))

    (test-case "fallback: lpt with missing evidence falls back to fifo with a named reason"
      (define rec
        (prepare-ordering '("tests/w3-a.rkt" "tests/w3-b.rkt") 'lpt default-max-age-seconds #f))
      (check-eq? (ordering-record-mode rec) 'fifo)
      (check-eq? (ordering-record-requested rec) 'lpt)
      (check-eq? (ordering-record-snapshot-status rec) 'missing)
      (check-true (string? (ordering-record-fallback-reason rec))
                  "missing-evidence fallback must name its reason, never fall back silently"))

    (test-case "fallback: stale evidence falls back to fifo with the named stale reason"
      (define dir (make-temporary-file "w3-stale-~a" 'directory))
      (define snap (build-path dir "durations.json"))
      (write-json-snapshot! snap (list (cons "tests/w3-a.rkt" 2.0)))
      (file-or-directory-modify-seconds snap (- (current-seconds) (* 2 default-max-age-seconds)))
      (define rec
        (prepare-ordering '("tests/w3-a.rkt") 'lpt default-max-age-seconds (path->string snap)))
      (check-eq? (ordering-record-mode rec) 'fifo)
      (check-eq? (ordering-record-snapshot-status rec) 'stale)
      (check-true (and (string? (ordering-record-fallback-reason rec))
                       (string-contains? (ordering-record-fallback-reason rec) "max accepted age"))
                  (format "stale fallback must name the freshness threshold; got: ~a"
                          (ordering-record-fallback-reason rec)))
      (delete-dir/safe dir))

    (test-case "fallback: malformed evidence falls back to fifo with the named malformed reason"
      (define dir (make-temporary-file "w3-bad-~a" 'directory))
      (define snap (build-path dir "durations.json"))
      (call-with-output-file snap
                             #:exists 'replace
                             (lambda (out) (display "{\"files\":[{\"path\": " out)))
      (define rec
        (prepare-ordering '("tests/w3-a.rkt") 'lpt default-max-age-seconds (path->string snap)))
      (check-eq? (ordering-record-mode rec) 'fifo)
      (check-eq? (ordering-record-snapshot-status rec) 'malformed)
      (check-true (and (string? (ordering-record-fallback-reason rec))
                       (string-contains? (ordering-record-fallback-reason rec) "invalid shape"))
                  (format "malformed fallback must name its reason; got: ~a"
                          (ordering-record-fallback-reason rec)))
      (delete-dir/safe dir))

    (test-case "fallback: wrong-inventory evidence falls back to fifo with the named overlap reason"
      (define dir (make-temporary-file "w3-inv-~a" 'directory))
      (define snap (build-path dir "durations.json"))
      (write-json-snapshot! snap (list (cons "elsewhere/not-selected.rkt" 42.0)))
      (define rec
        (prepare-ordering '("tests/w3-a.rkt") 'lpt default-max-age-seconds (path->string snap)))
      (check-eq? (ordering-record-mode rec) 'fifo)
      (check-eq? (ordering-record-snapshot-status rec) 'wrong-inventory)
      (check-true (and (string? (ordering-record-fallback-reason rec))
                       (string-contains? (ordering-record-fallback-reason rec) "do not overlap"))
                  (format "wrong-inventory fallback must name its reason; got: ~a"
                          (ordering-record-fallback-reason rec)))
      (delete-dir/safe dir))

    (test-case "ordering: lpt applies only from fresh, well-formed, inventory-compatible evidence; path ties break deterministically"
      (define dir (make-temporary-file "w3-lpt-~a" 'directory))
      (define snap (build-path dir "durations.json"))
      (write-json-snapshot!
       snap
       (list (cons "tests/w3-a.rkt" 2.0) (cons "tests/w3-b.rkt" 9.0) (cons "tests/w3-c.rkt" 2.0)))
      (define files (list "tests/w3-c.rkt" "tests/w3-a.rkt" "tests/w3-b.rkt"))
      (define rec (prepare-ordering files 'lpt default-max-age-seconds (path->string snap)))
      (check-eq? (ordering-record-mode rec) 'lpt)
      (check-eq? (ordering-record-snapshot-status rec) 'usable)
      (check-equal? (ordering-record-fallback-reason rec) #f)
      (check-true (and (string? (ordering-record-snapshot-checksum rec))
                       (not (string=? (ordering-record-snapshot-checksum rec) "")))
                  "the applied decision records the evidence checksum")
      (check-equal? (order-files files rec)
                    (list "tests/w3-b.rkt" "tests/w3-a.rkt" "tests/w3-c.rkt")
                    "longest first; byte-length-and-lexicographic path tie-break for equal durations")
      (delete-dir/safe dir))

    (test-case "determinism: consecutive ordering and shard-plan generations are byte-identical on identical inputs"
      (define dir (make-temporary-file "w3-det-~a" 'directory))
      (define snap (build-path dir "durations.json"))
      (write-json-snapshot!
       snap
       (list (cons "tests/w3-a.rkt" 2.0) (cons "tests/w3-b.rkt" 9.0) (cons "tests/w3-c.rkt" 2.0)))
      (define files (list "tests/w3-c.rkt" "tests/w3-a.rkt" "tests/w3-b.rkt"))
      (define rec1 (prepare-ordering files 'lpt default-max-age-seconds (path->string snap)))
      (define rec2 (prepare-ordering files 'lpt default-max-age-seconds (path->string snap)))
      (check-equal? (ordering-record->jsexpr rec1)
                    (ordering-record->jsexpr rec2)
                    "two consecutive ordering decisions serialize identically")
      (check-equal? (order-files files rec1) (order-files files rec2))
      (check-equal? (string->bytes/utf-8 (format "~s" (order-files files rec1)))
                    (string->bytes/utf-8 (format "~s" (order-files files rec2)))
                    "byte-identical orderings across runs")
      (define durations (hash "tests/w3-a.rkt" 2.0 "tests/w3-b.rkt" 9.0 "tests/w3-c.rkt" 2.0))
      (define plan1 (build-shard-plan files 2 #:durations durations))
      (define plan2 (build-shard-plan files 2 #:durations durations))
      (check-equal? (plan->jsexpr plan1)
                    (plan->jsexpr plan2)
                    "two consecutive shard-plan generations are structurally identical")
      (check-equal? (canonical-plan-bytes plan1)
                    (canonical-plan-bytes plan2)
                    "byte-identical shard assignments across runs")
      (delete-dir/safe dir))))

(module+ main
  (exit (run-tests suite)))

;; ── W3 helpers ──────────────────────────────────────────────────────────
;; parse-args is loaded dynamically (it lives in the CLI module the runner
;; wraps); the helper extracts only the ordering slot of its 26 values.
(define cli-parse-args
  (dynamic-require (build-path project-root "scripts" "run-tests" "cli.rkt") 'parse-args))

(define (parse-ordering args)
  (define-values (_jobs
                  _seq?
                  _timeout
                  _strict?
                  _suite
                  _extra
                  _repeat
                  _record?
                  _inventory?
                  _diagnose?
                  _mode
                  _scheduler
                  _json
                  _ledger
                  _profile
                  _lint-metadata?
                  _changed-base
                  _changed-head
                  _explain?
                  _impact-dry-run?
                  _prioritize
                  _failure-history
                  _generate-covers-manifest?
                  _shard-plan
                  _durations
                  ordering)
    (cli-parse-args args))
  ordering)

;; Deterministic JSON duration snapshot in the shard-plan artifact shape.
(define (write-json-snapshot! path pairs)
  (call-with-output-file
   path
   #:exists 'replace
   (lambda (out)
     (display "{\"files\":[" out)
     (for ([pair (in-list pairs)]
           [i (in-naturals)])
       (unless (zero? i)
         (display "," out))
       (fprintf out "{\"path\":~s,\"duration_seconds\":~a}" (car pair) (cdr pair)))
     (display "]}" out))))

;; Canonical byte rendering of a shard plan: hashes become key-sorted
;; association lists (recursively), so two generations on identical inputs
;; MUST render to identical bytes regardless of hash iteration order.
(define (canonical-plan-bytes plan)
  (string->bytes/utf-8 (format "~s" (canon/plan (plan->jsexpr plan)))))

(define (canon/plan v)
  (cond
    [(hash? v)
     (sort (for/list ([(k val) (in-hash v)])
             (cons k (canon/plan val)))
           string<?
           #:key (lambda (kv) (format "~a" (car kv))))]
    [(pair? v)
     (if (list? v)
         (map canon/plan v)
         (cons (canon/plan (car v)) (canon/plan (cdr v))))]
    [else v]))

(module+ test
  (void (run-tests suite)))
