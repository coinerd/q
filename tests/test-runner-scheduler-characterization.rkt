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

(require rackunit
         rackunit/text-ui
         racket/string
         racket/match
         racket/file
         racket/path
         racket/runtime-path
         racket/system
         (prefix-in p: racket/port)
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
      (delete-dir/safe dir))))

(module+ main
  (exit (run-tests suite)))

(module+ test
  (void (run-tests suite)))
