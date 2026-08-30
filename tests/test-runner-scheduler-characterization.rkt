#lang racket/base

;; @speed fast
;; @boundary unit

;; BOUNDARY: unit
;; tests/test-runner-scheduler-characterization.rkt
;; v1.00.23 W0 — characterize the v1.00.22 scheduler CONTRACT WITHOUT
;; changing any runner behavior. W2 flips the pinned seams.
;;
;; Pins:
;;  1. fixed-batch barrier: with jobs=2 the third file cannot start until
;;     BOTH files of the first batch finish (marker-synchronized, bounded).
;;  2. result order == input file order (never completion order).
;;  3. per-file timeout: file-timeout kills the subprocess -> exit 2 / TIMEOUT;
;;     the meta @timeout directive (seconds) is honored.
;;  4. status characters: "." exit 0, "T" exit 2 (timeout), "S" SKIPPED_BY_PROFILE,
;;     "F" any other failure (runner.rkt:353-358).
;;  5. exception/result classification matrix (parse.rkt classify-test-result).
;;  6. serial/parallel ownership seam (runner.rkt:412-414): mutation-sensitive
;;     files run alone and FIRST, then the parallel batch; results still sort
;;     by input order.
;;  7. absent --scheduler CLI seam: the option is rejected today; W2 flips
;;     this pin by adding the option.

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
                 "(sleep 1.1)\n"
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

    (test-case "fixed-batch barrier: jobs=2, third file waits for BOTH first-batch files"
      (define dir (make-temporary-file "w0-barrier-~a" 'directory))
      (define slow (write-fixture! dir "batch-slow.rkt" fixture-slow))
      (define quick (write-fixture! dir "batch-quick.rkt" fixture-quick))
      (define probe (write-fixture! dir "batch-probe.rkt" fixture-probe))
      (define quick-done (unique-tmp-path "w0-sched-quick-done"))
      (define slow-done (unique-tmp-path "w0-sched-slow-done"))
      (define probe-out (unique-tmp-path "w0-sched-probe-out"))
      (putenv "W0_SCHED_QUICK_DONE" quick-done)
      (putenv "W0_SCHED_SLOW_DONE" slow-done)
      (putenv "W0_SCHED_PROBE_OUT" probe-out)
      (define results (run-all-files (list slow quick probe) 2 #f))
      (putenv "W0_SCHED_QUICK_DONE" "")
      (putenv "W0_SCHED_SLOW_DONE" "")
      (putenv "W0_SCHED_PROBE_OUT" "")
      (check-equal? (map classify-test-result results)
                    (list 'ZERO_PARSED 'ZERO_PARSED 'ZERO_PARSED)
                    "all three fixtures exit 0 with zero parsed tests")
      (check-true (file-exists? quick-done) "first-batch quick file completed")
      (check-true (file-exists? slow-done) "first-batch slow file completed")
      (check-equal?
       (file->string probe-out)
       "after-full-batch"
       "batch-2 probe started only after BOTH first-batch files finished (fixed-batch barrier)")
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
               "#lang racket/base\n(require does-not-exist-coll/x)\n"
               'MODULE_LOAD_FAILURE
               1
               0
               0
               0)
         (list "boom.rkt" "#lang racket/base\n(error \"boom\")\n" 'UNKNOWN_FAILURE 1 0 0 0)))
      (for ([c (in-list cases)])
        (match-define (list name content expected exit-code passed failed total) c)
        (define r (run-single-file (mk name content) #:timeout 10000))
        (check-equal? (classify-test-result r) expected (format "~a classifies as ~a" name expected))
        (check-equal? (test-file-result-exit-code r) exit-code (format "~a exit code" name))
        (check-equal? (test-file-result-passed r) passed (format "~a passed" name))
        (check-equal? (test-file-result-failed r) failed (format "~a failed" name))
        (check-equal? (test-file-result-total r) total (format "~a total" name)))
      (delete-dir/safe dir))

    (test-case "serial/parallel ownership seam: mutation-sensitive files run first (runner.rkt:412-414)"
      (define dir (make-temporary-file "w0-seam-~a" 'directory))
      (define serial-a (write-fixture! dir "serial-a.rkt" fixture-serial))
      (define par-b (write-fixture! dir "par-b.rkt" fixture-par))
      (define par-c (write-fixture! dir "par-c.rkt" fixture-par))
      (define serial-marker (unique-tmp-path "w0-sched-serial-marker"))
      (define par-out (unique-tmp-path "w0-sched-par-out"))
      (putenv "W0_SCHED_SERIAL_MARKER" serial-marker)
      (putenv "W0_SCHED_PAR_OUT" par-out)
      (define-values (exit-code run-results)
        (run-suite-once (list serial-a par-b par-c)
                        2 ; jobs
                        #f ; timeout-ms (default 120s per file)
                        #f ; strict?
                        "w0-scheduler-characterization"
                        1
                        1 ; repeat-num / repeat-total
                        'subprocess
                        #f ; json-out
                        #f ; ledger
                        'fast ; profile
                        #:shard #f
                        #:phases (hasheq)))
      (putenv "W0_SCHED_SERIAL_MARKER" "")
      (putenv "W0_SCHED_PAR_OUT" "")
      (check-true (file-exists? serial-marker) "serial-phase file completed")
      (check-true
       (regexp-match? #rx"after-serial" (file->string par-out))
       "parallel files observed the serial phase's completion -> serial runs before parallel")
      (check-false (regexp-match? #rx"before-serial" (file->string par-out))
                   "no parallel file started before the serial phase finished")
      (check-equal? (map test-file-result-path run-results)
                    (list serial-a par-b par-c)
                    "run-suite-once results sorted by input order")
      (delete-dir/safe dir))

    (test-case "absent --scheduler CLI seam (W2 flips this pin)"
      (define dir (make-temporary-file "w0-cli-~a" 'directory))
      (write-fixture! dir "solo.rkt" "#lang racket/base\n(define x 1)\n")
      (define res (run/capture (format "racket ~a ~a --scheduler batch" (find-runner) dir)))
      (check-false (= (cdr res) 0) "today --scheduler is rejected (nonzero exit)")
      (check-true (or (regexp-match? #rx"--scheduler" (car res))
                      (or (regexp-match? #rx"unrecognized" (string-foldcase (car res)))
                          (regexp-match? #rx"unknown option" (string-foldcase (car res)))
                          (regexp-match? #rx"unexpected" (string-foldcase (car res)))
                          (regexp-match? #rx"not found" (string-foldcase (car res)))))
                  (format "error output names the option; got: ~a" (car res)))
      (define help-res (run/capture (format "racket ~a --help" (find-runner))))
      (check-equal? (cdr help-res) 0 "--help exits 0")
      (check-false (regexp-match? #rx"--scheduler" (car help-res))
                   "no scheduler option is advertised in --help today")
      (delete-dir/safe dir))))

(module+ main
  (exit (run-tests suite)))

(module+ test
  (void (run-tests suite)))
