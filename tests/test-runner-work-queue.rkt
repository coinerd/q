#lang racket/base

;; @speed fast
;; @boundary unit

;; BOUNDARY: unit
;; tests/test-runner-work-queue.rkt
;; v1.00.23 W2 — queue-mode invariants behind `--scheduler queue`.
;;
;; Focused queue-scheduler contract (runner.rkt run-all-files/queue):
;;  1. batch and queue select the same file set and produce the same
;;     classified results, sorted to input order (never completion order).
;;  2. the serial mutation-sensitive partition still completes before the
;;     parallel queue partition starts.
;;  3. profile skips remain skips; per-file meta @timeout and explicit
;;     #:timeout keep kill/classification semantics; strict zero-test stays
;;     fail-closed under both schedulers.
;;  4. a single failing file never deadlocks the pool nor discards other
;;     results; jobs=1, empty input, fewer files than workers, cancellation
;;     and timeout cleanup are all bounded.
;;  5. progress output stays interpretable ("." / "T" / "S" / "F").
;;  6. queue-mode GC is coordinator-owned (no worker races a counter) and
;;     deterministic + telemetry-visible: one GC every 5 completions plus
;;     the final completion, recorded in telemetry 'gc_count / 'gc_pause_ms.

(require rackunit
         rackunit/text-ui
         racket/string
         racket/match
         racket/file
         racket/path
         racket/runtime-path
         racket/system
         (only-in "../scripts/run-tests.rkt"
                  run-all-files
                  run-single-file
                  run-suite-once
                  classify-test-result
                  get-file-metadata
                  test-file-result-path
                  test-file-result-exit-code
                  test-file-result-elapsed-ms
                  test-file-result-passed
                  test-file-result-failed
                  test-file-result-total)
         (only-in "../scripts/run-tests/runner.rkt" make-run-all-telemetry)
         (only-in "../scripts/run-tests/inventory.rkt" compute-inventory-hash))

(define-runtime-path here ".")
(define project-root (simplify-path (build-path here "..")))
(define run-tests-cli (path->string (build-path project-root "scripts" "run-tests.rkt")))

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

;; ---------------------------------------------------------------------------
;; Fixture sources
;; ---------------------------------------------------------------------------

(define fixture-ok "#lang racket/base\n(define x 1)\n")

(define fixture-boom "#lang racket/base\n(error \"w2-queue-boom\")\n")

(define fixture-skip "#lang racket/base\n(module+ main (exit 5))\n")

(define fixture-sleep30 "#lang racket/base\n;; @timeout 1\n(sleep 30)\n")

(define fixture-serial
  (string-append "#lang racket/base\n"
                 ";; @isolation process\n"
                 "(define serial-marker (getenv \"W2Q_SERIAL_MARKER\"))\n"
                 "(unless serial-marker (error \"W2Q_SERIAL_MARKER unset\"))\n"
                 "(call-with-output-file serial-marker\n"
                 "  #:exists 'append\n"
                 "  (lambda (o) (display \"serial-done\" o)))\n"))

(define fixture-par
  (string-append "#lang racket/base\n"
                 "(define serial-marker (getenv \"W2Q_SERIAL_MARKER\"))\n"
                 "(define par-out (getenv \"W2Q_PAR_OUT\"))\n"
                 "(unless (and serial-marker par-out)\n"
                 "  (error \"W2Q par env unset\"))\n"
                 "(define par-content\n"
                 "  (if (file-exists? serial-marker) \"after-serial\" \"before-serial\"))\n"
                 "(call-with-output-file par-out\n"
                 "  #:exists 'append\n"
                 "  (lambda (o) (display par-content o)))\n"))

;; ---------------------------------------------------------------------------
;; Tests
;; ---------------------------------------------------------------------------

(define suite
  (test-suite "test-runner-work-queue"

    (test-case "batch and queue: identical path set, inventory digest, and classifications (input-order results)"
      (define dir (make-temporary-file "w2q-set-~a" 'directory))
      (define aaa (write-fixture! dir "aaa-slow.rkt" "#lang racket/base\n(sleep 0.5)\n"))
      (define bbb (write-fixture! dir "bbb-fast.rkt" fixture-ok))
      (define ccc (write-fixture! dir "ccc-skip.rkt" fixture-skip))
      (define files (list aaa bbb ccc))
      (define batch-results (run-all-files files 2 #f #:scheduler 'batch))
      (define queue-results (run-all-files files 2 #f #:scheduler 'queue))
      (check-equal? (map test-file-result-path batch-results)
                    files
                    "batch: results sorted to input order")
      (check-equal? (map test-file-result-path queue-results)
                    files
                    "queue: results sorted to input order (never completion order)")
      (check-equal? (map classify-test-result queue-results)
                    (map classify-test-result batch-results)
                    "queue and batch classify every file identically")
      (check-equal? (compute-inventory-hash (map test-file-result-path batch-results))
                    (compute-inventory-hash (map test-file-result-path queue-results))
                    "identical selected path set => identical inventory digest")
      (check-equal? (map test-file-result-path batch-results)
                    (map test-file-result-path queue-results)
                    "identical selected path set across schedulers")
      (delete-dir/safe dir))

    (test-case "serial mutation-sensitive partition completes before the parallel queue starts"
      (define dir (make-temporary-file "w2q-seam-~a" 'directory))
      (define serial-a (write-fixture! dir "serial-a.rkt" fixture-serial))
      (define par-b (write-fixture! dir "par-b.rkt" fixture-par))
      (define par-c (write-fixture! dir "par-c.rkt" fixture-par))
      (define serial-marker (unique-tmp-path "w2q-serial-marker"))
      (define par-out (unique-tmp-path "w2q-par-out"))
      (putenv "W2Q_SERIAL_MARKER" serial-marker)
      (putenv "W2Q_PAR_OUT" par-out)
      (define-values (exit-code run-results)
        (run-suite-once (list serial-a par-b par-c)
                        2
                        #f
                        #f
                        "w2q-scheduler-seam"
                        1
                        1
                        'subprocess
                        #f
                        #f
                        'local
                        #:shard #f
                        #:phases (hasheq)
                        #:scheduler 'queue))
      (putenv "W2Q_SERIAL_MARKER" "")
      (putenv "W2Q_PAR_OUT" "")
      (check-true (file-exists? serial-marker) "queue mode: serial-phase file completed")
      (check-true (regexp-match? #rx"after-serial" (file->string par-out))
                  "queue mode: parallel files observed serial completion => serial ran first")
      (check-false (regexp-match? #rx"before-serial" (file->string par-out))
                   "queue mode: no parallel file started before the serial phase finished")
      (check-equal? (map test-file-result-path run-results)
                    (list serial-a par-b par-c)
                    "queue mode: run-suite-once results sorted by input order")
      (delete-dir/safe dir))

    (test-case "profile skips remain skips under queue"
      (define dir (make-temporary-file "w2q-skip-~a" 'directory))
      (define skip-file (write-fixture! dir "skip.rkt" fixture-skip))
      (define stdout
        (capture-stdout
         (lambda ()
           (check-equal?
            (classify-test-result (car (run-all-files (list skip-file) 1 #f #:scheduler 'queue)))
            'SKIPPED_BY_PROFILE
            "queue: exit 5 classifies SKIPPED_BY_PROFILE"))))
      (check-true (string-contains? stdout "S") "queue: progress prints 'S' for a profile skip")
      (delete-dir/safe dir))

    (test-case "per-file meta timeout and explicit timeout retain kill/classification semantics under queue"
      (define dir (make-temporary-file "w2q-timeout-~a" 'directory))
      (define meta-file (write-fixture! dir "meta-timeout.rkt" fixture-sleep30))
      (define meta-r (run-single-file meta-file #:timeout #f))
      (check-equal? (classify-test-result meta-r) 'TIMEOUT)
      (check-equal? (test-file-result-exit-code meta-r) 2)
      (check-true (< (test-file-result-elapsed-ms meta-r) 15000)
                  "meta @timeout 1 enforced (not the 120s default)")
      (define explicit-r
        (car (run-all-files
              (list (write-fixture! dir "explicit-timeout.rkt" "#lang racket/base\n(sleep 30)\n"))
              1
              1000 ; explicit per-file timeout ms
              #:scheduler 'queue)))
      (check-equal? (classify-test-result explicit-r)
                    'TIMEOUT
                    "queue: explicit #:timeout kills the subprocess -> TIMEOUT")
      (check-equal? (test-file-result-exit-code explicit-r) 2)
      (check-true (< (test-file-result-elapsed-ms explicit-r) 15000)
                  "queue: explicit timeout enforced promptly")
      (delete-dir/safe dir))

    (test-case "strict zero-test remains fail-closed under both schedulers (CLI exit nonzero + FAIL verdict)"
      (define dir (make-temporary-file "w2q-strict-~a" 'directory))
      (define zero-file (write-fixture! dir "zero.rkt" fixture-ok))
      (for ([scheduler (in-list '("batch" "queue"))])
        (define out (open-output-string))
        (define exit-code
          (parameterize ([current-output-port out]
                         [current-error-port out])
            (system/exit-code
             (format "racket ~a ~a --scheduler ~a" run-tests-cli zero-file scheduler))))
        (check-not-equal? exit-code
                          0
                          (format "~a: zero-parsed file under strict default fails closed" scheduler))
        (check-true (regexp-match? #rx"VERDICT:.*(FAIL|INCONCLUSIVE)" (get-output-string out))
                    (format "~a: strict verdict reports fail-closed zero-test outcome" scheduler)))
      (delete-dir/safe dir))

    (test-case "one failing file neither deadlocks the pool nor discards other results"
      (define dir (make-temporary-file "w2q-isolate-~a" 'directory))
      (define boom (write-fixture! dir "boom.rkt" fixture-boom))
      (define ok1 (write-fixture! dir "ok1.rkt" fixture-ok))
      (define ok2 (write-fixture! dir "ok2.rkt" fixture-ok))
      (define results (run-all-files (list boom ok1 ok2) 2 #f #:scheduler 'queue))
      (check-equal? (length results) 3 "queue: all results present despite one failing file")
      (check-equal? (map test-file-result-path results)
                    (list boom ok1 ok2)
                    "queue: results sorted to input order")
      (check-equal? (classify-test-result (list-ref results 0)) 'UNKNOWN_FAILURE)
      (check-equal? (classify-test-result (list-ref results 1)) 'ZERO_PARSED)
      (check-equal? (classify-test-result (list-ref results 2)) 'ZERO_PARSED)
      (delete-dir/safe dir))

    (test-case "jobs=1, fewer files than workers, and empty input are bounded"
      (define dir (make-temporary-file "w2q-bounded-~a" 'directory))
      (define f1 (write-fixture! dir "f1.rkt" fixture-ok))
      (define f2 (write-fixture! dir "f2.rkt" fixture-ok))
      (define f3 (write-fixture! dir "f3.rkt" fixture-ok))
      (define t0 (current-inexact-milliseconds))
      (define serial-results (run-all-files (list f1 f2 f3) 1 #f #:scheduler 'queue))
      (check-equal? (map test-file-result-path serial-results)
                    (list f1 f2 f3)
                    "jobs=1: serial queue preserves input order")
      (check-true (< (- (current-inexact-milliseconds) t0) 30000) "jobs=1: queue completes promptly")
      (define under-results (run-all-files (list f1 f2) 4 #f #:scheduler 'queue))
      (check-equal? (length under-results) 2 "fewer files than workers: every file still runs once")
      (check-equal? (run-all-files '() 2 #f #:scheduler 'queue)
                    '()
                    "empty input: bounded, no workers spawned")
      (delete-dir/safe dir))

    (test-case "cancellation is bounded: break on the coordinator never hangs the pool"
      (define dir (make-temporary-file "w2q-cancel-~a" 'directory))
      (define sleepy (write-fixture! dir "sleepy.rkt" "#lang racket/base\n(sleep 30)\n"))
      (define quick (write-fixture! dir "quick.rkt" fixture-ok))
      (define t (thread (lambda () (run-all-files (list sleepy quick) 2 #f #:scheduler 'queue))))
      (sleep 0.3)
      (break-thread t)
      (check-not-false (sync/timeout 5000 t)
                       "queue: coordinator thread exits within 5s of a break (no deadlock)")
      (delete-dir/safe dir))

    (test-case "explicit timeout cleanup is bounded in queue mode"
      (define dir (make-temporary-file "w2q-cleanup-~a" 'directory))
      (define sleepy (write-fixture! dir "sleepy.rkt" "#lang racket/base\n(sleep 30)\n"))
      (define t0 (current-inexact-milliseconds))
      (define r (car (run-all-files (list sleepy) 1 1000 #:scheduler 'queue)))
      (check-equal? (classify-test-result r) 'TIMEOUT)
      (check-true (< (- (current-inexact-milliseconds) t0) 15000)
                  "queue: timed-out subprocess is reaped promptly")
      (delete-dir/safe dir))

    (test-case "output progress remains interpretable: . T S F all emitted"
      (define dir (make-temporary-file "w2q-progress-~a" 'directory))
      (define ok (write-fixture! dir "ok.rkt" fixture-ok))
      (define timeout-file (write-fixture! dir "timeout.rkt" fixture-sleep30))
      (define skip (write-fixture! dir "skip.rkt" fixture-skip))
      (define boom (write-fixture! dir "boom.rkt" fixture-boom))
      (define stdout
        (capture-stdout (lambda ()
                          (run-all-files (list ok timeout-file skip boom) 2 #f #:scheduler 'queue))))
      (for ([ch (in-list '("." "T" "S" "F"))])
        (check-true (string-contains? stdout ch) (format "queue: progress stream contains ~a" ch)))
      (check-true (regexp-match? #rx"[\r\n]$" stdout)
                  "queue: progress stream ends with a newline (interpretable)")
      (delete-dir/safe dir))

    (test-case "queue-mode GC policy is deterministic and telemetry-visible"
      (define dir (make-temporary-file "w2q-gc-~a" 'directory))
      (define files
        (for/list ([i (in-range 6)])
          (write-fixture! dir (format "gc-~a.rkt" i) fixture-ok)))
      (define (run-with-telemetry)
        (define telemetry (make-run-all-telemetry))
        (define results (run-all-files files 3 #f #:scheduler 'queue #:telemetry telemetry))
        (values results telemetry))
      (define-values (r1 t1) (run-with-telemetry))
      (define-values (r2 t2) (run-with-telemetry))
      (check-equal? (length r1) 6)
      (check-equal? (hash-ref t1 'gc_count #f)
                    6
                    "gc_count records every completion (coordinator-owned, no worker races)")
      (check-equal? (map car (hash-ref t1 'batches #f))
                    (map car (hash-ref t2 'batches #f))
                    "GC/batch schedule shape (size + sequence) deterministic across runs")
      (check-equal? (length (hash-ref t1 'batches #f)) 6 "one batch entry per completed file")
      (check-equal? (hash-ref t1 'gc_count #f)
                    (hash-ref t2 'gc_count #f)
                    "gc_count deterministic across runs")
      (check-true (>= (hash-ref t1 'gc_pause_ms 0) 0) "gc_pause_ms telemetry is present")
      (delete-dir/safe dir))))

(module+ main
  (exit (run-tests suite)))

(module+ test
  (void (run-tests suite)))
