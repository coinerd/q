#lang racket/base

;; run-tests.rkt - Parallel test runner with per-file result tracking.
;;
;; Thin facade: re-exports from focused sub-modules in run-tests/.
;;   classify.rkt     — metadata parsing, file classifiers, discovery
;;   parse.rkt        — output parsing, failure extraction, result struct
;;   reporting.rkt    — summary printing, failure logs, formatting
;;   cli.rkt          — argument parsing and validation
;;   gate-evidence.rkt — CI gate evidence recording
;;   inventory.rkt    — inventory report mode
;;
;; This module retains: run-single-file, run-all-files, run-suite-once, main.

(require racket/string
         racket/match
         racket/math
         racket/path
         racket/file
         racket/system
         racket/port
         racket/list
         racket/future
         (only-in "run-tests/classify.rkt"
                  base-dir
                  normalize-test-path
                  collect-test-files
                  get-file-metadata
                  mutating-file?
                  restore-repo-surfaces!
                  clean-stale-bytecode!
                  repo-surface-files
                  file-has-rackunit-tests?
                  slow-file?
                  tui-file?
                  security-file?
                  arch-file?
                  runtime-file?
                  extensions-file?
                  workflows-file?
                  support-test-module?
                  smoke-excluded?
                  slow-patterns
                  mutating-patterns
                  clear-metadata-cache!)
         (only-in "run-tests/parse.rkt"
                  test-file-result
                  test-file-result?
                  test-file-result-path
                  test-file-result-exit-code
                  test-file-result-stdout-bytes
                  test-file-result-stderr-bytes
                  test-file-result-elapsed-ms
                  test-file-result-passed
                  test-file-result-failed
                  test-file-result-total
                  make-test-file-result
                  bytes->string*
                  parse-raco-output
                  normalize-counts
                  effective-exit-code
                  extract-failure-lines
                  truncate-test-output
                  DEFAULT-OUTPUT-CAP)
         (only-in "run-tests/reporting.rkt"
                  print-summary
                  save-failure-logs
                  format-duration
                  summary-exit-code
                  make-unique-log-name)
         (only-in "run-tests/cli.rkt" usage parse-args validate-args! known-suites)
         (only-in "run-tests/gate-evidence.rkt" record-gate-evidence!)
         (only-in "run-tests/inventory.rkt"
                  print-inventory
                  classify-exclusion-reason
                  detect-high-risk-flags
                  compute-inventory-hash))

(provide test-file-result
         test-file-result?
         test-file-result-path
         test-file-result-exit-code
         test-file-result-stdout-bytes
         test-file-result-stderr-bytes
         test-file-result-elapsed-ms
         test-file-result-passed
         test-file-result-failed
         test-file-result-total
         make-test-file-result
         parse-raco-output
         normalize-counts
         extract-failure-lines
         run-single-file
         run-all-files
         collect-test-files
         mutating-file?
         mutating-patterns
         repo-surface-files
         restore-repo-surfaces!
         print-summary
         save-failure-logs
         format-duration
         summary-exit-code
         bytes->string*
         clean-stale-bytecode!
         file-has-rackunit-tests?
         parse-args
         arch-file?
         runtime-file?
         extensions-file?
         workflows-file?
         slow-file?
         tui-file?
         mutating-file?
         validate-args!
         known-suites
         record-gate-evidence!
         get-file-metadata
         clear-metadata-cache!
         make-unique-log-name
         truncate-test-output
         print-inventory
         classify-exclusion-reason
         detect-high-risk-flags
         compute-inventory-hash)

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

(define (run-single-file test-path #:timeout [timeout #f])
  (define resolved-path
    (if (absolute-path? test-path)
        test-path
        (simplify-path (build-path base-dir test-path))))
  (define file-timeout
    (let ([meta-timeout (hash-ref (get-file-metadata test-path) 'timeout #f)])
      (if meta-timeout
          (* meta-timeout 1000)
          (or timeout 120000))))
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

(define (split-list lst n)
  (cond
    [(null? lst) '()]
    [else (cons (take lst (min n (length lst))) (split-list (drop lst (min n (length lst))) n))]))

(define (run-all-files files jobs timeout)
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
                  (define result (run-single-file f #:timeout (or timeout 120000)))
                  (define exit-code (test-file-result-exit-code result))
                  (cond
                    [(= exit-code 0) (display ".")]
                    [(= exit-code 2) (display "T")]
                    [else (display "F")])
                  (flush-output)
                  (add-result! result)))))
    (for-each thread-wait batch-threads)
    (set! gc-counter (add1 gc-counter))
    (when (or (= 0 (modulo gc-counter 5)) (= gc-counter total-batches))
      (collect-garbage 'major)))
  (newline)
  (define file-order
    (for/hash ([f (in-list files)]
               [i (in-naturals)])
      (values f i)))
  (sort (unbox results) < #:key (lambda (r) (hash-ref file-order (test-file-result-path r) 0))))

(define (run-suite-once suite-files jobs timeout-ms strict? suite-label repeat-num repeat-total)
  (define t0 (current-inexact-milliseconds))
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

(define (main args)
  (define-values (jobs sequential? timeout strict? suite extra-files repeat record-gate? inventory?)
    (parse-args args))
  (validate-args! jobs sequential? timeout strict? suite extra-files repeat record-gate? inventory?)
  (define cleaned-dirs (clean-stale-bytecode! (current-directory)))
  (when (> cleaned-dirs 0)
    (printf ";; run-tests: cleaned ~a stale compiled/ director~a~n"
            cleaned-dirs
            (if (= cleaned-dirs 1) "y" "ies")))
  (define suite-files
    (if (pair? extra-files)
        (map normalize-test-path extra-files)
        (collect-test-files suite)))
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
  (define last-results (box '()))
  (for ([run-num (in-range 1 (add1 repeat))])
    (when (> repeat 1)
      (printf "~n;; ── Run ~a/~a ──~n" run-num repeat))
    (define-values (exit-code results)
      (run-suite-once suite-files jobs timeout-ms strict? suite-label run-num repeat))
    (set-box! last-results results)
    (unless (zero? exit-code)
      (exit exit-code)))
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
