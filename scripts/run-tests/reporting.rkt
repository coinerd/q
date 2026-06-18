#lang racket/base

;; q/scripts/run-tests/reporting.rkt — Summary output and failure logs
;;
;; Print-summary, save-failure-logs, format-duration, summary-exit-code.
;; Extracted from run-tests.rkt (v0.96.16, AX1-2).
;; STABILITY: internal

(require racket/string
         racket/path
         racket/list
         json
         (only-in "ledger.rkt"
                  summarize-ledger-results
                  ledger-summary-counts
                  ledger-entry-matches-result?)
         (only-in "parse.rkt"
                  test-file-result-path
                  test-file-result-exit-code
                  test-file-result-stdout-bytes
                  test-file-result-stderr-bytes
                  test-file-result-elapsed-ms
                  test-file-result-passed
                  test-file-result-failed
                  test-file-result-total
                  classify-test-result
                  test-result->jsexpr
                  bytes->string*
                  extract-failure-lines))

(provide print-summary
         save-failure-logs
         format-duration
         summary-exit-code
         make-unique-log-name
         compute-verdict
         classify-test-result
         test-result->jsexpr
         write-json-results!
         print-ledger-summary
         format-verdict-line)

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

(define (summary-exit-code failure-count timeout-count)
  (cond
    [(and (> failure-count 0) (> timeout-count 0)) 3]
    [(> failure-count 0) 1]
    [(> timeout-count 0) 2]
    [else 0]))

;; Compute a verdict symbol from results: 'pass, 'fail, 'incomplete, 'inconclusive.
;; - 'fail:          at least one file exited non-zero (not timeout)
;; - 'incomplete:    at least one file timed out (exit-code 2)
;; - 'inconclusive:  all files passed but zero tests were parsed
;; - 'pass:          all files passed and at least one test was parsed
(define (compute-verdict results)
  (define failed-files
    (count (lambda (r)
             (and (not (= (test-file-result-exit-code r) 0))
                  (not (= (test-file-result-exit-code r) 2))))
           results))
  (define timeout-files (count (lambda (r) (= (test-file-result-exit-code r) 2)) results))
  (define total-tests (for/sum ([r (in-list results)]) (test-file-result-total r)))
  (cond
    [(> failed-files 0) 'fail]
    [(> timeout-files 0) 'incomplete]
    [(= total-tests 0) 'inconclusive]
    [else 'pass]))

(define (format-verdict-line verdict timeout-count)
  (case verdict
    [(fail) "  VERDICT:   ❌ FAIL"]
    [(incomplete)
     (format "  VERDICT:   ⏱ INCOMPLETE (~a timeout~a)"
             timeout-count
             (if (= timeout-count 1) "" "s"))]
    [(inconclusive) "  VERDICT:   ⚠ INCONCLUSIVE (0 tests parsed)"]
    [(pass) "  VERDICT:   ✅ PASS"]
    [else "  VERDICT:   ❓ UNKNOWN"]))

(define known-result-categories
  '(PASS ASSERTION_FAILURE
         MODULE_LOAD_FAILURE
         COMPILE_FAILURE
         TIMEOUT
         ENVIRONMENT_MISSING
         ZERO_PARSED
         USER_BREAK
         UNKNOWN_FAILURE))

(define (category-counts results)
  (define counts (make-hasheq))
  (for ([category (in-list known-result-categories)])
    (hash-set! counts category 0))
  (for ([r (in-list results)])
    (define category (classify-test-result r))
    (hash-set! counts category (add1 (hash-ref counts category 0))))
  counts)

(define (print-category-summary results)
  (define counts (category-counts results))
  (define non-zero
    (filter (lambda (category) (> (hash-ref counts category 0) 0)) known-result-categories))
  (when (pair? non-zero)
    (printf "  Category:  ~a~n"
            (string-join (for/list ([category (in-list non-zero)])
                           (format "~a=~a" category (hash-ref counts category)))
                         ", "))))

(define (category-counts->jsexpr counts)
  (for/hasheq ([category (in-list known-result-categories)]
               #:when (> (hash-ref counts category 0) 0))
    (values (string->symbol (symbol->string category)) (hash-ref counts category))))

(define (ledger-match ledger r)
  (and ledger
       (for/first ([entry (in-list ledger)]
                   #:when (ledger-entry-matches-result? entry r))
         entry)))

(define (test-result->ledger-jsexpr r ledger)
  (define base (test-result->jsexpr r))
  (define entry (ledger-match ledger r))
  (if entry
      (hash-set* base
                 'known_failure
                 #t
                 'issue
                 (hash-ref entry 'issue)
                 'owner
                 (hash-ref entry 'owner)
                 'release_blocking
                 (hash-ref entry 'release_blocking))
      (hash-set base 'known_failure #f)))

(define (write-json-results! path
                             results
                             #:suite [suite 'all]
                             #:mode [mode 'subprocess]
                             #:elapsed-ms [elapsed-ms 0]
                             #:ledger [ledger #f])
  (define passed-files (count (lambda (r) (= (test-file-result-exit-code r) 0)) results))
  (define failed-files
    (count (lambda (r)
             (and (not (= (test-file-result-exit-code r) 0))
                  (not (= (test-file-result-exit-code r) 2))))
           results))
  (define timeout-files (count (lambda (r) (= (test-file-result-exit-code r) 2)) results))
  (define counts (category-counts results))
  (define ledger-summary (and ledger (summarize-ledger-results ledger results)))
  (define payload
    (hasheq 'suite
            (symbol->string suite)
            'mode
            (symbol->string mode)
            'verdict
            (symbol->string (compute-verdict results))
            'elapsed_ms
            elapsed-ms
            'summary
            (hasheq 'files_total
                    (length results)
                    'files_passed
                    passed-files
                    'files_failed
                    failed-files
                    'files_timeout
                    timeout-files
                    'tests_total
                    (for/sum ([r (in-list results)]) (test-file-result-total r))
                    'tests_passed
                    (for/sum ([r (in-list results)]) (test-file-result-passed r))
                    'tests_failed
                    (for/sum ([r (in-list results)]) (test-file-result-failed r)))
            'category_counts
            (category-counts->jsexpr counts)
            'ledger
            (and ledger-summary (ledger-summary-counts ledger-summary))
            'files
            (if ledger
                (map (lambda (r) (test-result->ledger-jsexpr r ledger)) results)
                (map test-result->jsexpr results))))
  (call-with-output-file path #:exists 'truncate/replace (lambda (out) (write-json payload out))))

(define (print-ledger-summary ledger results)
  (define summary (summarize-ledger-results ledger results))
  (define counts (ledger-summary-counts summary))
  (newline)
  (displayln "KNOWN-FAILURE LEDGER")
  (printf "  Known failures: ~a~n" (hash-ref counts 'known_failures))
  (printf "  New failures: ~a~n" (hash-ref counts 'new_failures))
  (printf "  Unclassified failures: ~a~n" (hash-ref counts 'unclassified_failures))
  (printf "  Resolved known failures: ~a~n" (hash-ref counts 'resolved_known_failures))
  (printf "  Release-blocking known failures: ~a~n"
          (hash-ref counts 'release_blocking_known_failures))
  (when (> (hash-ref counts 'unclassified_failures) 0)
    (displayln "  ⛔ Ledger incomplete: unclassified failures remain."))
  (when (> (hash-ref counts 'new_failures) 0)
    (displayln "  ⛔ New failures require triage before broad gate approval."))
  summary)

(define (print-summary results total-start-ms)
  (define total-files (length results))
  (define passed-files (count (lambda (r) (= (test-file-result-exit-code r) 0)) results))
  (define failed-files
    (count (lambda (r)
             (and (not (= (test-file-result-exit-code r) 0))
                  (not (= (test-file-result-exit-code r) 2))))
           results))
  (define timeout-files (count (lambda (r) (= (test-file-result-exit-code r) 2)) results))
  (define zero-test-files
    (count (lambda (r) (and (= (test-file-result-exit-code r) 0) (= (test-file-result-total r) 0)))
           results))
  (define total-passed (for/sum ([r (in-list results)]) (test-file-result-passed r)))
  (define total-failed (for/sum ([r (in-list results)]) (test-file-result-failed r)))
  (define total-tests (for/sum ([r (in-list results)]) (test-file-result-total r)))
  (define verdict (compute-verdict results))
  (newline)
  (displayln "═══════════════════════════════════════════════════════════")
  (displayln "                    TEST SUMMARY")
  (displayln "═══════════════════════════════════════════════════════════")
  (printf "  Files:     ~a total, ~a passed, ~a failed, ~a timeouts~n"
          total-files
          passed-files
          failed-files
          timeout-files)
  (when (> zero-test-files 0)
    (printf "             ⚠ ~a file~a with zero parsed tests (exit=0 but no rackunit output)~n"
            zero-test-files
            (if (= zero-test-files 1) "" "s")))
  (printf "  Tests:     ~a total, ~a passed, ~a failed~n" total-tests total-passed total-failed)
  (print-category-summary results)
  (when (and (= total-tests 0) (= passed-files total-files))
    (displayln "  ⚠ No test results parsed — files may use non-standard output format"))
  (printf "  Elapsed:   ~a~n" (format-duration total-start-ms))
  (displayln "═══════════════════════════════════════════════════════════")
  (displayln (format-verdict-line verdict timeout-files))
  (displayln "═══════════════════════════════════════════════════════════")
  (define failures (filter (lambda (r) (not (= (test-file-result-exit-code r) 0))) results))
  (when (pair? failures)
    (newline)
    (displayln "FAILURES:")
    (for ([f (in-list failures)])
      (define path (test-file-result-path f))
      (define ec (test-file-result-exit-code f))
      (printf "  ~a ~a [~a] (exit=~a, ~a passed, ~a failed, ~a)~n"
              (if (= ec 2) "⏱" "✗")
              path
              (classify-test-result f)
              ec
              (test-file-result-passed f)
              (test-file-result-failed f)
              (format-duration (test-file-result-elapsed-ms f)))
      (define flines (extract-failure-lines (test-file-result-stdout-bytes f)))
      (for ([line (in-list flines)])
        (printf "    ~a~n" line)))))

(define (make-unique-log-name test-path)
  (define base
    (let ([b (file-name-from-path test-path)])
      (if b
          (path->string b)
          "unknown")))
  (define safe-base (regexp-replace* #rx"[^a-zA-Z0-9._-]" base "_"))
  (define path-hash (number->string (equal-hash-code test-path) 16))
  (string-append "q-test-fail-" (string-replace safe-base ".rkt" "") "-" path-hash ".log"))

(define (save-failure-logs results)
  (define failures (filter (lambda (r) (not (= (test-file-result-exit-code r) 0))) results))
  (for ([f (in-list failures)])
    (define log-path (build-path "/tmp" (make-unique-log-name (test-file-result-path f))))
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
