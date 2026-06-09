#lang racket/base

;; q/scripts/run-tests/reporting.rkt — Summary output and failure logs
;;
;; Print-summary, save-failure-logs, format-duration, summary-exit-code.
;; Extracted from run-tests.rkt (v0.96.16, AX1-2).
;; STABILITY: internal

(require racket/string
         racket/path
         racket/list
         (only-in "parse.rkt"
                  test-file-result-path
                  test-file-result-exit-code
                  test-file-result-stdout-bytes
                  test-file-result-stderr-bytes
                  test-file-result-elapsed-ms
                  test-file-result-passed
                  test-file-result-failed
                  test-file-result-total
                  bytes->string*
                  extract-failure-lines))

(provide print-summary
         save-failure-logs
         format-duration
         summary-exit-code
         make-unique-log-name)

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
  (when (and (= total-tests 0) (= passed-files total-files))
    (displayln "  ⚠ No test results parsed — files may use non-standard output format"))
  (printf "  Elapsed:   ~a~n" (format-duration total-start-ms))
  (displayln "═══════════════════════════════════════════════════════════")
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
