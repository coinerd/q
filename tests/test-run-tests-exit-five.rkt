#lang racket/base
;; @speed fast
;; @suite testing
;; @isolation process
;; @boundary integration

(require rackunit
         racket/file
         json
         "../scripts/run-tests/runner.rkt"
         "../scripts/run-tests/reporting.rkt")

(module+ test
  (test-case "real exit 5 fails while metadata skip remains a skip"
    (define dir (make-temporary-file "exit-five-~a" 'directory))
    (define child (build-path dir "child.rkt"))
    (define skipped (build-path dir "skipped.rkt"))
    (define json-path (build-path dir "results.json"))
    (dynamic-wind
     (lambda ()
       (display-to-file "#lang racket/base\n(displayln \"SKIPPED_BY_PROFILE\")\n(exit 5)\n"
                        child
                        #:exists 'truncate)
       (display-to-file "#lang racket/base\n;; @requires browser\n(error 'skip \"executed\")\n"
                        skipped
                        #:exists 'truncate))
     (lambda ()
       (define-values (code results)
         (run-suite-once (list child skipped)
                         1
                         10000
                         #f
                         "explicit"
                         1
                         1
                         'subprocess
                         json-path
                         #f
                         'vps))
       (check-not-equal? code 0)
       (check-equal? (map test-file-result-exit-code results) '(5 5))
       (check-equal? (map classify-test-result results) '(UNKNOWN_FAILURE SKIPPED_BY_PROFILE))
       (define js (call-with-input-file json-path read-json))
       (define summary (hash-ref js 'summary))
       (check-equal? (hash-ref summary 'files_failed) 1)
       (check-equal? (hash-ref summary 'files_skipped_by_profile) 1)
       (define files (hash-ref js 'files))
       (check-equal? (map (lambda (f) (hash-ref f 'category)) files)
                     '("UNKNOWN_FAILURE" "SKIPPED_BY_PROFILE"))
       (check-equal? (map (lambda (f) (hash-ref f 'exit_code)) files) '(5 5))
       (check-equal? (hash-ref (car files) 'execution_mode) "subprocess"))
     (lambda () (delete-directory/files dir)))))
