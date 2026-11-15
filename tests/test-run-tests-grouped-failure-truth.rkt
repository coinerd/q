#lang racket/base
;; @suite testing
;; @speed slow
;; @boundary integration
;; @timeout 2100
(require rackunit
         racket/file
         racket/path
         racket/runtime-path
         racket/system
         racket/string
         "../scripts/run-tests/runner.rkt"
         "../scripts/run-tests/parse.rkt")

(define-runtime-path source "test-runner-grouped-characterization.rkt")

(module+ test
  (test-case "real characterization failure is visible standalone and grouped"
    (define p (make-temporary-file "grouped-failure-~a.rkt" #f (path-only source)))
    (dynamic-wind
     void
     (lambda ()
       (define original (file->string source))
       (define needle "(check-equal? (test-file-result-total grp) 2 \"eligible-a parses 2 tests\")")
       (check-true (string-contains? original needle))
       (call-with-output-file
        p
        #:exists 'truncate
        (lambda (out)
          (display
           (string-replace original
                           needle
                           "(check-equal? (test-file-result-total grp) 99 \"injected failure\")")
           out)))
       (for ([command (in-list (list (list (find-executable-path "raco") "test" p)
                                     (list (find-executable-path "racket") p)))])
         (define output (open-output-string))
         (define-values (_ in _pid _err control)
           (apply values (apply process*/ports output #f output command)))
         (close-output-port in)
         (control 'wait)
         (check-not-equal? (control 'exit-code) 0)
         (check-regexp-match #rx"injected failure" (get-output-string output)))
       (define result (run-single-file p #:mode 'grouped-in-process #:timeout 600000))
       (check-not-equal? (test-file-result-exit-code result) 0)
       (check-false (test-file-result-grouped-fallback-reason result))
       (check-equal? (execution-mode-of p) 'grouped-in-process)
       (check-regexp-match #rx"injected failure"
                           (bytes->string/utf-8 (test-file-result-stderr-bytes result)))
       (check-equal? (test-file-result-failed result) 1)
       (check-equal? (test-file-result-total result) 1))
     (lambda () (delete-file p)))))

(module+ main
  (require rackunit/log
           (submod ".." test))
  (define counts (test-log))
  (unless (and (zero? (car counts)) (positive? (cdr counts)))
    (error 'grouped-failure-truth "tests failed or did not run")))
