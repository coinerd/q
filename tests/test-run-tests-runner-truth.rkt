#lang racket
;; @speed fast
;; @suite testing
;; @boundary unit
;; @covers scripts/run-tests/runner.rkt

(require rackunit
         racket/file
         "../scripts/run-tests/runner.rkt"
         "../scripts/run-tests/parse.rkt"
         "../scripts/run-tests/profiles.rkt")

(define summary "2 success(es) 0 failure(s) 0 error(s) 2 test(s) run\n")
(define (failed-in text)
  (define-values (_ f __) (parse-raco-output (string->bytes/utf-8 text)))
  f)

(module+ test
  (test-case "mixed output retains structured failures"
    (check-true (positive? (failed-in (string-append summary "#<test-failure>\n"))))
    (check-true (positive? (failed-in (string-append summary "1 2 tests/bad.rkt\n")))))
  (test-case "plain markers are not RackUnit diagnostics"
    (check-equal? (failed-in (string-append "FAILURE\nERROR\n" summary)) 0))
  (test-case "failure text cannot create a profile skip"
    (define r (make-test-file-result "bad" 1 #"SKIPPED_BY_PROFILE" #"" 0 0 1 1))
    (check-not-equal? (classify-test-result r) 'SKIPPED_BY_PROFILE)
    (check-equal? (classify-test-result (make-skipped-result "skip" 'ci '("network")))
                  'SKIPPED_BY_PROFILE))
  (test-case "batch worker exceptions retain a failing result"
    (define results (run-all-files (list 123) 1 1000))
    (check-equal? (length results) 1)
    (when (pair? results)
      (check-not-equal? (test-file-result-exit-code (car results)) 0)))
  (test-case "grouped worker without an exit is not successful"
    (define p (make-temporary-file "runner-truth-~a.rkt"))
    (dynamic-wind
     (lambda ()
       (display-to-file
        "#lang racket\n(module+ test (displayln \"1 test passed\") (kill-thread (current-thread)))\n"
        p
        #:exists 'truncate))
     (lambda ()
       (check-not-equal?
        (test-file-result-exit-code (run-single-file p #:mode 'grouped-in-process #:timeout 5000))
        0))
     (lambda () (delete-file p)))))
