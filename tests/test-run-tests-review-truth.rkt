#lang racket
;; @speed fast
;; @suite testing
;; @boundary unit
;; @covers scripts/run-tests/runner.rkt
(require rackunit
         racket/file
         "../scripts/run-tests/runner.rkt"
         "../scripts/run-tests/parse.rkt")

(module+ test
  (test-case "grouped exit after success is not completion"
    (define p (make-temporary-file "exit-early-~a.rkt"))
    (dynamic-wind
     (lambda ()
       (display-to-file "#lang racket\n(module+ test (displayln \"1 test passed\") (exit 0))\n"
                        p
                        #:exists 'truncate))
     (lambda ()
       (check-not-equal?
        (test-file-result-exit-code (run-single-file p #:mode 'grouped-in-process #:timeout 5000))
        0))
     (lambda () (delete-file p))))
  (test-case "RackUnit diagnostic with intervening metadata"
    (define-values (_ f __)
      (parse-raco-output
       (bytes-append #"--------------------\ncase\nFAILURE\ncheck-info: value\nname: check-equal?\n"
                     #"--------------------\n1 success(es) 0 failure(s) 0 error(s) 1 test(s) run\n")))
    (check-true (positive? f))))
