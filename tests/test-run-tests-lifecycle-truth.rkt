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
  (test-case "grouped exit retains nonzero status"
    (define p (make-temporary-file "grouped-exit-~a.rkt"))
    (dynamic-wind (lambda ()
                    (display-to-file "#lang racket\n(module+ test (exit 7))\n" p #:exists 'truncate))
                  (lambda ()
                    (check-equal? (test-file-result-exit-code
                                   (run-single-file p #:mode 'grouped-in-process #:timeout 5000))
                                  7))
                  (lambda () (delete-file p))))
  (test-case "grouped success shuts down test-owned threads"
    (define dir (make-temporary-file "grouped-cleanup-~a" 'directory))
    (define p (build-path dir "test.rkt"))
    (define marker (build-path dir "marker"))
    (dynamic-wind
     (lambda ()
       (display-to-file
        (format
         "#lang racket\n(module+ test (thread (lambda () (sleep 1) (display-to-file \"leaked\" ~s))) (displayln \"1 test passed\"))\n"
         (path->string marker))
        p
        #:exists 'truncate))
     (lambda ()
       (check-equal?
        (test-file-result-exit-code (run-single-file p #:mode 'grouped-in-process #:timeout 5000))
        0)
       (sleep 1.3)
       (check-false (file-exists? marker)))
     (lambda () (delete-directory/files dir)))))
