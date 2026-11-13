#lang racket/base
;; @suite testing
;; @speed slow
;; @boundary integration
;; @timeout 900
(require rackunit
         racket/file
         racket/runtime-path
         "../scripts/run-tests/runner.rkt"
         "../scripts/run-tests/parse.rkt")

(define-runtime-path characterization "test-runner-grouped-characterization.rkt")

(module+ test
  (test-case "explicit exits are failures, never timeouts, and the host continues"
    (for ([code '(0 2 7)])
      (define p (make-temporary-file "grouped-truth-~a.rkt"))
      (dynamic-wind
       void
       (lambda ()
         (call-with-output-file p
                                #:exists 'truncate
                                (lambda (out)
                                  (fprintf out "#lang racket/base\n(module+ test (exit ~a))\n" code)))
         (define r (run-single-file p #:mode 'grouped-in-process #:timeout 10000))
         (check-equal? (test-file-result-exit-code r) (if (= code 7) 7 1))
         (check-equal? (classify-test-result r) 'UNKNOWN_FAILURE)
         (check-false (test-file-result-grouped-fallback-reason r))
         (check-equal? (execution-mode-of p) 'grouped-in-process)
         (check-regexp-match (regexp (format "exit ~a" code))
                             (bytes->string/utf-8 (test-file-result-stderr-bytes r)))
         (call-with-output-file
          p
          #:exists 'truncate
          (lambda (out)
            (display "#lang racket/base\n(module+ test (displayln \"HOST-CONTINUES\"))\n" out)))
         (define next (run-single-file p #:mode 'grouped-in-process #:timeout 10000))
         (check-equal? (test-file-result-exit-code next) 0)
         (check-false (test-file-result-grouped-fallback-reason next))
         (check-equal? (execution-mode-of p) (quote grouped-in-process))
         (check-true (regexp-match? #rx#"HOST-CONTINUES" (test-file-result-stdout-bytes next))))
       (lambda () (delete-file p)))))
  (test-case "silent grouped fixture executes exactly once"
    (define p (make-temporary-file "grouped-silent-~a.rkt"))
    (define counter (make-temporary-file "grouped-counter-~a"))
    (dynamic-wind
     void
     (lambda ()
       (call-with-output-file
        p
        #:exists 'truncate
        (lambda (out)
          (fprintf out
                   (string-append
                    "#lang racket/base\n(require rackunit)\n(module+ test "
                    "(call-with-output-file ~s #:exists 'append (lambda (out) (display \"x\" out))) "
                    "(check-true #t))\n")
                   (path->string counter))))
       (define r (run-single-file p #:mode 'grouped-in-process #:timeout 10000))
       (check-equal? (test-file-result-exit-code r) 0)
       (check-false (test-file-result-grouped-fallback-reason r))
       (check-equal? (test-file-result-total r) 0)
       (check-equal? (execution-mode-of p) 'grouped-in-process)
       (check-equal? (classify-test-result r) 'ZERO_PARSED)
       (check-equal? (file->string counter) "x"))
     (lambda ()
       (delete-file p)
       (delete-file counter))))
  (test-case "characterization really runs in grouped mode"
    (define r (run-single-file characterization #:mode 'grouped-in-process #:timeout 600000))
    (check-equal? (test-file-result-exit-code r) 0)
    (check-false (test-file-result-grouped-fallback-reason r))
    (check-equal? (execution-mode-of characterization) 'grouped-in-process)
    (check-true (positive? (test-file-result-total r)))
    (check-equal? (classify-test-result r) 'PASS)))

(module+ main
  (require rackunit/log
           (submod ".." test))
  (define counts (test-log))
  (unless (and (zero? (car counts)) (positive? (cdr counts)))
    (error 'grouped-truth "tests failed or did not run")))
