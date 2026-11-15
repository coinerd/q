#lang racket
;; @speed fast
;; @suite testing
;; @boundary integration
(require rackunit
         "../scripts/testing/quick-runner.rkt")
(define (quick-supported?)
  (and (eq? (system-type 'os*) 'linux)
       (directory-exists? "/proc/self")
       (find-executable-path "setsid")))
(module+ test
  (test-case "quick validates actual RackUnit results and ignores spoofed summaries"
    (if (quick-supported?)
        (let ([dir (make-temporary-file "qt-review-~a" 'directory)])
          (dynamic-wind
           void
           (lambda ()
             (define sources
               (list
                "#lang racket\n(require rackunit)\n(check-true #t)\n(module+ test (check-true #t))\n"
                "#lang racket\n(require rackunit)\n(displayln \"999 tests passed\")\n(check-true #f)\n"
                "#lang racket\n(displayln \"1 test passed\")\n"
                "#lang racket\n(this is malformed\n"))
             (define paths
               (for/list ([source sources]
                          [i (in-naturals)])
                 (define p (build-path dir (format "~a.rkt" i)))
                 (call-with-output-file p (lambda (out) (display source out)))
                 (path->string p)))
             (define results (run-quick paths 2))
             (define (result i)
               (findf (lambda (r) (equal? (quick-result-path r) (list-ref paths i))) results))
             (check-true (quick-result-valid? (result 0)))
             (check-equal? (quick-result-total (result 0)) 2)
             (for ([i '(1 2 3)])
               (check-false (quick-result-valid? (result i)))))
           (lambda () (delete-directory/files dir))))
        (displayln "SKIP RackUnit result validation: Linux /proc or setsid unavailable")))
  (test-case "output capture is bounded while both pipes are drained"
    (if (quick-supported?)
        (let ([p (make-temporary-file "qt-noisy-~a.rkt")])
          (dynamic-wind
           void
           (lambda ()
             (define (run size)
               (call-with-output-file
                p
                #:exists 'truncate
                (lambda (out)
                  (fprintf out
                           (string-append "#lang racket\n(require rackunit)\n"
                                          "(display (make-string ~a #\\x))\n"
                                          "(display (make-string ~a #\\y) (current-error-port))\n"
                                          "(check-true #t)\n")
                           size
                           size)))
               (car (run-quick (list (path->string p)) 1)))
             (define small (run 1024))
             (check-true (quick-result-valid? small))
             (check-equal? (quick-result-failed small) 0)
             (check-equal? (quick-result-total small) 1)
             (check-equal? (quick-result-stdout small) (make-string 1024 #\x))
             (check-equal? (quick-result-stderr small) (make-string 1024 #\y))
             (define large (run 5000000))
             (check-false (quick-result-valid? large))
             (check-equal? (quick-result-failed large) 0)
             (check-equal? (quick-result-total large) 1)
             (check-equal? (quick-result-stdout large) (make-string 4194304 #\x))
             (check-equal? (quick-result-stderr large) (make-string 4194304 #\y)))
           (lambda () (delete-file p))))
        (displayln "SKIP output capture: Linux /proc or setsid unavailable"))))
