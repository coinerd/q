#lang racket
;; @speed fast
;; @suite testing
;; @boundary integration
(require rackunit
         "../scripts/testing/quick-runner.rkt"
         (submod "../scripts/testing/quick-runner.rkt" test-support)
         "../sandbox/subprocess.rkt")

(define (run-fixture source #:timeout [timeout 30])
  (define path (make-temporary-file "qt-limits-~a.rkt"))
  (dynamic-wind void
                (lambda ()
                  (call-with-output-file path #:exists 'truncate (lambda (out) (display source out)))
                  (car (run-quick (list (path->string path)) 1 #:timeout timeout)))
                (lambda () (delete-file path))))

(module+ test
  (if (and (eq? (system-type 'os*) 'linux)
           (directory-exists? "/proc/self")
           (find-executable-path "setsid"))
      (begin
        (test-case "either truncated stream invalidates positive evidence"
          (for ([port '("(current-output-port)" "(current-error-port)")])
            (define result
              (run-fixture
               (format
                "#lang racket\n(require rackunit)\n(display (make-string 5000000 #\\x) ~a)\n(check-true #t)\n"
                port)
               #:timeout 120))
            (check-equal? (quick-result-failed result) 0)
            (check-equal? (quick-result-total result) 1)
            (check-false (quick-result-valid? result))
            (check-eq? (quick-result-reason result) 'truncated)
            (check-true (<= (string-length (quick-result-stdout result)) 4194304))
            (check-true (<= (string-length (quick-result-stderr result)) 4194304))))
        (test-case "timeout flag rejects otherwise successful evidence"
          (define observed #f)
          (define result
            (parameterize ([current-quick-result-transform
                            (lambda (r)
                              (set! observed r)
                              (struct-copy subprocess-result r [timed-out? #t]))])
              (run-fixture "#lang racket\n(require rackunit)\n(check-true #t)\n")))
          (check-equal? (subprocess-result-exit-code observed) 0)
          (check-false (subprocess-result-timed-out? observed))
          (check-false (subprocess-result-truncated? observed))
          (check-equal? (quick-result-failed result) 0)
          (check-equal? (quick-result-total result) 1)
          (check-false (quick-result-valid? result)))
        (test-case "timeout invalidates a running test"
          (define result
            (run-fixture "#lang racket\n(require rackunit)\n(check-true #t)\n(sync never-evt)\n"
                         #:timeout 2))
          (check-false (quick-result-valid? result))
          (check-not-equal? (quick-result-exit result) 0)
          (check-regexp-match #rx"Command timed out" (quick-result-stderr result))))
      (displayln "SKIP qt limits: Linux /proc or setsid unavailable")))
