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

(module+ test
  (test-case "interrupted grouped run shuts down its test-owned thread"
    (define dir (make-temporary-file "grouped-break-~a" 'directory))
    (define p (build-path dir "test.rkt"))
    (define ready (build-path dir "ready"))
    (define marker (build-path dir "marker"))
    (dynamic-wind
     (lambda ()
       (display-to-file
        (format
         "#lang racket\n(module+ test (thread (lambda () (sleep 2) (display-to-file \"leaked\" ~s))) (display-to-file \"ready\" ~s) (sleep 20))\n"
         (path->string marker)
         (path->string ready))
        p
        #:exists 'truncate))
     (lambda ()
       (define caller
         (thread (lambda ()
                   (with-handlers ([exn:break? void])
                     (run-single-file p #:mode 'grouped-in-process)))))
       (let loop ([n 100])
         (unless (or (file-exists? ready) (zero? n))
           (sleep 0.05)
           (loop (sub1 n))))
       (check-true (file-exists? ready))
       (break-thread caller)
       (check-not-false (sync/timeout 5 caller))
       (sleep 2.2)
       (check-false (file-exists? marker)))
     (lambda () (delete-directory/files dir))))
  (test-case "copying the skipped subtype preserves classification"
    (define r (make-skipped-result "skip" 'ci '("network")))
    (define copied (struct-copy profile-skipped-result r [exit-code #:parent test-file-result 5]))
    (check-equal? (classify-test-result copied) 'SKIPPED_BY_PROFILE))
  (test-case "real RackUnit failure and error layouts override a passing summary"
    (for ([kind '("FAILURE" "ERROR")])
      (define output
        (format (string-append "--------------------\ncase > suite\n~a\n"
                               "name:       check-equal?\nlocation:   test.rkt:17:20\n"
                               "actual:     1\nexpected:   7\n"
                               "--------------------\n"
                               "1 success(es) 0 failure(s) 0 error(s) 1 test(s) run\n")
                kind))
      (define-values (_ failed __) (parse-raco-output (string->bytes/utf-8 output)))
      (check-true (positive? failed)))))
