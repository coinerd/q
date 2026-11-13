#lang racket
;; @speed fast
;; @suite testing
;; @boundary integration
(require rackunit
         racket/runtime-path)

(define-runtime-path root "..")
(define-runtime-path source "test-gsd-delivery-verifier-decision.rkt")
(define-runtime-path runner "../scripts/run-tests.rkt")
(define-runtime-path child "../scripts/testing/quick-child.rkt")

(define (invoke . args)
  (define-values (proc out in err) (apply subprocess #f #f #f (find-executable-path "racket") args))
  (close-output-port in)
  (define output (port->string out))
  (define errors (port->string err))
  (subprocess-wait proc)
  (values (subprocess-status proc) (string-append output errors)))

(module+ test
  (test-case "direct, runner, and quick child fail on a decision assertion"
    (define temp (make-temporary-file "decision-failure~a" 'directory))
    (dynamic-wind
     void
     (lambda ()
       (define tests (build-path temp "tests"))
       (define gsd (build-path temp "extensions" "gsd"))
       (make-directory* tests)
       (make-directory* gsd)
       (for ([name '("verify-command-format.rkt" "verify-command-grammar.rkt")])
         (copy-file (build-path root "extensions" "gsd" name) (build-path gsd name)))
       (define altered
         (string-replace (file->string source)
                         "racket a.rkt && racket b.rkt\""
                         "intentionally incorrect\""))
       (check-not-equal? altered (file->string source))
       (define fixture (build-path tests "decision.rkt"))
       (call-with-output-file fixture #:exists 'truncate (lambda (out) (display altered out)))
       (for ([args (in-list (list (list fixture)
                                  (list runner fixture)
                                  (list child fixture (build-path temp "evidence"))))])
         (define-values (status text) (apply invoke args))
         (check-not-equal? status 0 text)
         (check-true
          (or (regexp-match? #rx"9 success\\(es\\) 1 failure\\(s\\) 0 error\\(s\\) 10 test\\(s\\) run"
                             text)
              (regexp-match? #rx"10 total, 9 passed, 1 failed" text))
          text)))
     (lambda () (delete-directory/files temp)))))
