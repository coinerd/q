#lang racket
;; @suite testing
;; @speed fast

;; Contract test for the quick child's entry-point convention
;; (scripts/testing/quick-child.rkt). A `main` submodule is the repository's
;; script convention, so it is executed — not rejected. This file previously
;; pinned the opposite behaviour.
(require rackunit
         racket/file
         "../scripts/testing/quick-runner.rkt")

(define (quick-supported?)
  (and (eq? (system-type 'os*) 'linux)
       (directory-exists? "/proc/self")
       (find-executable-path "setsid")))

(define (write-fixture! dir name source)
  (define path (build-path dir name))
  (call-with-output-file path #:exists 'truncate (lambda (out) (display source out)))
  (path->string path))

(define (main-submodule-case dir)
  ;; A main-only file runs and reports its own counts.
  (define ok-path
    (write-fixture! dir
                    "ok.rkt"
                    "#lang racket\n(require rackunit)\n(module+ main (check-true #t))\n"))
  (define ok (car (run-quick (list ok-path) 1)))
  (check-true (quick-result-valid? ok) (quick-result-stderr ok))
  (check-equal? (quick-result-total ok) 1)
  (check-equal? (quick-result-failed ok) 0)
  ;; A failing assertion inside main still fails the file: main is executed as
  ;; an entry point, not ignored.
  (define bad-path
    (write-fixture! dir
                    "bad.rkt"
                    (string-append "#lang racket\n(require rackunit)\n(check-true #t)\n"
                                   "(module+ main (check-true #f))\n")))
  (define bad (car (run-quick (list bad-path) 1)))
  (check-false (quick-result-valid? bad))
  (check-equal? (quick-result-failed bad) 1)
  (check-equal? (quick-result-total bad) 2))

(module+ test
  (if (quick-supported?)
      (let ([dir (make-temporary-file "qt-main-~a" 'directory)])
        (dynamic-wind void
                      (lambda () (main-submodule-case dir))
                      (lambda () (delete-directory/files dir))))
      (displayln "SKIP main-submodule execution: Linux /proc or setsid unavailable")))
