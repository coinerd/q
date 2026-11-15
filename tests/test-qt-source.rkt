#lang racket
;; @speed fast
;; @suite testing
;; @boundary integration
(require rackunit
         "../scripts/testing/quick-runner.rkt")

(module+ test
  (test-case "quick executes source, not substituted compiled test or dependency"
    (if (and (eq? (system-type 'os*) 'linux)
             (directory-exists? "/proc/self")
             (find-executable-path "setsid"))
        (let ([dir (make-temporary-file "qt-source-~a" 'directory)])
          (define (put name text)
            (call-with-output-file (build-path dir name)
                                   #:exists 'truncate
                                   (lambda (out) (display text out))))
          (dynamic-wind
           void
           (lambda ()
             (put "dep.rkt" "#lang racket/base\n(provide ok)\n(define ok #t)\n")
             (put "test.rkt" "#lang racket/base\n(require rackunit \"dep.rkt\")\n(check-true ok)\n")
             (check-true (system* (find-executable-path "raco") "make" (build-path dir "test.rkt")))
             (check-true (quick-result-valid?
                          (car (run-quick (list (path->string (build-path dir "test.rkt"))) 1))))
             ;; Compiled artifacts remain newer than the source and would report success.
             (put "test.rkt" "#lang racket/base\n(require rackunit)\n(check-true #f)\n")
             (file-or-directory-modify-seconds (build-path dir "test.rkt") 1)
             (check-false (quick-result-valid?
                           (car (run-quick (list (path->string (build-path dir "test.rkt"))) 1))))
             (put "test.rkt" "#lang racket/base\n(require rackunit \"dep.rkt\")\n(check-true ok)\n")
             (put "dep.rkt" "#lang racket/base\n(provide ok)\n(define ok #f)\n")
             (file-or-directory-modify-seconds (build-path dir "test.rkt") 1)
             (file-or-directory-modify-seconds (build-path dir "dep.rkt") 1)
             (check-false (quick-result-valid?
                           (car (run-quick (list (path->string (build-path dir "test.rkt"))) 1)))))
           (lambda () (delete-directory/files dir))))
        (displayln "SKIP source execution: Linux /proc or setsid unavailable"))))
