#lang racket
;; @speed fast
;; @suite extensions
;; @covers scripts/gsd-delivery.py

(require rackunit
         racket/runtime-path
         racket/system)

(define-runtime-path python-tests "test-gsd-evidence-rejection-binding.py")
(define-runtime-path checkout-root "..")

(module+ test
  (test-case "evidence rejection binding: seven Python unit regressions"
    (define python (find-executable-path "python3"))
    (check-not-false python "Python 3 is required for evidence rejection regressions")
    ;; Inherit stdout/stderr so Python failures and tracebacks remain visible.
    (parameterize ([current-directory checkout-root])
      (check-true (system* python (path->string python-tests))))))
