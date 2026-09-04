#lang racket/base

;; W7 characterization fixture: grouped-ELIGIBLE (module+ test, run-tests
;; self-report). Never collected by production suites; used only by
;; tests/test-runner-grouped-characterization.rkt.
(require rackunit
         rackunit/text-ui)

(module+ test
  (run-tests (test-suite "eligible-a"
               (test-case "a-adds"
                 (check-equal? (+ 1 1) 2))
               (test-case "a-true"
                 (check-true #t)))))
