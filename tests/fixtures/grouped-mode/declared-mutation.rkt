#lang racket/base

;; @mutates env
;; W7 characterization fixture: declares a truthy @mutates token, so a
;; grouped request must fall back with reason declared-mutation.
(require rackunit
         rackunit/text-ui)

(module+ test
  (run-tests (test-suite "declared-mutation"
               (test-case "trivial"
                 (check-true #t)))))
