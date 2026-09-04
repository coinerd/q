#lang racket/base

;; @isolation process
;; W7 characterization fixture: declares process isolation, so a grouped
;; request must fall back with reason declared-process-isolation.
(require rackunit
         rackunit/text-ui)

(module+ test
  (run-tests (test-suite "process-isolation"
               (test-case "trivial"
                 (check-true #t)))))
