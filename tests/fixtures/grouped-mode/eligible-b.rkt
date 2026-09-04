#lang racket/base

;; W7 characterization fixture: grouped-ELIGIBLE, second file for the
;; sequential-in-one-worker and cwd-restore characterizations.
(require rackunit
         rackunit/text-ui)

(module+ test
  (run-tests (test-suite "eligible-b"
               (test-case "b-works"
                 (check-true (equal? (string-upcase "b") "B"))))))
