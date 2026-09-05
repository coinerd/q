#lang racket/base

;; W7 characterization fixture: mutates the process environment WITHOUT
;; declaring @mutates. Characterization only: documents that undeclared env
;; mutation leaks across grouped files (the contract relies on @mutates
;; declarations; undeclared mutators are the named v1.00.27 boundary risk).
(require rackunit
         rackunit/text-ui)

(module+ test
  (putenv "GMD_W7_PROBE" "set")
  (run-tests (test-suite "mutates-env-undeclared"
               (test-case "trivial"
                 (check-true #t)))))
