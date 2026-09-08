#lang racket/base

;; W7 characterization fixture: mutates the process environment WITHOUT
;; declaring @mutates. Characterization only: documents that undeclared env
;; mutation leaks across grouped files (the contract relies on @mutates
;; declarations; undeclared mutators are the named boundary risk of
;; this series).
(require rackunit
         rackunit/text-ui)

(module+ test
  (putenv "GMD_W7_PROBE" "set")
  (run-tests (test-suite "mutates-env-undeclared"
               (test-case "trivial"
                 (check-true #t)))))
