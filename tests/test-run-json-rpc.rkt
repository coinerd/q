#lang racket

;; tests/test-run-json-rpc.rkt — Tests for wiring/run-json-rpc.rkt
;;
;; Tests the run-json and run-rpc functions. Since these depend on
;; complex runtime infrastructure, we test the exported symbols exist
;; and exercise what we can without full runtime mocking.

(require rackunit
         "../wiring/run-json-rpc.rkt")

;; ============================================================
;; Module exports
;; ============================================================

(test-case "run-json is exported as a procedure"
  (check-true (procedure? run-json)))

(test-case "run-rpc is exported as a procedure"
  (check-true (procedure? run-rpc)))
