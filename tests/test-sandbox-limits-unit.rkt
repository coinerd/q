#lang racket/base

;; tests/test-sandbox-limits-unit.rkt — S-1 tests for sandbox/limits.rkt

(require rackunit
         rackunit/text-ui
         "../sandbox/limits.rkt")

(define limits-suite
  (test-suite
   "Sandbox limits unit tests"

   (test-case "default-exec-limits has reasonable values"
     (define lim (default-exec-limits))
     (check-pred exec-limits? lim)
     (check-true (> (exec-limits-timeout-seconds lim) 0))
     (check-true (> (exec-limits-max-output-bytes lim) 0)))

   (test-case "within-limits? passes for default limits"
     (define lim (default-exec-limits))
     (check-true (within-limits? lim #:elapsed 1.0 #:output-size 100 #:memory 100000 #:processes 1)))

   (test-case "within-limits? fails when timeout exceeded"
     (define lim (strict-exec-limits))
     (check-false (within-limits? lim #:elapsed 999999.0)))

   (test-case "merge-limits takes minimum of both"
     (define strict (strict-exec-limits))
     (define permissive (permissive-exec-limits))
     (define merged (merge-limits strict permissive))
     (check-pred exec-limits? merged)
     ;; Merged timeout should be the minimum
     (check-true (<= (exec-limits-timeout-seconds merged)
                     (exec-limits-timeout-seconds strict))))
   ))

(run-tests limits-suite)
