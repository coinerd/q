#lang racket

(require rackunit
         "../sandbox/limits.rkt")

;; ============================================================
;; Preset constructors
;; ============================================================

(test-case "default-exec-limits returns reasonable values"
  (define lim (default-exec-limits))
  (check-true (exec-limits? lim))
  (check-equal? (exec-limits-timeout-seconds lim) 120)
  (check-equal? (exec-limits-max-output-bytes lim) 1048576))

(test-case "strict-exec-limits is tighter than default"
  (define strict (strict-exec-limits))
  (define default (default-exec-limits))
  (check-true (< (exec-limits-timeout-seconds strict)
                 (exec-limits-timeout-seconds default)))
  (check-true (< (exec-limits-max-output-bytes strict)
                 (exec-limits-max-output-bytes default))))

(test-case "permissive-exec-limits is looser than default"
  (define perm (permissive-exec-limits))
  (define default (default-exec-limits))
  (check-true (> (exec-limits-timeout-seconds perm)
                 (exec-limits-timeout-seconds default))))

;; ============================================================
;; merge-limits
;; ============================================================

(test-case "merge-limits takes minimum of each field"
  (define base (exec-limits 100 1000 10000 5))
  (define override (exec-limits 50 2000 5000 10))
  (define merged (merge-limits base override))
  (check-equal? (exec-limits-timeout-seconds merged) 50)
  (check-equal? (exec-limits-max-output-bytes merged) 1000)
  (check-equal? (exec-limits-max-memory-bytes merged) 5000)
  (check-equal? (exec-limits-max-processes merged) 5))

(test-case "merge-limits with identical limits is idempotent"
  (define lim (exec-limits 60 500 5000 3))
  (define merged (merge-limits lim lim))
  (check-equal? merged lim))

;; ============================================================
;; within-limits?
;; ============================================================

(test-case "within-limits? returns true with no checks specified"
  (define lim (default-exec-limits))
  (check-true (within-limits? lim)))

(test-case "within-limits? checks elapsed time"
  (define lim (exec-limits 10 1000 10000 5))
  (check-true (within-limits? lim #:elapsed 5))
  (check-true (within-limits? lim #:elapsed 10))
  (check-false (within-limits? lim #:elapsed 11)))

(test-case "within-limits? checks output size"
  (define lim (exec-limits 10 1000 10000 5))
  (check-true (within-limits? lim #:output-size 500))
  (check-false (within-limits? lim #:output-size 1001)))

(test-case "within-limits? checks memory"
  (define lim (exec-limits 10 1000 10000 5))
  (check-true (within-limits? lim #:memory 5000))
  (check-false (within-limits? lim #:memory 10001)))

(test-case "within-limits? all checks combined"
  (define lim (exec-limits 10 1000 10000 5))
  (check-true (within-limits? lim #:elapsed 5 #:output-size 500 #:memory 5000))
  (check-false (within-limits? lim #:elapsed 5 #:output-size 500 #:memory 20000)))

;; ============================================================
;; SEC-12: max-processes enforcement
;; ============================================================

(test-case "within-limits? checks process count"
  (define lim (exec-limits 10 1000 10000 5))
  (check-true (within-limits? lim #:processes 3))
  (check-true (within-limits? lim #:processes 5))
  (check-false (within-limits? lim #:processes 6)))

(test-case "track-process! increments counter (#452)"
  ;; Save/restore box state instead of parameterize
  (define saved (unbox process-count-box))
  (set-box! process-count-box 0)
  (track-process!)
  (check-equal? (get-process-count) 1)
  (track-process!)
  (check-equal? (get-process-count) 2)
  (set-box! process-count-box saved))

(test-case "untrack-process! decrements counter (#452)"
  (define saved (unbox process-count-box))
  (set-box! process-count-box 2)
  (untrack-process!)
  (check-equal? (get-process-count) 1)
  (set-box! process-count-box saved))

(test-case "untrack-process! never goes below zero (#452)"
  (define saved (unbox process-count-box))
  (set-box! process-count-box 0)
  (untrack-process!)
  (check-equal? (get-process-count) 0)
  (set-box! process-count-box saved))

(test-case "process count over limit fails within-limits?"
  (define lim (exec-limits 10 1000 10000 3))
  (check-false (within-limits? lim #:processes 4)))

;; ============================================================
;; Standalone defaults
;; ============================================================

(test-case "default-timeout-seconds is 300"
  (check-equal? default-timeout-seconds 300))

(test-case "default-max-output-bytes is 10MB"
  (check-equal? default-max-output-bytes 10485760))

;; ============================================================
;; with-resource-limits
;; ============================================================

(test-case "with-resource-limits runs thunk and returns result"
  (define-values (result timed-out?)
    (with-resource-limits (λ (cust) 42) #:timeout 5))
  (check-equal? result 42)
  (check-false timed-out?))

(test-case "with-resource-limits returns timed-out on timeout"
  (define-values (result timed-out?)
    (with-resource-limits
     (λ (cust) (sleep 10) 'done)
     #:timeout 0.1))
  (check-false result)
  (check-true timed-out?))
