#lang racket

;; tests/test-gsd-steering.rkt — Steering module tests
;;
;; Wave 1c of v0.21.0: Mode-aware steering.

(require rackunit
         "../extensions/gsd/steering.rkt"
         "../extensions/gsd/state-machine.rkt")

;; ============================================================
;; Steering: returns #f in non-executing modes
;; ============================================================

(test-case "steering returns #f in idle"
  (reset-gsm!)
  (reset-steering-state!)
  (check-false (gsd-steering-check "read" (hasheq 'path "foo.rkt") "")))

(test-case "steering returns #f in exploring (DD-2)"
  (reset-gsm!)
  (reset-steering-state!)
  (gsm-transition! 'exploring)
  ;; Even with many reads, no steering
  (for ([_ (in-range 5)])
    (check-false (gsd-steering-check "read" (hasheq 'path "foo.rkt") ""))))

(test-case "steering returns #f in plan-written"
  (reset-gsm!)
  (reset-steering-state!)
  (gsm-transition! 'exploring)
  (gsm-transition! 'plan-written)
  (check-false (gsd-steering-check "read" (hasheq 'path "foo.rkt") "")))

(test-case "steering returns #f in verifying"
  (reset-gsm!)
  (reset-steering-state!)
  (gsm-transition! 'exploring)
  (gsm-transition! 'plan-written)
  (gsm-transition! 'executing)
  (gsm-transition! 'verifying)
  (check-false (gsd-steering-check "read" (hasheq 'path "foo.rkt") "")))

;; ============================================================
;; Steering: activates during executing after stall
;; ============================================================

(test-case "steering: no stall before threshold"
  (reset-gsm!)
  (reset-steering-state!)
  (gsm-transition! 'exploring)
  (gsm-transition! 'plan-written)
  (gsm-transition! 'executing)
  ;; Below threshold
  (for ([_ (in-range (sub1 stall-threshold))])
    (check-false (gsd-steering-check "read" (hasheq 'path "foo.rkt") ""))))

(test-case "steering: detects stall at threshold"
  (reset-gsm!)
  (reset-steering-state!)
  (gsm-transition! 'exploring)
  (gsm-transition! 'plan-written)
  (gsm-transition! 'executing)
  ;; Build up to threshold
  (for ([_ (in-range (sub1 stall-threshold))])
    (gsd-steering-check "read" (hasheq 'path "foo.rkt") ""))
  ;; This one triggers stall
  (define result (gsd-steering-check "read" (hasheq 'path "foo.rkt") ""))
  (check-not-false result)
  (check-true (string-contains? result "Stall detected"))
  (check-true (string-contains? result "foo.rkt")))

(test-case "steering: resets on non-read tool call"
  (reset-gsm!)
  (reset-steering-state!)
  (gsm-transition! 'exploring)
  (gsm-transition! 'plan-written)
  (gsm-transition! 'executing)
  ;; Build up reads
  (for ([_ (in-range stall-threshold)])
    (gsd-steering-check "read" (hasheq 'path "foo.rkt") ""))
  ;; Non-read resets stall
  (gsd-steering-check "edit" (hasheq 'file "foo.rkt") "")
  ;; Now reads shouldn't trigger stall yet
  (for ([_ (in-range (sub1 stall-threshold))])
    (check-false (gsd-steering-check "read" (hasheq 'path "foo.rkt") ""))))

(test-case "steering: different files don't cause stall"
  (reset-gsm!)
  (reset-steering-state!)
  (gsm-transition! 'exploring)
  (gsm-transition! 'plan-written)
  (gsm-transition! 'executing)
  ;; Alternate between different files
  (for ([_ (in-range (* 2 stall-threshold))])
    (gsd-steering-check "read" (hasheq 'path "foo.rkt") "")
    (gsd-steering-check "read" (hasheq 'path "bar.rkt") ""))
  ;; Shouldn't stall because files alternate
  (check-false (gsd-steering-check "read" (hasheq 'path "foo.rkt") "")))
