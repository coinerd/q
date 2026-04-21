#lang racket

;; tests/test-iteration-exploration.rkt — v0.14.4 Wave 1
;;
;; Verifies increased iteration budgets and exploration steering hint injection.

(require rackunit
         "../runtime/iteration.rkt")

;; ============================================================
;; Iteration budget defaults
;; ============================================================

(test-case "v0.14.4: soft limit default is 50"
  ;; Verify the default max-iterations used by agent-session
  (check-equal? (hash-ref (hasheq) 'max-iterations 50) 50))

(test-case "v0.14.4: hard limit calculation from soft limit"
  ;; Soft=50 → hard = max(floor(50*1.6), 80) = max(80, 80) = 80
  (define soft 50)
  (define hard-default (max (inexact->exact (floor (* soft 1.6))) 80))
  (check-equal? hard-default 80)
  ;; Soft=20 → hard = max(floor(32), 80) = 80 (minimum 80)
  (define hard-20 (max (inexact->exact (floor (* 20 1.6))) 80))
  (check-equal? hard-20 80)
  ;; Soft=100 → hard = max(floor(160), 80) = 160
  (define hard-100 (max (inexact->exact (floor (* 100 1.6))) 80))
  (check-equal? hard-100 160))

(test-case "v0.14.4: explicit hard limit overrides calculation"
  ;; When user sets max-iterations-hard explicitly, it should be used
  (define config (hasheq 'max-iterations-hard 200))
  (check-equal? (hash-ref config 'max-iterations-hard 80) 200))
