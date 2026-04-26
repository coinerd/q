#lang racket

;; tests/test-gsd-wave-executor.rkt — Wave execution engine tests
;;
;; Wave 2b: Status lifecycle and error recovery.

(require rackunit
         "../extensions/gsd/wave-executor.rkt"
         "../extensions/gsd/plan-types.rkt")

;; ============================================================
;; Helper
;; ============================================================

(define (make-test-plan n)
  (gsd-plan
   (for/list ([i (in-range n)])
     (gsd-wave i (format "Wave ~a" i) 'pending "" '("file.rkt") '() "test" '()))
   "" '() '()))

;; ============================================================
;; Lifecycle: pending → in-progress → completed
;; ============================================================

(test-case "status lifecycle: pending → in-progress → completed"
  (define exec (make-wave-executor (make-test-plan 3)))
  (check-equal? (next-pending-wave exec) 0)
  (wave-start! exec 0)
  (define s0 (car (wave-executor-statuses exec)))
  (check-eq? (wave-status-state s0) 'in-progress)
  (check-equal? (wave-status-attempt-count s0) 1)
  (wave-complete! exec 0)
  (define s0-done (car (wave-executor-statuses exec)))
  (check-eq? (wave-status-state s0-done) 'completed))

;; ============================================================
;; Failed wave recovery
;; ============================================================

(test-case "failed wave: pending → in-progress → failed"
  (define exec (make-wave-executor (make-test-plan 3)))
  (wave-start! exec 0)
  (wave-fail! exec 0 "compile error in foo.rkt")
  (define s0 (car (wave-executor-statuses exec)))
  (check-eq? (wave-status-state s0) 'failed)
  (check-equal? (wave-status-error-message s0) "compile error in foo.rkt"))

(test-case "failed wave does not block next wave"
  (define exec (make-wave-executor (make-test-plan 3)))
  (wave-start! exec 0)
  (wave-fail! exec 0 "error")
  ;; Next pending wave should be 1
  (check-equal? (next-pending-wave exec) 1)
  ;; And we can execute it
  (wave-start! exec 1)
  (wave-complete! exec 1)
  (define statuses (wave-executor-statuses exec))
  (check-eq? (wave-status-state (list-ref statuses 0)) 'failed)
  (check-eq? (wave-status-state (list-ref statuses 1)) 'completed))

;; ============================================================
;; Skip wave
;; ============================================================

(test-case "skip wave: marks as skipped"
  (define exec (make-wave-executor (make-test-plan 3)))
  (wave-skip! exec 1)
  (define statuses (wave-executor-statuses exec))
  (check-eq? (wave-status-state (list-ref statuses 1)) 'skipped)
  ;; Next pending should skip over wave 1
  (check-equal? (next-pending-wave exec) 0))

(test-case "skip advances to next pending wave"
  (define exec (make-wave-executor (make-test-plan 3)))
  (wave-start! exec 0)
  (wave-complete! exec 0)
  (wave-skip! exec 1)
  (check-equal? (next-pending-wave exec) 2))

;; ============================================================
;; All done check
;; ============================================================

(test-case "all-waves-done? returns #f when pending remain"
  (define exec (make-wave-executor (make-test-plan 3)))
  (wave-complete! exec 0)
  (check-false (all-waves-done? exec)))

(test-case "all-waves-done? returns #t when all completed"
  (define exec (make-wave-executor (make-test-plan 2)))
  (wave-complete! exec 0)
  (wave-complete! exec 1)
  (check-true (all-waves-done? exec)))

(test-case "all-waves-done? returns #t when mix of completed/failed/skipped"
  (define exec (make-wave-executor (make-test-plan 3)))
  (wave-complete! exec 0)
  (wave-fail! exec 1 "error")
  (wave-skip! exec 2)
  (check-true (all-waves-done? exec)))

;; ============================================================
;; Summary
;; ============================================================

(test-case "wave-summary shows correct counts"
  (define exec (make-wave-executor (make-test-plan 3)))
  (wave-complete! exec 0)
  (wave-fail! exec 1 "error")
  (wave-skip! exec 2)
  (define summary (wave-summary exec))
  (check-true (string-contains? summary "Total: 3"))
  (check-true (string-contains? summary "Completed: 1"))
  (check-true (string-contains? summary "Failed: 1"))
  (check-true (string-contains? summary "Skipped: 1")))

(test-case "wave-summary with all pending"
  (define exec (make-wave-executor (make-test-plan 2)))
  (define summary (wave-summary exec))
  (check-true (string-contains? summary "Total: 2"))
  (check-true (string-contains? summary "Pending: 2")))

;; ============================================================
;; next-pending-wave
;; ============================================================

(test-case "next-pending-wave returns #f when no pending"
  (define exec (make-wave-executor (make-test-plan 2)))
  (wave-complete! exec 0)
  (wave-complete! exec 1)
  (check-false (next-pending-wave exec)))

(test-case "attempt count increments on wave-start!"
  (define exec (make-wave-executor (make-test-plan 1)))
  (wave-start! exec 0)
  (wave-start! exec 0) ;; restart
  (define s0 (car (wave-executor-statuses exec)))
  (check-equal? (wave-status-attempt-count s0) 2))
