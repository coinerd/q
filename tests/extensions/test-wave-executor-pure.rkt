#lang racket/base

;; BOUNDARY: pure

;; tests/extensions/test-wave-executor-pure.rkt — Tests for pure wave-executor functions
;; Tests compute-next-wave-statuses pure function (v0.31.5 W0)

(require rackunit
         rackunit/text-ui
         racket/list
         "../../extensions/gsd/wave-executor.rkt")

;; ============================================================
;; Test data helpers
;; ============================================================

(define (make-test-status idx state)
  (wave-status idx state #f 0 (current-seconds)))

;; ============================================================
;; Tests for compute-next-wave-statuses
;; ============================================================

(define test-compute-next-wave-statuses
  (test-suite "compute-next-wave-statuses pure function"

    ;; Test 1: Basic status update
    (test-case "Updates matching status and leaves others unchanged"
      (define s1 (make-test-status 1 'pending))
      (define s2 (make-test-status 2 'pending))
      (define s3 (make-test-status 3 'pending))
      (define statuses (list s1 s2 s3))
      (define new-statuses
        (compute-next-wave-statuses statuses
                                    2
                                    (lambda (s) (wave-status 2 'in-progress #f 1 (current-seconds)))))
      ;; Check that we got a new list
      (check-false (eq? statuses new-statuses))
      ;; Check that s2 was updated
      (check-equal? (wave-status-state (second new-statuses)) 'in-progress)
      ;; Check that s1 and s3 are unchanged
      (check-equal? (wave-status-state (first new-statuses)) 'pending)
      (check-equal? (wave-status-state (third new-statuses)) 'pending))

    ;; Test 2: No match returns same structure with possible new list
    (test-case "Returns unchanged list when no status matches index"
      (define s1 (make-test-status 1 'pending))
      (define statuses (list s1))
      (define new-statuses
        (compute-next-wave-statuses statuses
                                    999
                                    (lambda (s) (wave-status 1 'in-progress #f 1 (current-seconds)))))
      ;; Should have same number of elements
      (check-equal? (length new-statuses) 1)
      (check-equal? (wave-status-state (first new-statuses)) 'pending))

    ;; Test 3: Multiple statuses
    (test-case "Works correctly with multiple statuses"
      (define statuses
        (list (make-test-status 1 'completed)
              (make-test-status 2 'pending)
              (make-test-status 3 'failed)))
      (define new-statuses
        (compute-next-wave-statuses
         statuses
         2
         (lambda (s)
           (wave-status 2 'in-progress #f (add1 (wave-status-attempt-count s)) (current-seconds)))))
      (check-equal? (wave-status-state (first new-statuses)) 'completed)
      (check-equal? (wave-status-state (second new-statuses)) 'in-progress)
      (check-equal? (wave-status-attempt-count (second new-statuses)) 1)
      (check-equal? (wave-status-state (third new-statuses)) 'failed))

    ;; Test 4: Pure function does not mutate input
    (test-case "Does not mutate the input list"
      (define s1 (make-test-status 1 'pending))
      (define statuses (list s1))
      (compute-next-wave-statuses statuses
                                  1
                                  (lambda (s) (wave-status 1 'completed #f 0 (current-seconds))))
      ;; Original should be unchanged
      (check-equal? (wave-status-state s1) 'pending))))

;; ============================================================
;; Run tests
;; ============================================================

(run-tests test-compute-next-wave-statuses)
