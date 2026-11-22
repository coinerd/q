#lang racket

;; @speed fast  ;; @suite default
;; @boundary unit

;; tests/test-cl5-ci-evidence.rkt — CL5 W2 (#9982) exact-SHA CI ground truth.
;;
;; Pure classification of CI run/job payloads against the frozen evidence
;; rules; no network calls.
;;   1. a completed all-success run is 'pass
;;   2. a completed run with a failed job is 'fail and names the detecting job/step
;;   3. a non-completed, cancelled, timed-out or action-required run is 'unavailable
;;   4. a skipped REQUIRED producer (test/security/platform/smoke/workflows) is 'unavailable
;;   5. a skipped non-required job (e.g. gsd-governance) does not invalidate the run
;;   6. the exact-SHA lookup ignores other SHAs and other workflows
;;   7. scratch branch names are unique per case/label/nonce

(require rackunit
         racket/list
         (file "../scripts/cl5/ci-evidence.rkt"))

(define (job name conclusion [failed-steps '()])
  (hash 'name
        name
        'conclusion
        conclusion
        'steps
        (map (lambda (step) (hash 'name step 'conclusion "failure")) failed-steps)))

(define (run #:status [status "completed"] #:conclusion [conclusion "success"] [sha "abc"])
  (hash 'id 42 'name "CI" 'status status 'conclusion conclusion 'head_sha sha))

(module+ test
  (test-case "a completed all-success run passes"
    (define result
      (classify-ci-run (run)
                       (list (job "test (0)" "success")
                             (job "test (1)" "success")
                             (job "test (2)" "success")
                             (job "security" "success")
                             (job "gsd-governance" "skipped"))))
    (check-equal? (hash-ref result 'verdict) 'pass)
    (check-equal? (hash-ref result 'detecting) '()))

  (test-case "a failed job fails the run and names the detecting job and step"
    (define result
      (classify-ci-run (run #:conclusion "failure")
                       (list (job "test (0)" "success")
                             (job "test (1)" "failure" (list "Run test shard 1/3")))))
    (check-equal? (hash-ref result 'verdict) 'fail)
    (check-equal? (map (lambda (j) (hash-ref j 'name)) (hash-ref result 'detecting)) '("test (1)"))
    (check-equal? (hash-ref (first (hash-ref result 'detecting)) 'failed-steps)
                  '("Run test shard 1/3")))

  (test-case "incomplete and cancelled runs are unavailable"
    (for ([r (in-list (list (run #:status "in_progress" #:conclusion #f)
                            (run #:conclusion "cancelled")
                            (run #:conclusion "timed_out")
                            (run #:conclusion "action_required")))])
      (check-equal? (hash-ref (classify-ci-run r '()) 'verdict) 'unavailable)))

  (test-case "a skipped required producer invalidates the run"
    (define result
      (classify-ci-run
       (run)
       (list (job "test (0)" "skipped") (job "test (1)" "success") (job "test (2)" "success"))))
    (check-equal? (hash-ref result 'verdict) 'unavailable)
    (check-true (for/or ([reason (in-list (hash-ref result 'unavailable-reasons))])
                  (string-contains? reason "test (0)"))))

  (test-case "a skipped non-required job does not invalidate the run"
    (define result
      (classify-ci-run (run)
                       (list (job "test (0)" "success")
                             (job "test (1)" "success")
                             (job "test (2)" "success")
                             (job "release-readiness" "skipped"))))
    (check-equal? (hash-ref result 'verdict) 'pass))

  (test-case "exact-SHA lookup ignores other SHAs and other workflows"
    (define runs
      (list (hash 'id 1 'name "CI" 'head_sha "old" 'status "completed" 'conclusion "success")
            (hash 'id 2 'name "CI" 'head_sha "abc" 'status "completed" 'conclusion "success")
            (hash 'id 3 'name "Nightly" 'head_sha "abc" 'status "completed" 'conclusion "success")
            (hash 'id 4 'name "CI" 'head_sha "abc" 'status "completed" 'conclusion "failure")))
    (check-equal? (hash-ref (run-for-sha runs "abc") 'id) 2)
    (check-false (run-for-sha runs "missing")))

  (test-case "scratch branch names identify case, label and nonce"
    (check-equal? (scratch-branch-name 'D5 'defect "n1") "cl5-w2/d5-defect-n1")
    (check-equal? (scratch-branch-name 'F-PE-1 'control "n2") "cl5-w2/f-pe-1-control-n2")
    (check-not-equal? (scratch-branch-name 'D5 'defect "n1")
                      (scratch-branch-name 'D5 'control "n1"))))
