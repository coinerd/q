#lang racket

;; @speed fast  ;; @suite default
;; @boundary unit

;; tests/test-cl5-ci-evidence.rkt — CL5 W2 (#9982) exact-SHA CI ground truth.
;;
;; Pure classification of CI run/job payloads against the frozen evidence
;; rules; no network calls.
;;   1. a completed run with ALL 14 required producers green passes
;;   2. a failed producer fails the run and names the detecting job/step
;;   3. a lint-only failure is still a fail, recorded separately
;;   4. non-completed, cancelled, timed-out or action-required runs are unavailable
;;   5. a skipped required producer is unavailable
;;   6. a REQUIRED producer ABSENT from the job list is unavailable (fail closed)
;;   7. a duplicate job name is unavailable (payload cannot be a complete graph)
;;   8. a truncated job list is unavailable, never a pass
;;   9. the four dispatch-expected skips are allowed only when the jobs ARE present
;;  10. the exact-SHA lookup ignores other SHAs and other workflows
;;  11. scratch branch names are unique per case/label/nonce
;;  12. case classification separates the selected and comprehensive axes

(require rackunit
         json
         racket/file
         racket/list
         racket/runtime-path
         (file "../scripts/cl5/ci-evidence.rkt"))

(define-runtime-path f1-fixture "fixtures/cl5-ci-f1-payloads.json")

;; Real recorded job payloads (steps only): the shape that produced the W3.1
;; F1 finding, pinned so a future refactor cannot silently reclassify it.
(define (f1-payload key)
  (define payloads (with-input-from-file f1-fixture (lambda () (read-json))))
  ;; read-json turns JSON object keys into symbols
  (hash-ref payloads (string->symbol key)))

(define (job name conclusion [failed-steps '()])
  (hash 'name
        name
        'status
        (if (eq? conclusion 'in_progress) "in_progress" "completed")
        'conclusion
        conclusion
        'steps
        (map (lambda (step) (hash 'name step 'conclusion "failure")) failed-steps)))

;; Every expected job (required + dispatch-expected-optional) in a green shape.
(define (green-jobs)
  (append (map (lambda (n) (job n "success")) required-producers)
          (map (lambda (n) (job n "skipped")) expected-optional-jobs)))

(define (run #:status [status "completed"] #:conclusion [conclusion "success"] [sha "abc"])
  (hash 'id 42 'name "CI" 'status status 'conclusion conclusion 'head_sha sha))

(module+ test
  (test-case "a completed run with every required producer green passes"
    (define result (classify-ci-run (run) (green-jobs)))
    (check-equal? (hash-ref result 'verdict) 'pass)
    (check-equal? (hash-ref result 'detecting) '())
    (check-equal? (hash-ref result 'missing-jobs) '()))

  (test-case "a failed producer fails the run and names the detecting job and step"
    (define jobs
      (for/list ([j (in-list (green-jobs))])
        (if (equal? (hash-ref j 'name) "test (1)")
            (job "test (1)" "failure" (list "Run test shard 1/3"))
            j)))
    (define result (classify-ci-run (run #:conclusion "failure") jobs))
    (check-equal? (hash-ref result 'verdict) 'fail)
    (check-equal? (map (lambda (j) (hash-ref j 'name)) (hash-ref result 'detecting)) '("test (1)"))
    (check-equal? (hash-ref (first (hash-ref result 'detecting)) 'failed-steps)
                  '("Run test shard 1/3")))

  (test-case "a lint-only failure is a fail, recorded with its job and step"
    (define jobs
      (for/list ([j (in-list (green-jobs))])
        (if (equal? (hash-ref j 'name) "lint-quality")
            (job "lint-quality" "failure" (list "Run all lint checks"))
            j)))
    (define result (classify-ci-run (run #:conclusion "failure") jobs))
    (check-equal? (hash-ref result 'verdict) 'fail)
    (check-equal? (map (lambda (j) (hash-ref j 'name)) (hash-ref result 'detecting))
                  '("lint-quality"))
    (check-equal? (hash-ref result 'unavailable-reasons) '()))

  (test-case "incomplete and cancelled runs are unavailable"
    (for ([r (in-list (list (run #:status "in_progress" #:conclusion #f)
                            (run #:conclusion "cancelled")
                            (run #:conclusion "timed_out")
                            (run #:conclusion "action_required")))])
      (check-equal? (hash-ref (classify-ci-run r (green-jobs)) 'verdict) 'unavailable)))

  (test-case "a skipped required producer invalidates the run"
    (define jobs
      (for/list ([j (in-list (green-jobs))])
        (if (equal? (hash-ref j 'name) "test (0)")
            (job "test (0)" "skipped")
            j)))
    (define result (classify-ci-run (run) jobs))
    (check-equal? (hash-ref result 'verdict) 'unavailable)
    (check-true (for/or ([reason (in-list (hash-ref result 'unavailable-reasons))])
                  (string-contains? reason "test (0)"))))

  (test-case "a required producer ABSENT from the job list is unavailable (fail closed)"
    (define evicted
      (for/list ([j (in-list (green-jobs))]
                 #:unless (member (hash-ref j 'name) (list "security" "test-aggregate")))
        j))
    (define result (classify-ci-run (run) evicted))
    (check-equal? (hash-ref result 'verdict) 'unavailable)
    (check-equal? (sort (hash-ref result 'missing-jobs) string<?)
                  (sort (list "security" "test-aggregate") string<?)))

  (test-case "a duplicate job name is unavailable"
    (define result (classify-ci-run (run) (append (green-jobs) (list (job "test (0)" "success")))))
    (check-equal? (hash-ref result 'verdict) 'unavailable)
    (check-equal? (hash-ref result 'duplicate-jobs) (list "test (0)")))

  (test-case "a truncated job list is unavailable, never a pass"
    ;; The reviewer's counter-example: four green jobs out of an 18-job graph.
    (define truncated
      (list (job "test (0)" "success")
            (job "test (1)" "success")
            (job "test (2)" "success")
            (job "security" "success")))
    (define result (classify-ci-run (run) truncated))
    (check-equal? (hash-ref result 'verdict) 'unavailable)
    (check-not-equal? (hash-ref result 'verdict) 'pass))

  (test-case "dispatch-expected skips are allowed only when the jobs are present"
    (check-equal? (hash-ref (classify-ci-run (run) (green-jobs)) 'verdict) 'pass)
    (define absent
      (for/list ([j (in-list (green-jobs))]
                 #:unless (equal? (hash-ref j 'name) "fast-env"))
        j))
    (check-equal? (hash-ref (classify-ci-run (run) absent) 'verdict) 'unavailable))

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
    (check-not-equal? (scratch-branch-name 'D5 'defect "n1") (scratch-branch-name 'D5 'control "n1")))

  (test-case "case classification keeps the selected and comprehensive axes apart"
    ;; both surfaces detect, both controls clean -> detected
    (check-equal? (classify-case-evidence 'fail 'pass 'fail 'pass) 'detected)
    ;; selector misses what comprehensive catches -> missed, NOT detected
    (check-equal? (classify-case-evidence 'pass 'pass 'fail 'pass) 'missed)
    ;; comprehensive green while selected red: the surfaces disagree -> unavailable
    (check-equal? (classify-case-evidence 'fail 'pass 'pass 'pass) 'unavailable)
    ;; neither fails: the injection is not a valid case
    (check-equal? (classify-case-evidence 'pass 'pass 'pass 'pass) 'not-detected)
    ;; any unavailable state or dirty control invalidates the comparison
    (check-equal? (classify-case-evidence 'unavailable 'pass 'fail 'pass) 'unavailable)
    (check-equal? (classify-case-evidence 'fail 'fail 'fail 'pass) 'unavailable)
    (check-equal? (classify-case-evidence 'fail 'pass 'fail 'fail) 'unavailable)))

(test-case "F1: a setup-action failure is infrastructure, not a detection"
  ;; Recorded payload: security job 138065565620 of run 45280459705. The
  ;; setup-racket action timed out; the lane step never ran.
  (define verdict (f1-payload "security-setup-timeout-45280459705"))
  (define jobs
    (for/list ([j (in-list (green-jobs))])
      (if (equal? (hash-ref j 'name) "security") verdict j)))
  (define result (classify-ci-run (run #:conclusion "failure") jobs))
  (check-eq? (hash-ref result 'verdict) 'unavailable)
  (check-equal? (hash-ref result 'detecting) '())
  (check-equal? (map (lambda (j) (hash-ref j 'name)) (hash-ref result 'infrastructure-failures))
                (list "security"))
  (check-true (for/or ([reason (in-list (hash-ref result 'unavailable-reasons))])
                (string-contains? reason "tests did not execute"))))

(test-case "F1: a real test step failure is still a named detection"
  ;; Recorded payload: shard test (0) of run 45043919793 (D5 defect).
  (define verdict (f1-payload "shard-assertion-failure-45043919793"))
  (define jobs
    (for/list ([j (in-list (green-jobs))])
      (if (equal? (hash-ref j 'name) "test (0)") verdict j)))
  (define result (classify-ci-run (run #:conclusion "failure") jobs))
  (check-eq? (hash-ref result 'verdict) 'fail)
  (check-equal? (map (lambda (j) (hash-ref j 'name)) (hash-ref result 'detecting)) (list "test (0)"))
  (check-equal? (hash-ref (first (hash-ref result 'detecting)) 'failed-steps)
                (list "Run test shard 0/3"))
  (check-equal? (hash-ref result 'infrastructure-failures) '()))

(test-case "F1: only setup/action step names are infrastructure"
  (for ([name (in-list (list "Set up job"
                             "Run actions/checkout@v7"
                             "Cache Racket packages"
                             "Run ./.github/actions/setup-racket"
                             "Restore verified installer archive"
                             "Install Racket"))])
    (check-true (infrastructure-step? name) name))
  (for ([name (in-list (list "Run security test suite (parallel)"
                             "Run test shard 0/3"
                             "Run fast test shard 1/3"
                             "Run all lint checks"
                             "Clear stale bytecode"
                             "Test results summary"))])
    (check-false (infrastructure-step? name) name)))

(test-case "F1: a failed job with no step evidence is unattributable, not a detection"
  (define jobs
    (for/list ([j (in-list (green-jobs))])
      (if (equal? (hash-ref j 'name) "test (2)")
          (hash 'name "test (2)" 'status "completed" 'conclusion "failure" 'steps '())
          j)))
  (define result (classify-ci-run (run #:conclusion "failure") jobs))
  (check-eq? (hash-ref result 'verdict) 'unavailable)
  (check-equal? (hash-ref result 'detecting) '())
  (check-true (for/or ([reason (in-list (hash-ref result 'unavailable-reasons))])
                (string-contains? reason "no step evidence"))))

(test-case "F1: infrastructure + behavioural failures together are unavailable evidence"
  (define setup-failed (f1-payload "security-setup-timeout-45280459705"))
  (define shard-failed (f1-payload "shard-assertion-failure-45043919793"))
  (define jobs
    (for/list ([j (in-list (green-jobs))])
      (cond
        [(equal? (hash-ref j 'name) "security") setup-failed]
        [(equal? (hash-ref j 'name) "test (0)") shard-failed]
        [else j])))
  (define result (classify-ci-run (run #:conclusion "failure") jobs))
  (check-eq? (hash-ref result 'verdict) 'unavailable)
  ;; the behavioural failure is still recorded, never promoted past the missing producer
  (check-equal? (map (lambda (j) (hash-ref j 'name)) (hash-ref result 'detecting)) (list "test (0)")))

(test-case "F1: an optional job's infrastructure failure stays red but names no detector"
  ;; Optional producers do not make the comprehensive surface unavailable, but
  ;; their infrastructure failure must still be recorded, never left a bare fail.
  (define jobs
    (for/list ([j (in-list (green-jobs))])
      (if (equal? (hash-ref j 'name) "fast-env")
          (hash 'name
                "fast-env"
                'status
                "completed"
                'conclusion
                "failure"
                'steps
                (list (hash 'name "Run actions/checkout@v7" 'conclusion "failure")
                      (hash 'name "Run the suite" 'conclusion "skipped")))
          j)))
  (define result (classify-ci-run (run #:conclusion "failure") jobs))
  (check-eq? (hash-ref result 'verdict) 'fail)
  (check-equal? (hash-ref result 'detecting) '())
  (check-equal? (map (lambda (j) (hash-ref j 'name)) (hash-ref result 'infrastructure-failures))
                (list "fast-env")))
