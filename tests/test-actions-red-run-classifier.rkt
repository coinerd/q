#lang racket

;; @suite ci
;; @speed fast
;; tests/test-actions-red-run-classifier.rkt
;;
;; W7 (#8547): Actions red-run classifier tests.
;; Fixture-based tests covering all 7 verdict types:
;;   current_blocking_red_release_run  (#581 shape)
;;   current_blocking_red_main_ci
;;   historical_superseded_red
;;   cancelled_superseded
;;   in_progress_blocking
;;   success_current
;;   unknown_due_api_error

(require rackunit
         racket/list)

(define script-path "../scripts/actions-red-run-classifier.rkt")

;; --- Import classifier functions ---

(define classify-red-run (dynamic-require script-path 'classify-red-run))

(define classify-red-run-set (dynamic-require script-path 'classify-red-run-set))

(define blocking-verdict? (dynamic-require script-path 'blocking-verdict?))

(define RED_RUN_VERDICTS (dynamic-require script-path 'RED_RUN_VERDICTS))

(define all-blocked-verdicts (dynamic-require script-path 'all-blocked-verdicts))

;; --- Fixtures ---

(define fixture-581-release-fail
  ;; #581 shape: release.yml latest run is failure, assets published
  (hasheq 'id
          12345678901
          'run_number
          581
          'status
          "completed"
          'conclusion
          "failure"
          'head_branch
          "v0.99.40"))

(define fixture-release-success
  (hasheq 'id
          12345678902
          'run_number
          582
          'status
          "completed"
          'conclusion
          "success"
          'head_branch
          "v0.99.41"))

(define fixture-ci-fail-latest
  (hasheq 'id
          12345678903
          'run_number
          100
          'status
          "completed"
          'conclusion
          "failure"
          'head_branch
          "main"))

(define fixture-ci-success
  (hasheq 'id
          12345678904
          'run_number
          101
          'status
          "completed"
          'conclusion
          "success"
          'head_branch
          "main"))

(define fixture-ci-in-progress
  (hasheq 'id 12345678905 'run_number 102 'status "in_progress" 'conclusion #f 'head_branch "main"))

(define fixture-release-cancelled
  (hasheq 'id
          12345678906
          'run_number
          580
          'status
          "completed"
          'conclusion
          "cancelled"
          'head_branch
          "v0.99.40"))

(define fixture-old-release-fail
  (hasheq 'id
          12345678907
          'run_number
          550
          'status
          "completed"
          'conclusion
          "failure"
          'head_branch
          "v0.99.39"))

;; ===================================================================
;; classify-red-run — single run tests
;; ===================================================================

(test-case "#581 shape → current_blocking_red_release_run"
  (define verdict (classify-red-run fixture-581-release-fail "release" #t #f))
  (check-equal? verdict "current_blocking_red_release_run"))

(test-case "latest CI failure → current_blocking_red_main_ci"
  (define verdict (classify-red-run fixture-ci-fail-latest "ci-main" #t #f))
  (check-equal? verdict "current_blocking_red_main_ci"))

(test-case "historical release failure (not latest) → historical_superseded_red"
  (define verdict (classify-red-run fixture-old-release-fail "release" #f #f))
  (check-equal? verdict "historical_superseded_red"))

(test-case "cancelled run → cancelled_superseded"
  (define verdict (classify-red-run fixture-release-cancelled "release" #t #f))
  (check-equal? verdict "cancelled_superseded"))

(test-case "in-progress run → in_progress_blocking"
  (define verdict (classify-red-run fixture-ci-in-progress "ci-main" #t #f))
  (check-equal? verdict "in_progress_blocking"))

(test-case "success run → success_current"
  (define verdict (classify-red-run fixture-release-success "release" #t #f))
  (check-equal? verdict "success_current"))

(test-case "API error → unknown_due_api_error"
  (define verdict (classify-red-run fixture-ci-success "ci-main" #t #t))
  (check-equal? verdict "unknown_due_api_error"))

(test-case "no run data → unknown_due_api_error"
  (define verdict (classify-red-run #f "release" #t #f))
  (check-equal? verdict "unknown_due_api_error"))

;; ===================================================================
;; classify-red-run-set — set of runs tests
;; ===================================================================

(test-case "release set with latest failure → current_blocking_red_release_run"
  (define result (classify-red-run-set (list fixture-581-release-fail) "release" #f))
  (check-equal? (hash-ref result 'verdict) "current_blocking_red_release_run")
  (check-true (hash-ref result 'blocking)))

(test-case "release set with success latest → success_current"
  (define result
    (classify-red-run-set (list fixture-release-success fixture-581-release-fail) "release" #f))
  (check-equal? (hash-ref result 'verdict) "success_current")
  (check-false (hash-ref result 'blocking)))

(test-case "CI set with failure latest → current_blocking_red_main_ci"
  (define result (classify-red-run-set (list fixture-ci-fail-latest) "ci-main" #f))
  (check-equal? (hash-ref result 'verdict) "current_blocking_red_main_ci")
  (check-true (hash-ref result 'blocking)))

(test-case "CI set with in-progress latest → in_progress_blocking"
  (define result (classify-red-run-set (list fixture-ci-in-progress) "ci-main" #f))
  (check-equal? (hash-ref result 'verdict) "in_progress_blocking")
  (check-true (hash-ref result 'blocking)))

(test-case "CI set with success latest → success_current"
  (define result (classify-red-run-set (list fixture-ci-success) "ci-main" #f))
  (check-equal? (hash-ref result 'verdict) "success_current")
  (check-false (hash-ref result 'blocking)))

(test-case "API error → unknown_due_api_error (blocks)"
  (define result (classify-red-run-set (list fixture-ci-success) "ci-main" #t))
  (check-equal? (hash-ref result 'verdict) "unknown_due_api_error")
  (check-true (hash-ref result 'blocking)))

(test-case "empty run set → unknown_due_api_error (blocks)"
  (define result (classify-red-run-set '() "release" #f))
  (check-equal? (hash-ref result 'verdict) "unknown_due_api_error")
  (check-true (hash-ref result 'blocking)))

(test-case "cancelled latest → cancelled_superseded (blocks)"
  (define result (classify-red-run-set (list fixture-release-cancelled) "release" #f))
  (check-equal? (hash-ref result 'verdict) "cancelled_superseded")
  (check-true (hash-ref result 'blocking)))

;; ===================================================================
;; blocking-verdict? — blocking semantics tests
;; ===================================================================

(test-case "current_blocking_red_release_run blocks"
  (check-true (blocking-verdict? "current_blocking_red_release_run")))

(test-case "current_blocking_red_main_ci blocks"
  (check-true (blocking-verdict? "current_blocking_red_main_ci")))

(test-case "in_progress_blocking blocks"
  (check-true (blocking-verdict? "in_progress_blocking")))

(test-case "unknown_due_api_error blocks"
  (check-true (blocking-verdict? "unknown_due_api_error")))

(test-case "historical_superseded_red does NOT block"
  (check-false (blocking-verdict? "historical_superseded_red")))

(test-case "success_current does NOT block"
  (check-false (blocking-verdict? "success_current")))

(test-case "cancelled_superseded does NOT block (latest cancelled is superseded)"
  (check-false (blocking-verdict? "cancelled_superseded")))

;; ===================================================================
;; RED_RUN_VERDICTS — completeness tests
;; ===================================================================

(test-case "RED_RUN_VERDICTS has exactly 7 verdicts"
  (check-equal? (length RED_RUN_VERDICTS) 7))

(test-case "RED_RUN_VERDICTS contains all expected verdicts"
  (for ([v '("current_blocking_red_release_run" "current_blocking_red_main_ci"
                                                "historical_superseded_red"
                                                "cancelled_superseded"
                                                "in_progress_blocking"
                                                "success_current"
                                                "unknown_due_api_error")])
    (check-not-false (member v RED_RUN_VERDICTS) (format "Missing verdict: ~a" v))))
