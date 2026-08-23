#lang racket/base

;; @speed fast  ;; @suite default
;; @boundary integration
;; q/tests/test-goal-evaluations-persistence.rkt — W1 v0.99.78 (G-8)
;;
;; Contract: evaluator decisions are persisted to the session log as
;; structured `goal.evaluation` entries. After a goal turn with a known
;; evaluator response, load-goal-evaluations returns a list containing that
;; evaluation-result with exact achieved?/ok, reason, turn, token-cost.
;; An auditor can reconstruct every evaluator decision from the session log
;; without the live process.

(require rackunit
         racket/file
         racket/string
         (only-in "../runtime/session/session-store.rkt"
                  write-session-version-header!
                  append-goal-state!)
         (only-in "../runtime/session/session-store-goal-task.rkt"
                  append-evaluation-result!
                  load-goal-evaluations)
         (only-in "../runtime/goal/goal-state.rkt"
                  make-evaluation-result
                  evaluation-result?
                  evaluation-result-achieved?
                  evaluation-result-reason
                  evaluation-result-model-used
                  evaluation-result-token-cost))

;; ------------------------------------------------------------
;; Test 1: append-evaluation-result! round-trips via load-goal-evaluations
;; ------------------------------------------------------------

(define test-dir (make-temporary-file "goal-eval-persist-~a" 'directory))

(test-case "evaluations persistence: round-trip exact fields"
  (define log-path (build-path test-dir "session.jsonl"))
  ;; Write session version header so the log is well-formed
  (write-session-version-header! log-path)
  (define ev
    (make-evaluation-result #:achieved? #t
                            #:reason "all checks passed"
                            #:model-used "mock-eval"
                            #:token-cost 1234
                            #:check-results '()))
  (append-evaluation-result! log-path ev 3)
  (define loaded (load-goal-evaluations log-path))
  (check-true (list? loaded))
  (check-equal? (length loaded) 1)
  (define entry (car loaded)) ; (cons turn evaluation-result)
  (check-true (pair? entry))
  (check-equal? (car entry) 3 "turn number persists")
  (define restored (cdr entry))
  (check-true (evaluation-result? restored))
  (check-true (evaluation-result-achieved? restored) "achieved? round-trips exactly")
  (check-equal? (evaluation-result-reason restored) "all checks passed")
  (check-equal? (evaluation-result-model-used restored) "mock-eval")
  (check-equal? (evaluation-result-token-cost restored) 1234)
  (check-equal? (evaluation-result-reason restored) (evaluation-result-reason ev)))

;; ------------------------------------------------------------
;; Test 2: multiple evaluations append in order; not-achieved round-trips
;; ------------------------------------------------------------

(test-case "evaluations persistence: ordered trail, not-achieved"
  (define log-path (build-path test-dir "multi.jsonl"))
  (write-session-version-header! log-path)
  (define ev1
    (make-evaluation-result #:achieved? #f
                            #:reason "test failed"
                            #:model-used "mock-eval"
                            #:token-cost 100))
  (define ev2 (make-evaluation-result #:achieved? #t #:reason "green"))
  (append-evaluation-result! log-path ev1 1)
  (append-evaluation-result! log-path ev2 2)
  (define loaded (load-goal-evaluations log-path))
  (check-equal? (length loaded) 2 "two evaluations persist")
  (check-false (evaluation-result-achieved? (cdr (car loaded))))
  (check-true (evaluation-result-achieved? (cdr (cadr loaded))))
  (check-equal? (evaluation-result-reason (cdr (car loaded))) "test failed")
  (check-equal? (car (cadr loaded)) 2 "second turn number persists"))

;; ------------------------------------------------------------
;; Test 3: load-goal-evaluations returns '() when no evaluations
;; ------------------------------------------------------------

(test-case "evaluations persistence: empty log yields empty list"
  (define log-path (build-path test-dir "empty.jsonl"))
  (write-session-version-header! log-path)
  (check-equal? (load-goal-evaluations log-path) '()))

;; ------------------------------------------------------------
;; Cleanup
;; ------------------------------------------------------------

(delete-directory/files test-dir)
