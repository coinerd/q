#lang racket/base

;; tests/test-iteration-pure.rkt — Property tests for pure iteration functions
;;
;; v0.33.1 W0: Test pure transition functions extracted from iteration loop.
;; These tests verify that compute-step-result produces correct outputs
;; for given inputs, without any I/O.

(require rackunit
         rackunit/text-ui
         (only-in "../runtime/iteration.rkt"
                  compute-step-result
                  step-result
                  step-result?
                  step-result-action
                  step-result-termination
                  step-result-new-counters
                  step-result-metadata
                  step-action?
                  iteration-ctx
                  iteration-ctx-iteration
                  iteration-ctx-consecutive-tool-count
                  iteration-ctx-explore-count
                  iteration-ctx-max-iterations
                  iteration-ctx-max-iterations-hard)
         "../runtime/iteration/loop-state.rkt"
         (only-in "../util/loop-result.rkt"
                  make-loop-result
                  loop-result-termination-reason
                  loop-result-messages
                  loop-result-metadata))

;; ============================================================
;; Helpers
;; ============================================================

(define (make-test-ctx #:iteration [iter 0]
                       #:consecutive-tool-count [tc 0]
                       #:explore-count [ec 0]
                       #:max-iterations [mi 10]
                       #:max-iterations-hard [mih 16])
  (iteration-ctx iter tc ec mi mih))

(define (make-test-result #:termination [term 'completed]
                          #:messages [msgs '()]
                          #:metadata [md (hasheq)])
  (make-loop-result msgs term md))

(define (make-test-counters #:iteration [iter 0]
                            #:consecutive-tool-count [tc 0]
                            #:seen-paths [sp '()]
                            #:intent-retry-count [irc 0]
                            #:consecutive-error-count [cec 0]
                            #:recent-tool-names [rtn '()]
                            #:explore-count [ec 0]
                            #:implement-count [ic 0]
                            #:stall-retry-count [src 0])
  (loop-counters iter tc sp irc cec rtn ec ic src))

;; ============================================================
;; compute-step-result tests
;; ============================================================

(define compute-step-result-tests
  (test-suite "compute-step-result"

    (test-case "completed → stop with completed termination"
      (define ctx (make-test-ctx))
      (define result (make-test-result #:termination 'completed))
      (define counters (make-test-counters))
      (define sr (compute-step-result ctx result counters))
      (check-pred step-result? sr)
      (check-equal? (step-result-action sr) 'stop)
      (check-equal? (step-result-termination sr) 'completed))

    (test-case "cancelled → stop with cancelled termination"
      (define ctx (make-test-ctx))
      (define result (make-test-result #:termination 'cancelled))
      (define counters (make-test-counters))
      (define sr (compute-step-result ctx result counters))
      (check-equal? (step-result-action sr) 'stop)
      (check-equal? (step-result-termination sr) 'cancelled))

    (test-case "tool-calls-pending, iter 0, max 10 → continue"
      (define ctx (make-test-ctx #:iteration 0 #:max-iterations 10))
      (define result (make-test-result #:termination 'tool-calls-pending))
      (define counters (make-test-counters #:iteration 0))
      (define sr (compute-step-result ctx result counters))
      (check-equal? (step-result-action sr) 'continue)
      (check-equal? (step-result-termination sr) 'tool-calls-pending))

    (test-case "tool-calls-pending, iter at hard limit → stop-hard-limit"
      (define ctx (make-test-ctx #:iteration 16 #:max-iterations-hard 16))
      (define result (make-test-result #:termination 'tool-calls-pending))
      (define counters (make-test-counters #:iteration 16))
      (define sr (compute-step-result ctx result counters))
      (check-equal? (step-result-action sr) 'stop-hard-limit)
      (check-equal? (step-result-termination sr) 'tool-calls-pending))

    (test-case "tool-calls-pending, iter at soft limit → stop-soft-limit"
      (define ctx (make-test-ctx #:iteration 10 #:max-iterations 10 #:max-iterations-hard 16))
      (define result (make-test-result #:termination 'tool-calls-pending))
      (define counters (make-test-counters #:iteration 10))
      (define sr (compute-step-result ctx result counters))
      (check-equal? (step-result-action sr) 'stop-soft-limit)
      (check-equal? (step-result-termination sr) 'tool-calls-pending))

    (test-case "error → stop with error termination"
      (define ctx (make-test-ctx))
      (define result (make-test-result #:termination 'error))
      (define counters (make-test-counters))
      (define sr (compute-step-result ctx result counters))
      (check-equal? (step-result-action sr) 'stop)
      (check-equal? (step-result-termination sr) 'error))

    (test-case "stop-hard-limit metadata contains maxIterationsReached"
      (define ctx (make-test-ctx #:iteration 16 #:max-iterations-hard 16))
      (define result (make-test-result #:termination 'tool-calls-pending))
      (define counters (make-test-counters #:iteration 16))
      (define sr (compute-step-result ctx result counters))
      (define md (step-result-metadata sr))
      (check-true (hash-ref md 'maxIterationsReached #f)
                  "metadata should contain maxIterationsReached #t"))

    (test-case "new-counters preserves explore/implement counts"
      (define ctx (make-test-ctx #:iteration 0))
      (define result (make-test-result #:termination 'tool-calls-pending))
      (define counters (make-test-counters #:iteration 0 #:explore-count 3 #:implement-count 2))
      (define sr (compute-step-result ctx result counters))
      (define new-c (step-result-new-counters sr))
      ;; For continue action, new-counters should be computed from compute-next-counters
      ;; With no new messages, counters should be mostly preserved
      (check-equal? (loop-counters-explore-count new-c) 3)
      (check-equal? (loop-counters-implement-count new-c) 2))

    (test-case "step-action? is a valid contract"
      (check-true (procedure? step-action?)))

    (test-case "step-result rejects invalid action symbol via contract"
      (check-exn exn:fail:contract?
                 (lambda () (step-result 'invalid-action 'completed (make-test-counters) (hasheq)))))

    (test-case "step-result accepts all valid action symbols"
      (for-each (lambda (action)
                  (check-pred step-result? (step-result action 'completed (make-test-counters) (hasheq))))
                '(continue stop stop-hard-limit stop-soft-limit stop-budget)))))

;; ============================================================
;; Run all tests
;; ============================================================

(define all-tests
  (test-suite "Iteration Pure Functions (v0.33.1 W0)"
    compute-step-result-tests))

(run-tests all-tests)
