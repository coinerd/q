#lang racket/base

;; tests/test-iteration-pure.rkt — Property tests for pure iteration functions
;;
;; v0.33.1 W0: Test pure transition functions extracted from iteration loop.
;; These tests verify that compute-step-result and compute-termination
;; produce correct outputs for given inputs, without any I/O.

(require rackunit
         rackunit/text-ui
         (only-in "../runtime/iteration.rkt"
                  compute-step-result
                  compute-termination
                  step-result
                  step-result?
                  step-result-action
                  step-result-termination
                  step-result-new-counters
                  step-result-metadata
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
;; compute-termination tests
;; ============================================================

(define compute-termination-tests
  (test-suite "compute-termination"

    (test-case "completed → 'completed"
      (define ctx (make-test-ctx))
      (define result (make-test-result #:termination 'completed))
      (check-equal? (compute-termination ctx result) 'completed))

    (test-case "cancelled → 'cancelled"
      (define ctx (make-test-ctx))
      (define result (make-test-result #:termination 'cancelled))
      (check-equal? (compute-termination ctx result) 'cancelled))

    (test-case "force-shutdown → 'force-shutdown"
      (define ctx (make-test-ctx))
      (define result (make-test-result #:termination 'force-shutdown))
      (check-equal? (compute-termination ctx result) 'force-shutdown))

    (test-case "shutdown → 'shutdown"
      (define ctx (make-test-ctx))
      (define result (make-test-result #:termination 'shutdown))
      (check-equal? (compute-termination ctx result) 'shutdown))

    (test-case "hook-blocked → 'hook-blocked"
      (define ctx (make-test-ctx))
      (define result (make-test-result #:termination 'hook-blocked))
      (check-equal? (compute-termination ctx result) 'hook-blocked))

    (test-case "max-iterations-exceeded → 'max-iterations-exceeded"
      (define ctx (make-test-ctx))
      (define result (make-test-result #:termination 'max-iterations-exceeded))
      (check-equal? (compute-termination ctx result) 'max-iterations-exceeded))

    (test-case "error → 'error"
      (define ctx (make-test-ctx))
      (define result (make-test-result #:termination 'error))
      (check-equal? (compute-termination ctx result) 'error))

    (test-case "tool-calls-pending below soft limit → 'tool-calls-pending"
      (define ctx (make-test-ctx #:iteration 5 #:max-iterations 10))
      (define result (make-test-result #:termination 'tool-calls-pending))
      (check-equal? (compute-termination ctx result) 'tool-calls-pending))

    (test-case "tool-calls-pending at soft limit → 'tool-calls-pending"
      (define ctx (make-test-ctx #:iteration 10 #:max-iterations 10))
      (define result (make-test-result #:termination 'tool-calls-pending))
      ;; At soft limit, action is 'stop-soft-limit but termination is still tool-calls-pending
      (check-equal? (compute-termination ctx result) 'tool-calls-pending))

    (test-case "tool-calls-pending at hard limit → 'tool-calls-pending"
      (define ctx (make-test-ctx #:iteration 16 #:max-iterations-hard 16))
      (define result (make-test-result #:termination 'tool-calls-pending))
      (check-equal? (compute-termination ctx result) 'tool-calls-pending))))

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
      (check-equal? (loop-counters-implement-count new-c) 2))))

;; ============================================================
;; Run all tests
;; ============================================================

(define all-tests
  (test-suite "Iteration Pure Functions (v0.33.1 W0)"
    compute-termination-tests
    compute-step-result-tests))

(run-tests all-tests)
