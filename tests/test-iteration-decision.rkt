#lang racket
;; BOUNDARY: pure
;; BOUNDARY: unit
;; tests/test-iteration-decision.rkt — Pure decision function tests (T-1a)

(require rackunit
         rackunit/text-ui
         "../runtime/iteration/decision.rkt"
         "../agent/iteration/loop-state.rkt"
         "../util/loop-result.rkt")

(define helper-suite
  (test-suite "iteration-decision helpers"

    (test-case "known-termination-reasons contains expected symbols"
      (define reasons (known-termination-reasons))
      (for ([r '(completed cancelled tool-calls-pending error
                 force-shutdown shutdown max-iterations-exceeded hook-blocked)])
        (check-not-false (member r reasons)
                         (format "expected ~a in known-termination-reasons" r))))

    (test-case "step-action? accepts known actions"
      (for ([a '(continue stop stop-hard-limit stop-soft-limit)])
        (check-true (step-action? a)
                    (format "step-action? should accept ~a" a))))

    (test-case "step-action? rejects unknown actions"
      (check-false (step-action? 'unknown-action))
      (check-false (step-action? 42))
      (check-false (step-action? "continue")))

    (test-case "iteration-ctx constructor and accessors"
      (define ctx (iteration-ctx 5 3 2 20 30))
      (check-equal? (iteration-ctx-iteration ctx) 5)
      (check-equal? (iteration-ctx-consecutive-tool-count ctx) 3)
      (check-equal? (iteration-ctx-explore-count ctx) 2)
      (check-equal? (iteration-ctx-max-iterations ctx) 20)
      (check-equal? (iteration-ctx-max-iterations-hard ctx) 30))

    (test-case "step-result constructor and accessors"
      (define sr (step-result 'continue #f (void) (hasheq)))
      (check-equal? (step-result-action sr) 'continue)
      (check-false (step-result-termination sr))
      (check-equal? (step-result-metadata sr) (hasheq)))
  ))

(define decide-suite
  (test-suite "decide-next-action"

    (test-case "completed -> stop"
      (define ctx (iteration-ctx 0 0 0 10 20))
      (define result (make-loop-result '() 'completed (hasheq)))
      (check-equal? (decide-next-action ctx result) 'stop))

    (test-case "cancelled -> stop"
      (define ctx (iteration-ctx 0 0 0 10 20))
      (define result (make-loop-result '() 'cancelled (hasheq)))
      (check-equal? (decide-next-action ctx result) 'stop))

    (test-case "force-shutdown -> stop"
      (define ctx (iteration-ctx 0 0 0 10 20))
      (define result (make-loop-result '() 'force-shutdown (hasheq)))
      (check-equal? (decide-next-action ctx result) 'stop))

    (test-case "error -> stop"
      (define ctx (iteration-ctx 0 0 0 10 20))
      (define result (make-loop-result '() 'error (hasheq)))
      (check-equal? (decide-next-action ctx result) 'stop))

    (test-case "hook-blocked -> stop"
      (define ctx (iteration-ctx 0 0 0 10 20))
      (define result (make-loop-result '() 'hook-blocked (hasheq)))
      (check-equal? (decide-next-action ctx result) 'stop))

    (test-case "max-iterations-exceeded -> stop"
      (define ctx (iteration-ctx 0 0 0 10 20))
      (define result (make-loop-result '() 'max-iterations-exceeded (hasheq)))
      (check-equal? (decide-next-action ctx result) 'stop))

    (test-case "tool-calls-pending iteration < soft limit -> continue"
      (define ctx (iteration-ctx 5 0 0 10 20))
      (define result (make-loop-result '() 'tool-calls-pending (hasheq)))
      (check-equal? (decide-next-action ctx result) 'continue))

    (test-case "tool-calls-pending iteration at soft limit < hard -> stop-soft-limit"
      (define ctx (iteration-ctx 9 0 0 10 20))
      (define result (make-loop-result '() 'tool-calls-pending (hasheq)))
      (check-equal? (decide-next-action ctx result) 'stop-soft-limit))

    (test-case "tool-calls-pending iteration at hard limit -> stop-hard-limit"
      (define ctx (iteration-ctx 19 0 0 10 20))
      (define result (make-loop-result '() 'tool-calls-pending (hasheq)))
      (check-equal? (decide-next-action ctx result) 'stop-hard-limit))

    (test-case "unknown termination reason -> stop"
      (define ctx (iteration-ctx 0 0 0 10 20))
      (define result (make-loop-result '() 'unknown-reason (hasheq)))
      (check-equal? (decide-next-action ctx result) 'stop))
  ))

(define compute-suite
  (test-suite "compute-step-result"

    (test-case "continue action preserves termination #f"
      (define ctx (iteration-ctx 0 0 0 10 20))
      (define result (make-loop-result '() 'tool-calls-pending (hasheq)))
      (define counters (make-initial-counters))
      (define sr (compute-step-result ctx result counters))
      (check-equal? (step-result-action sr) 'continue)
      (check-equal? (step-result-termination sr) 'tool-calls-pending))

    (test-case "stop-hard-limit metadata has maxIterationsReached"
      (define ctx (iteration-ctx 19 0 0 10 20))
      (define result (make-loop-result '() 'tool-calls-pending (hasheq)))
      (define counters (make-initial-counters))
      (define sr (compute-step-result ctx result counters))
      (check-equal? (step-result-action sr) 'stop-hard-limit)
      (check-true (hash-ref (step-result-metadata sr) 'maxIterationsReached #f)))

    (test-case "completed stop has empty metadata"
      (define ctx (iteration-ctx 0 0 0 10 20))
      (define result (make-loop-result '() 'completed (hasheq)))
      (define counters (make-initial-counters))
      (define sr (compute-step-result ctx result counters))
      (check-equal? (step-result-action sr) 'stop)
      (check-equal? (step-result-metadata sr) (hasheq)))

    (test-case "step-result has new-counters"
      (define ctx (iteration-ctx 0 0 0 10 20))
      (define result (make-loop-result '() 'completed (hasheq)))
      (define counters (make-initial-counters))
      (define sr (compute-step-result ctx result counters))
      (check-not-false (step-result-new-counters sr)))
  ))

(run-tests helper-suite 'verbose)
(run-tests decide-suite 'verbose)
(run-tests compute-suite 'verbose)
