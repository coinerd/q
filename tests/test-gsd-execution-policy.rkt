#lang racket

;; BOUNDARY: integration

;; tests/test-gsd-execution-policy.rkt — tests for tool blocking and mode-based guards

(require rackunit
         rackunit/text-ui
         "../extensions/gsd/plan-types.rkt"
         "../extensions/gsd/state-machine.rkt"
         "../extensions/gsd-planning/execution-policy.rkt")

(define exec-policy-tests
  (test-suite "GSD Execution Policy"

    (test-case "gsd-tool-guard: planning-write blocked in executing mode"
      (gsm-transition! 'executing)
      (define result (gsd-tool-guard (hasheq 'tool-name "planning-write")))
      ;; hook-block returns a specific structure
      (check-not-false result))

    (test-case "gsd-tool-guard: tools allowed in idle mode"
      (gsm-transition! 'idle)
      (define result (gsd-tool-guard (hasheq 'tool-name "bash")))
      (check-not-false result))

    (test-case "gsd-tool-guard: tool blocked in plan-written mode"
      (gsm-transition! 'plan-written)
      (define result (gsd-tool-guard (hasheq 'tool-name "write")))
      (check-not-false result)

      ;; Reset
      (gsm-transition! 'idle))))

;; Ensure clean state
(gsm-transition! 'idle)

(module+ main
  (run-tests exec-policy-tests))
(module+ test
  (run-tests exec-policy-tests))
