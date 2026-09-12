#lang racket
;; @covers extensions/gsd-planning/execution-policy.rkt

;; @speed fast  ;; @suite extensions
;; @boundary unit

;; BOUNDARY: integration

;; tests/test-gsd-execution-policy.rkt — tests for tool blocking and mode-based guards

(require rackunit
         rackunit/text-ui
         "../extensions/gsd/plan-types.rkt"
         "../extensions/gsd/state-machine.rkt"
         (only-in "../extensions/gsd/session-state.rkt" current-gsd-ctx make-gsd-context)
         (only-in "../extensions/hooks.rkt" hook-result-action)
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
      (check-not-false result))

    ;; BUG-0072 seam regression: the real scheduler seam delivers these exact
    ;; keys; the planning-artifact guard must see them as a hash. Uses an
    ;; isolated ctx (current-gsd-ctx parameter) so the mode is authoritative.
    (test-case "gsd-tool-guard: hash seam delivers tool-arguments for planning-artifact guard"
      (define ctx (make-gsd-context))
      (gsm-ctx-transition-to! ctx 'executing)
      (define result
        (parameterize ([current-gsd-ctx ctx])
          (gsd-tool-guard
           (hasheq 'tool-name "write" 'tool-arguments (hasheq 'path "/repo/.planning/PLAN.md")))))
      (check-equal? (hook-result-action result)
                    'block
                    "executing-mode write to a protected planning artifact must be blocked")
      (gsm-ctx-reset! ctx))))

;; Ensure clean state
(gsm-transition! 'idle)

(module+ main
  (run-tests exec-policy-tests))
