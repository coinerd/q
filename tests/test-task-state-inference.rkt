#lang racket/base

;; tests/test-task-state-inference.rkt — tests for state-inference heuristics
;; v0.75.2 W0: Tool-call pattern → task-state inference

(require rackunit
         rackunit/text-ui
         "../runtime/context-assembly/state-inference.rkt"
         (only-in "../runtime/context-assembly/task-state.rkt"
                  task-idle
                  task-exploration
                  task-planning
                  task-implementation
                  task-verification
                  task-debugging))

(define suite
  (test-suite "task-state-inference"

    ;; ── Empty / no tools ──

    (test-case "no tools → idle with high confidence"
      (define-values (state conf) (infer-task-state-from-tools '()))
      (check-equal? state task-idle)
      (check-true (>= conf 0.7)))

    ;; ── Exploration heuristics ──

    (test-case "3+ reads, 0 writes → exploration"
      (define-values (state conf) (infer-task-state-from-tools '("read" "read" "read" "find")))
      (check-equal? state task-exploration)
      (check-true (>= conf 0.8)))

    (test-case "2 reads → exploration with low confidence"
      (define-values (state conf) (infer-task-state-from-tools '("read" "grep")))
      (check-equal? state task-exploration)
      (check-true (< conf 0.7)))

    ;; ── Implementation heuristics ──

    (test-case "edit tool → implementation"
      (define-values (state conf) (infer-task-state-from-tools '("read" "edit")))
      (check-equal? state task-implementation)
      (check-true (>= conf 0.7)))

    (test-case "write tool → implementation"
      (define-values (state conf) (infer-task-state-from-tools '("write")))
      (check-equal? state task-implementation)
      (check-true (>= conf 0.7)))

    ;; ── Verification heuristics ──

    (test-case "test bash → verification"
      (define-values (state conf)
        (infer-task-state-from-tools '("bash") (list (hasheq 'command "raco test foo.rkt"))))
      (check-equal? state task-verification)
      (check-true (>= conf 0.7)))

    (test-case "test runner → verification"
      (define-values (state conf)
        (infer-task-state-from-tools '("bash")
                                     (list (hasheq 'command "racket scripts/run-tests.rkt"))))
      (check-equal? state task-verification))

    ;; ── Debugging heuristics ──

    (test-case "test + edit → debugging"
      (define-values (state conf)
        (infer-task-state-from-tools '("bash" "read" "edit")
                                     (list (hasheq 'command "raco test test.rkt") #f #f)))
      (check-equal? state task-debugging)
      (check-true (>= conf 0.7)))

    ;; ── Planning heuristics ──

    (test-case "save-conclusion → planning"
      (define-values (state conf) (infer-task-state-from-tools '("save-conclusion")))
      (check-equal? state task-planning))

    ;; ── Confidence threshold ──

    (test-case "confidence threshold parameter defaults to 0.7"
      (check-equal? (current-state-inference-threshold) 0.7))

    (test-case "low-confidence reads don't trigger transition"
      (define-values (state conf) (infer-task-state-from-tools '("read")))
      ;; Only 1 read → confidence 0.5, below threshold
      (check-true (< conf (current-state-inference-threshold))))

    ;; ── Tool name categorization ──

    (test-case "tool-name->category classifies read tools"
      (check-eq? (tool-name->category "read") 'read)
      (check-eq? (tool-name->category "find") 'read)
      (check-eq? (tool-name->category "grep") 'read))

    (test-case "tool-name->category classifies write tools"
      (check-eq? (tool-name->category "edit") 'write)
      (check-eq? (tool-name->category "write") 'write))

    (test-case "tool-name->category classifies bash"
      (check-eq? (tool-name->category "bash") 'bash))

    (test-case "tool-name->category classifies unknown"
      (check-eq? (tool-name->category "unknown-tool") 'other))

    ;; ── Batch inference ──

    (test-case "infer-from-recent-turns with empty turns → idle"
      (define-values (state conf) (infer-from-recent-turns '()))
      (check-equal? state task-idle))

    (test-case "infer-from-recent-turns with mixed turns"
      (define-values (state conf) (infer-from-recent-turns '(("read" "read" "read"))))
      (check-equal? state task-exploration))))

(run-tests suite)
