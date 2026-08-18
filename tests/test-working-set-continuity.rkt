#lang racket

;; @speed fast
;; @suite default

;; BOUNDARY: unit

;; tests/test-working-set-continuity.rkt
;; W2 (#8939): Working-set continuity and prompt-boundary classification.
;;
;; Tests the decision of whether to reset the working set when a new
;; user prompt arrives. The D9 audit showed that a related prompt
;; ("now commit and push") wiped a 30-entry working set to zero because
;; the lifecycle unconditionally called working-set-reset!.
;;
;; The fix classifies the prompt boundary:
;; @boundary unit
;;   continuation  → retain working set
;;   narrowing     → retain working set
;;   superseding   → retain working set (update objective)
;;   new-task      → archive/reset working set
;;   ambiguous     → retain conservatively (mark tentative)

(require rackunit
         rackunit/text-ui
         "../runtime/task-memory/working-set-continuity.rkt")

(define continuity-tests
  (test-suite "Working-set continuity and boundary classification"

    ;; ── T01: classify continuation prompts ──
    (test-case "continuation: 'continue' retains working set"
      (define boundary
        (classify-prompt-boundary "finish the implementation" "continue with the next step"))
      (check-equal? (prompt-boundary-type-boundary boundary) 'continuation)
      (check-false (should-reset-working-set? boundary)))

    (test-case "continuation: 'proceed' retains working set"
      (define boundary (classify-prompt-boundary "implement feature X" "proceed with the plan"))
      (check-equal? (prompt-boundary-type-boundary boundary) 'continuation)
      (check-false (should-reset-working-set? boundary)))

    (test-case "continuation: 'now commit and push' retains working set"
      ;; This is the exact D9 scenario — a related follow-up prompt
      (define boundary
        (classify-prompt-boundary "implement the task-ledger module"
                                  "now commit and push the changes"))
      (check-false (should-reset-working-set? boundary)))

    ;; ── T02: classify narrowing prompts ──
    (test-case "narrowing: 'actually only do the first part' retains working set"
      (define boundary
        (classify-prompt-boundary "implement waves W1 through W5" "actually only do W1 first"))
      (check-equal? (prompt-boundary-type-boundary boundary) 'narrowing)
      (check-false (should-reset-working-set? boundary)))

    (test-case "narrowing: 'wait, just the tests' retains working set"
      (define boundary
        (classify-prompt-boundary "write the module and tests" "wait, just write the tests first"))
      (check-false (should-reset-working-set? boundary)))

    ;; ── T03: classify superseding prompts ──
    (test-case "superseding: 'instead of X, do Y' retains working set"
      (define boundary
        (classify-prompt-boundary "use approach A for the codec"
                                  "instead, use approach B for the codec"))
      (check-equal? (prompt-boundary-type-boundary boundary) 'superseding)
      (check-false (should-reset-working-set? boundary)))

    ;; ── T04: classify new-task prompts ──
    (test-case "new-task: 'switch to a different feature' resets working set"
      (define boundary
        (classify-prompt-boundary "implement the memory module"
                                  "switch to a completely different feature now"))
      (check-equal? (prompt-boundary-type-boundary boundary) 'new-task)
      (check-true (should-reset-working-set? boundary)))

    (test-case "new-task: 'start a new task' resets working set"
      (define boundary (classify-prompt-boundary "fix bug #123" "start a new task: work on the TUI"))
      (check-equal? (prompt-boundary-type-boundary boundary) 'new-task)
      (check-true (should-reset-working-set? boundary)))

    ;; ── T05: classify ambiguous prompts ──
    (test-case "ambiguous: unrelated content is retained conservatively"
      (define boundary
        (classify-prompt-boundary "implement the memory module" "the weather is nice today"))
      (check-equal? (prompt-boundary-type-boundary boundary) 'ambiguous)
      (check-false (should-reset-working-set? boundary)))

    (test-case "ambiguous: empty current objective yields continuation"
      ;; No prior context → treat as initial/continuation
      (define boundary (classify-prompt-boundary "" "start working on the project"))
      (check-false (should-reset-working-set? boundary)))

    ;; ── T06: boundary relation metadata ──
    (test-case "boundary carries tentative flag for ambiguous"
      (define boundary (classify-prompt-boundary "do task A" "something unrelated"))
      (check-equal? (prompt-boundary-type-boundary boundary) 'ambiguous)
      (check-true (prompt-boundary-type-tentative? boundary)))

    (test-case "boundary does not flag tentative for continuation"
      (define boundary (classify-prompt-boundary "do task A" "continue with task A"))
      (check-false (prompt-boundary-type-tentative? boundary)))

    ;; ── T07: D9 regression — commit/push prompt preserves attention ──
    (test-case "D9 regression: commit/push prompt does not reset working set"
      ;; Simulate the exact D9 scenario: 30 entries of active attention
      ;; followed by a "commit and push" related prompt.
      (define boundary
        (classify-prompt-boundary
         "implement the memory-first context remediation milestone"
         "Address the plan comprehensively by implementing the milestone step by step"))
      ;; This is a continuation/superseding, NOT a new task
      (check-false (should-reset-working-set? boundary)))

    ;; ── T08: boundary with no current objective ──
    (test-case "first prompt has no prior objective"
      (define boundary (classify-prompt-boundary #f "implement feature X"))
      (check-equal? (prompt-boundary-type-boundary boundary) 'initial)
      (check-false (should-reset-working-set? boundary)))))

(run-tests continuity-tests)
