#lang racket/base

;; @speed fast
;; @suite default

;; tests/test-ws-evolution.rkt — Working-set evolution tests
;; v0.75.4 W0: Verify ws-evolution behavior on state transitions.

(require rackunit
         rackunit/text-ui
         racket/string
         (only-in "../runtime/working-set.rkt"
                  make-working-set
                  working-set?
                  working-set-entries
                  working-set-entry-count
                  working-set-token-count
                  working-set-add!
                  working-set-reset!
                  ws-entry-path)
         (only-in "../runtime/context-assembly/ws-evolution.rkt"
                  evolve-working-set-for-state
                  working-set-selective-keep
                  evolve-working-set-for-state/result
                  evolution-result?
                  evolution-result-kept-entries
                  evolution-result-archived-entries
                  evolution-result-evicted-conclusions)
         (only-in "../runtime/context-assembly/task-state.rkt"
                  task-idle
                  task-exploration
                  task-planning
                  task-implementation
                  task-verification
                  task-debugging)
         (only-in "../runtime/context-assembly/task-conclusion.rkt" task-conclusion))

;; Helpers
(define (populate-ws ws paths)
  (for ([p (in-list paths)])
    (working-set-add! ws p (format "msg-~a" p) 500)))

(define conclusions
  (list (task-conclusion "c1" "Use struct for data" 'fact 'idle '() (current-seconds) '() '())
        (task-conclusion "c2" "Tests in tests/" 'fact 'idle '() (current-seconds) '() '())))

(define suite
  (test-suite "ws-evolution"

    ;; ── exploration → planning: full reset, conclusions returned ──

    (test-case "exploration→planning clears working-set, returns conclusions"
      (define ws (make-working-set))
      (populate-ws ws '("file1.rkt" "file2.rkt" "file3.rkt"))
      (check-equal? (working-set-entry-count ws) 3)
      (define result (evolve-working-set-for-state ws task-exploration task-planning conclusions))
      (check-equal? (working-set-entry-count ws) 0 "ws should be cleared")
      (check-equal? (length result) 2 "conclusions returned"))

    ;; ── exploration → implementation: full reset ──

    (test-case "exploration→implementation clears working-set"
      (define ws (make-working-set))
      (populate-ws ws '("a.rkt" "b.rkt" "c.rkt" "d.rkt" "e.rkt"))
      (evolve-working-set-for-state ws task-exploration task-implementation conclusions)
      (check-equal? (working-set-entry-count ws) 0 "ws should be cleared"))

    ;; ── implementation → debugging: keep test/error/spec files ──

    (test-case "implementation→debugging keeps error-related files"
      (define ws (make-working-set))
      (populate-ws
       ws
       '("src/main.rkt" "tests/test-foo.rkt" "src/error-handler.rkt" "docs/spec.rkt" "src/util.rkt"))
      (evolve-working-set-for-state ws task-implementation task-debugging conclusions)
      (define remaining (working-set-entries ws))
      (define remaining-paths (map ws-entry-path remaining))
      (check-not-false (member "tests/test-foo.rkt" remaining-paths) "test file kept")
      (check-not-false (member "src/error-handler.rkt" remaining-paths) "error file kept")
      (check-not-false (member "docs/spec.rkt" remaining-paths) "spec file kept")
      (check-false (member "src/main.rkt" remaining-paths) "main.rkt should be removed")
      (check-false (member "src/util.rkt" remaining-paths) "util.rkt should be removed"))

    ;; ── debugging → implementation: full reset ──

    (test-case "debugging→implementation clears working-set"
      (define ws (make-working-set))
      (populate-ws ws '("tests/test-x.rkt" "src/error.rkt"))
      (evolve-working-set-for-state ws task-debugging task-implementation conclusions)
      (check-equal? (working-set-entry-count ws) 0 "ws should be cleared"))

    ;; ── any → idle: full reset ──

    (test-case "any→idle resets working-set"
      (define ws (make-working-set))
      (populate-ws ws '("a.rkt" "b.rkt" "c.rkt"))
      (evolve-working-set-for-state ws task-implementation task-idle conclusions)
      (check-equal? (working-set-entry-count ws) 0 "ws should be reset on idle"))

    (test-case "exploration→idle resets working-set"
      (define ws (make-working-set))
      (populate-ws ws '("a.rkt" "b.rkt"))
      (define result (evolve-working-set-for-state ws task-exploration task-idle conclusions))
      (check-equal? (working-set-entry-count ws) 0)
      (check-equal? result '() "idle returns no conclusions"))

    ;; ── Other transitions: no change ──

    ;; GAP-B v0.97.8: planning→verification now keeps spec/test/validation files
    (test-case "planning→verification keeps spec/test files"
      (define ws (make-working-set))
      (populate-ws ws '("plan.rkt" "tests/test-foo.rkt" "spec.rkt" "src/main.rkt" "validation.rkt"))
      (evolve-working-set-for-state ws task-planning task-verification conclusions)
      (define remaining (map ws-entry-path (working-set-entries ws)))
      (check-not-false (member "tests/test-foo.rkt" remaining) "test file kept")
      (check-not-false (member "spec.rkt" remaining) "spec file kept")
      (check-not-false (member "validation.rkt" remaining) "validation file kept")
      (check-false (member "plan.rkt" remaining) "plan.rkt removed")
      (check-false (member "src/main.rkt" remaining) "main.rkt removed"))

    (test-case "idle→exploration keeps working-set"
      (define ws (make-working-set))
      (populate-ws ws '("x.rkt"))
      (evolve-working-set-for-state ws task-idle task-exploration conclusions)
      (check-equal? (working-set-entry-count ws) 1 "ws unchanged for idle→exploration"))

    ;; ── working-set-selective-keep ──

    (test-case "working-set-selective-keep filters entries"
      (define ws (make-working-set))
      (populate-ws ws '("test-a.rkt" "src/b.rkt" "test-c.rkt" "src/d.rkt"))
      (working-set-selective-keep ws (lambda (e) (string-contains? (ws-entry-path e) "test")))
      (define remaining (map ws-entry-path (working-set-entries ws)))
      (check-equal? (length remaining) 2 "only test files kept")
      (check-not-false (member "test-a.rkt" remaining))
      (check-not-false (member "test-c.rkt" remaining)))

    ;; v0.77.1 W1.1: evolution-result struct

    (test-case "evolution-result exploration->planning archives all"
      (define ws (make-working-set))
      (populate-ws ws '("file1.rkt" "file2.rkt" "file3.rkt"))
      (define result
        (evolve-working-set-for-state/result ws task-exploration task-planning conclusions))
      (check-true (evolution-result? result))
      (check-equal? (length (evolution-result-kept-entries result)) 0)
      (check-equal? (length (evolution-result-archived-entries result)) 3)
      (check-equal? (length (evolution-result-evicted-conclusions result)) 2))

    (test-case "evolution-result impl->debugging archives non-error files"
      (define ws (make-working-set))
      (populate-ws ws '("src/main.rkt" "tests/test-foo.rkt" "src/error-handler.rkt"))
      (define result
        (evolve-working-set-for-state/result ws task-implementation task-debugging conclusions))
      (check-true (evolution-result? result))
      (check-equal? (length (evolution-result-kept-entries result)) 2)
      (check-equal? (length (evolution-result-archived-entries result)) 1))

    (test-case "evolution-result no-change transition returns #f"
      (define ws (make-working-set))
      (populate-ws ws '("plan.rkt"))
      (define result
        (evolve-working-set-for-state/result ws task-exploration task-exploration conclusions))
      (check-false result "same-state transition returns #f"))

    ;; ============================================================
    ;; v0.97.8 W1: GAP-B WS transition regression tests
    ;; ============================================================

    (test-case "GAP-B: planning→implementation clears working-set"
      (define ws (make-working-set))
      (populate-ws ws '("plan.rkt" "notes.md" "spec.rkt"))
      (define result (evolve-working-set-for-state ws task-planning task-implementation conclusions))
      (check-equal? (working-set-entry-count ws) 0 "ws should be cleared")
      (check-equal? (length result) 2 "conclusions returned"))

    (test-case "GAP-B: planning→debugging keeps error/test/spec files"
      (define ws (make-working-set))
      (populate-ws ws '("plan.rkt" "tests/test-bug.rkt" "src/error.rkt" "spec.rkt" "docs.md"))
      (evolve-working-set-for-state ws task-planning task-debugging conclusions)
      (define remaining (map ws-entry-path (working-set-entries ws)))
      (check-not-false (member "tests/test-bug.rkt" remaining) "test file kept")
      (check-not-false (member "src/error.rkt" remaining) "error file kept")
      (check-not-false (member "spec.rkt" remaining) "spec file kept")
      (check-false (member "plan.rkt" remaining) "plan.rkt removed")
      (check-false (member "docs.md" remaining) "docs.md removed"))

    (test-case "GAP-B: verification→implementation clears working-set"
      (define ws (make-working-set))
      (populate-ws ws '("tests/test-a.rkt" "spec.rkt" "validation.rkt"))
      (define result
        (evolve-working-set-for-state ws task-verification task-implementation conclusions))
      (check-equal? (working-set-entry-count ws) 0 "ws should be cleared")
      (check-equal? (length result) 2 "conclusions returned"))

    (test-case "GAP-B: planning→implementation returns all valid conclusions"
      (define ws (make-working-set))
      (populate-ws ws '("plan.rkt"))
      (define many-conclusions
        (for/list ([i (in-range 5)])
          (task-conclusion (format "bc~a" i)
                           (format "finding ~a" i)
                           'fact
                           'planning
                           '()
                           (current-seconds)
                           '()
                           '())))
      (define result
        (evolve-working-set-for-state ws task-planning task-implementation many-conclusions))
      (check-equal? (length result) 5 "all conclusions returned")
      (check-equal? (working-set-entry-count ws) 0))

    (test-case "GAP-B: planning→verification result struct tracks archives"
      (define ws (make-working-set))
      (populate-ws ws '("plan.rkt" "tests/test-x.rkt" "spec.rkt" "src/main.rkt"))
      (define result
        (evolve-working-set-for-state/result ws task-planning task-verification conclusions))
      (check-true (evolution-result? result))
      (check-equal? (length (evolution-result-kept-entries result)) 2 "test+spec kept")
      (check-equal? (length (evolution-result-archived-entries result)) 2 "plan+main archived")
      (check-equal? (length (evolution-result-evicted-conclusions result)) 2))

    (test-case "GAP-B: verification→implementation result struct tracks archives"
      (define ws (make-working-set))
      (populate-ws ws '("tests/test-a.rkt" "spec.rkt"))
      (define result
        (evolve-working-set-for-state/result ws task-verification task-implementation conclusions))
      (check-true (evolution-result? result))
      (check-equal? (length (evolution-result-kept-entries result)) 0 "all cleared")
      (check-equal? (length (evolution-result-archived-entries result)) 2 "both archived")
      (check-equal? (length (evolution-result-evicted-conclusions result)) 2))

    ;; LF5: planning→exploration falls to else (no WS mutation)
    (test-case "GAP-B: planning→exploration returns conclusions without WS mutation"
      (define ws (make-working-set))
      (populate-ws ws '("plan.md" "src/main.rkt"))
      (define result
        (evolve-working-set-for-state/result ws task-planning task-exploration conclusions))
      (check-true (evolution-result? result))
      ;; else clause: returns conclusions, keeps WS intact
      (check-equal? (length (evolution-result-kept-entries result)) 2 "WS unchanged")
      (check-equal? (length (evolution-result-archived-entries result)) 0 "nothing archived")
      (check-equal? (length (evolution-result-evicted-conclusions result)) 2))

    ;; ============================================================
    ;; v0.97.10 W1: GAP-B idle-reset regression tests
    ;; ============================================================

    (test-case "GAP-B v0.97.10: implementation→idle resets WS entries"
      (define ws (make-working-set))
      (populate-ws ws '("src/impl.rkt" "src/helper.rkt" "tests/test.rkt"))
      (define result
        (evolve-working-set-for-state/result ws task-implementation task-idle conclusions))
      (check-true (evolution-result? result))
      (check-equal? (working-set-entry-count ws) 0 "WS should be reset on impl→idle")
      (check-equal? (length (evolution-result-kept-entries result)) 0 "no entries kept")
      (check-equal? (length (evolution-result-archived-entries result)) 3 "all archived"))

    (test-case "GAP-B v0.97.10: idle→idle is no-op (same state)"
      (define ws (make-working-set))
      (populate-ws ws '("a.rkt"))
      ;; Same-state transition should return #f (no change)
      (define result (evolve-working-set-for-state/result ws task-idle task-idle conclusions))
      (check-false result "idle→idle should be no-op")
      (check-equal? (working-set-entry-count ws) 1 "WS unchanged"))

    (test-case "GAP-B v0.97.10: exploration→idle with populated WS"
      (define ws (make-working-set))
      (populate-ws ws '("explore1.rkt" "explore2.rkt" "explore3.rkt" "explore4.rkt"))
      (define result (evolve-working-set-for-state/result ws task-exploration task-idle conclusions))
      (check-true (evolution-result? result))
      (check-equal? (working-set-entry-count ws) 0 "WS reset on exploration→idle")
      (check-equal? (length (evolution-result-archived-entries result)) 4 "all entries archived"))))

(run-tests suite)
