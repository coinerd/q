#lang racket/base

;; @speed fast
;; @suite default

;; tests/test-context-assembly-e2e.rkt — End-to-end context assembly lifecycle test
;; v0.75.4 W1: Full lifecycle integration test.

(require rackunit
         rackunit/text-ui
         racket/string
         (only-in "../runtime/context-assembly/task-state.rkt"
                  task-idle
                  task-exploration
                  task-planning
                  task-implementation
                  task-verification
                  task-debugging)
         (only-in "../runtime/context-assembly/task-conclusion.rkt" task-conclusion task-conclusion?)
         (only-in "../runtime/context-assembly/ws-evolution.rkt" evolve-working-set-for-state)
         (only-in "../runtime/context-assembly/serialization.rkt"
                  build-tiered-context/state-aware
                  tiered-context-tier-a
                  current-task-state-aware-assembly?)
         (only-in "../runtime/context-assembly/conclusion-graph.rkt"
                  build-conclusion-graph
                  graph-detect-cycles)
         (only-in "../runtime/context-assembly/conclusion-ranker.rkt" rank-and-budget)
         (only-in "../runtime/context-assembly/auto-distillation.rkt"
                  auto-distill
                  current-auto-distillation-enabled?)
         (only-in "../runtime/context-assembly/rollback-actions.rkt"
                  warnings->actions
                  select-highest-priority-action
                  maybe-execute-action
                  current-rollback-action-execution?
                  rollback-action-type)
         (only-in "../runtime/session/session-config.rkt"
                  apply-context-assembly-profile!
                  current-context-assembly-profile)
         (only-in "../runtime/context-assembly/state-aware-builder.rkt"
                  current-conclusion-token-budget)
         (only-in "../runtime/working-set.rkt"
                  make-working-set
                  working-set-entry-count
                  working-set-add!
                  working-set-token-count)
         (only-in "../util/message/protocol-types.rkt" make-message make-text-part)
         (only-in "../runtime/context-assembly/state-relevance.rkt" context-level-for-state))

;; Helpers
(define (test-msg role text)
  (make-message "test-id" #f role 'text (list (make-text-part text)) (current-seconds) (hasheq)))

(define (populate-ws ws paths)
  (for ([p (in-list paths)])
    (working-set-add! ws p (format "msg-~a" p) 500)))

(define (make-conclusions n)
  (for/list ([i (in-range n)])
    (task-conclusion (format "c~a" i)
                     (format "Conclusion ~a: important finding" i)
                     'fact
                     'idle
                     '()
                     (current-seconds)
                     '() ; relevance-tags
                     '()))) ; dependencies

(define base-messages
  (list (test-msg 'user "explore the codebase")
        (test-msg 'assistant "reading files...")
        (test-msg 'user "now implement the feature")
        (test-msg 'assistant "editing main.rkt...")))

(define suite
  (test-suite "context-assembly-e2e"

    ;; Full lifecycle
    (test-case "full lifecycle: exploration through idle"
      (define ws (make-working-set))
      (define conclusions '())

      ;; Phase 1: Exploration — 5 files pinned
      (populate-ws
       ws
       '("src/main.rkt" "src/util.rkt" "tests/test.rkt" "docs/README.md" "src/helper.rkt"))
      (check-equal? (working-set-entry-count ws) 5 "5 files pinned")

      ;; Phase 2: Transition exploration → planning
      (set! conclusions (make-conclusions 3))
      (define evolved-c (evolve-working-set-for-state ws task-exploration task-planning conclusions))
      (check-equal? (working-set-entry-count ws) 0 "ws cleared exploration→planning")
      (check-equal? (length evolved-c) 3 "3 conclusions returned")

      ;; Phase 3: Build state-aware context in planning state
      (define tc
        (build-tiered-context/state-aware base-messages
                                          #:working-set-messages '()
                                          #:task-state task-planning
                                          #:conclusions evolved-c))
      (define tier-a (tiered-context-tier-a tc))
      (check-true (>= (length tier-a) 3) "planning has conclusions in context")

      ;; Phase 4: planning → implementation
      (define impl-c (evolve-working-set-for-state ws task-planning task-implementation evolved-c))
      (check-equal? (working-set-entry-count ws) 0 "ws still clear")

      ;; Phase 5: implementation → idle
      (define idle-c (evolve-working-set-for-state ws task-implementation task-idle impl-c))
      (check-equal? (working-set-entry-count ws) 0 "ws cleared on idle")
      (check-equal? idle-c '() "idle returns no conclusions"))

    ;; Debugging cycle
    (test-case "debugging cycle preserves error-related files"
      (define ws (make-working-set))
      (populate-ws
       ws
       '("src/main.rkt" "tests/test-foo.rkt" "src/error.rkt" "src/util.rkt" "tests/test-bar.rkt"))
      (check-equal? (working-set-entry-count ws) 5)

      (define conclusions (make-conclusions 2))
      (evolve-working-set-for-state ws task-implementation task-debugging conclusions)
      (check-true (>= (working-set-entry-count ws) 2) "error-related files kept")
      (check-true (<= (working-set-entry-count ws) 3) "non-error files removed")

      (evolve-working-set-for-state ws task-debugging task-implementation conclusions)
      (check-equal? (working-set-entry-count ws) 0 "ws cleared debugging→impl"))

    ;; Relevance level consistency
    (test-case "state relevance levels are internally consistent"
      (for ([state (list task-idle
                         task-exploration
                         task-planning
                         task-implementation
                         task-verification
                         task-debugging)])
        (for ([cat '(system-prompt conclusions working-set recent-messages tool-results plan-notes)])
          (define level (context-level-for-state state cat))
          (check-not-false (memq level '(full summary filtered excluded))
                           (format "invalid level ~a" level)))))

    ;; Token reduction
    (test-case "exploration→implementation reduces ws tokens"
      (define ws (make-working-set))
      (populate-ws ws '("a.rkt" "b.rkt" "c.rkt" "d.rkt" "e.rkt"))
      (define before (working-set-token-count ws))
      (evolve-working-set-for-state ws task-exploration task-implementation (make-conclusions 3))
      (define after (working-set-token-count ws))
      (check-true (< after before) (format "tokens reduced: ~a < ~a" after before)))

    ;; Conclusions persist through transitions
    (test-case "conclusions persist through multiple transitions"
      (define ws (make-working-set))
      (define original (make-conclusions 5))
      (populate-ws ws '("file1.rkt"))
      (define c1 (evolve-working-set-for-state ws task-exploration task-planning original))
      (check-equal? (length c1) 5 "preserved through exploration→planning")
      (define c2 (evolve-working-set-for-state ws task-planning task-implementation c1))
      (check-equal? (length c2) 5 "preserved through planning→impl"))

    ;; v0.77.9 T3.2: Wiring verification tests
    (test-case "graph selection builds DAG from conclusions with dependencies"
      (define cA (task-conclusion "a" "textA" 'fact 'implementation '("m1") 100 '() '("b")))
      (define cB (task-conclusion "b" "textB" 'fact 'implementation '("m2") 200 '() '()))
      (define g (build-conclusion-graph (list cA cB)))
      (check-equal? (graph-detect-cycles g) '() "acyclic graph has no cycles"))

    (test-case "rank-and-budget enforces token budget"
      (define concs
        (for/list ([i (in-range 20)])
          (task-conclusion (format "bc~a" i)
                           (format "A substantial conclusion about finding ~a" i)
                           'fact
                           'implementation
                           '()
                           1000
                           '()
                           '())))
      (define budgeted
        (rank-and-budget concs #:current-state 'implementation #:max-conclusion-tokens 500))
      (check-true (< (length budgeted) (length concs))
                  (format "budget trims ~a to ~a" (length concs) (length budgeted))))

    (test-case "auto-distill generates conclusions for uncovered entries"
      (parameterize ([current-auto-distillation-enabled? #t])
        (define ws-ids '("m1" "m2" "m3" "m4"))
        (define existing (list (task-conclusion "c1" "covers m1" 'fact 'idle '("m1") 100 '() '())))
        (define result (auto-distill ws-ids existing 'idle))
        (check-equal? (length result) 3 "3 uncovered entries get fallback conclusions")))

    (test-case "auto-distill returns empty when disabled"
      (parameterize ([current-auto-distillation-enabled? #f])
        (define result (auto-distill '("m1" "m2") '() 'idle))
        (check-equal? result '() "no distillation when flag off")))

    (test-case "rollback action executes when enabled"
      (parameterize ([current-rollback-action-execution? #t])
        (define actions (warnings->actions '("amnesia detected")))
        (define best (select-highest-priority-action actions))
        (check-true (eq? (rollback-action-type best) 'force-distill))
        (define result (maybe-execute-action best))
        (check-true (symbol? result) "action was executed")))

    (test-case "rollback action blocked when disabled"
      (parameterize ([current-rollback-action-execution? #f])
        (define actions (warnings->actions '("amnesia detected")))
        (define best (select-highest-priority-action actions))
        (check-false (maybe-execute-action best) "action blocked when flag off")))

    (test-case "profile bounded activates state-aware + budget"
      (parameterize ([current-task-state-aware-assembly? #f]
                     [current-conclusion-token-budget 0])
        (apply-context-assembly-profile! 'bounded)
        (check-true (current-task-state-aware-assembly?))
        (check-equal? (current-conclusion-token-budget) 2000)
        ;; Reset
        (apply-context-assembly-profile! 'off)))

    (test-case "profile off deactivates state-aware"
      (parameterize ([current-task-state-aware-assembly? #t])
        (apply-context-assembly-profile! 'off)
        (check-false (current-task-state-aware-assembly?))))))

(run-tests suite)
