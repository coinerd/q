#lang racket/base

;; tests/test-context-assembly-integration.rkt — Full pipeline integration test
;; v0.77.10 M5: Exercises the complete context assembly pipeline with all flags ON.

(require rackunit
         rackunit/text-ui
         racket/string
         (only-in "../runtime/context-assembly/task-state.rkt"
                  task-exploration
                  task-planning
                  task-implementation
                  task-debugging
                  task-idle)
         (only-in "../runtime/context-assembly/task-conclusion.rkt"
                  task-conclusion
                  task-conclusion?
                  task-conclusion-id)
         (only-in "../runtime/context-assembly/serialization.rkt"
                  build-tiered-context/state-aware
                  current-task-state-aware-assembly?
                  tiered-context-tier-a
                  tiered-context-tier-b
                  tiered-context-tier-c)
         (only-in "../runtime/context-assembly/conclusion-graph.rkt"
                  build-conclusion-graph
                  graph-select-conclusions)
         (only-in "../runtime/context-assembly/conclusion-ranker.rkt" rank-and-budget)
         (only-in "../runtime/context-assembly/auto-distillation.rkt"
                  auto-distill
                  current-auto-distillation-enabled?)
         (only-in "../runtime/context-assembly/rollback-actions.rkt"
                  warnings->actions
                  select-highest-priority-action
                  maybe-execute-action
                  current-rollback-action-execution?
                  current-force-distill-fn
                  current-expand-context-fn
                  current-rollback-action-log
                  rollback-action-type)
         (only-in "../runtime/context-assembly/ws-evolution.rkt" evolve-working-set-for-state)
         (only-in "../runtime/context-assembly/state-aware-builder.rkt"
                  current-task-state-aware-assembly?
                  current-conclusion-token-budget
                  current-graph-conclusion-selection?)
         (only-in "../runtime/context-assembly/state-aware-builder.rkt" current-ws-evolution-enabled?)
         (only-in "../runtime/session/session-config.rkt"
                  apply-context-assembly-profile!
                  current-context-assembly-profile)
         (only-in "../runtime/working-set.rkt" make-working-set working-set-add! working-set-entries)
         (only-in "../runtime/agent-session.rkt" session-rollout-enabled?)
         (only-in "../util/message/protocol-types.rkt"
                  make-message
                  make-text-part
                  message-role
                  message-content)
         (only-in "../util/message/message.rkt" message-id message-kind))

;; Helper: create a simple test message
(define (test-msg role text)
  (make-message (format "msg-~a" text)
                #f
                role
                'text
                (list (make-text-part text))
                (current-seconds)
                (hasheq)))

(define suite
  (test-suite "context-assembly-integration"

    ;; M4: graph-select-conclusions returns conclusion objects directly
    (test-case "graph selection with dependencies returns full objects"
      (define c1 (task-conclusion "c1" "use X for auth" 'decision 'implementation '() 1000 '() '()))
      (define c2 (task-conclusion "c2" "X depends on Y" 'fact 'implementation '() 2000 '() '("c1")))
      (define c3 (task-conclusion "c3" "unrelated" 'fact 'exploration '() 3000 '() '()))
      (define g (build-conclusion-graph (list c1 c2 c3)))
      ;; Seed with c2 → should get c2 + c1 (dependency)
      (define selected (graph-select-conclusions g '("c2")))
      (check-true (>= (length selected) 2))
      (define ids (map (lambda (c) (task-conclusion-id c)) selected))
      (check-not-false (member "c2" ids))
      (check-not-false (member "c1" ids))
      ;; c3 should NOT be selected (not reachable from c2)
      (check-false (member "c3" ids)))

    ;; M5: Full assembly with bounded profile
    (test-case "bounded profile activates state-aware + budget"
      (apply-context-assembly-profile! 'off) ; reset
      (apply-context-assembly-profile! 'bounded)
      (check-true (current-task-state-aware-assembly?))
      (check-true (> (current-conclusion-token-budget) 0))
      ;; Restore
      (apply-context-assembly-profile! 'off))

    ;; M5: Rollback action execution pipeline
    (test-case "rollback action executes with real callbacks"
      (define action-log '())
      (define distilled? (box #f))
      (define expanded? (box #f))
      (parameterize ([current-rollback-action-execution? #t]
                     [current-rollback-action-log '()]
                     [current-force-distill-fn (lambda (_action)
                                                 (set-box! distilled? #t)
                                                 (list 'force-distill-executed))]
                     [current-expand-context-fn (lambda (_action)
                                                  (set-box! expanded? #t)
                                                  (list 'expand-context-executed))])
        ;; Generate a force-distill action from warnings ("amnesia" triggers force-distill)
        (define actions (warnings->actions (list "amnesia-warning-detected")))
        (check-true (pair? actions))
        (define action (select-highest-priority-action actions))
        (when action
          (maybe-execute-action action))
        (check-true (unbox distilled?)))
      ;; Expand-context from budget warning
      (define expanded?2 (box #f))
      (parameterize ([current-rollback-action-execution? #t]
                     [current-rollback-action-log '()]
                     [current-force-distill-fn (lambda (_action) '())]
                     [current-expand-context-fn (lambda (_action)
                                                  (set-box! expanded?2 #t)
                                                  (list 'expand-context-executed))])
        (define actions (warnings->actions (list "excessive-savings-detected")))
        (define action (select-highest-priority-action actions))
        (when action
          (maybe-execute-action action))
        (check-true (unbox expanded?2))))

    ;; M5: WS evolution produces valid conclusions
    (test-case "WS evolution on state transition resets working set"
      (define ws (make-working-set))
      (working-set-add! ws "src/main.rkt" 0 100)
      (working-set-add! ws "tests/test-main.rkt" 0 100)
      (check-equal? (length (working-set-entries ws)) 2)
      ;; Exploration → planning should reset WS
      (define conclusions
        (list (task-conclusion "tc1" "plan: use X" 'decision 'planning '() 1000 '() '())))
      (define result (evolve-working-set-for-state ws task-exploration task-planning conclusions))
      (check-equal? (length (working-set-entries ws)) 0)
      (check-true (pair? result)))

    ;; M5: Auto-distill returns empty when no conclusions needed
    (test-case "auto-distill with no conclusions returns empty"
      (parameterize ([current-auto-distillation-enabled? #t])
        (define result (auto-distill '() '() 'exploration))
        (check-equal? result '())))

    ;; v0.78.0 AC2: Real pipeline with build-tiered-context/state-aware + flags ON
    (test-case "full pipeline produces valid tiered context with flags ON"
      (define conclusions
        (list
         (task-conclusion "c1" "found bug in parser" 'fact 'debugging '() 1000 '() '())
         (task-conclusion "c2" "fix: add null check" 'decision 'implementation '() 2000 '() '())))
      (define msgs
        (for/list ([i (in-range 8)])
          (test-msg (if (even? i) 'user 'assistant) (format "msg-~a" i))))
      (parameterize ([current-task-state-aware-assembly? #t]
                     [current-conclusion-token-budget 2000]
                     [current-rollback-action-execution? #t])
        (define tc
          (build-tiered-context/state-aware msgs
                                            #:task-state 'implementation
                                            #:working-set-messages
                                            (list (car msgs) (cadr msgs) (caddr msgs))
                                            #:conclusions conclusions))
        (check-not-false tc)
        ;; v0.78.6 W3: Verify preamble was injected (state-aware adds system-instruction)
        (define tier-a (tiered-context-tier-a tc))
        (check-true (> (length tier-a) 0) "tier-a should not be empty")
        ;; Preamble should have role=system, kind=system-instruction
        (check-true (for/or ([m (in-list tier-a)])
                      (and (eq? (message-role m) 'system) (eq? (message-kind m) 'system-instruction)))
                    "tier-a should contain system-instruction preamble")))

    ;; v0.78.0 AW1: Graph-through-builder with current-graph-conclusion-selection? #t
    (test-case "graph selection integrates with builder when flag ON"
      (define conclusions
        (list (task-conclusion "gc1" "use module A" 'decision 'implementation '() 1000 '() '())
              (task-conclusion "gc2" "A requires B" 'dependency 'implementation '() 2000 '() '("gc1"))
              (task-conclusion "gc3" "unrelated note" 'fact 'exploration '() 3000 '() '())))
      (define msgs
        (for/list ([i (in-range 6)])
          (test-msg (if (even? i) 'user 'assistant) (format "msg-~a" i))))
      (parameterize ([current-task-state-aware-assembly? #t]
                     [current-graph-conclusion-selection? #t]
                     [current-conclusion-token-budget 2000])
        (define tc
          (build-tiered-context/state-aware msgs
                                            #:task-state 'implementation
                                            #:working-set-messages msgs
                                            #:conclusions conclusions))
        (check-not-false tc)
        ;; v0.78.6 W3: Verify graph selection ran — context should have tier-a entries
        ;; The key validation is that the pipeline didn't crash with graph selection ON.
        ;; Graph selection filters conclusions by reachability from seeds, so gc3
        ;; (unrelated exploration) is a candidate for filtering.
        (define tier-a (tiered-context-tier-a tc))
        (check-true (> (length tier-a) 0) "tier-a should have entries after graph selection")
        (check-true (<= (length tier-a) 20) "graph selection should produce reasonable tier-a size")))

    ;; v0.78.1 G1: Profile activation matrix — each profile is strict superset
    (test-case "profile activation: full > self-healing > bounded > observe > off"
      (apply-context-assembly-profile! 'off)
      (check-false (current-task-state-aware-assembly?))
      (apply-context-assembly-profile! 'observe)
      (check-true (current-task-state-aware-assembly?))
      (apply-context-assembly-profile! 'bounded)
      (check-true (current-task-state-aware-assembly?))
      (check-true (> (current-conclusion-token-budget) 0))
      (apply-context-assembly-profile! 'self-healing)
      (check-true (current-auto-distillation-enabled?))
      (check-true (current-rollback-action-execution?))
      (apply-context-assembly-profile! 'full)
      (check-true (current-graph-conclusion-selection?))
      (check-true (current-ws-evolution-enabled?))
      ;; Restore
      (apply-context-assembly-profile! 'off))

    ;; v0.78.2 G5: WS evolution merges conclusions by ID (union, not replace)
    (test-case "WS evolution merges existing conclusions with new"
      (define ws (make-working-set))
      (working-set-add! ws "file1.rkt" 0 50)
      (define existing-conclusions
        (list (task-conclusion "old-1" "old finding" 'fact 'exploration '() 1000 '() '())))
      (define new-conclusions
        (list (task-conclusion "old-1" "old finding" 'fact 'exploration '() 1000 '() '())
              (task-conclusion "new-1" "new finding" 'decision 'implementation '() 2000 '() '())))
      (define result (evolve-working-set-for-state ws task-idle task-implementation new-conclusions))
      (check-true (pair? result))
      ;; Result should include both old and new conclusions (union by ID)
      (define ids (map task-conclusion-id result))
      (check-not-false (member "old-1" ids))
      (check-not-false (member "new-1" ids)))

    ;; v0.78.3 G6+G7+G8: Builder consistency — budgeted conclusions used throughout
    (test-case "builder preamble uses budgeted conclusions not raw"
      (define many-conclusions
        (for/list ([i (in-range 20)])
          (task-conclusion (format "bc-~a" i)
                           (format "conclusion ~a" i)
                           'fact
                           'implementation
                           '()
                           (* i 100)
                           '()
                           '())))
      (define msgs
        (for/list ([i (in-range 4)])
          (test-msg (if (even? i) 'user 'assistant) (format "msg-~a" i))))
      (parameterize ([current-task-state-aware-assembly? #t]
                     [current-conclusion-token-budget 500])
        (define tc
          (build-tiered-context/state-aware msgs
                                            #:task-state 'implementation
                                            #:working-set-messages msgs
                                            #:conclusions many-conclusions))
        (check-not-false tc)
        ;; v0.78.6 W3: Verify budget was enforced — not all 20 conclusions should appear
        (define tier-a (tiered-context-tier-a tc))
        (define conclusion-count
          (for/sum ([m (in-list tier-a)] #:when (message-content m))
                   (for/sum ([p (in-list (message-content m))])
                            (if (regexp-match? #rx"conclusion" (format "~a" p)) 1 0))))
        (check-true (< conclusion-count 20) "budget should reduce conclusion count below raw 20")))

    ;; v0.78.4 G10: FSM workflow instructions in preamble
    (test-case "preamble includes FSM workflow guidance"
      (apply-context-assembly-profile! 'full)
      (define msgs
        (for/list ([i (in-range 4)])
          (test-msg (if (even? i) 'user 'assistant) (format "msg-~a" i))))
      (define conclusions
        (list (task-conclusion "fw-1" "found X" 'fact 'exploration '() 1000 '() '())))
      (parameterize ([current-task-state-aware-assembly? #t])
        (define tc
          (build-tiered-context/state-aware msgs
                                            #:task-state 'exploration
                                            #:working-set-messages msgs
                                            #:conclusions conclusions))
        (check-not-false tc)
        ;; v0.78.6 C5: Verify preamble text contains FSM guidance keywords
        (define tier-a (tiered-context-tier-a tc))
        (define preamble-text
          (string-join (for*/list ([m (in-list tier-a)]
                                   #:when (message-content m)
                                   [p (in-list (message-content m))])
                         (format "~a" p))))
        (check-true (> (string-length preamble-text) 0) "preamble should not be empty")
        (check-not-false (regexp-match? #rx"record_conclusion" preamble-text)
                         "preamble should mention record_conclusion tool")
        (check-not-false (regexp-match? #rx"set_task_state" preamble-text)
                         "preamble should mention set_task_state tool"))
      (apply-context-assembly-profile! 'off))

    ;; v0.78.4 G4: Rollout gate — profile bypass enables assembly
    (test-case "rollout gate: profile bypass overrides rate=0"
      (apply-context-assembly-profile! 'off)
      (define result-off (session-rollout-enabled? "test-session-id"))
      (check-false result-off)
      (apply-context-assembly-profile! 'bounded)
      (define result-bounded (session-rollout-enabled? "test-session-id"))
      (check-true result-bounded)
      (apply-context-assembly-profile! 'off))))

(run-tests suite)
