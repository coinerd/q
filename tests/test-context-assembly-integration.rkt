#lang racket/base

;; tests/test-context-assembly-integration.rkt — Full pipeline integration test
;; v0.77.10 M5: Exercises the complete context assembly pipeline with all flags ON.

(require rackunit
         rackunit/text-ui
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
                  current-task-state-aware-assembly?)
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
         (only-in "../runtime/session-events.rkt" current-ws-evolution-enabled?)
         (only-in "../runtime/session-config.rkt"
                  apply-context-assembly-profile!
                  current-context-assembly-profile)
         (only-in "../runtime/working-set.rkt" make-working-set working-set-add! working-set-entries)
         (only-in "../util/protocol-types.rkt" make-message make-text-part)
         (only-in "../util/message.rkt" message-id))

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
        (check-not-false tc)))

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
        (check-not-false tc)))))

(run-tests suite)
