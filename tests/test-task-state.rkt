#lang racket/base

;; @speed fast
;; @suite default

;; tests/test-task-state.rkt — tests for task-state FSM, conclusion types, and task memory
;; v0.75.0: Foundation milestone tests.

(require racket/list
         rackunit
         rackunit/text-ui
         "../runtime/context-assembly/task-state.rkt"
         "../runtime/context-assembly/task-conclusion.rkt"
         "../runtime/context-assembly/task-memory.rkt"
         "../runtime/session/session-mutation.rkt"
         "../runtime/session/session-types.rkt"
         "../tests/helpers/session-fixture.rkt"
         (only-in "../util/fsm/fsm.rkt" fsm-state-name))

;; ── FSM Tests ──

(define suite
  (test-suite "task-state-fsm"

    ;; ── State singleton identity ──

    (test-case "task-state predicates recognize all 6 states"
      (check-true (task-state? task-idle))
      (check-true (task-state? task-exploration))
      (check-true (task-state? task-planning))
      (check-true (task-state? task-implementation))
      (check-true (task-state? task-verification))
      (check-true (task-state? task-debugging)))

    (test-case "task-state rejects non-FSM values"
      (check-false (task-state? 'idle))
      (check-false (task-state? 42))
      (check-false (task-state? "exploration")))

    ;; ── Transition validity (12 core + 6 revisit + 5 task-complete + force = all) ──

    (test-case "idle → exploration is valid via begin-explore"
      (check-true (task-valid-transition? task-idle task-begin-explore)))

    (test-case "exploration → planning is valid via begin-plan"
      (check-true (task-valid-transition? task-exploration task-begin-plan)))

    (test-case "exploration → implementation is valid via begin-implement"
      (check-true (task-valid-transition? task-exploration task-begin-implement)))

    (test-case "planning → implementation is valid via begin-implement"
      (check-true (task-valid-transition? task-planning task-begin-implement)))

    (test-case "implementation → verification is valid via begin-verify"
      (check-true (task-valid-transition? task-implementation task-begin-verify)))

    (test-case "implementation → debugging is valid via begin-debug"
      (check-true (task-valid-transition? task-implementation task-begin-debug)))

    (test-case "verification → idle is valid via task-complete"
      (check-true (task-valid-transition? task-verification task-task-complete)))

    (test-case "verification → debugging is valid via begin-debug"
      (check-true (task-valid-transition? task-verification task-begin-debug)))

    (test-case "debugging → implementation is valid via begin-implement"
      (check-true (task-valid-transition? task-debugging task-begin-implement)))

    (test-case "debugging → verification is valid via begin-verify"
      (check-true (task-valid-transition? task-debugging task-begin-verify)))

    ;; ── Invalid transitions ──

    (test-case "idle → planning is invalid (must go through exploration)"
      (check-false (task-valid-transition? task-idle task-begin-plan)))

    (test-case "idle → implementation is invalid"
      (check-false (task-valid-transition? task-idle task-begin-implement)))

    (test-case "idle → debugging is invalid"
      (check-false (task-valid-transition? task-idle task-begin-debug)))

    (test-case "exploration → idle is invalid via begin-explore"
      (check-false (task-valid-transition? task-exploration task-begin-explore)))

    ;; ── Revisit (any → exploration) ──

    (test-case "revisit works from all states"
      (for ([state (in-list (list task-idle
                                  task-exploration
                                  task-planning
                                  task-implementation
                                  task-verification
                                  task-debugging))])
        (check-true (task-valid-transition? state task-revisit)
                    (format "revisit should work from ~a" (fsm-state-name state)))))

    ;; ── task-complete (any → idle) ──

    (test-case "task-complete works from all non-idle states"
      (for ([state (in-list (list task-exploration
                                  task-planning
                                  task-implementation
                                  task-verification
                                  task-debugging))])
        (check-true (task-valid-transition? state task-task-complete)
                    (format "task-complete should work from ~a" (fsm-state-name state)))))

    ;; ── force-transition escape hatch ──

    (test-case "force-transition works from any state to any state"
      (for ([from (in-list (list task-idle
                                 task-exploration
                                 task-planning
                                 task-implementation
                                 task-verification
                                 task-debugging))])
        (for ([to-name (in-list '(idle exploration planning implementation verification debugging))])
          (check-true (task-valid-transition? from task-force-transition)
                      (format "force-transition from ~a" (fsm-state-name from))))))

    ;; ── next-state ──

    (test-case "task-next-state returns correct state"
      (check-equal? (fsm-state-name (task-next-state task-idle task-begin-explore)) 'exploration)
      (check-equal? (fsm-state-name (task-next-state task-exploration task-begin-plan)) 'planning)
      (check-equal? (fsm-state-name (task-next-state task-implementation task-begin-verify))
                    'verification)
      (check-equal? (fsm-state-name (task-next-state task-debugging task-begin-implement))
                    'implementation))

    (test-case "task-next-state returns #f for invalid transition"
      (check-false (task-next-state task-idle task-begin-plan)))

    ;; ── Metadata ──

    (test-case "task-states-list returns 6 states"
      (check-equal? (length (task-states-list)) 6))

    (test-case "task-events-list returns 8 events"
      (check-equal? (length (task-events-list)) 8))))

(run-tests suite)

;; ── Conclusion + Memory Tests ──

(define conclusion-suite
  (test-suite "task-conclusion+memory"

    ;; ── Conclusion creation ──

    (test-case "task-conclusion creation with all fields"
      (define c
        (task-conclusion "c1"
                         "Found the bug"
                         'fact
                         'exploration
                         '("m1" "m2")
                         1000
                         '(bug pattern)
                         '()))
      (check-equal? (task-conclusion-id c) "c1")
      (check-equal? (task-conclusion-text c) "Found the bug")
      (check-equal? (task-conclusion-category c) 'fact)
      (check-equal? (task-conclusion-fsm-state-origin c) 'exploration)
      (check-equal? (task-conclusion-origin-message-ids c) '("m1" "m2"))
      (check-equal? (task-conclusion-timestamp c) 1000)
      (check-equal? (task-conclusion-relevance-tags c) '(bug pattern)))

    (test-case "task-conclusion-category? accepts valid categories"
      (for ([cat (in-list '(fact decision pattern error-cause test-result))])
        (check-true (task-conclusion-category? cat) (format "~a should be valid" cat))))

    (test-case "task-conclusion-category? rejects invalid categories"
      (check-false (task-conclusion-category? 'invalid))
      (check-false (task-conclusion-category? 42)))

    ;; ── Serialization round-trip ──

    (test-case "conclusion->hash / hash->conclusion round-trip"
      (define c
        (task-conclusion "c2"
                         "Decided to use FSM"
                         'decision
                         'planning
                         '("m3")
                         2000
                         '(architecture)
                         '()))
      (define h (conclusion->hash c))
      (define c2 (hash->conclusion h))
      (check-equal? (task-conclusion-id c2) "c2")
      (check-equal? (task-conclusion-text c2) "Decided to use FSM")
      (check-equal? (task-conclusion-category c2) 'decision)
      (check-equal? (task-conclusion-fsm-state-origin c2) 'planning)
      (check-equal? (task-conclusion-origin-message-ids c2) '("m3"))
      (check-equal? (task-conclusion-timestamp c2) 2000)
      (check-equal? (task-conclusion-relevance-tags c2) '(architecture)))

    ;; ── Task memory ──

    (test-case "make-task-memory defaults to 50 max"
      (define m (make-task-memory))
      (check-equal? (task-memory-max-conclusions m) 50)
      (check-equal? (task-memory-conclusions m) '()))

    (test-case "add-conclusion appends and returns new store"
      (define m (make-task-memory))
      (define c1 (task-conclusion "c1" "text1" 'fact 'exploration '() 1000 '() '()))
      (define c2 (task-conclusion "c2" "text2" 'decision 'planning '() 2000 '() '()))
      (define m1 (add-conclusion m c1))
      (check-equal? (length (task-memory-conclusions m1)) 1)
      (define m2 (add-conclusion m1 c2))
      (check-equal? (length (task-memory-conclusions m2)) 2)
      ;; Original unchanged (functional)
      (check-equal? (length (task-memory-conclusions m)) 0))

    (test-case "conclusions-for-states filters correctly"
      (define c1 (task-conclusion "c1" "t1" 'fact 'exploration '() 1000 '() '()))
      (define c2 (task-conclusion "c2" "t2" 'decision 'planning '() 2000 '() '()))
      (define c3 (task-conclusion "c3" "t3" 'pattern 'implementation '() 3000 '() '()))
      (define m (add-conclusion (add-conclusion (add-conclusion (make-task-memory) c1) c2) c3))
      ;; Filter by exploration only
      (check-equal? (length (conclusions-for-states m '(exploration))) 1)
      ;; Filter by exploration + planning
      (check-equal? (length (conclusions-for-states m '(exploration planning))) 2)
      ;; Filter by idle (none match)
      (check-equal? (length (conclusions-for-states m '(idle))) 0))

    (test-case "conclusions-with-tags filters correctly"
      (define c1 (task-conclusion "c1" "t1" 'fact 'exploration '() 1000 '(bug) '()))
      (define c2 (task-conclusion "c2" "t2" 'decision 'planning '() 2000 '(architecture) '()))
      (define c3 (task-conclusion "c3" "t3" 'pattern 'exploration '() 3000 '(bug pattern) '()))
      (define m (add-conclusion (add-conclusion (add-conclusion (make-task-memory) c1) c2) c3))
      (check-equal? (length (conclusions-with-tags m '(bug))) 2)
      (check-equal? (length (conclusions-with-tags m '(architecture))) 1)
      (check-equal? (length (conclusions-with-tags m '(nonexistent))) 0))

    (test-case "evict-stale-conclusions respects max limit"
      ;; Create store with max 3, add 5 conclusions
      (define m (make-task-memory 3))
      (define conclusions
        (for/list ([i (in-range 5)])
          (task-conclusion (format "c~a" i)
                           (format "text~a" i)
                           'fact
                           (if (even? i) 'exploration 'planning)
                           '()
                           (* (add1 i) 1000)
                           '()
                           '())))
      (define filled
        (for/fold ([m m]) ([c conclusions])
          (add-conclusion m c)))
      ;; Should have 3 conclusions (evicted 2 oldest)
      (check-equal? (length (task-memory-conclusions filled)) 3)
      ;; The newest 3 should survive
      (check-equal? (task-conclusion-id (third (task-memory-conclusions filled))) "c4"))

    (test-case "task-memory serialization round-trip"
      (define m (make-task-memory 10))
      (define c (task-conclusion "c1" "test" 'fact 'exploration '("m1") 1000 '(tag) '()))
      (define m1 (add-conclusion m c))
      (define h (task-memory->hash m1))
      (define m2 (hash->task-memory h))
      (check-equal? (task-memory-max-conclusions m2) 10)
      (check-equal? (length (task-memory-conclusions m2)) 1)
      (check-equal? (task-conclusion-id (first (task-memory-conclusions m2))) "c1"))))

(run-tests conclusion-suite)

;; ── Session Mutation Tests ──

(define mutation-suite
  (test-suite "session-mutation"

    (test-case "guarded-set-task-fsm-state! allows initialization from idle"
      (define sess (make-test-session))
      (guarded-set-task-fsm-state! sess 'exploration)
      (check-equal? (agent-session-task-fsm-state sess) 'exploration))

    (test-case "guarded-set-task-fsm-state! allows valid forward transition"
      (define sess (make-test-session))
      (guarded-set-task-fsm-state! sess 'exploration)
      (guarded-set-task-fsm-state! sess 'planning)
      (check-equal? (agent-session-task-fsm-state sess) 'planning))

    (test-case "guarded-set-task-fsm-state! allows transition to idle (reset)"
      (define sess (make-test-session))
      (guarded-set-task-fsm-state! sess 'implementation)
      (guarded-set-task-fsm-state! sess 'idle)
      (check-equal? (agent-session-task-fsm-state sess) 'idle))

    (test-case "guarded-set-task-fsm-state! rejects invalid transition"
      (define sess (make-test-session))
      (guarded-set-task-fsm-state! sess 'exploration)
      (check-exn exn:fail? (lambda () (guarded-set-task-fsm-state! sess 'verification))))

    (test-case "guarded-set-task-fsm-state! rejects unknown state"
      (define sess (make-test-session))
      (check-exn exn:fail? (lambda () (guarded-set-task-fsm-state! sess 'unknown-state))))

    (test-case "guarded-set-task-fsm-state! allows any state from #f (uninitialized)"
      (define sess (make-test-session))
      ;; task-fsm-state defaults to #f for new sessions
      (guarded-set-task-fsm-state! sess 'debugging)
      (check-equal? (agent-session-task-fsm-state sess) 'debugging))

    (test-case "guarded-set-recent-tool-calls! validates input"
      (define sess (make-test-session))
      (guarded-set-recent-tool-calls! sess '("read" "edit" "bash"))
      (check-equal? (length (agent-session-recent-tool-calls sess)) 3)
      (check-exn exn:fail? (lambda () (guarded-set-recent-tool-calls! sess '(read 42)))))))

(run-tests mutation-suite)
