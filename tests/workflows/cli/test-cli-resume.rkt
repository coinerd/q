#lang racket

;; tests/workflows/cli/test-cli-resume.rkt — CLI session resume workflow tests (#169)
;;
;; Tests that multi-turn and resumed sessions preserve history correctly.

(require rackunit
         rackunit/text-ui
         "../fixtures/mock-provider.rkt"
         "../fixtures/temp-project.rkt"
         "../fixtures/session-assert.rkt"
         "../fixtures/event-recorder.rkt"
         "../fixtures/workflow-runner.rkt"
         (only-in "../../../tools/tool.rkt" make-tool-registry)
         "../../../agent/types.rkt"
         (prefix-in sdk: "../../../interfaces/sdk.rkt")
         "../../../agent/event-bus.rkt")

;; ============================================================
;; Test suite
;; ============================================================

(define suite
  (test-suite
   "CLI session resume workflow tests"

   ;; ────────────────────────────────────────────────────────
   ;; Test 1: multi-turn preserves history
   ;; ────────────────────────────────────────────────────────
   (test-case "wf-resume: multi-turn preserves history with 2 prompts"
     ;; Provider returns text for each of 2 turns
     (define prov (make-scripted-provider
                   (list (text-response "First answer")
                         (text-response "Second answer"))))
     (define wr (run-workflow-multi-turn prov
                                          (list "First question"
                                                "Second question")))

     ;; OUTCOME: session log has 2 user turns and 2 assistant turns
     (check-equal? (check-session-contains-turns (workflow-result-session-log wr)
                                                  #:user-turns 2
                                                  #:assistant-turns 2)
                   #t)

     ;; DURABLE STATE: session log is valid with monotonic timestamps
     (check-equal? (check-session-jsonl-valid (workflow-result-session-log wr)) #t)

     ;; SIDE-EFFECTS: events include session.started and session.updated
     (define recorder (workflow-result-events wr))
     (check-true (>= (length (events-of-type recorder "session.started")) 1)
                 "expected session.started event")
     (check-true (>= (length (events-of-type recorder "session.updated")) 1)
                 "expected session.updated event")

     ;; BOUNDARY: tree structure is valid (second user message parents to first assistant)
     (check-equal? (check-session-tree-structure (workflow-result-session-log wr)) #t)

     ;; Verify message chaining — second user message parent is the first assistant message
     (define entries (workflow-session-entries wr))
     (check-equal? (length entries) 4) ; user, assistant, user, assistant
     (define second-user-msg (third entries))
     (define first-asst-msg (second entries))
     (check-equal? (message-parent-id second-user-msg)
                   (message-id first-asst-msg)
                   "second user message should parent to first assistant message")

     ;; Cleanup
     (cleanup-temp-project! (workflow-result-project-dir wr)
                            (workflow-result-session-dir wr)))

   ;; ────────────────────────────────────────────────────────
   ;; Test 2: explicit session resume via SDK preserves full history
   ;; ────────────────────────────────────────────────────────
   (test-case "wf-resume: explicit session resume preserves full history"
     ;; Phase 1: Create session, run first prompt
     (define session-dir (make-temp-session-dir))
     (define bus (make-event-bus))
     (define recorder (make-event-recorder bus))

     (define prov1 (make-scripted-provider
                    (list (text-response "Paris"))))
     (define reg (make-tool-registry))
     (define rt1 (sdk:make-runtime #:provider prov1
                                    #:session-dir session-dir
                                    #:tool-registry reg
                                    #:event-bus bus))
     (define rt2 (sdk:open-session rt1))
     (define sid (hash-ref (sdk:session-info rt2) 'session-id))
     (define-values (rt3 _result1) (sdk:run-prompt! rt2 "What is the capital of France?"))

     ;; Phase 2: Create new runtime with same session-dir, resume session
     (define bus2 (make-event-bus))
     (define prov2 (make-scripted-provider
                    (list (text-response "Berlin"))))
     (define rt4 (sdk:make-runtime #:provider prov2
                                    #:session-dir session-dir
                                    #:tool-registry reg
                                    #:event-bus bus2))
     (define rt5 (sdk:open-session rt4 sid))
     (define-values (rt6 _result2) (sdk:run-prompt! rt5 "What is the capital of Germany?"))

     ;; OUTCOME: full history is present (2 user + 2 assistant = 4 messages)
     (define log-path (build-path session-dir sid "session.jsonl"))
     (check-equal? (check-session-contains-turns log-path
                                                  #:user-turns 2
                                                  #:assistant-turns 2)
                   #t)

     ;; DURABLE STATE: session log is valid
     (check-equal? (check-session-jsonl-valid log-path) #t)

     ;; SIDE-EFFECTS: second runtime's session-info has correct history
     (define info (sdk:session-info rt6))
     (check-pred hash? info)
     (check-equal? (hash-ref info 'history-length #f) 4)

     ;; BOUNDARY: tree structure is valid
     (check-equal? (check-session-tree-structure log-path) #t)

     ;; Cleanup
     (cleanup-temp-project! #f session-dir))

   ;; ────────────────────────────────────────────────────────
   ;; Test 3: resumed session has session.resumed event
   ;; ────────────────────────────────────────────────────────
   (test-case "wf-resume: resumed session emits session.resumed event"
     (define session-dir (make-temp-session-dir))

     ;; Phase 1: create and run
     (define bus1 (make-event-bus))
     (define prov1 (make-scripted-provider (list (text-response "one"))))
     (define reg (make-tool-registry))
     (define rt1 (sdk:make-runtime #:provider prov1
                                    #:session-dir session-dir
                                    #:tool-registry reg
                                    #:event-bus bus1))
     (define rt2 (sdk:open-session rt1))
     (define sid (hash-ref (sdk:session-info rt2) 'session-id))
     (define-values (rt3 _) (sdk:run-prompt! rt2 "question 1"))

     ;; Phase 2: resume with a recorder to capture events
     (define bus2 (make-event-bus))
     (define recorder2 (make-event-recorder bus2))
     (define prov2 (make-scripted-provider (list (text-response "two"))))
     (define rt4 (sdk:make-runtime #:provider prov2
                                    #:session-dir session-dir
                                    #:tool-registry reg
                                    #:event-bus bus2))
     (define rt5 (sdk:open-session rt4 sid))

     ;; OUTCOME: session.resumed event was emitted on the second bus
     (check-true (>= (length (events-of-type recorder2 "session.resumed")) 1)
                 "expected session.resumed event on resumed session")

     ;; Cleanup
     (cleanup-temp-project! #f session-dir))

   ;; ────────────────────────────────────────────────────────
   ;; Test 4: multi-turn event sequence is correct
   ;; ────────────────────────────────────────────────────────
   (test-case "wf-resume: multi-turn event sequence has two turn cycles"
     (define prov (make-scripted-provider
                   (list (text-response "alpha")
                         (text-response "beta"))))
     (define wr (run-workflow-multi-turn prov
                                          (list "prompt 1"
                                                "prompt 2")))

     ;; SIDE-EFFECTS: two turn.started and two turn.completed events
     (define recorder (workflow-result-events wr))
     (define turn-started (events-of-type recorder "turn.started"))
     (define turn-completed (events-of-type recorder "turn.completed"))
     (check-equal? (length turn-started) 2
                   "expected 2 turn.started events in 2-turn workflow")
     (check-equal? (length turn-completed) 2
                   "expected 2 turn.completed events in 2-turn workflow")

     ;; BOUNDARY: at least one session.updated event
     (check-true (>= (length (events-of-type recorder "session.updated")) 1)
                 "expected session.updated event")

     ;; Cleanup
     (cleanup-temp-project! (workflow-result-project-dir wr)
                            (workflow-result-session-dir wr)))

   )) ;; end test-suite

;; ============================================================
;; Run
;; ============================================================

(run-tests suite)
