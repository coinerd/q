#lang racket

;; tests/workflows/session/test-session-fork.rkt — Session fork workflow test (#174)
;;
;; Verifies that session fork creates a clean branch without mutating
;; the source session. Tests durable state (logs), side-effects (events),
;; and boundary conditions (tree structure).

(require rackunit
         rackunit/text-ui
         (prefix-in sdk: "../../../interfaces/sdk.rkt")
         "../../../agent/event-bus.rkt"
         "../../../agent/types.rkt"
         (only-in "../../../tools/tool.rkt" make-tool-registry)
         "../fixtures/mock-provider.rkt"
         "../fixtures/temp-project.rkt"
         "../fixtures/session-assert.rkt"
         "../fixtures/event-recorder.rkt"
         "../fixtures/workflow-runner.rkt")

;; ============================================================
;; Test suite
;; ============================================================

(define session-fork-tests
  (test-suite
   "Session Fork Workflow"

   ;; ── Outcome: fork creates independent branch ──
   (test-case "fork creates clean branch from source session"

     ;; Provider script: 3 text responses for 3 prompts
     (define provider
       (make-scripted-provider
        (list (text-response "First response")
              (text-response "Second response")
              (text-response "Third response on fork"))))

     (define-values (project-dir session-dir)
       (make-temp-project '()))

     (with-handlers ([exn:fail? (lambda (e)
                                  (cleanup-temp-project! project-dir session-dir)
                                  (raise e))])
       (define reg (make-tool-registry))
       (define bus (make-event-bus))
       (define recorder (make-event-recorder bus))

       ;; 1. Create runtime and open session
       (define rt
         (sdk:make-runtime #:provider provider
                           #:session-dir session-dir
                           #:tool-registry reg
                           #:event-bus bus
                           #:max-iterations 10))
       (define rt-open (sdk:open-session rt))
       (define sid (hash-ref (sdk:session-info rt-open) 'session-id))

       ;; 2. Run 2 prompts (multi-turn)
       (define-values (rt-after-1 _result-1) (sdk:run-prompt! rt-open "Prompt one"))
       (define-values (rt-after-2 _result-2) (sdk:run-prompt! rt-after-1 "Prompt two"))

       ;; 3. Capture source log state before fork
       (define source-log-path (build-path session-dir sid "session.jsonl"))
       (define source-entries-before (session-log-entries source-log-path))
       (define source-entry-count-before (length source-entries-before))

       ;; Verify we have 2 user + 2 assistant entries before fork
       (check-equal? (length (entries-with-role source-entries-before 'user)) 2
                     "Source should have 2 user entries before fork")
       (check-equal? (length (entries-with-role source-entries-before 'assistant)) 2
                     "Source should have 2 assistant entries before fork")

       ;; 4. Fork the session
       (define forked-rt (sdk:fork-session! rt-after-2))
       (check-pred (lambda (x) (not (eq? x 'no-active-session))) forked-rt
                   "Fork should return a runtime, not 'no-active-session")

       ;; 5. Verify forked session has a different session ID
       (define forked-info (sdk:session-info forked-rt))
       (check-not-false forked-info "Forked session should have info")
       (define forked-sid (hash-ref forked-info 'session-id))
       (check-not-equal? forked-sid sid
                         "Forked session must have a different session ID")

       ;; 6. Verify forked log has same initial entries
       (define forked-log-path (build-path session-dir forked-sid "session.jsonl"))
       (define forked-entries (session-log-entries forked-log-path))
       (check-equal? (length forked-entries) source-entry-count-before
                     "Forked session should inherit all source entries")

       ;; 7. Run a third prompt on the forked session
       (define-values (forked-after-3 _result-3) (sdk:run-prompt! forked-rt "Prompt three on fork"))

       ;; 8. Source session log is unchanged
       (define source-entries-after (session-log-entries source-log-path))
       (check-equal? (length source-entries-after) source-entry-count-before
                     "Source session log must be unchanged after fork + fork-prompt")

       ;; 9. Forked session has original entries + new prompt
       (define forked-entries-after (session-log-entries forked-log-path))
       (check > (length forked-entries-after) source-entry-count-before
              "Forked session should have more entries after new prompt")
       (check-equal? (length (entries-with-role forked-entries-after 'user)) 3
                     "Forked session should have 3 user entries")
       (check-equal? (length (entries-with-role forked-entries-after 'assistant)) 3
                     "Forked session should have 3 assistant entries")

       ;; 10. Session tree structure valid in both
       (check-eq? (check-session-tree-structure source-log-path) #t
                  "Source session tree structure should be valid")
       (check-eq? (check-session-tree-structure forked-log-path) #t
                  "Forked session tree structure should be valid")

       ;; 11. Both logs are valid JSONL
       (check-eq? (check-session-jsonl-valid source-log-path) #t
                  "Source session log should be valid JSONL")
       (check-eq? (check-session-jsonl-valid forked-log-path) #t
                  "Forked session log should be valid JSONL")

       ;; ── Side-effects: events ──
       (check-not-false (member "session.forked" (event-names recorder))
                        "Events should include session.forked")

       (cleanup-temp-project! project-dir session-dir)))

   ;; ── Boundary: source session remains usable after fork ──
   (test-case "source session remains usable after fork"

     (define provider
       (make-scripted-provider
        (list (text-response "First")
              (text-response "Second")
              (text-response "Third on source"))))

     (define-values (project-dir session-dir)
       (make-temp-project '()))

     (with-handlers ([exn:fail? (lambda (e)
                                  (cleanup-temp-project! project-dir session-dir)
                                  (raise e))])
       (define reg (make-tool-registry))
       (define bus (make-event-bus))

       (define rt
         (sdk:make-runtime #:provider provider
                           #:session-dir session-dir
                           #:tool-registry reg
                           #:event-bus bus
                           #:max-iterations 10))
       (define rt-open (sdk:open-session rt))
       (define sid (hash-ref (sdk:session-info rt-open) 'session-id))

       ;; Run a prompt, fork, then run another prompt on source
       (define-values (rt-1 _) (sdk:run-prompt! rt-open "Hello"))
       (define _forked (sdk:fork-session! rt-1))
       (define-values (rt-2 result-2) (sdk:run-prompt! rt-1 "After fork"))

       ;; Source should still work and have 3 user + 3 assistant entries
       (define source-log (build-path session-dir sid "session.jsonl"))
       (define entries (session-log-entries source-log))
       (check-equal? (length (entries-with-role entries 'user)) 2
                     "Source should have 2 user entries after fork + extra prompt")
       (check-equal? (length (entries-with-role entries 'assistant)) 2
                     "Source should have 2 assistant entries after fork + extra prompt")

       (cleanup-temp-project! project-dir session-dir)))))

;; ============================================================
;; Run
;; ============================================================

(module+ main
  (run-tests session-fork-tests))

(module+ test
  (run-tests session-fork-tests))
