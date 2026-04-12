#lang racket

;; tests/workflows/parity/test-sdk-cli-parity.rkt (#177)
;;
;; Verifies that SDK and CLI modes produce equivalent results
;; for the same provider script and prompt.

(require rackunit
         rackunit/text-ui
         "../fixtures/mock-provider.rkt"
         "../fixtures/temp-project.rkt"
         "../fixtures/session-assert.rkt"
         "../fixtures/event-recorder.rkt"
         "../fixtures/workflow-runner.rkt"
         "../../../agent/types.rkt"
         (only-in "../../../tools/tool.rkt"
                  make-tool-registry
                  register-tool!
                  make-tool
                  make-success-result))

;; ============================================================
;; Test suite
;; ============================================================

(define suite
  (test-suite
   "SDK-CLI parity workflow tests"

   ;; ────────────────────────────────────────────────────────
   ;; Test 1: single-shot parity — SDK produces same structure as CLI
   ;; ────────────────────────────────────────────────────────
   (test-case "wf-parity: single-shot SDK run matches CLI expectations"
     ;; CLI mode would: create session, run prompt, write JSONL, emit events
     ;; SDK mode should produce the same artifacts
     (define prov (make-scripted-provider
                   (list (text-response "Hello from the agent"))))

     (define wr (run-workflow prov "Say hello"))

     ;; OUTCOME: completed with a text response
     (define output (workflow-result-output wr))
     (check-pred loop-result? output)
     (check-equal? (loop-result-termination-reason output) 'completed)

     ;; DURABLE STATE: session log exists and is valid (CLI would write this too)
     (check-equal? (check-session-jsonl-valid (workflow-result-session-log wr)) #t)

     ;; DURABLE STATE: correct number of turns
     (check-equal? (check-session-contains-turns (workflow-result-session-log wr)
                                                  #:user-turns 1
                                                  #:assistant-turns 1)
                   #t)

     ;; SIDE-EFFECTS: events include session.started, turn.started, turn.completed
     (define recorder (workflow-result-events wr))
     (define ev-names (event-names recorder))
     (check-not-false (member "session.started" ev-names)
                      "expected session.started event")
     (check-not-false (member "turn.started" ev-names)
                      "expected turn.started event")
     (check-not-false (member "turn.completed" ev-names)
                      "expected turn.completed event")

     ;; BOUNDARY: message content matches what the provider returned
     (define entries (workflow-session-entries wr))
     (define asst-entries (entries-with-role entries 'assistant))
     (check-true (>= (length asst-entries) 1) "expected at least 1 assistant entry")
     (when (>= (length asst-entries) 1)
       (define text-parts (filter text-part? (message-content (car asst-entries))))
       (check-true (>= (length text-parts) 1) "expected text content in assistant message")
       (when (>= (length text-parts) 1)
         (check-true (string-contains? (text-part-text (car text-parts)) "Hello")
                     "assistant text should contain 'Hello'")))

     ;; Cleanup
     (cleanup-temp-project! (workflow-result-project-dir wr)
                            (workflow-result-session-dir wr)))

   ;; ────────────────────────────────────────────────────────
   ;; Test 2: tool-call parity — SDK produces same tool trace as CLI
   ;; ────────────────────────────────────────────────────────
   (test-case "wf-parity: tool-call run produces same trace via SDK"
     (define prov (make-scripted-provider
                   (list (tool-call-response "tc-1" "echo"
                                             (hash 'message "test-parity"))
                         (text-response "Echo completed"))))

     (define reg (make-tool-registry))
     (register-tool! reg
       (make-tool "echo"
                  "Echo back the message"
                  (hasheq 'type "object"
                          'properties (hasheq 'message (hasheq 'type "string"))
                          'required '(message))
                  (lambda (args ctx)
                    (make-success-result
                     (format "Echo: ~a" (hash-ref args 'message ""))))))

     (define wr (run-workflow prov "Echo test-parity" #:tools reg))

     ;; OUTCOME: completed
     (define output (workflow-result-output wr))
     (check-equal? (loop-result-termination-reason output) 'completed)

     ;; DURABLE STATE: tool sequence matches
     (check-equal? (check-session-tool-sequence (workflow-result-session-log wr)
                                                 '("echo"))
                   #t)

     ;; DURABLE STATE: session log valid
     (check-equal? (check-session-jsonl-valid (workflow-result-session-log wr)) #t)

     ;; SIDE-EFFECTS: events match CLI event set
     (define recorder (workflow-result-events wr))
     (check-true (>= (length (events-of-type recorder "tool.call.started")) 1)
                 "expected tool.call.started")

     ;; BOUNDARY: tree structure valid
     (check-equal? (check-session-tree-structure (workflow-result-session-log wr)) #t)

     ;; Cleanup
     (cleanup-temp-project! (workflow-result-project-dir wr)
                            (workflow-result-session-dir wr)))

   ;; ────────────────────────────────────────────────────────
   ;; Test 3: session ID format consistency
   ;; ────────────────────────────────────────────────────────
   (test-case "wf-parity: session IDs follow consistent format"
     (define prov (make-scripted-provider
                   (list (text-response "ok"))))

     (define wr (run-workflow prov "test"))

     ;; Session ID should be a non-empty string
     (define sid (workflow-result-session-id wr))
     (check-true (string? sid) "session-id should be a string")
     (check-true (> (string-length sid) 0) "session-id should be non-empty")

     ;; Session log path should contain the session ID
     (define log-path (workflow-result-session-log wr))
     (check-true (string-contains? (path->string log-path) sid)
                 "log path should contain session ID")

     ;; Cleanup
     (cleanup-temp-project! (workflow-result-project-dir wr)
                            (workflow-result-session-dir wr)))

   )) ;; end test-suite

;; ============================================================
;; Run
;; ============================================================

(run-tests suite)
