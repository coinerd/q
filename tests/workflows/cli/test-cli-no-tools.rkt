#lang racket

;; tests/workflows/cli/test-cli-no-tools.rkt — CLI no-tools/restricted tools workflow tests (#170)
;;
;; Tests that tool-call responses degrade correctly when tools are
;; not registered, and that normal text works without any tools.

(require rackunit
         rackunit/text-ui
         "../fixtures/mock-provider.rkt"
         "../fixtures/temp-project.rkt"
         "../fixtures/session-assert.rkt"
         "../fixtures/event-recorder.rkt"
         "../fixtures/workflow-runner.rkt"
         "../../../util/protocol-types.rkt"
         (only-in "../../../tools/tool.rkt"
                  make-tool-registry
                  register-tool!
                  make-tool
                  tool-result?
                  tool-result-is-error?
                  tool-result-content
                  make-success-result
                  make-error-result))

;; ============================================================
;; Test suite
;; ============================================================

(define suite
  (test-suite
   "CLI no-tools / restricted tools workflow tests"

   ;; ────────────────────────────────────────────────────────
   ;; Test 1: provider returns tool-call, empty registry → error in loop
   ;; ────────────────────────────────────────────────────────
   (test-case "wf-no-tools: tool-call with empty registry produces error"
     ;; Provider returns a tool-call for a tool that is NOT registered
     (define prov (make-scripted-provider
                   (list (tool-call-response "tc-1" "bash"
                                             (hash 'command "ls")))))
     ;; Empty tool registry (default)
     (define wr (run-workflow prov "List the files"))

     ;; OUTCOME: loop terminates — either with 'completed (after tool-error)
     ;; or 'error, depending on iteration loop error handling.
     ;; The tool call fails to dispatch (unknown tool), which produces
     ;; an error result from the scheduler. The iteration loop continues
     ;; after a tool error, sending the error back to the model.
     ;; Since the provider only has 1 entry, it returns "done" for the
     ;; second call. So the final result should be 'completed.
     (define output (workflow-result-output wr))
     (check-pred loop-result? output)
     (define termination (loop-result-termination-reason output))
     (check-not-false (member termination '(completed error max-iterations-exceeded))
                       (format "expected completed/error/max-iterations, got ~a" termination))

     ;; SIDE-EFFECTS: at least one tool.call.failed event was emitted
     (define recorder (workflow-result-events wr))
     (define failed-events (events-of-type recorder "tool.call.failed"))
     (check-true (>= (length failed-events) 1)
                 (format "expected >=1 tool.call.failed, got ~a" (length failed-events)))

     ;; BOUNDARY: session log is still valid (error messages are recorded)
     (check-equal? (check-session-jsonl-valid (workflow-result-session-log wr)) #t)

     ;; Cleanup
     (cleanup-temp-project! (workflow-result-project-dir wr)
                            (workflow-result-session-dir wr)))

   ;; ────────────────────────────────────────────────────────
   ;; Test 2: plain text, no tools → completes normally
   ;; ────────────────────────────────────────────────────────
   (test-case "wf-no-tools: plain text with no tools completes normally"
     (define prov (make-scripted-provider
                   (list (text-response "No tools needed"))))
     ;; Empty tool registry (default)
     (define wr (run-workflow prov "Just answer"))

     ;; OUTCOME: completed normally
     (define output (workflow-result-output wr))
     (check-pred loop-result? output)
     (check-equal? (loop-result-termination-reason output) 'completed)

     ;; DURABLE STATE: session log has 1 user + 1 assistant
     (check-equal? (check-session-contains-turns (workflow-result-session-log wr)
                                                  #:user-turns 1
                                                  #:assistant-turns 1)
                   #t)

     ;; SIDE-EFFECTS: no tool events at all
     (define recorder (workflow-result-events wr))
     (define tool-events (events-of-type recorder "tool.call.started"))
     (check-equal? (length tool-events) 0
                   "expected zero tool.call.started events for text-only response")

     ;; BOUNDARY: tree structure valid
     (check-equal? (check-session-tree-structure (workflow-result-session-log wr)) #t)

     ;; Cleanup
     (cleanup-temp-project! (workflow-result-project-dir wr)
                            (workflow-result-session-dir wr)))

   ;; ────────────────────────────────────────────────────────
   ;; Test 3: provider returns tool-call for a registered tool → executes
   ;; ────────────────────────────────────────────────────────
   (test-case "wf-no-tools: tool-call for registered tool executes successfully"
     ;; Create a minimal test tool
     (define reg (make-tool-registry))
     ;; Note: JSON round-trip converts symbol keys to string keys,
     ;; so we avoid 'required in schema and look up string keys in the tool.
     (define echo-tool
       (make-tool "echo"
                  "Echo back the input"
                  (hasheq 'type "object"
                          'properties (hasheq "message" (hasheq 'type "string")))
                  (lambda (args ctx)
                    (define msg (or (hash-ref args "message" #f)
                                    (hash-ref args 'message "no message")))
                    (make-success-result
                     (list (hasheq 'type "text" 'text msg))))))
     (register-tool! reg echo-tool)

     ;; Provider: first returns tool-call, then returns text summary
     (define prov (make-scripted-provider
                   (list (tool-call-response "tc-1" "echo"
                                             (hash 'message "hello world"))
                         (text-response "The echo tool said: hello world"))))

     (define wr (run-workflow prov "Run the echo tool" #:tools reg))

     ;; OUTCOME: completed successfully
     (define output (workflow-result-output wr))
     (check-pred loop-result? output)
     (check-equal? (loop-result-termination-reason output) 'completed
                   "expected completed after successful tool execution + follow-up")

     ;; DURABLE STATE: session log has 1 user turn + 2 assistant turns
     ;; (first assistant with tool-call, second assistant with text summary)
     ;; plus 1 tool result message
     (check-equal? (check-session-jsonl-valid (workflow-result-session-log wr)) #t)
     (check-equal? (check-session-contains-turns (workflow-result-session-log wr)
                                                  #:user-turns 1
                                                  #:assistant-turns 2)
                   #t)

     ;; SIDE-EFFECTS: tool events were emitted
     (define recorder (workflow-result-events wr))
     (define tc-started (events-of-type recorder "tool.call.started"))
     ;; Tool either succeeds (tool.call.completed) or fails gracefully (tool.call.failed)
     (define tc-completed-or-failed
       (+ (length (events-of-type recorder "tool.call.completed"))
          (length (events-of-type recorder "tool.call.failed"))))
     (check-true (>= (length tc-started) 1)
                 "expected >=1 tool.call.started")
     (check-true (>= tc-completed-or-failed 1)
                 "expected >=1 tool.call.completed or tool.call.failed")

     ;; BOUNDARY: tool sequence in session log matches
     (check-equal? (check-session-tool-sequence (workflow-result-session-log wr)
                                                 '("echo"))
                   #t)

     ;; Cleanup
     (cleanup-temp-project! (workflow-result-project-dir wr)
                            (workflow-result-session-dir wr)))

   ;; ────────────────────────────────────────────────────────
   ;; Test 4: tool-call for unknown tool name → error result
   ;; ────────────────────────────────────────────────────────
   (test-case "wf-no-tools: tool-call for unknown tool produces error not crash"
     (define reg (make-tool-registry))
     ;; Register a DIFFERENT tool than what the provider will call
     (define dummy-tool
       (make-tool "read"
                  "Read a file"
                  (hasheq 'type "object"
                          'properties (hasheq 'path (hasheq 'type "string")))
                  (lambda (args ctx) (make-success-result "file contents"))))
     (register-tool! reg dummy-tool)

     ;; Provider calls "bash" which is NOT registered
     (define prov (make-scripted-provider
                   (list (tool-call-response "tc-1" "bash"
                                             (hash 'command "rm -rf /"))
                         (text-response "I couldn't run that command"))))
     (define wr (run-workflow prov "Delete everything" #:tools reg))

     ;; OUTCOME: loop completes (tool error is fed back to model)
     (define output (workflow-result-output wr))
     (check-pred loop-result? output)
     (define termination (loop-result-termination-reason output))
     (check-not-false (member termination '(completed error max-iterations-exceeded))
                       (format "expected completed/error/max-iterations, got ~a" termination))

     ;; SIDE-EFFECTS: tool.call.failed event for the unknown tool
     (define recorder (workflow-result-events wr))
     (define failed-events (events-of-type recorder "tool.call.failed"))
     (check-true (>= (length failed-events) 1)
                 "expected tool.call.failed for unknown tool")

     ;; BOUNDARY: session log remains valid despite the error
     (check-equal? (check-session-jsonl-valid (workflow-result-session-log wr)) #t)
     (check-equal? (check-session-tree-structure (workflow-result-session-log wr)) #t)

     ;; Cleanup
     (cleanup-temp-project! (workflow-result-project-dir wr)
                            (workflow-result-session-dir wr)))

   )) ;; end test-suite

;; ============================================================
;; Run
;; ============================================================

(run-tests suite)
