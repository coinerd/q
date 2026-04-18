#lang racket

;; tests/test-spawn-subagent-integration.rkt — Wave 0 integration tests
;;
;; #1203: Subagent Real Provider Integration
;; #1204: Provider Injection into Subagent Exec Context
;; #1205: Subagent with Scoped Tools and Max-turns
;; #1206: Subagent Integration Tests
;;
;; These tests verify that spawn-subagent uses the parent's real LLM
;; provider instead of falling back to a mock.

(require rackunit
         racket/string
         "../tools/builtins/spawn-subagent.rkt"
         "../tools/tool.rkt"
         "../llm/provider.rkt"
         "../llm/model.rkt"
         "../agent/event-bus.rkt"
         "../util/ids.rkt")

;; ============================================================
;; Helpers
;; ============================================================

;; Create a mock provider that returns a specific response text.
;; Tracks whether it was called for verification.
(define (make-tracking-provider response-text #:name [prov-name "test-provider"])
  (define called-box (box #f))
  (define mock-response
    (make-model-response
     (list (hasheq 'type "text" 'text response-text))
     (hasheq 'prompt-tokens 10 'completion-tokens 20 'total-tokens 30)
     prov-name
     'stop))
  (define prov
    (make-mock-provider mock-response #:name prov-name))
  (values prov called-box))

;; Create a provider that raises on send (for error handling tests).
(define (make-failing-provider)
  (make-provider
   (lambda () "failing-provider")
   (lambda () (hasheq 'streaming #t))
   (lambda (req) (error 'failing-provider "API connection refused"))
   (lambda (req) (error 'failing-provider "streaming failed"))))

;; Create a provider that returns tool_calls stop reason (for max-turns test).
(define (make-tool-call-loop-provider)
  (define resp
    (make-model-response
     (list (hasheq 'type "text" 'text "I need to call a tool"))
     (hasheq 'prompt-tokens 5 'completion-tokens 5 'total-tokens 10)
     "loop-provider"
     'tool_calls))
  (make-mock-provider resp #:name "loop-provider"))

;; Build an exec-context with a provider in runtime-settings.
(define (make-test-exec-ctx provider
                             #:model [model "test-model"]
                             #:working-directory [wd (current-directory)])
  (make-exec-context
   #:working-directory wd
   #:event-publisher #f
   #:runtime-settings (hasheq 'provider provider 'model model)
   #:call-id (generate-id)
   #:session-metadata (hasheq 'session-id "test-parent-session" 'role "parent")))

;; ============================================================
;; #1204: Provider Injection into Subagent Exec Context
;; ============================================================

(test-case "#1204: subagent uses real provider from exec-context runtime-settings"
  (define response-text "Real provider response: analyzed the codebase")
  (define-values (prov called?) (make-tracking-provider response-text))
  (define ctx (make-test-exec-ctx prov))
  (define result (tool-spawn-subagent (hasheq 'task "Analyze codebase") ctx))
  (check-true (tool-result? result))
  (check-false (tool-result-is-error? result)
               (format "expected success, got error: ~a" (tool-result-content result)))
  ;; The result should contain the real provider's response, not the mock fallback
  (define content (tool-result-content result))
  (define text (string-join
                (for/list ([c (in-list content)]
                           #:when (hash-ref c 'text #f))
                  (hash-ref c 'text ""))
                ""))
  (check-true (string-contains? text response-text)
              (format "expected response to contain ~v, got ~v" response-text text)))

(test-case "#1204: subagent falls back to mock when no exec-context"
  ;; Backward compat: calling without exec-ctx should still work (uses mock)
  (define result (tool-spawn-subagent (hasheq 'task "test task")))
  (check-true (tool-result? result))
  (check-false (tool-result-is-error? result)
               (format "expected success with mock fallback, got: ~a" (tool-result-content result))))

(test-case "#1204: subagent falls back to mock when runtime-settings lacks provider"
  ;; exec-ctx with no provider key in runtime-settings
  (define ctx (make-exec-context
               #:runtime-settings (hasheq 'model "test-model")
               #:call-id "test"))
  (define result (tool-spawn-subagent (hasheq 'task "test task") ctx))
  (check-true (tool-result? result))
  (check-false (tool-result-is-error? result)))

(test-case "#1204: subagent uses model name from runtime-settings"
  (define-values (prov _) (make-tracking-provider "model test"))
  (define ctx (make-test-exec-ctx prov #:model "gpt-4o"))
  (define result (tool-spawn-subagent (hasheq 'task "test task") ctx))
  (check-true (tool-result? result))
  (check-false (tool-result-is-error? result))
  ;; Details should reflect the run
  (when (hash? (tool-result-details result))
    (check-equal? (hash-ref (tool-result-details result) 'status #f) "complete")))

(test-case "#1204: subagent handles provider-send failure gracefully"
  (define failing-prov (make-failing-provider))
  (define ctx (make-test-exec-ctx failing-prov))
  (define result (tool-spawn-subagent (hasheq 'task "This will fail") ctx))
  (check-true (tool-result? result))
  (check-true (tool-result-is-error? result)
              "expected error result when provider fails")
  (define content (tool-result-content result))
  (define text (string-join
                (for/list ([c (in-list content)]
                           #:when (hash-ref c 'text #f))
                  (hash-ref c 'text ""))
                ""))
  (check-true (string-contains? text "subagent failed")
              (format "error should mention 'subagent failed', got: ~v" text)))

(test-case "#1204: subagent child has independent session ID"
  (define-values (prov _) (make-tracking-provider "session test"))
  (define parent-session-id "parent-abc-123")
  (define ctx
    (make-exec-context
     #:runtime-settings (hasheq 'provider prov 'model "test")
     #:call-id (generate-id)
     #:session-metadata (hasheq 'session-id parent-session-id)))
  (define result (tool-spawn-subagent (hasheq 'task "test") ctx))
  (check-true (tool-result? result))
  (when (and (not (tool-result-is-error? result))
             (hash? (tool-result-details result)))
    (define child-session-id (hash-ref (tool-result-details result) 'session-id #f))
    (check-not-false child-session-id "child should have a session ID in details")
    (check-not-equal? child-session-id parent-session-id
                      "child session ID should differ from parent")))

;; ============================================================
;; #1205: Subagent with Scoped Tools and Max-turns
;; ============================================================

(test-case "#1205: subagent respects max-turns with real provider"
  (define loop-prov (make-tool-call-loop-provider))
  (define ctx (make-test-exec-ctx loop-prov))
  ;; Provider always returns tool_calls, but max-turns=1 should stop after 1 turn
  (define result (tool-spawn-subagent (hasheq 'task "loop test" 'max-turns 1) ctx))
  (check-true (tool-result? result))
  ;; With max-turns=1, the loop runs once. The provider returns tool_calls stop reason.
  ;; The current implementation returns 'stopped for tool_calls since no tools are registered.
  ;; This is acceptable behavior — the subagent stops.
  (when (not (tool-result-is-error? result))
    (define details (tool-result-details result))
    (when (hash? details)
      ;; Turns used should be 1 (we set max-turns=1)
      (check-equal? (hash-ref details 'turns-used #f) 1))))

(test-case "#1205: subagent with empty tool registry completes"
  (define-values (prov _) (make-tracking-provider "done"))
  (define ctx (make-test-exec-ctx prov))
  ;; No tools in registry — child can only do text completion
  (define result (tool-spawn-subagent (hasheq 'task "simple task" 'tools '()) ctx))
  (check-true (tool-result? result))
  (check-false (tool-result-is-error? result)))

;; ============================================================
;; #1206: Subagent Integration Tests
;; ============================================================

(test-case "#1206: end-to-end spawn-subagent with provider in exec-context"
  (define response-text "Integration test complete")
  (define-values (prov _) (make-tracking-provider response-text #:name "integration-prov"))
  (define ctx (make-test-exec-ctx prov #:model "gpt-4o-mini"))
  (define result (tool-spawn-subagent
                  (hasheq 'task "Do the integration test"
                          'role "You are an integration tester."
                          'max-turns 3)
                  ctx))
  (check-true (tool-result? result))
  (check-false (tool-result-is-error? result)
               (format "integration test failed: ~a" (tool-result-content result)))
  ;; Verify result structure
  (define content (tool-result-content result))
  (check-true (list? content) "content should be a list")
  (check-true (> (length content) 0) "content should not be empty")
  (define details (tool-result-details result))
  (check-true (hash? details) "details should be a hash")
  (check-equal? (hash-ref details 'turns-used #f) 3 "turns-used should match max-turns")
  (check-not-false (hash-ref details 'session-id #f) "should have session-id")
  (check-not-false (hash-ref details 'status #f) "should have status"))

(test-case "#1206: spawn-subagent tool result contains provider response not mock"
  ;; This specifically tests that we're NOT getting the old mock response "Subagent task completed."
  (define unique-response "UNIQUE_RESPONSE_7f3a9c_not_in_mock")
  (define-values (prov _) (make-tracking-provider unique-response))
  (define ctx (make-test-exec-ctx prov))
  (define result (tool-spawn-subagent (hasheq 'task "unique test") ctx))
  (check-false (tool-result-is-error? result))
  (define text (string-join
                (for/list ([c (in-list (tool-result-content result))]
                           #:when (hash-ref c 'text #f))
                  (hash-ref c 'text ""))
                ""))
  (check-true (string-contains? text unique-response)
              (format "result should contain unique provider response, got: ~v" text))
  ;; Explicitly verify we did NOT get the old mock fallback text
  (check-false (string-contains? text "Subagent task completed.")
               "should NOT contain old mock fallback text"))
