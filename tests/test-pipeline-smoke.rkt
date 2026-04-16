#lang racket

;; q/tests/test-pipeline-smoke.rkt — End-to-end pipeline smoke tests
;;
;; Exercises the complete make-agent-session → run-prompt! → verify output
;; flow. These tests would have caught BUG-05, BUG-07→11, BUG-18, #512, #513.
;;
;; Each test verifies a full pipeline scenario: provider → agent loop →
;; tool execution → session persistence → event emission.

(require rackunit
         rackunit/text-ui
         json
         "../llm/model.rkt"
         "../llm/provider.rkt"
         "../agent/event-bus.rkt"
         (only-in "../util/protocol-types.rkt"
                  make-event
                  event?
                  event-event
                  event-payload
                  event-time
                  event-session-id
                  event-turn-id
                  event->jsexpr
                  jsexpr->event
                  message?
                  message-id
                  message-role
                  message-content
                  make-message
                  make-text-part
                  message->jsexpr
                  jsexpr->message
                  loop-result?
                  loop-result-termination-reason
                  loop-result-messages
                  loop-result-metadata)
         (only-in "../tools/tool.rkt"
                  make-tool-registry
                  register-tool!
                  make-tool
                  tool-names
                  tool?
                  tool-execute
                  make-exec-context
                  make-success-result
                  make-error-result
                  tool-result?
                  tool-result-content
                  tool-result-is-error?)
         "../extensions/api.rkt"
         "../util/jsonl.rkt"
         (prefix-in sdk: "../interfaces/sdk.rkt")
         "helpers/mock-provider.rkt")

;; Import register-default-tools! and make-event-bus from main
(require (only-in "../main.rkt" register-default-tools! make-event-bus))

;; ============================================================
;; Helpers
;; ============================================================

(define (make-temp-dir)
  (make-temporary-file "q-pipe-~a" 'directory))

(define (cleanup-dir dir)
  (when (directory-exists? dir)
    (delete-directory/files dir)))

(define (make-test-runtime prov
                           #:session-dir [session-dir #f]
                           #:max-iterations [max-iter 10]
                           #:tool-registry [tool-reg #f]
                           #:event-bus [bus #f])
  (define dir (or session-dir (make-temp-dir)))
  (define reg (or tool-reg (make-tool-registry)))
  (define b (or bus (make-event-bus)))
  (sdk:make-runtime #:provider prov
                    #:session-dir dir
                    #:tool-registry reg
                    #:event-bus b
                    #:max-iterations max-iter))

(define (log-entries dir sid)
  (define log-path (build-path dir sid "session.jsonl"))
  (if (file-exists? log-path)
      (filter (lambda (e) (not (equal? (hash-ref e 'kind #f) "session-info")))
              (jsonl-read-all-valid log-path))
      '()))

(define (log-roles dir sid)
  (map (lambda (e) (hash-ref e 'role #f)) (log-entries dir sid)))

(define (collect-events bus)
  (define evts-box (box '()))
  (subscribe! bus
              (lambda (evt) (set-box! evts-box (append (unbox evts-box) (list (event-event evt))))))
  evts-box)

;; ============================================================
;; Tests
;; ============================================================

;; ---- 1. Multi-turn tool-use round-trip ----
(test-case "smoke: multi-turn tool-use round-trip"
  (define dir (make-temp-dir))
  (define reg (make-tool-registry))
  (register-tool!
   reg
   (make-tool
    "echo"
    "Echo tool"
    (hasheq 'type "object" 'required '("text") 'properties (hasheq 'text (hasheq 'type "string")))
    (lambda (args ctx) (make-success-result (hash-ref args 'text "echo")))))
  ;; Provider: first returns tool-call, then returns follow-up text
  (define prov
    (make-tool-call-mock-provider "echo" (hasheq 'text "hello world") "Got the echo result"))
  (define rt (make-test-runtime prov #:session-dir dir #:tool-registry reg))
  (define rt2 (sdk:open-session rt))
  (define-values (rt3 result) (sdk:run-prompt! rt2 "echo hello"))
  (check-equal? (loop-result-termination-reason result)
                'completed
                "should complete after tool execution and follow-up text")
  ;; Verify log has at least: user, assistant(tool-call), tool-result,
  ;; assistant(text)
  (define sid (hash-ref (sdk:session-info rt3) 'session-id))
  (define entries (log-entries dir sid))
  (check >= (length entries) 3 (format "should have >=3 entries, got ~a" (length entries)))
  (define roles (log-roles dir sid))
  (check-not-false (member "user" roles) "log should contain user message")
  (check-not-false (member "assistant" roles) "log should contain assistant message")
  (cleanup-dir dir))

;; ---- 2. Streaming response persists ----
(test-case "smoke: streaming response renders and persists"
  (define dir (make-temp-dir))
  (define prov (make-simple-mock-provider "streaming text here"))
  (define rt (make-test-runtime prov #:session-dir dir))
  (define rt2 (sdk:open-session rt))
  (define-values (rt3 result) (sdk:run-prompt! rt2 "test streaming"))
  (check-pred loop-result? result)
  (check-equal? (loop-result-termination-reason result) 'completed)
  ;; Verify assistant text persisted in log
  (define sid (hash-ref (sdk:session-info rt3) 'session-id))
  (define entries (log-entries dir sid))
  (check >= (length entries) 2)
  (define assistant-entry (findf (lambda (e) (equal? (hash-ref e 'role #f) "assistant")) entries))
  (check-not-false assistant-entry "should have an assistant entry after streaming")
  (define content (hash-ref assistant-entry 'content #f))
  (check-not-false content "assistant entry should have content")
  ;; Content is a list of content-part hashes
  (check-true (list? content) "content should be a list of content parts")
  (cleanup-dir dir))

;; ---- 3. Resume session preserves history ----
(test-case "smoke: resume session preserves history"
  (define dir (make-temp-dir))
  (define bus (make-event-bus))
  (define reg (make-tool-registry))
  ;; First session: run 2 prompts
  (define prov1 (make-simple-mock-provider "first" "second"))
  (define rt1 (make-test-runtime prov1 #:session-dir dir #:tool-registry reg #:event-bus bus))
  (define rt2 (sdk:open-session rt1))
  (define sid (hash-ref (sdk:session-info rt2) 'session-id))
  (sdk:run-prompt! rt2 "prompt one")
  (sdk:run-prompt! rt2 "prompt two")
  ;; Resume with new provider
  (define prov2 (make-simple-mock-provider "resumed response"))
  (define rt3 (make-test-runtime prov2 #:session-dir dir #:tool-registry reg #:event-bus bus))
  (define rt4 (sdk:open-session rt3 sid))
  (define info (sdk:session-info rt4))
  ;; Should have 4 entries: 2 user + 2 assistant from first session
  (check >=
         (hash-ref info 'history-length)
         4
         (format "resumed session should have >=4 entries, got ~a" (hash-ref info 'history-length)))
  (check-equal? (hash-ref info 'session-id) sid "session ID should match")
  (check-true (hash-ref info 'active?) "resumed session should be active")
  (cleanup-dir dir))

;; ---- 4. Error path — provider raises exception ----
(test-case "smoke: provider error does not crash session"
  (define dir (make-temp-dir))
  (define reg (make-tool-registry))
  (define bus (make-event-bus))
  (define events-box (collect-events bus))
  ;; Create a provider that raises an error
  (define error-prov
    (make-provider
     (lambda () "error-mock")
     (lambda () (hash 'streaming #t))
     ;; send raises
     (lambda (req) (raise (exn:fail "simulated provider error" (current-continuation-marks))))
     ;; stream also raises
     (lambda (req) (raise (exn:fail "simulated provider error" (current-continuation-marks))))))
  (define rt
    (sdk:make-runtime #:provider error-prov #:session-dir dir #:tool-registry reg #:event-bus bus))
  (define rt2 (sdk:open-session rt))
  ;; run-prompt! should not crash — it should handle the error
  (define-values (rt3 result) (sdk:run-prompt! rt2 "trigger error"))
  ;; Session should still be usable
  (check-true (sdk:runtime? rt3) "runtime should survive provider error")
  (cleanup-dir dir))

;; ---- 5. Multi-tool dispatch ----
(test-case "smoke: multiple tool calls in single turn"
  (define dir (make-temp-dir))
  (define reg (make-tool-registry))
  ;; Register two tools
  (register-tool! reg
                  (make-tool "add"
                             "Add numbers"
                             (hasheq 'type
                                     "object"
                                     'required
                                     '("a" "b")
                                     'properties
                                     (hasheq 'a (hasheq 'type "number") 'b (hasheq 'type "number")))
                             (lambda (args ctx)
                               (make-success-result (number->string (+ (hash-ref args 'a 0)
                                                                       (hash-ref args 'b 0)))))))
  (register-tool! reg
                  (make-tool "multiply"
                             "Multiply numbers"
                             (hasheq 'type
                                     "object"
                                     'required
                                     '("a" "b")
                                     'properties
                                     (hasheq 'a (hasheq 'type "number") 'b (hasheq 'type "number")))
                             (lambda (args ctx)
                               (make-success-result (number->string (* (hash-ref args 'a 0)
                                                                       (hash-ref args 'b 0)))))))
  ;; Provider: first returns 2 tool calls, second returns summary text
  (define prov
    (make-multi-mock-provider
     ;; First response: two tool calls
     (list (make-model-response
            (list (hash 'type "tool-call" 'id "tc-1" 'name "add" 'arguments (hasheq 'a 3 'b 4))
                  (hash 'type "tool-call" 'id "tc-2" 'name "multiply" 'arguments (hasheq 'a 2 'b 5)))
            (hasheq 'prompt-tokens 10 'completion-tokens 5 'total-tokens 15)
            "mock-model"
            'tool-calls)
           ;; Second response: summary text
           (make-model-response (list (hash 'type "text" 'text "3+4=7 and 2*5=10"))
                                (hasheq 'prompt-tokens 20 'completion-tokens 10 'total-tokens 30)
                                "mock-model"
                                'stop))))
  (define rt (make-test-runtime prov #:session-dir dir #:tool-registry reg #:max-iterations 5))
  (define rt2 (sdk:open-session rt))
  (define-values (rt3 result) (sdk:run-prompt! rt2 "compute"))
  (check-equal? (loop-result-termination-reason result)
                'completed
                "should complete after both tool calls and follow-up")
  ;; Log should have: user, assistant(2 tool-calls), 2 tool-results,
  ;; assistant(summary)
  (define sid (hash-ref (sdk:session-info rt3) 'session-id))
  (define entries (log-entries dir sid))
  (check >=
         (length entries)
         4
         (format "should have >=4 entries for multi-tool, got ~a" (length entries)))
  (cleanup-dir dir))

;; ---- 6. Event sequence verification ----
(test-case "smoke: complete event sequence during prompt"
  (define dir (make-temp-dir))
  (define bus (make-event-bus))
  (define events-box (collect-events bus))
  (define prov (make-simple-mock-provider "event check"))
  (define rt (make-test-runtime prov #:session-dir dir #:event-bus bus))
  (define rt2 (sdk:open-session rt))
  (define-values (rt3 _result) (sdk:run-prompt! rt2 "test events"))
  (define events (unbox events-box))
  ;; Verify critical event sequence
  (check-not-false (member "session.started" events) "should emit session.started")
  (check-not-false (member "turn.started" events) "should emit turn.started")
  (check-not-false (member "turn.completed" events) "should emit turn.completed")
  ;; Verify ordering: started before completed
  (define turn-start-idx (index-of events "turn.started"))
  (define turn-end-idx (index-of events "turn.completed"))
  (when (and turn-start-idx turn-end-idx)
    (check < turn-start-idx turn-end-idx "turn.started should come before turn.completed"))
  (cleanup-dir dir))

;; ---- 7. Empty session context ----
(test-case "smoke: first prompt in empty session"
  (define dir (make-temp-dir))
  (define prov (make-simple-mock-provider "initial response"))
  (define rt (make-test-runtime prov #:session-dir dir))
  (define rt2 (sdk:open-session rt))
  (define info-before (sdk:session-info rt2))
  (check-equal? (hash-ref info-before 'history-length) 0 "new session should have empty history")
  (define-values (rt3 result) (sdk:run-prompt! rt2 "first prompt"))
  (define info-after (sdk:session-info rt3))
  (check >= (hash-ref info-after 'history-length) 2 "after one prompt, should have user + assistant")
  (check-pred loop-result? result)
  (cleanup-dir dir))

;; ---- 8. Session persistence after multiple prompts ----
(test-case "smoke: JSONL log integrity after multiple prompts"
  (define dir (make-temp-dir))
  (define prov (make-simple-mock-provider "a" "b" "c"))
  (define rt (make-test-runtime prov #:session-dir dir))
  (define rt2 (sdk:open-session rt))
  (define sid (hash-ref (sdk:session-info rt2) 'session-id))
  (sdk:run-prompt! rt2 "p1")
  (sdk:run-prompt! rt2 "p2")
  (sdk:run-prompt! rt2 "p3")
  ;; Verify log integrity
  (define entries (log-entries dir sid))
  (check >= (length entries) 6 "3 prompts should produce >=6 entries (3 user + 3 assistant)")
  ;; Every entry should be valid JSON with 'role' key
  (for ([e (in-list entries)])
    (check-not-false (hash-ref e 'role #f) "every JSONL entry should have 'role'"))
  ;; Check user/assistant alternation starts with user
  (define roles (log-roles dir sid))
  (check-equal? (first roles) "user" "first entry should be user")
  (cleanup-dir dir))

;; ---- 9. Tool execution error is captured, not crash ----
(test-case "smoke: tool execution error captured in session"
  (define dir (make-temp-dir))
  (define reg (make-tool-registry))
  (register-tool! reg
                  (make-tool "fail-tool"
                             "Always fails"
                             (hasheq 'type "object" 'properties (hasheq))
                             (lambda (args ctx) (make-error-result "deliberate tool failure"))))
  ;; Provider: calls fail-tool, then recovers with text
  (define prov (make-tool-call-mock-provider "fail-tool" (hasheq) "Recovered from tool error"))
  (define rt (make-test-runtime prov #:session-dir dir #:tool-registry reg #:max-iterations 5))
  (define rt2 (sdk:open-session rt))
  (define-values (rt3 result) (sdk:run-prompt! rt2 "use fail tool"))
  (check-pred loop-result? result)
  ;; Session should complete — error captured in tool-result
  (check-equal? (loop-result-termination-reason result)
                'completed
                "should complete even when tool returns error")
  (cleanup-dir dir))
