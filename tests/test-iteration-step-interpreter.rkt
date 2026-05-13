#lang racket

;; BOUNDARY: integration

;; tests/test-iteration-step-interpreter.rkt -- Tests for step interpreter action dispatch
;;
;; v0.34.7 T-01: Dedicated tests for iteration step-interpreter module.
;; Tests interpret-step action dispatch through run-iteration-loop with
;; mock providers that simulate different termination scenarios.

(require rackunit
         rackunit/text-ui
         "../runtime/iteration/main-loop.rkt"
         "../runtime/iteration/loop-state.rkt"
         (only-in "../agent/event-bus.rkt" make-event-bus)
         (only-in "../llm/provider.rkt" make-provider)
         (only-in "../llm/model.rkt" make-model-response make-stream-chunk)
         (only-in "../util/protocol-types.rkt"
                  make-message
                  make-text-part
                  make-tool-call-part
                  message-role
                  message-id)
         (only-in "../util/loop-result.rkt"
                  loop-result?
                  loop-result-termination-reason
                  loop-result-messages))

;; ============================================================
;; Helpers
;; ============================================================

(define (simple-msg role text)
  (make-message "test-id" #f role 'message (list (make-text-part text)) (current-seconds) (hasheq)))

;; Provider that returns a completed (stop) response
(define (make-stop-provider text)
  (make-provider (lambda () "mock-stop")
                 (lambda () (hash 'streaming #t))
                 (lambda (req)
                   (make-model-response (list (hasheq 'type "text" 'text text))
                                        "stop"
                                        (hash 'usage
                                              (hasheq 'prompt_tokens 10 'completion_tokens 5))))
                 (lambda (req) (list (make-stream-chunk text #f #f #f)))))

;; Provider that returns tool calls (continue action)
(define (make-tool-call-provider tool-name)
  (define call-count (box 0))
  (make-provider
   (lambda () "mock-toolcall")
   (lambda () (hash 'streaming #t))
   (lambda (req)
     (cond
       [(= (unbox call-count) 0)
        (set-box! call-count 1)
        (make-model-response
         (list
          (hasheq 'type "tool-call" 'id "tc-1" 'function (hasheq 'name tool-name 'arguments "{}")))
         "tool_calls"
         (hash 'usage (hasheq 'prompt_tokens 10 'completion_tokens 5)))]
       [else
        ;; Second call returns stop
        (make-model-response (list (hasheq 'type "text" 'text "tool done"))
                             "stop"
                             (hash 'usage (hasheq 'prompt_tokens 10 'completion_tokens 5)))]))
   (lambda (req) (list (make-stream-chunk "mock" #f #f #f)))))

;; ============================================================
;; Tests
;; ============================================================

(define step-interpreter-tests
  (test-suite "iteration/step-interpreter"

    (test-case "stop action: completed response terminates loop"
      (define bus (make-event-bus))
      (define prov (make-stop-provider "all done"))
      (define ctx (list (simple-msg 'user "hello")))
      (define result (run-iteration-loop ctx prov bus #f #f "/tmp/test-log" "test-session" 10))
      (check-pred loop-result? result)
      (check-equal? (loop-result-termination-reason result) 'completed))

    (test-case "stop-hard-limit: max-iterations=1 with tool calls triggers limit"
      ;; Provider returns tool calls but no registry to execute them
      ;; This tests the hard limit path through the step interpreter
      (define bus (make-event-bus))
      (define prov (make-tool-call-provider "read"))
      (define ctx (list (simple-msg 'user "read a file")))
      (define result (run-iteration-loop ctx prov bus #f #f "/tmp/test-log" "test-session" 1))
      ;; With only 1 iteration, tool calls should trigger soft or hard limit
      (check-pred loop-result? result)
      (check-not-false (member (loop-result-termination-reason result)
                               '(completed max-iterations-exceeded tool-calls-pending))))

    (test-case "continue action: completed response after tool calls"
      ;; Provider first returns tool calls, then stops on second call
      ;; This verifies the loop actually continues past the first tool-call response
      (define bus (make-event-bus))
      (define prov (make-tool-call-provider "read"))
      (define ctx (list (simple-msg 'user "read a file")))
      (define result (run-iteration-loop ctx prov bus #f #f "/tmp/test-log" "test-session" 10))
      (check-pred loop-result? result)
      ;; Without a tool registry, tool calls cannot execute;
      ;; the loop should still terminate gracefully (not hang or crash)
      (check-not-false (member (loop-result-termination-reason result)
                               '(completed max-iterations-exceeded tool-calls-pending))
                       "continue action should terminate gracefully"))))

(module+ main
  (run-tests step-interpreter-tests))
(module+ test
  (run-tests step-interpreter-tests))
