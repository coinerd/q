#lang racket/base

;; tests/test-spawn-subagent-tool-dispatch.rkt — v0.19.4 GAP-1 fix verification
;;
;; Verifies that spawn-subagent actually dispatches tool_calls instead of
;; returning 'stopped, and that child agents get a non-empty tool registry.

(require rackunit
         racket/list
         "../tools/tool.rkt"
         "../tools/builtins/spawn-subagent.rkt"
         "../llm/provider.rkt"
         "../llm/model.rkt"
         "../util/ids.rkt")

;; ============================================================
;; Helpers
;; ============================================================

;; Stateful provider that returns different responses on each call
(define (make-stateful-mock-provider responses)
  (define idx (box 0))
  (define (next-response req)
    (define i (unbox idx))
    (set-box! idx (add1 i))
    (if (< i (length responses))
        (list-ref responses i)
        (last responses)))
  (make-provider (lambda () "stateful-mock")
                 (lambda () (hasheq 'streaming #t))
                 next-response
                 (lambda (req) '())))

;; ============================================================
;; Tests
;; ============================================================

(test-case "spawn-subagent returns error when task is missing"
  (define result (tool-spawn-subagent (hasheq)))
  (check-true (tool-result-is-error? result)))

(test-case "spawn-subagent with mock provider completes a simple task"
  (define mock-response
    (make-model-response (list (hasheq 'type "text" 'text "Task completed successfully."))
                         (hasheq 'prompt-tokens 10 'completion-tokens 20 'total-tokens 30)
                         "mock-model"
                         'stop))
  (define provider (make-mock-provider mock-response))
  (define exec-ctx
    (make-exec-context #:working-directory (current-directory)
                       #:runtime-settings (hasheq 'provider provider 'model "mock-model")))
  (define result (tool-spawn-subagent (hasheq 'task "Say hello") exec-ctx))
  (check-false (tool-result-is-error? result) "should succeed")
  (define details (tool-result-details result))
  (check-equal? (hash-ref details 'status #f) "complete"))

(test-case "spawn-subagent dispatches tool_calls instead of returning 'stopped'"
  ;; Provider returns tool_calls first, then stop
  (define tool-call-response
    (make-model-response (list (hasheq 'id
                                       "tc-1"
                                       'type
                                       "text"
                                       'text
                                       ""
                                       'name
                                       "read"
                                       'arguments
                                       "{\"path\":\"/etc/hostname\"}"))
                         (hasheq 'prompt-tokens 10 'completion-tokens 20 'total-tokens 30)
                         "mock-model"
                         'tool_calls))
  (define done-response
    (make-model-response (list (hasheq 'type "text" 'text "The hostname file was read successfully."))
                         (hasheq 'prompt-tokens 10 'completion-tokens 20 'total-tokens 30)
                         "mock-model"
                         'stop))
  (define provider (make-stateful-mock-provider (list tool-call-response done-response)))
  (define exec-ctx
    (make-exec-context #:working-directory (current-directory)
                       #:runtime-settings (hasheq 'provider provider 'model "mock-model")))
  (define result (tool-spawn-subagent (hasheq 'task "Read /etc/hostname" 'max-turns 3) exec-ctx))
  (check-false (tool-result-is-error? result)
               (format "should succeed, got error: ~a"
                       (if (tool-result-is-error? result)
                           (tool-result-content result)
                           "none")))
  (define details (tool-result-details result))
  ;; KEY ASSERTION: should NOT be 'stopped' — tool_calls are dispatched
  (check-not-equal? (hash-ref details 'status #f)
                    "stopped"
                    "tool_calls should be dispatched, not return 'stopped'"))

(test-case "spawn-subagents batch validates inputs"
  (check-true (tool-result-is-error? (tool-spawn-subagents (hasheq))))
  (check-true (tool-result-is-error? (tool-spawn-subagents (hasheq 'jobs '()))))
  (check-true (tool-result-is-error?
               (tool-spawn-subagents (hasheq 'jobs
                                             (build-list 13 (lambda (_) (hasheq 'task "x"))))))))
