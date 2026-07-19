#lang racket

;; @speed fast
;; @suite default

;; test-spawn-subagent-serialization.rkt — TDD regression for JSON serialization
;;
;; Verifies that spawn-subagent and spawn-subagents send provider-facing
;; JSON-serializable message hashes (not internal message structs) through
;; real provider request bodies.
;;
;; Bug: run-subagent-loop passed internal `message` structs to
;; make-model-request, which caused `jsexpr->bytes` failures in real
;; providers (OpenAI-compatible, Anthropic, Gemini).

(require rackunit
         rackunit/text-ui
         json
         racket/generator
         "../tools/builtins/spawn-subagent.rkt"
         "../tools/tool.rkt"
         "../llm/provider.rkt"
         "../llm/model.rkt"
         "../llm/openai-compatible.rkt"
         "../runtime/settings.rkt")

;; ============================================================
;; Serialization-checking fake provider
;; ============================================================

;; This provider simulates what real providers do: build a request body
;; and serialize it with jsexpr->bytes. If the messages contain Racket
;; structs, jsexpr->bytes will fail — reproducing the live GUI bug.
(define (make-serialization-checking-provider)
  (define (send-handler req)
    ;; This is the critical check: real providers serialize the request body.
    ;; If messages contain `message` structs, this blows up.
    (define body (openai-build-request-body req))
    (jsexpr->bytes body) ;; <-- triggers the bug if messages are structs
    ;; Return a simple text response
    (make-model-response (list (hasheq 'type "text" 'text "subagent completed"))
                         (hasheq 'prompt-tokens 10 'completion-tokens 5 'total-tokens 15)
                         "test-model"
                         'stop))
  (make-provider
   (lambda () "serialization-check")
   (lambda () (hasheq 'streaming #t))
   send-handler
   (lambda (req)
     (generator () (yield (make-stream-chunk "subagent completed" #f #f #t)) (yield #f)))))

;; ============================================================
;; Provider returning tool-call responses (reproduces #hasheq bug)
;; ============================================================

;; This provider returns responses with BOTH text AND tool_call parts,
;; reproducing the bug where (format "~a" content) produces #hasheq garbage.
(define (make-tool-call-returning-provider)
  (define call-count 0)
  (define (send-handler req)
    (set! call-count (add1 call-count))
    (if (= call-count 1)
        ;; First response: text + tool_call (triggers the bug)
        (make-model-response (list (hasheq 'type "text" 'text "I'll check the project.")
                                   (hasheq 'type
                                           "tool_call"
                                           'id
                                           "call_abc123"
                                           'name
                                           "bash"
                                           'arguments
                                           (hasheq 'command "echo done")))
                             (hasheq 'prompt-tokens 10 'completion-tokens 20 'total-tokens 30)
                             "test-model"
                             'tool_calls)
        ;; Second response (after tool result): text only
        (make-model-response (list (hasheq 'type "text" 'text "Task done. Created 5 files."))
                             (hasheq 'prompt-tokens 10 'completion-tokens 10 'total-tokens 20)
                             "test-model"
                             'stop)))
  (make-provider (lambda () "tool-call-check")
                 (lambda () (hasheq 'streaming #f))
                 send-handler
                 (lambda (req)
                   (generator () (yield (make-stream-chunk "done" #f #f #t)) (yield #f)))))

;; ============================================================
;; Tests
;; ============================================================

(define serialization-tests
  (test-suite "spawn-subagent serialization regression"

    (test-case "spawn-subagent sends JSON-serializable messages to real-provider-like provider"
      (define provider (make-serialization-checking-provider))
      (define settings (q-settings (hash) (hash) (hasheq 'provider provider 'model "test-model")))
      (define ctx (make-exec-context #:runtime-settings settings #:call-id "serialization-test"))
      (define result
        (tool-spawn-subagent
         (hasheq 'task "do something" 'capabilities '(read-only))
         ctx))
      (check-true (tool-result? result))
      (check-false (tool-result-is-error? result)
                   (format "expected success, got error: ~a"
                           (if (tool-result-is-error? result)
                               (hash-ref (car (tool-result-content result)) 'text "unknown")
                               "no error")))
      (check-true (string-contains? (hash-ref (car (tool-result-content result)) 'text "")
                                    "subagent completed")))

    (test-case "spawn-subagents sends JSON-serializable messages for batch jobs"
      (define provider (make-serialization-checking-provider))
      (define settings (q-settings (hash) (hash) (hasheq 'provider provider 'model "test-model")))
      (define ctx
        (make-exec-context #:runtime-settings settings #:call-id "batch-serialization-test"))
      (define result
        (tool-spawn-subagents
         (hasheq 'jobs
                 (list (hasheq 'task "task one"
                               'jobId "j1"
                               'capabilities '(read-only)))
                 'maxParallel
                 1)
         ctx))
      (check-true (tool-result? result))
      (check-false (tool-result-is-error? result)
                   (format "expected success, got error: ~a"
                           (if (tool-result-is-error? result)
                               (hash-ref (car (tool-result-content result)) 'text "unknown")
                               "no error"))))

    ;; ============================================================
    ;; #hasheq bug regression tests
    ;; ============================================================

    (test-case "BUG: spawn-subagent result must NOT contain #hasheq garbage"
      ;; When the provider returns responses with tool_call parts,
      ;; the result text must contain ONLY readable text, not raw
      ;; #hasheq((arguments . #hasheq((command . "ls ...")))) garbage.
      (define provider (make-tool-call-returning-provider))
      (define settings (q-settings (hash) (hash) (hasheq 'provider provider 'model "test-model")))
      (define ctx (make-exec-context #:runtime-settings settings #:call-id "hasheq-test"))
      (define result
        (tool-spawn-subagent
         (hasheq 'task "check files" 'capabilities '(read-only))
         ctx))
      (check-true (tool-result? result))
      (check-false (tool-result-is-error? result)
                   (format "expected success, got error: ~a"
                           (if (tool-result-is-error? result)
                               (hash-ref (car (tool-result-content result)) 'text "unknown")
                               "no error")))
      (define result-text (hash-ref (car (tool-result-content result)) 'text ""))
      ;; CRITICAL CHECK: No #hasheq in result text
      (check-false (string-contains? result-text "#hasheq")
                   (format "result must not contain #hasheq garbage, got: ~a"
                           (substring result-text 0 (min 500 (string-length result-text)))))
      ;; Must contain actual readable text from the assistant
      (check-true (string-contains? result-text "check the project")
                  (format "result should contain readable text, got: ~a" result-text))
      (check-true (string-contains? result-text "Task done")
                  (format "result should contain final text, got: ~a" result-text)))

    (test-case "extract-text-summary: handles string items via spawn-subagents result"
      ;; Test indirectly: spawn-subagents calls extract-text-summary on results.
      ;; The tool-call-returning-provider produces assistant messages with text content.
      ;; The batch result should contain readable text, no #hasheq garbage.
      (define provider (make-tool-call-returning-provider))
      (define settings (q-settings (hash) (hash) (hasheq 'provider provider 'model "test-model")))
      (define ctx (make-exec-context #:runtime-settings settings #:call-id "batch-hasheq-test"))
      (define result
        (tool-spawn-subagents
         (hasheq 'jobs
                 (list (hasheq 'task "check files"
                               'jobId "j1"
                               'capabilities '(read-only)))
                 'maxParallel
                 1)
         ctx))
      (check-true (tool-result? result))
      (check-false (tool-result-is-error? result))
      (define result-text (hash-ref (car (tool-result-content result)) 'text ""))
      ;; No #hasheq garbage in aggregated batch result
      (check-false (string-contains? result-text "#hasheq")
                   (format "batch result must not contain #hasheq, got: ~a"
                           (substring result-text 0 (min 500 (string-length result-text))))))

    (test-case "extract-text-summary: handles mixed content via single subagent result"
      ;; Test indirectly through tool-spawn-subagent
      (define provider (make-tool-call-returning-provider))
      (define settings (q-settings (hash) (hash) (hasheq 'provider provider 'model "test-model")))
      (define ctx (make-exec-context #:runtime-settings settings #:call-id "mixed-test"))
      (define result
        (tool-spawn-subagent
         (hasheq 'task "check files" 'capabilities '(read-only))
         ctx))
      (define result-text (hash-ref (car (tool-result-content result)) 'text ""))
      ;; Should have readable text from both assistant turns
      (check-false (string-contains? result-text "#hasheq")
                   (format "result must not contain #hasheq, got: ~a"
                           (substring result-text 0 (min 500 (string-length result-text)))))
      (check-true (string-contains? result-text "check the project"))
      (check-true (string-contains? result-text "Task done")))))

(module+ test
  (run-tests serialization-tests))

(module+ main
  (run-tests serialization-tests))
