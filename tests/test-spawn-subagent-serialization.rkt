#lang racket

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
;; Tests
;; ============================================================

(define serialization-tests
  (test-suite "spawn-subagent serialization regression"

    (test-case "spawn-subagent sends JSON-serializable messages to real-provider-like provider"
      (define provider (make-serialization-checking-provider))
      (define settings (q-settings (hash) (hash) (hasheq 'provider provider 'model "test-model")))
      (define ctx (make-exec-context #:runtime-settings settings #:call-id "serialization-test"))
      (define result (tool-spawn-subagent (hasheq 'task "do something") ctx))
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
         (hasheq 'jobs (list (hasheq 'task "task one" 'jobId "j1")) 'maxParallel 1)
         ctx))
      (check-true (tool-result? result))
      (check-false (tool-result-is-error? result)
                   (format "expected success, got error: ~a"
                           (if (tool-result-is-error? result)
                               (hash-ref (car (tool-result-content result)) 'text "unknown")
                               "no error"))))))

(run-tests serialization-tests)
