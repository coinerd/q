#lang racket

(require rackunit
         rackunit/text-ui
         racket/string
         "../tools/builtins/spawn-subagent.rkt"
         "../tools/tool.rkt"
         "../runtime/settings.rkt"
         "../llm/provider.rkt"
         "../llm/model.rkt")

(define (text-content result)
  (string-join (for/list ([part (in-list (tool-result-content result))]
                          #:when (and (hash? part) (hash-ref part 'text #f)))
                 (hash-ref part 'text ""))
               "\n"))

(define (make-provider-with-text text)
  (make-mock-provider (make-model-response (list (hasheq 'type "text" 'text text))
                                           (hasheq)
                                           "provider-resolution-test"
                                           'stop)
                      #:name "provider-resolution-test"))

(define provider-resolution-tests
  (test-suite "spawn-subagent provider resolution"
    (test-case "spawn-subagent does not pass string default-provider to provider-send"
      ;; This reproduces the live failure mode: runtime settings loaded from JSON may
      ;; contain a string provider name under default-provider. The tool must not pass
      ;; that string to provider-send.
      (define settings (make-minimal-settings #:provider "openai-compatible" #:model "test-model"))
      (define ctx (make-exec-context #:runtime-settings settings #:call-id "provider-string-test"))
      (define result (tool-spawn-subagent (hasheq 'task "return ok") ctx))
      (check-true (tool-result? result))
      (check-false (tool-result-is-error? result)
                   (format "expected safe fallback, got: ~a" (text-content result)))
      (check-false (string-contains? (text-content result) "provider-send")))

    (test-case "spawn-subagent accepts provider struct from q-settings provider key"
      (define provider-response "STRUCT_PROVIDER_RESPONSE")
      (define provider (make-provider-with-text provider-response))
      (define settings (q-settings (hash) (hash) (hasheq 'provider provider 'model "test-model")))
      (define ctx (make-exec-context #:runtime-settings settings #:call-id "provider-struct-test"))
      (define result (tool-spawn-subagent (hasheq 'task "return provider response") ctx))
      (check-true (tool-result? result))
      (check-false (tool-result-is-error? result)
                   (format "expected success, got: ~a" (text-content result)))
      (check-true (string-contains? (text-content result) provider-response)))))

(run-tests provider-resolution-tests)
