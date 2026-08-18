#lang racket/base

;; @speed fast
;; @suite default
;; @boundary unit

;; test-sse-shared-azure.rkt — Tests for Azure OpenAI SSE dedup (W1)
;; Part of v0.81.0

(require rackunit
         racket/port
         racket/generator
         "../llm/model.rkt"
         (only-in "../llm/stream.rkt" stream-sse-events)
         (only-in "../llm/openai-compatible.rkt" normalize-openai-chunk))

(test-case "normalize-openai-chunk: text delta"
  (define chunk
    (normalize-openai-chunk
     (hasheq 'choices (list (hasheq 'delta (hasheq 'content "hello") 'finish_reason #f)))))
  (check-pred stream-chunk? chunk)
  (check-equal? (stream-chunk-delta-text chunk) "hello")
  (check-false (stream-chunk-done? chunk)))

(test-case "normalize-openai-chunk: finish_reason triggers done"
  (define chunk
    (normalize-openai-chunk
     (hasheq 'choices (list (hasheq 'delta (hasheq 'content "") 'finish_reason "stop")))))
  (check-pred stream-chunk? chunk)
  (check-equal? (stream-chunk-finish-reason chunk) "stop")
  (check-true (stream-chunk-done? chunk)))

(test-case "normalize-openai-chunk: usage extraction"
  (define chunk
    (normalize-openai-chunk
     (hasheq 'choices
             (list (hasheq 'delta (hasheq) 'finish_reason "stop"))
             'usage
             (hasheq 'prompt_tokens 10 'completion_tokens 20 'total_tokens 30))))
  (check-pred stream-chunk? chunk)
  (define usage (stream-chunk-usage chunk))
  (check-equal? (hash-ref usage 'prompt_tokens) 10)
  (check-equal? (hash-ref usage 'completion_tokens) 20))

(test-case "normalize-openai-chunk: usage null in stream chunk (DeepSeek) coerced to #f"
  ;; DeepSeek's OpenAI-compatible endpoint emits "usage": null on every
  ;; intermediate streaming chunk (only the final chunk carries a usage hash).
  ;; q's strict JSON parser maps JSON null to the symbol 'null, which violates
  ;; the (or/c hash? #f) usage contract on make-stream-chunk. Regression for the
  ;; deepseek-v4-flash provider crash (make-stream-chunk: contract violation).
  (define chunk
    (normalize-openai-chunk
     (hasheq 'choices (list (hasheq 'delta (hasheq 'content "hi") 'finish_reason #f)) 'usage 'null)))
  (check-pred stream-chunk? chunk)
  (check-false (stream-chunk-usage chunk) "usage null coerced to #f"))

(test-case "normalize-openai-chunk: reasoning_content null in stream chunk (DeepSeek) coerced to #f"
  ;; DeepSeek also emits "reasoning_content": null on chunks where no reasoning
  ;; delta is present (e.g. the first chunk and after reasoning completes).
  ;; 'null must be coerced to #f for the (or/c string? #f) delta-thinking
  ;; contract. Regression for the deepseek-v4-flash #:delta-thinking crash.
  (define chunk
    (normalize-openai-chunk
     (hasheq 'choices
             (list (hasheq 'delta
                           (hasheq 'role "assistant" 'content 'null 'reasoning_content 'null)
                           'finish_reason
                           'null)))))
  (check-pred stream-chunk? chunk)
  (check-false (stream-chunk-delta-thinking chunk) "reasoning_content null coerced to #f")
  (check-false (stream-chunk-delta-text chunk) "content null coerced to #f"))

(test-case "normalize-openai-chunk: tool_calls delta (was missing in Azure inline)"
  (define chunk
    (normalize-openai-chunk
     (hasheq
      'choices
      (list (hasheq
             'delta
             (hasheq 'tool_calls
                     (list (hasheq 'id "call_123" 'function (hasheq 'name "read" 'arguments ""))))
             'finish_reason
             #f)))))
  (check-pred stream-chunk? chunk)
  (check-not-false (stream-chunk-delta-tool-call chunk) "tool_calls now captured"))

(test-case "stream-sse-events: processes port through normalize-openai-chunk"
  (define port
    (open-input-string
     (string-append
      "data: {\"choices\":[{\"delta\":{\"content\":\"hi\"},\"finish_reason\":null}]}\n\n"
      "data: [DONE]\n\n")))
  (define gen (stream-sse-events port (lambda (parsed) (list (normalize-openai-chunk parsed)))))
  (define chunks
    (for/list ([ch (in-producer gen #f)])
      ch))
  (check-equal? (length chunks) 1)
  (check-equal? (stream-chunk-delta-text (car chunks)) "hi"))
