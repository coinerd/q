#lang racket/base

;; @speed fast
;; @suite default

;; test-sse-shared-azure.rkt — Tests for Azure OpenAI SSE dedup (W1)
;; Part of v0.81.0

(require rackunit
         racket/port
         racket/generator
         "../llm/model.rkt"
         (only-in "../llm/stream.rkt" stream-sse-events normalize-openai-chunk))

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
