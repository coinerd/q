#lang racket

(require rackunit
         "../llm/model.rkt"
         "../llm/provider.rkt"
         "../llm/stream.rkt"
         "../llm/anthropic.rkt")

;; ============================================================
;; Test suite: llm/anthropic.rkt — Anthropic provider adapter
;; ============================================================

;; ============================================================
;; 1. anthropic-build-request-body — basic structure
;; ============================================================

(define req-basic
  (make-model-request
   (list (hash 'role "user" 'content "Hello"))
   #f
   (hash 'model "claude-sonnet-4-20250514")))

(define body-basic (anthropic-build-request-body req-basic))
(check-pred hash? body-basic
            "build-request-body returns a hash")
(check-equal? (hash-ref body-basic 'model) "claude-sonnet-4-20250514"
              "model from settings")
(check-equal? (hash-ref body-basic 'stream) #f
              "non-streaming by default")
(check-pred list? (hash-ref body-basic 'messages)
            "messages is a list")
(check-equal? (length (hash-ref body-basic 'messages)) 1
              "one message")

;; ============================================================
;; 2. anthropic-build-request-body — includes max_tokens
;; ============================================================

(check-equal? (hash-ref body-basic 'max_tokens) 4096
              "default max_tokens is 4096")

;; Custom max_tokens
(define req-custom-max
  (make-model-request
   (list (hash 'role "user" 'content "Hello"))
   #f
   (hash 'model "claude-sonnet-4-20250514" 'max-tokens 8192)))

(define body-custom-max (anthropic-build-request-body req-custom-max))
(check-equal? (hash-ref body-custom-max 'max_tokens) 8192
              "custom max_tokens from settings")

;; ============================================================
;; 3. anthropic-build-request-body — tools translated to Anthropic format
;; ============================================================

(define tool-def (hash 'type "function"
                       'function (hash 'name "bash"
                                       'description "Run a shell command"
                                       'parameters (hash 'type "object"
                                                         'properties (hash)))))

(define req-with-tools
  (make-model-request
   (list (hash 'role "user" 'content "Run ls"))
   (list tool-def)
   (hash 'model "claude-sonnet-4-20250514")))

(define body-tools (anthropic-build-request-body req-with-tools))
(check-true (hash-has-key? body-tools 'tools)
            "tools key present")
(check-pred list? (hash-ref body-tools 'tools)
            "tools is a list")
(check-equal? (length (hash-ref body-tools 'tools)) 1
              "one tool")

(define anthropic-tool (car (hash-ref body-tools 'tools)))
(check-equal? (hash-ref anthropic-tool 'name) "bash"
              "tool name preserved")
(check-equal? (hash-ref anthropic-tool 'description) "Run a shell command"
              "tool description preserved")
(check-true (hash-has-key? anthropic-tool 'input_schema)
            "tool has input_schema (not parameters)")
(check-false (hash-has-key? anthropic-tool 'function)
             "tool does NOT have 'function' key")

;; ============================================================
;; 4. anthropic-parse-response — basic text response
;; ============================================================

(define fake-text-response
  (hash 'id "msg_abc123"
        'type "message"
        'role "assistant"
        'content (list (hash 'type "text" 'text "Hello from Claude!"))
        'model "claude-sonnet-4-20250514"
        'stop_reason "end_turn"
        'usage (hash 'input_tokens 10 'output_tokens 5)))

(define parsed-text (anthropic-parse-response fake-text-response))
(check-pred model-response? parsed-text
            "parsed text response is model-response?")
(check-equal? (model-response-model parsed-text) "claude-sonnet-4-20250514"
              "model name preserved")
(check-equal? (model-response-stop-reason parsed-text) 'stop
              "end_turn → stop")
(check-equal? (length (model-response-content parsed-text)) 1
              "one content block")

(define text-content (car (model-response-content parsed-text)))
(check-equal? (hash-ref text-content 'type) "text"
              "content type is text")
(check-equal? (hash-ref text-content 'text) "Hello from Claude!"
              "text content preserved")

;; ============================================================
;; 5. anthropic-parse-response — tool_use content blocks
;; ============================================================

(define fake-tool-response
  (hash 'id "msg_def456"
        'type "message"
        'role "assistant"
        'content (list (hash 'type "tool_use"
                             'id "toolu_123"
                             'name "bash"
                             'input (hash 'command "ls -la")))
        'model "claude-sonnet-4-20250514"
        'stop_reason "tool_use"
        'usage (hash 'input_tokens 20 'output_tokens 15)))

(define parsed-tool (anthropic-parse-response fake-tool-response))
(check-equal? (model-response-stop-reason parsed-tool) 'tool-calls
              "tool_use → tool-calls")

(define tool-content (car (model-response-content parsed-tool)))
(check-equal? (hash-ref tool-content 'type) "tool-call"
              "tool_use → tool-call type")
(check-equal? (hash-ref tool-content 'id) "toolu_123"
              "tool id preserved")
(check-equal? (hash-ref tool-content 'name) "bash"
              "tool name preserved")
(check-equal? (hash-ref (hash-ref tool-content 'arguments) 'command) "ls -la"
              "tool input → arguments")

;; ============================================================
;; 6. anthropic-parse-response — usage translation
;; ============================================================

(define parsed-usage-text (anthropic-parse-response fake-text-response))
(define usage-result (model-response-usage parsed-usage-text))
(check-equal? (hash-ref usage-result 'prompt_tokens) 10
              "input_tokens → prompt_tokens")
(check-equal? (hash-ref usage-result 'completion_tokens) 5
              "output_tokens → completion_tokens")
(check-equal? (hash-ref usage-result 'total_tokens) 15
              "total_tokens = input + output")

;; ============================================================
;; 7. anthropic-parse-response — stop_reason translation
;; ============================================================

;; end_turn → stop (already tested above)

;; max_tokens → length
(define fake-max-response
  (hash 'id "msg_max"
        'type "message"
        'role "assistant"
        'content (list (hash 'type "text" 'text "truncated"))
        'model "claude-sonnet-4-20250514"
        'stop_reason "max_tokens"
        'usage (hash 'input_tokens 10 'output_tokens 4096)))
(check-equal? (model-response-stop-reason
               (anthropic-parse-response fake-max-response))
              'length
              "max_tokens → length")

;; tool_use → tool-calls (already tested above)

;; stop_sequence → stop
(define fake-stop-seq-response
  (hash 'id "msg_stopseq"
        'type "message"
        'role "assistant"
        'content (list (hash 'type "text" 'text "stopped"))
        'model "claude-sonnet-4-20250514"
        'stop_reason "stop_sequence"
        'usage (hash 'input_tokens 10 'output_tokens 3)))
(check-equal? (model-response-stop-reason
               (anthropic-parse-response fake-stop-seq-response))
              'stop
              "stop_sequence → stop")

;; ============================================================
;; 8. make-anthropic-provider — returns provider?
;; ============================================================

(define anthropic-config
  (hash 'api-key "test-key-123"
        'model "claude-sonnet-4-20250514"))

(define anthropic-provider (make-anthropic-provider anthropic-config))
(check-pred provider? anthropic-provider
            "make-anthropic-provider returns provider?")

;; ============================================================
;; 9. make-anthropic-provider — provider-name is "anthropic"
;; ============================================================

(check-equal? (provider-name anthropic-provider) "anthropic"
              "provider name is 'anthropic'")

;; Capabilities
(define anthropic-caps (provider-capabilities anthropic-provider))
(check-pred hash? anthropic-caps)
(check-true (hash-ref anthropic-caps 'streaming)
            "streaming capability is #t")
(check-false (hash-ref anthropic-caps 'token-counting)
             "token-counting capability is #f")

;; ============================================================
;; 10. anthropic-parse-stream-chunks — content_block_delta → text delta
;; ============================================================

(define stream-text-events
  (list
   (hash 'type "message_start"
         'message (hash 'id "msg_stream"
                        'role "assistant"
                        'model "claude-sonnet-4-20250514"
                        'usage (hash 'input_tokens 8 'output_tokens 0)))
   (hash 'type "content_block_start"
         'index 0
         'content_block (hash 'type "text" 'text ""))
   (hash 'type "content_block_delta"
         'index 0
         'delta (hash 'type "text_delta" 'text "Hello"))
   (hash 'type "content_block_delta"
         'index 0
         'delta (hash 'type "text_delta" 'text " world"))
   (hash 'type "content_block_stop"
         'index 0)))

(define stream-text-chunks (anthropic-parse-stream-chunks stream-text-events))
(check-pred list? stream-text-chunks
            "stream parsing returns list")
(check-true (andmap stream-chunk? stream-text-chunks)
            "all results are stream-chunk?")

;; Find text delta chunks (not usage-only chunks)
(define text-deltas
  (filter (lambda (c) (stream-chunk-delta-text c)) stream-text-chunks))
(check-equal? (length text-deltas) 2
              "two text deltas")
(check-equal? (stream-chunk-delta-text (car text-deltas)) "Hello")
(check-equal? (stream-chunk-delta-text (cadr text-deltas)) " world")

;; ============================================================
;; 11. anthropic-parse-stream-chunks — message_delta → usage + done
;; ============================================================

(define stream-done-events
  (list
   (hash 'type "content_block_delta"
         'index 0
         'delta (hash 'type "text_delta" 'text "Hi"))
   (hash 'type "message_delta"
         'delta (hash 'stop_reason "end_turn")
         'usage (hash 'output_tokens 5))))

(define stream-done-chunks (anthropic-parse-stream-chunks stream-done-events))
(define done-chunk (last stream-done-chunks))
(check-pred stream-chunk-done? done-chunk
            "message_delta with stop_reason → done? #t")
(check-pred hash? (stream-chunk-usage done-chunk)
            "message_delta carries usage")
(check-equal? (hash-ref (stream-chunk-usage done-chunk) 'completion_tokens) 5
              "output_tokens → completion_tokens in stream")

;; ============================================================
;; 12. anthropic-parse-stream-chunks — tool_use content block → tool-call stream-chunk
;; ============================================================

(define stream-tool-events
  (list
   (hash 'type "content_block_start"
         'index 1
         'content_block (hash 'type "tool_use"
                              'id "toolu_abc"
                              'name "bash"))
   (hash 'type "content_block_delta"
         'index 1
         'delta (hash 'type "input_json_delta" 'partial_json "{\"comm"))
   (hash 'type "content_block_delta"
         'index 1
         'delta (hash 'type "input_json_delta" 'partial_json "and\":\"ls\"}"))
   (hash 'type "message_delta"
         'delta (hash 'stop_reason "tool_use")
         'usage (hash 'output_tokens 20))))

(define stream-tool-chunks (anthropic-parse-stream-chunks stream-tool-events))
(check-pred list? stream-tool-chunks)

;; Find tool-call deltas
(define tool-deltas
  (filter (lambda (c) (stream-chunk-delta-tool-call c)) stream-tool-chunks))
(check-equal? (length tool-deltas) 2
              "two tool input deltas")
(check-equal? (hash-ref (stream-chunk-delta-tool-call (car tool-deltas)) 'id)
              "toolu_abc"
              "tool call id preserved in stream")

;; Last chunk is done
(define tool-done (last stream-tool-chunks))
(check-pred stream-chunk-done? tool-done
            "tool_use stream ends with done chunk")

;; ============================================================
;; 13. make-anthropic-provider — with custom base-url
;; ============================================================

(define anthropic-custom-config
  (hash 'api-key "test-key-456"
        'base-url "https://custom-proxy.example.com"
        'model "claude-sonnet-4-20250514"))

(define anthropic-custom-provider
  (make-anthropic-provider anthropic-custom-config))
(check-pred provider? anthropic-custom-provider
            "custom base-url provider is provider?")
(check-equal? (provider-name anthropic-custom-provider) "anthropic"
              "custom provider name is still 'anthropic'")

;; ============================================================
;; 14. anthropic-parse-response — multiple content blocks (text + tool_use)
;; ============================================================

(define fake-multi-response
  (hash 'id "msg_multi"
        'type "message"
        'role "assistant"
        'content (list
                  (hash 'type "text" 'text "I'll run that command.")
                  (hash 'type "tool_use"
                        'id "toolu_multi1"
                        'name "bash"
                        'input (hash 'command "ls"))
                  (hash 'type "tool_use"
                        'id "toolu_multi2"
                        'name "read"
                        'input (hash 'path "/tmp/test.txt")))
        'model "claude-sonnet-4-20250514"
        'stop_reason "tool_use"
        'usage (hash 'input_tokens 50 'output_tokens 30)))

(define parsed-multi (anthropic-parse-response fake-multi-response))
(check-equal? (length (model-response-content parsed-multi)) 3
              "three content blocks preserved")

(define first-block (car (model-response-content parsed-multi)))
(check-equal? (hash-ref first-block 'type) "text")

(define second-block (cadr (model-response-content parsed-multi)))
(check-equal? (hash-ref second-block 'type) "tool-call")
(check-equal? (hash-ref second-block 'name) "bash")

(define third-block (caddr (model-response-content parsed-multi)))
(check-equal? (hash-ref third-block 'type) "tool-call")
(check-equal? (hash-ref third-block 'name) "read")

;; Usage totals correct
(define multi-usage (model-response-usage parsed-multi))
(check-equal? (hash-ref multi-usage 'total_tokens) 80
              "total_tokens = 50 + 30")

;; ============================================================
;; 15. anthropic-build-request-body — model from settings
;; ============================================================

(define req-different-model
  (make-model-request
   (list (hash 'role "user" 'content "Hello"))
   #f
   (hash 'model "claude-haiku-4-20250414")))

(define body-diff-model (anthropic-build-request-body req-different-model))
(check-equal? (hash-ref body-diff-model 'model) "claude-haiku-4-20250414"
              "model from settings overrides default")

;; No model in settings → uses default
(define req-no-model
  (make-model-request
   (list (hash 'role "user" 'content "Hello"))
   #f
   (hash)))

(define body-no-model (anthropic-build-request-body req-no-model))
(check-equal? (hash-ref body-no-model 'model) "claude-sonnet-4-20250514"
              "default model when not in settings")

;; ============================================================
;; 16. anthropic-build-request-body — streaming flag
;; ============================================================

(define body-stream (anthropic-build-request-body req-basic #:stream? #t))
(check-equal? (hash-ref body-stream 'stream) #t
              "stream flag set to #t")

;; ============================================================
;; 17. anthropic-build-request-body — temperature passthrough
;; ============================================================

(define req-with-temp
  (make-model-request
   (list (hash 'role "user" 'content "Hello"))
   #f
   (hash 'model "claude-sonnet-4-20250514" 'temperature 0.5)))

(define body-temp (anthropic-build-request-body req-with-temp))
(check-equal? (hash-ref body-temp 'temperature) 0.5
              "temperature passed through")

;; ============================================================
;; 18. anthropic-build-request-body — system prompt from settings
;; ============================================================

(define req-with-system
  (make-model-request
   (list (hash 'role "user" 'content "Hello"))
   #f
   (hash 'model "claude-sonnet-4-20250514" 'system "You are a helpful assistant.")))

(define body-system (anthropic-build-request-body req-with-system))
(check-equal? (hash-ref body-system 'system) "You are a helpful assistant."
              "system prompt passed through")

;; ============================================================
;; 19. anthropic-build-request-body — message content wrapped in typed blocks
;; ============================================================

(define body-msg-check (anthropic-build-request-body req-basic))
(define first-msg (car (hash-ref body-msg-check 'messages)))
(check-equal? (hash-ref first-msg 'role) "user")
;; Content should be a list of typed blocks
(define msg-content (hash-ref first-msg 'content))
(check-pred list? msg-content
            "message content is a list of typed blocks")
(check-equal? (hash-ref (car msg-content) 'type) "text"
              "content block type is text")
(check-equal? (hash-ref (car msg-content) 'text) "Hello"
              "content block text preserved")

;; ============================================================
;; 20. anthropic-parse-response — empty content
;; ============================================================

(define fake-empty-response
  (hash 'id "msg_empty"
        'type "message"
        'role "assistant"
        'content '()
        'model "claude-sonnet-4-20250514"
        'stop_reason "end_turn"
        'usage (hash 'input_tokens 5 'output_tokens 0)))

(define parsed-empty (anthropic-parse-response fake-empty-response))
(check-equal? (length (model-response-content parsed-empty)) 0
              "empty content list preserved")

;; ============================================================
;; 21. anthropic-parse-stream-chunks — message_start with input usage
;; ============================================================

(define stream-with-usage
  (list
   (hash 'type "message_start"
         'message (hash 'id "msg_usage"
                        'role "assistant"
                        'model "claude-sonnet-4-20250514"
                        'usage (hash 'input_tokens 42 'output_tokens 0)))))

(define stream-usage-chunks (anthropic-parse-stream-chunks stream-with-usage))
(check-equal? (length stream-usage-chunks) 1
              "message_start with usage → 1 chunk")
(define usage-start-chunk (car stream-usage-chunks))
(check-pred hash? (stream-chunk-usage usage-start-chunk)
            "message_start chunk has usage")
(check-equal? (hash-ref (stream-chunk-usage usage-start-chunk) 'prompt_tokens) 42
              "input_tokens → prompt_tokens in stream start")

;; ============================================================
;; 22-26. Error Handling Tests — HTTP status codes
;; ============================================================

(test-case
 "HTTP 200 passes without error"
 (check-not-exn
  (λ () (anthropic-check-http-status! #"HTTP/1.1 200 OK" #"{}"))))

(test-case
 "HTTP 401 raises authentication error"
 (check-exn
  #rx"authentication failed [(]401[)]"
  (λ () (anthropic-check-http-status! #"HTTP/1.1 401 Unauthorized"
                                      #"{\"error\":{\"type\":\"authentication_error\",\"message\":\"Invalid API key\"}}"))))

(test-case
 "HTTP 403 raises forbidden error"
 (check-exn
  #rx"forbidden [(]403[)]"
  (λ () (anthropic-check-http-status! #"HTTP/1.1 403 Forbidden"
                                      #"{\"error\":{\"type\":\"permission_error\",\"message\":\"Access denied\"}}"))))

(test-case
 "HTTP 429 raises rate limit error"
 (check-exn
  #rx"rate limited [(]429[)]"
  (λ () (anthropic-check-http-status! #"HTTP/1.1 429 Too Many Requests"
                                      #"{\"error\":{\"type\":\"rate_limit_error\",\"message\":\"Rate limited\"}}"))))

(test-case
 "HTTP 500 raises server error"
 (check-exn
  #rx"server error [(]500[)]"
  (λ () (anthropic-check-http-status! #"HTTP/1.1 500 Internal Server Error"
                                      #"{\"error\":{\"type\":\"api_error\",\"message\":\"Internal error\"}}"))))

(test-case
 "HTTP 502 raises server error"
 (check-exn
  #rx"server error [(]502[)]"
  (λ () (anthropic-check-http-status! #"HTTP/1.1 502 Bad Gateway"
                                      #"Bad Gateway"))))

(test-case
 "HTTP 400 raises generic error"
 (check-exn
  #rx"error [(]400[)]"
  (λ () (anthropic-check-http-status! #"HTTP/1.1 400 Bad Request"
                                      #"{\"error\":{\"type\":\"invalid_request_error\"}}"))))

(test-case
 "String status-line also works"
 (check-not-exn
  (λ () (anthropic-check-http-status! "HTTP/1.1 200 OK" "{}"))))

;; ============================================================
;; 27. SSE Streaming format with parse-sse-lines integration
;; ============================================================

(define fake-sse-text
  (string-append
   "event: message_start\ndata: {\"type\":\"message_start\",\"message\":{\"id\":\"msg_123\",\"role\":\"assistant\",\"model\":\"claude-sonnet\","
   "\"usage\":{\"input_tokens\":10}}}\n\n"
   "event: content_block_delta\ndata: {\"type\":\"content_block_delta\",\"index\":0,\"delta\":{\"type\":\"text_delta\",\"text\":\"Hello\"}}\n\n"
   "event: content_block_delta\ndata: {\"type\":\"content_block_delta\",\"index\":0,\"delta\":{\"type\":\"text_delta\",\"text\":\" world\"}}\n\n"
   "event: message_delta\ndata: {\"type\":\"message_delta\",\"delta\":{\"stop_reason\":\"end_turn\"},\"usage\":{\"output_tokens\":5}}\n\n"
   "event: message_stop\ndata: {\"type\":\"message_stop\"}\n"))
(define parsed-sse-events (parse-sse-lines fake-sse-text))
(check-equal? (length parsed-sse-events) 5
              "SSE parsing extracts 5 data events")

(define sse-chunks (anthropic-parse-stream-chunks parsed-sse-events))
(check-true (>= (length sse-chunks) 3)
            "SSE stream produces at least 3 chunks (2 text + done)")

;; Find text chunks
(define sse-text-chunks
  (filter (lambda (c) (stream-chunk-delta-text c)) sse-chunks))
(check-equal? (length sse-text-chunks) 2
              "Two text deltas from SSE stream")

;; Verify concatenation
(define concatenated
  (apply string-append (map stream-chunk-delta-text sse-text-chunks)))
(check-equal? concatenated "Hello world"
              "Text deltas concatenate correctly")

;; ============================================================
;; 28. Tool translation helper
;; ============================================================

(define test-tool
  (hash 'type "function"
        'function (hash 'name "read_file"
                       'description "Read a file"
                       'parameters (hash 'type "object"
                                        'properties (hash 'path (hash 'type "string"))))))

(define anthropic-tool-result (anthropic-translate-tool test-tool))
(check-equal? (hash-ref anthropic-tool-result 'name) "read_file")
(check-equal? (hash-ref anthropic-tool-result 'description) "Read a file")
(check-true (hash-has-key? anthropic-tool-result 'input_schema))
(check-false (hash-has-key? anthropic-tool-result 'function))

;; ============================================================
;; 29. Stop reason translation helper
;; ============================================================

(check-equal? (anthropic-translate-stop-reason "end_turn") 'stop)
(check-equal? (anthropic-translate-stop-reason "max_tokens") 'length)
(check-equal? (anthropic-translate-stop-reason "stop_sequence") 'stop)
(check-equal? (anthropic-translate-stop-reason "tool_use") 'tool-calls)
(check-equal? (anthropic-translate-stop-reason "unknown_reason") 'unknown_reason)
(check-equal? (anthropic-translate-stop-reason 'already-symbol) 'already-symbol)
(check-equal? (anthropic-translate-stop-reason 123) 'stop)

(println "All Anthropic provider tests passed!")

;; ============================================================
;; 30. read-response-body — size limit enforcement (SEC-10)
;; ============================================================

(test-case
 "read-response-body reads normal-sized responses"
 (define port (open-input-string "Hello World"))
 (define result (read-response-body port))
 (check-equal? result (string->bytes/utf-8 "Hello World")))

(test-case
 "read-response-body rejects oversized responses (>10 MB)"
 ;; Create a port that returns more than max-response-size bytes
 (define overflow-size (+ max-response-size 1))
 (define buf (make-bytes 8192 65)) ; fill with 'A'
 (define total-read 0)
 (define port
   (make-input-port
    'overflow
    (lambda (b)
      ;; read-bytes-avail! callback
      (cond
        [(>= total-read overflow-size) eof]
        [else
         (define n (min 8192 (- overflow-size total-read)))
         (bytes-copy! b 0 buf 0 n)
         (set! total-read (+ total-read n))
         n]))
    #f
    void))
 (check-exn
  #rx"exceeds maximum size limit"
  (lambda () (read-response-body port))))

(test-case
 "max-response-size is 10 MB"
 (check-equal? max-response-size (* 10 1024 1024)))
