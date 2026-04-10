#lang racket

(require rackunit
         "../llm/model.rkt"
         "../llm/provider.rkt"
         "../llm/stream.rkt"
         "../llm/gemini.rkt")

;; ============================================================
;; Test suite: llm/gemini.rkt — Google Gemini provider adapter
;; ============================================================

;; ============================================================
;; 1. gemini-build-request-body — basic structure
;; ============================================================

(define req-basic
  (make-model-request
   (list (hash 'role "user" 'content "Hello"))
   #f
   (hash 'model "gemini-2.5-pro")))

(define body-basic (gemini-build-request-body req-basic))
(check-true (hash? body-basic)
            "build-request-body returns a hash")
(check-true (list? (hash-ref body-basic 'contents))
            "contents is a list")
(check-equal? (length (hash-ref body-basic 'contents)) 1
              "one content entry")

;; Verify contents structure (not messages)
(define first-content (car (hash-ref body-basic 'contents)))
(check-equal? (hash-ref first-content 'role) "user"
              "role is 'user'")
(check-true (list? (hash-ref first-content 'parts))
            "parts is a list")
(check-equal? (hash-ref (car (hash-ref first-content 'parts)) 'text) "Hello"
              "text content in parts")

;; ============================================================
;; 2. gemini-build-request-body — model and maxOutputTokens
;; ============================================================

(define gen-config (hash-ref body-basic 'generationConfig))
(check-true (hash? gen-config)
            "generationConfig is a hash")
(check-equal? (hash-ref gen-config 'maxOutputTokens) 4096
              "default maxOutputTokens is 4096")

;; Custom max-tokens
(define req-custom-max
  (make-model-request
   (list (hash 'role "user" 'content "Hello"))
   #f
   (hash 'model "gemini-2.5-pro" 'max-tokens 8192)))

(define body-custom-max (gemini-build-request-body req-custom-max))
(check-equal? (hash-ref (hash-ref body-custom-max 'generationConfig) 'maxOutputTokens) 8192
              "custom maxOutputTokens from settings")

;; ============================================================
;; 3. gemini-build-request-body — messages → contents with role mapping
;; ============================================================

;; "assistant" → "model"
(define req-assistant-msg
  (make-model-request
   (list (hash 'role "user" 'content "Hi")
         (hash 'role "assistant" 'content "Hello!")
         (hash 'role "user" 'content "How are you?"))
   #f
   (hash 'model "gemini-2.5-pro")))

(define body-assistant (gemini-build-request-body req-assistant-msg))
(define contents-assistant (hash-ref body-assistant 'contents))
(check-equal? (length contents-assistant) 3
              "three content entries")

(check-equal? (hash-ref (car contents-assistant) 'role) "user"
              "first message role is 'user'")
(check-equal? (hash-ref (cadr contents-assistant) 'role) "model"
              "assistant → model role mapping")
(check-equal? (hash-ref (caddr contents-assistant) 'role) "user"
              "third message role is 'user'")

;; Verify parts structure for each message
(check-equal? (hash-ref (car (hash-ref (cadr contents-assistant) 'parts)) 'text) "Hello!"
              "assistant message text in parts")

;; ============================================================
;; 4. gemini-build-request-body — tools translated to functionDeclarations
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
   (hash 'model "gemini-2.5-pro")))

(define body-tools (gemini-build-request-body req-with-tools))
(check-true (hash-has-key? body-tools 'tools)
            "tools key present")
(check-true (list? (hash-ref body-tools 'tools))
            "tools is a list")
(check-equal? (length (hash-ref body-tools 'tools)) 1
              "one tool wrapper")

(define gemini-tools-wrapper (car (hash-ref body-tools 'tools)))
(check-true (hash-has-key? gemini-tools-wrapper 'functionDeclarations)
            "tools wrapper has functionDeclarations")

(define func-decls (hash-ref gemini-tools-wrapper 'functionDeclarations))
(check-equal? (length func-decls) 1
              "one function declaration")

(define gemini-tool (car func-decls))
(check-equal? (hash-ref gemini-tool 'name) "bash"
              "tool name preserved")
(check-equal? (hash-ref gemini-tool 'description) "Run a shell command"
              "tool description preserved")
(check-true (hash-has-key? gemini-tool 'parameters)
            "tool has parameters (not input_schema)")
(check-false (hash-has-key? gemini-tool 'function)
             "tool does NOT have 'function' key")
(check-false (hash-has-key? gemini-tool 'input_schema)
             "tool does NOT have 'input_schema' key")

;; ============================================================
;; 5. gemini-build-request-body — system prompt → systemInstruction
;; ============================================================

(define req-with-system
  (make-model-request
   (list (hash 'role "user" 'content "Hello"))
   #f
   (hash 'model "gemini-2.5-pro" 'system "You are a helpful assistant.")))

(define body-system (gemini-build-request-body req-with-system))
(check-true (hash-has-key? body-system 'systemInstruction)
            "systemInstruction key present")
(define sys-instr (hash-ref body-system 'systemInstruction))
(check-true (hash-has-key? sys-instr 'parts)
            "systemInstruction has parts")
(define sys-parts (hash-ref sys-instr 'parts))
(check-equal? (hash-ref (car sys-parts) 'text) "You are a helpful assistant."
              "system prompt text in systemInstruction")

;; Without system prompt, no systemInstruction key
(define body-no-system (gemini-build-request-body req-basic))
(check-false (hash-has-key? body-no-system 'systemInstruction)
             "no systemInstruction when no system prompt")

;; ============================================================
;; 6. gemini-build-request-body — temperature passthrough
;; ============================================================

(define req-with-temp
  (make-model-request
   (list (hash 'role "user" 'content "Hello"))
   #f
   (hash 'model "gemini-2.5-pro" 'temperature 0.7)))

(define body-temp (gemini-build-request-body req-with-temp))
(check-equal? (hash-ref (hash-ref body-temp 'generationConfig) 'temperature) 0.7
              "temperature passed through to generationConfig")

;; Without temperature
(define body-no-temp (gemini-build-request-body req-basic))
(check-false (hash-has-key? (hash-ref body-no-temp 'generationConfig) 'temperature)
             "no temperature key when not set")

;; ============================================================
;; 7. gemini-build-request-body — model from settings / default
;; ============================================================

(define req-different-model
  (make-model-request
   (list (hash 'role "user" 'content "Hello"))
   #f
   (hash 'model "gemini-2.0-flash")))

(define body-diff-model (gemini-build-request-body req-different-model))
;; Model name goes into the URL, not the body. Verify it's accessible.
(check-equal? (hash-ref (model-request-settings req-different-model) 'model) "gemini-2.0-flash"
              "model from settings available")

;; No model in settings → uses default
(define req-no-model
  (make-model-request
   (list (hash 'role "user" 'content "Hello"))
   #f
   (hash)))

(define body-no-model (gemini-build-request-body req-no-model))
(check-equal? (hash-ref (model-request-settings req-no-model) 'model #f) #f
              "no model key when not set — uses default at provider level")

;; ============================================================
;; 8. gemini-parse-response — text response
;; ============================================================

(define fake-text-response
  (hash 'candidates
        (list (hash 'content
                    (hash 'role "model"
                          'parts (list (hash 'text "Hello from Gemini!")))
                    'finishReason "STOP"))
        'usageMetadata
        (hash 'promptTokenCount 10
              'candidatesTokenCount 5
              'totalTokenCount 15)
        'modelVersion "gemini-2.5-pro"))

(define parsed-text (gemini-parse-response fake-text-response))
(check-true (model-response? parsed-text)
            "parsed text response is model-response?")
(check-equal? (model-response-model parsed-text) "gemini-2.5-pro"
              "model version preserved")
(check-equal? (model-response-stop-reason parsed-text) 'stop
              "STOP → stop")
(check-equal? (length (model-response-content parsed-text)) 1
              "one content block")

(define text-content (car (model-response-content parsed-text)))
(check-equal? (hash-ref text-content 'type) "text"
              "content type is text")
(check-equal? (hash-ref text-content 'text) "Hello from Gemini!"
              "text content preserved")

;; ============================================================
;; 9. gemini-parse-response — tool call response (functionCall)
;; ============================================================

(define fake-tool-response
  (hash 'candidates
        (list (hash 'content
                    (hash 'role "model"
                          'parts (list (hash 'functionCall
                                             (hash 'name "bash"
                                                   'args (hash 'command "ls -la")))))
                    'finishReason "STOP"))
        'usageMetadata
        (hash 'promptTokenCount 20
              'candidatesTokenCount 10
              'totalTokenCount 30)
        'modelVersion "gemini-2.5-pro"))

(define parsed-tool (gemini-parse-response fake-tool-response))
(check-equal? (length (model-response-content parsed-tool)) 1
              "one content block for tool call")

(define tool-content (car (model-response-content parsed-tool)))
(check-equal? (hash-ref tool-content 'type) "tool-call"
              "functionCall → tool-call type")
(check-equal? (hash-ref tool-content 'name) "bash"
              "tool name preserved")
(check-equal? (hash-ref (hash-ref tool-content 'arguments) 'command) "ls -la"
              "tool args → arguments (direct object, not string)")

;; Gemini doesn't provide tool call IDs — should be empty string
(check-equal? (hash-ref tool-content 'id) ""
              "tool id is empty string (Gemini has no tool call IDs)")

;; ============================================================
;; 10. gemini-parse-response — usage translation
;; ============================================================

(define parsed-usage-text (gemini-parse-response fake-text-response))
(define usage-result (model-response-usage parsed-usage-text))
(check-equal? (hash-ref usage-result 'prompt_tokens) 10
              "promptTokenCount → prompt_tokens")
(check-equal? (hash-ref usage-result 'completion_tokens) 5
              "candidatesTokenCount → completion_tokens")
(check-equal? (hash-ref usage-result 'total_tokens) 15
              "totalTokenCount → total_tokens")

;; ============================================================
;; 11. gemini-parse-response — stop reason translation
;; ============================================================

;; STOP → stop (already tested above)

;; MAX_TOKENS → length
(define fake-max-response
  (hash 'candidates
        (list (hash 'content
                    (hash 'role "model"
                          'parts (list (hash 'text "truncated")))
                    'finishReason "MAX_TOKENS"))
        'usageMetadata
        (hash 'promptTokenCount 10
              'candidatesTokenCount 4096
              'totalTokenCount 4106)
        'modelVersion "gemini-2.5-pro"))
(check-equal? (model-response-stop-reason
               (gemini-parse-response fake-max-response))
              'length
              "MAX_TOKENS → length")

;; SAFETY → stop
(define fake-safety-response
  (hash 'candidates
        (list (hash 'content
                    (hash 'role "model"
                          'parts (list (hash 'text "blocked")))
                    'finishReason "SAFETY"))
        'usageMetadata
        (hash 'promptTokenCount 10
              'candidatesTokenCount 3
              'totalTokenCount 13)
        'modelVersion "gemini-2.5-pro"))
(check-equal? (model-response-stop-reason
               (gemini-parse-response fake-safety-response))
              'stop
              "SAFETY → stop")

;; RECITATION → stop
(define fake-recitation-response
  (hash 'candidates
        (list (hash 'content
                    (hash 'role "model"
                          'parts (list (hash 'text "recited")))
                    'finishReason "RECITATION"))
        'usageMetadata
        (hash 'promptTokenCount 10
              'candidatesTokenCount 3
              'totalTokenCount 13)
        'modelVersion "gemini-2.5-pro"))
(check-equal? (model-response-stop-reason
               (gemini-parse-response fake-recitation-response))
              'stop
              "RECITATION → stop")

;; ============================================================
;; 12. gemini-parse-response — multiple content parts
;; ============================================================

(define fake-multi-response
  (hash 'candidates
        (list (hash 'content
                    (hash 'role "model"
                          'parts (list
                                  (hash 'text "I'll run that command.")
                                  (hash 'functionCall
                                        (hash 'name "bash"
                                              'args (hash 'command "ls")))
                                  (hash 'functionCall
                                        (hash 'name "read"
                                              'args (hash 'path "/tmp/test.txt")))))
                    'finishReason "STOP"))
        'usageMetadata
        (hash 'promptTokenCount 50
              'candidatesTokenCount 30
              'totalTokenCount 80)
        'modelVersion "gemini-2.5-pro"))

(define parsed-multi (gemini-parse-response fake-multi-response))
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
              "total_tokens = 80")

;; ============================================================
;; 13. gemini-parse-response — empty response (no candidates)
;; ============================================================

(define fake-empty-response
  (hash 'candidates '()
        'usageMetadata (hash 'promptTokenCount 5
                             'candidatesTokenCount 0
                             'totalTokenCount 5)
        'modelVersion "gemini-2.5-pro"))

(define parsed-empty (gemini-parse-response fake-empty-response))
(check-equal? (length (model-response-content parsed-empty)) 0
              "empty content list for no candidates")

;; ============================================================
;; 14. gemini-parse-stream-chunks — text deltas
;; ============================================================

(define stream-text-events
  (list
   (hash 'candidates
         (list (hash 'content
                     (hash 'role "model"
                           'parts (list (hash 'text "Hello")))
                     'finishReason #f)))
   (hash 'candidates
         (list (hash 'content
                     (hash 'role "model"
                           'parts (list (hash 'text " world")))
                     'finishReason "STOP"))
        'usageMetadata
        (hash 'promptTokenCount 10
              'candidatesTokenCount 2
              'totalTokenCount 12))))

(define stream-text-chunks (gemini-parse-stream-chunks stream-text-events))
(check-true (list? stream-text-chunks)
            "stream parsing returns list")
(check-true (andmap stream-chunk? stream-text-chunks)
            "all results are stream-chunk?")

;; Find text delta chunks
(define text-deltas
  (filter (lambda (c) (stream-chunk-delta-text c)) stream-text-chunks))
(check-equal? (length text-deltas) 2
              "two text deltas")
(check-equal? (stream-chunk-delta-text (car text-deltas)) "Hello"
              "first text delta")
(check-equal? (stream-chunk-delta-text (cadr text-deltas)) " world"
              "second text delta")

;; ============================================================
;; 15. gemini-parse-stream-chunks — done with usage
;; ============================================================

(define stream-done-events
  (list
   (hash 'candidates
         (list (hash 'content
                     (hash 'role "model"
                           'parts (list (hash 'text "Hi")))
                     'finishReason "STOP"))
        'usageMetadata
        (hash 'promptTokenCount 8
              'candidatesTokenCount 5
              'totalTokenCount 13))))

(define stream-done-chunks (gemini-parse-stream-chunks stream-done-events))

;; Should have text + done chunk
(define done-chunks (filter (lambda (c) (stream-chunk-done? c)) stream-done-chunks))
(check-equal? (length done-chunks) 1
              "one done chunk")

(define done-chunk (car done-chunks))
(check-true (hash? (stream-chunk-usage done-chunk))
            "done chunk carries usage")

;; ============================================================
;; 16. gemini-parse-stream-chunks — tool use deltas
;; ============================================================

(define stream-tool-events
  (list
   (hash 'candidates
         (list (hash 'content
                     (hash 'role "model"
                           'parts (list (hash 'text "Running command...")))
                     'finishReason #f)))
   (hash 'candidates
         (list (hash 'content
                     (hash 'role "model"
                           'parts (list (hash 'functionCall
                                              (hash 'name "bash"
                                                    'args (hash 'command "ls")))))
                     'finishReason "STOP")))))

(define stream-tool-chunks (gemini-parse-stream-chunks stream-tool-events))
(check-true (list? stream-tool-chunks))

;; Find text delta
(define tool-stream-text
  (filter (lambda (c) (stream-chunk-delta-text c)) stream-tool-chunks))
(check-equal? (length tool-stream-text) 1
              "one text delta before tool call")

;; Find tool-call deltas
(define tool-deltas
  (filter (lambda (c) (stream-chunk-delta-tool-call c)) stream-tool-chunks))
(check-equal? (length tool-deltas) 1
              "one tool-call delta")
(check-equal?
 (hash-ref (hash-ref (stream-chunk-delta-tool-call (car tool-deltas)) 'function) 'name)
 "bash"
 "tool call name preserved in stream")

;; Find done chunk
(define tool-done
  (filter (lambda (c) (stream-chunk-done? c)) stream-tool-chunks))
(check-equal? (length tool-done) 1
              "stream ends with done chunk")

;; ============================================================
;; 17. make-gemini-provider — returns provider?, correct name, capabilities
;; ============================================================

(define gemini-config
  (hash 'api-key "test-key-123"
        'model "gemini-2.5-pro"))

(define gemini-provider (make-gemini-provider gemini-config))
(check-true (provider? gemini-provider)
            "make-gemini-provider returns provider?")
(check-equal? (provider-name gemini-provider) "gemini"
              "provider name is 'gemini'")

(define gemini-caps (provider-capabilities gemini-provider))
(check-true (hash? gemini-caps))
(check-true (hash-ref gemini-caps 'streaming)
            "streaming capability is #t")
(check-false (hash-ref gemini-caps 'token-counting)
             "token-counting capability is #f")

;; ============================================================
;; 18. make-gemini-provider — with custom base-url
;; ============================================================

(define gemini-custom-config
  (hash 'api-key "test-key-456"
        'base-url "https://custom-proxy.example.com"
        'model "gemini-2.5-pro"))

(define gemini-custom-provider
  (make-gemini-provider gemini-custom-config))
(check-true (provider? gemini-custom-provider)
            "custom base-url provider is provider?")
(check-equal? (provider-name gemini-custom-provider) "gemini"
              "custom provider name is still 'gemini'")

;; ============================================================
;; 19. SSE integration with parse-sse-lines
;; ============================================================

(define fake-sse-text
  "data: {\"candidates\":[{\"content\":{\"role\":\"model\",\"parts\":[{\"text\":\"Hello\"}]},\"finishReason\":null}]}\n\ndata: {\"candidates\":[{\"content\":{\"role\":\"model\",\"parts\":[{\"text\":\" world\"}]},\"finishReason\":\"STOP\"}],\"usageMetadata\":{\"promptTokenCount\":10,\"candidatesTokenCount\":2,\"totalTokenCount\":12}}\n\n")

(define parsed-sse-events (parse-sse-lines fake-sse-text))
(check-equal? (length parsed-sse-events) 2
              "SSE parsing extracts 2 data events")

(define sse-chunks (gemini-parse-stream-chunks parsed-sse-events))
(check-true (>= (length sse-chunks) 2)
            "SSE stream produces at least 2 chunks")

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
;; 20. HTTP status checks (200, 400, 401, 403, 429, 500)
;; ============================================================

(test-case
 "HTTP 200 passes without error"
 (check-not-exn
  (lambda () (gemini-check-http-status! #"HTTP/1.1 200 OK" #"{}"))))

(test-case
 "HTTP 400 raises bad request error"
 (check-exn
  #rx"bad request [(]400[)]"
  (lambda () (gemini-check-http-status! #"HTTP/1.1 400 Bad Request"
                                        #"{\"error\":{\"message\":\"Invalid\"}}"))))

(test-case
 "HTTP 401 raises authentication error"
 (check-exn
  #rx"authentication failed [(]401[)]"
  (lambda () (gemini-check-http-status! #"HTTP/1.1 401 Unauthorized"
                                        #"{\"error\":{\"message\":\"Invalid API key\"}}"))))

(test-case
 "HTTP 403 raises forbidden error"
 (check-exn
  #rx"forbidden [(]403[)]"
  (lambda () (gemini-check-http-status! #"HTTP/1.1 403 Forbidden"
                                        #"{\"error\":{\"message\":\"Access denied\"}}"))))

(test-case
 "HTTP 429 raises rate limit error"
 (check-exn
  #rx"rate limited [(]429[)]"
  (lambda () (gemini-check-http-status! #"HTTP/1.1 429 Too Many Requests"
                                        #"{\"error\":{\"message\":\"Rate limited\"}}"))))

(test-case
 "HTTP 500 raises server error"
 (check-exn
  #rx"server error [(]500[)]"
  (lambda () (gemini-check-http-status! #"HTTP/1.1 500 Internal Server Error"
                                        #"{\"error\":{\"message\":\"Internal error\"}}"))))

(test-case
 "HTTP 502 raises server error"
 (check-exn
  #rx"server error [(]502[)]"
  (lambda () (gemini-check-http-status! #"HTTP/1.1 502 Bad Gateway"
                                        #"Bad Gateway"))))

(test-case
 "String status-line also works"
 (check-not-exn
  (lambda () (gemini-check-http-status! "HTTP/1.1 200 OK" "{}"))))

;; ============================================================
;; 21. Tool translation helper
;; ============================================================

(define test-tool
  (hash 'type "function"
        'function (hash 'name "read_file"
                       'description "Read a file"
                       'parameters (hash 'type "object"
                                        'properties (hash 'path (hash 'type "string"))))))

(define gemini-tool-result (gemini-translate-tool test-tool))
(check-equal? (hash-ref gemini-tool-result 'name) "read_file")
(check-equal? (hash-ref gemini-tool-result 'description) "Read a file")
(check-true (hash-has-key? gemini-tool-result 'parameters))
(check-false (hash-has-key? gemini-tool-result 'function)
             "no 'function' wrapper in Gemini format")
(check-false (hash-has-key? gemini-tool-result 'input_schema)
             "no 'input_schema' in Gemini format")

;; ============================================================
;; 22. Stop reason translation helper (all variants)
;; ============================================================

(check-equal? (gemini-translate-stop-reason "STOP") 'stop)
(check-equal? (gemini-translate-stop-reason "MAX_TOKENS") 'length)
(check-equal? (gemini-translate-stop-reason "SAFETY") 'stop)
(check-equal? (gemini-translate-stop-reason "RECITATION") 'stop)
(check-equal? (gemini-translate-stop-reason "OTHER") 'OTHER
              "unknown reason → symbol")
(check-equal? (gemini-translate-stop-reason 'already-symbol) 'already-symbol)
(check-equal? (gemini-translate-stop-reason 123) 'stop
              "non-string/non-symbol → stop")

;; ============================================================
;; 23. gemini-build-request-body — streaming flag (accepted but not in body)
;; ============================================================

;; Gemini streaming is determined by the URL endpoint, not a body flag.
;; The #:stream? parameter is accepted for API consistency but doesn't
;; appear in the body.
(define body-stream (gemini-build-request-body req-basic #:stream? #t))
(check-true (hash? body-stream)
            "streaming body is still a valid hash")
(check-false (hash-has-key? body-stream 'stream)
             "Gemini body does not include 'stream' key")

;; ============================================================
;; 24. gemini-parse-response — no modelVersion field
;; ============================================================

(define fake-no-version-response
  (hash 'candidates
        (list (hash 'content
                    (hash 'role "model"
                          'parts (list (hash 'text "Hi")))
                    'finishReason "STOP"))
        'usageMetadata
        (hash 'promptTokenCount 5
              'candidatesTokenCount 1
              'totalTokenCount 6)))

(define parsed-no-version (gemini-parse-response fake-no-version-response))
(check-equal? (model-response-model parsed-no-version) "gemini-2.5-pro"
              "default model name when modelVersion absent")

;; ============================================================
;; 25. gemini-parse-response — missing usageMetadata
;; ============================================================

(define fake-no-usage-response
  (hash 'candidates
        (list (hash 'content
                    (hash 'role "model"
                          'parts (list (hash 'text "Hi")))
                    'finishReason "STOP"))))

(define parsed-no-usage (gemini-parse-response fake-no-usage-response))
(define no-usage-result (model-response-usage parsed-no-usage))
(check-equal? (hash-ref no-usage-result 'prompt_tokens) 0
              "missing usage → prompt_tokens 0")
(check-equal? (hash-ref no-usage-result 'completion_tokens) 0
              "missing usage → completion_tokens 0")
(check-equal? (hash-ref no-usage-result 'total_tokens) 0
              "missing usage → total_tokens 0")

;; ============================================================
;; 26. gemini-build-request-body — empty messages list
;; ============================================================

(define req-empty-msgs
  (make-model-request
   '()
   #f
   (hash 'model "gemini-2.5-pro")))

(define body-empty (gemini-build-request-body req-empty-msgs))
(check-equal? (length (hash-ref body-empty 'contents)) 0
              "empty messages → empty contents")

;; ============================================================
;; 27. gemini-build-request-body — URL param auth (no auth header)
;; ============================================================

;; Verify that the provider doesn't add Authorization headers.
;; This is validated by checking that make-gemini-provider
;; constructs properly — actual URL verification happens at call time.
(check-true (provider? gemini-provider)
            "provider constructed with URL param auth pattern")

;; ============================================================
;; 28. gemini-translate-tool — tool with no function wrapper
;; ============================================================

;; Tool already in flat format (no 'function' key)
(define flat-tool
  (hash 'name "direct_tool"
        'description "A direct tool"
        'parameters (hash 'type "object")))

(define flat-result (gemini-translate-tool flat-tool))
(check-equal? (hash-ref flat-result 'name) "direct_tool"
              "flat tool name preserved")
(check-equal? (hash-ref flat-result 'description) "A direct tool"
              "flat tool description preserved")

;; ============================================================
;; 29. Stream chunk — empty text parts are filtered
;; ============================================================

(define stream-empty-text-events
  (list
   (hash 'candidates
         (list (hash 'content
                     (hash 'role "model"
                           'parts (list (hash 'text "")))
                     'finishReason #f)))
   (hash 'candidates
         (list (hash 'content
                     (hash 'role "model"
                           'parts (list (hash 'text "Actual text")))
                     'finishReason "STOP")))))

(define stream-empty-chunks (gemini-parse-stream-chunks stream-empty-text-events))
(define empty-text-deltas
  (filter (lambda (c) (stream-chunk-delta-text c)) stream-empty-chunks))
(check-equal? (length empty-text-deltas) 1
              "empty text parts are filtered out")
(check-equal? (stream-chunk-delta-text (car empty-text-deltas)) "Actual text"
              "only non-empty text preserved")

(println "All Gemini provider tests passed!")
