#lang racket

;; BOUNDARY: integration

;; tests/test-gemini.rkt — thin aggregator + remaining Gemini tests
;; Extracted domains live in:
;;   - test-gemini-stream.rkt
;;   - test-gemini-provider.rkt

(require rackunit
         "../llm/model.rkt"
         "../llm/provider.rkt"
         "../llm/stream.rkt"
         "../llm/gemini.rkt"
         "../llm/http-helpers.rkt")

;; ============================================================
;; 1. gemini-build-request-body — basic structure
;; ============================================================

(define req-basic
  (make-model-request (list (hash 'role "user" 'content "Hello")) #f (hash 'model "gemini-2.5-pro")))

(define body-basic (gemini-build-request-body req-basic))
(check-pred hash? body-basic "build-request-body returns a hash")
(check-pred list? (hash-ref body-basic 'contents) "contents is a list")
(check-equal? (length (hash-ref body-basic 'contents)) 1 "one content entry")

(define first-content (car (hash-ref body-basic 'contents)))
(check-equal? (hash-ref first-content 'role) "user" "role is 'user'")
(check-pred list? (hash-ref first-content 'parts) "parts is a list")
(check-equal? (hash-ref (car (hash-ref first-content 'parts)) 'text) "Hello" "text content in parts")

;; ============================================================
;; 2. gemini-build-request-body — model and maxOutputTokens
;; ============================================================

(define gen-config (hash-ref body-basic 'generationConfig))
(check-pred hash? gen-config "generationConfig is a hash")
(check-equal? (hash-ref gen-config 'maxOutputTokens) 4096 "default maxOutputTokens is 4096")

(define req-custom-max
  (make-model-request (list (hash 'role "user" 'content "Hello"))
                      #f
                      (hash 'model "gemini-2.5-pro" 'max-tokens 8192)))

(define body-custom-max (gemini-build-request-body req-custom-max))
(check-equal? (hash-ref (hash-ref body-custom-max 'generationConfig) 'maxOutputTokens)
              8192
              "custom maxOutputTokens from settings")

;; ============================================================
;; 3. gemini-build-request-body — messages → contents with role mapping
;; ============================================================

(define req-assistant-msg
  (make-model-request (list (hash 'role "user" 'content "Hi")
                            (hash 'role "assistant" 'content "Hello!")
                            (hash 'role "user" 'content "How are you?"))
                      #f
                      (hash 'model "gemini-2.5-pro")))

(define body-assistant (gemini-build-request-body req-assistant-msg))
(define contents-assistant (hash-ref body-assistant 'contents))
(check-equal? (length contents-assistant) 3 "three content entries")
(check-equal? (hash-ref (car contents-assistant) 'role) "user" "first message role is 'user'")
(check-equal? (hash-ref (cadr contents-assistant) 'role) "model" "assistant → model role mapping")
(check-equal? (hash-ref (caddr contents-assistant) 'role) "user" "third message role is 'user'")
(check-equal? (hash-ref (car (hash-ref (cadr contents-assistant) 'parts)) 'text)
              "Hello!"
              "assistant message text in parts")

;; ============================================================
;; 4. gemini-build-request-body — tools translated to functionDeclarations
;; ============================================================

(define tool-def
  (hash 'type
        "function"
        'function
        (hash 'name
              "bash"
              'description
              "Run a shell command"
              'parameters
              (hash 'type "object" 'properties (hash)))))

(define req-with-tools
  (make-model-request (list (hash 'role "user" 'content "Run ls"))
                      (list tool-def)
                      (hash 'model "gemini-2.5-pro")))

(define body-tools (gemini-build-request-body req-with-tools))
(check-true (hash-has-key? body-tools 'tools) "tools key present")
(check-pred list? (hash-ref body-tools 'tools) "tools is a list")
(check-equal? (length (hash-ref body-tools 'tools)) 1 "one tool wrapper")

(define gemini-tools-wrapper (car (hash-ref body-tools 'tools)))
(check-true (hash-has-key? gemini-tools-wrapper 'functionDeclarations)
            "tools wrapper has functionDeclarations")

(define func-decls (hash-ref gemini-tools-wrapper 'functionDeclarations))
(check-equal? (length func-decls) 1 "one function declaration")

(define gemini-tool (car func-decls))
(check-equal? (hash-ref gemini-tool 'name) "bash" "tool name preserved")
(check-equal? (hash-ref gemini-tool 'description) "Run a shell command" "tool description preserved")
(check-true (hash-has-key? gemini-tool 'parameters) "tool has parameters (not input_schema)")
(check-false (hash-has-key? gemini-tool 'function) "tool does NOT have 'function' key")
(check-false (hash-has-key? gemini-tool 'input_schema) "tool does NOT have 'input_schema' key")

;; ============================================================
;; 5. gemini-build-request-body — system prompt → systemInstruction
;; ============================================================

(define req-with-system
  (make-model-request (list (hash 'role "user" 'content "Hello"))
                      #f
                      (hash 'model "gemini-2.5-pro" 'system "You are a helpful assistant.")))

(define body-system (gemini-build-request-body req-with-system))
(check-true (hash-has-key? body-system 'systemInstruction) "systemInstruction key present")
(define sys-instr (hash-ref body-system 'systemInstruction))
(check-true (hash-has-key? sys-instr 'parts) "systemInstruction has parts")
(define sys-parts (hash-ref sys-instr 'parts))
(check-equal? (hash-ref (car sys-parts) 'text)
              "You are a helpful assistant."
              "system prompt text in systemInstruction")

(define body-no-system (gemini-build-request-body req-basic))
(check-false (hash-has-key? body-no-system 'systemInstruction)
             "no systemInstruction when no system prompt")

;; ============================================================
;; 6. gemini-build-request-body — temperature passthrough
;; ============================================================

(define req-with-temp
  (make-model-request (list (hash 'role "user" 'content "Hello"))
                      #f
                      (hash 'model "gemini-2.5-pro" 'temperature 0.7)))

(define body-temp (gemini-build-request-body req-with-temp))
(check-equal? (hash-ref (hash-ref body-temp 'generationConfig) 'temperature)
              0.7
              "temperature passed through to generationConfig")

(define body-no-temp (gemini-build-request-body req-basic))
(check-false (hash-has-key? (hash-ref body-no-temp 'generationConfig) 'temperature)
             "no temperature key when not set")

;; ============================================================
;; 7. gemini-build-request-body — model from settings / default
;; ============================================================

(define req-different-model
  (make-model-request (list (hash 'role "user" 'content "Hello"))
                      #f
                      (hash 'model "gemini-2.0-flash")))

(define body-diff-model (gemini-build-request-body req-different-model))
(check-equal? (hash-ref (model-request-settings req-different-model) 'model)
              "gemini-2.0-flash"
              "model from settings available")

(define req-no-model (make-model-request (list (hash 'role "user" 'content "Hello")) #f (hash)))
(define body-no-model (gemini-build-request-body req-no-model))
(check-equal? (hash-ref (model-request-settings req-no-model) 'model #f)
              #f
              "no model key when not set — uses default at provider level")

;; ============================================================
;; 8. gemini-parse-response — text response
;; ============================================================

(define fake-text-response
  (hash 'candidates
        (list (hash 'content
                    (hash 'role "model" 'parts (list (hash 'text "Hello from Gemini!")))
                    'finishReason
                    "STOP"))
        'usageMetadata
        (hash 'promptTokenCount 10 'candidatesTokenCount 5 'totalTokenCount 15)
        'modelVersion
        "gemini-2.5-pro"))

(define parsed-text (gemini-parse-response fake-text-response))
(check-pred model-response? parsed-text "parsed text response is model-response?")
(check-equal? (model-response-model parsed-text) "gemini-2.5-pro" "model version preserved")
(check-equal? (model-response-stop-reason parsed-text) 'stop "STOP → stop")
(check-equal? (length (model-response-content parsed-text)) 1 "one content block")

(define text-content (car (model-response-content parsed-text)))
(check-equal? (hash-ref text-content 'type) "text" "content type is text")
(check-equal? (hash-ref text-content 'text) "Hello from Gemini!" "text content preserved")

;; ============================================================
;; 9. gemini-parse-response — tool call response (functionCall)
;; ============================================================

(define fake-tool-response
  (hash 'candidates
        (list (hash 'content
                    (hash 'role
                          "model"
                          'parts
                          (list (hash 'functionCall
                                      (hash 'name "bash" 'args (hash 'command "ls -la")))))
                    'finishReason
                    "STOP"))
        'usageMetadata
        (hash 'promptTokenCount 20 'candidatesTokenCount 10 'totalTokenCount 30)
        'modelVersion
        "gemini-2.5-pro"))

(define parsed-tool (gemini-parse-response fake-tool-response))
(check-equal? (length (model-response-content parsed-tool)) 1 "one content block for tool call")

(define tool-content (car (model-response-content parsed-tool)))
(check-equal? (hash-ref tool-content 'type) "tool-call" "functionCall → tool-call type")
(check-equal? (hash-ref tool-content 'name) "bash" "tool name preserved")
(check-equal? (hash-ref (hash-ref tool-content 'arguments) 'command)
              "ls -la"
              "tool args → arguments (direct object, not string)")
(check-true (and (hash-has-key? tool-content 'id) (> (string-length (hash-ref tool-content 'id)) 0))
            "tool id is non-empty (generated by gemini-gen-tool-id)")

;; ============================================================
;; 10. gemini-parse-response — usage translation
;; ============================================================

(define parsed-usage-text (gemini-parse-response fake-text-response))
(define usage-result (model-response-usage parsed-usage-text))
(check-equal? (hash-ref usage-result 'prompt_tokens) 10 "promptTokenCount → prompt_tokens")
(check-equal? (hash-ref usage-result 'completion_tokens) 5 "candidatesTokenCount → completion_tokens")
(check-equal? (hash-ref usage-result 'total_tokens) 15 "totalTokenCount → total_tokens")

;; ============================================================
;; 11. gemini-parse-response — stop reason translation
;; ============================================================

(define fake-max-response
  (hash 'candidates
        (list (hash 'content
                    (hash 'role "model" 'parts (list (hash 'text "truncated")))
                    'finishReason
                    "MAX_TOKENS"))
        'usageMetadata
        (hash 'promptTokenCount 10 'candidatesTokenCount 4096 'totalTokenCount 4106)
        'modelVersion
        "gemini-2.5-pro"))
(check-equal? (model-response-stop-reason (gemini-parse-response fake-max-response))
              'length
              "MAX_TOKENS → length")

(define fake-safety-response
  (hash
   'candidates
   (list
    (hash 'content (hash 'role "model" 'parts (list (hash 'text "blocked"))) 'finishReason "SAFETY"))
   'usageMetadata
   (hash 'promptTokenCount 10 'candidatesTokenCount 3 'totalTokenCount 13)
   'modelVersion
   "gemini-2.5-pro"))
(check-equal? (model-response-stop-reason (gemini-parse-response fake-safety-response))
              'content-filtered
              "SAFETY → content-filtered")

(define fake-recitation-response
  (hash 'candidates
        (list (hash 'content
                    (hash 'role "model" 'parts (list (hash 'text "recited")))
                    'finishReason
                    "RECITATION"))
        'usageMetadata
        (hash 'promptTokenCount 10 'candidatesTokenCount 3 'totalTokenCount 13)
        'modelVersion
        "gemini-2.5-pro"))
(check-equal? (model-response-stop-reason (gemini-parse-response fake-recitation-response))
              'content-filtered
              "RECITATION → content-filtered")

;; ============================================================
;; 12. gemini-parse-response — multiple content parts
;; ============================================================

(define fake-multi-response
  (hash 'candidates
        (list (hash 'content
                    (hash 'role
                          "model"
                          'parts
                          (list (hash 'text "I'll run that command.")
                                (hash 'functionCall (hash 'name "bash" 'args (hash 'command "ls")))
                                (hash 'functionCall
                                      (hash 'name "read" 'args (hash 'path "/tmp/test.txt")))))
                    'finishReason
                    "STOP"))
        'usageMetadata
        (hash 'promptTokenCount 50 'candidatesTokenCount 30 'totalTokenCount 80)
        'modelVersion
        "gemini-2.5-pro"))

(define parsed-multi (gemini-parse-response fake-multi-response))
(check-equal? (length (model-response-content parsed-multi)) 3 "three content blocks preserved")

(define first-block (car (model-response-content parsed-multi)))
(check-equal? (hash-ref first-block 'type) "text")

(define second-block (cadr (model-response-content parsed-multi)))
(check-equal? (hash-ref second-block 'type) "tool-call")
(check-equal? (hash-ref second-block 'name) "bash")

(define third-block (caddr (model-response-content parsed-multi)))
(check-equal? (hash-ref third-block 'type) "tool-call")
(check-equal? (hash-ref third-block 'name) "read")

(define multi-usage (model-response-usage parsed-multi))
(check-equal? (hash-ref multi-usage 'total_tokens) 80 "total_tokens = 80")

;; ============================================================
;; 13. gemini-parse-response — empty response (no candidates)
;; ============================================================

(define fake-empty-response
  (hash 'candidates
        '()
        'usageMetadata
        (hash 'promptTokenCount 5 'candidatesTokenCount 0 'totalTokenCount 5)
        'modelVersion
        "gemini-2.5-pro"))

(define parsed-empty (gemini-parse-response fake-empty-response))
(check-equal? (length (model-response-content parsed-empty)) 0 "empty content list for no candidates")

;; ============================================================
;; 19. SSE integration with parse-sse-lines
;; ============================================================

(define fake-sse-text
  (string-append
   "data: {\"candidates\":[{\"content\":{\"role\":\"model\",\"parts\":[{\"text\":\"Hello\"}]},\"finishReason\":null}]}\n\n"
   "data: {\"candidates\":[{\"content\":{\"role\":\"model\",\"parts\":[{\"text\":\" world\"}]},\"finishReason\":\"STOP\"}],"
   "\"usageMetadata\":{\"promptTokenCount\":10,\"candidatesTokenCount\":2,\"totalTokenCount\":12}}\n\n"))

(define parsed-sse-events (parse-sse-lines fake-sse-text))
(check-equal? (length parsed-sse-events) 2 "SSE parsing extracts 2 data events")

(define sse-chunks (gemini-parse-stream-chunks parsed-sse-events))
(check-true (>= (length sse-chunks) 2) "SSE stream produces at least 2 chunks")

(define sse-text-chunks (filter (lambda (c) (stream-chunk-delta-text c)) sse-chunks))
(check-equal? (length sse-text-chunks) 2 "Two text deltas from SSE stream")

(define concatenated (apply string-append (map stream-chunk-delta-text sse-text-chunks)))
(check-equal? concatenated "Hello world" "Text deltas concatenate correctly")

;; ============================================================
;; 21. Tool translation helper
;; ============================================================

(define test-tool
  (hash 'type
        "function"
        'function
        (hash 'name
              "read_file"
              'description
              "Read a file"
              'parameters
              (hash 'type "object" 'properties (hash 'path (hash 'type "string"))))))

(define gemini-tool-result (gemini-translate-tool test-tool))
(check-equal? (hash-ref gemini-tool-result 'name) "read_file")
(check-equal? (hash-ref gemini-tool-result 'description) "Read a file")
(check-true (hash-has-key? gemini-tool-result 'parameters))
(check-false (hash-has-key? gemini-tool-result 'function) "no 'function' wrapper in Gemini format")
(check-false (hash-has-key? gemini-tool-result 'input_schema) "no 'input_schema' in Gemini format")

;; ============================================================
;; 22. Stop reason translation helper (all variants)
;; ============================================================

(check-equal? (translate-stop-reason 'gemini "STOP") 'stop)
(check-equal? (translate-stop-reason 'gemini "MAX_TOKENS") 'length)
(check-equal? (translate-stop-reason 'gemini "SAFETY") 'content-filtered)
(check-equal? (translate-stop-reason 'gemini "RECITATION") 'content-filtered)
(check-equal? (translate-stop-reason 'gemini "OTHER") 'OTHER "unknown reason → symbol")
(check-equal? (translate-stop-reason 'gemini 'already-symbol) 'already-symbol)
(check-equal? (translate-stop-reason 'gemini 123) 'stop "non-string/non-symbol → stop")

;; ============================================================
;; 23. gemini-build-request-body — streaming flag (accepted but not in body)
;; ============================================================

(define body-stream (gemini-build-request-body req-basic #:stream? #t))
(check-pred hash? body-stream "streaming body is still a valid hash")
(check-false (hash-has-key? body-stream 'stream) "Gemini body does not include 'stream' key")

;; ============================================================
;; 24. gemini-parse-response — no modelVersion field
;; ============================================================

(define fake-no-version-response
  (hash
   'candidates
   (list (hash 'content (hash 'role "model" 'parts (list (hash 'text "Hi"))) 'finishReason "STOP"))
   'usageMetadata
   (hash 'promptTokenCount 5 'candidatesTokenCount 1 'totalTokenCount 6)))

(define parsed-no-version (gemini-parse-response fake-no-version-response))
(check-equal? (model-response-model parsed-no-version)
              "gemini-2.5-pro"
              "default model name when modelVersion absent")

;; ============================================================
;; 25. gemini-parse-response — missing usageMetadata
;; ============================================================

(define fake-no-usage-response
  (hash
   'candidates
   (list (hash 'content (hash 'role "model" 'parts (list (hash 'text "Hi"))) 'finishReason "STOP"))))

(define parsed-no-usage (gemini-parse-response fake-no-usage-response))
(define no-usage-result (model-response-usage parsed-no-usage))
(check-equal? (hash-ref no-usage-result 'prompt_tokens) 0 "missing usage → prompt_tokens 0")
(check-equal? (hash-ref no-usage-result 'completion_tokens) 0 "missing usage → completion_tokens 0")
(check-equal? (hash-ref no-usage-result 'total_tokens) 0 "missing usage → total_tokens 0")

;; ============================================================
;; 26. gemini-build-request-body — empty messages list
;; ============================================================

(define req-empty-msgs (make-model-request '() #f (hash 'model "gemini-2.5-pro")))

(define body-empty (gemini-build-request-body req-empty-msgs))
(check-equal? (length (hash-ref body-empty 'contents)) 0 "empty messages → empty contents")

;; ============================================================
;; 28. gemini-translate-tool — tool with no function wrapper
;; ============================================================

(define flat-tool
  (hash 'name "direct_tool" 'description "A direct tool" 'parameters (hash 'type "object")))

(define flat-result (gemini-translate-tool flat-tool))
(check-equal? (hash-ref flat-result 'name) "direct_tool" "flat tool name preserved")
(check-equal? (hash-ref flat-result 'description) "A direct tool" "flat tool description preserved")

;; ============================================================
;; 31. Issue #107 — Gemini multi-turn tool use: assistant tool_calls
;; ============================================================

(define req-multiturn-tool
  (make-model-request
   (list
    (hash 'role "user" 'content "What files are here?")
    (hash
     'role
     "assistant"
     'content
     (list (hash 'type "tool-call" 'id "call_abc123" 'name "bash" 'arguments (hash 'command "ls"))))
    (hash 'role
          "tool"
          'content
          (hash 'type "tool-result" 'toolCallId "call_abc123" 'content "file1.txt\nfile2.txt"))
    (hash 'role "user" 'content "Read file1.txt"))
   #f
   (hash 'model "gemini-2.5-pro")))

(define body-multiturn-tool (gemini-build-request-body req-multiturn-tool))
(define mt-contents (hash-ref body-multiturn-tool 'contents))

(check-equal? (length mt-contents) 4 "multi-turn: 4 content entries")

(define mt-entry1 (car mt-contents))
(check-equal? (hash-ref mt-entry1 'role) "user" "multi-turn: first entry role is user")

(define mt-entry2 (cadr mt-contents))
(check-equal? (hash-ref mt-entry2 'role) "model" "multi-turn: assistant → model role")
(define mt-entry2-parts (hash-ref mt-entry2 'parts))
(check-pred list? mt-entry2-parts "multi-turn: model parts is a list")
(define mt-fc-part (car mt-entry2-parts))
(check-true (hash-has-key? mt-fc-part 'functionCall)
            "multi-turn: assistant tool_call → functionCall part")
(define mt-fc (hash-ref mt-fc-part 'functionCall))
(check-equal? (hash-ref mt-fc 'name) "bash" "multi-turn: functionCall name preserved")
(check-equal? (hash-ref (hash-ref mt-fc 'args) 'command)
              "ls"
              "multi-turn: functionCall args preserved")

(define mt-entry3 (caddr mt-contents))
(check-equal? (hash-ref mt-entry3 'role) "user" "multi-turn: tool → user role for Gemini")
(define mt-entry3-parts (hash-ref mt-entry3 'parts))
(check-pred list? mt-entry3-parts "multi-turn: tool result parts is a list")
(define mt-fr-part (car mt-entry3-parts))
(check-true (hash-has-key? mt-fr-part 'functionResponse)
            "multi-turn: tool result → functionResponse part")
(define mt-fr (hash-ref mt-fr-part 'functionResponse))
(check-equal? (hash-ref mt-fr 'name) "bash" "multi-turn: functionResponse name matches tool name")
(check-true (hash-has-key? mt-fr 'response) "multi-turn: functionResponse has response field")

(define mt-entry4 (cadddr mt-contents))
(check-equal? (hash-ref mt-entry4 'role) "user" "multi-turn: fourth entry role is user")

;; ============================================================
;; 32. Issue #107 — Gemini: assistant with mixed text + tool_calls
;; ============================================================

(define req-gemini-mixed
  (make-model-request
   (list
    (hash 'role "user" 'content "Run it")
    (hash
     'role
     "assistant"
     'content
     (list
      (hash 'type "text" 'text "Sure, let me run that.")
      (hash 'type "tool-call" 'id "call_xyz" 'name "bash" 'arguments (hash 'command "echo hi")))))
   #f
   (hash 'model "gemini-2.5-pro")))

(define body-gemini-mixed (gemini-build-request-body req-gemini-mixed))
(define gemini-mixed-contents (hash-ref body-gemini-mixed 'contents))

(define gemini-mixed-model (cadr gemini-mixed-contents))
(check-equal? (hash-ref gemini-mixed-model 'role) "model")
(define gemini-mixed-parts (hash-ref gemini-mixed-model 'parts))
(check-equal? (length gemini-mixed-parts) 2 "gemini mixed: text + functionCall = 2 parts")

(check-equal? (hash-ref (car gemini-mixed-parts) 'text)
              "Sure, let me run that."
              "gemini mixed: text part preserved")

(check-true (hash-has-key? (cadr gemini-mixed-parts) 'functionCall)
            "gemini mixed: second part is functionCall")

;; ============================================================
;; 33. Issue #110 — Gemini tool calls have unique IDs
;; ============================================================

(gemini-reset-tool-id-counter!)

(define fake-dual-tool-response
  (hash 'candidates
        (list (hash 'content
                    (hash 'role
                          "model"
                          'parts
                          (list (hash 'functionCall (hash 'name "bash" 'args (hash 'command "ls")))
                                (hash 'functionCall (hash 'name "read" 'args (hash 'path "/tmp")))))
                    'finishReason
                    "STOP"))
        'usageMetadata
        (hash 'promptTokenCount 20 'candidatesTokenCount 10 'totalTokenCount 30)
        'modelVersion
        "gemini-2.5-pro"))

(define parsed-dual-tool (gemini-parse-response fake-dual-tool-response))
(define dual-content (model-response-content parsed-dual-tool))

(define tc1 (car dual-content))
(define tc2 (cadr dual-content))

(check-true (and (hash-has-key? tc1 'id) (> (string-length (hash-ref tc1 'id)) 0))
            "gemini tool call 1 has non-empty ID")
(check-true (and (hash-has-key? tc2 'id) (> (string-length (hash-ref tc2 'id)) 0))
            "gemini tool call 2 has non-empty ID")

(check-not-equal? (hash-ref tc1 'id) (hash-ref tc2 'id) "gemini tool calls have unique IDs")

(check-true (string-prefix? (hash-ref tc1 'id) "gemini_") "gemini tool call ID has 'gemini_' prefix")
(check-true (string-prefix? (hash-ref tc2 'id) "gemini_") "gemini tool call ID has 'gemini_' prefix")

;; ============================================================
;; Issue #138 — SAFETY/RECITATION filtering produces warning content
;; ============================================================

(test-case "SAFETY with empty content produces warning text block"
  (define fake-safety-empty
    (hash 'candidates
          (list (hash 'content (hash 'role "model" 'parts '()) 'finishReason "SAFETY"))
          'usageMetadata
          (hash 'promptTokenCount 10 'candidatesTokenCount 0 'totalTokenCount 10)
          'modelVersion
          "gemini-2.5-pro"))
  (define parsed (gemini-parse-response fake-safety-empty))
  (check-equal? (model-response-stop-reason parsed) 'content-filtered)
  (define content (model-response-content parsed))
  (check-equal? (length content) 1 "SAFETY empty content replaced with warning")
  (define text-block (car content))
  (check-equal? (hash-ref text-block 'type) "text")
  (check-true (string-contains? (hash-ref text-block 'text) "filtered for safety")
              "SAFETY warning mentions safety"))

(test-case "RECITATION with empty content produces warning text block"
  (define fake-recitation-empty
    (hash 'candidates
          (list (hash 'content (hash 'role "model" 'parts '()) 'finishReason "RECITATION"))
          'usageMetadata
          (hash 'promptTokenCount 10 'candidatesTokenCount 0 'totalTokenCount 10)
          'modelVersion
          "gemini-2.5-pro"))
  (define parsed (gemini-parse-response fake-recitation-empty))
  (check-equal? (model-response-stop-reason parsed) 'content-filtered)
  (define content (model-response-content parsed))
  (check-equal? (length content) 1 "RECITATION empty content replaced with warning")
  (define text-block (car content))
  (check-equal? (hash-ref text-block 'type) "text")
  (check-true (string-contains? (hash-ref text-block 'text) "filtered for recitation")
              "RECITATION warning mentions recitation"))

(test-case "SAFETY with actual content preserves original content"
  (define fake-safety-with-content
    (hash 'candidates
          (list (hash 'content
                      (hash 'role "model" 'parts (list (hash 'text "I cannot help with that.")))
                      'finishReason
                      "SAFETY"))
          'usageMetadata
          (hash 'promptTokenCount 10 'candidatesTokenCount 5 'totalTokenCount 15)
          'modelVersion
          "gemini-2.5-pro"))
  (define parsed (gemini-parse-response fake-safety-with-content))
  (check-equal? (model-response-stop-reason parsed) 'content-filtered)
  (define content (model-response-content parsed))
  (check-equal? (length content) 1)
  (check-equal? (hash-ref (car content) 'text) "I cannot help with that."))

(println "All Gemini provider tests passed!")
