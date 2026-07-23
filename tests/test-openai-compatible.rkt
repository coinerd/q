#lang racket

;; @speed fast
;; @suite default

;; BOUNDARY: integration

;; test-openai-compatible.rkt — Tests for OpenAI-compatible provider adapter
;;
;; BUG-34: Verify that error messages are formatted readably (not #hasheq)

(require rackunit
         rackunit/text-ui
         net/url
         json
         "../llm/provider.rkt"
         "../llm/openai-compatible.rkt"
         "../llm/http-helpers.rkt"
         "../llm/stream.rkt"
         "../llm/model.rkt"
         "../util/message/provider-transport.rkt")

;; ============================================================
;; Tests for error message formatting (BUG-34)
;; ============================================================

(define-test-suite
 error-formatting-tests
 (test-case "check-provider-status! extracts error.message from JSON response"
   (define error-json
     (jsexpr->bytes
      (hasheq 'error
              (hasheq 'code
                      1214
                      'message
                      "The messages parameter is illegal. Please check the documentation."))))
   (define exn
     (with-handlers ([exn:fail? identity])
       (check-provider-status! "OpenAI" #"HTTP/1.1 400 Bad Request" error-json)))
   (check-pred exn? exn)
   (define msg (exn-message exn))
   ;; Should contain status code
   (check-true (string-contains? msg "400") "Message should contain status code")
   ;; Should NOT contain #hasheq (the raw Racket representation)
   (check-false (string-contains? msg "#hasheq") "Message should NOT contain #hasheq")
   ;; Should contain the actual error message text
   (check-true (string-contains? msg "The messages parameter is illegal")
               "Message should contain the error text"))
 (test-case "check-provider-status! extracts top-level message field"
   (define error-json (jsexpr->bytes (hasheq 'message "Invalid API key provided")))
   (define exn
     (with-handlers ([exn:fail? identity])
       (check-provider-status! "OpenAI" #"HTTP/1.1 401 Unauthorized" error-json)))
   (check-pred exn? exn)
   (define msg (exn-message exn))
   (check-true (string-contains? msg "401") "Message should contain status code")
   (check-false (string-contains? msg "#hasheq") "Message should NOT contain #hasheq")
   (check-true (string-contains? msg "Invalid API key provided")
               "Message should contain the error text"))
 (test-case "check-provider-status! falls back to raw body when error.message missing"
   (define error-json (jsexpr->bytes (hasheq 'unknown_field "some value")))
   (define exn
     (with-handlers ([exn:fail? identity])
       (check-provider-status! "OpenAI" #"HTTP/1.1 500 Internal Server Error" error-json)))
   (check-pred exn? exn)
   (define msg (exn-message exn))
   (check-true (string-contains? msg "500") "Message should contain status code")
   ;; Fallback to raw jsexpr representation (but not #hasheq since it's a hash without 'error or 'message)
   (check-true (string-contains? msg "unknown_field") "Message should contain raw JSON content"))
 (test-case "check-provider-status! handles binary/non-JSON response"
   (define binary-data #"<html>Error page</html>")
   (define exn
     (with-handlers ([exn:fail? identity])
       (check-provider-status! "OpenAI" #"HTTP/1.1 502 Bad Gateway" binary-data)))
   (check-pred exn? exn)
   (define msg (exn-message exn))
   (check-true (string-contains? msg "502") "Message should contain status code")
   (check-true (string-contains? msg "binary body") "Message should indicate binary body"))
 (test-case "check-provider-status! extracts error.code when message is missing"
   (define error-json (jsexpr->bytes (hasheq 'error (hasheq 'code 1234))))
   (define exn
     (with-handlers ([exn:fail? identity])
       (check-provider-status! "OpenAI" #"HTTP/1.1 422 Unprocessable Entity" error-json)))
   (check-pred exn? exn)
   (define msg (exn-message exn))
   (check-true (string-contains? msg "422") "Message should contain status code")
   (check-true (string-contains? msg "1234") "Message should contain error code")
   (check-false (string-contains? msg "#hasheq") "Message should NOT contain #hasheq"))
 (test-case "check-provider-status! handles error as string (not hash)"
   (define error-json (jsexpr->bytes (hasheq 'error "Something went wrong")))
   (define exn
     (with-handlers ([exn:fail? identity])
       (check-provider-status! "OpenAI" #"HTTP/1.1 400 Bad Request" error-json)))
   (check-pred exn? exn)
   (define msg (exn-message exn))
   (check-true (string-contains? msg "400") "Message should contain status code")
   (check-true (string-contains? msg "Something went wrong") "Message should contain error string")
   (check-false (string-contains? msg "#hasheq") "Message should NOT contain #hasheq")))

;; ============================================================
;; Run all tests
;; ============================================================

;; ============================================================
;; SEC-10: read-response-body size limit tests
;; ============================================================

(define-test-suite
 api-key-validation-tests
 (test-case "openai-compatible: empty API key raises clear error"
   (check-exn exn:fail?
              (lambda ()
                (make-openai-compatible-provider (hash 'api-key "" 'base-url "http://localhost")))
              "empty-api-key"))
 (test-case "openai-compatible: missing API key raises clear error"
   (check-exn exn:fail?
              (lambda () (make-openai-compatible-provider (hash 'base-url "http://localhost")))
              "missing-api-key"))
 (test-case "openai-compatible: whitespace-only API key raises clear error"
   (check-exn exn:fail?
              (lambda ()
                (make-openai-compatible-provider (hash 'api-key "   " 'base-url "http://localhost")))
              "whitespace-api-key"))
 (test-case "error message mentions OpenAI and OPENAI_API_KEY"
   (define exn
     (with-handlers ([exn:fail? identity])
       (make-openai-compatible-provider (hash 'api-key "" 'base-url "http://localhost"))))
   (check-pred exn? exn)
   (define msg (exn-message exn))
   (check-true (string-contains? msg "OpenAI") "error message mentions OpenAI")
   (check-true (string-contains? msg "OPENAI_API_KEY") "error message mentions OPENAI_API_KEY")
   (check-true (string-contains? msg "API key not set") "error message contains 'API key not set'"))
 (test-case "openai-compatible: valid API key does not raise"
   (check-not-exn (lambda ()
                    (make-openai-compatible-provider
                     (hash 'api-key "sk-valid-key-123" 'base-url "http://localhost"))))
   (define prov
     (make-openai-compatible-provider
      (hash 'api-key "sk-valid-key-123" 'base-url "http://localhost")))
   (check-equal? (provider-name prov) "openai-compatible")))

;; ============================================================
;; SEC-10: read-response-body size limit tests
;; ============================================================

(define-test-suite response-size-limit-tests
                   (test-case "openai-compatible: read-response-body reads normal-sized responses"
                     (define port (open-input-string "Hello World"))
                     (define result (read-response-body port))
                     (check-equal? result (string->bytes/utf-8 "Hello World")))
                   (test-case "read-response-body rejects oversized responses"
                     (define overflow-size (+ max-response-size 1))
                     (define buf (make-bytes 8192 65))
                     (define total-read 0)
                     (define port
                       (make-input-port 'overflow
                                        (lambda (b)
                                          (cond
                                            [(>= total-read overflow-size) eof]
                                            [else
                                             (define n (min 8192 (- overflow-size total-read)))
                                             (bytes-copy! b 0 buf 0 n)
                                             (set! total-read (+ total-read n))
                                             n]))
                                        #f
                                        void))
                     (check-exn #rx"exceeds maximum size limit"
                                (lambda () (read-response-body port))))
                   (test-case "read-response-body handles empty port"
                     (define port (open-input-string ""))
                     (define result (read-response-body port))
                     (check-equal? result (bytes))))

;; ============================================================
;; Issue #137 — OpenAI-compatible 429 rate-limit error includes retry guidance
;; ============================================================

(define-test-suite
 rate-limit-guidance-tests
 (test-case "OpenAI HTTP 429 includes wait/retry guidance"
   (define error-json (jsexpr->bytes (hasheq 'error (hasheq 'message "Rate limit exceeded"))))
   (define exn
     (with-handlers ([exn:fail? identity])
       (check-provider-status! "OpenAI" #"HTTP/1.1 429 Too Many Requests" error-json)))
   (check-pred exn? exn)
   (define msg (exn-message exn))
   (check-true (or (string-contains? msg "wait")
                   (string-contains? msg "retry")
                   (string-contains? msg "Wait")
                   (string-contains? msg "Retry"))
               "429 error includes wait/retry guidance"))
 (test-case "OpenAI HTTP 429 with rate-limited message"
   (define exn
     (with-handlers ([exn:fail? identity])
       (check-provider-status! "OpenAI" #"HTTP/1.1 429 Too Many Requests" #"Rate limited")))
   (check-pred exn? exn)
   (define msg (exn-message exn))
   (check-true (string-contains? msg "429") "Message contains status code")
   (check-true (or (string-contains? msg "wait")
                   (string-contains? msg "Wait")
                   (string-contains? msg "retry")
                   (string-contains? msg "Retry"))
               "429 error includes wait/retry guidance")))

;; ============================================================
;; Tests for SSE stream timeout scaling (v0.14.3 Wave 1)
;; ============================================================

(define-test-suite sse-timeout-scaling-tests
                   (test-case "default timeout produces expected stream-timeout"
                     ;; Default request timeout = 600s
                     ;; stream-timeout = max(120, quotient(600, 4)) = max(120, 150) = 150
                     (define default-timeout (current-http-request-timeout))
                     (define expected-stream (max 120 (quotient default-timeout 4)))
                     (check-equal? expected-stream 150))
                   (test-case "slow model timeout scales stream-timeout correctly"
                     ;; For a model with request=900s: stream = max(120, 225) = 225
                     (define slow-timeout 900)
                     (define expected-stream (max 120 (quotient slow-timeout 4)))
                     (check-equal? expected-stream 225))
                   (test-case "very small timeout still gets minimum 120s stream timeout"
                     ;; Even with request=60s: stream = max(120, 15) = 120
                     (define tiny-timeout 60)
                     (define expected-stream (max 120 (quotient tiny-timeout 4)))
                     (check-equal? expected-stream 120)
                     ;; And with request=480s: stream = max(120, 120) = 120
                     (define four-eighty 480)
                     (check-equal? (max 120 (quotient four-eighty 4)) 120))
                   (test-case "effective-request-timeout-for falls back to default"
                     ;; Unknown model name should return default timeout
                     (define result (effective-request-timeout-for "nonexistent-model-xyz"))
                     (check-equal? result (current-http-request-timeout))))

;; ============================================================
;; v0.99.58 FIX: Assistant messages with tool_calls must have content key
;; OpenAI-compatible APIs reject messages without content (400:
;; "messages parameter is illegal").
;; ============================================================

(define-test-suite
 content-key-fix-tests
 (test-case "make-provider-assistant-message always includes content key"
   ;; Even with empty text, content must be present (as JSON null)
   (define msg (make-provider-assistant-message "" '()))
   (check-true (hash-has-key? msg 'content)
               "assistant message with empty text must still have content key"))
 (test-case "make-provider-assistant-message with tool_calls has content key"
   (define msg
     (make-provider-assistant-message
      ""
      (list (hasheq 'id "tc1" 'type "function" 'function (hasheq 'name "test" 'arguments "{}")))))
   (check-true (hash-has-key? msg 'content) "assistant message with tool_calls must have content key")
   ;; Content should serialize as JSON null
   (define json-str (bytes->string/utf-8 (jsexpr->bytes msg)))
   (check-true (string-contains? json-str "\"content\":null")
               "content should serialize as JSON null"))
 (test-case "openai-normalize-message adds content to messages without it"
   ;; Simulate a message that arrives without content (legacy/buggy upstream)
   (define msg-no-content
     (hasheq
      'role
      "assistant"
      'tool_calls
      (list
       (hasheq 'id "tc1" 'type "function" 'function (hasheq 'name "search_memory" 'arguments "{}")))))
   (define normalized (openai-normalize-message msg-no-content))
   (check-true (hash-has-key? normalized 'content)
               "normalize must add content key to messages without it")
   ;; Content should be JSON null
   (define json-str (bytes->string/utf-8 (jsexpr->bytes normalized)))
   (check-true (string-contains? json-str "\"content\":null")
               "added content should serialize as JSON null"))
 (test-case "openai-build-request-body includes content in all messages"
   ;; Build a request with an assistant message that has tool_calls but no text
   (define assistant-msg
     (make-provider-assistant-message
      ""
      (list
       (hasheq 'id "tc1" 'type "function" 'function (hasheq 'name "list_memory" 'arguments "{}")))))
   (define tool-result-msg (hasheq 'role "tool" 'tool_call_id "tc1" 'content "no memories found"))
   (define req
     (make-model-request
      (list (hasheq 'role "user" 'content "What is in my memory?") assistant-msg tool-result-msg)
      #f
      (hasheq 'model "test-model")))
   (define body (openai-build-request-body req))
   (define body-json (bytes->string/utf-8 (jsexpr->bytes body)))
   ;; Every message in the array should have a content field
   (check-true (string-contains? body-json "\"content\":null")
               "request body should contain content:null for assistant with tool_calls"))
 (test-case "full request body is valid for memory tool scenario"
   ;; Simulate the exact scenario: user asks to test memory tools,
   ;; model calls list_memory, gets result, then sends follow-up request.
   ;; The follow-up request must include all messages with content fields.
   (define messages
     ;; System message
     (list (hasheq 'role "system" 'content "You are a helpful assistant.")
           ;; User message
           (hasheq 'role "user" 'content "Test your memory tools.")
           ;; Assistant message with tool_call but no text content
           (make-provider-assistant-message
            ""
            (list (hasheq 'id
                          "call_abc"
                          'type
                          "function"
                          'function
                          (hasheq 'name "search_memory" 'arguments "{\"query\":\"\"}"))))
           ;; Tool result
           (hasheq 'role "tool" 'tool_call_id "call_abc" 'content "No memories found.")))
   (define req (make-model-request messages #f (hasheq 'model "test-model")))
   (define body (openai-build-request-body req))
   (define body-str (bytes->string/utf-8 (jsexpr->bytes body)))
   ;; Verify NO message is missing content (check for role without content nearby)
   ;; The body should be parseable JSON with all messages having content
   (define parsed (string->jsexpr body-str))
   (define msgs (hash-ref parsed 'messages))
   (for ([m (in-list msgs)])
     (check-true (hash-has-key? m 'content)
                 (format "message with role=~a must have content key" (hash-ref m 'role #f))))))

;; ============================================================
;; Run all tests (updated to include new suites)
;; ============================================================

(module+ main
  (run-tests error-formatting-tests)
  (run-tests api-key-validation-tests)
  (run-tests response-size-limit-tests)
  (run-tests rate-limit-guidance-tests)
  (run-tests sse-timeout-scaling-tests)
  (run-tests content-key-fix-tests))
