#lang racket

;; tests/test-e2e-tool-serialization.rkt — E2E tool→API serialization pipeline tests
;;
;; Issue #111: Validates the full chain: make-tool → register! → list-tools-jsexpr →
;;   build-request-body → jsexpr->bytes for all three providers (OpenAI, Anthropic, Gemini).
;; Issue #114: Hot-path integration test — ensures no struct or unserializable data
;;   leaks into the JSON pipeline. The tool-serialization crash (struct reaching
;;   jsexpr->bytes) existed from v0.1.0 to v0.6.4 because no test assembled the full chain.

(require rackunit
         json
         "../tools/tool.rkt"
         "../llm/model.rkt"
         "../llm/openai-compatible.rkt"
         "../llm/anthropic.rkt"
         "../llm/gemini.rkt")

;; ============================================================
;; Shared fixtures
;; ============================================================

(define messages
  (list (hash 'role "user" 'content "What files are in /tmp?")))

(define (dummy-execute args ctx)
  (make-success-result (list (hash 'type "text" 'text "ok"))))

;; Tool 1: read_file with realistic schema
(define read-file-schema
  (hasheq 'type "object"
          'properties (hasheq 'path (hasheq 'type "string"
                                            'description "Absolute file path")
                              'encoding (hasheq 'type "string"
                                                'description "File encoding"
                                                'enum '("utf-8" "latin-1")))
          'required '("path")))

(define read-file-tool
  (make-tool "read_file"
             "Read the contents of a file from disk"
             read-file-schema
             dummy-execute))

;; Tool 2: list_directory with nested schema
(define list-dir-schema
  (hasheq 'type "object"
          'properties (hasheq 'path (hasheq 'type "string"
                                            'description "Directory path")
                              'options (hasheq 'type "object"
                                               'properties (hasheq 'recursive (hasheq 'type "boolean")
                                                                   'max_depth (hasheq 'type "integer"))
                                               'required '("recursive")))
          'required '("path")))

(define list-dir-tool
  (make-tool "list_directory"
             "List files and subdirectories"
             list-dir-schema
             dummy-execute))

;; Tool 3: search_files with deeply nested schema
(define search-schema
  (hasheq 'type "object"
          'properties (hasheq 'pattern (hasheq 'type "string")
                              'filters (hasheq 'type "object"
                                               'properties (hasheq 'glob (hasheq 'type "string")
                                                                   'modified (hasheq 'type "object"
                                                                                     'properties (hasheq 'after (hasheq 'type "string")
                                                                                                         'before (hasheq 'type "string"))
                                                                                     'required '()))
                                               'required '()))
          'required '("pattern")))

(define search-tool
  (make-tool "search_files"
             "Search for files matching a pattern with optional filters"
             search-schema
             dummy-execute))

;; Build a registry with all three tools
(define (make-test-registry)
  (define reg (make-tool-registry))
  (register-tool! reg read-file-tool)
  (register-tool! reg list-dir-tool)
  (register-tool! reg search-tool)
  reg)

;; ============================================================
;; 1. OpenAI: register tools → build-request-body → jsexpr->bytes
;; ============================================================

(test-case "OpenAI: full tool serialization pipeline"
  (define reg (make-test-registry))
  (define tools (list-tools-jsexpr reg))
  (check-equal? (length tools) 3 "three tools registered")

  (define req (make-model-request messages tools (hash 'model "gpt-4")))
  (define body (openai-build-request-body req))

  (check-pred hash? body "body is a hash")
  (check-true (hash-has-key? body 'tools) "body has 'tools key")
  (check-equal? (length (hash-ref body 'tools)) 3 "three tools in body")

  ;; The critical check: jsexpr->bytes must NOT raise
  (define bytes (jsexpr->bytes body))
  (check-true (bytes? bytes) "jsexpr->bytes returns bytes")
  (check-true (> (bytes-length bytes) 0) "output is non-empty"))

;; ============================================================
;; 2. Anthropic: register tools → build-request-body → jsexpr->bytes
;; ============================================================

(test-case "Anthropic: full tool serialization pipeline"
  (define reg (make-test-registry))
  (define tools (list-tools-jsexpr reg))

  (define req (make-model-request messages tools (hash 'model "claude-sonnet-4-20250514")))
  (define body (anthropic-build-request-body req))

  (check-pred hash? body "body is a hash")
  (check-true (hash-has-key? body 'tools) "body has 'tools key")
  (check-equal? (length (hash-ref body 'tools)) 3 "three tools in body")

  ;; Each tool should have Anthropic format keys
  (for ([t (in-list (hash-ref body 'tools))])
    (check-true (hash-has-key? t 'name) "anthropic tool has name")
    (check-true (hash-has-key? t 'description) "anthropic tool has description")
    (check-true (hash-has-key? t 'input_schema) "anthropic tool has input_schema"))

  ;; The critical check: jsexpr->bytes must NOT raise
  (define bytes (jsexpr->bytes body))
  (check-true (bytes? bytes) "jsexpr->bytes returns bytes")
  (check-true (> (bytes-length bytes) 0) "output is non-empty"))

;; ============================================================
;; 3. Gemini: register tools → build-request-body → jsexpr->bytes
;; ============================================================

(test-case "Gemini: full tool serialization pipeline"
  (gemini-reset-tool-id-counter!)
  (define reg (make-test-registry))
  (define tools (list-tools-jsexpr reg))

  (define req (make-model-request messages tools (hash 'model "gemini-2.5-pro")))
  (define body (gemini-build-request-body req))

  (check-pred hash? body "body is a hash")
  (check-true (hash-has-key? body 'tools) "body has 'tools key")

  ;; Gemini wraps tools in a list with functionDeclarations
  (define tools-val (hash-ref body 'tools))
  (check-true (list? tools-val) "gemini tools is a list")
  (check-equal? (length tools-val) 1 "one tool group")
  (define declarations (hash-ref (car tools-val) 'functionDeclarations))
  (check-equal? (length declarations) 3 "three function declarations")

  ;; The critical check: jsexpr->bytes must NOT raise
  (define bytes (jsexpr->bytes body))
  (check-true (bytes? bytes) "jsexpr->bytes returns bytes")
  (check-true (> (bytes-length bytes) 0) "output is non-empty"))

;; ============================================================
;; 4. Round-trip: tool-call response serialization
;; ============================================================

(test-case "Model-response with tool-call content round-trips through JSON"
  (define tool-call-content
    (list (hash 'type "text" 'text "I will read the file for you.")
          (hash 'type "tool-call"
                'id "call_abc123"
                'name "read_file"
                'arguments (hash 'path "/tmp/test.txt"))))

  (define usage (hash 'prompt_tokens 42
                      'completion_tokens 17
                      'total_tokens 59))

  (define resp (make-model-response tool-call-content usage "gpt-4" 'tool-calls))

  ;; Serialize to jsexpr
  (define jsexpr (model-response->jsexpr resp))
  (check-pred hash? jsexpr "response jsexpr is a hash")

  ;; jsexpr->bytes must succeed
  (define bytes (jsexpr->bytes jsexpr))
  (check-true (bytes? bytes) "response serializes to bytes")

  ;; Deserialize back
  (define resp2 (jsexpr->model-response jsexpr))
  (check-equal? (model-response-model resp2) "gpt-4" "model preserved")
  (check-equal? (model-response-stop-reason resp2) 'tool-calls "stop-reason preserved")

  ;; Content preserved
  (define content2 (model-response-content resp2))
  (check-equal? (length content2) 2 "two content parts")
  (define tc (cadr content2))
  (check-equal? (hash-ref tc 'type) "tool-call" "tool-call type preserved")
  (check-equal? (hash-ref tc 'name) "read_file" "tool name preserved")
  (check-equal? (hash-ref (hash-ref tc 'arguments) 'path) "/tmp/test.txt"
                "tool arguments preserved"))

;; ============================================================
;; 5. Multiple tools with nested schemas — all providers
;; ============================================================

(test-case "Nested schemas serialize through all providers"
  (define reg (make-test-registry))
  (define tools (list-tools-jsexpr reg))
  (define settings (hash 'model "test-model"))
  (define req (make-model-request messages tools settings))

  ;; OpenAI
  (define openai-body (openai-build-request-body req))
  (check-not-exn (lambda () (jsexpr->bytes openai-body))
                 "OpenAI nested-schema body serializes")

  ;; Anthropic
  (define anthropic-body (anthropic-build-request-body req))
  (check-not-exn (lambda () (jsexpr->bytes anthropic-body))
                 "Anthropic nested-schema body serializes")

  ;; Gemini
  (define gemini-body (gemini-build-request-body req))
  (check-not-exn (lambda () (jsexpr->bytes gemini-body))
                 "Gemini nested-schema body serializes")

  ;; Verify nested structure survived in OpenAI output
  (define openai-tools (hash-ref openai-body 'tools))
  (define list-dir-jx (car (filter
                            (lambda (t)
                              (equal? (hash-ref (hash-ref t 'function) 'name)
                                      "list_directory"))
                            openai-tools)))
  (define params (hash-ref (hash-ref list-dir-jx 'function) 'parameters))
  (check-true (hash-has-key? params 'properties) "nested properties present")
  (define options (hash-ref (hash-ref params 'properties) 'options))
  (check-true (hash-has-key? (hash-ref options 'properties) 'recursive)
              "deeply nested property present"))

;; ============================================================
;; 6. Tool with empty schema serializes for all providers
;; ============================================================

(test-case "Empty-schema tool serializes for all providers"
  (define reg (make-tool-registry))
  (register-tool! reg
                  (make-tool "noop"
                             "A no-op tool"
                             (hasheq)
                             dummy-execute))
  (define tools (list-tools-jsexpr reg))
  (define settings (hash 'model "test-model"))
  (define req (make-model-request messages tools settings))

  ;; OpenAI
  (define openai-body (openai-build-request-body req))
  (check-true (hash-has-key? openai-body 'tools))
  (check-not-exn (lambda () (jsexpr->bytes openai-body))
                 "OpenAI empty-schema body serializes")

  ;; Anthropic
  (define anthropic-body (anthropic-build-request-body req))
  (check-true (hash-has-key? anthropic-body 'tools))
  (check-not-exn (lambda () (jsexpr->bytes anthropic-body))
                 "Anthropic empty-schema body serializes")

  ;; Gemini
  (define gemini-body (gemini-build-request-body req))
  (check-true (hash-has-key? gemini-body 'tools))
  (check-not-exn (lambda () (jsexpr->bytes gemini-body))
                 "Gemini empty-schema body serializes"))
