#lang racket

;; @speed fast  ;; @suite security

;; tests/test-mcp-protocol-compliance.rkt — W2 (v0.99.10) MCP Protocol Compliance Remediation
;;
;; FIXED in W2: H2 and H3 audit blockers.
;; Tests now assert CORRECT behavior.
;;
;; Blockers characterized:
;;   H2: tools/list uses OpenAI schema instead of MCP schema
;;   H3: JSON-RPC notification/error semantics incomplete

(require rackunit
         rackunit/text-ui
         json
         "../extensions/mcp-adapter.rkt"
         "../tools/registry.rkt"
         (only-in "../tools/tool.rkt" make-tool make-success-result))

;; ── Helpers ──

(define (make-test-registry)
  (define reg (make-tool-registry))
  (register-tool!
   reg
   (make-tool "echo"
              "Echo input"
              (hasheq 'type "object" 'properties (hasheq 'text (hasheq 'type "string")))
              (lambda (args ctx) (make-success-result "ok"))))
  reg)

(define (json-roundtrip v)
  (string->jsexpr (jsexpr->string v)))

(define noop-execute (lambda (name args) (hasheq 'content (list (hasheq 'type "text" 'text "ok")))))

(define suite
  (test-suite "MCP Protocol Compliance (W2 Remediation)"

    ;; ════════════════════════════════════════════════════════════
    ;; H2: tools/list uses OpenAI function schema, not MCP tool schema
    ;; ════════════════════════════════════════════════════════════

    (test-case "H2 FIXED: tools/list returns MCP-shaped entries"
      ;; W2 FIX: tools/list uses MCP-specific serializer:
      ;; {name, description, inputSchema}, not OpenAI {type,function}.
      (define reg (make-test-registry))
      (define req (json-roundtrip (build-mcp-tools-list 1)))
      (define resp (handle-mcp-request req reg noop-execute))
      (define tools (parse-mcp-tools-list-response resp))
      (check-equal? (length tools) 1 "one tool registered")
      (define entry (car tools))
      ;; MCP shape
      (check-equal? (hash-ref entry 'name #f) "echo" "top-level name present")
      (check-equal? (hash-ref entry 'description #f) "Echo input" "top-level description present")
      (check-not-false (hash-ref entry 'inputSchema #f) "inputSchema present")
      ;; OpenAI shape is gone
      (check-false (hash-ref entry 'type #f) "no top-level OpenAI type key")
      (check-false (hash-ref entry 'function #f) "no nested OpenAI function key"))

    (test-case "H2 FIXED: MCP schema is JSON-serializable"
      (define reg (make-test-registry))
      (define req (json-roundtrip (build-mcp-tools-list 1)))
      (define resp (handle-mcp-request req reg noop-execute))
      (define tools (parse-mcp-tools-list-response resp))
      (define entry (car tools))
      (check-not-false (hash-ref entry 'name #f))
      (check-not-false (with-handlers ([exn:fail? (lambda (_) #f)])
                         (jsexpr->string resp))
                       "MCP tools/list response is JSON-serializable"))

    ;; ════════════════════════════════════════════════════════════
    ;; H3: JSON-RPC notification/error semantics incomplete
    ;; ════════════════════════════════════════════════════════════

    (test-case "H3 FIXED: notification produces empty hash (no jsonrpc key)"
      ;; JSON-RPC notifications must not receive responses.
      (define reg (make-test-registry))
      (define req (json-roundtrip (build-mcp-initialized-notification)))
      (define resp (handle-mcp-request req reg noop-execute))
      (check-false (hash-has-key? resp 'jsonrpc) "Notification produces empty hash (no response)"))

    (test-case "H3 FIXED: request without id is treated as notification"
      ;; JSON-RPC spec: a request without 'id is a notification and should
      ;; NOT get a response. id=null still means a response with id null/#f.
      (define reg (make-test-registry))
      (define req (hasheq 'jsonrpc "2.0" 'method "ping")) ;; no 'id key
      (define resp (handle-mcp-request req reg noop-execute))
      (check-false (hash-has-key? resp 'jsonrpc) "FIXED: notification without id gets no response"))

    (test-case "H3: unknown method returns -32601 method not found"
      ;; This is correct behavior. Characterize it.
      (define reg (make-test-registry))
      (define req (json-roundtrip (hasheq 'jsonrpc "2.0" 'id 1 'method "unknown/method")))
      (define resp (handle-mcp-request req reg noop-execute))
      (check-true (mcp-error-response? resp))
      (check-equal? (mcp-error-code resp) -32601)
      (check-equal? (mcp-error-message resp) "Method not found"))

    (test-case "H3 FIXED: missing params returns -32602 invalid params"
      (define reg (make-test-registry))
      (define req (json-roundtrip (hasheq 'jsonrpc "2.0" 'id 1 'method "tools/call")))
      ;; Missing 'params entirely — should produce -32602 invalid params
      (define resp (handle-mcp-request req reg noop-execute))
      (check-true (mcp-error-response? resp))
      (check-equal? (mcp-error-code resp) -32602)
      (check-not-false (regexp-match? #rx"Invalid params" (mcp-error-message resp))))

    (test-case "H3 FIXED: execute-fn exception returns -32603 internal error"
      (define reg (make-test-registry))
      (define crashing-fn (lambda (name args) (error 'crash "boom")))
      (define req (json-roundtrip (build-mcp-tools-call 1 "echo" (hasheq))))
      (define resp (handle-mcp-request req reg crashing-fn))
      (check-true (mcp-error-response? resp))
      (check-equal? (mcp-error-code resp) -32603)
      (check-equal? (mcp-error-message resp) "Internal error"))))

(run-tests suite)
