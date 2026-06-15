#lang racket

;; @speed fast  ;; @suite security

;; tests/test-mcp-protocol-compliance.rkt — W0 (v0.99.10) MCP Protocol Compliance Characterization
;;
;; CHARACTERIZATION TESTS — These document the CURRENT (buggy) behavior.
;; They will be flipped in W2 to assert the CORRECT behavior.
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
  (test-suite "MCP Protocol Compliance (W0 Characterization)"

    ;; ════════════════════════════════════════════════════════════
    ;; H2: tools/list uses OpenAI function schema, not MCP tool schema
    ;; ════════════════════════════════════════════════════════════

    (test-case "CHARACTERIZE H2: tools/list returns OpenAI-shaped entries"
      ;; CURRENT BUG: tools/list reuses list-tools-jsexpr which produces
      ;; OpenAI-style entries: {type: "function", function: {name, description, ...}}
      ;; MCP spec requires: {name, description, inputSchema}
      ;; W2 FIX: add MCP-specific tool serializer.
      (define reg (make-test-registry))
      (define req (json-roundtrip (build-mcp-tools-list 1)))
      (define resp (handle-mcp-request req reg noop-execute))
      (define tools (parse-mcp-tools-list-response resp))
      (check-equal? (length tools) 1 "one tool registered")
      (define entry (car tools))
      ;; CHARACTERIZATION: entry HAS 'type and 'function keys (OpenAI shape)
      (check-equal? (hash-ref entry 'type #f)
                    "function"
                    "CURRENT: OpenAI-shaped entry with type='function'")
      (check-not-false (hash-ref entry 'function #f)
                       "CURRENT: has nested 'function' key (OpenAI shape)")
      ;; CHARACTERIZATION: entry does NOT have top-level 'name' (MCP shape)
      (check-false (hash-ref entry 'name #f)
                   "CURRENT: no top-level 'name' (MCP schema would have it)")
      (check-false (hash-ref entry 'inputSchema #f)
                   "CURRENT: no 'inputSchema' (MCP schema would have it)"))

    (test-case "CORRECT MCP SCHEMA (for W2): what the fix should produce"
      ;; This documents the target MCP schema shape.
      (define reg (make-test-registry))
      (define req (json-roundtrip (build-mcp-tools-list 1)))
      (define resp (handle-mcp-request req reg noop-execute))
      (define tools (parse-mcp-tools-list-response resp))
      (define entry (car tools))
      ;; In W2, these will all pass after the serializer is fixed
      ;; For now, the MCP fields are ABSENT (characterized above)
      (check-false (hash-ref entry 'name #f) "W2 will add 'name'"))

    ;; ════════════════════════════════════════════════════════════
    ;; H3: JSON-RPC notification/error semantics incomplete
    ;; ════════════════════════════════════════════════════════════

    (test-case "CHARACTERIZE H3: notification produces empty hash (no jsonrpc key)"
      ;; CURRENT: notifications/initialized returns (hasheq) — an empty hash.
      ;; This is actually CORRECT behavior: notifications should not get responses.
      ;; The stdio loop checks for 'jsonrpc key presence before writing.
      ;; W2 will add explicit notification detection for all notification methods.
      (define reg (make-test-registry))
      (define req (json-roundtrip (build-mcp-initialized-notification)))
      (define resp (handle-mcp-request req reg noop-execute))
      (check-false (hash-has-key? resp 'jsonrpc) "Notification produces empty hash (no response)"))

    (test-case "CHARACTERIZE H3: request without id returns response with id=#f"
      ;; CURRENT: a request without 'id gets id=#f from hash-ref.
      ;; JSON-RPC spec: a request without 'id is a notification and should
      ;; NOT get a response. But current code treats it as a request with id=#f.
      ;; W2 FIX: distinguish absent id (notification) from id=null (request).
      (define reg (make-test-registry))
      (define req (hasheq 'jsonrpc "2.0" 'method "ping")) ;; no 'id key
      (define resp (handle-mcp-request req reg noop-execute))
      ;; CHARACTERIZATION: response IS produced (it should not be for a notification)
      (check-not-false (hash-has-key? resp 'jsonrpc)
                       "CURRENT BUG: notification without id still gets a response"))

    (test-case "CHARACTERIZE H3: unknown method returns -32601 method not found"
      ;; This is correct behavior. Characterize it.
      (define reg (make-test-registry))
      (define req (json-roundtrip (hasheq 'jsonrpc "2.0" 'id 1 'method "unknown/method")))
      (define resp (handle-mcp-request req reg noop-execute))
      (check-true (mcp-error-response? resp))
      (check-equal? (mcp-error-code resp) -32601)
      (check-equal? (mcp-error-message resp) "Method not found"))

    (test-case "CHARACTERIZE H3: missing JSON-RPC error codes"
      ;; CURRENT: only -32601 (method not found) is implemented.
      ;; Missing: -32700 (parse error), -32600 (invalid request),
      ;;          -32602 (invalid params), -32603 (internal error)
      ;; W2 will add these.
      ;; Characterize: malformed params don't produce -32602
      (define reg (make-test-registry))
      (define req (json-roundtrip (hasheq 'jsonrpc "2.0" 'id 1 'method "tools/call")))
      ;; Missing 'params entirely — should produce -32602 invalid params
      (define resp (handle-mcp-request req reg noop-execute))
      ;; CURRENT: no error, returns a result (with empty name lookup)
      (check-false (mcp-error-response? resp)
                   "CURRENT BUG: missing params does not produce -32602 error"))

    (test-case "CHARACTERIZE H3: execute-fn exception is not caught"
      ;; CURRENT: if execute-fn raises, the exception propagates uncaught.
      ;; W2 FIX: wrap exceptions into -32603 internal error.
      (define reg (make-test-registry))
      (define crashing-fn (lambda (name args) (error 'crash "boom")))
      (define req (json-roundtrip (build-mcp-tools-call 1 "echo" (hasheq))))
      (check-exn exn:fail?
                 (lambda () (handle-mcp-request req reg crashing-fn))
                 "CURRENT BUG: execute-fn exception propagates uncaught"))))

(run-tests suite)
