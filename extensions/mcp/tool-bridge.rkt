#lang racket/base

;; extensions/mcp/tool-bridge.rkt — MCP tool serialization and call handling
;; STABILITY: evolving
;;
;; Sub-module of the MCP adapter split. Handles tool serialization into
;; MCP spec format, tools/call request validation, execution, and
;; result conversion.
;;
;; Part of the mcp-adapter module split (T-4).

(require racket/contract
         racket/match
         json
         (only-in "../../tools/registry.rkt" list-active-tools tool-registry? lookup-tool)
         (only-in "../../tools/tool.rkt"
                  tool-result?
                  tool-result->jsexpr
                  tool?
                  tool-name
                  tool-description
                  tool-schema))

;; ============================================================
;; MCP Server: Tool Serialization
;; ============================================================

;; Serialize a single tool into the MCP spec format.
;; MCP requires: {name, description, inputSchema}
;; NOT the OpenAI format: {type: "function", function: {name, ...}}
(define (tool->mcp-jsexpr t)
  (hasheq 'name (tool-name t) 'description (tool-description t) 'inputSchema (tool-schema t)))

;; Serialize all active tools into MCP spec format.
(define (tools->mcp-list registry)
  (map tool->mcp-jsexpr (list-active-tools registry)))

;; ============================================================
;; MCP Server: Tools/Call Handling
;; ============================================================

;; H3 (v0.99.10 W2): Handle tools/call request with full validation and error handling.
;; Extracted from handle-mcp-request to keep match clauses readable.
(define (handle-tools-call req id registry execute-fn)
  ;; H3: Validate params presence — return -32602 if missing.
  ;; F-02 (v0.99.11 W2): Validate params type and structure.
  (cond
    [(not (hash-has-key? req 'params))
     (hasheq 'jsonrpc
             "2.0"
             'id
             id
             'error
             (hasheq 'code -32602 'message "Invalid params: missing 'params'"))]
    [else
     (define params (hash-ref req 'params))
     (cond
       ;; F-02: params must be a hash/object.
       [(not (hash? params))
        (hasheq 'jsonrpc
                "2.0"
                'id
                id
                'error
                (hasheq 'code -32602 'message "Invalid params: params must be an object"))]
       [else
        (define tool-name-str (hash-ref params 'name ""))
        (define tool-args (hash-ref params 'arguments (hasheq)))
        (cond
          ;; F-02: name must be a non-empty string.
          [(not (and (string? tool-name-str) (positive? (string-length tool-name-str))))
           (hasheq
            'jsonrpc
            "2.0"
            'id
            id
            'error
            (hasheq 'code -32602 'message "Invalid params: 'name' must be a non-empty string"))]
          ;; F-02: arguments must be a hash/object if present.
          [(not (hash? tool-args))
           (hasheq 'jsonrpc
                   "2.0"
                   'id
                   id
                   'error
                   (hasheq 'code -32602 'message "Invalid params: 'arguments' must be an object"))]
          [else (handle-tools-call-exec id tool-name-str tool-args registry execute-fn)])])]))

;; Inner handler for tools/call once params are validated.
;; C1: check tool existence. H3: wrap exceptions. C2: convert tool-result.
(define (handle-tools-call-exec id tool-name-str tool-args registry execute-fn)
  (cond
    ;; C1 (v0.99.10 W1): Reject unknown tools with JSON-RPC error.
    [(not (lookup-tool registry tool-name-str))
     (hasheq 'jsonrpc
             "2.0"
             'id
             id
             'error
             (hasheq 'code
                     -32602
                     'message
                     (format "Unknown tool: ~a" tool-name-str)
                     'data
                     (hasheq 'tool-name tool-name-str)))]
    [else
     ;; H3 (v0.99.10 W2): Wrap execute-fn exceptions into -32603 internal error.
     (define raw-result
       (with-handlers ([exn:fail? (lambda (e) (hasheq '__internal-error (exn-message e)))])
         (execute-fn tool-name-str tool-args)))
     (handle-tools-call-result id tool-name-str raw-result)]))

(define (raw-result-route raw-result)
  (if (hash? raw-result)
      (hash-ref raw-result 'route 'local)
      'local))

;; Convert raw execute-fn result to MCP response.
(define (handle-tools-call-result id tool-name-str raw-result)
  (cond
    [(and (hash? raw-result) (hash-has-key? raw-result '__internal-error))
     ;; F-03 (v0.99.11 W2): Do NOT expose internal exception details to clients.
     ;; The error message is logged server-side only.
     (hasheq 'jsonrpc "2.0" 'id id 'error (hasheq 'code -32603 'message "Internal error"))]
    [else
     ;; C2 (v0.99.10 W1): Convert tool-result structs to MCP-compatible jsexpr.
     (define result
       (cond
         [(tool-result? raw-result) (tool-result->jsexpr raw-result)]
         [(hash? raw-result) raw-result]
         [else (hasheq 'content (list (hasheq 'type "text" 'text (format "~a" raw-result))))]))
     (hasheq 'jsonrpc "2.0" 'id id 'result result)]))

;; ============================================================
;; Provides
;; ============================================================

(provide (contract-out [tool->mcp-jsexpr (-> tool? hash?)]
                       [tools->mcp-list (-> tool-registry? (listof hash?))]
                       [handle-tools-call (-> hash? any/c tool-registry? procedure? hash?)]
                       [handle-tools-call-exec (-> any/c string? hash? tool-registry? procedure? hash?)]
                       [handle-tools-call-result (-> any/c string? any/c hash?)]
                       [raw-result-route (-> any/c symbol?)]))
