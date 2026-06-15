#lang racket/base

;; extensions/mcp-adapter.rkt — Model Context Protocol (MCP) adapter
;; STABILITY: evolving
;;
;; MAS Schritt 6: Verteilte Härtung & MCP-Integration
;;
;; MCP is an open protocol (JSON-RPC 2.0) for communication between
;; LLM applications and tool providers. No external Racket package required.
;;
;; Two modes:
;;   MCP Server: Expose q's tools as an MCP server over stdio.
;;   MCP Client: Connect to external MCP servers and register their tools.
;;
;; This module implements the protocol layer only — it handles JSON-RPC
;; message construction, parsing, and routing. The transport layer
;; (stdio or TCP) is handled by the caller.
;;
;; Feature-gated via mas.mcp.enabled (default false).

(require racket/contract
         racket/match
         json
         (only-in "../tools/registry.rkt" list-active-tools tool-registry? lookup-tool)
         (only-in "../tools/tool.rkt"
                  tool-result?
                  tool-result->jsexpr
                  tool?
                  tool-name
                  tool-description
                  tool-schema)
         (only-in "../util/version.rkt" q-version))

;; ============================================================
;; MCP Protocol Constants
;; ============================================================

(define MCP-PROTOCOL-VERSION "2024-11-05")
(define MCP-SERVER-NAME "q")
(define MCP-SERVER-VERSION q-version)

;; ============================================================
;; MCP Event Emission (H4 fix)
;; ============================================================

;; Event sink parameter for the real MCP path.
;; Procedure shape: (event-name-symbol data-hash -> void). Default is no-op.
(define current-mcp-event-sink (make-parameter void))

(define (safe-emit-mcp-event! event-name data)
  (define sink (current-mcp-event-sink))
  (when (procedure? sink)
    (with-handlers ([exn:fail? (lambda (_) (void))])
      (sink event-name data))))

(define (emit-mcp-connected! method)
  (safe-emit-mcp-event!
   'mas.mcp.connected
   (hasheq 'server-name MCP-SERVER-NAME 'method method 'protocol-version MCP-PROTOCOL-VERSION)))

(define (emit-mcp-tool-called! tool-name success? #:route [route 'local] #:error-code [error-code #f])
  (define base
    (hasheq 'tool-name tool-name 'server-name MCP-SERVER-NAME 'success? success? 'route route))
  (safe-emit-mcp-event! 'mas.mcp.tool.called
                        (if error-code
                            (hash-set base 'error-code error-code)
                            base)))

;; ============================================================
;; MCP Server: Tool Serialization (H2 fix)
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
;; MCP Server: Message Handler
;; ============================================================

;; Check if a request is a JSON-RPC notification (no 'id key present).
;; Per JSON-RPC 2.0 spec: "A Notification is a Request object without an id member."
;; A Request object MUST contain an id member (even if null).
(define (mcp-notification? req)
  (and (hash? req) (not (hash-has-key? req 'id))))

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
     (emit-mcp-tool-called! tool-name-str #f #:error-code -32602)
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
     (emit-mcp-tool-called! tool-name-str
                            #f
                            #:route (raw-result-route raw-result)
                            #:error-code -32603)
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
     (emit-mcp-tool-called! tool-name-str #t #:route (raw-result-route raw-result))
     (hasheq 'jsonrpc "2.0" 'id id 'result result)]))

;; Handle a single MCP request (JSON-RPC 2.0).
;; req: jsexpr — incoming request hash
;; registry: tool-registry? — q's tool registry
;; execute-fn: procedure — tool execution function (tool-name args -> result)
;; Returns: jsexpr — response hash (empty hash for notifications)
(define (handle-mcp-request req registry execute-fn)
  (define method (hash-ref req 'method ""))
  ;; H3 (v0.99.10 W2): Notifications (requests without 'id) get no response.
  (cond
    ;; This is a notification — no response per JSON-RPC spec.
    [(mcp-notification? req) (hasheq)]
    [else
     (define id (hash-ref req 'id #f))
     (match method
       ["initialize"
        (emit-mcp-connected! method)
        (hasheq 'jsonrpc
                "2.0"
                'id
                id
                'result
                (hasheq 'protocolVersion
                        MCP-PROTOCOL-VERSION
                        'capabilities
                        (hasheq 'tools (hasheq))
                        'serverInfo
                        (hasheq 'name MCP-SERVER-NAME 'version MCP-SERVER-VERSION)))]
       ;; H2 (v0.99.10 W2): Use MCP-specific tool serializer producing
       ;; {name, description, inputSchema} instead of OpenAI shape.
       ["tools/list"
        (hasheq 'jsonrpc "2.0" 'id id 'result (hasheq 'tools (tools->mcp-list registry)))]
       ["tools/call" (handle-tools-call req id registry execute-fn)]
       ["notifications/initialized" (hasheq)]
       ["ping" (hasheq 'jsonrpc "2.0" 'id id 'result (hasheq))]
       [_ (hasheq 'jsonrpc "2.0" 'id id 'error (hasheq 'code -32601 'message "Method not found"))])]))

;; ============================================================
;; MCP Client: Message Construction
;; ============================================================

;; Build an MCP initialize request.
(define (build-mcp-initialize id)
  (hasheq 'jsonrpc
          "2.0"
          'id
          id
          'method
          "initialize"
          'params
          (hasheq 'protocolVersion
                  MCP-PROTOCOL-VERSION
                  'clientInfo
                  (hasheq 'name MCP-SERVER-NAME 'version MCP-SERVER-VERSION)
                  'capabilities
                  (hasheq))))

;; Build an MCP tools/list request.
(define (build-mcp-tools-list id)
  (hasheq 'jsonrpc "2.0" 'id id 'method "tools/list" 'params (hasheq)))

;; Build an MCP tools/call request.
(define (build-mcp-tools-call id tool-name args)
  (hasheq 'jsonrpc
          "2.0"
          'id
          id
          'method
          "tools/call"
          'params
          (hasheq 'name tool-name 'arguments args)))

;; Build an MCP notifications/initialized message.
(define (build-mcp-initialized-notification)
  (hasheq 'jsonrpc "2.0" 'method "notifications/initialized"))

;; Build an MCP ping request.
(define (build-mcp-ping id)
  (hasheq 'jsonrpc "2.0" 'id id 'method "ping"))

;; ============================================================
;; MCP Client: Response Parsing
;; ============================================================

;; Parse an MCP initialize response.
;; Returns server capabilities hash or #f on error.
(define (parse-mcp-initialize-response resp)
  (define result (hash-ref resp 'result #f))
  (and result (hash-ref result 'capabilities #f)))

;; Parse an MCP tools/list response.
;; Returns list of tool hashes or '() on error.
(define (parse-mcp-tools-list-response resp)
  (define result (hash-ref resp 'result #f))
  (if result
      (hash-ref result 'tools '())
      '()))

;; Parse an MCP tools/call response.
;; Returns the content list or '() if missing.
(define (parse-mcp-tools-call-response resp)
  (define result (hash-ref resp 'result #f))
  (if result
      (hash-ref result 'content '())
      '()))

;; Check if a response is an error response.
(define (mcp-error-response? resp)
  (and (hash? resp) (hash-has-key? resp 'error)))

;; Extract error details from an error response.
;; Returns (cons code message) or #f.
(define (mcp-error-code resp)
  (define err (hash-ref resp 'error #f))
  (and err (hash-ref err 'code #f)))

(define (mcp-error-message resp)
  (define err (hash-ref resp 'error #f))
  (and err (hash-ref err 'message #f)))

;; ============================================================
;; MCP Stdio Server Loop
;; ============================================================

;; Parameter for tool execution injection.
;; The wiring layer sets this to the real tool execution function.
;; Default returns a "not implemented" placeholder.
(define current-mcp-execute-fn
  (make-parameter (lambda (name args)
                    (hasheq 'content (list (hasheq 'type "text" 'text "not implemented"))))))

;; Process a raw line of input from the stdio transport.
;; Handles the JSON parse boundary per JSON-RPC 2.0 spec:
;;   - Invalid JSON      -> -32700 Parse error (id null)
;;   - Valid non-object  -> -32600 Invalid Request (id null)
;;   - Valid JSON-RPC    -> delegate to handle-mcp-request
;; Returns a hash response (empty for notifications).
(define (handle-mcp-raw-input line registry execute-fn)
  (define-values (parse-ok? req)
    (with-handlers ([exn:fail? (lambda (_) (values #f #f))])
      (values #t (string->jsexpr line))))
  (cond
    [(not parse-ok?)
     (hasheq 'jsonrpc "2.0" 'id #f 'error (hasheq 'code -32700 'message "Parse error"))]
    [(not (hash? req))
     (hasheq 'jsonrpc "2.0" 'id #f 'error (hasheq 'code -32600 'message "Invalid Request"))]
    [else (handle-mcp-request req registry execute-fn)]))

;; Run the MCP server loop on stdin/stdout.
;; Reads JSON-RPC requests from stdin, writes responses to stdout.
;; This is the standard MCP transport for local servers.
(define (run-mcp-stdio-server! registry)
  (define execute-fn (current-mcp-execute-fn))
  (let loop ()
    (define line (read-line (current-input-port) 'any))
    (unless (eof-object? line)
      (define resp (handle-mcp-raw-input line registry execute-fn))
      ;; Only write responses (not notifications or empty hashes)
      (when (and (hash? resp) (hash-has-key? resp 'jsonrpc))
        (write-json resp (current-output-port))
        (newline (current-output-port))
        (flush-output (current-output-port)))
      (loop))))

;; ============================================================
;; Provides
;; ============================================================

(provide MCP-PROTOCOL-VERSION
         MCP-SERVER-NAME
         MCP-SERVER-VERSION
         current-mcp-execute-fn
         current-mcp-event-sink)

(provide (contract-out [handle-mcp-request (-> hash? tool-registry? procedure? hash?)]
                       [handle-mcp-raw-input (-> string? tool-registry? procedure? hash?)]
                       [build-mcp-initialize (-> (or/c string? number?) hash?)]
                       [build-mcp-tools-list (-> (or/c string? number?) hash?)]
                       [build-mcp-tools-call (-> (or/c string? number?) string? hash? hash?)]
                       [build-mcp-initialized-notification (-> hash?)]
                       [build-mcp-ping (-> (or/c string? number?) hash?)]
                       [parse-mcp-initialize-response (-> hash? (or/c hash? #f))]
                       [parse-mcp-tools-list-response (-> hash? (listof hash?))]
                       [parse-mcp-tools-call-response (-> hash? (listof hash?))]
                       [mcp-error-response? (-> any/c boolean?)]
                       [mcp-error-code (-> hash? (or/c number? #f))]
                       [mcp-error-message (-> hash? (or/c string? #f))]
                       [run-mcp-stdio-server! (-> tool-registry? void?)]
                       [tool->mcp-jsexpr (-> tool? hash?)]
                       [tools->mcp-list (-> tool-registry? (listof hash?))]
                       [mcp-notification? (-> any/c boolean?)]))
