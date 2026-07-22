#lang racket/base

;; extensions/mcp/protocol.rkt — MCP protocol dispatch and event emission
;; STABILITY: evolving
;;
;; Sub-module of the MCP adapter split. Handles JSON-RPC 2.0 protocol
;; dispatch: request handling, notification detection, stdio server loop,
;; and MCP event emission.
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
                  tool-schema)
         (only-in "../../util/version.rkt" q-version)
         "tool-bridge.rkt")

;; ============================================================
;; MCP Protocol Constants
;; ============================================================

(define MCP-PROTOCOL-VERSION "2024-11-05")
(define MCP-SERVER-NAME "q")
(define MCP-SERVER-VERSION q-version)

;; ============================================================
;; MCP Event Emission
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
;; MCP Server: Tool Execution Parameter
;; ============================================================

;; Parameter for tool execution injection.
;; The wiring layer sets this to the real tool execution function.
;; Default returns a "not implemented" placeholder.
(define current-mcp-execute-fn
  (make-parameter (lambda (name args)
                    (hasheq 'content (list (hasheq 'type "text" 'text "not implemented"))))))

;; ============================================================
;; MCP Server: Message Handler
;; ============================================================

;; Check if a request is a JSON-RPC notification (no 'id key present).
;; Per JSON-RPC 2.0 spec: "A Notification is a Request object without an id member."
;; A Request object MUST contain an id member (even if null).
(define (mcp-notification? req)
  (and (hash? req) (not (hash-has-key? req 'id))))

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
    ;; JSON-RPC 2.0 spec: invalid JSON or EOF must produce -32700 Parse error
    ;; with id: null. Racket 'null serializes to JSON null; #f serializes to false.
    [(or (not parse-ok?) (eof-object? req))
     (hasheq 'jsonrpc "2.0" 'id 'null 'error (hasheq 'code -32700 'message "Parse error"))]
    [(not (hash? req))
     (hasheq 'jsonrpc "2.0" 'id 'null 'error (hasheq 'code -32600 'message "Invalid Request"))]
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
         current-mcp-event-sink
         safe-emit-mcp-event!
         emit-mcp-connected!
         emit-mcp-tool-called!)

(provide (contract-out [handle-mcp-request (-> hash? tool-registry? procedure? hash?)]
                       [handle-mcp-raw-input (-> string? tool-registry? procedure? hash?)]
                       [run-mcp-stdio-server! (-> tool-registry? void?)]
                       [mcp-notification? (-> any/c boolean?)]))
