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
         (only-in "../tools/registry.rkt" list-tools-jsexpr tool-registry? lookup-tool)
         (only-in "../tools/tool.rkt" tool-result? tool-result->jsexpr))

;; ============================================================
;; MCP Protocol Constants
;; ============================================================

(define MCP-PROTOCOL-VERSION "2024-11-05")
(define MCP-SERVER-NAME "q")
(define MCP-SERVER-VERSION "0.99.9")

;; ============================================================
;; MCP Server: Message Handler
;; ============================================================

;; Handle a single MCP request (JSON-RPC 2.0).
;; req: jsexpr — incoming request hash
;; registry: tool-registry? — q's tool registry
;; execute-fn: procedure — tool execution function (tool-name args -> result)
;; Returns: jsexpr — response hash
(define (handle-mcp-request req registry execute-fn)
  (define method (hash-ref req 'method ""))
  (define id (hash-ref req 'id #f))
  (match method
    ["initialize"
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
    ["tools/list" (hasheq 'jsonrpc "2.0" 'id id 'result (hasheq 'tools (list-tools-jexpr registry)))]
    ["tools/call"
     (define params (hash-ref req 'params (hasheq)))
     (define tool-name (hash-ref params 'name ""))
     (define tool-args (hash-ref params 'arguments (hasheq)))
     (cond
       ;; C1 (v0.99.10 W1): Reject unknown tools with JSON-RPC error.
       ;; Previously execute-fn was called blindly, returning fake content.
       [(not (lookup-tool registry tool-name))
        (hasheq 'jsonrpc
                "2.0"
                'id
                id
                'error
                (hasheq 'code
                        -32602
                        'message
                        (format "Unknown tool: ~a" tool-name)
                        'data
                        (hasheq 'tool-name tool-name)))]
       [else
        ;; C2 (v0.99.10 W1): Convert tool-result structs to MCP-compatible jsexpr.
        ;; Previously raw structs passed through, failing JSON serialization.
        (define raw-result (execute-fn tool-name tool-args))
        (define result
          (cond
            [(tool-result? raw-result) (tool-result->jsexpr raw-result)]
            [(hash? raw-result) raw-result]
            [else (hasheq 'content (list (hasheq 'type "text" 'text (format "~a" raw-result))))]))
        (hasheq 'jsonrpc "2.0" 'id id 'result result)])]
    ;; Notification — no response needed
    ["notifications/initialized" (hasheq)]
    ["ping" (hasheq 'jsonrpc "2.0" 'id id 'result (hasheq))]
    [_ (hasheq 'jsonrpc "2.0" 'id id 'error (hasheq 'code -32601 'message "Method not found"))]))

;; Alias for list-tools-jsexpr (avoid name collision with contract export)
(define list-tools-jexpr list-tools-jsexpr)

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

;; Run the MCP server loop on stdin/stdout.
;; Reads JSON-RPC requests from stdin, writes responses to stdout.
;; This is the standard MCP transport for local servers.
(define (run-mcp-stdio-server! registry)
  (define execute-fn (current-mcp-execute-fn))
  (let loop ()
    (define line (read-line (current-input-port) 'any))
    (unless (eof-object? line)
      (define req
        (with-handlers ([exn:fail? (lambda (_) #f)])
          (string->jsexpr line)))
      (when (and req (hash? req))
        (define resp (handle-mcp-request req registry execute-fn))
        ;; Only write responses (not notifications or empty hashes)
        (when (and (hash? resp) (hash-has-key? resp 'jsonrpc))
          (write-json resp (current-output-port))
          (newline (current-output-port))
          (flush-output (current-output-port))))
      (loop))))

;; ============================================================
;; Provides
;; ============================================================

(provide MCP-PROTOCOL-VERSION
         MCP-SERVER-NAME
         MCP-SERVER-VERSION
         current-mcp-execute-fn)

(provide (contract-out [handle-mcp-request (-> hash? tool-registry? procedure? hash?)]
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
                       [run-mcp-stdio-server! (-> tool-registry? void?)]))
