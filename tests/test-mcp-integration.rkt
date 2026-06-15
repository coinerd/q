#lang racket

;; @speed fast  ;; @suite integration

;; tests/test-mcp-integration.rkt — W6 (v0.99.10) MCP End-to-End Integration Tests
;;
;; Exercises the protocol lifecycle without networking:
;;   initialize → initialized notification → tools/list → tools/call → ping

(require rackunit
         rackunit/text-ui
         json
         "../extensions/mcp-adapter.rkt"
         "../tools/registry.rkt"
         (only-in "../tools/tool.rkt" make-tool make-success-result)
         "../util/version.rkt")

(define (json-roundtrip v)
  (string->jsexpr (jsexpr->string v)))

(define (make-integration-registry)
  (define reg (make-tool-registry))
  (register-tool!
   reg
   (make-tool "echo"
              "Echo an input string"
              (hasheq 'type "object" 'properties (hasheq 'text (hasheq 'type "string")))
              (lambda (args ctx) (make-success-result (hash-ref args 'text "")))))
  reg)

(define (make-recording-execute-fn calls)
  (lambda (name args)
    (set-box! calls (cons (list name args) (unbox calls)))
    (hasheq 'content
            (list (hasheq 'type "text" 'text (format "~a:~a" name (hash-ref args 'text "")))))))

(define (make-event-recorder events)
  (lambda (event-name data) (set-box! events (append (unbox events) (list (cons event-name data))))))

(define suite
  (test-suite "MCP Integration (v0.99.10 W6)"

    (test-case "MCP server version is synchronized with q-version"
      (check-equal? MCP-SERVER-VERSION q-version))

    (test-case "initialize request round-trips through handler and parser"
      (define reg (make-integration-registry))
      (define req (json-roundtrip (build-mcp-initialize 1)))
      (define resp (handle-mcp-request req reg (make-recording-execute-fn (box '()))))
      (define result (hash-ref resp 'result))
      (check-equal? (hash-ref result 'protocolVersion) MCP-PROTOCOL-VERSION)
      (check-equal? (hash-ref (hash-ref result 'serverInfo) 'name) "q")
      (check-not-false (parse-mcp-initialize-response resp)))

    (test-case "H4 FIXED: initialize emits mas.mcp.connected event from real handler"
      (define reg (make-integration-registry))
      (define events (box '()))
      (parameterize ([current-mcp-event-sink (make-event-recorder events)])
        (define resp
          (handle-mcp-request (json-roundtrip (build-mcp-initialize 101))
                              reg
                              (make-recording-execute-fn (box '()))))
        (check-not-false (parse-mcp-initialize-response resp)))
      (check-equal? (map car (unbox events)) '(mas.mcp.connected))
      (define data (cdr (car (unbox events))))
      (check-equal? (hash-ref data 'server-name) "q")
      (check-equal? (hash-ref data 'method) "initialize"))

    (test-case "initialized notification produces no JSON-RPC response"
      (define reg (make-integration-registry))
      (define req (json-roundtrip (build-mcp-initialized-notification)))
      (define resp (handle-mcp-request req reg (make-recording-execute-fn (box '()))))
      (check-false (hash-has-key? resp 'jsonrpc)))

    (test-case "tools/list request returns registered tool metadata"
      (define reg (make-integration-registry))
      (define req (json-roundtrip (build-mcp-tools-list 2)))
      (define resp (handle-mcp-request req reg (make-recording-execute-fn (box '()))))
      (define tools (parse-mcp-tools-list-response resp))
      (check-equal? (length tools) 1)
      (define tool-entry (car tools))
      ;; H2 (v0.99.10 W2): MCP schema uses {name, description, inputSchema}
      (check-equal? (hash-ref tool-entry 'name) "echo")
      (check-not-false (hash-ref tool-entry 'description #f))
      (check-not-false (hash-ref tool-entry 'inputSchema #f)))

    (test-case "tools/call request routes to execute-fn and parses content"
      (define reg (make-integration-registry))
      (define calls (box '()))
      (define req (json-roundtrip (build-mcp-tools-call 3 "echo" (hasheq 'text "hello"))))
      (define resp (handle-mcp-request req reg (make-recording-execute-fn calls)))
      (define content (parse-mcp-tools-call-response resp))
      (check-equal? (unbox calls) (list (list "echo" (hasheq 'text "hello"))))
      (check-equal? (length content) 1)
      (check-equal? (hash-ref (car content) 'text) "echo:hello"))

    (test-case "H4 FIXED: tools/call emits success event from real handler"
      (define reg (make-integration-registry))
      (define events (box '()))
      (parameterize ([current-mcp-event-sink (make-event-recorder events)])
        (define resp
          (handle-mcp-request
           (json-roundtrip (build-mcp-tools-call 303 "echo" (hasheq 'text "hello")))
           reg
           (make-recording-execute-fn (box '()))))
        (check-equal? (length (parse-mcp-tools-call-response resp)) 1))
      (check-equal? (map car (unbox events)) '(mas.mcp.tool.called))
      (define data (cdr (car (unbox events))))
      (check-equal? (hash-ref data 'tool-name) "echo")
      (check-equal? (hash-ref data 'success?) #t)
      (check-equal? (hash-ref data 'route) 'local))

    (test-case "H4 FIXED: tools/call emits failure event when executor raises"
      (define reg (make-integration-registry))
      (define events (box '()))
      (parameterize ([current-mcp-event-sink (make-event-recorder events)])
        (define resp
          (handle-mcp-request (json-roundtrip (build-mcp-tools-call 304 "echo" (hasheq 'text "boom")))
                              reg
                              (lambda (_name _args) (error 'boom "executor failed"))))
        (check-true (mcp-error-response? resp)))
      (check-equal? (map car (unbox events)) '(mas.mcp.tool.called))
      (define data (cdr (car (unbox events))))
      (check-equal? (hash-ref data 'tool-name) "echo")
      (check-equal? (hash-ref data 'success?) #f)
      (check-equal? (hash-ref data 'error-code) -32603))

    (test-case "ping request succeeds after tool call lifecycle"
      (define reg (make-integration-registry))
      (define resp
        (handle-mcp-request (json-roundtrip (build-mcp-ping "ping-1"))
                            reg
                            (make-recording-execute-fn (box '()))))
      (check-equal? (hash-ref resp 'id) "ping-1")
      (check-true (hash-has-key? resp 'result)))

    (test-case "unknown MCP method is reported as JSON-RPC method-not-found"
      (define reg (make-integration-registry))
      (define resp
        (handle-mcp-request (hasheq 'jsonrpc "2.0" 'id 99 'method "missing/method")
                            reg
                            (make-recording-execute-fn (box '()))))
      (check-true (mcp-error-response? resp))
      (check-equal? (mcp-error-code resp) -32601)
      (check-equal? (mcp-error-message resp) "Method not found"))))

(run-tests suite)
