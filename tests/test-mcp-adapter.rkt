#lang racket

;; @speed fast  ;; @suite extensions

;; tests/test-mcp-adapter.rkt — W2 (v0.99.9) MCP Adapter Tests
;;
;; Tests MCP JSON-RPC protocol layer: message handling, construction,
;; response parsing. No subprocess or networking required.

(require rackunit
         rackunit/text-ui
         json
         "../extensions/mcp-adapter.rkt"
         "../tools/registry.rkt"
         (only-in "../tools/tool.rkt" make-tool make-success-result))

;; ── Test helpers ──

(define (make-test-registry)
  (define reg (make-tool-registry))
  (register-tool! reg
                  (make-tool "test-tool"
                             "A test tool"
                             (hasheq 'type "object" 'properties (hasheq))
                             (lambda (args ctx) (make-success-result "test result"))))
  reg)

(define (make-mock-execute-fn [results (make-hash)])
  (lambda (name args)
    (hash-ref results
              name
              (lambda ()
                (hasheq 'content (list (hasheq 'type "text" 'text (format "executed ~a" name))))))))

(define suite
  (test-suite "MCP Adapter (v0.99.9 W2)"

    ;; ── Protocol constants ──

    (test-case "MCP-PROTOCOL-VERSION is correct"
      (check-equal? MCP-PROTOCOL-VERSION "2024-11-05"))

    (test-case "MCP-SERVER-NAME is q"
      (check-equal? MCP-SERVER-NAME "q"))

    ;; ── handle-mcp-request: initialize ──

    (test-case "initialize returns protocol version"
      (define reg (make-test-registry))
      (define exec-fn (make-mock-execute-fn))
      (define req (hasheq 'jsonrpc "2.0" 'id 1 'method "initialize"))
      (define resp (handle-mcp-request req reg exec-fn))
      (check-equal? (hash-ref resp 'jsonrpc) "2.0")
      (check-equal? (hash-ref resp 'id) 1)
      (define result (hash-ref resp 'result))
      (check-equal? (hash-ref result 'protocolVersion) MCP-PROTOCOL-VERSION))

    (test-case "initialize includes serverInfo"
      (define reg (make-test-registry))
      (define exec-fn (make-mock-execute-fn))
      (define req (hasheq 'jsonrpc "2.0" 'id 1 'method "initialize"))
      (define resp (handle-mcp-request req reg exec-fn))
      (define result (hash-ref resp 'result))
      (define server-info (hash-ref result 'serverInfo))
      (check-equal? (hash-ref server-info 'name) "q"))

    (test-case "initialize includes tools capability"
      (define reg (make-test-registry))
      (define exec-fn (make-mock-execute-fn))
      (define req (hasheq 'jsonrpc "2.0" 'id 1 'method "initialize"))
      (define resp (handle-mcp-request req reg exec-fn))
      (define result (hash-ref resp 'result))
      (define capabilities (hash-ref result 'capabilities))
      (check-true (hash-has-key? capabilities 'tools)))

    ;; ── handle-mcp-request: tools/list ──

    (test-case "tools/list returns registered tools"
      (define reg (make-test-registry))
      (define exec-fn (make-mock-execute-fn))
      (define req (hasheq 'jsonrpc "2.0" 'id 2 'method "tools/list"))
      (define resp (handle-mcp-request req reg exec-fn))
      (define result (hash-ref resp 'result))
      (define tools (hash-ref result 'tools))
      (check-true (list? tools))
      (check-true (>= (length tools) 1)))

    (test-case "tools/list with empty registry returns empty list"
      (define reg (make-tool-registry))
      (define exec-fn (make-mock-execute-fn))
      (define req (hasheq 'jsonrpc "2.0" 'id 2 'method "tools/list"))
      (define resp (handle-mcp-request req reg exec-fn))
      (define result (hash-ref resp 'result))
      (define tools (hash-ref result 'tools))
      (check-equal? tools '()))

    ;; ── handle-mcp-request: tools/call ──

    (test-case "tools/call routes to execute-fn"
      (define reg (make-test-registry))
      (define call-log '())
      (define exec-fn
        (lambda (name args)
          (set! call-log (cons (list name args) call-log))
          (hasheq 'content (list (hasheq 'type "text" 'text "ok")))))
      (define req
        (hasheq 'jsonrpc
                "2.0"
                'id
                3
                'method
                "tools/call"
                'params
                (hasheq 'name "my-tool" 'arguments (hasheq 'x 1))))
      (define resp (handle-mcp-request req reg exec-fn))
      (check-equal? (hash-ref resp 'id) 3)
      (check-true (hash-has-key? resp 'result))
      ;; The execute-fn was called with correct args
      (check-equal? (length call-log) 1)
      (check-equal? (first (first call-log)) "my-tool"))

    (test-case "tools/call with empty arguments"
      (define reg (make-test-registry))
      (define exec-fn (make-mock-execute-fn))
      (define req
        (hasheq 'jsonrpc "2.0" 'id 3 'method "tools/call" 'params (hasheq 'name "test-tool")))
      (define resp (handle-mcp-request req reg exec-fn))
      (check-true (hash-has-key? resp 'result)))

    ;; ── handle-mcp-request: ping ──

    (test-case "ping returns empty result"
      (define reg (make-test-registry))
      (define exec-fn (make-mock-execute-fn))
      (define req (hasheq 'jsonrpc "2.0" 'id 4 'method "ping"))
      (define resp (handle-mcp-request req reg exec-fn))
      (check-equal? (hash-ref resp 'id) 4)
      (check-true (hash-has-key? resp 'result)))

    ;; ── handle-mcp-request: unknown method ──

    (test-case "unknown method returns error -32601"
      (define reg (make-test-registry))
      (define exec-fn (make-mock-execute-fn))
      (define req (hasheq 'jsonrpc "2.0" 'id 5 'method "unknown/method"))
      (define resp (handle-mcp-request req reg exec-fn))
      (define err (hash-ref resp 'error))
      (check-equal? (hash-ref err 'code) -32601)
      (check-true (string-contains? (hash-ref err 'message) "Method not found")))

    ;; ── handle-mcp-request: notifications/initialized ──

    (test-case "notifications/initialized returns empty hash (no response)"
      (define reg (make-test-registry))
      (define exec-fn (make-mock-execute-fn))
      (define req (hasheq 'jsonrpc "2.0" 'method "notifications/initialized"))
      (define resp (handle-mcp-request req reg exec-fn))
      ;; Should return empty hash — no jsonrpc key means stdio loop won't write it
      (check-false (hash-has-key? resp 'jsonrpc)))

    ;; ── Client: build-mcp-initialize ──

    (test-case "build-mcp-initialize produces valid structure"
      (define req (build-mcp-initialize 1))
      (check-equal? (hash-ref req 'jsonrpc) "2.0")
      (check-equal? (hash-ref req 'method) "initialize")
      (check-equal? (hash-ref req 'id) 1)
      (define params (hash-ref req 'params))
      (check-equal? (hash-ref params 'protocolVersion) MCP-PROTOCOL-VERSION))

    (test-case "build-mcp-initialize with string id"
      (define req (build-mcp-initialize "req-1"))
      (check-equal? (hash-ref req 'id) "req-1"))

    ;; ── Client: build-mcp-tools-list ──

    (test-case "build-mcp-tools-list produces valid structure"
      (define req (build-mcp-tools-list 2))
      (check-equal? (hash-ref req 'jsonrpc) "2.0")
      (check-equal? (hash-ref req 'method) "tools/list")
      (check-equal? (hash-ref req 'id) 2))

    ;; ── Client: build-mcp-tools-call ──

    (test-case "build-mcp-tools-call includes name and arguments"
      (define req (build-mcp-tools-call 3 "my-tool" (hasheq 'key "value")))
      (check-equal? (hash-ref req 'method) "tools/call")
      (define params (hash-ref req 'params))
      (check-equal? (hash-ref params 'name) "my-tool")
      (check-equal? (hash-ref (hash-ref params 'arguments) 'key) "value"))

    ;; ── Client: build-mcp-initialized-notification ──

    (test-case "build-mcp-initialized-notification has no id"
      (define notif (build-mcp-initialized-notification))
      (check-false (hash-has-key? notif 'id))
      (check-equal? (hash-ref notif 'method) "notifications/initialized"))

    ;; ── Client: build-mcp-ping ──

    (test-case "build-mcp-ping produces valid structure"
      (define req (build-mcp-ping 4))
      (check-equal? (hash-ref req 'method) "ping")
      (check-equal? (hash-ref req 'id) 4))

    ;; ── Response parsing: parse-mcp-initialize-response ──

    (test-case "parse-mcp-initialize-response extracts capabilities"
      (define resp (hasheq 'result (hasheq 'capabilities (hasheq 'tools (hasheq)))))
      (define caps (parse-mcp-initialize-response resp))
      (check-true (hash? caps))
      (check-true (hash-has-key? caps 'tools)))

    (test-case "parse-mcp-initialize-response returns #f on error response"
      (define resp (hasheq 'error (hasheq 'code -1 'message "err")))
      (check-false (parse-mcp-initialize-response resp)))

    ;; ── Response parsing: parse-mcp-tools-list-response ──

    (test-case "parse-mcp-tools-list-response extracts tools"
      (define resp
        (hasheq 'result (hasheq 'tools (list (hasheq 'name "tool1") (hasheq 'name "tool2")))))
      (define tools (parse-mcp-tools-list-response resp))
      (check-equal? (length tools) 2))

    (test-case "parse-mcp-tools-list-response returns empty on error"
      (define resp (hasheq 'error (hasheq 'code -1)))
      (check-equal? (parse-mcp-tools-list-response resp) '()))

    ;; ── Response parsing: parse-mcp-tools-call-response ──

    (test-case "parse-mcp-tools-call-response extracts content"
      (define resp (hasheq 'result (hasheq 'content (list (hasheq 'type "text" 'text "hi")))))
      (define content (parse-mcp-tools-call-response resp))
      (check-equal? (length content) 1))

    ;; ── Error response helpers ──

    (test-case "mcp-error-response? detects error"
      (check-true (mcp-error-response? (hasheq 'error (hasheq 'code -1))))
      (check-false (mcp-error-response? (hasheq 'result (hasheq)))))

    (test-case "mcp-error-code extracts code"
      (check-equal? (mcp-error-code (hasheq 'error (hasheq 'code -32601 'message "x"))) -32601))

    (test-case "mcp-error-message extracts message"
      (check-equal? (mcp-error-message (hasheq 'error (hasheq 'code -1 'message "oops"))) "oops"))

    ;; ── current-mcp-execute-fn parameter ──

    (test-case "current-mcp-execute-fn has default value"
      (check-true (procedure? (current-mcp-execute-fn))))

    (test-case "current-mcp-execute-fn can be parameterized"
      (define called? #f)
      (parameterize ([current-mcp-execute-fn (lambda (name args)
                                               (set! called? #t)
                                               (hasheq 'content '()))])
        ((current-mcp-execute-fn) "test" (hasheq))
        (check-true called?)))

    ;; ── Round-trip: build request → handle → parse response ──

    (test-case "initialize round-trip"
      (define reg (make-test-registry))
      (define exec-fn (make-mock-execute-fn))
      (define req (build-mcp-initialize 1))
      (define resp (handle-mcp-request req reg exec-fn))
      (define caps (parse-mcp-initialize-response resp))
      (check-true (hash? caps)))

    (test-case "tools/list round-trip"
      (define reg (make-test-registry))
      (define exec-fn (make-mock-execute-fn))
      (define req (build-mcp-tools-list 2))
      (define resp (handle-mcp-request req reg exec-fn))
      (define tools (parse-mcp-tools-list-response resp))
      (check-true (>= (length tools) 1)))

    (test-case "tools/call round-trip"
      (define reg (make-test-registry))
      (define results (make-hash))
      (hash-set! results "test-tool" (hasheq 'content (list (hasheq 'type "text" 'text "success"))))
      (define exec-fn (make-mock-execute-fn results))
      (define req (build-mcp-tools-call 3 "test-tool" (hasheq)))
      (define resp (handle-mcp-request req reg exec-fn))
      (define content (parse-mcp-tools-call-response resp))
      (check-equal? (length content) 1))))

(run-tests suite)
