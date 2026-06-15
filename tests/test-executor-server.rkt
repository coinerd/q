#lang racket

;; @speed slow  ;; @suite security

;; tests/test-executor-server.rkt — W2 (v0.99.12) Executor Node Server Tests
;;
;; Integration tests with real mTLS:
;;   - Server accepts valid mTLS connections
;;   - Valid capability token → dispatch succeeds
;;   - Missing capability token → rejected
;;   - Invalid/expired capability token → rejected
;;   - Oversized request → rejected
;;   - Graceful shutdown
;;   - worker-main.rkt tests still pass after extraction

(require rackunit
         rackunit/text-ui
         racket/file
         racket/string
         racket/port
         json
         openssl
         (only-in "../util/security/cert-generator.rkt" generate-cert-set!)
         (only-in "../util/security/capability-tokens.rkt" sign-capability-token CAPABILITY-TOKEN-TTL)
         (only-in "../util/security/tls-contexts.rkt" make-server-ssl-context make-client-ssl-context)
         (only-in "../sandbox/ipc-protocol.rkt"
                  ipc-response-status
                  ipc-response-request-id
                  ipc-response-content
                  ipc-response-error-message
                  ipc-request->jsexpr
                  ipc-request
                  IPC-SCHEMA-VERSION)
         "../sandbox/executor-server.rkt"
         "../sandbox/worker-dispatch.rkt")

;; ── Test Fixture ──

(define test-certs-dir (make-temporary-file "executor-server-test-~a" 'directory))
(define cert-paths (generate-cert-set! test-certs-dir))

(define server-ctx
  (make-server-ssl-context #:cert-path (hash-ref cert-paths 'server-cert)
                           #:key-path (hash-ref cert-paths 'server-key)
                           #:ca-path (hash-ref cert-paths 'ca-cert)))

(define client-ctx
  (make-client-ssl-context #:cert-path (hash-ref cert-paths 'client-cert)
                           #:key-path (hash-ref cert-paths 'client-key)
                           #:ca-path (hash-ref cert-paths 'ca-cert)))

(define TEST-SECRET "executor-server-test-secret")
(define TEST-AGENT "test-agent-w2")

;; ── Helper: find a free port ──

(define (find-free-port)
  (define listener (tcp-listen 0))
  (define-values (_host port __ ___) (tcp-addresses listener #t))
  (tcp-close listener)
  port)

;; ── Helper: connect as client and send a request ──

(define (connect-and-request port request-jsexpr)
  (define-values (in out) (ssl-connect "localhost" port client-ctx))
  (displayln (jsexpr->string request-jsexpr) out)
  (flush-output out)
  (define response-line (read-line in 'any))
  (close-input-port in)
  (close-output-port out)
  (and (not (eof-object? response-line)) (string->jsexpr (string-trim response-line))))

;; ── Helper: build a valid request with capability token ──

(define (make-test-request #:request-id [req-id "test-req-1"]
                           #:tool-name [tool "bash"]
                           #:capability [cap 'execute-tools]
                           #:capability-token [token #f]
                           #:arguments [args (hasheq 'command "echo hello")])
  (define enriched-args
    (if token
        (hash-set args 'capability-token token)
        args))
  (hasheq 'request-id
          req-id
          'tool-name
          tool
          'arguments
          enriched-args
          'timeout-ms
          30000
          'working-dir
          #f
          'capability
          (symbol->string cap)
          'schema-version
          IPC-SCHEMA-VERSION))

;; ── Tests ──

(define suite
  (test-suite "Executor Node Server (W2)"

    ;; ════════════════════════════════════════════════════════════
    ;; Server starts and accepts connections
    ;; ════════════════════════════════════════════════════════════

    (test-case "server starts and accepts mTLS connections"
      (define port (find-free-port))
      (define server
        (start-executor-server! #:port port #:ssl-context server-ctx #:capability-secret TEST-SECRET))
      (check-true (executor-server-running? server))
      ;; Give server a moment to start
      (sleep 0.2)
      ;; Connect with valid client cert and token
      (define token (sign-capability-token 'execute-tools TEST-AGENT TEST-SECRET))
      (define req (make-test-request #:capability-token token))
      (define resp (connect-and-request port req))
      (check-not-false resp "got a response")
      (when resp
        (check-equal? (hash-ref resp 'status) "ok"))
      (shutdown-executor-server! server))

    ;; ════════════════════════════════════════════════════════════
    ;; Valid request with valid token → ok response
    ;; ════════════════════════════════════════════════════════════

    (test-case "valid request with valid token returns ok"
      (define port (find-free-port))
      (define server
        (start-executor-server! #:port port #:ssl-context server-ctx #:capability-secret TEST-SECRET))
      (sleep 0.2)
      (define token (sign-capability-token 'execute-tools TEST-AGENT TEST-SECRET))
      (define req (make-test-request #:capability-token token))
      (define resp (connect-and-request port req))
      (check-not-false resp)
      (when resp
        (check-equal? (hash-ref resp 'status) "ok")
        (check-equal? (hash-ref resp 'request-id) "test-req-1"))
      (shutdown-executor-server! server))

    ;; ════════════════════════════════════════════════════════════
    ;; Missing capability token → rejected
    ;; ════════════════════════════════════════════════════════════

    (test-case "request without capability token is rejected"
      (define port (find-free-port))
      (define server
        (start-executor-server! #:port port #:ssl-context server-ctx #:capability-secret TEST-SECRET))
      (sleep 0.2)
      ;; No capability token in arguments
      (define req (make-test-request))
      (define resp (connect-and-request port req))
      (check-not-false resp)
      (when resp
        (check-equal? (hash-ref resp 'status) "error")
        (check-true (string-contains? (or (hash-ref resp 'error-message "") "") "capability token")
                    "error message mentions capability token"))
      (shutdown-executor-server! server))

    ;; ════════════════════════════════════════════════════════════
    ;; Invalid capability token → rejected
    ;; ════════════════════════════════════════════════════════════

    (test-case "request with invalid capability token is rejected"
      (define port (find-free-port))
      (define server
        (start-executor-server! #:port port #:ssl-context server-ctx #:capability-secret TEST-SECRET))
      (sleep 0.2)
      (define req (make-test-request #:capability-token "cap:bogus:agent:1:deadbeef"))
      (define resp (connect-and-request port req))
      (check-not-false resp)
      (when resp
        (check-equal? (hash-ref resp 'status) "error")
        (check-true (string-contains? (or (hash-ref resp 'error-message "") "") "invalid")
                    "error message mentions invalid token"))
      (shutdown-executor-server! server))

    ;; ════════════════════════════════════════════════════════════
    ;; Capability mismatch → rejected
    ;; ════════════════════════════════════════════════════════════

    (test-case "capability mismatch is rejected"
      (define port (find-free-port))
      (define server
        (start-executor-server! #:port port #:ssl-context server-ctx #:capability-secret TEST-SECRET))
      (sleep 0.2)
      ;; Token for write-files, but request says execute-tools
      (define token (sign-capability-token 'write-files TEST-AGENT TEST-SECRET))
      (define req (make-test-request #:capability-token token #:capability 'execute-tools))
      (define resp (connect-and-request port req))
      (check-not-false resp)
      (when resp
        (check-equal? (hash-ref resp 'status) "error")
        (check-true (string-contains? (or (hash-ref resp 'error-message "") "") "mismatch")
                    "error message mentions mismatch"))
      (shutdown-executor-server! server))

    ;; ════════════════════════════════════════════════════════════
    ;; Oversized request → rejected
    ;; ════════════════════════════════════════════════════════════

    (test-case "oversized request is rejected"
      (define port (find-free-port))
      (define server
        (start-executor-server! #:port port #:ssl-context server-ctx #:capability-secret TEST-SECRET))
      (sleep 0.2)
      ;; Create a request larger than 1MB
      (define huge-args (hasheq 'command (make-string 1100000 #\x)))
      (define token (sign-capability-token 'execute-tools TEST-AGENT TEST-SECRET))
      (define req (make-test-request #:capability-token token #:arguments huge-args))
      (define resp (connect-and-request port req))
      (check-not-false resp)
      (when resp
        (check-equal? (hash-ref resp 'status) "error")
        (check-true (string-contains? (or (hash-ref resp 'error-message "") "") "too large")
                    "error message mentions too large"))
      (shutdown-executor-server! server))

    ;; ════════════════════════════════════════════════════════════
    ;; Graceful shutdown
    ;; ════════════════════════════════════════════════════════════

    (test-case "server stops accepting after shutdown"
      (define port (find-free-port))
      (define server
        (start-executor-server! #:port port #:ssl-context server-ctx #:capability-secret TEST-SECRET))
      (sleep 0.2)
      (check-true (executor-server-running? server))
      (shutdown-executor-server! server 1000)
      (check-false (executor-server-running? server)))

    ;; ════════════════════════════════════════════════════════════
    ;; Multiple requests on same connection
    ;; ════════════════════════════════════════════════════════════

    (test-case "multiple requests on same connection"
      (define port (find-free-port))
      (define server
        (start-executor-server! #:port port #:ssl-context server-ctx #:capability-secret TEST-SECRET))
      (sleep 0.2)
      (define token (sign-capability-token 'execute-tools TEST-AGENT TEST-SECRET))
      (define-values (in out) (ssl-connect "localhost" port client-ctx))
      ;; Send 3 requests
      (for ([i (in-range 3)])
        (define req (make-test-request #:request-id (format "req-~a" i) #:capability-token token))
        (displayln (jsexpr->string req) out)
        (flush-output out)
        (define resp-line (read-line in 'any))
        (check-not-false (not (eof-object? resp-line)))
        (unless (eof-object? resp-line)
          (define resp (string->jsexpr (string-trim resp-line)))
          (check-equal? (hash-ref resp 'status) "ok")
          (check-equal? (hash-ref resp 'request-id) (format "req-~a" i))))
      (close-input-port in)
      (close-output-port out)
      (shutdown-executor-server! server))

    ;; ════════════════════════════════════════════════════════════
    ;; Worker dispatch extraction tests
    ;; ════════════════════════════════════════════════════════════

    (test-case "process-ipc-request dispatches correctly"
      (define req
        (ipc-request "w2-test"
                     "bash"
                     (hasheq 'command "echo w2-extraction-test")
                     30000
                     #f
                     'execute-tools
                     IPC-SCHEMA-VERSION))
      (define resp (process-ipc-request req))
      (check-equal? (ipc-response-status resp) 'ok)
      (check-equal? (ipc-response-request-id resp) "w2-test")
      (check-equal? (ipc-response-content resp) "w2-extraction-test"))

    (test-case "serialize-response produces valid JSON"
      (define req
        (ipc-request "serial-test"
                     "bash"
                     (hasheq 'command "echo test")
                     30000
                     #f
                     'execute-tools
                     IPC-SCHEMA-VERSION))
      (define resp (process-ipc-request req))
      (define json-str (serialize-response resp))
      (check-not-false (string->jsexpr json-str) "output is valid JSON"))))

(run-tests suite 'verbose)
