#lang racket

;; @speed slow  ;; @suite security

;; tests/test-remote-ipc.rkt — W1 (v0.99.12) Remote IPC Client Tests
;;
;; Tests the async TLS client against a mock TLS server that:
;;   - Accepts mTLS connections
;;   - Reads ipc-request JSON lines
;;   - Echoes back ipc-response JSON lines
;;   - Can simulate connection drop and slow responses

(require rackunit
         rackunit/text-ui
         racket/file
         racket/string
         racket/port
         racket/tcp
         racket/match
         json
         openssl
         (only-in "../util/security/cert-generator.rkt" generate-cert-set!)
         (only-in "../util/security/tls-contexts.rkt" make-server-ssl-context make-client-ssl-context)
         (only-in "../sandbox/ipc-protocol.rkt"
                  ipc-request
                  ipc-response
                  ipc-response-request-id
                  ipc-response-status
                  ipc-response-content
                  ipc-response-error-message
                  ipc-request->jsexpr
                  jsexpr->ipc-response
                  make-error-response
                  make-timeout-response)
         "../agent/distributed/remote-ipc.rkt")

;; ── Test Fixture: Generate certs and find an open port ──

(define test-certs-dir (make-temporary-file "remote-ipc-test-~a" 'directory))
(define cert-paths (generate-cert-set! test-certs-dir))

(define server-ctx
  (make-server-ssl-context #:cert-path (hash-ref cert-paths 'server-cert)
                           #:key-path (hash-ref cert-paths 'server-key)
                           #:ca-path (hash-ref cert-paths 'ca-cert)))

(define client-ctx
  (make-client-ssl-context #:cert-path (hash-ref cert-paths 'client-cert)
                           #:key-path (hash-ref cert-paths 'client-key)
                           #:ca-path (hash-ref cert-paths 'ca-cert)))

;; Find an available TCP port
(define (find-open-port)
  (define listener (tcp-listen 0))
  (define-values (_ port __ ___) (tcp-addresses listener #t))
  (tcp-close listener)
  port)

;; ── Mock TLS Server Helpers ──

;; Start a simple echo server that reads a JSON request and sends back a response.
;; proc: (jsexpr -> jsexpr) — transforms the request jsexpr into a response jsexpr
;; Returns (values listener-port shutdown-thunk)
(define (start-mock-server proc [delay-ms 0])
  (define server-listener (ssl-listen 0 5 #t #f 'tls12))
  (ssl-load-certificate-chain! server-listener (path->string (hash-ref cert-paths 'server-cert)))
  (ssl-load-private-key! server-listener (path->string (hash-ref cert-paths 'server-key)) #f #f)
  (ssl-load-verify-root-certificates! server-listener (path->string (hash-ref cert-paths 'ca-cert)))
  (ssl-set-verify! server-listener #t)
  (define-values (_host port _a _b) (ssl-addresses server-listener #t))
  (define server-thread
    (thread (lambda ()
              (let loop ()
                (define-values (in out) (ssl-accept server-listener))
                (thread (lambda ()
                          (let req-loop ()
                            (define line (read-line in 'any))
                            (unless (eof-object? line)
                              (define trimmed (string-trim line))
                              (unless (string=? trimmed "")
                                (when (> delay-ms 0)
                                  (sleep (/ delay-ms 1000.0)))
                                (define req-jsexpr (with-input-from-string trimmed read-json))
                                (define resp-jsexpr (proc req-jsexpr))
                                (displayln (jsexpr->string resp-jsexpr) out)
                                (flush-output out))
                              (req-loop)))
                          (close-input-port in)
                          (close-output-port out)))
                (loop)))))
  (values port
          (lambda ()
            (kill-thread server-thread)
            (ssl-close server-listener))))

;; Build a simple ok response jsexpr for a given request-id
(define (make-ok-response-jsexpr request-id [content "ok"])
  (hasheq 'request-id
          request-id
          'status
          "ok"
          'content
          content
          'details
          (hasheq)
          'error-message
          #f
          'schema-version
          1))

;; ── Tests ──

(define suite
  (test-suite "Remote IPC Client (W1)"

    ;; ════════════════════════════════════════════════════════════
    ;; Basic Round-Trip
    ;; ════════════════════════════════════════════════════════════

    (test-case "normal request/response round-trip"
      (define-values (port shutdown)
        (start-mock-server (lambda (req)
                             (make-ok-response-jsexpr (hash-ref req 'request-id) "echo"))))
      (define conn (start-remote-connection! "localhost" port client-ctx 5000))
      (check-true (remote-connection-alive? conn))
      (define req-id (generate-remote-request-id))
      (define req (ipc-request req-id "echo" (hasheq 'msg "hello") 5000 #f 'execute-tools 1))
      (define resp (send-remote-request! conn req 5000))
      (check-equal? (ipc-response-request-id resp) req-id)
      (check-equal? (ipc-response-status resp) 'ok)
      (close-remote-connection! conn)
      (shutdown))

    ;; ════════════════════════════════════════════════════════════
    ;; Concurrent Requests (no cross-talk)
    ;; ════════════════════════════════════════════════════════════

    (test-case "concurrent requests get correct responses"
      (define-values (port shutdown)
        (start-mock-server (lambda (req)
                             (make-ok-response-jsexpr (hash-ref req 'request-id)
                                                      (hash-ref req 'request-id)))))
      (define conn (start-remote-connection! "localhost" port client-ctx 5000))
      ;; Send 5 requests concurrently in separate threads
      (define results (make-hash))
      (define threads
        (for/list ([i (in-range 5)])
          (thread (lambda ()
                    (define req-id (generate-remote-request-id))
                    (define req (ipc-request req-id "echo" (hasheq) 5000 #f 'execute-tools 1))
                    (define resp (send-remote-request! conn req 5000))
                    (hash-set! results req-id (ipc-response-content resp))))))
      (for-each sync threads)
      ;; Each request should get back its own request-id as content
      (for ([(req-id content) (in-hash results)])
        (check-equal? content req-id))
      (close-remote-connection! conn)
      (shutdown))

    ;; ════════════════════════════════════════════════════════════
    ;; Request Timeout
    ;; ════════════════════════════════════════════════════════════

    (test-case "request timeout returns timeout status"
      ;; Server that delays 2s before responding
      (define-values (port shutdown)
        (start-mock-server (lambda (req) (make-ok-response-jsexpr (hash-ref req 'request-id))) 2000))
      (define conn (start-remote-connection! "localhost" port client-ctx 5000))
      (define req-id (generate-remote-request-id))
      (define req (ipc-request req-id "echo" (hasheq) 500 #f 'execute-tools 1)) ; 500ms timeout
      (define resp (send-remote-request! conn req 500))
      (check-equal? (ipc-response-status resp) 'timeout)
      (close-remote-connection! conn)
      (shutdown))

    ;; ════════════════════════════════════════════════════════════
    ;; Connection Drop
    ;; ════════════════════════════════════════════════════════════

    (test-case "connection drop notifies pending requests"
      ;; Server that closes connection immediately without responding
      (define server-listener (ssl-listen 0 5 #t #f 'tls12))
      (ssl-load-certificate-chain! server-listener (path->string (hash-ref cert-paths 'server-cert)))
      (ssl-load-private-key! server-listener (path->string (hash-ref cert-paths 'server-key)) #f #f)
      (ssl-load-verify-root-certificates! server-listener
                                          (path->string (hash-ref cert-paths 'ca-cert)))
      (ssl-set-verify! server-listener #t)
      (define-values (_ port _a _b) (ssl-addresses server-listener #t))
      (define drop-thread
        (thread (lambda ()
                  (let loop ()
                    (define-values (in out) (ssl-accept server-listener))
                    ;; Read the request, then close without responding
                    (read-line in 'any)
                    (close-input-port in)
                    (close-output-port out)
                    (loop)))))
      (define conn (start-remote-connection! "localhost" port client-ctx 5000))
      (define req-id (generate-remote-request-id))
      (define req (ipc-request req-id "echo" (hasheq) 5000 #f 'execute-tools 1))
      (define resp (send-remote-request! conn req 5000))
      ;; Should get error response with connection error message
      (check-equal? (ipc-response-status resp) 'error)
      (check-true (string-contains? (or (ipc-response-error-message resp) "") "connection error"))
      (close-remote-connection! conn)
      (kill-thread drop-thread)
      (ssl-close server-listener))

    ;; ════════════════════════════════════════════════════════════
    ;; Connection Timeout (unreachable host)
    ;; ════════════════════════════════════════════════════════════

    (test-case "connection to unreachable host raises connection error"
      ;; Use a port that's almost certainly not listening
      (check-exn exn:fail:remote-connection?
                 (lambda () (start-remote-connection! "127.0.0.1" 39998 client-ctx 2000))))))

(run-tests suite 'verbose)
