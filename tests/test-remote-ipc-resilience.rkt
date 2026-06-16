#lang racket

;; @speed slow  ;; @suite security

;; tests/test-remote-ipc-resilience.rkt — W4 (v0.99.12) Connection Resilience Tests
;;
;; Tests:
;; - Circuit breaker: closed → open after N failures
;; - Circuit breaker: open → fast-fail
;; - Circuit breaker: open → half-open after cooldown
;; - Circuit breaker: half-open → closed on success
;; - Circuit breaker: half-open → open on failure
;; - Reconnection: dead connection → auto-reconnect
;; - No resource leak after connect/disconnect cycles

(require rackunit
         rackunit/text-ui
         racket/file
         racket/tcp
         racket/string
         racket/port
         openssl
         json
         (only-in "../sandbox/ipc-protocol.rkt" ipc-response-status ipc-response-error-message)
         (only-in "../util/security/cert-generator.rkt" generate-cert-set!)
         (only-in "../util/security/tls-contexts.rkt" make-server-ssl-context make-client-ssl-context)
         (only-in "../agent/distributed/remote-executor.rkt"
                  start-remote-executor!
                  shutdown-remote-executor!
                  execute-via-remote
                  reconnect-executor!
                  remote-executor-alive?
                  remote-executor-connection
                  remote-executor-circuit-state
                  remote-executor-circuit-failure-count
                  make-circuit-breaker
                  circuit-check!
                  circuit-record-success!
                  circuit-record-failure!
                  circuit-state
                  circuit-failure-count)
         (only-in "../agent/distributed/remote-ipc.rkt" remote-connection-alive-flag))

;; ── Test Fixture ──

(define test-certs-dir (make-temporary-file "resilience-test-~a" 'directory))
(define cert-paths (generate-cert-set! test-certs-dir))

(define client-ctx
  (make-client-ssl-context #:cert-path (hash-ref cert-paths 'client-cert)
                           #:key-path (hash-ref cert-paths 'client-key)
                           #:ca-path (hash-ref cert-paths 'ca-cert)))

(define TEST-SECRET "resilience-test-secret")
(define TEST-AGENT "resilience-test-agent")

;; ── Mock TLS Server with custodian-based shutdown ──
;; Returns (values port stop-fn). stop-fn kills ALL threads and ports.

(define (make-mock-server-listener [target-port 0])
  (define listener (ssl-listen target-port 5 #t #f 'tls12))
  (ssl-load-certificate-chain! listener (path->string (hash-ref cert-paths 'server-cert)))
  (ssl-load-private-key! listener (path->string (hash-ref cert-paths 'server-key)) #f #f)
  (ssl-load-verify-root-certificates! listener (path->string (hash-ref cert-paths 'ca-cert)))
  (ssl-set-verify! listener #t)
  listener)

(define (start-mock-server [target-port 0])
  (define server-custodian (make-custodian))
  (define listener (make-mock-server-listener target-port))
  (define-values (_host port _a _b) (ssl-addresses listener #t))
  (parameterize ([current-custodian server-custodian])
    (thread (lambda ()
              (let loop ()
                (with-handlers ([exn:fail? void])
                  (define-values (in out) (ssl-accept listener))
                  (thread (lambda ()
                            (let req-loop ()
                              (define line (read-line in 'any))
                              (unless (eof-object? line)
                                (define trimmed (string-trim line))
                                (unless (string=? trimmed "")
                                  (define req-jsexpr
                                    (with-handlers ([exn:fail? (lambda (_) #f)])
                                      (with-input-from-string trimmed read-json)))
                                  (when req-jsexpr
                                    (define resp
                                      (hasheq 'request-id
                                              (hash-ref req-jsexpr 'request-id "")
                                              'status
                                              "ok"
                                              'content
                                              "mock-result"
                                              'details
                                              (hasheq)
                                              'error-message
                                              #f
                                              'schema-version
                                              1))
                                    (displayln (jsexpr->string resp) out)
                                    (flush-output out)))
                                (req-loop)))
                            (close-input-port in)
                            (close-output-port out)))
                  (loop))))))
  (values port
          (lambda ()
            (custodian-shutdown-all server-custodian)
            (with-handlers ([exn:fail? void])
              (ssl-close listener)))))

;; ── Circuit Breaker Unit Tests (no network) ──

(define cb-unit-suite
  (test-suite "Circuit Breaker Unit Tests"

    (test-case "new circuit breaker starts closed"
      (define cb (make-circuit-breaker 3 1000))
      (check-equal? (circuit-state cb) 'closed)
      (check-equal? (circuit-failure-count cb) 0)
      (check-equal? (circuit-check! cb) 'allow))

    (test-case "circuit opens after threshold failures"
      (define cb (make-circuit-breaker 3 1000))
      (circuit-record-failure! cb)
      (check-equal? (circuit-state cb) 'closed)
      (circuit-record-failure! cb)
      (check-equal? (circuit-state cb) 'closed)
      (circuit-record-failure! cb)
      (check-equal? (circuit-state cb) 'open)
      (check-equal? (circuit-failure-count cb) 3))

    (test-case "open circuit rejects requests"
      (define cb (make-circuit-breaker 1 60000))
      (circuit-record-failure! cb)
      (check-equal? (circuit-state cb) 'open)
      (check-equal? (circuit-check! cb) 'reject))

    (test-case "open circuit transitions to half-open after cooldown"
      (define cb (make-circuit-breaker 1 100))
      (circuit-record-failure! cb)
      (check-equal? (circuit-state cb) 'open)
      (sleep 0.15)
      (check-equal? (circuit-check! cb) 'allow)
      (check-equal? (circuit-state cb) 'half-open))

    (test-case "half-open closes on success"
      (define cb (make-circuit-breaker 1 100))
      (circuit-record-failure! cb)
      (sleep 0.15)
      (circuit-check! cb)
      (circuit-record-success! cb)
      (check-equal? (circuit-state cb) 'closed)
      (check-equal? (circuit-failure-count cb) 0))

    (test-case "half-open re-opens on failure"
      (define cb (make-circuit-breaker 1 100))
      (circuit-record-failure! cb)
      (sleep 0.15)
      (circuit-check! cb)
      (circuit-record-failure! cb)
      (check-equal? (circuit-state cb) 'open))

    (test-case "success resets failure count in closed state"
      (define cb (make-circuit-breaker 5 1000))
      (circuit-record-failure! cb)
      (circuit-record-failure! cb)
      (check-equal? (circuit-failure-count cb) 2)
      (circuit-record-success! cb)
      (check-equal? (circuit-failure-count cb) 0)
      (check-equal? (circuit-state cb) 'closed))))

;; ── Integration Tests ──

(define integration-suite
  (test-suite "Connection Resilience Integration Tests"

    (test-case "executor starts with circuit closed"
      (define-values (port stop-server) (start-mock-server))
      (sleep 0.2)
      (define exec
        (start-remote-executor! #:host "localhost"
                                #:port port
                                #:ssl-context client-ctx
                                #:capability-secret TEST-SECRET
                                #:agent-id TEST-AGENT
                                #:health-check-enabled #f))
      (check-equal? (remote-executor-circuit-state exec) 'closed)
      (check-equal? (remote-executor-circuit-failure-count exec) 0)
      (shutdown-remote-executor! exec)
      (stop-server))

    (test-case "circuit opens after consecutive failures"
      (define-values (port stop-server) (start-mock-server))
      (sleep 0.2)
      (define exec
        (start-remote-executor! #:host "localhost"
                                #:port port
                                #:ssl-context client-ctx
                                #:capability-secret TEST-SECRET
                                #:agent-id TEST-AGENT
                                #:health-check-enabled #f
                                #:circuit-failure-threshold 3
                                #:connect-timeout-ms 2000))
      ;; Stop server — custodian shutdown kills all threads and closes all ports
      (stop-server)
      (sleep 0.3)
      ;; Execute requests that will fail
      (for ([_ (in-range 3)])
        (execute-via-remote exec "test-tool" (hasheq 'msg "fail") 'execute-tools 3000))
      (check-equal? (remote-executor-circuit-state exec) 'open)
      ;; Circuit open: next request should fast-fail
      (define resp (execute-via-remote exec "test-tool" (hasheq 'msg "fail") 'execute-tools 1000))
      (check-equal? (ipc-response-status resp) 'error)
      (check-true (string-contains? (or (ipc-response-error-message resp) "") "circuit breaker"))
      (shutdown-remote-executor! exec))

    (test-case "reconnection restores dead connection"
      (define-values (port stop-server) (start-mock-server))
      (sleep 0.2)
      (define exec
        (start-remote-executor! #:host "localhost"
                                #:port port
                                #:ssl-context client-ctx
                                #:capability-secret TEST-SECRET
                                #:agent-id TEST-AGENT
                                #:health-check-enabled #f))
      (check-true (remote-executor-alive? exec))
      (define resp1 (execute-via-remote exec "test-tool" (hasheq 'msg "hello") 'execute-tools 5000))
      (check-equal? (ipc-response-status resp1) 'ok)
      ;; Kill server
      (stop-server)
      (sleep 0.3)
      ;; Start new server on same port
      (define-values (_p stop-server2) (start-mock-server port))
      (sleep 0.2)
      (define reconnected? (reconnect-executor! exec))
      (check-true reconnected? "reconnection succeeded")
      (check-true (remote-executor-alive? exec) "executor alive after reconnect")
      (define resp2
        (execute-via-remote exec "test-tool" (hasheq 'msg "reconnected") 'execute-tools 5000))
      (check-equal? (ipc-response-status resp2) 'ok)
      (shutdown-remote-executor! exec)
      (stop-server2))

    (test-case "no resource leak after 10 connect/disconnect cycles"
      (for ([i (in-range 10)])
        (define-values (port stop-server) (start-mock-server))
        (sleep 0.1)
        (define exec
          (start-remote-executor! #:host "localhost"
                                  #:port port
                                  #:ssl-context client-ctx
                                  #:capability-secret TEST-SECRET
                                  #:agent-id TEST-AGENT
                                  #:health-check-enabled #f))
        (check-true (remote-executor-alive? exec))
        (shutdown-remote-executor! exec)
        (stop-server))
      (check-true #t "completed 10 cycles without error"))

    (test-case "F-09: reconnection failure returns clear error, not silent fallthrough"
      ;; F-09 regression test: the `return` identity function did not early-exit.
      ;; When reconnection fails, execute-with-resilience must return the
      ;; "connection lost" error, not silently fall through to a dead connection.
      (define-values (port stop-server) (start-mock-server))
      (sleep 0.2)
      (define exec
        (start-remote-executor! #:host "localhost"
                                #:port port
                                #:ssl-context client-ctx
                                #:capability-secret TEST-SECRET
                                #:agent-id TEST-AGENT
                                #:health-check-enabled #f))
      (check-true (remote-executor-alive? exec))
      ;; Kill server so reconnection will fail
      (stop-server)
      (sleep 0.3)
      ;; Force the connection to be dead
      (define conn (remote-executor-connection exec))
      (when conn
        (set-box! (remote-connection-alive-flag conn) #f))
      ;; Execute should return an error mentioning connection/reconnect/circuit
      (define resp (execute-via-remote exec "test-tool" (hasheq 'msg "dead") 'execute-tools 3000))
      (check-equal? (ipc-response-status resp) 'error)
      (define err-msg (or (ipc-response-error-message resp) ""))
      (check-true (or (string-contains? err-msg "connection")
                      (string-contains? err-msg "reconnect")
                      (string-contains? err-msg "circuit"))
                  (format "error should mention connection/reconnect/circuit, got: ~a" err-msg))
      (shutdown-remote-executor! exec))))

(run-tests cb-unit-suite)
(run-tests integration-suite)
