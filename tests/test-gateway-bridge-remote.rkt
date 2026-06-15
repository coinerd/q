#lang racket

;; @speed slow  ;; @suite security

;; tests/test-gateway-bridge-remote.rkt — W3 (v0.99.12) Gateway Bridge Remote Tests
;;
;; Integration tests for the remote executor bridge in gateway-bridge.rkt:
;; - execute-via-remote-envelope with a real executor server
;; - current-remote-executor parameter wiring
;; - Fail-closed when no executor configured
;; - Fail-closed when executor not alive
;; - Full round-trip: envelope → remote → response hash

(require rackunit
         rackunit/text-ui
         racket/file
         racket/string
         racket/port
         json
         openssl
         racket/tcp
         (only-in "../util/security/cert-generator.rkt" generate-cert-set!)
         (only-in "../util/security/capability-tokens.rkt" sign-capability-token)
         (only-in "../util/security/tls-contexts.rkt" make-server-ssl-context make-client-ssl-context)
         (only-in "../util/message/mas-envelope.rkt" make-mas-envelope mas-envelope-trace-id)
         (only-in "../sandbox/executor-server.rkt" start-executor-server! shutdown-executor-server!)
         (only-in "../agent/distributed/remote-executor.rkt"
                  start-remote-executor!
                  shutdown-remote-executor!
                  remote-executor?)
         "../sandbox/gateway-bridge.rkt")

;; ── Test Fixture: generate certs once at module level ──

(define test-certs-dir (make-temporary-file "gw-bridge-remote-test-~a" 'directory))
(define cert-paths (generate-cert-set! test-certs-dir))

(define server-ctx
  (make-server-ssl-context #:cert-path (hash-ref cert-paths 'server-cert)
                           #:key-path (hash-ref cert-paths 'server-key)
                           #:ca-path (hash-ref cert-paths 'ca-cert)))

(define client-ctx
  (make-client-ssl-context #:cert-path (hash-ref cert-paths 'client-cert)
                           #:key-path (hash-ref cert-paths 'client-key)
                           #:ca-path (hash-ref cert-paths 'ca-cert)))

(define TEST-SECRET "gw-bridge-remote-test-secret")
(define TEST-AGENT "gw-bridge-test-agent")

;; ── Helper: find a free port ──

(define (find-free-port)
  (define listener (tcp-listen 0))
  (define-values (_host port __ ___) (tcp-addresses listener #t))
  (tcp-close listener)
  port)

;; ── Helper: make a test envelope ──

(define (make-test-envelope #:risk-level [risk 'high]
                            #:tool-name [tool "bash"]
                            #:arguments [args (hasheq 'command "echo bridge-test")])
  (make-mas-envelope 'supervisor
                     'tool-gateway
                     'shell-exec
                     (hasheq 'tool-name tool 'arguments args)
                     #:risk-level risk))

;; ── Tests ──

(define suite
  (test-suite "Gateway Bridge Remote Execution (W3 v0.99.12)"

    ;; ════════════════════════════════════════════════════════════
    ;; Fail-closed: no remote executor configured
    ;; ════════════════════════════════════════════════════════════

    (test-case "execute-via-remote-envelope fails closed when no executor"
      (define env (make-test-envelope))
      (parameterize ([current-remote-executor #f])
        (define result (execute-via-remote-envelope env))
        (check-equal? (hash-ref result 'status) 'error)
        (check-true (string-contains? (hash-ref result 'error-message "")
                                      "remote executor not configured"))))

    (test-case "execute-via-remote-envelope fails closed for non-envelope input"
      (parameterize ([current-remote-executor #f])
        (define result (execute-via-remote-envelope "not-an-envelope"))
        (check-equal? (hash-ref result 'status) 'error)
        (check-true (string-contains? (hash-ref result 'error-message "") "mas-envelope"))))

    ;; ════════════════════════════════════════════════════════════
    ;; Full round-trip: start server, connect executor, execute, verify
    ;; ════════════════════════════════════════════════════════════

    (test-case "full round-trip: envelope → remote executor → result hash"
      (define port (find-free-port))
      ;; Start executor server
      (define server
        (start-executor-server! #:port port #:ssl-context server-ctx #:capability-secret TEST-SECRET))
      (sleep 0.2)
      ;; Start remote executor client
      (define executor
        (start-remote-executor! #:host "localhost"
                                #:port port
                                #:ssl-context client-ctx
                                #:capability-secret TEST-SECRET
                                #:agent-id TEST-AGENT))
      ;; Execute via the bridge
      (define env (make-test-envelope #:arguments (hasheq 'command "echo bridge-roundtrip")))
      (parameterize ([current-remote-executor executor])
        (define result (execute-via-remote-envelope env))
        (check-equal? (hash-ref result 'status) 'ok)
        (check-equal? (hash-ref result 'result) "bridge-roundtrip"))
      ;; Cleanup
      (shutdown-remote-executor! executor)
      (shutdown-executor-server! server))

    ;; ════════════════════════════════════════════════════════════
    ;; Error handling: remote server unavailable
    ;; ════════════════════════════════════════════════════════════

    (test-case "error when remote server is not reachable"
      ;; Start and immediately stop a server to get a port
      (define port (find-free-port))
      ;; Try to connect to a port with no server
      (define executor
        (with-handlers ([exn:fail? (lambda (e) #f)])
          (start-remote-executor! #:host "localhost"
                                  #:port port
                                  #:ssl-context client-ctx
                                  #:capability-secret TEST-SECRET
                                  #:agent-id TEST-AGENT
                                  #:connect-timeout-ms 3000)))
      (when executor
        (define env (make-test-envelope))
        (parameterize ([current-remote-executor executor])
          (define result (execute-via-remote-envelope env))
          (check-equal? (hash-ref result 'status) 'error))
        (shutdown-remote-executor! executor)))

    ;; ════════════════════════════════════════════════════════════
    ;; current-remote-executor parameter is thread-local
    ;; ════════════════════════════════════════════════════════════

    (test-case "current-remote-executor defaults to #f"
      ;; Outside parameterize, it should be #f (or whatever the outer scope set)
      ;; We just check it's falsy by default
      (check-false (current-remote-executor)))))

(run-tests suite 'verbose)
