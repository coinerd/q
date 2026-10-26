#lang racket

;; @speed slow  ;; @suite security
;; @boundary unit

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
         racket/runtime-path
         json
         openssl
         racket/tcp
         (only-in "../util/security/cert-generator.rkt" generate-cert-set!)
         (only-in "../util/security/capability-tokens.rkt" sign-capability-token)
         (only-in "../util/security/tls-contexts.rkt" make-server-ssl-context make-client-ssl-context)
         (only-in "../util/message/mas-envelope.rkt" make-mas-envelope mas-envelope-trace-id)
         (only-in "../sandbox/executor-server.rkt"
                  start-executor-server!
                  shutdown-executor-server!
                  executor-server-running?)
         (only-in "../agent/distributed/remote-executor.rkt"
                  start-remote-executor!
                  shutdown-remote-executor!
                  execute-via-remote
                  remote-executor?)
         (only-in "../sandbox/ipc-protocol.rkt"
                  ipc-request?
                  ipc-request-working-dir
                  ipc-request-trusted-working-dir)
         (only-in "../tools/registry-defaults.rkt" register-default-tools!)
         (only-in "../tools/tool.rkt" make-tool-registry lookup-tool)
         "../sandbox/gateway-bridge.rkt"
         "helpers/fast-fixtures.rkt")  ; wait-until: deterministic test sync

(define-runtime-path worker-main-path "../sandbox/worker-main.rkt")

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
    ;; R91/BUG-0091: trust never crosses the remote seam; the local
    ;; bridge producer normalizes path cwd values to JSON strings.
    ;; ════════════════════════════════════════════════════════════

    (test-case "R91: envelope path builds trust-free requests (legacy wire unchanged)"
      (define env (make-test-envelope))
      (define req (envelope->ipc-request env))
      (check-true (ipc-request? req))
      (check-false (ipc-request-working-dir req))
      (check-false (ipc-request-trusted-working-dir req)))

    (test-case "R91: remote producer refuses cwd/trust-carrying arguments"
      (define port (find-free-port))
      (define server
        (start-executor-server! #:port port #:ssl-context server-ctx #:capability-secret TEST-SECRET))
      ;; R91-SLEEP: deterministic readiness synchronization replaces the raw
      ;; settle sleep. start-executor-server! binds its ssl-listen synchronously,
      ;; so instead of guessing with a fixed 0.2s wait we poll the observable
      ;; accept-loop predicate (wait-until: no live sleep, no fixed-delay race).
      (check-true (wait-until (lambda () (executor-server-running? server)) 5.0)
                  "executor server accept loop must be running before the client connects")
      (define executor
        (start-remote-executor! #:host "localhost"
                                #:port port
                                #:ssl-context client-ctx
                                #:capability-secret TEST-SECRET
                                #:agent-id TEST-AGENT
                                #:health-check-enabled #f))
      ;; Coordinator-owned worktree intent (the approval-visible scheduler
      ;; injection) is refused explicitly — never silently dispatched wd=#f.
      (define refused-owned
        (execute-via-remote executor "bash"
                            (hasheq 'command "echo hi" 'working-directory "/some/owned/worktree")
                            'shell-exec 5000))
      (check-equal? (ipc-response-status refused-owned) 'error)
      (check-true (string-contains? (or (ipc-response-error-message refused-owned) "")
                                    "refused"))
      (check-true (string-contains? (or (ipc-response-error-message refused-owned) "")
                                    "owned working directories"))
      ;; Misplaced trust fields are refused the same way.
      (define refused-trust
        (execute-via-remote executor "bash"
                            (hasheq 'command "echo hi" 'trusted-working-dir "/some/owned/worktree")
                            'shell-exec 5000))
      (check-equal? (ipc-response-status refused-trust) 'error)
      (check-true (string-contains? (or (ipc-response-error-message refused-trust) "")
                                    "refused"))
      ;; A policy refusal is not a transport failure: the executor stays usable.
      (define still-ok
        (execute-via-remote executor "bash" (hasheq 'command "echo clean") 'shell-exec 5000))
      (check-equal? (ipc-response-status still-ok) 'ok)
      (check-equal? (ipc-response-content still-ok) "clean")
      (shutdown-remote-executor! executor)
      (shutdown-executor-server! server))

    (test-case "R91: bridge producer normalizes path coordinator-wd to a JSON string on the wire"
      (define repo (make-temporary-file "r91-bridge-~a" 'directory))
      (define elsewhere (make-temporary-file "r91-bridge-spawn-~a" 'directory))
      (dynamic-wind
       void
       (lambda ()
         (define registry (make-tool-registry))
         (register-default-tools! registry)
         (check-not-false (lookup-tool registry "bash") "bash tool must be registered")
         (parameterize ([current-execution-plane-enabled #t]
                        [current-worker-args (list "-tm" (path->string worker-main-path))])
           ;; Spawn roots cover `elsewhere` only; the request carries the
           ;; coordinator wd as a PATH OBJECT and must still run inside `repo`.
           (parameterize ([current-directory elsewhere])
             (ensure-worker!))
           (define resp
             (execute-tool-via-worker "bash"
                                      (hasheq 'command "pwd")
                                      'shell-exec
                                      #:coordinator-wd repo))
           (check-equal? (ipc-response-status resp) 'ok (ipc-response-error-message resp))
           (check-equal? (string-trim (format "~a" (ipc-response-content resp)))
                         (path->string repo))))
       (lambda ()
         (shutdown-worker!)
         (delete-directory/files repo #:must-exist? #f)
         (delete-directory/files elsewhere #:must-exist? #f))))

    ;; ════════════════════════════════════════════════════════════
    ;; current-remote-executor parameter is thread-local
    ;; ════════════════════════════════════════════════════════════

    (test-case "current-remote-executor defaults to #f"
      ;; Outside parameterize, it should be #f (or whatever the outer scope set)
      ;; We just check it's falsy by default
      (check-false (current-remote-executor)))))

(run-tests suite 'verbose)
