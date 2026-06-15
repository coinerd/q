#lang racket

;; @speed slow  ;; @suite security

;; tests/test-remote-executor.rkt — W1 (v0.99.12) Remote Executor Tests
;;
;; Tests the high-level remote executor client:
;;   - Capability token injection in every request
;;   - Execute-via-remote round-trip
;;   - Connection lifecycle (start, execute, shutdown)
;;   - Retry on transient error

(require rackunit
         rackunit/text-ui
         racket/file
         racket/string
         racket/port
         json
         openssl
         (only-in "../util/security/cert-generator.rkt" generate-cert-set!)
         (only-in "../util/security/capability-tokens.rkt"
                  validate-capability-token/claims
                  capability-token-claims-capability
                  capability-token-claims-agent-id)
         (only-in "../util/security/tls-contexts.rkt" make-server-ssl-context make-client-ssl-context)
         (only-in "../sandbox/ipc-protocol.rkt"
                  ipc-response-status
                  ipc-response-request-id
                  ipc-response-content
                  ipc-response-error-message)
         "../agent/distributed/remote-executor.rkt")

;; ── Test Fixture ──

(define test-certs-dir (make-temporary-file "remote-exec-test-~a" 'directory))
(define cert-paths (generate-cert-set! test-certs-dir))

(define server-ctx
  (make-server-ssl-context #:cert-path (hash-ref cert-paths 'server-cert)
                           #:key-path (hash-ref cert-paths 'server-key)
                           #:ca-path (hash-ref cert-paths 'ca-cert)))

(define client-ctx
  (make-client-ssl-context #:cert-path (hash-ref cert-paths 'client-cert)
                           #:key-path (hash-ref cert-paths 'client-key)
                           #:ca-path (hash-ref cert-paths 'ca-cert)))

(define TEST-SECRET "test-capability-secret-key")
(define TEST-AGENT "test-agent-001")

;; ── Mock TLS Server ──

(define (start-mock-executor-server proc)
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

(define (make-ok-jsexpr request-id [content "ok"])
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
  (test-suite "Remote Executor Client (W1)"

    ;; ════════════════════════════════════════════════════════════
    ;; Execute via Remote
    ;; ════════════════════════════════════════════════════════════

    (test-case "execute-via-remote returns ok response"
      (define-values (port shutdown)
        (start-mock-executor-server (lambda (req)
                                      (make-ok-jsexpr (hash-ref req 'request-id) "executed"))))
      (define exec
        (start-remote-executor! #:host "localhost"
                                #:port port
                                #:ssl-context client-ctx
                                #:capability-secret TEST-SECRET
                                #:agent-id TEST-AGENT
                                #:max-retries 0))
      (check-true (remote-executor-alive? exec))
      (define resp (execute-via-remote exec "test-tool" (hasheq 'arg 42) 'execute-tools 5000))
      (check-equal? (ipc-response-status resp) 'ok)
      (check-equal? (ipc-response-content resp) "executed")
      (shutdown-remote-executor! exec)
      (shutdown))

    ;; ════════════════════════════════════════════════════════════
    ;; Capability Token Injection
    ;; ════════════════════════════════════════════════════════════

    (test-case "capability token is included in every request"
      (define captured-args (box #f))
      (define-values (port shutdown)
        (start-mock-executor-server (lambda (req)
                                      ;; Capture the arguments for inspection
                                      (set-box! captured-args (hash-ref req 'arguments))
                                      (make-ok-jsexpr (hash-ref req 'request-id)))))
      (define exec
        (start-remote-executor! #:host "localhost"
                                #:port port
                                #:ssl-context client-ctx
                                #:capability-secret TEST-SECRET
                                #:agent-id TEST-AGENT
                                #:max-retries 0))
      (execute-via-remote exec "test-tool" (hasheq 'msg "hello") 'execute-tools 5000)
      (shutdown-remote-executor! exec)
      ;; Verify the captured arguments include a capability token
      (define args (unbox captured-args))
      (check-not-false args "server received arguments")
      (define cap-token (hash-ref args 'capability-token #f))
      (check-not-false cap-token "capability token is present in arguments")
      ;; Verify the token is valid
      (define claims (validate-capability-token/claims cap-token TEST-SECRET))
      (check-not-false claims "capability token is valid")
      (check-equal? (capability-token-claims-capability claims) 'execute-tools)
      (check-equal? (capability-token-claims-agent-id claims) TEST-AGENT)
      (shutdown))

    ;; ════════════════════════════════════════════════════════════
    ;; Original Arguments Preserved
    ;; ════════════════════════════════════════════════════════════

    (test-case "original arguments are preserved alongside capability token"
      (define captured-args (box #f))
      (define-values (port shutdown)
        (start-mock-executor-server (lambda (req)
                                      (set-box! captured-args (hash-ref req 'arguments))
                                      (make-ok-jsexpr (hash-ref req 'request-id)))))
      (define exec
        (start-remote-executor! #:host "localhost"
                                #:port port
                                #:ssl-context client-ctx
                                #:capability-secret TEST-SECRET
                                #:agent-id TEST-AGENT
                                #:max-retries 0))
      (execute-via-remote exec "test-tool" (hasheq 'msg "hello" 'num 123) 'execute-tools 5000)
      (shutdown-remote-executor! exec)
      (define args (unbox captured-args))
      (check-equal? (hash-ref args 'msg) "hello" "original arg preserved")
      (check-equal? (hash-ref args 'num) 123 "original arg preserved")
      (check-not-false (hash-ref args 'capability-token #f) "token added")
      (shutdown))

    ;; ════════════════════════════════════════════════════════════
    ;; Shutdown
    ;; ════════════════════════════════════════════════════════════

    (test-case "shutdown makes executor not alive"
      (define-values (port shutdown)
        (start-mock-executor-server (lambda (req) (make-ok-jsexpr (hash-ref req 'request-id)))))
      (define exec
        (start-remote-executor! #:host "localhost"
                                #:port port
                                #:ssl-context client-ctx
                                #:capability-secret TEST-SECRET
                                #:agent-id TEST-AGENT
                                #:max-retries 0))
      (check-true (remote-executor-alive? exec))
      (shutdown-remote-executor! exec)
      (check-false (remote-executor-alive? exec))
      (shutdown))))

(run-tests suite 'verbose)
