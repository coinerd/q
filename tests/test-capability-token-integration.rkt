#lang racket

;; @speed fast  ;; @suite integration

;; tests/test-capability-token-integration.rkt — W6 (v0.99.9)
;;
;; Integration coverage for signed capability tokens across simulated agents.

(require rackunit
         rackunit/text-ui
         racket/string
         "../util/security/capability-tokens.rkt")

(define secret "shared-secret-key")
(define wrong-secret "wrong-secret-key")
(define issued-at 1000)

(define (token-agent-id token)
  (list-ref (string-split token ":") 2))

(define suite
  (test-suite "Capability Token Integration (v0.99.9 W6)"

    (test-case "agent can sign and validate a read-only capability token"
      (define token (sign-capability-token 'read-only "planner" secret #:timestamp issued-at))
      (check-equal? (validate-capability-token token secret #:now issued-at) 'read-only))

    (test-case "same secret validates token across simulated agent boundary"
      (define planner-token
        (sign-capability-token 'plan-write "planner-agent" secret #:timestamp issued-at))
      ;; Supervisor/verifier receives only the token and shared validation key.
      (define verifier-observed-capability
        (validate-capability-token planner-token secret #:now (+ issued-at 10)))
      (check-equal? verifier-observed-capability 'plan-write)
      (check-equal? (token-agent-id planner-token) "planner-agent"))

    (test-case "wrong shared secret rejects otherwise well-formed token"
      (define token (sign-capability-token 'shell-exec "executor" secret #:timestamp issued-at))
      (check-false (validate-capability-token token wrong-secret #:now issued-at)))

    (test-case "expired token is rejected after TTL"
      (define token (sign-capability-token 'file-write "tool-gateway" secret #:timestamp issued-at))
      (check-false
       (validate-capability-token token secret #:now (+ issued-at CAPABILITY-TOKEN-TTL 1))))

    (test-case "token remains valid at inclusive TTL boundary"
      (define token (sign-capability-token 'git-write "tool-gateway" secret #:timestamp issued-at))
      (check-equal? (validate-capability-token token secret #:now (+ issued-at CAPABILITY-TOKEN-TTL))
                    'git-write))

    (test-case "future-issued token is rejected"
      (define token (sign-capability-token 'network "broker" secret #:timestamp (+ issued-at 60)))
      (check-false (validate-capability-token token secret #:now issued-at)))

    (test-case "tampering with capability invalidates signature"
      (define token (sign-capability-token 'read-only "browser-agent" secret #:timestamp issued-at))
      (define tampered (string-replace token "read-only" "browser"))
      (check-false (validate-capability-token tampered secret #:now issued-at)))

    (test-case "independent capabilities for same agent validate separately"
      (define read-token (sign-capability-token 'read-only "assistant" secret #:timestamp issued-at))
      (define write-token
        (sign-capability-token 'memory-write "assistant" secret #:timestamp issued-at))
      (check-equal? (validate-capability-token read-token secret #:now issued-at) 'read-only)
      (check-equal? (validate-capability-token write-token secret #:now issued-at) 'memory-write))))

(run-tests suite)
