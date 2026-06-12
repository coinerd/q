#lang racket/base

;; @speed fast
;; @suite default

;; tests/test-mas-envelope.rkt — Unit tests for MAS envelope and capability taxonomy
;; STABILITY: evolving

(require rackunit
         rackunit/text-ui
         (only-in "../agent/capability.rkt"
                  VALID-CAPABILITIES
                  valid-capability?
                  ROLE-CAPABILITIES
                  role-has-capability?
                  all-capabilities)
         (only-in "../util/message/mas-envelope.rkt"
                  mas-envelope
                  mas-envelope?
                  mas-envelope-message-id
                  mas-envelope-trace-id
                  mas-envelope-source-agent
                  mas-envelope-target-agent
                  mas-envelope-capability
                  mas-envelope-payload
                  mas-envelope-deadline
                  mas-envelope-risk-level
                  mas-envelope-schema-version
                  make-mas-envelope
                  envelope->hash
                  hash->envelope))

(define suite
  (test-suite "MAS Envelope + Capability Taxonomy"

    ;; ── Capability Taxonomy ──

    (test-case "valid-capability?: all 10 capabilities"
      (for ([cap (in-list VALID-CAPABILITIES)])
        (check-true (valid-capability? cap) (format "capability ~a should be valid" cap))))

    (test-case "valid-capability?: rejects invalid symbol"
      (check-false (valid-capability? 'invalid-cap))
      (check-false (valid-capability? 42))
      (check-false (valid-capability? "read-only")))

    (test-case "valid-capability?: rejects #f"
      (check-false (valid-capability? #f)))

    (test-case "role-has-capability?: supervisor has subagent"
      (check-true (role-has-capability? 'supervisor 'subagent)))

    (test-case "role-has-capability?: verifier does not have subagent"
      (check-false (role-has-capability? 'verifier 'subagent)))

    (test-case "role-has-capability?: all roles accept 'any"
      (for ([role (in-list '(supervisor planner verifier tool-gateway executor))])
        (check-true (role-has-capability? role 'any) (format "role ~a should accept 'any" role))))

    (test-case "role-has-capability?: verifier only has read-only"
      (check-true (role-has-capability? 'verifier 'read-only))
      (check-false (role-has-capability? 'verifier 'shell-exec))
      (check-false (role-has-capability? 'verifier 'file-write)))

    (test-case "all-capabilities: returns 9 non-any capabilities"
      (define caps (all-capabilities))
      (check-equal? (length caps) 9)
      (check-false (member 'any caps)))

    (test-case "ROLE-CAPABILITIES: all roles have at least one capability"
      (for ([(role caps) (in-hash ROLE-CAPABILITIES)])
        (check-true (pair? caps) (format "role ~a should have at least one capability" role))))

    ;; ── MAS Envelope Construction ──

    (test-case "make-mas-envelope: minimal construction"
      (define env (make-mas-envelope 'supervisor 'planner 'read-only "test payload"))
      (check-true (mas-envelope? env))
      (check-equal? (mas-envelope-source-agent env) 'supervisor)
      (check-equal? (mas-envelope-target-agent env) 'planner)
      (check-equal? (mas-envelope-capability env) 'read-only)
      (check-equal? (mas-envelope-payload env) "test payload"))

    (test-case "make-mas-envelope: auto-generates message-id and trace-id"
      (define env (make-mas-envelope 'supervisor 'planner 'read-only #f))
      (check-true (string? (mas-envelope-message-id env)))
      (check-true (string? (mas-envelope-trace-id env))))

    (test-case "make-mas-envelope: default deadline is ~5min from now"
      (define before (current-inexact-milliseconds))
      (define env (make-mas-envelope 'supervisor 'planner 'read-only #f))
      (define after (current-inexact-milliseconds))
      (check-true (>= (mas-envelope-deadline env) (+ before 290000)))
      (check-true (<= (mas-envelope-deadline env) (+ after 310000))))

    (test-case "make-mas-envelope: explicit deadline and risk-level"
      (define env
        (make-mas-envelope 'supervisor
                           'tool-gateway
                           'shell-exec
                           '()
                           #:deadline 9999999
                           #:risk-level 'high))
      (check-equal? (mas-envelope-deadline env) 9999999)
      (check-equal? (mas-envelope-risk-level env) 'high))

    (test-case "make-mas-envelope: default risk-level is 'low"
      (define env (make-mas-envelope 'supervisor 'planner 'read-only #f))
      (check-equal? (mas-envelope-risk-level env) 'low))

    (test-case "make-mas-envelope: rejects invalid capability"
      (check-exn exn:fail? (lambda () (make-mas-envelope 'supervisor 'planner 'bogus #f))))

    (test-case "make-mas-envelope: rejects invalid risk-level"
      (check-exn exn:fail?
                 (lambda ()
                   (make-mas-envelope 'supervisor 'planner 'read-only #f #:risk-level 'extreme))))

    ;; ── Serialization ──

    (test-case "envelope->hash: produces hash with all fields"
      (define env
        (make-mas-envelope 'supervisor
                           'planner
                           'read-only
                           "payload"
                           #:message-id "msg-1"
                           #:trace-id "trace-1"))
      (define h (envelope->hash env))
      (check-equal? (hash-ref h 'message-id) "msg-1")
      (check-equal? (hash-ref h 'trace-id) "trace-1")
      (check-equal? (hash-ref h 'source-agent) 'supervisor)
      (check-equal? (hash-ref h 'target-agent) 'planner)
      (check-equal? (hash-ref h 'capability) 'read-only)
      (check-equal? (hash-ref h 'payload) "payload"))

    (test-case "hash->envelope: roundtrip"
      (define env
        (make-mas-envelope 'supervisor
                           'planner
                           'read-only
                           #f
                           #:message-id "test-id"
                           #:trace-id "test-trace"))
      (define h (envelope->hash env))
      (define env2 (hash->envelope h))
      (check-true (mas-envelope? env2))
      (check-equal? (mas-envelope-message-id env2) "test-id")
      (check-equal? (mas-envelope-trace-id env2) "test-trace"))

    (test-case "hash->envelope: missing fields get defaults"
      (define env (hash->envelope (hasheq 'message-id "x")))
      (check-true (mas-envelope? env))
      (check-equal? (mas-envelope-message-id env) "x")
      (check-equal? (mas-envelope-capability env) 'any)
      (check-equal? (mas-envelope-risk-level env) 'low))

    (test-case "hash->envelope: returns #f for invalid input"
      (check-false (hash->envelope "not a hash")))

    (test-case "schema-version defaults to 1"
      (define env (make-mas-envelope 'supervisor 'planner 'read-only #f))
      (check-equal? (mas-envelope-schema-version env) 1))))

(run-tests suite)
