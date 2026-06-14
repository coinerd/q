#lang racket/base

;; @speed fast
;; @suite default

;; tests/test-mas-envelope-json.rkt — JSON serialization regression tests
;; STABILITY: evolving
;;
;; H1 regression: envelope->hash must produce JSON-serializable output.
;; H4 regression: hash->envelope must roundtrip through JSON safely.

(require rackunit
         rackunit/text-ui
         json
         (only-in "../util/message/mas-envelope.rkt"
                  make-mas-envelope
                  envelope->hash
                  hash->envelope
                  mas-envelope?
                  mas-envelope-source-agent
                  mas-envelope-target-agent
                  mas-envelope-capability
                  mas-envelope-payload
                  mas-envelope-deadline
                  mas-envelope-trace-id))

(define suite
  (test-suite "MAS Envelope JSON Serialization"

    ;; ── H1: envelope->hash produces JSON-serializable output ──

    (test-case "envelope->hash produces JSON-serializable output"
      (define env (make-mas-envelope 'supervisor 'tool-gateway 'file-write (hasheq)))
      (define h (envelope->hash env))
      (check-not-false (jsexpr->string h)))

    (test-case "envelope->hash source-agent is a string"
      (define env (make-mas-envelope 'supervisor 'planner 'read-only "data"))
      (define h (envelope->hash env))
      (check-true (string? (hash-ref h 'source-agent))))

    (test-case "envelope->hash capability is a string"
      (define env (make-mas-envelope 'supervisor 'planner 'plan-write "data"))
      (define h (envelope->hash env))
      (check-true (string? (hash-ref h 'capability))))

    (test-case "envelope->hash risk-level is a string"
      (define env (make-mas-envelope 'supervisor 'planner 'read-only "data"))
      (define h (envelope->hash env))
      (check-true (string? (hash-ref h 'risk-level))))

    ;; ── H4: hash->envelope roundtrips through JSON ──

    (test-case "hash->envelope roundtrips through JSON"
      (define env (make-mas-envelope 'supervisor 'planner 'plan-write "data"))
      (define json-str (jsexpr->string (envelope->hash env)))
      (define restored (hash->envelope (string->jsexpr json-str)))
      (check-not-false restored)
      (check-true (mas-envelope? restored))
      (check-equal? (mas-envelope-source-agent restored) 'supervisor)
      (check-equal? (mas-envelope-target-agent restored) 'planner)
      (check-equal? (mas-envelope-capability restored) 'plan-write))

    (test-case "hash->envelope accepts symbol values directly"
      (define h
        (hasheq 'source-agent
                'supervisor
                'target-agent
                'verifier
                'capability
                'read-only
                'payload
                "test"))
      (define restored (hash->envelope h))
      (check-not-false restored)
      (check-equal? (mas-envelope-source-agent restored) 'supervisor)
      (check-equal? (mas-envelope-capability restored) 'read-only))

    ;; ── H8: deadline type invariant ──

    (test-case "H8: deadline is exact integer"
      (define env (make-mas-envelope 'supervisor 'planner 'read-only "data"))
      (check-true (exact? (mas-envelope-deadline env))))

    ;; ── H4: invalid capability returns #f ──

    (test-case "hash->envelope returns #f for invalid capability"
      (define h
        (hasheq 'source-agent 'supervisor 'target-agent 'verifier 'capability 'bogus 'payload "test"))
      (define restored (hash->envelope h))
      (check-false restored))))

(run-tests suite)
