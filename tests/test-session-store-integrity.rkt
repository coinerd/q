#lang racket

;; BOUNDARY: integration

;; tests/test-session-store-integrity.rkt — W2-D5: Test scaffold for runtime/session-store-integrity.rkt
;; v0.29.13: Smoke tests for hash-chain creation and verification.

(require rackunit
         "../runtime/session-store-integrity.rkt")

(test-case "GENESIS-HASH is a non-empty string"
  (check-true (string? GENESIS-HASH))
  (check-true (> (string-length GENESIS-HASH) 0)))

(test-case "compute-event-hash returns a string"
  (define entry (hasheq 'type "test" 'data "hello"))
  (define h (compute-event-hash entry GENESIS-HASH))
  (check-true (string? h))
  (check-true (> (string-length h) 0)))

(test-case "compute-event-hash is deterministic"
  (define entry (hasheq 'type "test" 'data "hello"))
  (define h1 (compute-event-hash entry GENESIS-HASH))
  (define h2 (compute-event-hash entry GENESIS-HASH))
  (check-equal? h1 h2))

(test-case "compute-event-hash changes with different inputs"
  (define entry1 (hasheq 'type "test" 'data "hello"))
  (define entry2 (hasheq 'type "test" 'data "world"))
  (define h1 (compute-event-hash entry1 GENESIS-HASH))
  (define h2 (compute-event-hash entry2 GENESIS-HASH))
  (check-not-equal? h1 h2))

(test-case "canonical-jsexpr->string produces consistent output"
  (define entry (hasheq 'a 1 'b 2))
  (define s1 (canonical-jsexpr->string entry))
  (define s2 (canonical-jsexpr->string entry))
  (check-equal? s1 s2))
