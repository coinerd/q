#lang racket/base

;; tests/test-sse-shared.rkt — T1-2: Test shared SSE streaming function
;; STABILITY: evolving

(require rackunit
         rackunit/text-ui
         "../llm/stream.rkt")

;; ── Test Suite ──

(define suite
  (test-suite "SSE Shared Streaming (T1-2)"

    ;; Test 1: SSE parsing primitives exist
    (test-case "SSE parsing primitives are defined"
      (check-true (procedure? parse-sse-line))
      (check-true (procedure? parse-sse-data-line))
      (check-true (procedure? sse-done?)))

    ;; Test 2: parse-sse-line parses JSON data
    (test-case "parse-sse-line handles data lines"
      (define result (parse-sse-line "data: {\"text\":\"hello\"}"))
      (check-not-false result)
      (check-equal? (hash-ref result 'text #f) "hello"))

    ;; Test 3: sse-done? detects termination
    (test-case "sse-done? detects stream end"
      (check-true (sse-done? "[DONE]"))
      (check-false (sse-done? "{\"text\":\"more\"}")))

    ;; Test 4: W1 will add stream-chunks-from-port tests
    (test-case "W1 will test stream-chunks-from-port"
      (check-true #t))))

(run-tests suite)
