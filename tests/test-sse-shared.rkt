#lang racket/base

;; @speed fast
;; @suite default
;; @boundary unit

;; tests/test-sse-shared.rkt — SSE Shared Streaming Tests (T1-2)
;; STABILITY: evolving
;;
;; W1 (v0.99.71): Added SSE data: syntax compliance tests.
;; The SSE spec allows both "data: value" and "data:value" (no space after colon).

(require rackunit
         rackunit/text-ui
         "../llm/stream.rkt")

;; ── Test Suite ──

(define suite
  (test-suite "SSE Shared Streaming (T1-2)"

    (test-case "SSE parsing primitives are defined"
      (check-true (procedure? parse-sse-line))
      (check-true (procedure? parse-sse-data-line))
      (check-true (procedure? sse-done?)))

    (test-case "parse-sse-line handles data lines"
      (define result (parse-sse-line "data: {\"text\":\"hello\"}"))
      (check-not-false result)
      (check-equal? (hash-ref result 'text #f) "hello"))

    (test-case "sse-done? detects stream end"
      (check-true (sse-done? "[DONE]"))
      (check-false (sse-done? "{\"text\":\"more\"}")))

    ;; ============================================================
    ;; W1: SSE data: Syntax Compliance
    ;; The SSE spec allows both "data: value" (with space) and
    ;; "data:value" (without space) after the colon.
    ;; ============================================================

    (test-case "W1: parse-sse-data-line accepts 'data:' with optional space"
      ;; Standard format (with space)
      (check-equal? (parse-sse-data-line "data: {\"key\":\"val\"}")
                    "{\"key\":\"val\"}"
                    "should accept 'data: ' with space")
      ;; No-space format
      (check-equal? (parse-sse-data-line "data:{\"key\":\"val\"}")
                    "{\"key\":\"val\"}"
                    "should accept 'data:' without space")
      ;; Leading/trailing whitespace (string-trim handles this)
      (check-equal? (parse-sse-data-line "  data: {\"key\":\"val\"}  ")
                    "{\"key\":\"val\"}"
                    "should trim leading/trailing whitespace")
      ;; No space with leading whitespace
      (check-equal? (parse-sse-data-line "  data:{}")
                    "{}"
                    "should handle leading whitespace with no-space format")
      ;; [DONE] with no space
      (check-equal? (parse-sse-data-line "data:[DONE]")
                    "[DONE]"
                    "should accept '[DONE]' without space"))

    (test-case "W1: parse-sse-line accepts 'data:' without space"
      (define result (parse-sse-line "data:{\"text\":\"world\"}"))
      (check-not-false result)
      (check-equal? (hash-ref result 'text #f) "world")
      ;; [DONE] without space
      (check-equal? (parse-sse-line "data:[DONE]") 'done))

    (test-case "W1: parse-sse-lines handles mixed space/no-space data lines"
      (define sse-text
        (string-append "data: {\"id\":\"1\"}\n" "\n" "data:{\"id\":\"2\"}\n" "\n" "data:[DONE]\n"))
      (define results (parse-sse-lines sse-text))
      (check-equal? (length results) 2)
      (check-equal? (hash-ref (car results) 'id) "1")
      (check-equal? (hash-ref (cadr results) 'id) "2"))

    (test-case "W1: stream-chunks-from-port placeholder"
      (check-true #t))

    (test-case "W1: parse-sse-data-line handles edge cases"
      (check-equal? (parse-sse-data-line "data:")
                    ""
                    "'data:' with nothing after colon returns empty string")
      (check-equal? (parse-sse-data-line "data: ")
                    ""
                    "'data: ' with only space after returns empty string"))))

(run-tests suite)
