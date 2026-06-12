#lang racket

;; @speed fast
;; @suite default

;; BOUNDARY: integration

;; tests/test-stream-chunk-accumulation.rkt — AXIS3-F02: Stream chunk accumulation + SSE parsing
;;
;; Tests S11-F1: shared pure helper for stream chunk accumulation
;; Tests SSE parsing: parse-sse-line, parse-sse-lines, parse-sse-data-line

(require rackunit
         rackunit/text-ui
         (only-in "../agent/stream-reducer.rkt" accumulate-stream-chunks)
         (only-in "../llm/model.rkt" stream-chunk stream-chunk? make-stream-chunk)
         (only-in "../llm/stream.rkt"
                  parse-sse-line
                  parse-sse-lines
                  parse-sse-data-line
                  sse-done?
                  normalize-openai-chunk
                  normalize-openai-chunks))

(define acc-suite
  (test-suite "Stream chunk accumulation + SSE parsing (AXIS3-F02)"

    ;; ── accumulate-stream-chunks ──

    ;; -- empty chunks --
    (test-case "empty chunks returns empty text and no tool calls"
      (define result (accumulate-stream-chunks '()))
      (check-equal? (hash-ref result 'text) "")
      (check-equal? (hash-ref result 'tool-calls) '())
      (check-equal? (hash-ref result 'thinking) "")
      (check-equal? (hash-ref result 'usage) #f)
      (check-equal? (hash-ref result 'finish-reason) #f))

    ;; -- text accumulation --
    (test-case "accumulates text deltas in order"
      (define chunks
        (list (stream-chunk "Hello" #f #f #f #f #f)
              (stream-chunk " " #f #f #f #f #f)
              (stream-chunk "world" #f #f #f #f #f)))
      (define result (accumulate-stream-chunks chunks))
      (check-equal? (hash-ref result 'text) "Hello world"))

    ;; -- thinking accumulation --
    (test-case "accumulates thinking deltas"
      (define chunks
        (list (stream-chunk #f #f "Thinking..." #f #f #f) (stream-chunk #f #f " done" #f #f #f)))
      (define result (accumulate-stream-chunks chunks))
      (check-equal? (hash-ref result 'thinking) "Thinking... done"))

    ;; -- usage extraction --
    (test-case "extracts usage from first chunk with usage"
      (define usage (hasheq 'prompt_tokens 10 'completion_tokens 5))
      (define chunks
        (list (stream-chunk "hi" #f #f #f #f #f)
              (stream-chunk #f #f #f usage #f #f)
              (stream-chunk " there" #f #f #f #f #f)))
      (define result (accumulate-stream-chunks chunks))
      (check-equal? (hash-ref result 'usage) usage))

    ;; -- finish-reason extraction --
    (test-case "extracts finish-reason from chunk"
      (define chunks (list (stream-chunk "hi" #f #f #f #f "stop")))
      (define result (accumulate-stream-chunks chunks))
      (check-equal? (hash-ref result 'finish-reason) "stop"))

    ;; ── SSE Parsing (AXIS3-F02) ──

    ;; -- parse-sse-data-line --
    (test-case "parse-sse-data-line: extracts data after 'data: '"
      (check-equal? (parse-sse-data-line "data: {\"text\":\"hi\"}") "{\"text\":\"hi\"}"))

    (test-case "parse-sse-data-line: returns #f for non-data line"
      (check-false (parse-sse-data-line "event: message"))
      (check-false (parse-sse-data-line ": comment"))
      (check-false (parse-sse-data-line "")))

    ;; -- sse-done? --
    (test-case "sse-done?: recognizes [DONE] signal"
      (check-true (sse-done? "[DONE]"))
      (check-false (sse-done? "not done")))

    ;; -- parse-sse-line --
    (test-case "parse-sse-line: parses valid data line to hash"
      (define result (parse-sse-line "data: {\"choices\":[]}"))
      (check-true (hash? result))
      (check-true (hash-has-key? result 'choices)))

    (test-case "parse-sse-line: returns 'done for [DONE]"
      (check-equal? (parse-sse-line "data: [DONE]") 'done))

    (test-case "parse-sse-line: returns #f for non-data line"
      (check-false (parse-sse-line "event: chunk")))

    ;; -- parse-sse-lines --
    (test-case "parse-sse-lines: parses multi-line SSE text"
      (define sse-text "data: {\"id\":\"1\"}\n\ndata: {\"id\":\"2\"}\n\ndata: [DONE]\n\n")
      (define results (parse-sse-lines sse-text))
      (check-equal? (length results) 2)
      (check-equal? (hash-ref (car results) 'id) "1")
      (check-equal? (hash-ref (cadr results) 'id) "2"))

    (test-case "parse-sse-lines: empty string returns '()"
      (check-equal? (parse-sse-lines "") '()))

    (test-case "parse-sse-lines: skips malformed JSON lines"
      (define sse-text "data: {\"valid\":true}\ndata: {broken\ndata: {\"also_valid\":true}\n")
      (define results (parse-sse-lines sse-text))
      ;; Should have 2 valid results, skipping the malformed one
      (check-equal? (length results) 2))

    ;; -- normalize-openai-chunk --
    (test-case "normalize-openai-chunk: text delta"
      (define chunk-hash
        (hasheq 'choices (list (hasheq 'delta (hasheq 'content "hello") 'finish_reason #f))))
      (define result (normalize-openai-chunk chunk-hash))
      (check-true (stream-chunk? result)))

    (test-case "normalize-openai-chunk: empty hash returns stream-chunk with #f fields"
      (define result (normalize-openai-chunk (hasheq)))
      ;; Even with no choices, returns a stream-chunk with all #f fields
      (check-true (stream-chunk? result)))))

(run-tests acc-suite 'verbose)
