#lang racket/base

;; @speed fast
;; @suite default

;; tests/test-stream-chunk-handlers.rkt — AXIS3-F01: Real chunk classification tests
;; STABILITY: evolving
;;
;; Replaces placeholder tests with contract-enforced tests for
;; classify-chunk, chunk-has-data?, and chunk handler dispatch.

(require rackunit
         rackunit/text-ui
         (only-in "../agent/stream-reducer.rkt"
                  classify-chunk
                  chunk-has-data?
                  accumulate-stream-chunks)
         (only-in "../llm/model.rkt" stream-chunk make-stream-chunk))

;; ── Test Suite ──

(define suite
  (test-suite "Stream Chunk Handlers (AXIS3-F01)"

    ;; ── classify-chunk: text ──
    (test-case "classify-chunk: text-delta"
      (define c (stream-chunk "hello" #f #f #f #f #f))
      (define classified (classify-chunk c))
      (check-equal? (length classified) 1)
      (check-equal? (car classified) (cons 'text-delta "hello")))

    (test-case "classify-chunk: empty text is still text-delta"
      (define c (stream-chunk "" #f #f #f #f #f))
      (define classified (classify-chunk c))
      (check-equal? (length classified) 1)
      (check-equal? (car classified) (cons 'text-delta "")))

    ;; ── classify-chunk: thinking ──
    (test-case "classify-chunk: thinking-delta"
      (define c (stream-chunk #f #f "reasoning..." #f #f #f))
      (define classified (classify-chunk c))
      (check-equal? (length classified) 1)
      (check-equal? (car classified) (cons 'thinking-delta "reasoning...")))

    ;; ── classify-chunk: tool-call ──
    (test-case "classify-chunk: tool-call-delta"
      (define tc (hasheq 'id "tc1" 'name "read_file" 'arguments "{}"))
      (define c (stream-chunk #f tc #f #f #f #f))
      (define classified (classify-chunk c))
      (check-equal? (length classified) 1)
      (check-equal? (caar classified) 'tool-call-delta))

    ;; ── classify-chunk: done ──
    (test-case "classify-chunk: done with usage and finish-reason"
      (define c (stream-chunk #f #f #f #f #t "stop"))
      (define classified (classify-chunk c))
      (check-not-false (assoc 'done classified))
      (define done-data (cdr (assoc 'done classified)))
      (check-equal? (hash-ref done-data 'finish-reason) "stop"))

    (test-case "classify-chunk: done with no usage returns empty hash"
      (define c (stream-chunk #f #f #f #f #t #f))
      (define classified (classify-chunk c))
      (define done-data (cdr (assoc 'done classified)))
      (check-equal? (hash-ref done-data 'usage) (hasheq)))

    ;; ── classify-chunk: empty chunk ──
    (test-case "classify-chunk: empty chunk returns '()"
      (define c (stream-chunk #f #f #f #f #f #f))
      (define classified (classify-chunk c))
      (check-equal? classified '()))

    ;; ── classify-chunk: multi-type chunk ──
    (test-case "classify-chunk: text + thinking combined"
      (define c (stream-chunk "hello" #f "thinking" #f #f #f))
      (define classified (classify-chunk c))
      (check-equal? (length classified) 2)
      (check-equal? (car classified) (cons 'text-delta "hello"))
      (check-equal? (cadr classified) (cons 'thinking-delta "thinking")))

    (test-case "classify-chunk: text + tool-call + done"
      (define tc (hasheq 'id "tc1" 'name "tool"))
      (define usage (hasheq 'prompt_tokens 10 'completion_tokens 5))
      (define c (stream-chunk "hi" tc #f usage #t "stop"))
      (define classified (classify-chunk c))
      (check-equal? (length classified) 3)
      (check-equal? (car classified) (cons 'text-delta "hi"))
      (check-equal? (cadr classified) (cons 'tool-call-delta tc))
      (check-not-false (assoc 'done classified)))

    ;; ── chunk-has-data? ──
    (test-case "chunk-has-data?: #t for text chunk"
      (check-true (chunk-has-data? (stream-chunk "x" #f #f #f #f #f))))

    (test-case "chunk-has-data?: #t for done chunk"
      (check-true (chunk-has-data? (stream-chunk #f #f #f #f #t #f))))

    (test-case "chunk-has-data?: #f for empty chunk"
      (check-false (chunk-has-data? (stream-chunk #f #f #f #f #f #f))))

    ;; ── accumulate-stream-chunks integration ──
    (test-case "accumulate: mixed text + thinking + done"
      (define chunks
        (list (stream-chunk "Hello" #f #f #f #f #f)
              (stream-chunk #f #f "Let me think" #f #f #f)
              (stream-chunk " world" #f #f #f #f #f)
              (stream-chunk #f #f #f (hasheq 'prompt_tokens 5) #t "stop")))
      (define result (accumulate-stream-chunks chunks))
      (check-equal? (hash-ref result 'text) "Hello world")
      (check-equal? (hash-ref result 'thinking) "Let me think")
      (check-equal? (hash-ref result 'finish-reason) "stop"))

    (test-case "accumulate: no chunks gives safe defaults"
      (define result (accumulate-stream-chunks '()))
      (check-equal? (hash-ref result 'text) "")
      (check-equal? (hash-ref result 'tool-calls) '())
      (check-equal? (hash-ref result 'thinking) "")
      (check-equal? (hash-ref result 'usage) #f)
      (check-equal? (hash-ref result 'finish-reason) #f))))

(run-tests suite)
