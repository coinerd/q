#lang racket/base

;; tests/test-stream-step.rkt — Tests for pure stream chunk processing
;;
;; v0.29.5 W1: Tests for the pure process-chunk function extracted from
;; agent/loop-stream.rkt.

(require rackunit
         (only-in "../llm/model.rkt" stream-chunk make-stream-chunk)
         (only-in "../agent/loop-stream.rkt"
                  stream-accumulator?
                  stream-accumulator-text-parts
                  stream-accumulator-tool-calls
                  stream-accumulator-thinking-parts
                  stream-accumulator-usage
                  stream-accumulator-chunk-count
                  stream-accumulator-finish-reason
                  stream-accumulator-done?
                  make-empty-accumulator
                  process-chunk))

;; Helper: make-stream-chunk uses positional args:
;;   (make-stream-chunk delta-text delta-tool-call usage done?
;;     #:delta-thinking ... #:finish-reason ...)

;; ============================================================
;; 1. Accumulator struct
;; ============================================================

(test-case "empty accumulator has zero count and empty parts"
  (define acc (make-empty-accumulator))
  (check-true (stream-accumulator? acc))
  (check-equal? (stream-accumulator-text-parts acc) '())
  (check-equal? (stream-accumulator-tool-calls acc) '())
  (check-equal? (stream-accumulator-thinking-parts acc) '())
  (check-false (stream-accumulator-usage acc))
  (check-equal? (stream-accumulator-chunk-count acc) 0)
  (check-false (stream-accumulator-finish-reason acc))
  (check-false (stream-accumulator-done? acc)))

;; ============================================================
;; 2. Text delta processing
;; ============================================================

(test-case "process-chunk appends text delta"
  (define acc (make-empty-accumulator))
  (define chunk (make-stream-chunk "hello" #f #f #f))
  (define result (process-chunk chunk acc))
  (check-equal? (stream-accumulator-text-parts result) '("hello")))

(test-case "process-chunk appends multiple text deltas"
  (define acc (make-empty-accumulator))
  (define r1 (process-chunk (make-stream-chunk "hello" #f #f #f) acc))
  (define r2 (process-chunk (make-stream-chunk " world" #f #f #f) r1))
  (check-equal? (stream-accumulator-text-parts r2) '("hello" " world")))

(test-case "process-chunk increments chunk-count for text delta"
  (define acc (make-empty-accumulator))
  (define result (process-chunk (make-stream-chunk "x" #f #f #f) acc))
  (check-equal? (stream-accumulator-chunk-count result) 1))

;; ============================================================
;; 3. Tool call delta processing
;; ============================================================

(test-case "process-chunk appends tool-call delta"
  (define acc (make-empty-accumulator))
  (define tc-delta (hasheq 'id "tc-1" 'name "bash" 'arguments "{}"))
  (define chunk (make-stream-chunk #f tc-delta #f #f))
  (define result (process-chunk chunk acc))
  (check-equal? (stream-accumulator-tool-calls result) (list tc-delta)))

(test-case "process-chunk increments chunk-count for tool-call delta"
  (define acc (make-empty-accumulator))
  (define result (process-chunk
                  (make-stream-chunk #f (hasheq 'id "tc-1") #f #f)
                  acc))
  (check-equal? (stream-accumulator-chunk-count result) 1))

;; ============================================================
;; 4. Finish/chunk-done processing
;; ============================================================

(test-case "process-chunk captures usage from done chunk"
  (define acc (make-empty-accumulator))
  (define usage (hasheq 'prompt_tokens 100 'completion_tokens 50))
  (define chunk (make-stream-chunk #f #f usage #t))
  (define result (process-chunk chunk acc))
  (check-equal? (stream-accumulator-usage result) usage)
  (check-true (stream-accumulator-done? result)))

(test-case "process-chunk captures finish-reason from done chunk"
  (define acc (make-empty-accumulator))
  (define chunk (make-stream-chunk #f #f #f #t #:finish-reason "stop"))
  (define result (process-chunk chunk acc))
  (check-equal? (stream-accumulator-finish-reason result) "stop"))

;; ============================================================
;; 5. Thinking delta processing
;; ============================================================

(test-case "process-chunk appends thinking delta"
  (define acc (make-empty-accumulator))
  (define chunk (make-stream-chunk #f #f #f #f #:delta-thinking "Let me think..."))
  (define result (process-chunk chunk acc))
  (check-equal? (stream-accumulator-thinking-parts result) '("Let me think...")))

;; ============================================================
;; 6. Malformed chunk handling
;; ============================================================

(test-case "process-chunk ignores #f chunk"
  (define acc (make-empty-accumulator))
  (define result (process-chunk #f acc))
  (check-equal? (stream-accumulator-chunk-count result) 0))

(test-case "process-chunk ignores non-stream-chunk"
  (define acc (make-empty-accumulator))
  (define result (process-chunk "not a chunk" acc))
  (check-equal? (stream-accumulator-chunk-count result) 0))

(test-case "process-chunk ignores hash chunk"
  (define acc (make-empty-accumulator))
  (define result (process-chunk (hasheq 'foo 'bar) acc))
  (check-equal? (stream-accumulator-chunk-count result) 0))

;; ============================================================
;; 7. Combined processing
;; ============================================================

(test-case "process-chunk handles chunk with multiple fields"
  (define acc (make-empty-accumulator))
  (define usage (hasheq 'total_tokens 42))
  (define chunk (make-stream-chunk "hi" #f usage #f))
  (define result (process-chunk chunk acc))
  (check-equal? (stream-accumulator-text-parts result) '("hi"))
  (check-equal? (stream-accumulator-usage result) usage)
  (check-equal? (stream-accumulator-chunk-count result) 1))

;; ============================================================
;; 8. Pure function contract
;; ============================================================

(test-case "process-chunk never mutates accumulator"
  (define acc (make-empty-accumulator))
  (define chunk (make-stream-chunk "test" #f #f #f))
  (define _ (process-chunk chunk acc))
  ;; Original accumulator unchanged
  (check-equal? (stream-accumulator-text-parts acc) '())
  (check-equal? (stream-accumulator-chunk-count acc) 0))

(test-case "process-chunk is deterministic"
  (define acc (make-empty-accumulator))
  (define chunk (make-stream-chunk "hello" #f #f #f))
  (define r1 (process-chunk chunk acc))
  (define r2 (process-chunk chunk acc))
  (check-equal? (stream-accumulator-text-parts r1) (stream-accumulator-text-parts r2))
  (check-equal? (stream-accumulator-chunk-count r1) (stream-accumulator-chunk-count r2)))
