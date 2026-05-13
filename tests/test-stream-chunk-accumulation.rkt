#lang racket

;; BOUNDARY: integration

;; tests/test-stream-chunk-accumulation.rkt -- W0: accumulate-stream-chunks tests
;;
;; Tests S11-F1: shared pure helper for stream chunk accumulation

(require rackunit
         rackunit/text-ui
         "../agent/loop-stream.rkt"
         "../llm/model.rkt")

(define acc-suite
  (test-suite "accumulate-stream-chunks"

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
        (list (stream-chunk #f #f "Thinking..." #f #f #f)
              (stream-chunk #f #f " done" #f #f #f)))
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
      (define chunks
        (list (stream-chunk "hi" #f #f #f #f "stop")))
      (define result (accumulate-stream-chunks chunks))
      (check-equal? (hash-ref result 'finish-reason) "stop"))))

(run-tests acc-suite 'verbose)
