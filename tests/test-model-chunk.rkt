#lang racket

(require rackunit
         rackunit/text-ui
         "../llm/model.rkt")

(define chunk-tests
  (test-suite "stream-chunk thinking preservation"

    (test-case "make-stream-chunk 4-arg backward compat: delta-thinking is #f"
      (define c (make-stream-chunk "hello" #f #f #f))
      (check-equal? (stream-chunk-delta-text c) "hello")
      (check-false (stream-chunk-delta-thinking c)))

    (test-case "make-stream-chunk with #:delta-thinking preserves thinking"
      (define c (make-stream-chunk "hello" #f #f #f #:delta-thinking "hmm"))
      (check-equal? (stream-chunk-delta-text c) "hello")
      (check-equal? (stream-chunk-delta-thinking c) "hmm"))

    (test-case "stream-chunk direct constructor works with 6 fields"
      (define c (stream-chunk "hi" #f "thinking..." (hasheq 'total_tokens 10) #f #f))
      (check-equal? (stream-chunk-delta-thinking c) "thinking..."))))

(module+ main
  (run-tests chunk-tests))
(module+ test
  (run-tests chunk-tests))
