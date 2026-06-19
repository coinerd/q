#lang racket/base

;; agent/stream-reducer.rkt — Pure stream chunk classification and accumulation
;;
;; Extracted from loop-stream.rkt (v0.53.2 decomposition).
;; Contains only pure functions with no I/O, event emission, or state mutation.
;; These can be tested without mocks or providers.

(require racket/contract
         racket/match
         "../llm/model.rkt"
         (only-in "../llm/stream.rkt" accumulate-tool-call-deltas))

;; ============================================================
;; Configurable chunk limit (v0.12.3 Wave 0.1)
;; ============================================================

(define MAX-STREAM-CHUNKS (make-parameter 10000))

(provide classify-chunk
         chunk-has-data?
         MAX-STREAM-CHUNKS
         (contract-out [accumulate-stream-chunks (-> list? hash?)]))

;; ============================================================
;; accumulate-stream-chunks : pure helper (S11-F1)
;; ============================================================

;; Given a list of stream-chunks, return accumulated:
;;   'text, 'tool-calls, 'thinking, 'usage, 'finish-reason
(define (accumulate-stream-chunks chunks)
  (define text-parts
    (for/list ([c (in-list chunks)]
               #:when (stream-chunk-delta-text c))
      (stream-chunk-delta-text c)))
  (define thinking-parts
    (for/list ([c (in-list chunks)]
               #:when (stream-chunk-delta-thinking c))
      (stream-chunk-delta-thinking c)))
  (define tool-calls (accumulate-tool-call-deltas chunks))
  (define usage
    (for/first ([c (in-list chunks)]
                #:when (stream-chunk-usage c))
      (stream-chunk-usage c)))
  (define finish-reason
    (for/first ([c (in-list chunks)]
                #:when (stream-chunk-finish-reason c))
      (stream-chunk-finish-reason c)))
  (hasheq 'text
          (apply string-append text-parts)
          'thinking
          (apply string-append thinking-parts)
          'tool-calls
          tool-calls
          'usage
          usage
          'finish-reason
          finish-reason))

;; ============================================================
;; Pure chunk classification (T2-6)
;; ============================================================

;; classify-chunk : stream-chunk -> (listof (cons symbol any))
;; Pure: returns a list of (type . data) pairs describing what the chunk contains.
;; Each pair is (cons type-symbol data) where:
;;   ('text-delta . string)          — text content to accumulate
;;   ('thinking-delta . string)      — thinking/reasoning content
;;   ('tool-call-delta . hash)       — tool call delta
;;   ('done . (hash 'usage _ 'finish-reason _)) — stream completion
;; Returns '() if chunk has no useful data.
(define (classify-chunk chunk)
  (append (let ([dt (stream-chunk-delta-text chunk)])
            (if dt
                (list (cons 'text-delta dt))
                '()))
          (let ([th (stream-chunk-delta-thinking chunk)])
            (if th
                (list (cons 'thinking-delta th))
                '()))
          (let ([tc (stream-chunk-delta-tool-call chunk)])
            (if tc
                (list (cons 'tool-call-delta tc))
                '()))
          (if (stream-chunk-done? chunk)
              (list (cons 'done
                          (hasheq 'usage
                                  (or (stream-chunk-usage chunk) (hasheq))
                                  'finish-reason
                                  (or (stream-chunk-finish-reason chunk) "unknown"))))
              '())))

;; chunk-has-data? : stream-chunk -> boolean
;; Pure: returns #t if chunk carries any useful payload.
(define (chunk-has-data? chunk)
  (pair? (classify-chunk chunk)))
