#lang racket

;;; test-memory-tool-turn-extraction-g1.rkt — W0 characterization for G1 tool-call extraction gap

(require rackunit
         "../llm/stream.rkt"
         "../llm/provider.rkt"
         "../agent/loop-stream.rkt"
         "../agent/state.rkt"
         "../agent/event-bus.rkt"
         "../runtime/memory/auto-extraction.rkt"
         "../runtime/memory/backends/memory-hash.rkt"
         "../runtime/memory/policy.rkt"
         "../runtime/memory/protocol.rkt"
         "../runtime/memory/service.rkt"
         "../runtime/memory/types.rkt"
         "../util/loop-result.rkt")

(define (test-provider)
  (make-provider (lambda () "test-provider")
                 (lambda () (hash 'streaming #t 'token-counting #t))
                 (lambda (req) (hasheq))
                 (lambda (req) '())))

(define (memory-count backend)
  (define result (gen:list-memory backend (memory-query "" #f #f #f #f #f 100 #t)))
  (if (memory-result? result)
      (length (memory-result-value result))
      (length result)))

(define tc-raw
  (hash
   'id
   "chatcmpl-tc"
   'choices
   (list (hash 'delta
               (hash 'tool_calls
                     (list (hash 'index 0 'id "call-1" 'function (hash 'name "read" 'arguments ""))))
               'finish_reason
               'null))))

(define (text-raw text)
  (hash 'id "chatcmpl-txt" 'choices (list (hash 'delta (hash 'content text) 'finish_reason 'null))))

(define (make-tool-call-stream-data text)
  (hasheq 'text
          text
          'tool-calls
          '()
          'thinking
          ""
          'all-chunks
          (normalize-openai-chunks (list tc-raw (text-raw text)))
          'cancelled?
          #f
          'stream-blocked?
          #f))

(define (make-text-only-stream-data text)
  (hasheq 'text
          text
          'tool-calls
          '()
          'thinking
          ""
          'all-chunks
          (normalize-openai-chunks (list (text-raw text)))
          'cancelled?
          #f
          'stream-blocked?
          #f))

(define fact-text "The project uses Racket for memory extraction configuration.")

(test-case "W0 G1 characterization: tool-call turn currently skips auto-extraction"
  (define backend (make-memory-hash-backend))
  (parameterize ([current-auto-extraction-enabled #t]
                 [current-auto-extraction-min-confidence 0.1]
                 [current-memory-backend backend]
                 [current-memory-policy default-memory-policy])
    (define st (make-loop-state "sess-g1" "turn-g1"))
    (define bus (make-event-bus))
    (define stream-data (make-tool-call-stream-data fact-text))
    (define result
      (build-stream-result stream-data '() bus "sess-g1" "turn-g1" st #f (test-provider) #f))
    (check-equal? (memory-count backend) 0 "Tool-call turn: currently no extraction occurs")
    (check-equal? (hash-ref (loop-result-metadata result) 'toolCallCount) 1)))

(test-case "W0 G1 characterization: no-tool-call turn still extracts once"
  (define backend (make-memory-hash-backend))
  (parameterize ([current-auto-extraction-enabled #t]
                 [current-auto-extraction-min-confidence 0.1]
                 [current-memory-backend backend]
                 [current-memory-policy default-memory-policy])
    (define st (make-loop-state "sess-g1" "turn-g1-no-tools"))
    (define bus (make-event-bus))
    (define stream-data (make-text-only-stream-data fact-text))
    (build-stream-result stream-data '() bus "sess-g1" "turn-g1-no-tools" st #f (test-provider) #f)
    (check-equal? (memory-count backend) 1 "No-tool-call turn: extraction occurs once")))
