#lang racket

;; @speed fast  ;; @suite runtime

;;; test-loop-stream-thinking-meta.rkt — v0.99.78 Bug B part 2 regression:
;;; build-stream-result MUST persist the accumulated reasoning_content into the
;;; assistant message's meta 'thinking so provider-transport can round-trip it
;;; as reasoning_content to DeepSeek thinking-mode APIs.
;;; (streaming-message-finalize already did this, but it is dead code — the real
;;; path is build-stream-result → build-final-stream-result, which previously
;;; hardcoded meta to (hasheq 'turnId ... 'model ...) with no 'thinking key.)

(require rackunit
         "../llm/stream.rkt"
         "../llm/provider.rkt"
         "../agent/loop-stream.rkt"
         "../agent/state.rkt"
         "../util/event/event-bus.rkt"
         "../util/message/message.rkt"
         "../util/loop-result.rkt")

(define (test-provider)
  (make-provider (lambda () "test-provider")
                 (lambda () (hash 'streaming #t 'token-counting #t))
                 (lambda (req) (hasheq))
                 (lambda (req) '())))

(define (thinking-raw text)
  (hash 'id
        "chatcmpl-think"
        'choices
        (list (hash 'delta (hash 'reasoning_content text) 'finish_reason 'null))))

(define (text-raw text)
  (hash 'id "chatcmpl-txt" 'choices (list (hash 'delta (hash 'content text) 'finish_reason 'null))))

(define (make-thinking-stream-data thinking text)
  (hasheq 'text
          text
          'tool-calls
          '()
          'thinking
          thinking
          'all-chunks
          (normalize-openai-chunks (list (thinking-raw thinking) (text-raw text)))
          'cancelled?
          #f
          'stream-blocked?
          #f))

(test-case "build-stream-result persists thinking in assistant message meta"
  (define st (make-loop-state "sess-think" "turn-think"))
  (define bus (make-event-bus))
  (define thinking "Let me reason step by step about the timeout design.")
  (define stream-data (make-thinking-stream-data thinking "The design is sound."))
  (define result
    (build-stream-result stream-data '() bus "sess-think" "turn-think" st #f (test-provider) #f))
  (define msgs (loop-result-messages result))
  (define assistant-msg
    (for/first ([m (in-list msgs)]
                #:when (eq? (message-role m) 'assistant))
      m))
  (check-true (message? assistant-msg) "assistant message present in loop-result")
  (check-equal? (hash-ref (message-meta-safe assistant-msg) 'thinking #f)
                thinking
                "thinking meta round-trips through build-stream-result"))
