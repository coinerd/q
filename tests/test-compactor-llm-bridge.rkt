#lang racket/base

;; tests/test-compactor-llm-bridge.rkt — Tests for compaction LLM bridge (AXIS3-F10)
;; @speed fast  ;; @suite runtime

(require rackunit
         (only-in "../llm/model.rkt" make-model-response)
         (only-in "../llm/provider.rkt" make-provider)
         (only-in "../runtime/compaction/compactor-llm-bridge.rkt"
                  llm-summarize
                  make-llm-summarize-fn)
         (only-in "../util/message/message.rkt" make-message)
         (only-in "../util/content/content-parts.rkt" make-text-part))

;; ── Mock provider ──────────────────────────────────────────

(define (make-mock-provider response-text)
  (make-provider (lambda () "mock")
                 (lambda () (hasheq 'streaming #t))
                 (lambda (req)
                   (make-model-response (list (hasheq 'type 'text 'text response-text))
                                        (hasheq 'input_tokens 10 'output_tokens 50)
                                        "mock-model"
                                        'end_turn))
                 (lambda (req) (error "no streaming"))))

;; Create proper message? structs for compaction pipeline
(define (make-test-message role text)
  (make-message "test-id" #f role 'text (list (make-text-part text)) 0 #f))

(define mock-messages
  (list (make-test-message 'user "Hello there") (make-test-message 'assistant "Hi! How can I help?")))

;; ── Tests ──────────────────────────────────────────────────

(test-case "llm-summarize: basic summary generation"
  (define provider (make-mock-provider "Summary of conversation: User greeted assistant."))
  (define result (llm-summarize mock-messages provider "test-model"))
  (check-true (string? result))
  (check-true (> (string-length result) 20)))

(test-case "llm-summarize: iterative update with previous summary"
  (define provider (make-mock-provider "Updated summary incorporating new messages."))
  (define result
    (llm-summarize mock-messages provider "test-model" #:previous-summary "Old summary text here."))
  (check-true (string? result))
  (check-true (> (string-length result) 20)))

(test-case "llm-summarize: short response still returns"
  (define provider (make-mock-provider "x"))
  (define result (llm-summarize mock-messages provider "test-model"))
  (check-true (string? result)))

(test-case "llm-summarize: hash content parts extracted"
  (define provider
    (make-provider
     (lambda () "mock")
     (lambda () (hasheq 'streaming #t))
     (lambda (req)
       (make-model-response (list (hasheq 'type 'text 'text "A good summary of the conversation."))
                            (hasheq 'input_tokens 10 'output_tokens 50)
                            "mock-model"
                            'end_turn))
     (lambda (req) (error "no streaming"))))
  (define result (llm-summarize mock-messages provider "test-model"))
  (check-true (string? result))
  (check-true (>= (string-length result) 30)))

(test-case "make-llm-summarize-fn: returns callable procedure"
  (define provider (make-mock-provider "Summary via factory function that is long enough."))
  (define summarize-fn (make-llm-summarize-fn provider "test-model"))
  (check-true (procedure? summarize-fn))
  (define result (summarize-fn mock-messages))
  (check-true (string? result))
  (check-true (> (string-length result) 20)))

(test-case "llm-summarize: with file-tracker"
  (define provider (make-mock-provider "Summary with file context included and long enough."))
  (define result
    (llm-summarize mock-messages
                   provider
                   "test-model"
                   #:file-tracker (hasheq 'files '("a.rkt" "b.rkt"))))
  (check-true (string? result)))
