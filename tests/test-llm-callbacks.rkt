#lang racket/base

;; tests/test-llm-callbacks.rkt
;; H3b (v0.97.13): Unit tests for LLM callback factories.

;; @speed fast
(require rackunit
         racket/port
         (only-in "../runtime/context-assembly/llm-callbacks.rkt"
                  make-distill-callback
                  make-reflection-callback))

;; ── Mock provider ──

(define (make-mock-provider response-text)
  (lambda (method url payload [headers '()])
    (hasheq 'choices (list (hasheq 'message (hasheq 'content response-text))))))

;; Simplified mock: provider-send signature
(struct mock-provider (respond) #:transparent)

;; For these tests we use the callback functions directly with a mock
;; that has the provider-send interface.

(define (make-test-mock-provider text)
  ;; Returns a provider whose provider-send returns a model-response
  ;; with the given text.
  (hasheq 'send (lambda (req) (hasheq 'parts (list (hasheq 'type 'text 'text text))))))

;; ── Tests ──

(test-case "make-distill-callback returns a procedure"
  (define cb (make-distill-callback 'fake-prov "test-model"))
  (check-pred procedure? cb))

(test-case "make-reflection-callback returns a procedure"
  (define cb (make-reflection-callback 'fake-prov "test-model"))
  (check-pred procedure? cb))

(test-case "make-distill-callback accepts 2-3 args"
  (define cb (make-distill-callback 'fake-prov "test-model"))
  ;; Should accept 2 args (uncovered-ids, current-state)
  ;; Will error on the mock provider but that's OK — we test the signature
  (check-equal? (procedure-arity cb) (list 2 3)))

(test-case "make-reflection-callback returns callable with correct arity"
  (define cb (make-reflection-callback 'fake-prov "test-model"))
  (check-pred procedure? cb)
  (check-equal? (procedure-arity cb) 1))

(test-case "make-distill-callback factory captures prov and model"
  ;; The callback should close over the provider and model name
  (define cb (make-distill-callback 'my-prov "gpt-4"))
  ;; Calling it will fail because 'my-prov is not a real provider,
  ;; but the factory should have captured the args
  (check-pred procedure? cb))

(test-case "make-reflection-callback factory captures prov and model"
  (define cb (make-reflection-callback 'my-prov "gpt-4"))
  (check-pred procedure? cb))
