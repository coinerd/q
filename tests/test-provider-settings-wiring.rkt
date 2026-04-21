#lang racket

;; tests/test-provider-settings-wiring.rkt — v0.14.4 Wave 2
;;
;; Verifies that provider settings (max-tokens) from config reach the API request body.

(require rackunit
         "../llm/model.rkt"
         "../llm/openai-compatible.rkt"
         "../llm/provider.rkt"
         "../agent/loop.rkt")

;; ============================================================
;; Test: max-tokens flows from config to request body
;; ============================================================

(test-case "make-model-request passes settings through"
  (define msgs (list (hasheq 'role "user" 'content "hello")))
  (define tools #f)
  (define settings (hasheq 'max-tokens 16384))
  (define req (make-model-request msgs tools settings))
  (check-equal? (hash-ref (model-request-settings req) 'max-tokens #f) 16384))

(test-case "make-model-request with #f settings → #f settings"
  (define msgs (list (hasheq 'role "user" 'content "hello")))
  (define req (make-model-request msgs #f #f))
  (check-false (model-request-settings req)))

(test-case "openai-build-request-body includes max_tokens from settings"
  (define msgs (list (hasheq 'role "user" 'content "hello")))
  (define settings (hasheq 'max-tokens 16384 'model "test-model"))
  (define req (make-model-request msgs #f settings))
  (define body (openai-build-request-body req))
  (check-equal? (hash-ref body 'max_tokens #f) 16384))

(test-case "openai-build-request-body without max-tokens → no max_tokens key"
  (define msgs (list (hasheq 'role "user" 'content "hello")))
  (define req (make-model-request msgs #f (hasheq 'model "test-model")))
  (define body (openai-build-request-body req))
  (check-false (hash-has-key? body 'max_tokens)))
