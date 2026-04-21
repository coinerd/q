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

;; Regression test for v0.14.4 P0: mutable config hash must not be passed as provider-settings.
;; provider.rkt ensure-model-setting calls hash-set (immutable-only), which contracts on mutable.
(test-case "ensure-model-setting works with immutable settings hash"
  (define msgs (list (hasheq 'role "user" 'content "hello")))
  ;; Simulate what iteration.rkt should produce: immutable hash with only provider keys
  (define settings (hasheq 'max-tokens 32768))
  (define req (make-model-request msgs #f settings))
  ;; ensure-model-setting must not fail
  (define req-with-model (ensure-model-setting req "glm-5.1"))
  (check-equal? (hash-ref (model-request-settings req-with-model) 'model) "glm-5.1")
  (check-equal? (hash-ref (model-request-settings req-with-model) 'max-tokens) 32768))

(test-case "ensure-model-setting rejects mutable hash (regression guard)"
  (define msgs (list (hasheq 'role "user" 'content "hello")))
  ;; Mutable hash like the full runtime config — must NOT be passed as settings
  (define mutable-cfg (make-hash '((event-bus . fake) (model-name . "glm-5.1"))))
  (define req (make-model-request msgs #f mutable-cfg))
  ;; This should raise a contract violation
  (check-exn exn:fail:contract? (lambda () (ensure-model-setting req "glm-5.1"))))
