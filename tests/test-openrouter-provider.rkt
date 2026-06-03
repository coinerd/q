#lang racket

;;; tests/test-openrouter-provider.rkt — OpenRouter provider wrapper tests

(require rackunit
         rackunit/text-ui
         "../llm/model.rkt"
         "../llm/openrouter.rkt"
         "../llm/openai-compatible.rkt"
         "../llm/provider.rkt"
         "../runtime/provider/provider-factory.rkt")

(define sample-request (make-model-request (list (hash 'role "user" 'content "Hello")) #f (hash)))

(define openrouter-provider-tests
  (test-suite "openrouter provider"

    (test-case "make-openrouter-provider returns named provider"
      (define p (make-openrouter-provider (hash 'api-key "sk-or-test")))
      (check-true (provider? p))
      (check-equal? (provider-name p) "openrouter"))

    (test-case "provider capabilities include streaming and static model list"
      (define p (make-openrouter-provider (hash 'api-key "sk-or-test")))
      (define caps (provider-capabilities p))
      (check-true (hash-ref caps 'streaming #f))
      (check-false (hash-ref caps 'token-counting #t))
      (check-true (list? (hash-ref caps 'models #f)))
      (check-not-false (member openrouter-default-model
                               (map (lambda (m) (hash-ref m 'id)) openrouter-models))))

    (test-case "hash->openrouter-config supplies OpenRouter defaults"
      (define cfg (hash->openrouter-config (hash 'api-key "sk-or-test")))
      (check-equal? (openrouter-config-base-url cfg) openrouter-default-base-url)
      (check-equal? (openrouter-config-model cfg) openrouter-default-model)
      (check-equal? (openrouter-config-api-key cfg) "sk-or-test"))

    (test-case "openrouter->openai-config preserves request settings"
      (define cfg
        (hash->openrouter-config (hash 'api-key
                                       "sk-or-test"
                                       'model
                                       "anthropic/claude-3.5-sonnet"
                                       'max-tokens
                                       1024
                                       'temperature
                                       0.2)))
      (define openai-cfg (openrouter->openai-config cfg))
      (check-equal? (openai-config-base-url openai-cfg) openrouter-default-base-url)
      (check-equal? (openai-config-model openai-cfg) "anthropic/claude-3.5-sonnet")
      (check-equal? (openai-config-max-tokens openai-cfg) 1024)
      (check-equal? (openai-config-temperature openai-cfg) 0.2))

    (test-case "provider factory dispatches openrouter by provider name"
      (define p (create-provider-for-name "openrouter" #f "sk-or-test" "openai/gpt-4o-mini"))
      (check-true (provider? p))
      (check-equal? (provider-name p) "openrouter"))

    (test-case "request body uses OpenAI-compatible chat-completions shape"
      (define body (openrouter-build-request-body sample-request #:stream? #t))
      (check-equal? (hash-ref body 'messages) (model-request-messages sample-request))
      (check-true (hash-ref body 'stream))
      (check-equal? (hash-ref body 'model) openrouter-default-model))))

(module+ main
  (run-tests openrouter-provider-tests))
