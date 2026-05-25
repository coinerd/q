#lang racket/base

;; llm/openrouter.rkt — OpenRouter provider wrapper
;;
;; OpenRouter exposes an OpenAI-compatible chat-completions API at
;; https://openrouter.ai/api/v1.  This module keeps q's provider name,
;; defaults, and model metadata explicit while delegating request/response
;; mechanics to llm/openai-compatible.rkt.

(require racket/contract
         "model.rkt"
         "openai-compatible.rkt"
         "provider.rkt")

(provide openrouter-default-base-url
         openrouter-default-model
         openrouter-models
         openrouter-config
         openrouter-config?
         openrouter-config-api-key
         openrouter-config-base-url
         openrouter-config-model
         openrouter-config-max-tokens
         openrouter-config-temperature
         (contract-out [hash->openrouter-config (-> hash? openrouter-config?)]
                       [openrouter->openai-config (-> openrouter-config? openai-config?)]
                       [openrouter-build-request-body
                        (->* (model-request?) (#:stream? boolean?) hash?)]
                       [make-openrouter-provider (-> (or/c hash? openrouter-config?) provider?)]))

(define openrouter-default-base-url "https://openrouter.ai/api/v1")
(define openrouter-default-model "openai/gpt-4o-mini")

;; Static starter set. OpenRouter's catalog is dynamic; these stable IDs provide
;; offline defaults for selection, tests, and registry bootstrap code.
(define openrouter-models
  (list (hasheq 'id
                "openai/gpt-4o-mini"
                'name
                "GPT-4o mini"
                'context-window
                128000
                'capabilities
                (hasheq 'chat #t 'streaming #t 'tools #t))
        (hasheq 'id
                "anthropic/claude-3.5-sonnet"
                'name
                "Claude 3.5 Sonnet"
                'context-window
                200000
                'capabilities
                (hasheq 'chat #t 'streaming #t 'tools #t))
        (hasheq 'id
                "google/gemini-2.0-flash-001"
                'name
                "Gemini 2.0 Flash"
                'context-window
                1048576
                'capabilities
                (hasheq 'chat #t 'streaming #t 'tools #t))))

(struct openrouter-config
        (api-key ; string
         base-url ; string
         model ; string
         max-tokens ; (or/c exact-positive-integer? #f)
         temperature) ; (or/c real? #f)
  #:transparent)

(define (hash->openrouter-config h)
  (openrouter-config (hash-ref h 'api-key "")
                     (hash-ref h 'base-url openrouter-default-base-url)
                     (hash-ref h 'model openrouter-default-model)
                     (hash-ref h 'max-tokens #f)
                     (hash-ref h 'temperature #f)))

(define (openrouter->openai-config cfg)
  (openai-config (openrouter-config-api-key cfg)
                 (openrouter-config-base-url cfg)
                 (openrouter-config-model cfg)
                 (openrouter-config-max-tokens cfg)
                 (openrouter-config-temperature cfg)))

(define (openrouter-build-request-body req #:stream? [stream? #f])
  (define settings (model-request-settings req))
  (define req-with-default
    (if (hash-has-key? settings 'model)
        req
        (make-model-request (model-request-messages req)
                            (model-request-tools req)
                            (hash-set settings 'model openrouter-default-model))))
  (openai-build-request-body req-with-default #:stream? stream?))

(define (make-openrouter-provider config)
  (define cfg
    (if (openrouter-config? config)
        config
        (hash->openrouter-config config)))
  (define delegate (make-openai-compatible-provider (openrouter->openai-config cfg)))
  (make-provider (lambda () "openrouter")
                 (lambda ()
                   (hasheq 'streaming
                           #t
                           'token-counting
                           #f
                           'models
                           openrouter-models
                           'base-url
                           (openrouter-config-base-url cfg)))
                 (lambda (req) (provider-send delegate req))
                 (lambda (req) (provider-stream delegate req))))
