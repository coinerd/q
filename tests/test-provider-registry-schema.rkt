#lang racket

;;; tests/test-provider-registry-schema.rkt — Provider schema loading and registry tests
;;;
;;; #5244: provider registry tests — load, merge, resolve

(require rackunit
         rackunit/text-ui
         "../runtime/provider-registry.rkt"
         "../runtime/provider-schema.rkt"
         "../llm/provider.rkt")

(define provider-registry-schema-tests
  (test-suite "provider registry schema"

    (test-case "load-providers-schema returns non-empty list"
      (define schema (load-providers-schema))
      (check-true (list? schema))
      (check-true (> (length schema) 0)))

    (test-case "schema entries have symbol keys and alist values"
      (define schema (load-providers-schema))
      (for ([entry (in-list schema)])
        (check-true (symbol? (car entry)))
        (check-true (list? (cdr entry)))))

    (test-case "schema contains expected providers"
      (define names (list-builtin-provider-names))
      (check-not-false (member "openai" names))
      (check-not-false (member "anthropic" names))
      (check-not-false (member "gemini" names))
      (check-not-false (member "openrouter" names)))

    (test-case "load-builtin-providers! registers all providers"
      (define reg (make-provider-registry))
      (define count (load-builtin-providers! reg))
      (check-true (> count 0))
      ;; Check each expected provider is registered
      (for ([name (in-list '("openai" "anthropic" "gemini" "azure" "openrouter"))])
        (define pinfo (lookup-provider reg name))
        (check-not-false pinfo (format "provider ~a should be registered" name))))

    (test-case "registered providers have correct metadata"
      (define reg (make-provider-registry))
      (load-builtin-providers! reg)
      (define openai (lookup-provider reg "openai"))
      (check-not-false openai)
      (define cfg (provider-info-config openai))
      (check-equal? (hash-ref cfg 'base-url) "https://api.openai.com/v1")
      (check-equal? (hash-ref cfg 'default-model) "gpt-4o-mini")
      (check-equal? (hash-ref cfg 'auth-type) 'api-key)
      (check-equal? (hash-ref cfg 'factory) 'openai-compatible)
      (check-true (hash-ref cfg 'builtin)))

    (test-case "openrouter has correct base-url and factory"
      (define reg (make-provider-registry))
      (load-builtin-providers! reg)
      (define or-info (lookup-provider reg "openrouter"))
      (check-not-false or-info)
      (define cfg (provider-info-config or-info))
      (check-equal? (hash-ref cfg 'base-url) "https://openrouter.ai/api/v1")
      (check-equal? (hash-ref cfg 'factory) 'openrouter))

    (test-case "load-builtin-providers! registers models for providers"
      (define reg (make-provider-registry))
      (load-builtin-providers! reg)
      ;; OpenAI should have gpt-4o and gpt-4o-mini
      (define openai-models (list-models-for-provider reg "openai"))
      (check-true (> (length openai-models) 0))
      (define ids (map registered-model-id openai-models))
      (check-not-false (member "gpt-4o" ids))
      (check-not-false (member "gpt-4o-mini" ids)))

    (test-case "find-model locates registered models"
      (define reg (make-provider-registry))
      (load-builtin-providers! reg)
      (define m (find-model reg "gpt-4o"))
      (check-not-false m)
      (check-equal? (registered-model-id m) "gpt-4o")
      (check-equal? (registered-model-provider-name m) "openai"))

    (test-case "find-model locates openrouter models with slash IDs"
      (define reg (make-provider-registry))
      (load-builtin-providers! reg)
      (define m (find-model reg "openai/gpt-4o-mini"))
      (check-not-false m)
      (check-equal? (registered-model-id m) "openai/gpt-4o-mini")
      (check-equal? (registered-model-provider-name m) "openrouter"))

    (test-case "registered providers are placeholders until configured"
      (define reg (make-provider-registry))
      (load-builtin-providers! reg)
      (define openai (lookup-provider reg "openai"))
      (check-true (provider-is-placeholder? openai)))

    (test-case "re-registration with real provider upgrades from placeholder"
      (define reg (make-provider-registry))
      (load-builtin-providers! reg)
      ;; Simulate real provider registration (no placeholder)
      (define real-prov
        (make-provider (lambda () "openai")
                       (lambda () (hasheq 'streaming #t))
                       (lambda (req) (void))
                       (lambda (req) (void))))
      (register-provider! reg "openai" real-prov #:config (hasheq 'real #t))
      (define updated (lookup-provider reg "openai"))
      (check-false (provider-is-placeholder? updated))
      (check-true (hash-ref (provider-info-config updated) 'real)))

    (test-case "builtin-provider-config returns config hash"
      (define reg (make-provider-registry))
      (load-builtin-providers! reg)
      (define cfg (builtin-provider-config reg "anthropic"))
      (check-not-false cfg)
      (check-equal? (hash-ref cfg 'base-url) "https://api.anthropic.com/v1")
      (check-equal? (hash-ref cfg 'factory) 'anthropic))

    (test-case "builtin-provider-config returns #f for unknown provider"
      (define reg (make-provider-registry))
      (load-builtin-providers! reg)
      (check-false (builtin-provider-config reg "nonexistent")))))

(module+ main
  (run-tests provider-registry-schema-tests))
