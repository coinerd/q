#lang racket

;; tests/test-provider-registry.rkt — tests for Dynamic Provider Registration (#700-#703)
;;
;; Covers:
;;   - #700: Centralized provider registry
;;   - #701: Dynamic model registration
;;   - #702: Model search across providers
;;   - #703: Parent feature integration

(require rackunit
         "../llm/provider.rkt"
         "../llm/model.rkt"
         "../runtime/provider-registry.rkt")

;; ============================================================
;; Helpers
;; ============================================================

(define (make-mock-response)
  (make-model-response
   (list (hasheq 'type "text" 'text "mock response"))
   (hasheq 'input-tokens 0 'output-tokens 0)
   "mock-model"
   'stop))

(define (make-test-provider #:name [name "test-provider"])
  (make-mock-provider (make-mock-response) #:name name))

(define (make-another-provider #:name [name "another-provider"])
  (make-mock-provider (make-mock-response) #:name name))

;; ============================================================
;; #700: Provider management
;; ============================================================

(test-case "make-provider-registry: creates empty registry"
  (define reg (make-provider-registry))
  (check-true (provider-registry? reg))
  (check-equal? (length (list-providers reg)) 0))

(test-case "register-provider!: adds provider to registry"
  (define reg (make-provider-registry))
  (define p (make-test-provider))
  (register-provider! reg "openai" p)
  (define found (lookup-provider reg "openai"))
  (check-true (provider-info? found))
  (check-equal? (provider-info-name found) "openai")
  (check-true (provider? (provider-info-provider found))))

(test-case "register-provider!: with config"
  (define reg (make-provider-registry))
  (define p (make-test-provider))
  (register-provider! reg "openai" p #:config (hasheq 'api-key "sk-test"))
  (define found (lookup-provider reg "openai"))
  (check-equal? (hash-ref (provider-info-config found) 'api-key) "sk-test"))

(test-case "register-provider!: merges config on re-registration"
  (define reg (make-provider-registry))
  (define p (make-test-provider))
  (register-provider! reg "openai" p #:config (hasheq 'api-key "key1" 'base-url "url1"))
  (register-provider! reg "openai" p #:config (hasheq 'api-key "key2"))
  (define found (lookup-provider reg "openai"))
  (check-equal? (hash-ref (provider-info-config found) 'api-key) "key2")
  (check-equal? (hash-ref (provider-info-config found) 'base-url) "url1"))

(test-case "unregister-provider!: removes provider"
  (define reg (make-provider-registry))
  (define p (make-test-provider))
  (register-provider! reg "openai" p)
  (unregister-provider! reg "openai")
  (check-false (lookup-provider reg "openai"))
  (check-equal? (length (list-providers reg)) 0))

(test-case "unregister-provider!: also removes models"
  (define reg (make-provider-registry))
  (define p (make-test-provider))
  (register-provider! reg "openai" p)
  (register-model! reg #:id "gpt-4o" #:name "GPT-4o" #:provider-name "openai")
  (unregister-provider! reg "openai")
  (check-false (find-model reg "gpt-4o")))

(test-case "lookup-provider: returns #f for unknown"
  (define reg (make-provider-registry))
  (check-false (lookup-provider reg "nonexistent")))

(test-case "list-providers: returns all providers"
  (define reg (make-provider-registry))
  (register-provider! reg "openai" (make-test-provider))
  (register-provider! reg "anthropic" (make-another-provider))
  (check-equal? (length (list-providers reg)) 2))

(test-case "provider-info: has registered-at timestamp"
  (define reg (make-provider-registry))
  (register-provider! reg "openai" (make-test-provider))
  (define found (lookup-provider reg "openai"))
  (check-true (exact-positive-integer? (provider-info-registered-at found))))

;; ============================================================
;; #701: Model registration
;; ============================================================

(test-case "register-model!: adds model to registry"
  (define reg (make-provider-registry))
  (register-provider! reg "openai" (make-test-provider))
  (register-model! reg #:id "gpt-4o" #:name "GPT-4o"
                       #:provider-name "openai"
                       #:context-window 128000
                       #:max-tokens 4096
                       #:capabilities (hasheq 'streaming #t))
  (define found (find-model reg "gpt-4o"))
  (check-true (registered-model? found))
  (check-equal? (registered-model-id found) "gpt-4o")
  (check-equal? (registered-model-name found) "GPT-4o")
  (check-equal? (registered-model-context-window found) 128000)
  (check-equal? (registered-model-max-tokens found) 4096))

(test-case "register-model!: without optional fields"
  (define reg (make-provider-registry))
  (register-provider! reg "openai" (make-test-provider))
  (register-model! reg #:id "gpt-3.5" #:name "GPT-3.5 Turbo"
                       #:provider-name "openai")
  (define found (find-model reg "gpt-3.5"))
  (check-true (registered-model? found))
  (check-false (registered-model-context-window found))
  (check-false (registered-model-max-tokens found)))

(test-case "unregister-model!: removes specific model"
  (define reg (make-provider-registry))
  (register-provider! reg "openai" (make-test-provider))
  (register-model! reg #:id "gpt-4o" #:name "GPT-4o" #:provider-name "openai")
  (register-model! reg #:id "gpt-3.5" #:name "GPT-3.5" #:provider-name "openai")
  (unregister-model! reg "openai" "gpt-4o")
  (check-false (find-model reg "gpt-4o"))
  (check-true (registered-model? (find-model reg "gpt-3.5"))))

(test-case "list-models-for-provider: returns models for specific provider"
  (define reg (make-provider-registry))
  (register-provider! reg "openai" (make-test-provider))
  (register-provider! reg "anthropic" (make-another-provider))
  (register-model! reg #:id "gpt-4o" #:name "GPT-4o" #:provider-name "openai")
  (register-model! reg #:id "claude-3" #:name "Claude 3" #:provider-name "anthropic")
  (define openai-models (list-models-for-provider reg "openai"))
  (check-equal? (length openai-models) 1)
  (check-equal? (registered-model-id (car openai-models)) "gpt-4o"))

(test-case "register-model!: overwrites existing model with same key"
  (define reg (make-provider-registry))
  (register-provider! reg "openai" (make-test-provider))
  (register-model! reg #:id "gpt-4o" #:name "GPT-4o"
                       #:provider-name "openai"
                       #:context-window 128000)
  (register-model! reg #:id "gpt-4o" #:name "GPT-4o Turbo"
                       #:provider-name "openai"
                       #:context-window 256000)
  (define found (find-model reg "gpt-4o"))
  (check-equal? (registered-model-name found) "GPT-4o Turbo")
  (check-equal? (registered-model-context-window found) 256000))

;; ============================================================
;; #702: Model search
;; ============================================================

(test-case "find-model: by exact ID"
  (define reg (make-provider-registry))
  (register-provider! reg "openai" (make-test-provider))
  (register-model! reg #:id "gpt-4o" #:name "GPT-4o" #:provider-name "openai")
  (check-true (registered-model? (find-model reg "gpt-4o"))))

(test-case "find-model: case-insensitive"
  (define reg (make-provider-registry))
  (register-provider! reg "openai" (make-test-provider))
  (register-model! reg #:id "gpt-4o" #:name "GPT-4o" #:provider-name "openai")
  (check-true (registered-model? (find-model reg "GPT-4O")))
  (check-true (registered-model? (find-model reg "gpt-4o"))))

(test-case "find-model: by name substring"
  (define reg (make-provider-registry))
  (register-provider! reg "openai" (make-test-provider))
  (register-model! reg #:id "gpt-4o" #:name "GPT-4o Omni" #:provider-name "openai")
  (check-true (registered-model? (find-model reg "Omni"))))

(test-case "find-model: by ID prefix"
  (define reg (make-provider-registry))
  (register-provider! reg "openai" (make-test-provider))
  (register-model! reg #:id "gpt-4o-mini" #:name "GPT-4o Mini" #:provider-name "openai")
  (check-true (registered-model? (find-model reg "gpt-4o"))))

(test-case "find-model: returns #f for no match"
  (define reg (make-provider-registry))
  (check-false (find-model reg "nonexistent")))

(test-case "find-models: returns all matches"
  (define reg (make-provider-registry))
  (register-provider! reg "openai" (make-test-provider))
  (register-model! reg #:id "gpt-4o" #:name "GPT-4o" #:provider-name "openai")
  (register-model! reg #:id "gpt-4o-mini" #:name "GPT-4o Mini" #:provider-name "openai")
  (register-model! reg #:id "gpt-3.5" #:name "GPT-3.5 Turbo" #:provider-name "openai")
  (define results (find-models reg "gpt-4"))
  (check-equal? (length results) 2))

(test-case "find-models: empty for no match"
  (define reg (make-provider-registry))
  (check-equal? (find-models reg "nonexistent") '()))

;; ============================================================
;; #703: Integration
;; ============================================================

(test-case "integration: full provider + model lifecycle"
  (define reg (make-provider-registry))

  ;; Register providers
  (register-provider! reg "openai" (make-test-provider)
                       #:config (hasheq 'base-url "https://api.openai.com"))
  (register-provider! reg "anthropic" (make-another-provider)
                       #:config (hasheq 'base-url "https://api.anthropic.com"))

  ;; Register models
  (register-model! reg #:id "gpt-4o" #:name "GPT-4o"
                       #:provider-name "openai"
                       #:context-window 128000
                       #:max-tokens 4096
                       #:capabilities (hasheq 'streaming #t 'vision #t))
  (register-model! reg #:id "gpt-3.5" #:name "GPT-3.5 Turbo"
                       #:provider-name "openai"
                       #:context-window 16384
                       #:max-tokens 2048)
  (register-model! reg #:id "claude-3" #:name "Claude 3 Opus"
                       #:provider-name "anthropic"
                       #:context-window 200000
                       #:max-tokens 4096)

  ;; Verify
  (check-equal? (length (list-providers reg)) 2)
  (check-equal? (length (list-models-for-provider reg "openai")) 2)
  (check-equal? (length (list-models-for-provider reg "anthropic")) 1)

  ;; Search across providers
  (define found (find-model reg "claude-3"))
  (check-true (registered-model? found))
  (check-equal? (registered-model-provider-name found) "anthropic")
  (check-equal? (registered-model-context-window found) 200000)

  ;; Unregister a provider
  (unregister-provider! reg "openai")
  (check-equal? (length (list-providers reg)) 1)
  (check-false (find-model reg "gpt-4o"))
  (check-true (registered-model? (find-model reg "claude-3"))))

(test-case "integration: thread safety - sequential access"
  (define reg (make-provider-registry))
  ;; Register and lookup sequentially - should always be consistent
  (for ([i (in-range 10)])
    (define name (format "provider-~a" i))
    (register-provider! reg name (make-test-provider #:name name))
    (check-true (provider-info? (lookup-provider reg name))))
  (check-equal? (length (list-providers reg)) 10))
