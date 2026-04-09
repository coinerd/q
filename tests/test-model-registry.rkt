#lang racket
;; test-model-registry.rkt — tests for runtime/model-registry.rkt

(require rackunit
         racket/string
         "../runtime/model-registry.rkt")

;; ============================================================
;; Test config helpers — build config hashes for testing
;; ============================================================

(define (make-test-config)
  (hasheq
   'providers
   (hasheq
    'openai
    (hasheq 'base-url "https://api.openai.com/v1"
            'api-key-env "OPENAI_API_KEY"
            'default-model "gpt-4o"
            'models '("gpt-4" "gpt-4o" "gpt-3.5-turbo"))
    'anthropic
    (hasheq 'base-url "https://api.anthropic.com/v1"
            'api-key-env "ANTHROPIC_API_KEY"
            'default-model "claude-3-sonnet"
            'models '("claude-3-opus" "claude-3-sonnet" "claude-3-haiku")))
   'default-provider "openai"
   'default-model "gpt-4o"))

(define (make-test-config-with-string-keys)
  ;; Simulates config parsed from JSON (string keys)
  (hash
   "providers"
   (hash
    "openai"
    (hash "base-url" "https://api.openai.com/v1"
          "api-key-env" "OPENAI_API_KEY"
          "default-model" "gpt-4o"
          "models" '("gpt-4" "gpt-4o" "gpt-3.5-turbo"))
    "anthropic"
    (hash "base-url" "https://api.anthropic.com/v1"
          "api-key-env" "ANTHROPIC_API_KEY"
          "default-model" "claude-3-sonnet"
          "models" '("claude-3-opus" "claude-3-sonnet" "claude-3-haiku")))
   "default-provider" "openai"
   "default-model" "gpt-4o"))

(define (make-single-provider-config)
  (hasheq
   'providers
   (hasheq
    'my-provider
    (hasheq 'base-url "https://my-llm.example.com/v1"
            'api-key-env "MY_API_KEY"
            'default-model "my-model-v2"
            'models '("my-model-v1" "my-model-v2")))
   'default-provider "my-provider"
   'default-model "my-model-v2"))

(define (make-empty-config)
  (hasheq
   'providers (hasheq)))

(define (make-minimal-config)
  ;; No default-provider or default-model
  (hasheq
   'providers
   (hasheq
    'groq
    (hasheq 'base-url "https://api.groq.com/v1"
            'api-key-env "GROQ_API_KEY"
            'default-model "llama3-70b"
            'models '("llama3-70b" "mixtral-8x7b")))))

;; ============================================================
;; Tests — registry creation
;; ============================================================

(check-not-false
 (make-model-registry-from-config (make-test-config))
 "registry creation from symbol-key config")

(check-not-false
 (make-model-registry-from-config (make-test-config-with-string-keys))
 "registry creation from string-key config")

(check-not-false
 (make-model-registry-from-config (make-empty-config))
 "registry creation from empty config")

;; ============================================================
;; Tests — available-models
;; ============================================================

(let* ([reg (make-model-registry-from-config (make-test-config))]
       [models (available-models reg)]
       [names (sort (map model-entry-name models) string<?)])
  (check-equal? names
                '("claude-3-haiku" "claude-3-opus" "claude-3-sonnet"
                  "gpt-3.5-turbo" "gpt-4" "gpt-4o")
                "available-models lists all models"))

(let* ([reg (make-model-registry-from-config (make-single-provider-config))]
       [models (available-models reg)])
  (check-equal? (sort (map model-entry-name models) string<?)
                '("my-model-v1" "my-model-v2")
                "available-models from single provider"))

(let ([reg (make-model-registry-from-config (make-empty-config))])
  (check-equal? (available-models reg) '()
                "available-models from empty config returns empty list"))

(let* ([reg (make-model-registry-from-config (make-test-config))]
       [models (available-models reg)]
       [openai-count (length (filter (λ (e) (equal? (model-entry-provider-name e) "openai")) models))]
       [anthropic-count (length (filter (λ (e) (equal? (model-entry-provider-name e) "anthropic")) models))])
  (check-equal? openai-count 3 "openai models count")
  (check-equal? anthropic-count 3 "anthropic models count"))

;; ============================================================
;; Tests — resolve-model exact match
;; ============================================================

(let* ([reg (make-model-registry-from-config (make-test-config))]
       [res (resolve-model reg "gpt-4o")])
  (check-not-false res "gpt-4o resolves")
  (check-equal? (model-resolution-model-name res) "gpt-4o")
  (check-equal? (model-resolution-provider-name res) "openai")
  (check-equal? (model-resolution-base-url res) "https://api.openai.com/v1"))

(let* ([reg (make-model-registry-from-config (make-test-config))]
       [res (resolve-model reg "claude-3-opus")])
  (check-not-false res "claude-3-opus resolves")
  (check-equal? (model-resolution-model-name res) "claude-3-opus")
  (check-equal? (model-resolution-provider-name res) "anthropic")
  (check-equal? (model-resolution-base-url res) "https://api.anthropic.com/v1"))

(let* ([reg (make-model-registry-from-config (make-test-config))]
       [res (resolve-model reg "gpt-3.5-turbo")])
  (check-not-false res "gpt-3.5-turbo resolves")
  (check-equal? (model-resolution-model-name res) "gpt-3.5-turbo")
  (check-equal? (model-resolution-provider-name res) "openai"))

;; ============================================================
;; Tests — resolve-model provider prefix syntax
;; ============================================================

(let* ([reg (make-model-registry-from-config (make-test-config))]
       [res (resolve-model reg "openai/gpt-4o")])
  (check-not-false res "provider prefix resolves")
  (check-equal? (model-resolution-model-name res) "gpt-4o")
  (check-equal? (model-resolution-provider-name res) "openai"))

(let* ([reg (make-model-registry-from-config (make-test-config))]
       [res (resolve-model reg "anthropic/claude-3-haiku")])
  (check-not-false res "anthropic prefix resolves")
  (check-equal? (model-resolution-model-name res) "claude-3-haiku")
  (check-equal? (model-resolution-provider-name res) "anthropic"))

(check-false
 (resolve-model (make-model-registry-from-config (make-test-config))
                "unknown-provider/gpt-4o")
 "provider prefix with unknown provider returns #f")

(check-false
 (resolve-model (make-model-registry-from-config (make-test-config))
                "openai/nonexistent-model")
 "provider prefix with unknown model returns #f")

;; ============================================================
;; Tests — resolve-model default (#f)
;; ============================================================

(let* ([reg (make-model-registry-from-config (make-test-config))]
       [res (resolve-model reg #f)])
  (check-not-false res "default model resolves")
  (check-equal? (model-resolution-model-name res) "gpt-4o")
  (check-equal? (model-resolution-provider-name res) "openai"))

(let ([cfg (hasheq
            'providers
            (hasheq
             'openai
             (hasheq 'base-url "https://api.openai.com/v1"
                     'api-key-env "OPENAI_API_KEY"
                     'default-model "gpt-4o"
                     'models '("gpt-4o")))
            'default-provider "openai")]
      ;; Note: no 'default-model key at all
      )
  (let* ([reg (make-model-registry-from-config cfg)]
         [res (resolve-model reg #f)])
    (check-not-false res "default from provider's default-model")
    (check-equal? (model-resolution-model-name res) "gpt-4o")))

;; ============================================================
;; Tests — resolve-model graceful failure
;; ============================================================

(check-false
 (resolve-model (make-model-registry-from-config (make-test-config)) "nonexistent-model")
 "unknown model returns #f")

(let ([reg (make-model-registry-from-config (make-empty-config))])
  (check-false (resolve-model reg "anything") "empty registry resolve returns #f")
  (check-false (resolve-model reg #f) "empty registry default returns #f"))

;; ============================================================
;; Tests — resolve-model-by-provider
;; ============================================================

(let* ([reg (make-model-registry-from-config (make-test-config))]
       [res (resolve-model-by-provider reg "openai")])
  (check-not-false res)
  (check-equal? (model-resolution-model-name res) "gpt-4o")
  (check-equal? (model-resolution-provider-name res) "openai"))

(let* ([reg (make-model-registry-from-config (make-test-config))]
       [res (resolve-model-by-provider reg "anthropic")])
  (check-not-false res)
  (check-equal? (model-resolution-model-name res) "claude-3-sonnet")
  (check-equal? (model-resolution-provider-name res) "anthropic"))

(check-false
 (resolve-model-by-provider (make-model-registry-from-config (make-test-config)) "nonexistent")
 "resolve-model-by-provider returns #f for unknown provider")

;; ============================================================
;; Tests — default-model
;; ============================================================

(check-equal?
 (default-model (make-model-registry-from-config (make-test-config)))
 "gpt-4o"
 "default-model returns global default")

(check-equal?
 (default-model (make-model-registry-from-config (make-single-provider-config)))
 "my-model-v2"
 "default-model with single provider")

(check-equal?
 (default-model (make-model-registry-from-config (make-minimal-config)))
 "llama3-70b"
 "default-model falls back to first provider's default")

(check-false
 (default-model (make-model-registry-from-config (make-empty-config)))
 "default-model returns #f for empty config")

;; ============================================================
;; Tests — default-model-for-mode
;; ============================================================

(let ([reg (make-model-registry-from-config (make-test-config))])
  (check-equal? (default-model-for-mode reg 'chat) "gpt-4o")
  (check-equal? (default-model-for-mode reg 'single) "gpt-4o")
  (check-equal? (default-model-for-mode reg 'tool-heavy) "gpt-4o")
  (check-equal? (default-model-for-mode reg 'fast) "gpt-4o"))

(check-false
 (default-model-for-mode (make-model-registry-from-config (make-empty-config)) 'chat)
 "default-model-for-mode returns #f for empty registry")

;; ============================================================
;; Tests — model-resolution includes provider-config
;; ============================================================

(let* ([reg (make-model-registry-from-config (make-test-config))]
       [res (resolve-model reg "gpt-4o")])
  (check-not-false (model-resolution-provider-config res))
  (check-equal? (hash-ref (model-resolution-provider-config res) 'base-url)
                "https://api.openai.com/v1"))

;; ============================================================
;; Tests — string-key config interop
;; ============================================================

(let* ([reg (make-model-registry-from-config (make-test-config-with-string-keys))]
       [res (resolve-model reg "gpt-4o")])
  (check-not-false res "string-key config resolves")
  (check-equal? (model-resolution-model-name res) "gpt-4o")
  (check-equal? (model-resolution-provider-name res) "openai")
  (check-equal? (model-resolution-base-url res) "https://api.openai.com/v1"))

(let* ([reg (make-model-registry-from-config (make-test-config-with-string-keys))]
       [res (resolve-model reg #f)])
  (check-not-false res "string-key config default resolves")
  (check-equal? (model-resolution-model-name res) "gpt-4o"))

;; ============================================================
;; Tests — edge cases
;; ============================================================

;; Duplicate model name across providers
(let* ([cfg (hasheq
             'providers
             (hasheq
              'alpha
              (hasheq 'base-url "https://alpha.example.com/v1"
                      'api-key-env "ALPHA_KEY"
                      'default-model "shared-model"
                      'models '("shared-model"))
              'beta
              (hasheq 'base-url "https://beta.example.com/v1"
                      'api-key-env "BETA_KEY"
                      'default-model "shared-model"
                      'models '("shared-model"))))]
       [reg (make-model-registry-from-config cfg)]
       [res (resolve-model reg "shared-model")]
       [alpha-res (resolve-model reg "alpha/shared-model")]
       [beta-res (resolve-model reg "beta/shared-model")])
  (check-not-false res "shared model resolves to something")
  (check-not-false alpha-res "alpha prefix resolves")
  (check-equal? (model-resolution-provider-name alpha-res) "alpha")
  (check-not-false beta-res "beta prefix resolves")
  (check-equal? (model-resolution-provider-name beta-res) "beta"))

;; provider-config preserved in model-entry
(let* ([reg (make-model-registry-from-config (make-test-config))]
       [models (available-models reg)]
       [gpt4-entry (findf (λ (e) (equal? (model-entry-name e) "gpt-4")) models)])
  (check-not-false gpt4-entry)
  (check-equal? (hash-ref (model-entry-provider-config gpt4-entry) 'api-key-env)
                "OPENAI_API_KEY"))

;; ============================================================
;; Tests — object-style model definitions (BUG-01 regression)
;; ============================================================

(define (make-object-style-config)
  ;; Config with object-style model definitions (hasheq with 'id field)
  ;; This is how JSON configs with {"id": "...", "name": "..."} parse
  (hasheq
   'providers
   (hasheq
    'local
    (hasheq 'base-url "http://127.0.0.1:8080/v1"
            'api-key-env "LOCAL_API_KEY"
            'default-model "gemma-4"
            'models (list (hasheq 'id "gemma-4"
                                  'name "Gemma 4"
                                  'context-window 131072)
                          (hasheq 'id "llama3-8b"
                                  'name "Llama 3 8B"
                                  'context-window 8192))))
   'default-provider "local"
   'default-model "gemma-4"))

(let* ([reg (make-model-registry-from-config (make-object-style-config))]
       [res (resolve-model reg "gemma-4")])
  (check-not-false res "object-style: gemma-4 resolves")
  (check-equal? (model-resolution-model-name res) "gemma-4")
  (check-equal? (model-resolution-provider-name res) "local")
  (check-equal? (model-resolution-base-url res) "http://127.0.0.1:8080/v1"))

(let* ([reg (make-model-registry-from-config (make-object-style-config))]
       [res (resolve-model reg "llama3-8b")])
  (check-not-false res "object-style: llama3-8b resolves")
  (check-equal? (model-resolution-model-name res) "llama3-8b"))

(let* ([reg (make-model-registry-from-config (make-object-style-config))]
       [models (available-models reg)]
       [names (map model-entry-name models)])
  (check-equal? (length models) 2 "object-style: two models available")
  (check-not-false (member "gemma-4" names) "object-style: gemma-4 in available models")
  (check-not-false (member "llama3-8b" names) "object-style: llama3-8b in available models"))

(let* ([reg (make-model-registry-from-config (make-object-style-config))]
       [res (resolve-model reg "local/gemma-4")])
  (check-not-false res "object-style: provider prefix resolves")
  (check-equal? (model-resolution-model-name res) "gemma-4"))

(let* ([reg (make-model-registry-from-config (make-object-style-config))]
       [res (resolve-model reg #f)])
  (check-not-false res "object-style: default model resolves")
  (check-equal? (model-resolution-model-name res) "gemma-4"))

;; Mixed string and object style (backward compatibility)
(define (make-mixed-style-config)
  (hasheq
   'providers
   (hasheq
    'mixed
    (hasheq 'base-url "http://localhost:8080/v1"
            'default-model "model-a"
            'models (list "model-a"  ; string style
                          (hasheq 'id "model-b" 'context-window 8192))))))  ; object style

(let* ([reg (make-model-registry-from-config (make-mixed-style-config))]
       [res-a (resolve-model reg "model-a")]
       [res-b (resolve-model reg "model-b")])
  (check-not-false res-a "mixed-style: string model resolves")
  (check-not-false res-b "mixed-style: object model resolves")
  (check-equal? (model-resolution-model-name res-a) "model-a")
  (check-equal? (model-resolution-model-name res-b) "model-b"))
