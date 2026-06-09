#lang racket/base

;; tests/test-gap-e4-dynamic-context-tokens.rkt
;; v0.97.6 W1: F4+F5 — Dynamic context tokens from model registry + startup log

(require rackunit
         rackunit/text-ui
         (only-in "../runtime/provider/model-registry.rkt"
                  make-model-registry-from-config
                  model-registry-context-window))

;; Build a minimal config with known model context windows (hash-format, matching JSON config)
(define test-config
  (hasheq
   'providers
   (hasheq 'test-provider
           (hasheq 'base-url
                   "https://api.test.com/v1"
                   'default-model
                   "test-model-4o"
                   'models
                   (list (hasheq 'id "test-model-4o" 'name "Test 4o" 'context-window 128000)
                         (hasheq 'id "test-model-sonnet" 'name "Test Sonnet" 'context-window 200000)
                         (hasheq 'id "test-model-flash" 'name "Test Flash" 'context-window 1048576))))
   'default-provider
   "test-provider"))

(define reg (make-model-registry-from-config test-config))

(define suite
  (test-suite "GAP-E4: Dynamic context tokens from model registry"

    ;; 1. Known model resolves context window
    (test-case "known model resolves context window (128k)"
      (check-equal? (model-registry-context-window reg "test-model-4o") 128000))

    ;; 2. Different model with different context window
    (test-case "sonnet model resolves 200k context window"
      (check-equal? (model-registry-context-window reg "test-model-sonnet") 200000))

    ;; 3. Flash model with 1M context window
    (test-case "flash model resolves 1M context window"
      (check-equal? (model-registry-context-window reg "test-model-flash") 1048576))

    ;; 4. Unknown model returns #f
    (test-case "unknown model returns #f"
      (check-false (model-registry-context-window reg "nonexistent-model")))

    ;; 5. #f model name returns #f
    (test-case "#f model name returns #f"
      (check-false (model-registry-context-window reg #f)))

    ;; 6. Model without context-window in config returns #f
    (test-case "model without context-window returns #f"
      (define no-cw-config
        (hasheq 'providers
                (hasheq 'prov
                        (hasheq 'base-url
                                "https://api.test.com"
                                'default-model
                                "no-cw-model"
                                'models
                                (list (hasheq 'id "no-cw-model" 'name "No CW Model"))))))
      (define no-cw-reg (make-model-registry-from-config no-cw-config))
      (check-false (model-registry-context-window no-cw-reg "no-cw-model")))))

(run-tests suite)
