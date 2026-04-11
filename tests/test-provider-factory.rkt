#lang racket

(require rackunit
         "../runtime/provider-factory.rkt"
         "../runtime/settings.rkt"
         "../llm/model.rkt"
         "../llm/provider.rkt")

;; ============================================================
;; local-provider? tests
;; ============================================================

(test-case "local-provider? recognizes localhost"
  (check-true (local-provider? "http://localhost:8080")))

(test-case "local-provider? recognizes 127.0.0.1"
  (check-true (local-provider? "http://127.0.0.1:3000")))

(test-case "local-provider? recognizes 192.168.x.x"
  (check-true (local-provider? "http://192.168.1.100/api")))

(test-case "local-provider? recognizes 10.x.x.x"
  (check-true (local-provider? "http://10.0.0.1/v1")))

(test-case "local-provider? recognizes 172.16.x.x (RFC 1918 start)"
  (check-true (local-provider? "http://172.16.0.1/v1")))

(test-case "local-provider? recognizes 172.31.255.255 (RFC 1918 end)"
  (check-true (local-provider? "http://172.31.255.255/v1")))

(test-case "local-provider? rejects 172.15.0.1 (below /12 range)"
  (check-false (local-provider? "http://172.15.0.1/v1")))

(test-case "local-provider? rejects 172.32.0.1 (above /12 range)"
  (check-false (local-provider? "http://172.32.0.1/v1")))

(test-case "local-provider? rejects 172.1.0.1 (public)"
  (check-false (local-provider? "http://172.1.0.1/v1")))

(test-case "local-provider? rejects 172.100.0.1 (public)"
  (check-false (local-provider? "http://172.100.0.1/v1")))

(test-case "local-provider? rejects public URLs"
  (check-false (local-provider? "https://api.openai.com/v1")))

(test-case "local-provider? rejects empty string"
  (check-false (local-provider? "")))

(test-case "local-provider? rejects non-string"
  (check-false (local-provider? 42)))

;; ============================================================
;; build-mock-provider
;; ============================================================

(test-case "build-mock-provider returns a provider"
  (define p (build-mock-provider))
  (check-pred provider? p)
  (check-equal? (provider-name p) "mock"))

(test-case "build-mock-provider send returns mock response"
  (define p (build-mock-provider))
  (define req (make-model-request '() #f (hasheq)))
  (define resp (provider-send p req))
  (check-pred model-response? resp)
  (check-equal? (model-response-model resp) "mock-model")
  (check-eq? (model-response-stop-reason resp) 'stop))

(test-case "build-mock-provider response has zero usage"
  (define p (build-mock-provider))
  (define resp (provider-send p (make-model-request '() #f (hasheq))))
  (check-equal? (hash-ref (model-response-usage resp) 'total-tokens) 0))

;; ============================================================
;; build-provider — fallback to mock when no config
;; ============================================================

(test-case "build-provider returns mock for empty settings"
  (define settings (q-settings (hash) (hash) (hash)))  ; empty settings
  (define config (hasheq))
  (define p (build-provider config settings))
  (check-pred provider? p)
  (check-equal? (provider-name p) "mock"))

;; ============================================================
;; create-provider-for-name tests (STRUC-01 fix)
;; ============================================================

(test-case "create-provider-for-name returns anthropic provider for anthropic name"
  (define p (create-provider-for-name "anthropic" "https://api.anthropic.com" "test-key" "claude-sonnet-4-20250514"))
  (check-pred provider? p)
  (check-equal? (provider-name p) "anthropic"))

(test-case "create-provider-for-name returns gemini provider for gemini name"
  (define p (create-provider-for-name "gemini" "https://generativelanguage.googleapis.com" "test-key" "gemini-2.0-flash"))
  (check-pred provider? p)
  (check-equal? (provider-name p) "gemini"))

(test-case "create-provider-for-name returns openai-compatible provider for openai name"
  (define p (create-provider-for-name "openai" "https://api.openai.com/v1" "test-key" "gpt-4o"))
  (check-pred provider? p)
  (check-equal? (provider-name p) "openai-compatible"))
