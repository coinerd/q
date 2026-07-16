#lang racket

;; @speed fast
;; @suite security
;; BOUNDARY: integration

(require rackunit
         racket/port
         racket/string
         "../util/credential-redaction.rkt"
         "../runtime/auth/oauth.rkt"
         "../runtime/provider/provider-registry.rkt"
         "../runtime/provider/model-registry.rkt"
         "../wiring/provider-rpc.rkt"
         "../llm/provider.rkt"
         "../llm/model.rkt"
         (prefix-in rpc: "../interfaces/rpc-mode.rkt"))

(define secrets
  '("api-value" "access-value"
                "refresh-value"
                "client-value"
                "authorization-value"
                "token-value"
                "secret-value"
                "password-value"
                "credential-value"
                "capability-value"
                "capability-secret-value"))

(define nested
  (hash "api_key"
        "api-value"
        'access-token
        "access-value"
        '#:refresh_token
        "refresh-value"
        "nested"
        (list (hash 'clientSecret "client-value")
              (vector (hash "Authorization" "authorization-value") (hash 'token "token-value")))
        'secret
        "secret-value"
        "password"
        "password-value"
        '#:credential
        "credential-value"
        'capability-token
        "capability-value"
        'capability-secret
        "capability-secret-value"
        'token-url
        "https://example.test/token"
        "token_type"
        "Bearer"
        'max-tokens
        4096
        "api-key-env"
        "OPENAI_API_KEY"
        'credential-policy
        "strict"
        'message
        "risk-score set-task-state Bearer authentication"))

(define (render-all value)
  (list (format "~a" value)
        (format "~s" value)
        (format "~v" value)
        (with-output-to-string (lambda () (display value)))
        (with-output-to-string (lambda () (write value)))
        (with-output-to-string (lambda () (print value)))))

(test-case "recursive credential redaction handles nested key variants without mutation"
  (define clean (redact-credential-data nested))
  (for ([secret (in-list secrets)])
    (check-false (string-contains? (format "~s" clean) secret))
    (check-true (string-contains? (format "~s" nested) secret)))
  (check-equal? (hash-ref clean 'token-url) "https://example.test/token")
  (check-equal? (hash-ref clean "token_type") "Bearer")
  (check-equal? (hash-ref clean 'max-tokens) 4096)
  (check-equal? (hash-ref clean "api-key-env") "OPENAI_API_KEY")
  (check-equal? (hash-ref clean 'credential-policy) "strict")
  (check-equal? (hash-ref clean 'message) "risk-score set-task-state Bearer authentication")
  (check-equal? (redact-credential-data clean) clean))

(test-case "canonical credential data and text APIs are idempotent"
  (check-equal? (redact-secrets (redact-secrets "access_token=short-opaque"))
                (redact-secrets "access_token=short-opaque"))
  (check-equal? (redact-credential-data (redact-credential-data nested))
                (redact-credential-data nested)))

(test-case "text redaction catches OAuth JSON, assignments, and headers"
  (for ([text (in-list '("access_token=short-opaque"
                         "refresh-token: short-refresh"
                         "client_secret=short-client"
                         "capability_secret=short-capability"
                         "code_verifier=short-verifier"
                         "authorization-code: short-code"
                         "{\"access_token\":\"short-access\"}"
                         "{\"refresh-token\":\"short-refresh\"}"
                         "Authorization: Bearer abcdefghijklmnopqrstuvwxyz"
                         "Authorization: Bearer short-secret"))])
    (define clean (redact-secrets text))
    (check-true (string-contains? clean "<REDACTED>") text)
    (check-false (contains-secret-leak? clean) text)))

(test-case "OAuth structs have safe writers while accessors and codec stay lossless"
  (define cfg
    (oauth-config "https://example.test/auth"
                  "https://example.test/token"
                  "client-id"
                  "client-secret-raw"
                  '("openid")
                  8089))
  (define tok (oauth-token "access-token-raw" "refresh-token-raw" 1700000000 "Bearer" "openid"))
  (for ([rendered (in-list (append (render-all cfg) (render-all tok)))])
    (check-false (string-contains? rendered "client-secret-raw"))
    (check-false (string-contains? rendered "access-token-raw"))
    (check-false (string-contains? rendered "refresh-token-raw")))
  (check-equal? (oauth-config-client-secret cfg) "client-secret-raw")
  (check-equal? (oauth-token-access-token tok) "access-token-raw")
  (check-equal? (oauth-token-refresh-token tok) "refresh-token-raw")
  (define js (oauth-token->jsexpr tok))
  (check-equal? (hash-ref js 'access-token) "access-token-raw")
  (check-equal? (hash-ref js 'refresh-token) "refresh-token-raw"))

(test-case "OAuth HTTP exchange receives exact raw client material"
  (define cfg
    (oauth-config "https://example.test/auth"
                  "https://example.test/token"
                  "client-id"
                  "client-secret-raw"
                  '("openid")
                  8089))
  (define captured #f)
  (parameterize ([current-oauth-http-sendrecv
                  (lambda (_host _path #:ssl? _ssl? #:method _method #:headers _headers #:data data)
                    (set! captured (bytes->string/utf-8 data))
                    (values
                     #"HTTP/1.1 200 OK"
                     '()
                     (open-input-string
                      "{\"access_token\":\"raw-access\",\"refresh_token\":\"raw-refresh\"}")))])
    (define tok (oauth-exchange-code cfg "raw-code" #:code-verifier "raw-verifier"))
    (check-true (oauth-token? tok)))
  (for ([raw (in-list '("client_secret=client-secret-raw" "code=raw-code"
                                                          "code_verifier=raw-verifier"))])
    (check-true (string-contains? captured raw))))

(define (mock-response)
  (make-model-response (list (hasheq 'type "text" 'text "ok"))
                       (hasheq 'input-tokens 0 'output-tokens 0)
                       "mock"
                       'stop))

(test-case "provider/model writers and public summaries redact while raw resolution stays lossless"
  (define raw-config
    (hash 'api-key
          "provider-api-raw"
          "nested"
          (hash 'refresh_token "provider-refresh-raw")
          'base-url
          "https://api.example.test"
          'models
          '("m1")))
  (define reg (make-provider-registry))
  (register-provider! reg "p" (make-mock-provider (mock-response)) #:config raw-config)
  (register-model! reg
                   #:id "m1"
                   #:name "M1"
                   #:provider-name "p"
                   #:capabilities (hash 'client-secret "model-secret-raw" 'streaming #t))
  (define pinfo (lookup-provider reg "p"))
  (check-equal? (hash-ref (provider-info-config pinfo) 'api-key) "provider-api-raw")
  (for ([rendered (in-list (render-all pinfo))])
    (check-false (string-contains? rendered "provider-api-raw"))
    (check-false (string-contains? rendered "provider-refresh-raw")))
  (define metadata (provider-metadata pinfo))
  (check-equal? (hash-ref (hash-ref metadata 'config) 'api-key) "<REDACTED>")
  (define summary (provider-summary reg "p"))
  (check-false (string-contains? (format "~s" summary) "model-secret-raw"))

  (define model-reg
    (make-model-registry-from-config
     (hash 'providers (hash 'p raw-config) 'default-provider "p" 'default-model "m1")))
  (define entry (car (available-models model-reg)))
  (define resolution (resolve-model model-reg "m1"))
  (check-equal? (hash-ref (model-entry-provider-config entry) 'api-key) "provider-api-raw")
  (check-equal? (hash-ref (model-resolution-provider-config resolution) 'api-key) "provider-api-raw")
  (for ([rendered (in-list (append (render-all entry) (render-all resolution)))])
    (check-false (string-contains? rendered "provider-api-raw")))

  (define handlers (make-provider-rpc-handlers (hasheq 'provider-registry reg)))
  (define rpc-list ((hash-ref handlers 'providers/list) (hasheq)))
  (check-false (string-contains? (format "~s" rpc-list) "provider-api-raw"))
  (define rpc-models ((hash-ref handlers 'providers/models) (hasheq)))
  (check-false (string-contains? (format "~s" rpc-models) "model-secret-raw")))
