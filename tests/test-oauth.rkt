#lang racket

;; tests/test-oauth.rkt — OAuth config, token, and URL generation tests

(require rackunit
         racket/file
         "../runtime/oauth.rkt")

;; ═══════════════════════════════════════════════════════════════════
;; OAuth config struct tests
;; ═══════════════════════════════════════════════════════════════════

(test-case "oauth-config struct creation"
  (define cfg
    (oauth-config "https://accounts.google.com/o/oauth2/v2/auth"
                  "https://oauth2.googleapis.com/token"
                  "client-123"
                  "secret-456"
                  '("openid" "email")
                  8089))
  (check-equal? (oauth-config-client-id cfg) "client-123")
  (check-equal? (oauth-config-redirect-port cfg) 8089)
  (check-equal? (oauth-config-scopes cfg) '("openid" "email")))

(test-case "valid-oauth-config? accepts valid config"
  (define cfg
    (oauth-config "https://example.com/auth"
                  "https://example.com/token"
                  "client-123"
                  "secret"
                  '("scope1")
                  8089))
  (check-true (valid-oauth-config? cfg)))

(test-case "valid-oauth-config? rejects empty client-id"
  (define cfg
    (oauth-config "https://example.com/auth"
                  "https://example.com/token"
                  ""
                  "secret"
                  '("scope1")
                  8089))
  (check-false (valid-oauth-config? cfg)))

(test-case "valid-oauth-config? rejects non-config"
  (check-false (valid-oauth-config? "not-a-config"))
  (check-false (valid-oauth-config? 42)))

(test-case "valid-oauth-config? rejects zero redirect-port"
  (define cfg
    (oauth-config "https://example.com/auth"
                  "https://example.com/token"
                  "client-123"
                  "secret"
                  '("scope1")
                  0))
  (check-false (valid-oauth-config? cfg)))

;; ═══════════════════════════════════════════════════════════════════
;; OAuth token struct tests
;; ═══════════════════════════════════════════════════════════════════

(test-case "oauth-token struct"
  (define tok (oauth-token "access-123" "refresh-456" (+ (current-seconds) 3600) "Bearer" "openid"))
  (check-equal? (oauth-token-access-token tok) "access-123")
  (check-equal? (oauth-token-refresh-token tok) "refresh-456")
  (check-false (oauth-token-expired? tok)))

(test-case "oauth-token-expired? detects expired tokens"
  (define tok (oauth-token "old" "refresh" (- (current-seconds) 100) "Bearer" "openid"))
  (check-true (oauth-token-expired? tok)))

(test-case "oauth-token-expired? with margin"
  (define tok (oauth-token "expiring" "refresh" (+ (current-seconds) 30) "Bearer" "openid"))
  (check-true (oauth-token-expired? tok 60)) ; 30s remaining, 60s margin → expired
  (check-false (oauth-token-expired? tok 10))) ; 30s remaining, 10s margin → not expired

;; ═══════════════════════════════════════════════════════════════════
;; URL generation tests
;; ═══════════════════════════════════════════════════════════════════

(test-case "oauth-authorize-url generates correct URL"
  (define cfg
    (oauth-config "https://auth.example.com/authorize"
                  "https://auth.example.com/token"
                  "my-client"
                  "my-secret"
                  '("openid" "profile")
                  8089))
  (define url (oauth-authorize-url cfg "random-state"))
  (check-true (string-contains? url "client_id=my-client"))
  (check-true (string-contains? url "redirect_uri=http%3A%2F%2Flocalhost%3A8089%2Fcallback"))
  (check-true (string-contains? url "state=random-state"))
  (check-true (string-contains? url "response_type=code"))
  (check-true (string-contains? url "openid%20profile")))

(test-case "oauth-authorize-url starts with authorize-url"
  (define cfg
    (oauth-config "https://auth.example.com/authorize"
                  "https://auth.example.com/token"
                  "my-client"
                  "my-secret"
                  '("scope1")
                  9090))
  (define url (oauth-authorize-url cfg "test-state"))
  (check-true (string-prefix? url "https://auth.example.com/authorize?")))

;; ═══════════════════════════════════════════════════════════════════
;; Token exchange/refresh stub tests
;; ═══════════════════════════════════════════════════════════════════

(test-case "oauth-exchange-code raises error (stub)"
  (define cfg
    (oauth-config "https://example.com/auth"
                  "https://example.com/token"
                  "client-id"
                  "secret"
                  '("scope")
                  8089))
  (check-exn exn:fail? (lambda () (oauth-exchange-code cfg "some-auth-code"))))

(test-case "oauth-refresh-token raises error (stub)"
  (define cfg
    (oauth-config "https://example.com/auth"
                  "https://example.com/token"
                  "client-id"
                  "secret"
                  '("scope")
                  8089))
  (define tok (oauth-token "access" "refresh" 1000 "Bearer" "openid"))
  (check-exn exn:fail? (lambda () (oauth-refresh-token cfg tok))))

;; ═══════════════════════════════════════════════════════════════════
;; Serialization tests
;; ═══════════════════════════════════════════════════════════════════

(test-case "oauth-token->jsexpr round-trip"
  (define tok (oauth-token "access-123" "refresh-456" 1700000000 "Bearer" "openid email"))
  (define js (oauth-token->jsexpr tok))
  (check-equal? (hash-ref js 'access-token) "access-123")
  (check-equal? (hash-ref js 'refresh-token) "refresh-456")
  (check-equal? (hash-ref js 'expires-at) 1700000000)
  (define restored (jsexpr->oauth-token js))
  (check-equal? (oauth-token-access-token restored) "access-123")
  (check-equal? (oauth-token-refresh-token restored) "refresh-456")
  (check-equal? (oauth-token-expires-at restored) 1700000000))

(test-case "jsexpr->oauth-token returns #f for invalid input"
  (check-false (jsexpr->oauth-token (hasheq)))
  (check-false (jsexpr->oauth-token "not a hash")))

;; ═══════════════════════════════════════════════════════════════════
;; Token persistence tests (using temp files)
;; ═══════════════════════════════════════════════════════════════════

(test-case "store-oauth-token! and get-oauth-token round-trip"
  (define tmp (make-temporary-file "oauth-test-~a.json"))
  (delete-file tmp) ; ensure clean start
  (define tok (oauth-token "access-abc" "refresh-xyz" (+ (current-seconds) 3600) "Bearer" "openid"))
  (store-oauth-token! "test-provider" tok #:path tmp)
  (define retrieved (get-oauth-token "test-provider" #:path tmp))
  (check-true (oauth-token? retrieved))
  (check-equal? (oauth-token-access-token retrieved) "access-abc")
  (check-equal? (oauth-token-refresh-token retrieved) "refresh-xyz")
  (delete-file tmp))

(test-case "get-oauth-token returns #f for missing provider"
  (define tmp (make-temporary-file "oauth-test-~a.json"))
  (delete-file tmp)
  (check-false (get-oauth-token "nonexistent" #:path tmp))
  (when (file-exists? tmp)
    (delete-file tmp)))

(test-case "store multiple OAuth tokens"
  (define tmp (make-temporary-file "oauth-test-~a.json"))
  (delete-file tmp)
  (define tok1 (oauth-token "access-1" "refresh-1" (+ (current-seconds) 3600) "Bearer" "scope1"))
  (define tok2 (oauth-token "access-2" "refresh-2" (+ (current-seconds) 7200) "Bearer" "scope2"))
  (store-oauth-token! "provider-a" tok1 #:path tmp)
  (store-oauth-token! "provider-b" tok2 #:path tmp)
  (check-equal? (oauth-token-access-token (get-oauth-token "provider-a" #:path tmp)) "access-1")
  (check-equal? (oauth-token-access-token (get-oauth-token "provider-b" #:path tmp)) "access-2")
  (delete-file tmp))

(test-case "get-valid-oauth-token returns non-expired token"
  (define tmp (make-temporary-file "oauth-test-~a.json"))
  (delete-file tmp)
  (define tok
    (oauth-token "valid-access" "valid-refresh" (+ (current-seconds) 3600) "Bearer" "openid"))
  (store-oauth-token! "my-provider" tok #:path tmp)
  (define cfg
    (oauth-config "https://example.com/auth"
                  "https://example.com/token"
                  "client-id"
                  "secret"
                  '("openid")
                  8089))
  (define result (get-valid-oauth-token "my-provider" cfg #:path tmp))
  (check-true (oauth-token? result))
  (check-equal? (oauth-token-access-token result) "valid-access")
  (delete-file tmp))

(test-case "get-valid-oauth-token raises for expired token (stub refresh)"
  (define tmp (make-temporary-file "oauth-test-~a.json"))
  (delete-file tmp)
  (define tok
    (oauth-token "expired-access" "expired-refresh" (- (current-seconds) 100) "Bearer" "openid"))
  (store-oauth-token! "expired-provider" tok #:path tmp)
  (define cfg
    (oauth-config "https://example.com/auth"
                  "https://example.com/token"
                  "client-id"
                  "secret"
                  '("openid")
                  8089))
  ;; oauth-refresh-token raises, so get-valid propagates the error
  (check-exn exn:fail? (lambda () (get-valid-oauth-token "expired-provider" cfg #:path tmp)))
  (delete-file tmp))

(test-case "get-valid-oauth-token returns #f for missing provider"
  (define tmp (make-temporary-file "oauth-test-~a.json"))
  (delete-file tmp)
  (define cfg
    (oauth-config "https://example.com/auth"
                  "https://example.com/token"
                  "client-id"
                  "secret"
                  '("openid")
                  8089))
  (check-false (get-valid-oauth-token "no-such-provider" cfg #:path tmp))
  (when (file-exists? tmp)
    (delete-file tmp)))
