#lang racket/base

;; @speed fast
;; @suite default
;; tests/test-audit-v09945-w7-creds.rkt — Credentials subsystem real-world audit
;;
;; Audit of the Credentials subsystem covering:
;;   1. Backend protocol (credential-backend struct, generic operations)
;;   2. Memory backend (store/load/delete/list)
;;   3. Env backend (env-var lookup, read-only store)
;;   4. File backend (JSON store/load/delete, atomic write)
;;   5. Chained backend (multi-backend fallback, merge, delete-all)
;;   6. Policy-aware backend (env-only, keychain-required, keychain-preferred)
;;   7. Auth store (credential struct, masking, lookup, validation)
;;   8. OAuth (config struct, token struct, URL generation, serialization)
;;   9. Platform backends (capability detection)

(require rackunit
         racket/file
         racket/list
         racket/string
         json
         ;; Backend protocol
         "../runtime/credentials/protocol.rkt"
         ;; Memory backend
         "../runtime/credentials/memory-backend.rkt"
         ;; Env backend
         "../runtime/credentials/env-backend.rkt"
         ;; File backend
         "../runtime/credentials/file-backend.rkt"
         ;; Chained backend
         "../runtime/credentials/chained-backend.rkt"
         ;; Platform backends (capabilities only — actual backends need OS commands)
         (only-in "../runtime/credentials/platform-backends.rkt" credential-backend-capabilities)
         ;; Auth store
         "../runtime/auth/auth-store.rkt"
         ;; OAuth
         (only-in "../runtime/auth/oauth.rkt"
                  oauth-config
                  oauth-config?
                  oauth-config-authorize-url
                  oauth-config-token-url
                  oauth-config-client-id
                  oauth-config-client-secret
                  oauth-config-scopes
                  oauth-config-redirect-port
                  oauth-token
                  oauth-token?
                  oauth-token-access-token
                  oauth-token-refresh-token
                  oauth-token-expires-at
                  oauth-token-token-type
                  oauth-token-scope
                  oauth-available?
                  oauth-token-expired?
                  valid-oauth-config?
                  oauth-authorize-url
                  oauth-token->jsexpr
                  jsexpr->oauth-token))

;; ---------------------------------------------------------------------------
;; 1. Backend Protocol
;; ---------------------------------------------------------------------------

(test-case "audit-proto-struct-predicate"
  (define be (make-memory-credential-backend))
  (check-true (credential-backend? be))
  (check-false (credential-backend? "not-a-backend"))
  (check-false (credential-backend? 42)))

(test-case "audit-proto-name-accessor"
  (define be (make-memory-credential-backend))
  (check-equal? (credential-backend-name be) "memory"))

(test-case "audit-proto-backend-name-generic"
  (define be (make-memory-credential-backend))
  (check-equal? (backend-name be) "memory"))

(test-case "audit-proto-policy-predicate"
  (check-true (credential-policy? 'auto))
  (check-true (credential-policy? 'keychain-preferred))
  (check-true (credential-policy? 'keychain-required))
  (check-true (credential-policy? 'env-only))
  (check-false (credential-policy? 'bogus))
  (check-false (credential-policy? "auto")))

(test-case "audit-proto-valid-policies"
  (check-equal? (length valid-credential-policies) 4)
  (check-not-false (member 'auto valid-credential-policies))
  (check-not-false (member 'env-only valid-credential-policies)))

(test-case "audit-proto-shell-escape"
  (check-equal? (shell-escape "hello") "hello")
  (check-equal? (shell-escape "it's") "it'\\''s"))

(test-case "audit-proto-command-runner-seams"
  (check-true (procedure? (current-external-command-runner)))
  (check-true (procedure? (current-shell-command-runner))))

;; ---------------------------------------------------------------------------
;; 2. Memory Backend
;; ---------------------------------------------------------------------------

(test-case "audit-mem-store-and-load"
  (define be (make-memory-credential-backend))
  (backend-store! be "anthropic" "sk-ant-test-key-123")
  (define cred (backend-load be "anthropic"))
  (check-true (hash? cred))
  (check-equal? (hash-ref cred 'api-key) "sk-ant-test-key-123")
  (check-equal? (hash-ref cred 'source) "memory")
  (check-equal? (hash-ref cred 'provider) "anthropic"))

(test-case "audit-mem-load-missing"
  (define be (make-memory-credential-backend))
  (check-false (backend-load be "nonexistent")))

(test-case "audit-mem-delete"
  (define be (make-memory-credential-backend))
  (backend-store! be "openai" "sk-test123")
  (backend-delete! be "openai")
  (check-false (backend-load be "openai")))

(test-case "audit-mem-list-providers"
  (define be (make-memory-credential-backend))
  (backend-store! be "openai" "key1")
  (backend-store! be "anthropic" "key2")
  (backend-store! be "google" "key3")
  (define providers (backend-list-providers be))
  (check-equal? (length providers) 3)
  (check-not-false (member "openai" providers))
  (check-not-false (member "anthropic" providers))
  (check-not-false (member "google" providers)))

(test-case "audit-mem-available"
  (define be (make-memory-credential-backend))
  (check-true (backend-available? be)))

(test-case "audit-mem-overwrite"
  (define be (make-memory-credential-backend))
  (backend-store! be "openai" "old-key")
  (backend-store! be "openai" "new-key")
  (check-equal? (hash-ref (backend-load be "openai") 'api-key) "new-key"))

;; ---------------------------------------------------------------------------
;; 3. Env Backend
;; ---------------------------------------------------------------------------

(test-case "audit-env-load-with-explicit-var"
  (define be (make-env-credential-backend))
  ;; Set env var temporarily
  (putenv "TEST_ENV_CRED_KEY" "env-value-12345")
  (define cred (backend-load be "test-provider" #:env-var "TEST_ENV_CRED_KEY"))
  (check-true (hash? cred))
  (check-equal? (hash-ref cred 'api-key) "env-value-12345")
  (check-equal? (hash-ref cred 'source) "environment")
  (check-equal? (hash-ref cred 'provider) "test-provider"))

(test-case "audit-env-load-with-derived-var"
  (define be (make-env-credential-backend))
  ;; provider-name "my-provider" → env var Q_MY_PROVIDER_API_KEY
  (putenv "Q_MY_PROVIDER_API_KEY" "derived-key-abc")
  (define cred (backend-load be "my-provider"))
  (check-true (hash? cred))
  (check-equal? (hash-ref cred 'api-key) "derived-key-abc"))

(test-case "audit-env-load-missing"
  (define be (make-env-credential-backend))
  (check-false (backend-load be "nonexistent-provider")))

(test-case "audit-env-store-readonly"
  (define be (make-env-credential-backend))
  (check-exn exn:fail?
             (lambda () (backend-store! be "test" "key"))
             "Env backend should reject store operations"))

(test-case "audit-env-delete-noop"
  (define be (make-env-credential-backend))
  ;; Should not raise
  (backend-delete! be "anything")
  (check-true #t))

(test-case "audit-env-list-empty"
  (define be (make-env-credential-backend))
  (check-equal? (backend-list-providers be) '()))

(test-case "audit-env-available"
  (define be (make-env-credential-backend))
  (check-true (backend-available? be)))

;; ---------------------------------------------------------------------------
;; 4. File Backend
;; ---------------------------------------------------------------------------

(test-case "audit-file-store-and-load"
  (define tmp-dir (make-temporary-file "cred-test-~a" 'directory))
  (define cred-path (build-path tmp-dir "credentials.json"))
  (define be (make-file-credential-backend cred-path))
  (backend-store! be "anthropic" "sk-ant-file-key-999")
  (define cred (backend-load be "anthropic"))
  (check-true (hash? cred))
  (check-equal? (hash-ref cred 'api-key) "sk-ant-file-key-999")
  (check-equal? (hash-ref cred 'source) "file")
  (check-equal? (hash-ref cred 'provider) "anthropic")
  (delete-directory/files tmp-dir #:must-exist? #f))

(test-case "audit-file-load-missing"
  (define tmp-dir (make-temporary-file "cred-test-~a" 'directory))
  (define cred-path (build-path tmp-dir "credentials.json"))
  (define be (make-file-credential-backend cred-path))
  (check-false (backend-load be "nonexistent"))
  (delete-directory/files tmp-dir #:must-exist? #f))

(test-case "audit-file-delete"
  (define tmp-dir (make-temporary-file "cred-test-~a" 'directory))
  (define cred-path (build-path tmp-dir "credentials.json"))
  (define be (make-file-credential-backend cred-path))
  (backend-store! be "openai" "sk-file-key-001")
  (backend-store! be "anthropic" "sk-ant-key-002")
  (backend-delete! be "openai")
  (check-false (backend-load be "openai"))
  (check-true (hash? (backend-load be "anthropic")))
  (delete-directory/files tmp-dir #:must-exist? #f))

(test-case "audit-file-list-providers"
  (define tmp-dir (make-temporary-file "cred-test-~a" 'directory))
  (define cred-path (build-path tmp-dir "credentials.json"))
  (define be (make-file-credential-backend cred-path))
  (backend-store! be "openai" "key1")
  (backend-store! be "anthropic" "key2")
  (define providers (backend-list-providers be))
  (check-equal? (length providers) 2)
  (delete-directory/files tmp-dir #:must-exist? #f))

(test-case "audit-file-overwrite"
  (define tmp-dir (make-temporary-file "cred-test-~a" 'directory))
  (define cred-path (build-path tmp-dir "credentials.json"))
  (define be (make-file-credential-backend cred-path))
  (backend-store! be "openai" "old-key")
  (backend-store! be "openai" "new-key")
  (check-equal? (hash-ref (backend-load be "openai") 'api-key) "new-key")
  (delete-directory/files tmp-dir #:must-exist? #f))

(test-case "audit-file-available-no-file"
  (define tmp-dir (make-temporary-file "cred-test-~a" 'directory))
  (define cred-path (build-path tmp-dir "credentials.json"))
  (define be (make-file-credential-backend cred-path))
  ;; Directory exists, so backend should be available even without file
  (check-true (backend-available? be))
  (delete-directory/files tmp-dir #:must-exist? #f))

(test-case "audit-file-available-existing"
  (define tmp-dir (make-temporary-file "cred-test-~a" 'directory))
  (define cred-path (build-path tmp-dir "credentials.json"))
  (define be (make-file-credential-backend cred-path))
  (backend-store! be "test" "key")
  (check-true (backend-available? be))
  (delete-directory/files tmp-dir #:must-exist? #f))

(test-case "audit-file-persist-across-instances"
  (define tmp-dir (make-temporary-file "cred-test-~a" 'directory))
  (define cred-path (build-path tmp-dir "credentials.json"))
  (define be1 (make-file-credential-backend cred-path))
  (backend-store! be1 "openai" "persisted-key")
  ;; Create a new instance pointing to the same file
  (define be2 (make-file-credential-backend cred-path))
  (define cred (backend-load be2 "openai"))
  (check-true (hash? cred))
  (check-equal? (hash-ref cred 'api-key) "persisted-key")
  (delete-directory/files tmp-dir #:must-exist? #f))

;; ---------------------------------------------------------------------------
;; 5. Chained Backend
;; ---------------------------------------------------------------------------

(test-case "audit-chained-load-from-first"
  (define mem1 (make-memory-credential-backend))
  (define mem2 (make-memory-credential-backend))
  (backend-store! mem1 "openai" "key-from-mem1")
  (backend-store! mem2 "openai" "key-from-mem2")
  (define chained (make-chained-credential-backend (list mem1 mem2)))
  (define cred (backend-load chained "openai"))
  (check-equal? (hash-ref cred 'api-key) "key-from-mem1" "Should return first backend's value"))

(test-case "audit-chained-fallback"
  (define mem1 (make-memory-credential-backend))
  (define mem2 (make-memory-credential-backend))
  (backend-store! mem2 "openai" "key-from-mem2")
  (define chained (make-chained-credential-backend (list mem1 mem2)))
  (define cred (backend-load chained "openai"))
  (check-equal? (hash-ref cred 'api-key) "key-from-mem2" "Should fall back to second backend"))

(test-case "audit-chained-load-missing"
  (define mem1 (make-memory-credential-backend))
  (define mem2 (make-memory-credential-backend))
  (define chained (make-chained-credential-backend (list mem1 mem2)))
  (check-false (backend-load chained "nonexistent")))

(test-case "audit-chained-store-first-writable"
  (define mem1 (make-memory-credential-backend))
  (define mem2 (make-memory-credential-backend))
  (define chained (make-chained-credential-backend (list mem1 mem2)))
  (backend-store! chained "openai" "chained-key")
  (check-equal? (hash-ref (backend-load mem1 "openai") 'api-key) "chained-key")
  (check-false (backend-load mem2 "openai") "Second backend should not have it"))

(test-case "audit-chained-delete-all"
  (define mem1 (make-memory-credential-backend))
  (define mem2 (make-memory-credential-backend))
  (backend-store! mem1 "openai" "key1")
  (backend-store! mem2 "openai" "key2")
  (define chained (make-chained-credential-backend (list mem1 mem2)))
  (backend-delete! chained "openai")
  (check-false (backend-load mem1 "openai"))
  (check-false (backend-load mem2 "openai")))

(test-case "audit-chained-list-merged"
  (define mem1 (make-memory-credential-backend))
  (define mem2 (make-memory-credential-backend))
  (backend-store! mem1 "openai" "key1")
  (backend-store! mem1 "anthropic" "key2")
  (backend-store! mem2 "google" "key3")
  (backend-store! mem2 "openai" "key4")
  (define chained (make-chained-credential-backend (list mem1 mem2)))
  (define providers (backend-list-providers chained))
  ;; Should be deduplicated: openai, anthropic, google = 3
  (check-equal? (length providers) 3))

(test-case "audit-chained-available-any"
  (define mem1 (make-memory-credential-backend))
  (define mem2 (make-memory-credential-backend))
  (define chained (make-chained-credential-backend (list mem1 mem2)))
  (check-true (backend-available? chained)))

(test-case "audit-chained-name"
  (define chained (make-chained-credential-backend (list (make-memory-credential-backend))))
  (check-equal? (backend-name chained) "chained"))

;; ---------------------------------------------------------------------------
;; 6. Policy-Aware Backend
;; ---------------------------------------------------------------------------

(test-case "audit-policy-auto-passthrough"
  (define mem (make-memory-credential-backend))
  (define pa (make-policy-aware-backend mem #:policy 'auto))
  (backend-store! pa "openai" "test-key")
  (check-equal? (hash-ref (backend-load pa "openai") 'api-key) "test-key"))

(test-case "audit-policy-env-only-blocks-file-store"
  (define tmp-dir (make-temporary-file "cred-test-~a" 'directory))
  (define cred-path (build-path tmp-dir "credentials.json"))
  (define file-be (make-file-credential-backend cred-path))
  (define pa (make-policy-aware-backend file-be #:policy 'env-only #:warn-port #f))
  (check-exn exn:fail?
             (lambda () (backend-store! pa "openai" "key"))
             "env-only policy should block file store")
  (delete-directory/files tmp-dir #:must-exist? #f))

(test-case "audit-policy-env-only-blocks-file-load"
  (define tmp-dir (make-temporary-file "cred-test-~a" 'directory))
  (define cred-path (build-path tmp-dir "credentials.json"))
  (define file-be (make-file-credential-backend cred-path))
  ;; First store directly using file backend
  (backend-store! file-be "openai" "stored-key")
  ;; Now policy-aware load should return #f
  (define pa (make-policy-aware-backend file-be #:policy 'env-only #:warn-port #f))
  (check-false (backend-load pa "openai"))
  (delete-directory/files tmp-dir #:must-exist? #f))

(test-case "audit-policy-keychain-required-blocks-file-store"
  (define tmp-dir (make-temporary-file "cred-test-~a" 'directory))
  (define cred-path (build-path tmp-dir "credentials.json"))
  (define file-be (make-file-credential-backend cred-path))
  (define pa (make-policy-aware-backend file-be #:policy 'keychain-required #:warn-port #f))
  (check-exn exn:fail?
             (lambda () (backend-store! pa "openai" "key"))
             "keychain-required policy should block file store")
  (delete-directory/files tmp-dir #:must-exist? #f))

(test-case "audit-policy-env-only-allows-env-backend"
  (define env-be (make-env-credential-backend))
  (putenv "Q_OPENAI_API_KEY" "env-key-12345")
  (define pa (make-policy-aware-backend env-be #:policy 'env-only #:warn-port #f))
  (define cred (backend-load pa "openai"))
  (check-true (hash? cred))
  (check-equal? (hash-ref cred 'api-key) "env-key-12345"))

(test-case "audit-policy-name"
  (define mem (make-memory-credential-backend))
  (define pa (make-policy-aware-backend mem #:policy 'env-only #:warn-port #f))
  (check-true (string-contains? (backend-name pa) "policy-aware"))
  (check-true (string-contains? (backend-name pa) "env-only")))

;; ---------------------------------------------------------------------------
;; 7. Auth Store
;; ---------------------------------------------------------------------------

(test-case "audit-as-credential-struct"
  (define cred (credential "openai" "sk-test12345678" 'environment))
  (check-true (credential? cred))
  (check-equal? (credential-provider-name cred) "openai")
  (check-equal? (credential-api-key cred) "sk-test12345678")
  (check-equal? (credential-source cred) 'environment))

(test-case "audit-as-credential-custom-write"
  (define cred (credential "openai" "sk-test12345678" 'environment))
  (define s (format "~a" cred))
  (check-true (string-contains? s "credential"))
  (check-true (string-contains? s "openai"))
  ;; Should be masked
  (check-false (string-contains? s "sk-test12345678") "Custom write should mask the API key"))

(test-case "audit-as-mask-api-key-long"
  (check-equal? (mask-api-key "sk-ant-api03-xxxxxxxxxxxxxxxxxxxx") "sk-...xxxx")
  ;; key: sk-ant-api03-xxxxxxxxxxxxxxxxxxxx
  ;; prefix = "sk-" (first 3), suffix = "xxxx" (last 4)
  (check-equal? (mask-api-key "12345678") "123...5678"))

(test-case "audit-as-mask-api-key-short"
  (check-equal? (mask-api-key "short") "****")
  (check-equal? (mask-api-key "1234567") "****" "7 chars < 8, should be ****"))

(test-case "audit-as-mask-api-key-non-string"
  (check-equal? (mask-api-key 42) "****")
  (check-equal? (mask-api-key #f) "****"))

(test-case "audit-as-redacted-credential"
  (define cred (credential "anthropic" "sk-ant-api03-abcdef123456" 'config))
  (define red (cred->redacted cred))
  (check-true (redacted-credential? red))
  (check-equal? (redacted-credential-provider-name red) "anthropic")
  (check-true (string? (redacted-credential-masked-api-key red)))
  (check-false (string-contains? (redacted-credential-masked-api-key red) "abcdef123456"))
  (check-equal? (redacted-credential-source red) 'config))

(test-case "audit-as-validate-openai"
  (check-true (validate-credential-format "openai" "sk-validkey123"))
  (check-false (validate-credential-format "openai" "invalid-key")))

(test-case "audit-as-validate-anthropic"
  (check-true (validate-credential-format "anthropic" "sk-ant-validkey123"))
  (check-false (validate-credential-format "anthropic" "sk-invalid")))

(test-case "audit-as-validate-unknown-provider"
  (check-true (validate-credential-format "custom-provider" "any-api-key"))
  (check-true (validate-credential-format "unknown" "somekey")))

(test-case "audit-as-validate-invalid-inputs"
  (check-false (validate-credential-format 42 "key"))
  (check-false (validate-credential-format "openai" #f)))

(test-case "audit-as-lookup-from-config"
  (define cfg (hash 'api-key "sk-config-key-12345"))
  (define cred (lookup-credential "openai" cfg))
  (check-true (credential? cred))
  (check-equal? (credential-api-key cred) "sk-config-key-12345")
  (check-equal? (credential-source cred) 'config))

(test-case "audit-as-lookup-from-env"
  (putenv "TEST_LOOKUP_ENV_KEY" "env-key-67890")
  (define cfg (hash 'api-key-env "TEST_LOOKUP_ENV_KEY"))
  (define cred (lookup-credential "custom" cfg))
  (check-true (credential? cred))
  (check-equal? (credential-api-key cred) "env-key-67890")
  (check-equal? (credential-source cred) 'environment))

(test-case "audit-as-lookup-env-takes-precedence"
  ;; When both env and config have keys, env should win
  (putenv "TEST_PRECEDENCE_KEY" "env-wins")
  (define cfg (hash 'api-key-env "TEST_PRECEDENCE_KEY" 'api-key "config-loses"))
  (define cred (lookup-credential "test" cfg))
  (check-equal? (credential-api-key cred) "env-wins")
  (check-equal? (credential-source cred) 'environment))

(test-case "audit-as-lookup-missing"
  (check-false (lookup-credential "nonexistent" (hash)))
  (check-false (lookup-credential "nonexistent" #f)))

(test-case "audit-as-credential-present"
  (define cfg (hash 'api-key "sk-present123"))
  (check-true (credential-present? "openai" cfg))
  (check-false (credential-present? "missing" (hash))))

(test-case "audit-as-resolve-all"
  (putenv "Q_GOOGLE_API_KEY" "google-key")
  (define providers
    (hash "openai"
          (hash 'api-key "sk-openai123")
          "google"
          (hash 'api-key-env "Q_GOOGLE_API_KEY")
          "missing"
          (hash)))
  (define resolved (resolve-provider-credentials providers))
  (check-true (credential? (hash-ref resolved "openai")))
  (check-true (credential? (hash-ref resolved "google")))
  (check-false (hash-ref resolved "missing")))

(test-case "audit-as-store-credential"
  (define cfg (hash 'api-key "old-key"))
  (define updated (store-credential! "openai" "new-key" #:provider-config cfg))
  (check-equal? (hash-ref updated 'api-key) "new-key"))

(test-case "audit-as-save-and-load-file"
  (define tmp-dir (make-temporary-file "cred-test-~a" 'directory))
  (define cred-path (build-path tmp-dir "credentials.json"))
  (save-credential-file! "openai" "sk-file-key-456" cred-path)
  (define loaded (load-credential-file cred-path))
  (check-true (hash? loaded))
  (define prov-cfg (hash-ref loaded "openai" #f))
  (check-true (hash? prov-cfg))
  (check-equal? (hash-ref prov-cfg 'api-key) "sk-file-key-456")
  (delete-directory/files tmp-dir #:must-exist? #f))

(test-case "audit-as-with-credential-macro"
  (define cfg (hash 'api-key "sk-macro-key"))
  (define result (with-credential "openai" cfg key (string-append "found:" key)))
  (check-equal? result "found:sk-macro-key"))

(test-case "audit-as-with-credential-macro-missing"
  (define result (with-credential "nonexistent" (hash) key (string-append "found:" key)))
  (check-false result))

;; ---------------------------------------------------------------------------
;; 8. OAuth
;; ---------------------------------------------------------------------------

(test-case "audit-oauth-available"
  (check-true (oauth-available?)))

(test-case "audit-oauth-config-struct"
  (define cfg
    (oauth-config "https://auth.example.com/authorize"
                  "https://auth.example.com/token"
                  "client-123"
                  "secret-456"
                  '("openid" "email")
                  8089))
  (check-true (oauth-config? cfg))
  (check-equal? (oauth-config-authorize-url cfg) "https://auth.example.com/authorize")
  (check-equal? (oauth-config-token-url cfg) "https://auth.example.com/token")
  (check-equal? (oauth-config-client-id cfg) "client-123")
  (check-equal? (oauth-config-client-secret cfg) "secret-456")
  (check-equal? (oauth-config-scopes cfg) '("openid" "email"))
  (check-equal? (oauth-config-redirect-port cfg) 8089))

(test-case "audit-oauth-valid-config"
  (define cfg
    (oauth-config "https://auth.example.com/auth"
                  "https://auth.example.com/token"
                  "client-123"
                  "secret-456"
                  '("openid")
                  8089))
  (check-true (valid-oauth-config? cfg)))

(test-case "audit-oauth-config-empty-url-rejected"
  ;; FINDING remediation: valid-oauth-config? rejects empty authorize/token URLs.
  (define missing-authorize (oauth-config "" "https://token.example.com" "client" "secret" '() 8089))
  (define missing-token (oauth-config "https://auth.example.com" "" "client" "secret" '() 8089))
  (check-false (valid-oauth-config? missing-authorize))
  (check-false (valid-oauth-config? missing-token)))

(test-case "audit-oauth-invalid-config-bad-port"
  (define cfg
    (oauth-config "https://auth.example.com" "https://token.example.com" "client" "secret" '() 0))
  (check-false (valid-oauth-config? cfg)))

(test-case "audit-oauth-invalid-config-not-oauth-config"
  (check-false (valid-oauth-config? "not-a-config"))
  (check-false (valid-oauth-config? (hash))))

(test-case "audit-oauth-token-struct"
  (define tok (oauth-token "access-123" "refresh-456" (+ (current-seconds) 3600) "Bearer" "openid"))
  (check-true (oauth-token? tok))
  (check-equal? (oauth-token-access-token tok) "access-123")
  (check-equal? (oauth-token-refresh-token tok) "refresh-456")
  (check-equal? (oauth-token-token-type tok) "Bearer")
  (check-equal? (oauth-token-scope tok) "openid"))

(test-case "audit-oauth-token-not-expired"
  (define tok (oauth-token "access" "refresh" (+ (current-seconds) 3600) "Bearer" "openid"))
  (check-false (oauth-token-expired? tok)))

(test-case "audit-oauth-token-expired"
  (define tok (oauth-token "access" "refresh" (- (current-seconds) 100) "Bearer" "openid"))
  (check-true (oauth-token-expired? tok)))

(test-case "audit-oauth-token-near-expiry-with-margin"
  ;; Token expires in 30 seconds — with default 60s margin, should be "expired"
  (define tok (oauth-token "access" "refresh" (+ (current-seconds) 30) "Bearer" "openid"))
  (check-true (oauth-token-expired? tok)))

(test-case "audit-oauth-authorize-url"
  (define cfg
    (oauth-config "https://auth.example.com/authorize"
                  "https://auth.example.com/token"
                  "client-123"
                  "secret-456"
                  '("openid" "email")
                  8089))
  (define url (oauth-authorize-url cfg "state-xyz"))
  (check-true (string? url))
  (check-true (string-contains? url "client_id=client-123"))
  (check-true (string-contains? url "response_type=code"))
  (check-true (string-contains? url "state=state-xyz"))
  (check-true (string-contains? url "redirect_uri=")))

(test-case "audit-oauth-authorize-url-with-pkce"
  (define cfg
    (oauth-config "https://auth.example.com/authorize"
                  "https://auth.example.com/token"
                  "client-123"
                  "secret-456"
                  '("openid")
                  8089))
  (define url (oauth-authorize-url cfg "state" "challenge-hash"))
  (check-true (string-contains? url "code_challenge=challenge-hash"))
  (check-true (string-contains? url "code_challenge_method=S256")))

(test-case "audit-oauth-token-round-trip"
  (define tok (oauth-token "access-abc" "refresh-def" 1234567890 "Bearer" "openid email"))
  (define jsexpr (oauth-token->jsexpr tok))
  (check-true (hash? jsexpr))
  (define restored (jsexpr->oauth-token jsexpr))
  (check-true (oauth-token? restored))
  (check-equal? (oauth-token-access-token restored) "access-abc")
  (check-equal? (oauth-token-refresh-token restored) "refresh-def")
  (check-equal? (oauth-token-expires-at restored) 1234567890)
  (check-equal? (oauth-token-token-type restored) "Bearer")
  (check-equal? (oauth-token-scope restored) "openid email"))

(test-case "audit-oauth-jsexpr->token-invalid"
  (check-false (jsexpr->oauth-token "not-a-hash"))
  (check-false (jsexpr->oauth-token (hash))))

;; ---------------------------------------------------------------------------
;; 9. Platform Backends (capability detection)
;; ---------------------------------------------------------------------------

(test-case "audit-platform-capabilities"
  (define caps (credential-backend-capabilities))
  (check-true (hash? caps))
  (check-true (hash-has-key? caps 'env))
  (check-true (hash-has-key? caps 'file))
  (check-true (hash-has-key? caps 'memory))
  (check-true (hash-has-key? caps 'keychain-linux))
  (check-true (hash-has-key? caps 'keychain-macos))
  (check-true (hash-has-key? caps 'keychain-windows))
  (check-true (hash-has-key? caps 'platform))
  ;; env and file should always be available
  (check-true (hash-ref caps 'env))
  (check-true (hash-ref caps 'file))
  (check-true (hash-ref caps 'memory))
  ;; platform should be a string
  (check-true (string? (hash-ref caps 'platform))))
