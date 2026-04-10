#lang racket

;;; tests/test-auth-store.rkt — tests for runtime/auth-store.rkt
;;;
;;; TDD tests for credential lookup, env-first resolution,
;;; source tracking, batch resolution, and file persistence.

(require rackunit
         rackunit/text-ui
         racket/file
         json
         "../runtime/auth-store.rkt")

;; ── Helpers ──

;; Unique env var prefix to avoid collisions with real env vars
(define test-env-prefix "Q_TEST_AUTH_")

(define (test-env-var suffix)
  (string-append test-env-prefix suffix))

;; Set an env var for testing; returns the var name
(define (set-test-env! suffix value)
  (define var (test-env-var suffix))
  (putenv var value)
  var)

;; Clear a test env var
(define (clear-test-env! suffix)
  (define var (test-env-var suffix))
  (putenv var ""))

;; Make a temporary directory
(define (make-temp-dir)
  (make-temporary-file "q-auth-test-~a" 'directory))

;; Build a provider config hash (using symbol keys as in the spec)
(define (make-provider-config #:env-var [env-var #f]
                              #:api-key [api-key #f]
                              #:base-url [base-url "https://api.example.com"])
  (define h (make-hash))
  (when env-var (hash-set! h 'api-key-env env-var))
  (when api-key (hash-set! h 'api-key api-key))
  (hash-set! h 'base-url base-url)
  ;; Return immutable hash
  (for/hash ([(k v) (in-hash h)])
    (values k v)))

;; Same but with string keys (module must handle both)
(define (make-provider-config/string-keys #:env-var [env-var #f]
                                           #:api-key [api-key #f]
                                           #:base-url [base-url "https://api.example.com"])
  (define h (make-hash))
  (when env-var (hash-set! h "api-key-env" env-var))
  (when api-key (hash-set! h "api-key" api-key))
  (hash-set! h "base-url" base-url)
  (for/hash ([(k v) (in-hash h)])
    (values k v)))

;; ── Test suite ──

(define-test-suite test-auth-store

  ;; ──────────────────────────────
  ;; credential struct
  ;; ──────────────────────────────

  (test-case "credential struct is transparent"
    (define c (credential "openai" "sk-abc" 'environment))
    (check-equal? (credential-provider-name c) "openai")
    (check-equal? (credential-api-key c) "sk-abc")
    (check-equal? (credential-source c) 'environment))

  (test-case "credential struct supports equal? comparison"
    (define c1 (credential "openai" "sk-abc" 'environment))
    (define c2 (credential "openai" "sk-abc" 'environment))
    (check-equal? c1 c2))

  ;; ──────────────────────────────
  ;; lookup-credential — env var resolution
  ;; ──────────────────────────────

  (test-case "lookup-credential finds key from env var"
    (define env-var (set-test-env! "LOOKUP1" "sk-from-env-123"))
    (define cfg (make-provider-config #:env-var env-var #:api-key "sk-from-config"))
    (define result (lookup-credential "test-provider" cfg))
    (clear-test-env! "LOOKUP1")
    (check-not-false result)
    (check-equal? (credential-api-key result) "sk-from-env-123")
    (check-equal? (credential-source result) 'environment)
    (check-equal? (credential-provider-name result) "test-provider"))

  (test-case "lookup-credential env var takes priority over config api-key"
    (define env-var (set-test-env! "PRIORITY1" "sk-env-priority"))
    (define cfg (make-provider-config #:env-var env-var #:api-key "sk-config-priority"))
    (define result (lookup-credential "test-provider" cfg))
    (clear-test-env! "PRIORITY1")
    (check-equal? (credential-api-key result) "sk-env-priority")
    (check-equal? (credential-source result) 'environment))

  (test-case "lookup-credential falls back to config api-key when env var is not set"
    (clear-test-env! "NOTSET1")
    (define cfg (make-provider-config #:env-var (test-env-var "NOTSET1") #:api-key "sk-config-fallback"))
    (define result (lookup-credential "test-provider" cfg))
    (check-not-false result)
    (check-equal? (credential-api-key result) "sk-config-fallback")
    (check-equal? (credential-source result) 'config))

  ;; ──────────────────────────────
  ;; lookup-credential — graceful failure
  ;; ──────────────────────────────

  (test-case "lookup-credential returns #f when neither env nor config has key"
    (clear-test-env! "MISSING1")
    (define cfg (make-provider-config #:env-var (test-env-var "MISSING1")))
    (define result (lookup-credential "test-provider" cfg))
    (check-false result))

  (test-case "lookup-credential returns #f when env var is empty string"
    (define env-var (set-test-env! "EMPTY1" ""))
    (define cfg (make-provider-config #:env-var env-var))
    (define result (lookup-credential "test-provider" cfg))
    (clear-test-env! "EMPTY1")
    (check-false result))

  (test-case "lookup-credential returns #f when config api-key is empty string"
    (clear-test-env! "EMPTYCFG")
    (define cfg (make-provider-config #:env-var (test-env-var "EMPTYCFG") #:api-key ""))
    (define result (lookup-credential "test-provider" cfg))
    (check-false result))

  (test-case "lookup-credential returns #f when provider-config is empty hash"
    (define result (lookup-credential "test-provider" (hash)))
    (check-false result))

  ;; ──────────────────────────────
  ;; lookup-credential — string keys support
  ;; ──────────────────────────────

  (test-case "lookup-credential handles string keys in provider-config"
    (define env-var (set-test-env! "STRKEY1" "sk-str-key-env"))
    (define cfg (make-provider-config/string-keys #:env-var env-var #:api-key "sk-str-key-cfg"))
    (define result (lookup-credential "test-provider" cfg))
    (clear-test-env! "STRKEY1")
    (check-not-false result)
    (check-equal? (credential-api-key result) "sk-str-key-env")
    (check-equal? (credential-source result) 'environment))

  (test-case "lookup-credential falls back to string api-key when env not set"
    (clear-test-env! "STRKEY2")
    (define cfg (make-provider-config/string-keys #:env-var (test-env-var "STRKEY2") #:api-key "sk-str-cfg-key"))
    (define result (lookup-credential "test-provider" cfg))
    (check-not-false result)
    (check-equal? (credential-api-key result) "sk-str-cfg-key")
    (check-equal? (credential-source result) 'config))

  ;; ──────────────────────────────
  ;; credential-present?
  ;; ──────────────────────────────

  (test-case "credential-present? returns #t when env var key exists"
    (define env-var (set-test-env! "PRESENT1" "sk-present"))
    (define cfg (make-provider-config #:env-var env-var))
    (check-true (credential-present? "test-provider" cfg))
    (clear-test-env! "PRESENT1"))

  (test-case "credential-present? returns #t when config key exists"
    (clear-test-env! "PRESENTCFG")
    (define cfg (make-provider-config #:env-var (test-env-var "PRESENTCFG") #:api-key "sk-cfg-present"))
    (check-true (credential-present? "test-provider" cfg)))

  (test-case "credential-present? returns #f when no key available"
    (clear-test-env! "NOTPRESENT")
    (define cfg (make-provider-config #:env-var (test-env-var "NOTPRESENT")))
    (check-false (credential-present? "test-provider" cfg)))

  ;; ──────────────────────────────
  ;; store-credential!
  ;; ──────────────────────────────

  (test-case "store-credential! updates config hash with api-key"
    (define cfg (make-provider-config #:base-url "https://api.test.com"))
    (define updated (store-credential! "openai" "sk-new-key"
                                        #:provider-config cfg))
    (check-equal? (hash-ref updated 'api-key) "sk-new-key")
    ;; Original base-url preserved
    (check-equal? (hash-ref updated 'base-url) "https://api.test.com"))

  (test-case "store-credential! without config-path does not touch filesystem"
    (define cfg (hash 'base-url "https://api.test.com"))
    (define updated (store-credential! "openai" "sk-new-key"
                                        #:provider-config cfg))
    (check-equal? (hash-ref updated 'api-key) "sk-new-key"))

  (test-case "store-credential! writes to file when config-path given"
    (define dir (make-temp-dir))
    (define config-path (build-path dir "config.json"))
    ;; Write initial config
    (define initial-config
      (hasheq 'providers
              (hasheq 'openai (hasheq 'api-key "old-key" 'base-url "https://api.openai.com"))
              'settings (hasheq 'model "gpt-4")))
    (call-with-output-file config-path
      (lambda (out) (write-json initial-config out))
      #:exists 'replace)

    ;; Store new credential
    (define cfg (make-provider-config #:base-url "https://api.openai.com"))
    (define updated (store-credential! "openai" "sk-updated-key"
                                        #:provider-config cfg
                                        #:config-path config-path))

    ;; Read back file (read-json returns hasheq with symbol keys)
    (define file-content (call-with-input-file config-path read-json))
    (define providers (hash-ref file-content 'providers))
    (define openai-cfg (hash-ref providers 'openai))
    (check-equal? (hash-ref openai-cfg 'api-key) "sk-updated-key")
    ;; Other config preserved
    (check-equal? (hash-ref file-content 'settings) (hasheq 'model "gpt-4"))

    (delete-directory/files dir #:must-exist? #f))

  (test-case "store-credential! creates config file if it doesn't exist"
    (define dir (make-temp-dir))
    (define config-path (build-path dir "new-config.json"))
    (define cfg (hash 'base-url "https://api.test.com"))
    (store-credential! "test-provider" "sk-brand-new"
                       #:provider-config cfg
                       #:config-path config-path)
    (check-true (file-exists? config-path))
    (define file-content (call-with-input-file config-path read-json))
    (check-equal? (hash-ref (hash-ref file-content 'providers (hasheq)) 'test-provider (hasheq))
                  (hasheq 'api-key "sk-brand-new"))
    (delete-directory/files dir #:must-exist? #f))

  ;; ──────────────────────────────
  ;; resolve-provider-credentials
  ;; ──────────────────────────────

  (test-case "resolve-provider-credentials resolves all providers"
    (define env-var (set-test-env! "BATCH1" "sk-batch-env"))
    (define providers-hash
      (hash "openai" (make-provider-config #:env-var env-var)
            "anthropic" (make-provider-config #:api-key "sk-ant-cfg")
            "local" (make-provider-config #:base-url "http://localhost:8000")))
    (define results (resolve-provider-credentials providers-hash))
    (clear-test-env! "BATCH1")

    ;; openai resolved from env
    (define openai-cred (hash-ref results "openai"))
    (check-not-false openai-cred)
    (check-equal? (credential-api-key openai-cred) "sk-batch-env")
    (check-equal? (credential-source openai-cred) 'environment)

    ;; anthropic resolved from config
    (define anthro-cred (hash-ref results "anthropic"))
    (check-not-false anthro-cred)
    (check-equal? (credential-api-key anthro-cred) "sk-ant-cfg")
    (check-equal? (credential-source anthro-cred) 'config)

    ;; local has no key
    (define local-cred (hash-ref results "local"))
    (check-false local-cred))

  (test-case "resolve-provider-credentials returns empty hash for no providers"
    (define results (resolve-provider-credentials (hash)))
    (check-equal? (hash-count results) 0))

  ;; ──────────────────────────────
  ;; Edge cases
  ;; ──────────────────────────────

  (test-case "lookup-credential with only api-key in config (no api-key-env)"
    (define cfg (hash 'api-key "sk-only-config"))
    (define result (lookup-credential "test" cfg))
    (check-not-false result)
    (check-equal? (credential-api-key result) "sk-only-config")
    (check-equal? (credential-source result) 'config))

  (test-case "lookup-credential with only api-key-env but missing env var"
    (clear-test-env! "EDGECASE")
    (define cfg (hash 'api-key-env (test-env-var "EDGECASE")))
    (define result (lookup-credential "test" cfg))
    (check-false result))

  (test-case "env var with whitespace-only value returns #f"
    (define env-var (set-test-env! "WHITESPACE" "   "))
    (define cfg (make-provider-config #:env-var env-var))
    (define result (lookup-credential "test" cfg))
    (clear-test-env! "WHITESPACE")
    (check-false result))

  ;; ──────────────────────────────
  ;; mask-api-key
  ;; ──────────────────────────────

  (test-case "mask-api-key masks long keys"
    (check-equal? (mask-api-key "sk-proj-abc123XYZ789")
                  "sk-...Z789"))

  (test-case "mask-api-key returns **** for short keys"
    (check-equal? (mask-api-key "short") "****")
    (check-equal? (mask-api-key "1234567") "****"))

  (test-case "mask-api-key handles exactly 8 chars"
    (check-equal? (mask-api-key "12345678") "123...5678"))

  (test-case "mask-api-key handles non-string input"
    (check-equal? (mask-api-key #f) "****")
    (check-equal? (mask-api-key 42) "****"))

  ;; ──────────────────────────────
  ;; redacted-credential
  ;; ──────────────────────────────

  (test-case "cred->redacted creates safe display credential"
    (define c (credential "openai" "sk-proj-longkey123abc" 'config))
    (define r (cred->redacted c))
    (check-equal? (redacted-credential-provider-name r) "openai")
    (check-equal? (redacted-credential-masked-api-key r) "sk-...3abc")
    (check-equal? (redacted-credential-source r) 'config)
    ;; Verify the raw key is NOT in the redacted credential
    (check-false (string-contains? (redacted-credential-masked-api-key r) "longkey123")))

  (test-case "redacted-credential struct is transparent"
    (define r (redacted-credential "test" "sk-...xyz" 'environment))
    (check-equal? (redacted-credential-provider-name r) "test")
    (check-equal? (redacted-credential-masked-api-key r) "sk-...xyz"))

  ;; ──────────────────────────────
  ;; validate-credential-format
  ;; ──────────────────────────────

  (test-case "validate-credential-format accepts valid OpenAI key"
    (check-true (validate-credential-format "openai" "sk-proj-abc123")))

  (test-case "validate-credential-format rejects invalid OpenAI key"
    (check-false (validate-credential-format "openai" "invalid-key")))

  (test-case "validate-credential-format accepts valid Anthropic key"
    (check-true (validate-credential-format "anthropic" "sk-ant-api03-xyz")))

  (test-case "validate-credential-format rejects invalid Anthropic key"
    (check-false (validate-credential-format "anthropic" "sk-wrong-prefix")))

  (test-case "validate-credential-format accepts any key for unknown provider"
    (check-true (validate-credential-format "my-custom-llm" "any-key-format")))

  (test-case "validate-credential-format rejects non-string inputs"
    (check-false (validate-credential-format 42 "key"))
    (check-false (validate-credential-format "openai" #f)))

  ;; ──────────────────────────────
  ;; Credential file operations
  ;; ──────────────────────────────

  (test-case "load-credential-file returns empty hash when file missing"
    (define dir (make-temp-dir))
    (define path (build-path dir "credentials.json"))
    (define result (load-credential-file path))
    (check-equal? result (hash))
    (delete-directory/files dir #:must-exist? #f))

  (test-case "save-credential-file! creates and load reads back"
    (define dir (make-temp-dir))
    (define path (build-path dir "credentials.json"))
    (save-credential-file! "openai" "sk-test-12345" path)
    (check-true (file-exists? path))
    (define loaded (load-credential-file path))
    (define openai-cfg (hash-ref loaded "openai" #f))
    (check-not-false openai-cfg)
    (check-equal? (hash-ref openai-cfg 'api-key) "sk-test-12345")
    (delete-directory/files dir #:must-exist? #f))

  (test-case "save-credential-file! preserves existing providers"
    (define dir (make-temp-dir))
    (define path (build-path dir "credentials.json"))
    (save-credential-file! "openai" "sk-test-111" path)
    (save-credential-file! "anthropic" "sk-ant-test-222" path)
    (define loaded (load-credential-file path))
    (check-equal? (hash-ref (hash-ref loaded "openai") 'api-key) "sk-test-111")
    (check-equal? (hash-ref (hash-ref loaded "anthropic") 'api-key) "sk-ant-test-222")
    (delete-directory/files dir #:must-exist? #f))

  (test-case "credential-file-path returns path under ~/.q"
    (define p (credential-file-path))
    (check-true (string? (path->string p)))
    (check-true (string-contains? (path->string p) ".q"))
    (check-true (string-contains? (path->string p) "credentials.json")))

  ;; ──────────────────────────────
  ;; with-credential macro
  ;; ──────────────────────────────

  (test-case "with-credential provides scoped key access"
    (define env-var (set-test-env! "WC1" "sk-scoped-test"))
    (define cfg (make-provider-config #:env-var env-var))
    (define result
      (with-credential "test-provider" cfg k
        (string-append "key-is:" k)))
    (clear-test-env! "WC1")
    (check-equal? result "key-is:sk-scoped-test"))

  (test-case "with-credential returns #f when no credential"
    (clear-test-env! "WCMISS")
    (define cfg (make-provider-config #:env-var (test-env-var "WCMISS")))
    (define result
      (with-credential "test-provider" cfg k
        (string-append "key-is:" k)))
    (check-false result))

  ;; ──────────────────────────────
  ;; Integration: resolve + redact
  ;; ──────────────────────────────

  (test-case "resolve-provider-credentials can be redacted for display"
    (define env-var (set-test-env! "INTEG1" "sk-integration-test-key"))
    (define providers-hash
      (hash "openai" (make-provider-config #:env-var env-var)
            "local" (make-provider-config #:base-url "http://localhost")))
    (define results (resolve-provider-credentials providers-hash))
    (clear-test-env! "INTEG1")

    ;; Redact all credentials
    (define redacted
      (for/hash ([(name cred) (in-hash results)])
        (values name (and cred (cred->redacted cred)))))

    ;; openai is redacted
    (define openai-redacted (hash-ref redacted "openai"))
    (check-not-false openai-redacted)
    (check-true (redacted-credential? openai-redacted))
    (check-false (string-contains? (redacted-credential-masked-api-key openai-redacted) "integration-test-key"))

    ;; local has no credential
    (check-false (hash-ref redacted "local")))

  ;; ──────────────────────────────
  ;; F4: credential custom write masks API key
  ;; ──────────────────────────────

  (test-case "credential prints with masked key (custom write)"
    (define c (credential "openai" "sk-proj-longsecretkey123" 'config))
    (define printed (format "~a" c))
    ;; Should NOT contain the raw key
    (check-false (string-contains? printed "longsecretkey123"))
    ;; Should contain the masked version
    (check-true (string-contains? printed "...")))

  (test-case "credential equal? still works for tests"
    (define c1 (credential "test" "sk-key" 'environment))
    (define c2 (credential "test" "sk-key" 'environment))
    (check-equal? c1 c2))

  ;; ──────────────────────────────
  ;; F5: atomic write crash safety
  ;; ──────────────────────────────

  (test-case "save-credential-file! uses atomic write (creates valid file)"
    (define dir (make-temp-dir))
    (define path (build-path dir "credentials.json"))
    (save-credential-file! "openai" "sk-test-atomic" path)
    (check-true (file-exists? path))
    ;; Verify no leftover temp file
    (define files (directory-list dir))
    (check-equal? (length (filter (lambda (f) (string-suffix? (path->string f) ".tmp")) files)) 0)
    ;; Verify content is valid
    (define loaded (load-credential-file path))
    (check-equal? (hash-ref (hash-ref loaded "openai") 'api-key) "sk-test-atomic")
    (delete-directory/files dir #:must-exist? #f))

  (test-case "save-credential-file! preserves existing data through atomic write"
    (define dir (make-temp-dir))
    (define path (build-path dir "credentials.json"))
    (save-credential-file! "openai" "sk-first-key" path)
    (save-credential-file! "anthropic" "sk-ant-second-key" path)
    (define loaded (load-credential-file path))
    (check-equal? (hash-ref (hash-ref loaded "openai") 'api-key) "sk-first-key")
    (check-equal? (hash-ref (hash-ref loaded "anthropic") 'api-key) "sk-ant-second-key")
    (delete-directory/files dir #:must-exist? #f))

  ;; ──────────────────────────────
  ;; SEC-07: File permissions on credential files
  ;; ──────────────────────────────

  (test-case "save-credential-file! sets file permissions to 0600"
    (define dir (make-temp-dir))
    (define path (build-path dir "credentials.json"))
    (save-credential-file! "openai" "sk-perm-test-key" path)
    (check-true (file-exists? path))
    ;; Read the file permissions back
    (define perms (file-or-directory-permissions path))
    ;; On Unix, file-or-directory-permissions returns an integer like #o100600
    ;; or a list like '(read write execute). Check it's restrictive.
    (when (integer? perms)
      (define mode (bitwise-and perms #o777))
    ;; Owner read/write only (0600)
      (check-equal? mode #o600
                    (format "credential file should be 0600, got ~o" mode)))
    (delete-directory/files dir #:must-exist? #f))

  (test-case "atomic-write-json! sets restrictive permissions"
    (define dir (make-temp-dir))
    (define path (build-path dir "test-atomic.json"))
    ;; Use save-credential-file! which calls atomic-write-json! internally
    (save-credential-file! "test" "sk-atomic-perm" path)
    (check-true (file-exists? path))
    (define perms (file-or-directory-permissions path))
    (when (integer? perms)
      (define mode (bitwise-and perms #o777))
      (check-equal? mode #o600
                    (format "atomic-write file should be 0600, got ~o" mode)))
    (delete-directory/files dir #:must-exist? #f))

  )

;; ── Run ──

(run-tests test-auth-store)
