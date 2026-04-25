#lang racket

;; test-credential-backend.rkt — Tests for credential backend abstraction
;;
;; Issue #1291: GAP-02a — Credential backend abstraction layer
;; Issue #1292: GAP-02b — OS keychain integration

(require rackunit
         racket/file
         racket/path
         (only-in "../runtime/credential-backend.rkt"
                  make-file-credential-backend
                  make-env-credential-backend
                  make-memory-credential-backend
                  make-keychain-credential-backend
                  make-chained-credential-backend
                  credential-backend?
                  backend-name
                  backend-store!
                  backend-load
                  backend-delete!
                  backend-list-providers
                  backend-available?))

;; ---------------------------------------------------------------------------
;; Helpers
;; ---------------------------------------------------------------------------

(define (make-tmp-dir)
  (define tmp (make-temporary-file "cred-backend-test-~a"))
  (delete-file tmp)
  (make-directory tmp)
  tmp)

(define (cleanup tmp)
  (when (directory-exists? tmp)
    (delete-directory/files tmp)))

;; ---------------------------------------------------------------------------
;; Tests: credential-backend struct
;; ---------------------------------------------------------------------------

(test-case "credential-backend: struct has required fields"
  (define be (make-memory-credential-backend))
  (check-true (credential-backend? be))
  (check-equal? (backend-name be) "memory"))

;; ---------------------------------------------------------------------------
;; Tests: file backend
;; ---------------------------------------------------------------------------

(test-case "file-backend: store and load roundtrip"
  (define tmp (make-tmp-dir))
  (define cred-path (build-path tmp "credentials.json"))
  (define be (make-file-credential-backend cred-path))
  (check-true (credential-backend? be))
  (check-equal? (backend-name be) "file")
  ;; Store
  (backend-store! be "openai" "sk-test-key-12345678")
  ;; Load
  (define cred (backend-load be "openai"))
  (check-not-false cred)
  (check-equal? (hash-ref cred 'api-key) "sk-test-key-12345678")
  (check-equal? (hash-ref cred 'source) "file")
  (cleanup tmp))

(test-case "file-backend: load returns #f for missing provider"
  (define tmp (make-tmp-dir))
  (define cred-path (build-path tmp "credentials.json"))
  (define be (make-file-credential-backend cred-path))
  (check-false (backend-load be "nonexistent"))
  (cleanup tmp))

(test-case "file-backend: delete removes credential"
  (define tmp (make-tmp-dir))
  (define cred-path (build-path tmp "credentials.json"))
  (define be (make-file-credential-backend cred-path))
  (backend-store! be "openai" "sk-test-key-12345678")
  (check-not-false (backend-load be "openai"))
  (backend-delete! be "openai")
  (check-false (backend-load be "openai"))
  (cleanup tmp))

(test-case "file-backend: list-providers returns stored providers"
  (define tmp (make-tmp-dir))
  (define cred-path (build-path tmp "credentials.json"))
  (define be (make-file-credential-backend cred-path))
  (backend-store! be "openai" "sk-test-1")
  (backend-store! be "anthropic" "sk-ant-test-2")
  (define providers (backend-list-providers be))
  (check-equal? (sort providers string<?) '("anthropic" "openai"))
  (cleanup tmp))

(test-case "file-backend: available? returns #t when path writable"
  (define tmp (make-tmp-dir))
  (define cred-path (build-path tmp "credentials.json"))
  (define be (make-file-credential-backend cred-path))
  (check-true (backend-available? be))
  (cleanup tmp))

(test-case "file-backend: overwrites existing credential"
  (define tmp (make-tmp-dir))
  (define cred-path (build-path tmp "credentials.json"))
  (define be (make-file-credential-backend cred-path))
  (backend-store! be "openai" "old-key")
  (backend-store! be "openai" "new-key")
  (define cred (backend-load be "openai"))
  (check-equal? (hash-ref cred 'api-key) "new-key")
  (cleanup tmp))

;; ---------------------------------------------------------------------------
;; Tests: env backend
;; ---------------------------------------------------------------------------

(test-case "env-backend: load from environment variable"
  (define be (make-env-credential-backend))
  (check-equal? (backend-name be) "env")
  ;; Set env var with guaranteed cleanup (TEST-01)
  (define old-val (environment-variables-ref (current-environment-variables)
                                              #"Q_TEST_PROVIDER_API_KEY"))
  (dynamic-wind
    (lambda () (putenv "Q_TEST_PROVIDER_API_KEY" "sk-env-test-key"))
    (lambda ()
      (define cred (backend-load be "test_provider" #:env-var "Q_TEST_PROVIDER_API_KEY"))
      (check-not-false cred)
      (check-equal? (hash-ref cred 'api-key) "sk-env-test-key")
      (check-equal? (hash-ref cred 'source) "environment"))
    (lambda ()
      (if old-val
          (putenv "Q_TEST_PROVIDER_API_KEY" (bytes->string/utf-8 old-val))
          (putenv "Q_TEST_PROVIDER_API_KEY" "")))))

(test-case "env-backend: load returns #f for unset variable"
  (define be (make-env-credential-backend))
  (check-false (backend-load be "nonexistent" #:env-var "Q_NONEXISTENT_VAR")))

(test-case "env-backend: store! raises error (read-only)"
  (define be (make-env-credential-backend))
  (check-exn exn:fail? (λ () (backend-store! be "openai" "test"))))

(test-case "env-backend: available? always returns #t"
  (define be (make-env-credential-backend))
  (check-true (backend-available? be)))

;; ---------------------------------------------------------------------------
;; Tests: memory backend
;; ---------------------------------------------------------------------------

(test-case "memory-backend: store and load roundtrip"
  (define be (make-memory-credential-backend))
  (backend-store! be "openai" "sk-mem-test")
  (define cred (backend-load be "openai"))
  (check-not-false cred)
  (check-equal? (hash-ref cred 'api-key) "sk-mem-test")
  (check-equal? (hash-ref cred 'source) "memory"))

(test-case "memory-backend: delete removes credential"
  (define be (make-memory-credential-backend))
  (backend-store! be "openai" "sk-mem-test")
  (backend-delete! be "openai")
  (check-false (backend-load be "openai")))

(test-case "memory-backend: list-providers"
  (define be (make-memory-credential-backend))
  (backend-store! be "openai" "sk-1")
  (backend-store! be "anthropic" "sk-2")
  (check-equal? (sort (backend-list-providers be) string<?) '("anthropic" "openai")))

;; ---------------------------------------------------------------------------
;; Tests: chained backend (env → file → memory)
;; ---------------------------------------------------------------------------

(test-case "chained-backend: tries backends in order"
  (define mem (make-memory-credential-backend))
  (define env (make-env-credential-backend))
  (define chained (make-chained-credential-backend (list env mem)))
  (check-equal? (backend-name chained) "chained")
  ;; Store in memory backend
  (backend-store! mem "test_provider" "sk-memory-key")
  ;; env should take priority — use dynamic-wind for cleanup (TEST-01)
  (define old-val (environment-variables-ref (current-environment-variables)
                                              #"Q_TEST_PROVIDER_API_KEY"))
  (dynamic-wind
    (lambda () (putenv "Q_TEST_PROVIDER_API_KEY" "sk-env-key"))
    (lambda ()
      (define cred-env (backend-load chained "test_provider" #:env-var "Q_TEST_PROVIDER_API_KEY"))
      (check-equal? (hash-ref cred-env 'api-key) "sk-env-key")
      ;; Remove env, should fall through to memory
      (putenv "Q_TEST_PROVIDER_API_KEY" "")
      (define cred-mem (backend-load chained "test_provider"))
      (check-equal? (hash-ref cred-mem 'api-key) "sk-memory-key"))
    (lambda ()
      (if old-val
          (putenv "Q_TEST_PROVIDER_API_KEY" (bytes->string/utf-8 old-val))
          (putenv "Q_TEST_PROVIDER_API_KEY" "")))))

(test-case "chained-backend: store! writes to first writable backend"
  (define mem (make-memory-credential-backend))
  (define env (make-env-credential-backend))
  (define chained (make-chained-credential-backend (list env mem)))
  ;; store! should go to first writable (memory, since env is read-only)
  (backend-store! chained "openai" "sk-chained-test")
  (define cred (backend-load mem "openai"))
  (check-equal? (hash-ref cred 'api-key) "sk-chained-test"))

(test-case "chained-backend: available? returns #t if any backend available"
  (define mem (make-memory-credential-backend))
  (define chained (make-chained-credential-backend (list mem)))
  (check-true (backend-available? chained)))

(test-case "chained-backend: list-providers merges from all backends"
  (define mem1 (make-memory-credential-backend))
  (define mem2 (make-memory-credential-backend))
  (backend-store! mem1 "openai" "sk-1")
  (backend-store! mem2 "anthropic" "sk-2")
  (define chained (make-chained-credential-backend (list mem1 mem2)))
  (define providers (backend-list-providers chained))
  (check-equal? (sort providers string<?) '("anthropic" "openai")))

;; ---------------------------------------------------------------------------
;; Tests: keychain backend (unavailable on this machine)
;; ---------------------------------------------------------------------------

(test-case "keychain-backend: struct is valid"
  (define be (make-keychain-credential-backend))
  (check-true (credential-backend? be))
  (check-equal? (backend-name be) "keychain"))

(test-case "keychain-backend: load returns #f when unavailable"
  (define be (make-keychain-credential-backend))
  (when (not (backend-available? be))
    (check-false (backend-load be "test_provider"))))

(test-case "keychain-backend: store! raises when unavailable"
  (define be (make-keychain-credential-backend))
  (when (not (backend-available? be))
    (check-exn exn:fail? (λ () (backend-store! be "test_provider" "sk-test")))))

(test-case "keychain-backend: list returns empty when unavailable"
  (define be (make-keychain-credential-backend))
  (when (not (backend-available? be))
    (check-equal? (backend-list-providers be) '())))
