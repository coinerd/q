#lang racket

;; test-settings.rkt — TDD tests for runtime/settings.rkt
;;
;; Covers:
;;   - merge-settings (pure deep merge, project wins)
;;   - setting-ref (flat key lookup on merged)
;;   - setting-ref* (nested path lookup)
;;   - provider-config / provider-names
;;   - load-global-settings / load-project-settings (temp dirs)
;;   - load-settings (integration: load + merge)
;;   - Graceful handling of missing / malformed config files

(require rackunit
         racket/file
         racket/port
         json
         "../runtime/settings.rkt")

;; ============================================================
;; Helpers
;; ============================================================

(define (make-temp-dir)
  (make-temporary-file "q-settings-test-~a" 'directory))

(define (cleanup-dir dir)
  (with-handlers ([exn:fail? void])
    (delete-directory/files dir #:must-exist? #f)))

(define (write-json-file path data)
  (call-with-output-file path
    (λ (out) (write-json data out))
    #:exists 'replace))

(define sample-global-config
  (hash 'default-provider "openai"
        'default-model "gpt-4o"
        'max-iterations 10
        'session-dir "~/.q/sessions"
        'providers
        (hash 'openai
              (hash 'base-url "https://api.openai.com/v1"
                    'api-key-env "OPENAI_API_KEY"
                    'api-key ""
                    'default-model "gpt-4o"
                    'models (list "gpt-4" "gpt-4o" "gpt-3.5-turbo"))
              'anthropic
              (hash 'base-url "https://api.anthropic.com/v1"
                    'api-key-env "ANTHROPIC_API_KEY"
                    'api-key ""
                    'default-model "claude-3-sonnet"
                    'models (list "claude-3-opus" "claude-3-sonnet" "claude-3-haiku")))))

(define sample-project-config
  (hash 'default-provider "anthropic"
        'default-model "claude-3-opus"
        'providers
        (hash 'anthropic
              (hash 'api-key "sk-test-123"
                    'default-model "claude-3-opus"))))

;; ============================================================
;; 1. merge-settings — pure deep merge
;; ============================================================

(test-case "merge-settings: empty hashes → empty hash"
  (check-equal? (merge-settings (hash) (hash)) (hash)))

(test-case "merge-settings: project overrides global at top level"
  (define global (hash 'a 1 'b 2))
  (define proj  (hash 'b 99))
  (check-equal? (merge-settings global proj) (hash 'a 1 'b 99)))

(test-case "merge-settings: project adds new top-level key"
  (define global (hash 'a 1))
  (define proj  (hash 'c 3))
  (check-equal? (merge-settings global proj) (hash 'a 1 'c 3)))

(test-case "merge-settings: deep merge — nested hash is merged, not replaced"
  (define global (hash 'providers (hash 'openai (hash 'base-url "https://api.openai.com/v1"
                                                     'api-key-env "OPENAI_API_KEY"))))
  (define proj  (hash 'providers (hash 'openai (hash 'api-key "sk-123"))))
  (define result (merge-settings global proj))
  (define openai (hash-ref (hash-ref result 'providers) 'openai))
  (check-equal? (hash-ref openai 'base-url) "https://api.openai.com/v1")
  (check-equal? (hash-ref openai 'api-key) "sk-123")
  (check-equal? (hash-ref openai 'api-key-env) "OPENAI_API_KEY"))

(test-case "merge-settings: deep merge — project adds new provider"
  (define global (hash 'providers (hash 'openai (hash 'base-url "https://api.openai.com/v1"))))
  (define proj  (hash 'providers (hash 'custom (hash 'base-url "http://localhost:8080/v1"))))
  (define result (merge-settings global proj))
  (check-equal? (hash-ref (hash-ref result 'providers) 'openai)
                (hash 'base-url "https://api.openai.com/v1"))
  (check-equal? (hash-ref (hash-ref result 'providers) 'custom)
                (hash 'base-url "http://localhost:8080/v1")))

(test-case "merge-settings: non-hash scalar in project replaces hash in global"
  (define global (hash 'x (hash 'a 1)))
  (define proj  (hash 'x "scalar"))
  (check-equal? (merge-settings global proj) (hash 'x "scalar")))

(test-case "merge-settings: non-hash scalar in global replaced by hash in project"
  (define global (hash 'x "scalar"))
  (define proj  (hash 'x (hash 'a 1)))
  (check-equal? (merge-settings global proj) (hash 'x (hash 'a 1))))

;; ============================================================
;; 2. setting-ref — flat key lookup
;; ============================================================

(test-case "setting-ref: returns value from merged settings"
  (define settings (q-settings sample-global-config (hash) sample-global-config))
  (check-equal? (setting-ref settings 'default-provider) "openai"))

(test-case "setting-ref: returns default for missing key"
  (define settings (q-settings (hash) (hash) (hash)))
  (check-equal? (setting-ref settings 'missing-key) #f)
  (check-equal? (setting-ref settings 'missing-key "fallback") "fallback"))

(test-case "setting-ref: returns project-override value"
  (define merged (merge-settings sample-global-config sample-project-config))
  (define settings (q-settings sample-global-config sample-project-config merged))
  (check-equal? (setting-ref settings 'default-provider) "anthropic"))

;; ============================================================
;; 3. setting-ref* — nested key path
;; ============================================================

(test-case "setting-ref*: walks nested hash with symbol keys"
  (define settings (q-settings sample-global-config (hash) sample-global-config))
  (check-equal? (setting-ref* settings '(providers openai base-url))
                "https://api.openai.com/v1"))

(test-case "setting-ref*: walks nested hash with string keys"
  (define global-str (hash "providers" (hash "openai" (hash "base-url" "https://api.openai.com/v1"))))
  (define settings (q-settings global-str (hash) global-str))
  (check-equal? (setting-ref* settings '("providers" "openai" "base-url"))
                "https://api.openai.com/v1"))

(test-case "setting-ref*: returns default for missing path"
  (define settings (q-settings (hash) (hash) (hash)))
  (check-equal? (setting-ref* settings '(providers nonexistent key)) #f)
  (check-equal? (setting-ref* settings '(providers nonexistent key) "default") "default"))

(test-case "setting-ref*: handles empty key path"
  (define settings (q-settings (hash) (hash) (hash)))
  (check-equal? (setting-ref* settings '()) #f)
  (check-equal? (setting-ref* settings '() "empty") "empty"))

;; ============================================================
;; 4. provider-config / provider-names
;; ============================================================

(test-case "provider-config: returns provider hash"
  (define settings (q-settings sample-global-config (hash) sample-global-config))
  (define openai (provider-config settings 'openai))
  (check-not-false openai)
  (check-equal? (hash-ref openai 'base-url) "https://api.openai.com/v1"))

(test-case "provider-config: returns #f for unknown provider"
  (define settings (q-settings sample-global-config (hash) sample-global-config))
  (check-false (provider-config settings 'nonexistent)))

(test-case "provider-names: lists configured providers"
  (define settings (q-settings sample-global-config (hash) sample-global-config))
  (define names (provider-names settings))
  (check-true (and (member 'openai names) #t))
  (check-true (and (member 'anthropic names) #t))
  (check-equal? (length names) 2))

(test-case "provider-names: returns empty list when no providers"
  (define settings (q-settings (hash) (hash) (hash)))
  (check-equal? (provider-names settings) '()))

(test-case "provider-config: project override sees merged config"
  (define merged (merge-settings sample-global-config sample-project-config))
  (define settings (q-settings sample-global-config sample-project-config merged))
  (define anthropic (provider-config settings 'anthropic))
  (check-equal? (hash-ref anthropic 'api-key) "sk-test-123")
  (check-equal? (hash-ref anthropic 'base-url) "https://api.anthropic.com/v1"))

;; ============================================================
;; 5. load-global-settings — file loading
;; ============================================================

(test-case "load-global-settings: reads config from home-dir/.q/config.json"
  (define tmp (make-temp-dir))
  (define q-dir (build-path tmp ".q"))
  (make-directory q-dir)
  (write-json-file (build-path q-dir "config.json")
                   (hash 'default-provider "test-global"))
  (define result (load-global-settings tmp))
  (check-equal? (hash-ref result 'default-provider) "test-global")
  (cleanup-dir tmp))

(test-case "load-global-settings: returns empty hash when .q dir missing"
  (define tmp (make-temp-dir))
  (define result (load-global-settings tmp))
  (check-equal? result (hash))
  (cleanup-dir tmp))

(test-case "load-global-settings: returns empty hash when config.json missing"
  (define tmp (make-temp-dir))
  (make-directory (build-path tmp ".q"))
  (define result (load-global-settings tmp))
  (check-equal? result (hash))
  (cleanup-dir tmp))

(test-case "load-global-settings: returns empty hash for malformed JSON"
  (define tmp (make-temp-dir))
  (define q-dir (build-path tmp ".q"))
  (make-directory q-dir)
  (call-with-output-file (build-path q-dir "config.json")
    (λ (out) (display "THIS IS NOT {JSON}" out))
    #:exists 'replace)
  (define result (load-global-settings tmp))
  (check-equal? result (hash))
  (cleanup-dir tmp))

(test-case "load-global-settings: returns empty hash for non-object JSON (array)"
  (define tmp (make-temp-dir))
  (define q-dir (build-path tmp ".q"))
  (make-directory q-dir)
  (write-json-file (build-path q-dir "config.json") (list 1 2 3))
  (define result (load-global-settings tmp))
  (check-equal? result (hash))
  (cleanup-dir tmp))

;; ============================================================
;; 6. load-project-settings — file loading
;; ============================================================

(test-case "load-project-settings: reads config from project-dir/.q/config.json"
  (define tmp (make-temp-dir))
  (make-directory (build-path tmp ".q"))
  (write-json-file (build-path tmp ".q" "config.json")
                   (hash 'default-model "project-model"))
  (define result (load-project-settings tmp))
  (check-equal? (hash-ref result 'default-model) "project-model")
  (cleanup-dir tmp))

(test-case "load-project-settings: falls back to .pi/config.json when .q/ missing"
  (define tmp (make-temp-dir))
  (make-directory (build-path tmp ".pi"))
  (write-json-file (build-path tmp ".pi" "config.json")
                   (hash 'default-model "pi-model"))
  (define result (load-project-settings tmp))
  (check-equal? (hash-ref result 'default-model) "pi-model")
  (cleanup-dir tmp))

(test-case "load-project-settings: prefers .q/ over .pi/"
  (define tmp (make-temp-dir))
  (make-directory (build-path tmp ".q"))
  (make-directory (build-path tmp ".pi"))
  (write-json-file (build-path tmp ".q" "config.json")
                   (hash 'source "q-dir"))
  (write-json-file (build-path tmp ".pi" "config.json")
                   (hash 'source "pi-dir"))
  (define result (load-project-settings tmp))
  (check-equal? (hash-ref result 'source) "q-dir")
  (cleanup-dir tmp))

(test-case "load-project-settings: returns empty hash when both .q/ and .pi/ missing"
  (define tmp (make-temp-dir))
  (define result (load-project-settings tmp))
  (check-equal? result (hash))
  (cleanup-dir tmp))

(test-case "load-project-settings: returns empty hash for malformed JSON"
  (define tmp (make-temp-dir))
  (make-directory (build-path tmp ".q"))
  (call-with-output-file (build-path tmp ".q" "config.json")
    (λ (out) (display "{{{broken" out))
    #:exists 'replace)
  (define result (load-project-settings tmp))
  (check-equal? result (hash))
  (cleanup-dir tmp))

;; ============================================================
;; 7. load-settings — integration (load + merge)
;; ============================================================

(test-case "load-settings: returns q-settings with global, project, and merged"
  (define tmp-home (make-temp-dir))
  (define tmp-proj (make-temp-dir))

  ;; Global config
  (make-directory (build-path tmp-home ".q"))
  (write-json-file (build-path tmp-home ".q" "config.json")
                   (hash 'default-provider "openai"
                         'max-iterations 5))

  ;; Project config
  (make-directory (build-path tmp-proj ".q"))
  (write-json-file (build-path tmp-proj ".q" "config.json")
                   (hash 'default-provider "anthropic"
                         'extra-key "project-value"))

  (define settings (load-settings tmp-proj #:home-dir tmp-home))

  ;; Check struct fields
  (check-equal? (hash-ref (q-settings-global settings) 'default-provider) "openai")
  (check-equal? (hash-ref (q-settings-project settings) 'default-provider) "anthropic")

  ;; Merged should have project override + global extras
  (check-equal? (hash-ref (q-settings-merged settings) 'default-provider) "anthropic")
  (check-equal? (hash-ref (q-settings-merged settings) 'max-iterations) 5)
  (check-equal? (hash-ref (q-settings-merged settings) 'extra-key) "project-value")

  (cleanup-dir tmp-home)
  (cleanup-dir tmp-proj))

(test-case "load-settings: works when both configs missing"
  (define tmp-home (make-temp-dir))
  (define tmp-proj (make-temp-dir))
  (define settings (load-settings tmp-proj #:home-dir tmp-home))
  (check-equal? (q-settings-global settings) (hash))
  (check-equal? (q-settings-project settings) (hash))
  (check-equal? (q-settings-merged settings) (hash))
  (cleanup-dir tmp-home)
  (cleanup-dir tmp-proj))

(test-case "load-settings: q-settings is transparent"
  (define s (q-settings (hash 'a 1) (hash) (hash 'a 1)))
  (check-true (q-settings? s))
  (check-equal? (q-settings-global s) (hash 'a 1)))

;; ============================================================
;; 8. default-session-dir / default-project-dir
;; ============================================================

(test-case "default-session-dir returns path under ~/.q/sessions"
  (define dir (default-session-dir))
  (check-true (path? dir))
  (define parts (explode-path dir))
  (check-equal? (last parts) (string->path "sessions"))
  ;; Should contain .q
  (check-not-false (member (string->path ".q") parts)))

(test-case "default-project-dir returns current-directory"
  (define dir (default-project-dir))
  (check-true (path? dir))
  (check-equal? dir (current-directory)))

;; ============================================================
;; 9. session-dir-from-settings / project-dir-from-settings
;; ============================================================

(test-case "session-dir-from-settings returns setting when present"
  (define settings (q-settings (hash 'session-dir "/tmp/q-sessions") (hash)
                                (hash 'session-dir "/tmp/q-sessions")))
  (check-equal? (session-dir-from-settings settings) "/tmp/q-sessions"))

(test-case "session-dir-from-settings falls back to default-session-dir"
  (define settings (q-settings (hash) (hash) (hash)))
  (check-equal? (session-dir-from-settings settings) (default-session-dir)))

(test-case "project-dir-from-settings returns setting when present"
  (define settings (q-settings (hash 'project-dir "/my/project") (hash)
                                (hash 'project-dir "/my/project")))
  (check-equal? (project-dir-from-settings settings) "/my/project"))

(test-case "project-dir-from-settings falls back to default-project-dir"
  (define settings (q-settings (hash) (hash) (hash)))
  (check-equal? (project-dir-from-settings settings) (default-project-dir)))

;; ============================================================
;; 10. load-settings #:config-path
;; ============================================================

(test-case "load-settings: #:config-path uses explicit config file"
  (define tmp-dir (make-temp-dir))
  (define config-path (build-path tmp-dir "custom.json"))
  (write-json-file config-path
                   (hash 'default-provider "custom-provider"
                         'providers
                         (hash 'custom-provider
                               (hash 'base-url "https://custom.example.com/v1"))))
  (define settings (load-settings tmp-dir #:config-path config-path))
  ;; Global should come from the custom config file
  (check-equal? (hash-ref (q-settings-global settings) 'default-provider) "custom-provider")
  ;; Merged should include the custom config
  (check-equal? (hash-ref (q-settings-merged settings) 'default-provider) "custom-provider")
  (cleanup-dir tmp-dir))

(test-case "load-settings: #:config-path #f loads default paths"
  (define tmp-home (make-temp-dir))
  (define tmp-proj (make-temp-dir))
  (make-directory (build-path tmp-home ".q"))
  (write-json-file (build-path tmp-home ".q" "config.json")
                   (hash 'default-provider "home-provider"))
  (define settings (load-settings tmp-proj #:home-dir tmp-home #:config-path #f))
  (check-equal? (hash-ref (q-settings-global settings) 'default-provider) "home-provider")
  (cleanup-dir tmp-home)
  (cleanup-dir tmp-proj))

(test-case "load-settings: #:config-path non-existent file returns empty global"
  (define tmp-dir (make-temp-dir))
  (define config-path (build-path tmp-dir "nonexistent.json"))
  (define settings (load-settings tmp-dir #:config-path config-path))
  (check-equal? (q-settings-global settings) (hash))
  (cleanup-dir tmp-dir))

(test-case "load-settings: #:config-path merges with project settings"
  (define tmp-dir (make-temp-dir))
  (define config-path (build-path tmp-dir "global.json"))
  (write-json-file config-path
                   (hash 'default-model "gpt-4o"
                         'max-iterations 20))
  ;; Project config
  (make-directory (build-path tmp-dir ".q"))
  (write-json-file (build-path tmp-dir ".q" "config.json")
                   (hash 'default-provider "openai"))
  (define settings (load-settings tmp-dir #:config-path config-path))
  ;; Global comes from config-path
  (check-equal? (hash-ref (q-settings-global settings) 'default-model) "gpt-4o")
  ;; Project comes from .q/config.json
  (check-equal? (hash-ref (q-settings-project settings) 'default-provider) "openai")
  ;; Merged has both
  (check-equal? (hash-ref (q-settings-merged settings) 'default-model) "gpt-4o")
  (check-equal? (hash-ref (q-settings-merged settings) 'default-provider) "openai")
  (cleanup-dir tmp-dir))

;; ============================================================
;; 11. parallel-tools-enabled?
;; ============================================================

(test-case "parallel-tools-enabled?: returns #f by default"
  (define settings (q-settings (hash) (hash) (hash)))
  (check-false (parallel-tools-enabled? settings)))

(test-case "parallel-tools-enabled?: returns #t when set in merged"
  (define settings (q-settings (hash 'parallel-tools #t) (hash) (hash 'parallel-tools #t)))
  (check-true (parallel-tools-enabled? settings)))

(test-case "parallel-tools-enabled?: project override wins"
  (define global (hash 'parallel-tools #f))
  (define proj  (hash 'parallel-tools #t))
  (define merged (merge-settings global proj))
  (define settings (q-settings global proj merged))
  (check-true (parallel-tools-enabled? settings)))

(test-case "parallel-tools-enabled?: loaded from file"
  (define tmp-home (make-temp-dir))
  (define tmp-proj (make-temp-dir))
  (make-directory (build-path tmp-home ".q"))
  (write-json-file (build-path tmp-home ".q" "config.json")
                   (hash 'parallel-tools #t))
  (define settings (load-settings tmp-proj #:home-dir tmp-home))
  (check-true (parallel-tools-enabled? settings))
  (cleanup-dir tmp-home)
  (cleanup-dir tmp-proj))
