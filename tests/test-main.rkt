#lang racket

;; q/tests/test-main.rkt — tests for main.rkt entry point

(require rackunit
         racket/port
         racket/file
         json
         "../main.rkt"
         (only-in "../extensions/api.rkt"
                  extension-registry?
                  list-extensions))

(define (capture-output thunk)
  (with-output-to-string thunk))

(define (make-temp-project-dir)
  (make-temporary-file "q-test-~a" 'directory))

(define (make-temp-project-with-config config-hash)
  (define dir (make-temp-project-dir))
  (define q-dir (build-path dir ".q"))
  (make-directory* q-dir)
  (call-with-output-file (build-path q-dir "config.json")
    (lambda (out)
      (write-json config-hash out)))
  dir)

(define (cleanup-temp-dir dir)
  (when (and dir (directory-exists? dir))
    (delete-directory/files dir #:must-exist? #f)))

;; ============================================================
;; register-default-tools!
;; ============================================================

(test-case "registers all 9 built-in tools"
  (define reg (make-tool-registry))
  (register-default-tools! reg)
  (check-equal? (length (list-tools reg)) 9))

(test-case "each tool is a valid tool struct with executable"
  (define reg (make-tool-registry))
  (register-default-tools! reg)
  (for ([name (in-list '("read" "write" "edit" "bash"))])
    (define t (lookup-tool reg name))
    (check-pred tool? t)
    (check-true (procedure? (tool-execute t)))))

(test-case "registry has correct count"
  (define reg (make-tool-registry))
  (register-default-tools! reg)
  (check-equal? (length (list-tools reg)) 9))

;; ============================================================
;; build-runtime-from-cli
;; ============================================================

(test-case "returns a hash with required keys"
  (define cfg (parse-cli-args #()))
  (define rt (build-runtime-from-cli cfg))
  (check-pred hash? rt)
  (check-true (hash-has-key? rt 'provider))
  (check-true (hash-has-key? rt 'tool-registry))
  (check-true (hash-has-key? rt 'event-bus))
  (check-true (hash-has-key? rt 'max-iterations)))

(test-case "provider is valid when config exists (may be real or mock depending on config)"
  (define cfg (parse-cli-args #()))
  (define rt (build-runtime-from-cli cfg))
  (check-true (provider? (hash-ref rt 'provider)))
  ;; Provider name depends on whether ~/.q/config.json exists and has valid config
  ;; Could be "mock" (no config) or "openai-compatible" (config with credentials or local provider)
  (check-true (not (false? (member (provider-name (hash-ref rt 'provider)) '("mock" "openai-compatible"))))))

(test-case "tool-registry has all 9 built-in tools"
  (define cfg (parse-cli-args #()))
  (define rt (build-runtime-from-cli cfg))
  (define reg (hash-ref rt 'tool-registry))
  (check-equal? (length (list-tools reg)) 9))

(test-case "event-bus is valid"
  (define cfg (parse-cli-args #()))
  (define rt (build-runtime-from-cli cfg))
  (check-true (event-bus? (hash-ref rt 'event-bus))))

(test-case "max-iterations from cli-config"
  (define cfg (parse-cli-args #("--max-turns" "5")))
  (define rt (build-runtime-from-cli cfg))
  (check-equal? (hash-ref rt 'max-iterations) 5))

(test-case "session-id from cli-config"
  (define cfg (parse-cli-args #("--session" "abc123")))
  (define rt (build-runtime-from-cli cfg))
  (check-equal? (hash-ref rt 'session-id) "abc123"))

(test-case "cli-config->runtime-config uses project-dir key for --project-dir"
  (define cfg (parse-cli-args #("--project-dir" "/tmp/myproject")))
  (define rt (cli-config->runtime-config cfg))
  (check-true (hash-has-key? rt 'project-dir))
  (check-false (hash-has-key? rt 'session-dir))
  (check-equal? (hash-ref rt 'project-dir) "/tmp/myproject"))

(test-case "no-tools flag propagated"
  (define cfg (parse-cli-args #("--no-tools")))
  (define rt (build-runtime-from-cli cfg))
  (check-true (hash-ref rt 'no-tools?)))

(test-case "default session-dir is provided"
  (define cfg (parse-cli-args #()))
  (define rt (build-runtime-from-cli cfg))
  (check-true (hash-has-key? rt 'session-dir))
  (check-true (path-string? (hash-ref rt 'session-dir))))

;; ============================================================
;; mode-for-config
;; ============================================================

(test-case "default (no args) -> interactive"
  (define cfg (parse-cli-args #()))
  (check-equal? (mode-for-config cfg) 'interactive))

(test-case "prompt -> single"
  (define cfg (parse-cli-args #("hello")))
  (check-equal? (mode-for-config cfg) 'single))

(test-case "--json -> json"
  (define cfg (parse-cli-args #("--json")))
  (check-equal? (mode-for-config cfg) 'json))

(test-case "--rpc -> rpc"
  (define cfg (parse-cli-args #("--rpc")))
  (check-equal? (mode-for-config cfg) 'rpc))

(test-case "help command -> help"
  (define cfg (parse-cli-args #("--help")))
  (check-equal? (mode-for-config cfg) 'help))

(test-case "version command -> version"
  (define cfg (parse-cli-args #("--version")))
  (check-equal? (mode-for-config cfg) 'version))

(test-case "--session -> resume (interactive)"
  (define cfg (parse-cli-args #("--session" "s1")))
  (check-equal? (mode-for-config cfg) 'interactive))

;; ============================================================
;; build-provider
;; ============================================================

(test-case "build-provider: returns valid provider when no config"
  (define p (build-provider (hasheq) (load-settings)))
  (check-pred provider? p)
  ;; Provider depends on whether ~/.q/config.json exists
  (check-true (not (false? (member (provider-name p) '("mock" "openai-compatible"))))))

(test-case "build-provider: returns valid provider with empty hash config"
  (define p (build-provider (make-hash) (load-settings)))
  (check-pred provider? p)
  ;; Provider depends on whether ~/.q/config.json exists
  (check-true (not (false? (member (provider-name p) '("mock" "openai-compatible"))))))

(test-case "build-provider: falls back to mock when no config exists anywhere"
  (define tmp-dir (make-temp-project-dir))
  ;; Use #:config-path with non-existent file to bypass global config
  (dynamic-wind
    (lambda () (void))
    (lambda ()
      (define config (make-hash (list (cons 'project-dir tmp-dir))))
      (define settings (load-settings tmp-dir #:config-path "/nonexistent/config.json"))
      (define p (build-provider config settings))
      (check-pred provider? p)
      (check-equal? (provider-name p) "mock"))
    (lambda () (cleanup-temp-dir tmp-dir))))

(test-case "build-provider: falls back to mock when config has no providers"
  (define tmp-dir
    (make-temp-project-with-config
     (hasheq 'default-model "gpt-4o")))
  (dynamic-wind
    (lambda () (void))
    (lambda ()
      (define config (make-hash (list (cons 'project-dir tmp-dir))))
      (define settings (load-settings tmp-dir))
      (define p (build-provider config settings))
      (check-pred provider? p)
      (check-equal? (provider-name p) "mock"))
    (lambda () (cleanup-temp-dir tmp-dir))))

(test-case "build-provider: falls back to mock when model not resolved"
  (define tmp-dir
    (make-temp-project-with-config
     (hasheq 'providers
             (hasheq 'openai
                     (hasheq 'base-url "https://api.openai.com/v1"
                             'models '("gpt-4o")
                             'api-key-env "NONEXISTENT_KEY_FOR_TEST_12345")))))
  (dynamic-wind
    (lambda () (void))
    (lambda ()
      (define config
        (make-hash
         (list (cons 'project-dir tmp-dir)
               (cons 'model "nonexistent-model"))))
      (define settings (load-settings tmp-dir))
      (define p (build-provider config settings))
      (check-pred provider? p)
      (check-equal? (provider-name p) "mock"))
    (lambda () (cleanup-temp-dir tmp-dir))))

(test-case "build-provider: falls back to mock when no credentials found"
  (define tmp-dir
    (make-temp-project-with-config
     (hasheq 'providers
             (hasheq 'openai
                     (hasheq 'base-url "https://api.openai.com/v1"
                             'models '("gpt-4o")
                             'api-key-env "Q_TEST_NONEXISTENT_KEY_XYZ")))))
  (dynamic-wind
    (lambda () (void))
    (lambda ()
      (define config
        (make-hash
         (list (cons 'project-dir tmp-dir)
               (cons 'model "gpt-4o"))))
      (define settings (load-settings tmp-dir))
      (define p (build-provider config settings))
      (check-pred provider? p)
      (check-equal? (provider-name p) "mock"))
    (lambda () (cleanup-temp-dir tmp-dir))))

(test-case "build-provider: creates real provider when config and credentials exist"
  (define tmp-dir
    (make-temp-project-with-config
     (hasheq 'providers
             (hasheq 'openai
                     (hasheq 'base-url "https://api.openai.com/v1"
                             'models '("gpt-4o")
                             'api-key-env "Q_TEST_PROVIDER_BUILD_KEY")))))
  (putenv "Q_TEST_PROVIDER_BUILD_KEY" "sk-test-key-12345")
  (dynamic-wind
    (lambda () (void))
    (lambda ()
      (define config
        (make-hash
         (list (cons 'project-dir tmp-dir)
               (cons 'model "gpt-4o"))))
      (define settings (load-settings tmp-dir))
      (define p (build-provider config settings))
      (check-pred provider? p)
      (check-equal? (provider-name p) "openai-compatible"))
    (lambda ()
      (cleanup-temp-dir tmp-dir)
      (putenv "Q_TEST_PROVIDER_BUILD_KEY" ""))))

(test-case "build-provider: creates real provider with default model from config"
  (define tmp-dir
    (make-temp-project-with-config
     (hasheq 'default-model "gpt-4o"
             'providers
             (hasheq 'openai
                     (hasheq 'base-url "https://api.openai.com/v1"
                             'models '("gpt-4o" "gpt-4o-mini")
                             'default-model "gpt-4o"
                             'api-key-env "Q_TEST_DEFAULT_MODEL_KEY")))))
  (putenv "Q_TEST_DEFAULT_MODEL_KEY" "sk-test-key-default")
  (dynamic-wind
    (lambda () (void))
    (lambda ()
      (define config
        (make-hash
         (list (cons 'project-dir tmp-dir))))
      (define settings (load-settings tmp-dir))
      (define p (build-provider config settings))
      (check-pred provider? p)
      (check-equal? (provider-name p) "openai-compatible"))
    (lambda ()
      (cleanup-temp-dir tmp-dir)
      (putenv "Q_TEST_DEFAULT_MODEL_KEY" ""))))

(test-case "build-provider: uses project-dir from config for settings loading"
  (define tmp-dir
    (make-temp-project-with-config
     (hasheq 'providers
             (hasheq 'myprovider
                     (hasheq 'base-url "https://example.com/v1"
                             'models '("claude-3")
                             'api-key-env "Q_TEST_PROJECT_DIR_KEY")))))
  (putenv "Q_TEST_PROJECT_DIR_KEY" "sk-test-project-dir")
  (dynamic-wind
    (lambda () (void))
    (lambda ()
      (define config
        (make-hash
         (list (cons 'project-dir tmp-dir)
               (cons 'model "claude-3"))))
      (define settings (load-settings tmp-dir))
      (define p (build-provider config settings))
      (check-pred provider? p)
      (check-equal? (provider-name p) "openai-compatible"))
    (lambda ()
      (cleanup-temp-dir tmp-dir)
      (putenv "Q_TEST_PROJECT_DIR_KEY" ""))))

;; ============================================================
;; print helpers
;; ============================================================

(test-case "print-usage outputs text containing Usage:"
  (define out (capture-output (lambda () (print-usage))))
  (check-true (string-contains? out "Usage: q")))

(test-case "print-version outputs version string"
  (define out (capture-output (lambda () (print-version))))
  (check-true (string-contains? out "q version")))

;; ============================================================
;; build-runtime-from-cli: extension-registry
;; ============================================================

(test-case "build-runtime-from-cli: includes extension-registry key"
  (define cfg (parse-cli-args #()))
  (define rt (build-runtime-from-cli cfg))
  (check-true (hash-has-key? rt 'extension-registry)
              "runtime config should have extension-registry key"))

(test-case "build-runtime-from-cli: extension-registry is an extension-registry?"
  (define cfg (parse-cli-args #()))
  (define rt (build-runtime-from-cli cfg))
  (check-pred extension-registry? (hash-ref rt 'extension-registry)
              "extension-registry value should satisfy extension-registry?"))

(test-case "build-runtime-from-cli: extension-registry starts empty"
  (define cfg (parse-cli-args #()))
  (define rt (build-runtime-from-cli cfg))
  (define ext-reg (hash-ref rt 'extension-registry))
  (check-equal? (length (list-extensions ext-reg)) 0
                "extension-registry should start with no extensions"))

;; ============================================================
;; build-runtime-from-cli: system-instructions (resource loading)
;; ============================================================

(test-case "build-runtime-from-cli: system-instructions empty when no .q dir"
  (define cfg (parse-cli-args #()))
  (define rt (build-runtime-from-cli cfg))
  (check-true (hash-has-key? rt 'system-instructions))
  (check-equal? (hash-ref rt 'system-instructions) '()
                "system-instructions should be empty when no .q dir exists"))

(test-case "build-runtime-from-cli: system-instructions loaded from .q/instructions.md"
  (define tmp-dir (make-temp-project-dir))
  (define q-dir (build-path tmp-dir ".q"))
  (make-directory* q-dir)
  (call-with-output-file (build-path q-dir "instructions.md")
    (lambda (out) (displayln "You are a helpful assistant." out)))
  (dynamic-wind
    (lambda () (void))
    (lambda ()
      (define cfg (parse-cli-args (vector "--project-dir" (path->string tmp-dir))))
      (define rt (build-runtime-from-cli cfg))
      (check-true (hash-has-key? rt 'system-instructions))
      (define instrs (hash-ref rt 'system-instructions))
      (check-true (and (list? instrs) (= (length instrs) 1))
                  "should have one instruction from instructions.md")
      (when (and (list? instrs) (not (null? instrs)))
        (check-true (string-contains? (first instrs) "helpful assistant")
                    "instruction should contain text from instructions.md")))
    (lambda () (cleanup-temp-dir tmp-dir))))

;; ============================================================
;; build-runtime-from-cli: model-name
;; ============================================================

(test-case "build-runtime-from-cli: includes model-name key when --model given"
  (define cfg (parse-cli-args #("--model" "gpt-4o")))
  (define rt (build-runtime-from-cli cfg))
  (check-true (hash-has-key? rt 'model-name)
              "runtime config should have model-name key")
  (check-equal? (hash-ref rt 'model-name) "gpt-4o"
                "model-name should match --model flag"))

(test-case "build-runtime-from-cli: model-name resolves from config when --model not given"
  (define cfg (parse-cli-args #()))
  (define rt (build-runtime-from-cli cfg))
  (check-true (hash-has-key? rt 'model-name))
  ;; When no --model flag, model-name is resolved from config default-model
  ;; (may be a string like "glm-5.1" or #f if no config/default found)
  (check-true (or (string? (hash-ref rt 'model-name))
                  (eq? (hash-ref rt 'model-name) #f))
              "model-name should be resolved from config or #f"))

;; ============================================================
;; mode-for-config: --tui
;; ============================================================

(test-case "mode-for-config: --tui → tui mode"
  (define cfg (parse-cli-args #("--tui")))
  (check-equal? (mode-for-config cfg) 'tui))

;; ============================================================
;; parse-cli-args: --tool accumulation
;; ============================================================

(test-case "parse-cli-args: --tool accumulates multiple tools"
  (define cfg (parse-cli-args #("--tool" "read" "--tool" "bash")))
  (check-equal? (sort (cli-config-tools cfg) string<?) '("bash" "read")))

(test-case "parse-cli-args: --tool with single tool"
  (define cfg (parse-cli-args #("--tool" "edit")))
  (check-equal? (cli-config-tools cfg) '("edit")))

(test-case "parse-cli-args: --tool with --no-tools"
  (define cfg (parse-cli-args #("--no-tools" "--tool" "read")))
  (check-pred cli-config-no-tools? cfg)
  (check-equal? (cli-config-tools cfg) '("read")))

;; ============================================================
;; cli-config->runtime-config: propagation tests
;; ============================================================

(test-case "cli-config->runtime-config: propagates model"
  (define cfg (parse-cli-args #("--model" "claude-3")))
  (define rt (cli-config->runtime-config cfg))
  (check-equal? (hash-ref rt 'model) "claude-3"))

(test-case "cli-config->runtime-config: propagates session-id"
  (define cfg (parse-cli-args #("--session" "s1")))
  (define rt (cli-config->runtime-config cfg))
  (check-equal? (hash-ref rt 'session-id) "s1"))

(test-case "cli-config->runtime-config: propagates config-path"
  (define cfg (parse-cli-args #("--config" "/tmp/cfg.json")))
  (define rt (cli-config->runtime-config cfg))
  (check-equal? (hash-ref rt 'config-path) "/tmp/cfg.json"))

(test-case "cli-config->runtime-config: omits model when not set"
  (define cfg (parse-cli-args #()))
  (define rt (cli-config->runtime-config cfg))
  (check-false (hash-has-key? rt 'model)))

(test-case "cli-config->runtime-config: omits session-id when not set"
  (define cfg (parse-cli-args #()))
  (define rt (cli-config->runtime-config cfg))
  (check-false (hash-has-key? rt 'session-id)))

(test-case "cli-config->runtime-config: always includes max-iterations"
  (define cfg (parse-cli-args #()))
  (define rt (cli-config->runtime-config cfg))
  (check-true (hash-has-key? rt 'max-iterations))
  (check-equal? (hash-ref rt 'max-iterations) 10))

(test-case "cli-config->runtime-config: propagates tools list"
  (define cfg (parse-cli-args #("--tool" "read" "--tool" "bash")))
  (define rt (cli-config->runtime-config cfg))
  (check-true (hash-has-key? rt 'tools))
  (check-equal? (sort (hash-ref rt 'tools) string<?) '("bash" "read")))

;; ============================================================
;; build-runtime-from-cli: multiple flags combined
;; ============================================================

(test-case "build-runtime-from-cli with multiple flags combined"
  (define cfg (parse-cli-args #("--model" "gpt-4" "--max-turns" "5" "--verbose")))
  (define rt (build-runtime-from-cli cfg))
  (check-equal? (hash-ref rt 'model-name) "gpt-4")
  (check-equal? (hash-ref rt 'max-iterations) 5)
  (check-true (hash-ref rt 'verbose? #f))
  (check-true (hash-has-key? rt 'provider))
  (check-true (hash-has-key? rt 'session-dir))
  (check-true (hash-has-key? rt 'system-instructions)))

;; ============================================================
;; build-runtime-from-cli: empty extension dir
;; ============================================================

(test-case "build-runtime-from-cli: empty extension dir does not crash"
  (define tmp-dir (make-temp-project-dir))
  (define q-ext-dir (build-path tmp-dir ".q" "extensions"))
  (make-directory* q-ext-dir)
  (dynamic-wind
    (lambda () (void))
    (lambda ()
      (define cfg (parse-cli-args (vector "--project-dir" (path->string tmp-dir))))
      (define rt (build-runtime-from-cli cfg))
      (check-true (hash-has-key? rt 'extension-registry))
      (define ext-reg (hash-ref rt 'extension-registry))
      (check-equal? (length (list-extensions ext-reg)) 0))
    (lambda () (cleanup-temp-dir tmp-dir))))

(test-case "build-runtime-from-cli: nonexistent extension dir is safe"
  (define cfg (parse-cli-args #()))
  (define rt (build-runtime-from-cli cfg))
  (check-true (hash-has-key? rt 'extension-registry))
  (define ext-reg (hash-ref rt 'extension-registry))
  (check-equal? (length (list-extensions ext-reg)) 0
                "extension-registry should be empty when no extension dirs exist"))

;; ============================================================
;; build-runtime-from-cli: integration with real config
;; ============================================================

(test-case "build-runtime-from-cli: integration with real config"
  (define tmp-dir
    (make-temp-project-with-config
     (hasheq 'providers
             (hasheq 'openai
                     (hasheq 'base-url "https://api.openai.com/v1"
                             'models '("gpt-4o")
                             'api-key-env "Q_TEST_INTEGRATION_BUILD_KEY")))))
  (putenv "Q_TEST_INTEGRATION_BUILD_KEY" "sk-test-integration-key")
  (dynamic-wind
    (lambda () (void))
    (lambda ()
      (define cfg (parse-cli-args (vector "--project-dir" (path->string tmp-dir)
                                          "--model" "gpt-4o")))
      (define rt (build-runtime-from-cli cfg))
      (check-true (hash-has-key? rt 'provider))
      (check-true (provider? (hash-ref rt 'provider)))
      (check-equal? (provider-name (hash-ref rt 'provider)) "openai-compatible")
      (check-equal? (hash-ref rt 'model-name) "gpt-4o")
      (check-true (hash-has-key? rt 'session-dir))
      (check-true (hash-has-key? rt 'tool-registry))
      (check-true (hash-has-key? rt 'extension-registry))
      (check-true (hash-has-key? rt 'system-instructions)))
    (lambda ()
      (cleanup-temp-dir tmp-dir)
      (putenv "Q_TEST_INTEGRATION_BUILD_KEY" ""))))

(test-case "build-runtime-from-cli: integration with --no-tools and real config"
  (define tmp-dir
    (make-temp-project-with-config
     (hasheq 'providers
             (hasheq 'openai
                     (hasheq 'base-url "https://api.openai.com/v1"
                             'models '("gpt-4o")
                             'api-key-env "Q_TEST_NO_TOOLS_KEY")))))
  (putenv "Q_TEST_NO_TOOLS_KEY" "sk-test-no-tools")
  (dynamic-wind
    (lambda () (void))
    (lambda ()
      (define cfg (parse-cli-args (vector "--project-dir" (path->string tmp-dir)
                                          "--model" "gpt-4o"
                                          "--no-tools")))
      (define rt (build-runtime-from-cli cfg))
      (check-true (hash-ref rt 'no-tools?))
      (define reg (hash-ref rt 'tool-registry))
      (check-equal? (length (list-tools reg)) 0
                    "no tools should be registered with --no-tools"))
    (lambda ()
      (cleanup-temp-dir tmp-dir)
      (putenv "Q_TEST_NO_TOOLS_KEY" ""))))

;; ============================================================
;; build-runtime-from-cli: --tool filters registered tools
;; ============================================================

(test-case "build-runtime-from-cli: --tool limits registered tools"
  (define cfg (parse-cli-args #("--tool" "read" "--tool" "bash")))
  (define rt (build-runtime-from-cli cfg))
  (define reg (hash-ref rt 'tool-registry))
  (define names (sort (list-tools reg) string<? #:key tool-name))
  (check-equal? (map tool-name names) '("bash" "read")))

;; ============================================================
;; run-cli-interactive tests
;; ============================================================

(test-case "run-cli-interactive: processes prompts through session-fn"
  (define captured '())
  (define cfg (parse-cli-args #()))
  (define in (open-input-string "hello\n/quit\n"))
  (define out (open-output-string))
  (run-cli-interactive cfg
    #:session-fn (lambda (prompt) (set! captured (cons prompt captured)))
    #:in in
    #:out out)
  (check-equal? (reverse captured) '("hello")))

(test-case "run-cli-interactive: handles /quit command"
  (define cfg (parse-cli-args #()))
  (define in (open-input-string "/quit\n"))
  (define out (open-output-string))
  (run-cli-interactive cfg
    #:session-fn (lambda (prompt) (void))
    #:in in
    #:out out)
  (check-true (string-contains? (get-output-string out) "Goodbye.")))

(test-case "run-cli-interactive: handles /exit command"
  (define cfg (parse-cli-args #()))
  (define in (open-input-string "/exit\n"))
  (define out (open-output-string))
  (run-cli-interactive cfg
    #:session-fn (lambda (prompt) (void))
    #:in in
    #:out out)
  (check-true (string-contains? (get-output-string out) "Goodbye.")))

(test-case "run-cli-interactive: handles /help command"
  (define cfg (parse-cli-args #()))
  (define in (open-input-string "/help\n/quit\n"))
  (define out (open-output-string))
  (run-cli-interactive cfg
    #:session-fn (lambda (prompt) (void))
    #:in in
    #:out out)
  (check-true (string-contains? (get-output-string out) "Usage: q")))

(test-case "run-cli-interactive: empty lines are ignored"
  (define captured '())
  (define cfg (parse-cli-args #()))
  (define in (open-input-string "\n\n\n/quit\n"))
  (define out (open-output-string))
  (run-cli-interactive cfg
    #:session-fn (lambda (prompt) (set! captured (cons prompt captured)))
    #:in in
    #:out out)
  (check-equal? captured '()))

(test-case "run-cli-interactive: multiple prompts in sequence"
  (define captured '())
  (define cfg (parse-cli-args #()))
  (define in (open-input-string "first\nsecond\nthird\n/quit\n"))
  (define out (open-output-string))
  (run-cli-interactive cfg
    #:session-fn (lambda (prompt) (set! captured (cons prompt captured)))
    #:in in
    #:out out)
  (check-equal? (reverse captured) '("first" "second" "third")))

(test-case "run-cli-interactive: handles session-fn errors gracefully"
  (define cfg (parse-cli-args #()))
  (define in (open-input-string "hello\n/quit\n"))
  (define out (open-output-string))
  ;; session-fn raises an error — should be caught and printed, not crash
  (run-cli-interactive cfg
    #:session-fn (lambda (prompt) (error 'test "intentional error"))
    #:in in
    #:out out)
  (check-true (string-contains? (get-output-string out) "Error:")))

(test-case "run-cli-interactive: displays q> prompt"
  (define cfg (parse-cli-args #()))
  (define in (open-input-string "/quit\n"))
  (define out (open-output-string))
  (run-cli-interactive cfg
    #:session-fn (lambda (prompt) (void))
    #:in in
    #:out out)
  (check-true (string-contains? (get-output-string out) "q> ")))

(test-case "run-cli-interactive: handles EOF"
  (define captured '())
  (define cfg (parse-cli-args #()))
  (define in (open-input-string "hello"))
  (define out (open-output-string))
  (run-cli-interactive cfg
    #:session-fn (lambda (prompt) (set! captured (cons prompt captured)))
    #:in in
    #:out out)
  ;; "hello" without newline: read-line returns "hello", then next read returns eof
  (check-true (string-contains? (get-output-string out) "Goodbye.")))

;; ============================================================
;; run-cli-single tests
;; ============================================================

(test-case "run-cli-single: calls session-fn with prompt"
  (define captured '())
  (define cfg (parse-cli-args #("hello world")))
  (define out (open-output-string))
  (run-cli-single cfg
    #:session-fn (lambda (prompt) (set! captured (cons prompt captured)))
    #:out out)
  (check-equal? (reverse captured) '("hello world")))

(test-case "run-cli-single: does nothing when no prompt"
  (define captured '())
  (define cfg (parse-cli-args #()))
  (define out (open-output-string))
  (run-cli-single cfg
    #:session-fn (lambda (prompt) (set! captured (cons prompt captured)))
    #:out out)
  (check-equal? captured '()))

(test-case "run-cli-single: handles session-fn errors gracefully"
  (define cfg (parse-cli-args #("hello")))
  (define out (open-output-string))
  (run-cli-single cfg
    #:session-fn (lambda (prompt) (error 'test "intentional error"))
    #:out out)
  (check-true (string-contains? (get-output-string out) "Error:")))

;; ============================================================
;; build-runtime-from-cli: session-dir with --project-dir
;; ============================================================

(test-case "build-runtime-from-cli: session-dir present with --project-dir"
  (define tmp-dir (make-temp-project-dir))
  (dynamic-wind
    (lambda () (void))
    (lambda ()
      (define cfg (parse-cli-args (vector "--project-dir" (path->string tmp-dir))))
      (define rt (build-runtime-from-cli cfg))
      (check-true (hash-has-key? rt 'session-dir))
      (check-true (path-string? (hash-ref rt 'session-dir))))
    (lambda () (cleanup-temp-dir tmp-dir))))

;; ============================================================
;; register-default-tools!: #:only filter
;; ============================================================

(test-case "register-default-tools!: #:only with unknown tool name registers nothing extra"
  (define reg (make-tool-registry))
  (register-default-tools! reg #:only '("nonexistent"))
  (check-equal? (length (list-tools reg)) 0
                "unknown tool name should not register anything"))

;; ============================================================
;; build-runtime-from-cli: verbose? flag
;; ============================================================

(test-case "build-runtime-from-cli: verbose? flag propagated"
  (define cfg (parse-cli-args #("--verbose")))
  (define rt (build-runtime-from-cli cfg))
  (check-true (hash-ref rt 'verbose?)))

(test-case "build-runtime-from-cli: verbose? defaults to #f"
  (check-false (hash-ref (build-runtime-from-cli (parse-cli-args #())) 'verbose?)))

;; ============================================================
;; register-default-tools! #:only filtering
;; ============================================================

(test-case "register-default-tools! #:only filters tools"
  (define reg (make-tool-registry))
  (register-default-tools! reg #:only '("read" "bash"))
  (check-equal? (sort (tool-names reg) string<?) '("bash" "read")))

(test-case "register-default-tools! #:only #f registers all"
  (define reg (make-tool-registry))
  (register-default-tools! reg #:only #f)
  (check-equal? (length (tool-names reg)) 9))

(test-case "register-default-tools! #:only empty list registers none"
  (define reg (make-tool-registry))
  (register-default-tools! reg #:only '())
  (check-equal? (length (tool-names reg)) 0))

(test-case "build-runtime-from-cli: --tool flag filters registered tools"
  (define cfg (parse-cli-args #("--tool" "read")))
  (define rt (build-runtime-from-cli cfg))
  (check-equal? (tool-names (hash-ref rt 'tool-registry)) '("read")))

;; ============================================================
;; build-provider: --config path overrides global settings
;; ============================================================

(test-case "build-provider: --config path overrides global settings"
  (define tmp-dir (make-temp-project-dir))
  (define config-path (build-path tmp-dir "custom-config.json"))
  (call-with-output-file config-path
    (lambda (out)
      (write-json
       (hasheq 'providers
               (hasheq 'openai
                       (hasheq 'base-url "https://api.openai.com/v1"
                               'models '("gpt-4o")
                               'api-key-env "Q_TEST_CONFIG_PATH_KEY")))
       out)))
  (putenv "Q_TEST_CONFIG_PATH_KEY" "sk-test-config-path")
  (dynamic-wind
    (lambda () (void))
    (lambda ()
      (define cfg (parse-cli-args (vector "--config" (path->string config-path)
                                           "--model" "gpt-4o"
                                           "--project-dir" (path->string tmp-dir))))
      (define rt-config (cli-config->runtime-config cfg))
      (define settings (load-settings tmp-dir #:config-path config-path))
      (define p (build-provider rt-config settings))
      (check-pred provider? p)
      (check-equal? (provider-name p) "openai-compatible"))
    (lambda ()
      (cleanup-temp-dir tmp-dir)
      (putenv "Q_TEST_CONFIG_PATH_KEY" ""))))

;; ============================================================
;; build-provider: local providers without API keys (BUG-02 fix)
;; ============================================================

(test-case "build-provider: local provider (localhost) works without API key"
  (define tmp-dir
    (make-temp-project-with-config
     (hasheq 'providers
             (hasheq 'local
                     (hasheq 'base-url "http://127.0.0.1:8080/v1"
                             'models '("gemma-4-e4b-it"))))))
  (dynamic-wind
    (lambda () (void))
    (lambda ()
      (define config
        (make-hash
         (list (cons 'project-dir tmp-dir)
               (cons 'model "gemma-4-e4b-it"))))
      (define settings (load-settings tmp-dir))
      (define p (build-provider config settings))
      ;; Should NOT fall back to mock provider
      (check-pred provider? p)
      (check-equal? (provider-name p) "openai-compatible"))
    (lambda () (cleanup-temp-dir tmp-dir))))

(test-case "build-provider: local provider (localhost hostname) works without API key"
  (define tmp-dir
    (make-temp-project-with-config
     (hasheq 'providers
             (hasheq 'local
                     (hasheq 'base-url "http://localhost:8080/v1"
                             'models '("llama-3-8b"))))))
  (dynamic-wind
    (lambda () (void))
    (lambda ()
      (define config
        (make-hash
         (list (cons 'project-dir tmp-dir)
               (cons 'model "llama-3-8b"))))
      (define settings (load-settings tmp-dir))
      (define p (build-provider config settings))
      ;; Should NOT fall back to mock provider
      (check-pred provider? p)
      (check-equal? (provider-name p) "openai-compatible"))
    (lambda () (cleanup-temp-dir tmp-dir))))

(test-case "build-provider: local provider (192.168.x.x) works without API key"
  (define tmp-dir
    (make-temp-project-with-config
     (hasheq 'providers
             (hasheq 'local
                     (hasheq 'base-url "http://192.168.1.100:8080/v1"
                             'models '("mistral-7b"))))))
  (dynamic-wind
    (lambda () (void))
    (lambda ()
      (define config
        (make-hash
         (list (cons 'project-dir tmp-dir)
               (cons 'model "mistral-7b"))))
      (define settings (load-settings tmp-dir))
      (define p (build-provider config settings))
      ;; Should NOT fall back to mock provider
      (check-pred provider? p)
      (check-equal? (provider-name p) "openai-compatible"))
    (lambda () (cleanup-temp-dir tmp-dir))))

(test-case "build-provider: cloud provider still requires API key"
  (define tmp-dir
    (make-temp-project-with-config
     (hasheq 'providers
             (hasheq 'openai
                     (hasheq 'base-url "https://api.openai.com/v1"
                             'models '("gpt-4o")
                             'api-key-env "Q_TEST_CLOUD_NO_KEY")))))
  ;; Ensure env var is NOT set
  (putenv "Q_TEST_CLOUD_NO_KEY" "")
  (dynamic-wind
    (lambda () (void))
    (lambda ()
      (define config
        (make-hash
         (list (cons 'project-dir tmp-dir)
               (cons 'model "gpt-4o"))))
      (define settings (load-settings tmp-dir))
      (define p (build-provider config settings))
      ;; Should fall back to mock provider for cloud without API key
      (check-pred provider? p)
      (check-equal? (provider-name p) "mock"))
    (lambda () (cleanup-temp-dir tmp-dir))))
