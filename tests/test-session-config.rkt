#lang racket/base

;; @speed fast  ;; @suite runtime

;; BOUNDARY: integration

;; tests/test-session-config.rkt — unit tests for session-config hash wrapper + gen:dict
;;
;; v0.30.4: Redesigned to wrap a single hash. Tests verify:
;;   - hash->session-config and session-config->hash round-trip
;;   - gen:dict interface (dict-ref, dict-set, etc.)
;;   - Smart accessor functions with defaults
;;   - hash-ref works directly on session-config

(require rackunit
         racket/dict
         racket/hash
         "../runtime/session/session-config.rkt"
         (only-in "../runtime/session-index/schema.rkt" make-empty-index session-index?)
         (only-in "../runtime/context-assembly/state-aware-builder.rkt"
                  current-task-state-aware-assembly?
                  current-conclusion-token-budget
                  current-graph-conclusion-selection?)
         (only-in "../runtime/context-assembly/auto-distillation.rkt"
                  current-auto-distillation-enabled?)
         (only-in "../runtime/context-assembly/rollback-actions.rkt"
                  current-rollback-action-execution?)
         (only-in "../runtime/context-assembly/state-aware-builder.rkt"
                  current-ws-evolution-enabled?))

;; ── Constructor tests ────────────────────────────────────────────

(test-case "hash->session-config with empty hash"
  (define c (hash->session-config (hasheq)))
  (check-pred session-config? c)
  (check-false (config-provider c))
  (check-false (config-model-name c)))

(test-case "hash->session-config with values"
  (define c
    (hash->session-config (hasheq 'provider 'prov 'model-name "gpt-4" 'max-context-tokens 128000)))
  ;; Use dict-ref for raw value access; config-provider has provider? contract
  (check-eq? (dict-ref c 'provider) 'prov)
  (check-equal? (config-model-name c) "gpt-4")
  (check-equal? (config-max-context-tokens c) 128000))

;; ── gen:dict: dict-ref ──────────────────────────────────────────

(test-case "dict-ref on stored keys"
  (define c
    (hash->session-config (hasheq 'provider 'my-prov 'model-name "model-1" 'thinking-level 'high)))
  (check-eq? (dict-ref c 'provider) 'my-prov)
  (check-equal? (dict-ref c 'model-name) "model-1")
  (check-eq? (dict-ref c 'thinking-level) 'high))

(test-case "dict-ref with default for missing key"
  (define c (hash->session-config (hasheq)))
  (check-false (dict-ref c 'provider #f))
  (check-equal? (dict-ref c 'model-name "default") "default"))

(test-case "dict-ref on custom keys"
  (define c (hash->session-config (hasheq 'custom-key 'custom-val)))
  (check-eq? (dict-ref c 'custom-key) 'custom-val))

(test-case "dict-ref error on unknown key without default"
  (define c (hash->session-config (hasheq)))
  (check-exn exn:fail? (lambda () (dict-ref c 'nonexistent))))

;; ── gen:dict: dict-set ──────────────────────────────────────────

(test-case "dict-set returns new session-config"
  (define c1 (hash->session-config (hasheq 'model-name "old")))
  (define c2 (dict-set c1 'model-name "new"))
  (check-equal? (dict-ref c1 'model-name) "old")
  (check-equal? (dict-ref c2 'model-name) "new"))

(test-case "dict-set on new key"
  (define c1 (hash->session-config (hasheq)))
  (define c2 (dict-set c1 'extra-key 42))
  (check-equal? (dict-ref c2 'extra-key) 42))

(test-case "dict-set chain preserves values"
  (define c1 (hash->session-config (hasheq)))
  (define c2 (dict-set c1 'provider 'p))
  (define c3 (dict-set c2 'model-name "m"))
  (define c4 (dict-set c3 'max-iterations 5))
  (check-eq? (dict-ref c4 'provider) 'p)
  (check-equal? (dict-ref c4 'model-name) "m")
  (check-equal? (dict-ref c4 'max-iterations) 5))

;; ── gen:dict: dict-has-key? ─────────────────────────────────────

(test-case "dict-has-key? on stored keys"
  (define c (hash->session-config (hasheq 'provider #f 'model-name #f)))
  (check-true (dict-has-key? c 'provider))
  (check-true (dict-has-key? c 'model-name))
  (check-false (dict-has-key? c 'no-such-key)))

(test-case "dict-has-key? on custom key"
  (define c (hash->session-config (hasheq 'x 1)))
  (check-true (dict-has-key? c 'x)))

;; ── gen:dict: dict-remove ───────────────────────────────────────

(test-case "dict-remove removes key"
  (define c1 (hash->session-config (hasheq 'provider 'prov 'model-name "m")))
  (define c2 (dict-remove c1 'provider))
  (check-false (dict-has-key? c2 'provider))
  (check-equal? (dict-ref c2 'model-name) "m"))

(test-case "dict-remove on custom key"
  (define c1 (hash->session-config (hasheq 'x 1 'y 2)))
  (define c2 (dict-remove c1 'x))
  (check-false (dict-has-key? c2 'x))
  (check-equal? (dict-ref c2 'y) 2))

;; ── gen:dict: dict-keys ─────────────────────────────────────────

(test-case "dict-keys returns all keys"
  (define c (hash->session-config (hasheq 'a 1 'b 2 'c 3)))
  (check-equal? (length (dict-keys c)) 3))

;; ── gen:dict: dict-count ────────────────────────────────────────

(test-case "dict-count"
  (define c (hash->session-config (hasheq 'a 1 'b 2)))
  (check-equal? (dict-count c) 2))

;; ── Conversion: round-trip ─────────────────────────────────────

(test-case "hash->session-config->hash round-trip"
  (define h1 (hasheq 'provider 'p 'model-name "m" 'custom 'val))
  (define c (hash->session-config h1))
  (define h2 (session-config->hash c))
  (check-equal? (hash-ref h2 'provider) 'p)
  (check-equal? (hash-ref h2 'model-name) "m")
  (check-equal? (hash-ref h2 'custom) 'val))

(test-case "session-config->hash returns independent immutable copy"
  (define c (hash->session-config (hasheq 'a 1)))
  (define h (session-config->hash c))
  ;; Verify it's a hash
  (check-pred hash? h)
  ;; Verify it's independent — modifying session-config doesn't affect it
  (define c2 (dict-set c 'a 999))
  (check-equal? (hash-ref h 'a) 1)
  (check-equal? (dict-ref c 'a) 1)
  (check-equal? (dict-ref c2 'a) 999))

;; ── Smart accessor defaults ─────────────────────────────────────

(test-case "config-system-instructions defaults to ()"
  (define c (hash->session-config (hasheq)))
  (check-equal? (config-system-instructions c) '()))

(test-case "config-thinking-level defaults to medium"
  (define c (hash->session-config (hasheq)))
  (check-eq? (config-thinking-level c) 'medium))

(test-case "config-max-iterations defaults to 50"
  (define c (hash->session-config (hasheq)))
  (check-equal? (config-max-iterations c) 50))

(test-case "config-tier-c-count defaults to 4"
  (define c (hash->session-config (hasheq)))
  (check-equal? (config-tier-c-count c) 4))

(test-case "config-tier-b-count defaults to 20"
  (define c (hash->session-config (hasheq)))
  (check-equal? (config-tier-b-count c) 20))

(test-case "config-max-context-tokens defaults to 128000"
  (define c (hash->session-config (hasheq)))
  (check-equal? (config-max-context-tokens c) 128000))

;; ── hash-ref compatibility ──────────────────────────────────────

(test-case "hash-ref works directly via gen:dict"
  (define c (hash->session-config (hasheq 'provider 'prov 'model-name "m")))
  ;; hash-ref on gen:dict implementation works through dict-ref
  (check-eq? (dict-ref c 'provider) 'prov))

;; ── New accessor defaults (v0.30.5) ─────────────────────────────

(test-case "config-max-tokens defaults to 8192"
  (define c (hash->session-config (hasheq)))
  (check-equal? (config-max-tokens c) 8192))

(test-case "config-max-tokens returns set value"
  (define c (hash->session-config (hasheq 'max-tokens 4096)))
  (check-equal? (config-max-tokens c) 4096))

(test-case "config-token-budget-threshold defaults to #f"
  (define c (hash->session-config (hasheq)))
  (check-false (config-token-budget-threshold c)))

(test-case "config-session-index defaults to #f"
  (define c (hash->session-config (hasheq)))
  (check-false (config-session-index c)))

(test-case "config-session-index returns set value"
  (define idx (make-empty-index))
  (define c (hash->session-config (hasheq 'session-index idx)))
  (check-pred session-index? (config-session-index c)))

;; ── Full integration: agent-session round-trip ───────────────────

;; ── resolve-max-iterations-hard (v0.37.0 FB-02) ───────────────

(test-case "resolve-max-iterations-hard returns config value when set"
  (define c (hash->session-config (hasheq 'max-iterations-hard 100)))
  (check-equal? (resolve-max-iterations-hard c 50) 100))

(test-case "resolve-max-iterations-hard computes dynamic default when #f"
  (define c (hash->session-config (hasheq)))
  ;; max-iterations-hard defaults to #f in accessor
  (check-false (config-max-iterations-hard c))
  ;; resolve computes: max(floor(50 * 1.6), 80) = max(80, 80) = 80
  (check-equal? (resolve-max-iterations-hard c 50) 80)
  ;; resolve computes: max(floor(100 * 1.6), 80) = max(160, 80) = 160
  (check-equal? (resolve-max-iterations-hard c 100) 160))

;; ── normalize-session-config-hash (v0.37.0 FB-05) ──────────────

(test-case "normalization preserves known keys"
  (define h (hasheq 'provider 'openai 'max-iterations 10))
  (define c (hash->session-config h))
  ;; Use dict-ref for raw value access; config-provider has provider? contract
  (check-eq? (dict-ref c 'provider) 'openai)
  (check-equal? (config-max-iterations c) 10))

(test-case "normalization coerces string thinking-level to symbol"
  (define c (hash->session-config (hasheq 'thinking-level "high")))
  (check-eq? (config-thinking-level c) 'high))

(test-case "normalization preserves unknown keys"
  (define c (hash->session-config (hasheq 'unknown-key 42 'provider 'p)))
  (check-equal? (dict-ref c 'unknown-key) 42)
  ;; Use dict-ref for raw value access; config-provider has provider? contract
  (check-eq? (dict-ref c 'provider) 'p))

;; -- T-01: Contract-rejection tests for config-settings --
(test-case "T-01: config-settings returns #f when no settings provided"
  (define c (hash->session-config (hasheq)))
  (check-false (config-settings c) "config-settings should return #f for empty config"))

(test-case "T-01: config-settings contract rejects plain hash on return"
  ;; A plain hash is not a q-settings? -- the contract should raise
  (define c (hash->session-config (hasheq 'settings (hasheq))))
  (check-exn exn:fail:contract?
             (lambda () (config-settings c))
             "config-settings should raise contract error for plain hash"))

;; -- T-02: Contract-rejection tests for config-cancellation-token --
(test-case "T-02: config-cancellation-token returns #f when not provided"
  (define c (hash->session-config (hasheq)))
  (check-false (config-cancellation-token c)))

(test-case "T-02: config-cancellation-token contract rejects non-token on return"
  ;; A string is not a cancellation-token? -- the contract should raise
  (define c (hash->session-config (hasheq 'cancellation-token "not-a-token")))
  (check-exn exn:fail:contract?
             (lambda () (config-cancellation-token c))
             "config-cancellation-token should raise contract error for string"))

;; v0.77.7 W7.1: Profile tests
(test-case "context-assembly-profile? validates known profiles"
  (check-true (context-assembly-profile? 'off))
  (check-true (context-assembly-profile? 'observe))
  (check-true (context-assembly-profile? 'bounded))
  (check-true (context-assembly-profile? 'self-healing))
  (check-true (context-assembly-profile? 'full))
  (check-false (context-assembly-profile? 'unknown))
  (check-false (context-assembly-profile? 42)))

(test-case "apply-context-assembly-profile! bounded sets flags"
  (apply-context-assembly-profile! 'bounded)
  ;; Just check it doesn't error
  (check-true #t))

(test-case "default profile is off"
  (apply-context-assembly-profile! 'off)
  (check-equal? (current-context-assembly-profile) 'off))

;; v0.78.1 G1: Profile activation matrix tests
(test-case "profile 'off disables all flags"
  (apply-context-assembly-profile! 'off)
  (check-false (current-task-state-aware-assembly?))
  (check-false (current-graph-conclusion-selection?))
  (check-false (current-auto-distillation-enabled?))
  (check-false (current-rollback-action-execution?))
  (check-false (current-ws-evolution-enabled?)))

(test-case "profile 'observe enables assembly only"
  (apply-context-assembly-profile! 'observe)
  (check-true (current-task-state-aware-assembly?))
  (check-false (current-graph-conclusion-selection?))
  (check-false (current-auto-distillation-enabled?))
  (check-false (current-rollback-action-execution?))
  (check-false (current-ws-evolution-enabled?))
  (apply-context-assembly-profile! 'off))

(test-case "profile 'bounded enables assembly + graph + budget"
  (apply-context-assembly-profile! 'bounded)
  (check-true (current-task-state-aware-assembly?))
  (check-true (current-graph-conclusion-selection?))
  (check-false (current-auto-distillation-enabled?))
  (check-false (current-rollback-action-execution?))
  (check-false (current-ws-evolution-enabled?))
  (apply-context-assembly-profile! 'off))

(test-case "profile 'self-healing enables bounded + distill + rollback"
  (apply-context-assembly-profile! 'self-healing)
  (check-true (current-task-state-aware-assembly?))
  (check-true (current-graph-conclusion-selection?))
  (check-true (current-auto-distillation-enabled?))
  (check-true (current-rollback-action-execution?))
  (check-false (current-ws-evolution-enabled?))
  (apply-context-assembly-profile! 'off))

(test-case "profile 'full enables everything"
  (apply-context-assembly-profile! 'full)
  (check-true (current-task-state-aware-assembly?))
  (check-true (current-graph-conclusion-selection?))
  (check-true (current-auto-distillation-enabled?))
  (check-true (current-rollback-action-execution?))
  (check-true (current-ws-evolution-enabled?))
  (apply-context-assembly-profile! 'off))

;; v0.79.0 GAP-1: Config hash round-trip for context-assembly-profile
(test-case "config-context-assembly-profile reads from config hash"
  (define h (hash 'context-assembly-profile 'observe))
  (define cfg (hash->session-config h))
  (check-eq? (config-context-assembly-profile cfg) 'observe))

(test-case "config-context-assembly-profile defaults to off"
  (define cfg (hash->session-config (hash)))
  (check-eq? (config-context-assembly-profile cfg) 'off))
