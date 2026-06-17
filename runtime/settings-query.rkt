#lang racket/base
;; runtime/settings-query.rkt — Settings query/accessor functions (F6b extraction)
;;
;; Pure query functions that read from q-settings struct.
;; Depends only on settings-core.rkt for the struct definition.
;;
;; STABILITY: public (re-exported through settings.rkt facade)

(require "settings-core.rkt"
         racket/contract
         racket/hash
         racket/file
         racket/runtime-path
         "../util/config-paths.rkt")

;; Resolve worker-main.rkt to an absolute path at compile time.
;; This ensures the execution plane works regardless of current-directory.
(define-runtime-path default-worker-main-rkt "../sandbox/worker-main.rkt")

(provide (contract-out
          [setting-ref (->* (q-settings? (or/c symbol? string?)) (any/c) any/c)]
          [setting-ref* (->* (q-settings? (listof any/c)) (any/c) any/c)]
          [provider-config (-> q-settings? (or/c symbol? string?) (or/c hash? #f))]
          [provider-names (-> q-settings? (listof symbol?))]
          [parallel-tools-enabled? (-> q-settings? boolean?)]
          [http-request-timeout (-> q-settings? number?)]
          [get-model-timeout (-> q-settings? string? symbol? (or/c number? #f))]
          [effective-request-timeout (-> q-settings? string? number?)]
          [warn-on-destructive? (-> q-settings? boolean?)]
          [security-config-from-settings (-> q-settings? hash?)]
          [default-session-dir (-> path-string?)]
          [default-project-dir (-> path?)]
          [session-dir-from-settings (-> q-settings? (or/c path-string? #f))]
          [project-dir-from-settings (-> q-settings? (or/c path-string? #f))]
          [trace-enabled? (-> q-settings? boolean?)]
          [trace-max-files (-> q-settings? exact-positive-integer?)]
          [steering-gentle-threshold (-> q-settings? number?)]
          [steering-strong-threshold (-> q-settings? number?)]
          [steering-hard-cap (-> q-settings? number?)]
          [steering-same-file-dedup? (-> q-settings? boolean?)]
          [credential-policy
           (-> q-settings? (or/c 'auto 'keychain-preferred 'keychain-required 'env-only))]
          [shell-risk-classifier (-> q-settings? (or/c 'regex 'structured 'both))]
          [setting-context-assembly-profile (-> q-settings? symbol?)]
          [setting-memory-injection-budget (-> q-settings? (or/c exact-positive-integer? #f))]
          [setting-memory-backend (-> q-settings? (or/c symbol? hash? #f))]
          [setting-memory-auto-extraction-enabled? (-> q-settings? boolean?)]
          [setting-memory-auto-extraction-min-confidence
           (-> q-settings? (and/c real? (between/c 0 1)))]
          [setting-memory-user-scope-enabled? (-> q-settings? boolean?)]
          [setting-memory-auto-reflection-enabled? (-> q-settings? boolean?)]
          [setting-memory-auto-reflection-min-items
           (-> q-settings? (and/c exact-positive-integer? (>=/c 2)))]
          [setting-reflection-prompt-enabled? (-> q-settings? boolean?)]
          [setting-auto-distillation-enabled? (-> q-settings? (or/c boolean? 'unset))]
          [execution-plane-enabled? (-> q-settings? boolean?)]
          [execution-plane-timeout-ms (-> q-settings? exact-positive-integer?)]
          [execution-plane-command (-> q-settings? (or/c string? #f))]
          [execution-plane-worker-args (-> q-settings? (listof string?))]
          [verifier-enabled? (-> q-settings? boolean?)]
          [verifier-model (-> q-settings? (or/c string? #f))]
          [verifier-risk-threshold (-> q-settings? symbol?)]
          [verifier-max-rework-iterations (-> q-settings? exact-positive-integer?)]
          [blackboard-enabled? (-> q-settings? boolean?)]
          [hot-swap-enabled? (-> q-settings? boolean?)]
          [mcp-enabled? (-> q-settings? boolean?)]
          [mcp-server-enabled? (-> q-settings? boolean?)]
          [mcp-server-transport (-> q-settings? string?)]
          [mcp-client-servers (-> q-settings? (listof any/c))]
          [broker-enabled? (-> q-settings? boolean?)]
          [broker-remote-host (-> q-settings? string?)]
          [broker-remote-port (-> q-settings? exact-positive-integer?)]
          [broker-cert-dir (-> q-settings? string?)]
          [broker-capability-secret (-> q-settings? (or/c string? #f))]))

;; Query
;; ============================================================

;; Get a setting by key from merged settings.
;; Returns default if key not found.
(define (setting-ref settings key [default #f])
  (hash-ref (q-settings-merged settings) key default))

;; Get a nested setting by key path (list of keys).
;; e.g., (setting-ref* settings '(providers openai base-url))
;; Accepts both symbol and string keys.
;; Returns default if any step in the path is missing.
(define (setting-ref* settings key-path [default #f])
  (cond
    [(null? key-path) default]
    [else
     (define result (hash-nested-ref (q-settings-merged settings) key-path))
     (if (not-found? result) default result)]))

;; Get a specific provider's config hash.
;; Returns #f if provider not configured.
(define (provider-config settings provider-name)
  (setting-ref* settings (list 'providers provider-name)))

;; List configured provider names.
(define (provider-names settings)
  (define providers-hash (setting-ref settings 'providers (hash)))
  (hash-keys providers-hash))

;; ============================================================
;; Parallel execution setting
;; ============================================================

;; Whether tools may be executed in parallel within a batch.
;; Reads 'parallel-tools from merged settings, defaults to #f.
(define (parallel-tools-enabled? settings)
  (setting-ref settings 'parallel-tools #f))

;; ============================================================
;; HTTP request timeout setting
;; ============================================================

;; Overall HTTP request timeout in seconds (covers connection + response reading).
;; Defaults to 300 seconds (5 minutes).
(define (http-request-timeout settings)
  (setting-ref settings 'http-request-timeout 300))

;; ============================================================
;; Destructive command warning setting
;; ============================================================

;; Whether to emit a warning to stderr when destructive commands are detected.
;; Defaults to #f (no warning).
(define (warn-on-destructive? settings)
  (setting-ref settings 'warn-on-destructive #f))

;; ============================================================
;; Security config loader (v0.25.2 — F3)
;; ============================================================

(define (security-config-from-settings settings)
  (define merged (q-settings-merged settings))
  (hasheq 'execution-policy-mode
          (hash-ref merged 'execution-policy (hash-ref merged 'execution-policy.mode #f))
          'execution-policy-allowed
          (hash-ref merged 'execution-policy.allowed '())
          'secret-scrub-extra-denylist
          (hash-ref merged 'secret-scrub.extra-denylist '())
          'secret-scrub-allowlist
          (hash-ref merged 'secret-scrub.allowlist '())))

;; ============================================================
;; Sandbox settings — re-exported from util/sandbox-config.rkt
;; ============================================================

;; ============================================================
;; Defaults and derived paths
;; ============================================================

;; Default session directory: ~/.q/sessions
(define (default-session-dir)
  (build-path (global-config-dir) "sessions"))

;; Default project directory: current working directory
(define (default-project-dir)
  (current-directory))

;; Get session-dir from merged settings, falling back to default
(define (session-dir-from-settings settings)
  (or (setting-ref settings 'session-dir #f) (default-session-dir)))

;; Get project-dir from merged settings, falling back to default
(define (project-dir-from-settings settings)
  (or (setting-ref settings 'project-dir #f) (default-project-dir)))

;; ============================================================
;; Per-model timeout profiles (v0.14.2 Wave 3)
;; ============================================================

;; Get per-model timeout override for a specific timeout kind.
;; Config schema: { "timeouts": { "models": { "glm-5.1": { "request": 900 } } } }
;; Returns #f if no per-model override configured.
(define (get-model-timeout settings model-name timeout-key)
  (define model-overrides (setting-ref* settings `(timeouts models ,(string->symbol model-name)) #f))
  (and model-overrides (hash? model-overrides) (hash-ref model-overrides timeout-key #f)))

;; Get effective request timeout: per-model override or global default.
(define (effective-request-timeout settings model-name)
  (or (get-model-timeout settings model-name 'request) (http-request-timeout settings)))

;; ============================================================
;; Trace logging config (v0.15.0)
;; ============================================================

;; Is trace logging enabled? Reads logging.trace.enabled from config.
;; Default: #f (disabled — zero overhead when off)
(define (trace-enabled? settings)
  (define trace-cfg (setting-ref* settings '(logging trace) #f))
  (and (hash? trace-cfg) (hash-ref trace-cfg 'enabled #f)))

;; Maximum trace files to keep (rotation). Default: 10.
(define (trace-max-files settings)
  (define trace-cfg (setting-ref* settings '(logging trace) #f))
  (if (hash? trace-cfg)
      (hash-ref trace-cfg 'max-files 10)
      10))

;; ============================================================
;; Steering config (v0.18.0)
;; ============================================================

;; Gentle steering threshold. Default: 8 (raised from 5 in v0.18.0).
(define (steering-gentle-threshold settings)
  (setting-ref* settings '(steering gentle_threshold) 8))

;; Strong steering threshold. Default: 12 (raised from 7 in v0.18.0).
(define (steering-strong-threshold settings)
  (setting-ref* settings '(steering strong_threshold) 12))

;; Hard cap for steering. Default: 20 (raised from 12 in v0.18.0).
(define (steering-hard-cap settings)
  (setting-ref* settings '(steering hard_cap) 20))

;; Whether same-file dedup is enabled. Default: #t.
(define (steering-same-file-dedup? settings)
  (setting-ref* settings '(steering same_file_dedup) #t))

;; ============================================================
;; Context assembly profile (v0.79.0)
;; ============================================================

;; Context assembly profile from settings.
;; Reads (context-assembly profile) from merged settings.
;; Returns a symbol: off, observe, bounded, self-healing, or full.
;; Default: 'off.
(define (setting-context-assembly-profile settings)
  (define raw (setting-ref* settings '(context-assembly profile) "observe"))
  (define sym
    (if (symbol? raw)
        raw
        (string->symbol raw)))
  (if (memq sym '(off observe bounded self-healing full)) sym 'off))

;; v0.95.15 W4: Memory injection budget from settings
(define (setting-memory-injection-budget settings)
  (define raw (setting-ref* settings '(memory injection-budget) #f))
  (cond
    [(exact-positive-integer? raw) raw]
    [(string? raw)
     (define n (string->number raw))
     (if (exact-positive-integer? n) n #f)]
    [else #f]))

;; v0.95.16: Memory backend from settings (config.json)
;; Reads memory.memory-backend from config. Valid values:
;;   'hash, 'file-jsonl, #f (disabled)
;;   immutable hash spec for complex backends (v0.95.16 W4)
(define (setting-memory-backend settings)
  (define raw (setting-ref* settings '(memory backend) #f))
  (cond
    [(symbol? raw) (if (memq raw '(hash file-jsonl)) raw #f)]
    [(string? raw)
     (define sym (string->symbol raw))
     (if (memq sym '(hash file-jsonl)) sym #f)]
    [(and (hash? raw) (hash-has-key? raw 'type))
     ;; Complex backend spec — validate type field
     (define t (hash-ref raw 'type #f))
     (if (memq t '(chained external)) raw #f)]
    [else #f]))

;; v0.95.16 W1: Auto-extraction enabled from settings
;; Reads (memory auto-extraction enabled) from config.
;; Default: #f (disabled).
(define (setting-memory-auto-extraction-enabled? settings)
  (define raw (setting-ref* settings '(memory auto-extraction enabled) #f))
  (cond
    [(boolean? raw) raw]
    [(string? raw) (and (member (string-downcase raw) '("true" "1" "yes")) #t)]
    [else #f]))

;; v0.95.16 W1: Auto-extraction min-confidence from settings
;; Reads (memory auto-extraction min-confidence) from config.
;; Default: 0.5. Coerces strings to numbers.
(define (setting-memory-auto-extraction-min-confidence settings)
  (define raw (setting-ref* settings '(memory auto-extraction min-confidence) 0.5))
  (cond
    [(and (real? raw) (<= 0 raw 1)) raw]
    [(string? raw)
     (define n (string->number raw))
     (if (and (real? n) (<= 0 n 1)) n 0.5)]
    [else 0.5]))

;; v0.95.21 W1: User-scope enabled from settings
;; Reads (memory user-scope enabled) from config.
;; Default: #f (disabled).
(define (setting-memory-user-scope-enabled? settings)
  (define raw (setting-ref* settings '(memory user-scope enabled) #f))
  (cond
    [(boolean? raw) raw]
    [(string? raw) (and (member (string-downcase raw) '("true" "1" "yes")) #t)]
    [else #f]))

;; v0.95.21 W2: Auto-reflection enabled from settings
;; Reads (memory auto-reflection enabled) from config.
;; Default: #f (disabled).
(define (setting-memory-auto-reflection-enabled? settings)
  (define raw (setting-ref* settings '(memory auto-reflection enabled) #f))
  (cond
    [(boolean? raw) raw]
    [(string? raw) (and (member (string-downcase raw) '("true" "1" "yes")) #t)]
    [else #f]))

;; v0.95.21 W2: Auto-reflection min-items from settings
;; Reads (memory auto-reflection min-items) from config.
;; Default: 5. Must be >= 2.
(define (setting-memory-auto-reflection-min-items settings)
  (define raw (setting-ref* settings '(memory auto-reflection min-items) 5))
  (cond
    [(and (exact-positive-integer? raw) (>= raw 2)) raw]
    [(string? raw)
     (define n (string->number raw))
     (if (and (exact-positive-integer? n) (>= n 2)) n 5)]
    [else 5]))

;; v0.96.14: Reflection prompt enabled from settings
;; Reads (reflection-prompt-enabled) from config.
;; Default: #f (disabled).
(define (setting-reflection-prompt-enabled? settings)
  (define raw (setting-ref* settings '(reflection-prompt-enabled) #f))
  (cond
    [(boolean? raw) raw]
    [(string? raw) (and (member (string-downcase raw) '("true" "1" "yes")) #t)]
    [else #f]))

;; v0.96.14: Auto-distillation enabled from settings (overrides profile default)
;; Reads (auto-distillation-enabled) from config.
;; When not set, falls back to the context-assembly profile.
(define (setting-auto-distillation-enabled? settings)
  (define raw (setting-ref* settings '(auto-distillation-enabled) 'unset))
  (cond
    [(eq? raw 'unset) 'unset] ; caller uses profile default
    [(boolean? raw) raw]
    [(string? raw) (and (member (string-downcase raw) '("true" "1" "yes")) #t)]
    [else 'unset]))

;; ============================================================
;; Credential policy (v0.70.1)
;; ============================================================

;; Credential policy controls how credential backends behave when
;; keychain is unavailable and file/env fallback would be used.
;;
;; Modes:
;;   'auto               — current chain, no policy enforcement (default, backward-compatible)
;;   'keychain-preferred — file fallback allowed with warning
;;   'keychain-required  — file fallback forbidden for store/load
;;   'env-only           — no local credential file writes
;;
;; Config key: credentials.policy

(define (credential-policy settings)
  (setting-ref* settings '(credentials policy) 'auto))

;; ============================================================
;; Shell risk classifier mode (v0.70.3)
;; ============================================================

;; Controls which shell risk detection method to use.
;; Modes:
;;   'regex     — use regex-based patterns only (default, backward-compatible)
;;   'structured — use structured classifier only
;;   'both       — use both; classifier wins on disagreement
;;
;; Config key: security.shell-risk-classifier
(define (shell-risk-classifier settings)
  (setting-ref* settings '(security shell-risk-classifier) 'regex))

;; ============================================================
;; Execution Plane Settings (v0.99.2 MAS Schritt 2)
;; ============================================================

;; Config key: mas.execution-plane.enabled (default #t — Phase 3 activation)
(define (execution-plane-enabled? settings)
  (setting-ref* settings '(mas execution-plane enabled) #t))

;; Config key: mas.execution-plane.timeout-ms (default 120000)
(define (execution-plane-timeout-ms settings)
  (setting-ref* settings '(mas execution-plane timeout-ms) 120000))

;; Config key: mas.execution-plane.command (default #f = use "racket")
(define (execution-plane-command settings)
  (setting-ref* settings '(mas execution-plane command) #f))

;; Config key: mas.execution-plane.worker-args (default: absolute path to worker-main.rkt)
(define (execution-plane-worker-args settings)
  (setting-ref* settings
                '(mas execution-plane worker-args)
                (list "-tm" (path->string default-worker-main-rkt))))

;; ============================================================
;; Verifier Agent Settings (v0.99.5 MAS Schritt 3)
;; ============================================================

;; Config key: mas.verifier.enabled (default #t — enabled by default since v0.99.15)
;; When #t, verification gate runs between executing and idle/done.
;; v0.99.15: Flipped default from #f to #t (MAS Phase 2: Verifier Default-On).
;; The verifier only activates for GSD wave-done commands in sessions with
;; a verifier provider configured; non-GSD sessions are unaffected.
;; Safe fallback chain: no provider → auto-approve, error → escalate, timeout → escalate.
(define (verifier-enabled? settings)
  (define raw (setting-ref* settings '(mas verifier enabled) #t))
  (cond
    [(boolean? raw) raw]
    [(string? raw) (and (member (string-downcase raw) '("true" "1" "yes")) #t)]
    [else #t]))

;; Config key: mas.verifier.model (default #f = use session model)
;; When set, verification uses a specific model instead of the session default.
(define (verifier-model settings)
  (define raw (setting-ref* settings '(mas verifier model) #f))
  (cond
    [(string? raw) raw]
    [(symbol? raw) (symbol->string raw)]
    [else #f]))

;; Config key: mas.verifier.risk-threshold (default 'high — conservative since v0.99.15)
;; Valid values: 'low, 'medium, 'high.
;; Decisions with risk-level >= threshold are forced to require human review.
;; v0.99.15: Changed default from 'medium to 'high so the verifier gate
;; only escalates genuinely high-risk decisions by default.
(define (verifier-risk-threshold settings)
  (define raw (setting-ref* settings '(mas verifier risk-threshold) "high"))
  (define sym
    (if (symbol? raw)
        raw
        (string->symbol raw)))
  (if (memq sym '(low medium high)) sym 'high))

;; Config key: mas.verifier.max-rework-iterations (default 3)
;; v0.99.20 W1: Maximum consecutive verifying→executing (rework) transitions
;; before the GSD state machine blocks and forces 'idle (done).
;; Prevents infinite verifier-rework loops.
(define (verifier-max-rework-iterations settings)
  (define raw (setting-ref* settings '(mas verifier max-rework-iterations) 3))
  (cond
    [(exact-positive-integer? raw) raw]
    [(and (integer? raw) (positive? raw)) raw]
    [(string? raw)
     (define n (string->number raw))
     (if (and (exact-positive-integer? n)) n 3)]
    [else 3]))

;; ============================================================
;; Blackboard Settings (v0.99.7 MAS Schritt 4)
;; ============================================================

;; Config key: mas.blackboard.enabled (default #t — enabled by default since v0.99.14)
;; When #t, blackboard subscriber starts on session startup,
;; context injection is enabled, and crash recovery runs from trace.jsonl.
;; v0.99.14: Flipped default from #f to #t (MAS Phase 1: Blackboard Default-On).
(define (blackboard-enabled? settings)
  (define raw (setting-ref* settings '(mas blackboard enabled) #t))
  (cond
    [(boolean? raw) raw]
    [(string? raw) (and (member (string-downcase raw) '("true" "1" "yes")) #t)]
    [else #f]))

;; Check whether hot-swap (registry-based dispatch) is enabled.
;; Config path: mas.hot-swap.enabled
;; Default: #t (Phase 4 activation — enabled by default since v0.99.18)
(define (hot-swap-enabled? settings)
  (define raw (setting-ref* settings '(mas hot-swap enabled) #t))
  (cond
    [(boolean? raw) raw]
    [(string? raw) (and (member (string-downcase raw) '("true" "1" "yes")) #t)]
    [else #t]))

;; ============================================================
;; MCP Settings (v0.99.10 MAS Schritt 6)
;; ============================================================

;; Config key: mas.mcp.enabled (default #f — MCP feature gate)
;; Master switch for all MCP functionality.
(define (mcp-enabled? settings)
  (define raw (setting-ref* settings '(mas mcp enabled) #f))
  (cond
    [(boolean? raw) raw]
    [(string? raw) (and (member (string-downcase raw) '("true" "1" "yes")) #t)]
    [else #f]))

;; Config key: mas.mcp.server.enabled (default #f)
;; When #t, q runs as an MCP server over stdio.
(define (mcp-server-enabled? settings)
  (define raw (setting-ref* settings '(mas mcp server enabled) #f))
  (cond
    [(boolean? raw) raw]
    [(string? raw) (and (member (string-downcase raw) '("true" "1" "yes")) #t)]
    [else #f]))

;; Config key: mas.mcp.server.transport (default "stdio")
;; Phase 1 only supports local stdio. All other values fall back to stdio.
(define (mcp-server-transport settings)
  (define raw (setting-ref* settings '(mas mcp server transport) "stdio"))
  (cond
    [(string? raw) "stdio"]
    [(symbol? raw) "stdio"]
    [else "stdio"]))

;; Config key: mas.mcp.client.servers (default '())
;; List of external MCP server paths to connect to.
(define (mcp-client-servers settings)
  (define raw (setting-ref* settings '(mas mcp client servers) '()))
  (if (list? raw)
      raw
      '()))

;; ============================================================
;; Broker Settings (v0.99.12 MAS Schritt 6 Phase 2)
;; ============================================================

;; Config key: mas.broker.enabled (default #f — always disabled by default)
;; When #t, high-risk tool execution can be routed to remote executor nodes
;; via mTLS TCP broker.
(define (broker-enabled? settings)
  (define raw (setting-ref* settings '(mas broker enabled) #f))
  (cond
    [(boolean? raw) raw]
    [(string? raw) (and (member (string-downcase raw) '("true" "1" "yes")) #t)]
    [else #f]))

;; Config key: mas.broker.remote-host (default "localhost")
;; Hostname of the remote executor node.
(define (broker-remote-host settings)
  (setting-ref* settings '(mas broker remote-host) "localhost"))

;; Config key: mas.broker.remote-port (default 8443)
;; Port of the remote executor node.
(define (broker-remote-port settings)
  (define raw (setting-ref* settings '(mas broker remote-port) 8443))
  (if (exact-positive-integer? raw) raw 8443))

;; Config key: mas.broker.cert-dir (default "~/.q/certs/")
;; Directory containing mTLS certificates (ca.pem, client.pem, client-key.pem).
(define (broker-cert-dir settings)
  (setting-ref* settings '(mas broker cert-dir) "~/.q/certs/"))

;; Config key: mas.broker.capability-secret (default #f — MUST be set when enabled)
;; HMAC secret for signing capability tokens. Required for broker operation.
(define (broker-capability-secret settings)
  (setting-ref* settings '(mas broker capability-secret) #f))
