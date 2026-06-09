#lang racket/base

;; wiring/run-modes.rkt — Runtime construction and mode dispatch facade
;;
;; Extracted from main.rkt. Contains:
;;   - build-runtime-from-cli: assemble runtime config from CLI args
;;   - load-extensions-from-dir!: helper for extension discovery
;;   - mode-for-config: determine interface mode from cli-config
;;   - Re-exports all mode runners from run-interactive.rkt and run-json-rpc.rkt

(require racket/contract
         racket/dict
         racket/file
         racket/string
         "../interfaces/cli.rkt"
         (only-in "../runtime/agent-session.rkt"
                  make-agent-session
                  resume-agent-session
                  run-prompt!
                  session-id
                  session-history
                  fork-session
                  close-session!)
         "../runtime/settings.rkt"
         "../skills/types.rkt"
         (only-in "../skills/resource-loader.rkt" skills-summary-section)
         "../runtime/provider/model-registry.rkt"
         (only-in "../runtime/provider/provider-factory.rkt" build-provider)
         (only-in "../llm/model.rkt" make-model-request model-response-content)
         (only-in "../llm/provider.rkt" provider-send)
         "../tools/tool.rkt"
         (only-in "../tools/registry-defaults.rkt" register-default-tools!)
         "../agent/event-bus.rkt"
         (only-in "../runtime/session/session-config.rkt"
                  hash->session-config
                  session-config?
                  session-config->hash)
         (only-in "mode-helpers.rkt"
                  wire-security-config!
                  wire-timeouts!
                  make-trace-logger
                  start-trace-logger!
                  project-tree->string)
         (only-in "extension-setup.rkt" make-wired-extension-registry load-extensions-from-dir!)
         (only-in "../runtime/session/session-config.rkt" apply-context-assembly-profile!)
         (only-in "../runtime/context-assembly/memory-builder.rkt" current-memory-injection-budget)
         (only-in "../runtime/memory/auto-extraction.rkt"
                  current-auto-extraction-enabled
                  current-auto-extraction-min-confidence)
         (only-in "../runtime/memory/service.rkt"
                  update-memory-policy!
                  current-auto-reflection-enabled
                  current-auto-reflection-min-items)
         (only-in "../agent/iteration/step-interpreter.rkt" current-reflection-prompt-enabled)
         (only-in "../runtime/context-assembly/auto-distillation.rkt"
                  current-auto-distillation-enabled?
                  current-llm-distill-fn)
         (only-in "../runtime/context-assembly/task-conclusion.rkt" task-conclusion)
         (only-in "../util/ids.rkt" generate-id)
         (only-in "../runtime/memory/reflection.rkt" current-reflection-llm-fn)
         (only-in "../extensions/gsd/state-machine.rkt" gsm-current)
         (only-in "../runtime/gsd-query.rkt" current-gsd-mode-query))

;; Re-export mode runners from sub-modules
(require "run-interactive.rkt"
         "run-json-rpc.rkt")

(provide (contract-out [build-runtime-from-cli (-> cli-config? session-config?)]
                       [mode-for-config (-> cli-config? symbol?)]
                       [reload-config! (-> session-config? (values session-config? model-registry?))])
         ;; Direct re-exports (no contracts needed — re-exported)
         load-extensions-from-dir!
         make-terminal-subscriber
         run-interactive
         run-single-shot
         run-resume
         run-json
         run-rpc
         run-print-mode)

;; ============================================================
;; build-runtime-from-cli
;; ============================================================

;; Build a complete runtime config from a cli-config.
;; Creates event bus, tool registry (with defaults), provider,
;; extension registry, and session directory.
;;
;; v0.35.2 (W-03): Returns session-config? instead of mutable hash.
;; Consumers should use dict-ref (not hash-ref) for access.

(define (build-runtime-from-cli cfg)
  (define base-config (cli-config->runtime-config cfg))
  (define bus (make-event-bus))
  (define reg (make-tool-registry))
  ;; Register default tools unless --no-tools
  (unless (cli-config-no-tools? cfg)
    (define only-tools (cli-config-tools cfg))
    (register-default-tools! reg #:only (if (null? only-tools) #f only-tools)))

  ;; Load settings ONCE -- for both provider building and session-dir
  (define project-dir (or (hash-ref base-config 'project-dir #f) (current-directory)))
  (define config-path (hash-ref base-config 'config-path #f))
  (define settings (load-settings project-dir #:config-path config-path))

  ;; Load resources (system instructions, skills, templates)
  (define global-resources (load-global-resources))
  (define project-resources (load-project-resources project-dir))
  (define all-resources (merge-resources global-resources project-resources))
  (define system-instrs (resource-set-instructions all-resources))

  ;; Progressive skill disclosure: inject skill summaries into system prompt
  ;; Full SKILL.md content is available on-demand via the `read` tool
  (define skill-section (skills-summary-section (resource-set-skills all-resources)))

  ;; v0.19.3 Wave 3: Inject project file tree into system prompt
  ;; This saves 1-2 exploration iterations per session.
  (define project-tree-section
    (let ([tree-str (project-tree->string project-dir)]) (if (string=? tree-str "") #f tree-str)))

  (define final-system-instrs
    (append system-instrs
            (if skill-section
                (list skill-section)
                (list))
            (if project-tree-section
                (list project-tree-section)
                (list))))

  ;; Provider uses the shared settings
  (define prov (build-provider base-config settings))

  ;; Session dir: from CLI override, or from settings, or default
  (define session-dir
    (or (hash-ref base-config 'session-dir #f)
        (let ([sd (session-dir-from-settings settings)])
          (if (path? sd)
              (path->string sd)
              sd))))
  (make-directory* session-dir)

  ;; Extension registry -- discover from global + project dirs
  ;; Load order: global (~/.q/extensions/) first, then project-local.
  ;; Project-local extensions override global ones (same name wins from later registration).
  (define-values (ext-reg ext-cmds ext-shortcuts) (make-wired-extension-registry bus project-dir))

  ;; T3-5: Wire GSD mode query — moved from tui/tui-init.rkt to fix layer violation.
  ;; TUI imports are clean; GSD state is queried via parameter set here in wiring layer.
  (current-gsd-mode-query (lambda () (gsm-current)))

  ;; Model name from CLI --model flag
  (define model-name (hash-ref base-config 'model #f))

  ;; Resolve effective model name: CLI flag > settings default
  (define model-reg (make-model-registry-from-config (q-settings-merged settings)))
  (define effective-model-name (or model-name (default-model model-reg)))

  ;; v0.24.7: Merge max-iterations from config.json settings.
  (define max-iter
    (let ([settings-max-iter (setting-ref settings 'max-iterations #f)])
      (if (and settings-max-iter (= (cli-config-max-turns cfg) 10))
          settings-max-iter
          (hash-ref base-config 'max-iterations 50))))

  ;; v0.15.0 Wave 2: Wire trace logger if enabled in config
  (define trace-log
    (when (trace-enabled? settings)
      (define tl (make-trace-logger bus session-dir #:enabled? #t))
      (start-trace-logger! tl)
      tl))

  ;; Build final config: immutable hash -> session-config (W-03)
  (define final-hash
    (hash-set* base-config
               'provider
               prov
               'tool-registry
               reg
               'event-bus
               bus
               'session-dir
               session-dir
               'extension-registry
               ext-reg
               'extension-commands
               ext-cmds
               'extension-shortcuts
               ext-shortcuts
               'model-registry
               model-reg
               'settings
               settings
               'max-iterations
               max-iter
               'model-name
               effective-model-name
               'system-instructions
               final-system-instrs
               'templates
               (resource-set-templates all-resources)
               'verbose?
               (cli-config-verbose? cfg)
               'trace-logger
               trace-log))

  ;; v0.14.2 Wave 3: Set per-model timeouts from settings
  (wire-timeouts! settings)

  ;; v0.25.2 (F3): Wire security config from config.json
  (wire-security-config! settings)

  ;; v0.79.0 (GAP-1): Wire context assembly profile — CLI overrides settings
  (define settings-profile (setting-context-assembly-profile settings))
  (define cli-profile (cli-config-context-profile cfg))
  (define profile (or cli-profile settings-profile))
  (define final-hash-with-profile (hash-set final-hash 'context-assembly-profile profile))
  (apply-context-assembly-profile! profile)

  ;; v0.95.15 W4: Wire memory injection budget from settings
  (define settings-budget (setting-memory-injection-budget settings))
  (when settings-budget
    (current-memory-injection-budget settings-budget))

  ;; v0.95.16: Wire memory backend from settings (config.json)
  ;; Only applies when --memory CLI flag was NOT passed.
  (define settings-backend (setting-memory-backend settings))
  (define final-hash-with-memory
    (if (and settings-backend (not (hash-ref base-config 'memory-backend #f)))
        (hash-set final-hash-with-profile 'memory-backend settings-backend)
        final-hash-with-profile))

  ;; v0.95.16 W1: Wire auto-extraction from settings
  (define auto-extract-enabled? (setting-memory-auto-extraction-enabled? settings))
  (define auto-extract-min-conf (setting-memory-auto-extraction-min-confidence settings))
  (current-auto-extraction-enabled auto-extract-enabled?)
  (current-auto-extraction-min-confidence auto-extract-min-conf)
  (define final-hash-with-auto-extract
    (hash-set* final-hash-with-memory
               'memory-auto-extraction-enabled
               auto-extract-enabled?
               'memory-auto-extraction-min-confidence
               auto-extract-min-conf))

  ;; v0.95.21 fix: Wire user-scope policy and auto-reflection from settings
  (update-memory-policy! #:user-scope-enabled? (setting-memory-user-scope-enabled? settings))
  (current-auto-reflection-enabled (setting-memory-auto-reflection-enabled? settings))
  (current-auto-reflection-min-items (setting-memory-auto-reflection-min-items settings))

  ;; v0.96.14: Wire reflection-prompt-enabled and auto-distillation from config.
  ;; Precedence: apply-context-assembly-profile! sets profile-based default first (line 219),
  ;; then explicit config.json setting overrides it. 'unset sentinel preserves profile default.
  (current-reflection-prompt-enabled (setting-reflection-prompt-enabled? settings))
  (let ([ad (setting-auto-distillation-enabled? settings)])
    (unless (eq? ad 'unset)
      (current-auto-distillation-enabled? ad)))

  ;; LF1 (GAP-1/7): Wire LLM distill function and reflection LLM function
  ;; from provider. These enable LLM-powered conclusion distillation and
  ;; reflection merging when a provider is available. Both default to #f
  ;; (deterministic fallback) when provider is missing or fails.
  (when (and prov effective-model-name)
    ;; GAP-1: LLM distill factory - generates conclusions for uncovered WS entries
    (current-llm-distill-fn
     (lambda (uncovered-ids current-state [content-summaries (hash)])
       (with-handlers
           ([exn:fail?
             (lambda (e)
               (raise
                e))]) ; re-raise so distill-with-llm's outer handler generates deterministic fallback
         ;; GAP-6: Include content summaries in prompt for richer distillation
         (define content-lines
           (for/list ([id (in-list uncovered-ids)])
             (define summary (hash-ref content-summaries id ""))
             (if (string=? summary "")
                 (format "  ~a: (no content)" id)
                 (format "  ~a: ~a"
                         id
                         (if (> (string-length summary) 300)
                             (string-append (substring summary 0 300) "...")
                             summary)))))
         (define prompt-text
           (format
            "For each working set entry, generate a brief conclusion about what was learned.\nState: ~a\nEntries:\n~a\nOutput one conclusion per line."
            (or current-state 'unknown)
            (string-join content-lines "\n")))
         (define resp
           (provider-send prov
                          (make-model-request (list (hasheq 'role "user" 'content prompt-text))
                                              #f
                                              (hasheq 'model effective-model-name 'max_tokens 1000))))
         (define resp-parts (model-response-content resp))
         (define text
           (string-trim (string-join (for/list ([p (in-list resp-parts)])
                                       (cond
                                         [(hash? p) (hash-ref p 'text "")]
                                         [(string? p) p]
                                         [else ""]))
                                     "")))
         (define llm-lines (string-split text "\n"))
         ;; GAP-A: indexed iteration with fallback for truncated LLM output
         ;; GAP-B: unique conclusion IDs via generate-id
         (if (string=? text "")
             '()
             (for/list ([id (in-list uncovered-ids)]
                        [i (in-naturals)])
               (define line-text
                 (if (< i (length llm-lines))
                     (string-trim (list-ref llm-lines i))
                     (format "[auto] uncovered entry ~a" id)))
               (task-conclusion (generate-id)
                                line-text
                                'fact
                                (or current-state 'unknown)
                                (list id)
                                (current-seconds)
                                '()
                                '()))))))
    ;; GAP-7: Reflection LLM factory - synthesizes merged reflection text
    (current-reflection-llm-fn
     (lambda (contents)
       (with-handlers ([exn:fail? (lambda (e) (string-join (sort contents string<?) "; "))])
         (define prompt-text
           (format
            "Synthesize these related memory items into one concise observation:\n~a\nOutput one synthesized observation."
            (string-join contents "\n- ")))
         (define resp
           (provider-send prov
                          (make-model-request (list (hasheq 'role "user" 'content prompt-text))
                                              #f
                                              (hasheq 'model effective-model-name 'max_tokens 500))))
         (define resp-parts (model-response-content resp))
         (define text
           (string-trim (string-join (for/list ([p (in-list resp-parts)])
                                       (cond
                                         [(hash? p) (hash-ref p 'text "")]
                                         [(string? p) p]
                                         [else ""]))
                                     "")))
         (if (string=? text "")
             (string-join (sort contents string<?) "; ")
             text)))))
  (hash->session-config final-hash-with-auto-extract))

;; ============================================================
;; mode-for-config
;; ============================================================

;; Determine which interface mode to run based on cli-config.
;; Returns a symbol: 'interactive | 'single | 'json | 'rpc | 'tui | 'help | 'version

(define (mode-for-config cfg)
  (define cmd (cli-config-command cfg))
  (case cmd
    [(help) 'help]
    [(version) 'version]
    [(doctor) 'doctor]
    [(init) 'init]
    [(sessions) 'sessions]
    [else (cli-config-mode cfg)]))

;; ============================================================
;; reload-config! (#1182)
;; ============================================================

;; Reload settings and model registry from disk without restarting.
;; v0.35.2 (W-03): Works with session-config. Returns (values new-session-config new-model-registry).
(define (reload-config! base-config)
  (define project-dir (dict-ref base-config 'project-dir #f))
  (define home-dir (dict-ref base-config 'home-dir #f))
  (define config-path (dict-ref base-config 'config-path #f))
  ;; Re-read settings from disk
  (define new-settings
    (load-settings (or project-dir (current-directory))
                   #:home-dir (or home-dir (find-system-path 'home))
                   #:config-path config-path))
  ;; Rebuild model registry from new merged config
  (define new-reg (make-model-registry-from-config (q-settings-merged new-settings)))
  ;; v0.14.2 Wave 3: Refresh per-model timeouts
  (wire-timeouts! new-settings)
  ;; Return updated config + registry
  (values
   (hash->session-config
    (hash-set* (session-config->hash base-config) 'settings new-settings 'model-registry new-reg))
   new-reg))
