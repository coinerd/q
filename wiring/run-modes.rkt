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
         (only-in "../runtime/settings.rkt" setting-memory-injection-budget setting-memory-backend)
         (only-in "../runtime/context-assembly/memory-builder.rkt" current-memory-injection-budget)
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

  (hash->session-config final-hash-with-memory))

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
