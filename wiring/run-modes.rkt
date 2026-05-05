#lang racket/base

;; wiring/run-modes.rkt — Runtime construction and mode dispatch facade
;;
;; Extracted from main.rkt. Contains:
;;   - build-runtime-from-cli: assemble runtime config from CLI args
;;   - load-extensions-from-dir!: helper for extension discovery
;;   - mode-for-config: determine interface mode from cli-config
;;   - Re-exports all mode runners from run-interactive.rkt and run-json-rpc.rkt

(require racket/file
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
         "../runtime/model-registry.rkt"
         (only-in "../runtime/provider-factory.rkt" build-provider)
         "../tools/tool.rkt"
         (only-in "../tools/registry-defaults.rkt" register-default-tools!)
         "../agent/event-bus.rkt"
         (only-in "mode-helpers.rkt"
                  wire-security-config!
                  wire-timeouts!
                  make-trace-logger
                  start-trace-logger!
                  project-tree->string)
         (only-in "extension-setup.rkt" make-wired-extension-registry load-extensions-from-dir!))

;; Re-export mode runners from sub-modules
(require "run-interactive.rkt"
         "run-json-rpc.rkt")

(provide build-runtime-from-cli
         load-extensions-from-dir!
         mode-for-config
         reload-config!
         ;; Re-exported from run-interactive.rkt
         make-terminal-subscriber
         run-interactive
         run-single-shot
         run-resume
         ;; Re-exported from run-json-rpc.rkt
         run-json
         run-rpc
         ;; Re-exported from run-interactive.rkt
         run-print-mode)

;; ============================================================
;; build-runtime-from-cli
;; ============================================================

;; Build a complete runtime config hash from a cli-config.
;; Creates event bus, tool registry (with defaults), provider,
;; extension registry, and session directory.
;;
;; The returned hash is suitable for make-agent-session or
;; resume-agent-session.

(define (build-runtime-from-cli cfg)
  (define base-config (cli-config->runtime-config cfg))
  (define bus (make-event-bus))
  (define reg (make-tool-registry))
  ;; Register default tools unless --no-tools
  (unless (cli-config-no-tools? cfg)
    (define only-tools (cli-config-tools cfg))
    (register-default-tools! reg #:only (if (null? only-tools) #f only-tools)))

  ;; Load settings ONCE — for both provider building and session-dir
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
                '())
            (if project-tree-section
                (list project-tree-section)
                '())))

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

  ;; Extension registry — discover from global + project dirs
  ;; Load order: global (~/.q/extensions/) first, then project-local.
  ;; Project-local extensions override global ones (same name wins from later registration).
  (define-values (ext-reg ext-cmds ext-shortcuts) (make-wired-extension-registry bus project-dir))

  ;; Model name from CLI --model flag
  (define model-name (hash-ref base-config 'model #f))

  ;; Build final config: merge CLI options with runtime components
  (hash-set! base-config 'provider prov)
  (hash-set! base-config 'tool-registry reg)
  (hash-set! base-config 'event-bus bus)
  (hash-set! base-config 'session-dir session-dir)
  (hash-set! base-config 'extension-registry ext-reg)
  (hash-set! base-config 'extension-commands ext-cmds)
  (hash-set! base-config 'extension-shortcuts ext-shortcuts)
  ;; Resolve effective model name: CLI flag > settings default
  ;; Also persist model-registry for /model command
  (define model-reg (make-model-registry-from-config (q-settings-merged settings)))
  (hash-set! base-config 'model-registry model-reg)
  (hash-set! base-config 'settings settings)
  ;; v0.24.7: Merge max-iterations from config.json settings.
  ;; Precedence: CLI --max-turns > config.json max-iterations > default 10.
  ;; Only override when CLI used the default (10) and config.json has a value.
  (let ([settings-max-iter (setting-ref settings 'max-iterations #f)])
    (when (and settings-max-iter (= (cli-config-max-turns cfg) 10))
      (hash-set! base-config 'max-iterations settings-max-iter)))
  (define effective-model-name (or model-name (default-model model-reg)))
  (hash-set! base-config 'model-name effective-model-name)
  (hash-set! base-config 'system-instructions final-system-instrs)
  ;; #1187: Wire templates into runtime config for /template command
  (hash-set! base-config 'templates (resource-set-templates all-resources))
  (hash-set! base-config 'verbose? (cli-config-verbose? cfg))
  ;; v0.14.2 Wave 3: Set per-model timeouts from settings
  (wire-timeouts! settings)

  ;; v0.15.0 Wave 2: Wire trace logger if enabled in config
  (when (trace-enabled? settings)
    (define trace-log (make-trace-logger bus session-dir #:enabled? #t))
    (hash-set! base-config 'trace-logger trace-log)
    (start-trace-logger! trace-log))

  ;; v0.25.2 (F3): Wire security config from config.json
  (wire-security-config! settings)

  base-config)

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
;; Takes a mutable base-config hash (from build-runtime-from-cli)
;; and refreshes the settings + model-registry entries.
;; Returns the new model-registry.
(define (reload-config! base-config)
  (define project-dir (hash-ref base-config 'project-dir #f))
  (define home-dir (hash-ref base-config 'home-dir #f))
  (define config-path (hash-ref base-config 'config-path #f))
  ;; Re-read settings from disk
  (define new-settings
    (load-settings (or project-dir (current-directory))
                   #:home-dir (or home-dir (find-system-path 'home))
                   #:config-path config-path))
  ;; Rebuild model registry from new merged config
  (define new-reg (make-model-registry-from-config (q-settings-merged new-settings)))
  ;; Swap into base-config
  (hash-set! base-config 'settings new-settings)
  (hash-set! base-config 'model-registry new-reg)
  ;; v0.14.2 Wave 3: Refresh per-model timeouts
  (wire-timeouts! new-settings)
  new-reg)
