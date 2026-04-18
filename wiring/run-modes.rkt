#lang racket/base

;; wiring/run-modes.rkt — Runtime construction and mode dispatch facade
;;
;; Extracted from main.rkt. Contains:
;;   - build-runtime-from-cli: assemble runtime config from CLI args
;;   - load-extensions-from-dir!: helper for extension discovery
;;   - mode-for-config: determine interface mode from cli-config
;;   - Re-exports all mode runners from run-interactive.rkt and run-json-rpc.rkt

(require racket/file
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
         "../extensions/api.rkt"
         (only-in "../extensions/loader.rkt" load-extension!)
         (only-in "../extensions/hooks.rkt" dispatch-hooks)
         (only-in "../util/hook-types.rkt" hook-amend hook-result-action hook-result-payload)
         (only-in "../tui/palette.rkt" commands-from-hashes merge-extension-commands make-command-registry)
         (only-in "../tui/keymap.rkt" shortcut-specs->keymap keymap-merge))

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
         run-rpc)

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
  (define final-system-instrs
    (if skill-section
        (append system-instrs (list skill-section))
        system-instrs))

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

  ;; Extension registry — discover from project dir
  (define ext-reg (make-extension-registry))
  (load-extensions-from-dir! ext-reg (build-path project-dir ".q" "extensions") #:event-bus bus)
  (load-extensions-from-dir! ext-reg (build-path project-dir ".pi" "extensions") #:event-bus bus)

  ;; #677/#678: Query extensions for commands and shortcuts
  (define ext-cmds
    (let ()
      (define result (dispatch-hooks 'register-commands (hasheq) ext-reg))
      (if (and result (eq? (hook-result-action result) 'amend))
          (commands-from-hashes (hash-ref (hook-result-payload result) 'commands '()))
          '())))
  (define ext-shortcuts
    (let ()
      (define result (dispatch-hooks 'register-shortcuts (hasheq) ext-reg))
      (if (and result (eq? (hook-result-action result) 'amend))
          (hash-ref (hook-result-payload result) 'shortcuts '())
          '())))

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
  (define effective-model-name (or model-name (default-model model-reg)))
  (hash-set! base-config 'model-name effective-model-name)
  (hash-set! base-config 'system-instructions final-system-instrs)
  (hash-set! base-config 'verbose? (cli-config-verbose? cfg))
  base-config)

;; ============================================================
;; load-extensions-from-dir! helper
;; ============================================================

;; Load all .rkt extension files from a directory into the registry.
;; Silently skips if directory doesn't exist or individual loads fail.

(define (load-extensions-from-dir! ext-reg dir #:event-bus [event-bus #f])
  (when (directory-exists? dir)
    (define files
      (filter (λ (f) (regexp-match? #rx"\\.rkt$" (path->string f))) (directory-list dir #:build? #t)))
    (for ([f (in-list files)])
      (with-handlers ([exn:fail? (λ (e)
                                   (fprintf (current-error-port)
                                            "Warning: failed to load extension ~a: ~a\n"
                                            f
                                            (exn-message e)))])
        (load-extension! ext-reg f #:event-bus event-bus)))))

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
  new-reg)
