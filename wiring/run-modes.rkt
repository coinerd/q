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
         "../runtime/model-registry.rkt"
         (only-in "../runtime/provider-factory.rkt" build-provider)
         "../tools/tool.rkt"
         (only-in "../tools/registry-defaults.rkt" register-default-tools!)
         "../agent/event-bus.rkt"
         "../extensions/api.rkt"
         (only-in "../extensions/loader.rkt" load-extension!))

;; Re-export mode runners from sub-modules
(require "run-interactive.rkt"
         "run-json-rpc.rkt")

(provide build-runtime-from-cli
         load-extensions-from-dir!
         mode-for-config
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

  ;; Model name from CLI --model flag
  (define model-name (hash-ref base-config 'model #f))

  ;; Build final config: merge CLI options with runtime components
  (hash-set! base-config 'provider prov)
  (hash-set! base-config 'tool-registry reg)
  (hash-set! base-config 'event-bus bus)
  (hash-set! base-config 'session-dir session-dir)
  (hash-set! base-config 'extension-registry ext-reg)
  ;; Resolve effective model name: CLI flag > settings default
  ;; Also persist model-registry for /model command
  (define model-reg (make-model-registry-from-config (q-settings-merged settings)))
  (hash-set! base-config 'model-registry model-reg)
  (define effective-model-name (or model-name (default-model model-reg)))
  (hash-set! base-config 'model-name effective-model-name)
  (hash-set! base-config 'system-instructions system-instrs)
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
