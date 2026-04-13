#lang racket/base

;; main.rkt — Entry point for q agent
;; Thin facade: CLI parsing → runtime construction → mode dispatch.
;; All business logic lives in wiring/ and interface modules.

(require "interfaces/cli.rkt"
         "interfaces/json-mode.rkt"
         "interfaces/rpc-mode.rkt"
         "interfaces/tui.rkt"
         "interfaces/doctor.rkt"
         "interfaces/sessions.rkt"
         (only-in "runtime/agent-session.rkt"
                  make-agent-session
                  resume-agent-session
                  run-prompt!
                  session-id
                  session-history
                  fork-session
                  close-session!)
         "runtime/settings.rkt"
         "skills/types.rkt"
         "runtime/auth-store.rkt"
         "runtime/model-registry.rkt"
         "llm/provider.rkt"
         "llm/openai-compatible.rkt"
         "tools/tool.rkt"
         "agent/event-bus.rkt"
         "extensions/api.rkt"
         (only-in "runtime/provider-factory.rkt" build-provider build-mock-provider local-provider?)
         (only-in "tools/registry-defaults.rkt" register-default-tools!)
         (only-in "wiring/run-modes.rkt"
                  build-runtime-from-cli
                  mode-for-config
                  run-interactive
                  run-single-shot
                  run-resume
                  run-json
                  run-rpc))

;; Main entry
(provide main
         ;; Wiring functions (exported for testing)
         build-provider
         register-default-tools!
         build-runtime-from-cli
         mode-for-config
         ;; Re-exported from interfaces (all-from-out for fully-required modules)
         (all-from-out "interfaces/cli.rkt")
         (all-from-out "interfaces/json-mode.rkt")
         (all-from-out "interfaces/rpc-mode.rkt")
         (all-from-out "interfaces/tui.rkt")
         (all-from-out "interfaces/doctor.rkt")
         (all-from-out "interfaces/sessions.rkt")
         ;; Runtime / core modules
         (all-from-out "runtime/settings.rkt")
         (all-from-out "skills/types.rkt")
         (all-from-out "runtime/auth-store.rkt")
         (all-from-out "runtime/model-registry.rkt")
         (all-from-out "llm/provider.rkt")
         (all-from-out "llm/openai-compatible.rkt")
         (all-from-out "tools/tool.rkt")
         (all-from-out "agent/event-bus.rkt")
         (all-from-out "extensions/api.rkt")
         ;; From only-in requires — explicit provides
         build-mock-provider
         local-provider?
         make-agent-session
         resume-agent-session
         run-prompt!
         session-id
         session-history
         fork-session
         close-session!
         run-interactive
         run-single-shot
         run-resume
         run-json
         run-rpc)

;; ============================================================
;; main — entry point
;; ============================================================

(define (main)
  (define cfg (parse-cli-args))
  (define mode (mode-for-config cfg))
  (case mode
    [(help)
     (print-usage)
     (exit 0)]
    [(version)
     (print-version)
     (exit 0)]
    [(doctor)
     (define code (run-doctor))
     (exit code)]
    [(init)
     (run-init-wizard)
     (exit 0)]
    [(sessions)
     (run-sessions-command cfg)
     (exit 0)]
    [(interactive)
     (define rt-config (build-runtime-from-cli cfg))
     (define prov (hash-ref rt-config 'provider #f))
     (define prov-name (and (provider? prov) (provider-name prov)))
     (if (eq? (cli-config-command cfg) 'resume)
         (run-resume cfg rt-config)
         (run-interactive cfg rt-config #:provider-name prov-name))]
    [(single)
     (define rt-config (build-runtime-from-cli cfg))
     (run-single-shot cfg rt-config)]
    [(json)
     (define rt-config (build-runtime-from-cli cfg))
     (run-json cfg rt-config)]
    [(rpc)
     (define rt-config (build-runtime-from-cli cfg))
     (run-rpc cfg rt-config)]
    [(tui)
     (define rt-config (build-runtime-from-cli cfg))
     (run-tui-with-runtime rt-config cfg)]))

(module+ main
  (main))
