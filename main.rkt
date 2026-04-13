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
         ;; Re-exported from interfaces/doctor.rkt
         run-doctor
         check-result
         ;; Re-exported from interfaces/cli.rkt
         parse-cli-args
         cli-config->runtime-config
         cli-config
         cli-config-command
         cli-config-mode
         cli-config-session-id
         cli-config-prompt
         cli-config-model
         cli-config-project-dir
         cli-config-verbose?
         cli-config-max-turns
         cli-config-no-tools?
         cli-config-tools
         print-usage
         print-version
         run-cli-interactive
         run-cli-single
         run-init-wizard
         ;; Re-exported from tools/tool.rkt
         make-tool-registry
         make-tool
         tool?
         tool-name
         tool-description
         tool-schema
         tool-execute
         tool-registry?
         register-tool!
         lookup-tool
         list-tools
         tool-names
         ;; Re-exported from agent/event-bus.rkt
         make-event-bus
         event-bus?
         subscribe!
         unsubscribe!
         ;; Re-exported from llm/provider.rkt
         provider?
         provider-name
         make-mock-provider
         ;; Re-exported from runtime/settings.rkt
         q-settings
         q-settings-merged
         load-settings
         default-session-dir
         session-dir-from-settings
         ;; Re-exported from runtime/resource-loader.rkt
         load-global-resources
         load-project-resources
         merge-resources
         resource-set-instructions
         resource-set-skills
         resource-set-templates
         resource-set-config
         resource?
         resource-set?
         ;; Re-exported from extensions/api.rkt
         make-extension-registry
         extension-registry?
         ;; Re-exported from runtime/auth-store.rkt
         credential
         credential-api-key
         lookup-credential
         mask-api-key
         cred->redacted
         redacted-credential
         redacted-credential?
         redacted-credential-provider-name
         redacted-credential-masked-api-key
         redacted-credential-source
         validate-credential-format
         load-credential-file
         save-credential-file!
         credential-file-path
         ;; Re-exported from runtime/model-registry.rkt
         make-model-registry-from-config
         resolve-model
         model-resolution
         model-resolution-provider-name
         model-resolution-provider-config
         model-resolution-base-url
         model-resolution-model-name
         model-entry-name
         model-entry-provider-name
         default-model
         available-models
         ;; Re-exported from llm/openai-compatible.rkt
         make-openai-compatible-provider
         ;; Re-exported from interfaces/json-mode.rkt
         start-json-mode!
         stop-json-mode!
         parse-json-intent
         intent-type
         intent-payload
         ;; Re-exported from interfaces/rpc-mode.rkt
         run-rpc-loop
         start-rpc-event-forwarding!
         stop-rpc-event-forwarding!
         ;; Re-exported from runtime/agent-session.rkt
         make-agent-session
         resume-agent-session
         run-prompt!
         session-id
         session-history
         fork-session
         close-session!
         ;; Re-exported from interfaces/sessions.rkt
         sessions-list
         sessions-list->strings
         sessions-info
         sessions-info->string
         sessions-delete
         run-sessions-command)

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
