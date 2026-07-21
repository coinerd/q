#lang racket/base
;; STABILITY: public

;; runtime/settings.rkt — Settings facade (F6a+F6b extraction)
;;
;; Re-exports settings-core (struct, loading, merging) and
;; settings-query (accessor functions) as a single public API.
;; All consumers import from this module — no behavioral change.
;;
;; CONSUMERS: auth-store, model-registry, providers, session-config, etc.

(require "settings-core.rkt"
         "settings-query.rkt"
         (only-in "../util/sandbox-config.rkt"
                  sandbox-enabled?
                  sandbox-timeout
                  sandbox-memory-limit
                  sandbox-max-output
                  sandbox-max-processes))

;; Re-export everything from both sub-modules
(provide q-settings
         q-settings?
         q-settings-merged
         q-settings-global
         q-settings-project
         (rename-out [load-settings load-settings]
                     [load-global-settings load-global-settings]
                     [load-project-settings load-project-settings]
                     [make-minimal-settings make-minimal-settings]
                     [merge-settings merge-settings]
                     [deep-merge-hash deep-merge-hash]
                     [config-parse-error config-parse-error])
         hash-nested-ref
         not-found?
         (rename-out [setting-ref setting-ref]
                     [setting-ref* setting-ref*]
                     [provider-config provider-config]
                     [provider-names provider-names]
                     [parallel-tools-enabled? parallel-tools-enabled?]
                     [http-request-timeout http-request-timeout]
                     [get-model-timeout get-model-timeout]
                     [effective-request-timeout effective-request-timeout]
                     [warn-on-destructive? warn-on-destructive?]
                     [security-config-from-settings security-config-from-settings]
                     [default-session-dir default-session-dir]
                     [default-project-dir default-project-dir]
                     [session-dir-from-settings session-dir-from-settings]
                     [project-dir-from-settings project-dir-from-settings]
                     [trace-enabled? trace-enabled?]
                     [trace-max-files trace-max-files]
                     [steering-gentle-threshold steering-gentle-threshold]
                     [steering-strong-threshold steering-strong-threshold]
                     [steering-hard-cap steering-hard-cap]
                     [steering-same-file-dedup? steering-same-file-dedup?]
                     [credential-policy credential-policy]
                     [shell-risk-classifier shell-risk-classifier]
                     [setting-context-assembly-profile setting-context-assembly-profile]
                     [setting-memory-injection-budget setting-memory-injection-budget]
                     [setting-memory-backend setting-memory-backend]
                     [setting-memory-auto-extraction-enabled? setting-memory-auto-extraction-enabled?]
                     [setting-memory-auto-extraction-min-confidence setting-memory-auto-extraction-min-confidence]
                     [setting-memory-user-scope-enabled? setting-memory-user-scope-enabled?]
                     [setting-memory-auto-reflection-enabled? setting-memory-auto-reflection-enabled?]
                     [setting-memory-auto-reflection-min-items setting-memory-auto-reflection-min-items]
                     [setting-reflection-prompt-enabled? setting-reflection-prompt-enabled?]
                     [setting-auto-distillation-enabled? setting-auto-distillation-enabled?]
                     [execution-plane-enabled? execution-plane-enabled?]
                     [execution-plane-timeout-ms execution-plane-timeout-ms]
                     [execution-plane-command execution-plane-command]
                     [execution-plane-worker-args execution-plane-worker-args]
                     [verifier-enabled? verifier-enabled?]
                     [verifier-model verifier-model]
                     [verifier-risk-threshold verifier-risk-threshold]
                     [verifier-max-rework-iterations verifier-max-rework-iterations]
                     [blackboard-enabled? blackboard-enabled?]
                     [hot-swap-enabled? hot-swap-enabled?]
                     [auto-reload-enabled? auto-reload-enabled?]
                     [mcp-enabled? mcp-enabled?]
                     [mcp-server-enabled? mcp-server-enabled?]
                     [mcp-server-transport mcp-server-transport]
                     [mcp-client-servers mcp-client-servers]
                     [broker-enabled? broker-enabled?]
                     [broker-remote-host broker-remote-host]
                     [broker-remote-port broker-remote-port]
                     [broker-cert-dir broker-cert-dir]
                     [broker-capability-secret broker-capability-secret])
         sandbox-enabled?
         sandbox-timeout
         sandbox-memory-limit
         sandbox-max-output
         sandbox-max-processes)
