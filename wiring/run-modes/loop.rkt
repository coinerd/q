#lang racket/base

;; wiring/run-modes/loop.rkt — Runtime parameter wiring + config reload
;;
;; v0.99.43 W0: Extracted from wiring/run-modes.rkt (monolithic 658-line file).
;; Contains the shared parameter-wiring logic that governs the agent loop:
;;   - wire-runtime-parameters!: settings/profile → runtime parameters
;;   - reload-config!: hot reload of settings + model registry (#1182)
;;
;; STABILITY: stable — re-exported through wiring/run-modes.rkt facade.

(require racket/dict
         "../../runtime/settings.rkt"
         "../../runtime/provider/model-registry.rkt"
         (only-in "../../runtime/session/session-config.rkt"
                  hash->session-config
                  session-config?
                  session-config->hash
                  current-goal-loop-enabled?
                  apply-context-assembly-profile!)
         (only-in "../mode-helpers.rkt"
                  wire-security-config!
                  wire-timeouts!
                  resolve-permission-config)
         (only-in "../../runtime/context-assembly/config.rkt" current-task-state-aware-assembly?)
         (only-in "../../runtime/context-assembly/memory-builder.rkt" current-memory-injection-budget)
         (only-in "../../runtime/memory/auto-extraction.rkt"
                  current-auto-extraction-enabled
                  current-auto-extraction-min-confidence
                  maybe-auto-extract-tool-results!)
         (only-in "../../runtime/memory/service.rkt"
                  update-memory-policy!
                  current-auto-reflection-enabled
                  current-auto-reflection-min-items
                  current-memory-backend)
         (only-in "../../agent/iteration/step-interpreter.rkt"
                  current-reflection-prompt-enabled
                  current-post-tool-result-hook)
         (only-in "../../runtime/context-assembly/auto-distillation.rkt"
                  current-auto-distillation-enabled?
                  current-llm-distill-fn)
         (only-in "../../runtime/memory/reflection.rkt" current-reflection-llm-fn)
         (only-in "../../runtime/session/session-events.rkt" current-mid-session-bridge-enabled)
         (only-in "../../sandbox/gateway-bridge.rkt"
                  current-execution-plane-enabled
                  current-execution-plane-timeout-ms
                  current-worker-command
                  current-worker-args
                  execute-via-worker
                  current-remote-executor
                  execute-via-remote-envelope)
         (only-in "../../agent/roles/tool-gateway.rkt"
                  current-tool-executor
                  current-remote-tool-executor
                  current-routing-policy)
         (only-in "../../agent/verification/verifier-core.rkt"
                  current-verifier-enabled
                  current-verifier-model
                  current-verifier-risk-threshold
                  current-verifier-provider)
         (only-in "../../runtime/settings-query.rkt" broker-enabled? broker-capability-secret))

(provide wire-runtime-parameters!
         reload-config!)

;; ============================================================
;; wire-runtime-parameters! — shared parameter wiring
;; ============================================================

;; Extract shared wiring from build-runtime-from-cli and reload-config!.
;; Both functions set the same ~12 runtime parameters from settings + profile.
;; H3a (v0.97.13): pure extraction, no behavioral change.
(define (wire-runtime-parameters! settings profile max-ctx-tokens)
  (apply-context-assembly-profile! profile max-ctx-tokens)
  ;; Startup log: task-state-aware assembly status
  (log-info "context-assembly: task-state-aware assembly ~a"
            (if (current-task-state-aware-assembly?) "enabled" "disabled"))
  (wire-security-config! settings)
  (current-mid-session-bridge-enabled (and (memq profile '(self-healing full))
                                           (setting-ref settings 'mid-session-bridge-enabled #f)))
  (define settings-budget (setting-memory-injection-budget settings))
  (cond
    [settings-budget (current-memory-injection-budget settings-budget)]
    [(memq profile '(self-healing full))
     (current-memory-injection-budget (quotient max-ctx-tokens 20))])
  (current-auto-extraction-enabled (setting-memory-auto-extraction-enabled? settings))
  (current-auto-extraction-min-confidence (setting-memory-auto-extraction-min-confidence settings))
  ;; v0.99.84: Wire tool-result extraction hook — Agent Core calls this
  ;; parameter; Runtime provides the implementation.
  (current-post-tool-result-hook
   (lambda (msgs sid root)
     (maybe-auto-extract-tool-results! msgs #:session-id sid #:project-root root)))
  (update-memory-policy! #:user-scope-enabled? (setting-memory-user-scope-enabled? settings))
  (current-auto-reflection-enabled (setting-memory-auto-reflection-enabled? settings))
  (current-auto-reflection-min-items (setting-memory-auto-reflection-min-items settings))
  (current-reflection-prompt-enabled (setting-reflection-prompt-enabled? settings))
  (let ([ad (setting-auto-distillation-enabled? settings)])
    (unless (eq? ad 'unset)
      (current-auto-distillation-enabled? ad)))
  ;; AXIS2-F08 (v0.98.14): Warn when memory backend active but reflection LLM missing
  (when (and (current-memory-backend) (not (current-reflection-llm-fn)))
    (log-warning "memory: reflection LLM not available — auto-reflection disabled"))
  ;; AXIS2-F13 (v0.98.14): Wire goal-loop from settings (default #t)
  (current-goal-loop-enabled? (setting-ref* settings '(goal-loop-enabled?) #t))
  ;; v0.99.2: Wire execution plane from settings (default #f = disabled)
  ;; H2 (v0.99.3): Inject executor into tool-gateway via parameter
  (when (execution-plane-enabled? settings)
    (current-execution-plane-enabled #t)
    (current-execution-plane-timeout-ms (execution-plane-timeout-ms settings))
    (let ([cmd (execution-plane-command settings)])
      (when cmd
        (current-worker-command cmd)))
    (current-worker-args (execution-plane-worker-args settings))
    (current-tool-executor execute-via-worker))
  ;; v0.99.12 W3: Wire broker routing (remote executor).
  ;; When broker is enabled AND capability secret is set:
  ;;   - Enable risk-based routing (high/critical → remote)
  ;;   - Wire remote executor function into tool-gateway
  ;; When broker is enabled but secret is unset: fail fast.
  (when (broker-enabled? settings)
    (define secret (broker-capability-secret settings))
    (cond
      [(not secret)
       (error 'wire-runtime-parameters!
              "mas.broker.enabled is #t but mas.broker.capability-secret is not set")]
      [else
       ;; Enable risk-based routing policy
       (current-routing-policy 'risk-based)
       ;; Wire remote executor function
       (current-remote-tool-executor execute-via-remote-envelope)
       ;; The actual remote-executor connection is established lazily
       ;; by the wiring layer when broker is enabled.
       ;; current-remote-executor starts as #f; execute-via-remote-envelope
       ;; returns a clear error until a connection is established.
       (log-info "broker: enabled, risk-based routing active")]))
  ;; v0.99.5: Wire verifier agent from settings (default #f = disabled)
  ;; When enabled, verification gate runs between executing and idle/done.
  (current-verifier-enabled (verifier-enabled? settings))
  (let ([vmodel (verifier-model settings)]) (current-verifier-model vmodel))
  (current-verifier-risk-threshold (verifier-risk-threshold settings)))

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
  ;; v0.97.6 LF3: Re-apply context-assembly profile with updated context window
  (define new-profile
    (dict-ref base-config 'context-assembly-profile (setting-context-assembly-profile new-settings)))
  (define new-model-name (dict-ref base-config 'model #f))
  (define new-cw (model-registry-context-window new-reg (or new-model-name "")))
  (define new-max-ctx (or new-cw (dict-ref base-config 'max-context-tokens 128000)))
  ;; H3a: Wire all runtime parameters via shared function
  (wire-runtime-parameters! new-settings new-profile new-max-ctx)
  ;; v0.99.6: Re-wire verifier provider from session config
  (when (dict-ref base-config 'provider #f)
    (current-verifier-provider (dict-ref base-config 'provider)))
  ;; Re-resolve permissions from reloaded settings while preserving the
  ;; positive CLI override marker. Missing/invalid settings remain strict.
  (define new-permission-config
    (resolve-permission-config new-settings
                               #:cli-auto-approve? (dict-ref base-config 'cli-auto-approve? #f)
                               #:tui? (dict-ref base-config 'tui-interactive-approval? #f)))
  ;; Return updated config + registry
  (values (hash->session-config (hash-set* (session-config->hash base-config)
                                           'settings
                                           new-settings
                                           'model-registry
                                           new-reg
                                           'permission-config
                                           new-permission-config))
          new-reg))
