#lang racket/base

;; wiring/run-modes/cli.rkt — build-runtime-from-cli
;;
;; v0.99.43 W0: Extracted from wiring/run-modes.rkt (monolithic 658-line file).
;; Contains the full CLI→runtime construction: event bus, tool registry,
;; provider, extension registry, session directory, system prompt sections,
;; MCP server wiring, and the final session-config assembly.
;;
;; STABILITY: stable — re-exported through wiring/run-modes.rkt facade.

(require racket/dict
         racket/file
         racket/path
         racket/string
         "../../interfaces/cli.rkt"
         (only-in "../../runtime/agent-session.rkt"
                  make-agent-session
                  resume-agent-session
                  run-prompt!
                  session-id
                  session-history
                  fork-session
                  close-session!
                  agent-session?
                  open-or-resume-session)
         "../../runtime/settings.rkt"
         "../../skills/types.rkt"
         (only-in "../../skills/resource-loader.rkt" skills-summary-section)
         (only-in "../../skills/frontmatter.rkt" parse-skill-frontmatter-extended)
         "../../runtime/provider/model-registry.rkt"
         (only-in "../../runtime/provider/provider-factory.rkt" build-provider)
         "../../tools/tool.rkt"
         (only-in "../../tools/permission-gate.rkt" make-strict-permission-config)
         (only-in "../../tools/registry-defaults.rkt" register-default-tools!)
         "../../util/event/event-bus.rkt"
         (only-in "../../util/event/event.rkt" make-event)
         (only-in "../../runtime/session/session-config.rkt"
                  hash->session-config
                  session-config?
                  session-config->hash
                  current-goal-loop-enabled?
                  apply-context-assembly-profile!)
         (only-in "../mode-helpers.rkt"
                  wire-security-config!
                  wire-timeouts!
                  make-trace-logger
                  start-trace-logger!
                  project-tree->string
                  resolve-permission-config)
         (only-in "../extension-setup.rkt" make-wired-extension-registry load-extensions-from-dir!)
         (only-in "../../runtime/context-assembly/config.rkt" current-task-state-aware-assembly?)
         (only-in "../../runtime/context-assembly/memory-builder.rkt" current-memory-injection-budget)
         (only-in "../../runtime/memory/auto-extraction.rkt"
                  current-auto-extraction-enabled
                  current-auto-extraction-min-confidence)
         (only-in "../../runtime/memory/service.rkt"
                  update-memory-policy!
                  current-auto-reflection-enabled
                  current-auto-reflection-min-items
                  current-memory-backend)
         (only-in "../../runtime/iteration/step-executor.rkt" current-reflection-prompt-enabled)
         (only-in "../../runtime/context-assembly/auto-distillation.rkt"
                  current-auto-distillation-enabled?
                  current-llm-distill-fn)
         (only-in "../../runtime/context-assembly/llm-callbacks.rkt"
                  make-distill-callback
                  make-reflection-callback)
         (only-in "../../runtime/memory/reflection.rkt" current-reflection-llm-fn)
         (only-in "../../extensions/gsd/state-machine.rkt" gsm-ctx-current gsd-max-rework-iterations)
         (only-in "../../extensions/gsd/session-state.rkt" current-gsd-ctx current-gsd-session-id)
         (only-in "../../runtime/gsd-query.rkt" current-gsd-mode-query)
         (only-in "../../runtime/session/session-events.rkt" current-mid-session-bridge-enabled)
         (only-in "../../sandbox/gateway-bridge.rkt"
                  current-execution-plane-enabled
                  current-execution-plane-timeout-ms
                  current-worker-command
                  current-worker-args
                  execute-via-worker
                  shutdown-worker!
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
         (only-in "../../agent/blackboard.rkt" make-blackboard current-blackboard)
         (only-in "../../agent/blackboard-subscriber.rkt"
                  start-blackboard-subscriber!
                  rebuild-blackboard-from-log!)
         (only-in "../../runtime/context-assembly/state-aware-builder.rkt"
                  current-blackboard-injection-enabled)
         ;; v0.99.8: Registry + hot-swap
         (only-in "../../agent/registry-defaults.rkt" register-default-agents!)
         (only-in "../../agent/registry.rkt"
                  pin-current-versions
                  set-hot-swap-enabled!
                  set-session-active!
                  register-agent!)
         (only-in "../../agent/registry-watcher.rkt" start-registry-watcher!)
         (only-in "../../agent/roles/supervisor.rkt" current-use-registry)
         ;; v0.99.9 W4: MCP config + adapter
         (only-in "../../runtime/settings-query.rkt"
                  mcp-enabled?
                  mcp-server-enabled?
                  mcp-server-transport
                  broker-enabled?
                  broker-remote-host
                  broker-remote-port
                  broker-capability-secret
                  broker-cert-dir
                  auto-reload-enabled?
                  verifier-max-rework-iterations)
         ;; v0.99.21 §4.1: MAS delegation guidance
         (only-in "../../agent/mas-guidance.rkt" build-mas-delegation-guidance)
         (only-in "../../extensions/mcp-adapter.rkt" run-mcp-stdio-server! current-mcp-execute-fn)
         (only-in "../../tools/scheduler.rkt" run-tool-batch scheduler-result-results)
         ;; v0.99.23 §5.1: Agent pool limit for --agent-pool CLI flag
         (only-in "../../tools/builtins/spawn-subagent.rkt" current-agent-pool-limit)
         ;; v0.99.43 W0: Shared plumbing extracted to sub-modules
         (only-in "base.rkt"
                  make-mcp-governed-execute-fn
                  make-mcp-event-publisher
                  filter-workflow-skills)
         (only-in "loop.rkt" wire-runtime-parameters!))

(provide build-runtime-from-cli)

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
  (define tui-interactive-approval? (eq? (cli-config-mode cfg) 'tui))
  (define permission-config
    (resolve-permission-config settings
                               #:cli-auto-approve? (hash-ref base-config 'cli-auto-approve? #f)
                               #:tui? tui-interactive-approval?))

  ;; Load resources (system instructions, skills, templates)
  (define global-resources (load-global-resources))
  (define project-resources (load-project-resources project-dir))
  (define all-resources (merge-resources global-resources project-resources))
  (define system-instrs (resource-set-instructions all-resources))

  ;; Progressive skill disclosure: inject skill summaries into system prompt
  ;; Full SKILL.md content is available on-demand via the `read` tool
  (define skill-section (skills-summary-section (resource-set-skills all-resources)))

  ;; v0.99.26 §5.2: Detect mas-workflow skills and inject workflow guidance.
  ;; When workflow skills are present, tell the agent how to execute them.
  (define workflow-section
    (let ([wf-skills (filter-workflow-skills (resource-set-skills all-resources))])
      (if (null? wf-skills)
          #f
          (string-append
           "\n\n## Available Workflows\n\n"
           "Multi-agent workflows that can be triggered with `skill-route`:\n"
           (string-join (for/list ([s (in-list wf-skills)])
                          (format "- **~a**: ~a" (hash-ref s 'name "?") (hash-ref s 'description "")))
                        "\n")
           "\n\nExecute with: skill-route {action: \"workflow\", name: \"<skill-name>\", variables: {...}}"))))

  ;; v0.19.3 Wave 3: Inject project file tree into system prompt
  ;; This saves 1-2 exploration iterations per session.
  (define project-tree-section
    (let ([tree-str (project-tree->string project-dir)]) (if (string=? tree-str "") #f tree-str)))

  ;; v0.99.21 §4.1: Inject MAS delegation guidance when blackboard is enabled.
  ;; Makes the primary agent aware of spawn-subagent capabilities.
  (define mas-guidance-section
    (if (blackboard-enabled? settings)
        (build-mas-delegation-guidance settings)
        #f))

  ;; v0.99.23 §5.1: Inject parallel mode guidance when --parallel flag is active.
  ;; Rather than building a separate partitioning module, we instruct the
  ;; primary agent to use spawn-subagents for task partitioning.
  (define parallel-guidance-section
    (if (cli-config-parallel? cfg)
        (string-append "\n\n## Parallel Execution Mode\n"
                       "You are running in --parallel mode. Your FIRST action must be to:\n"
                       "1. Analyze the user's task and identify independent subtasks\n"
                       "2. Call spawn_subagents with one job per subtask\n"
                       "3. Synthesize the results into a single coherent answer\n"
                       "Do NOT work on the task directly — delegate everything to subagents.")
        #f))

  (define final-system-instrs
    (append system-instrs
            (if skill-section
                (list skill-section)
                (list))
            (if workflow-section
                (list workflow-section)
                (list))
            (if project-tree-section
                (list project-tree-section)
                (list))
            (if mas-guidance-section
                (list mas-guidance-section)
                (list))
            (if parallel-guidance-section
                (list parallel-guidance-section)
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

  ;; v0.99.6 H1: Set session ID from session-dir basename for event correlation.
  (current-gsd-session-id (path->string (file-name-from-path (string->path session-dir))))

  ;; Extension registry -- discover from global + project dirs
  ;; Load order: global (~/.q/extensions/) first, then project-local.
  ;; Project-local extensions override global ones (same name wins from later registration).
  (define-values (ext-reg ext-cmds ext-shortcuts) (make-wired-extension-registry bus project-dir))

  ;; T3-5: Wire GSD mode query — moved from tui/tui-init.rkt to fix layer violation.
  ;; TUI imports are clean; GSD state is queried via parameter set here in wiring layer.
  (current-gsd-mode-query (lambda () (gsm-ctx-current (current-gsd-ctx))))

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

  ;; v0.99.7 W6: Wire blackboard subscriber if enabled in config.
  ;; When mas.blackboard.enabled is true:
  ;;   1. Create blackboard container and set current-blackboard parameter
  ;;   2. Enable context injection into system prompt preamble
  ;;   3. Start event bus subscriber for live updates
  ;;   4. If trace.jsonl exists, rebuild state from it (crash recovery)
  (when (blackboard-enabled? settings)
    (define bb (make-blackboard))
    (current-blackboard bb)
    (current-blackboard-injection-enabled #t)
    (start-blackboard-subscriber! bus bb)
    ;; Crash recovery: replay from trace.jsonl if it exists from a prior run
    (define trace-path (build-path session-dir "trace.jsonl"))
    (when (file-exists? trace-path)
      (rebuild-blackboard-from-log! trace-path bb)))

  ;; v0.99.8 W3: Register agents and enable registry-based dispatch.
  ;; Always populate the registry (pure data, zero side effects).
  ;; Enable hot-swap only when mas.hot-swap.enabled is true.
  (register-default-agents!)
  ;; v0.99.15 W1 (F-14): Mark session as active so the registry
  ;; knows version switches should be deferred.
  (set-session-active! #t)
  (when (hot-swap-enabled? settings)
    ;; v0.99.15 W1 (F-13): Bridge config flag to registry parameter.
    ;; Without this, hot-swap-enabled? in registry.rkt stays #f even
    ;; when the config setting is #t.
    (set-hot-swap-enabled! #t)
    (current-use-registry #t)
    ;; v0.99.8 W4: Pin agent versions at session start.
    ;; Pinned versions ensure mid-session consistency.
    (pin-current-versions)
    ;; v0.99.20 W3 (§3.4): Start registry watcher for auto-reload.
    ;; When mas.hot-swap.auto-reload.enabled is true, monitor agent/roles/
    ;; for file changes and register new agent versions for hot-swap.
    ;; Default: #f (opt-in — even with hot-swap default-on).
    (when (auto-reload-enabled? settings)
      (define roles-dir (build-path project-dir "agent" "roles"))
      (start-registry-watcher!
       roles-dir
       ;; Callback: register new version via dynamic-require
       (lambda (role-name new-version module-path)
         (define factory-sym (string->symbol (format "make-~a-role" role-name)))
         (with-handlers ([exn:fail? (lambda (e)
                                      (log-warning "auto-reload: failed to load ~a: ~a"
                                                   module-path
                                                   (exn-message e)))])
           (define factory (dynamic-require module-path factory-sym))
           (register-agent! role-name new-version factory #:module-path module-path)
           (log-info "auto-reload: registered ~a v~a" role-name new-version))))))

  ;; v0.99.21 F-2: Wire verifier rework limit from settings.
  ;; The setting mas.verifier.max-rework-iterations was defined in v0.99.20
  ;; but never connected to the gsd-max-rework-iterations parameter.
  (gsd-max-rework-iterations (verifier-max-rework-iterations settings))

  ;; v0.99.9 W4: MCP server mode — alternative entry point.
  ;; When mas.mcp.server.enabled is true AND mas.mcp.enabled is true,
  ;; q runs as an MCP server over stdio.
  ;; H1 (v0.99.10 W1): Enforce master gate — both flags must be #t.
  ;; This blocks until stdin closes, then exits.
  ;; Feature-gated: zero behavioral change when disabled (default).
  (when (and (mcp-enabled? settings) (mcp-server-enabled? settings))
    (log-info "mcp: starting MCP stdio server (transport=~a)" (mcp-server-transport settings))
    ;; Wire tool execution: route MCP tools/call through the governed scheduler path.
    (current-mcp-execute-fn (make-mcp-governed-execute-fn
                             reg
                             #:working-directory project-dir
                             #:event-publisher (make-mcp-event-publisher bus (current-gsd-session-id))
                             #:runtime-settings settings
                             #:session-metadata
                             (hasheq 'session-id (current-gsd-session-id) 'route 'mcp)
                             #:permission-config permission-config))
    (run-mcp-stdio-server! reg)
    (exit 0))

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
               'permission-config
               permission-config
               ;; Persist interface authority so reload reselects interactive
               ;; strict approval without treating non-TUI modes as interactive.
               'tui-interactive-approval?
               tui-interactive-approval?
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
               trace-log
               ;; F1/EMIT-01 (v0.98.13 audit fix): Wire emit-event function so
               ;; extensions/ui-surface.rkt can publish ui.* events to the bus.
               ;; Takes a hash (from ui-event->hash) → creates event struct → publish!.
               'emit-event
               (lambda (h)
                 (define ev-type (hash-ref h 'type "ui.unknown"))
                 (define evt (make-event ev-type (current-inexact-milliseconds) #f #f h))
                 (publish! bus evt))
               ;; v0.99.60: Pass project-dir through to session-config so memory
               ;; context injection can filter by project root.
               'project-dir
               project-dir))

  ;; v0.14.2 Wave 3: Set per-model timeouts from settings
  (wire-timeouts! settings)

  ;; v0.25.2 (F3): Wire security config from config.json
  (wire-security-config! settings)

  ;; v0.79.0 (GAP-1): Wire context assembly profile — CLI overrides settings
  (define settings-profile (setting-context-assembly-profile settings))
  (define cli-profile (cli-config-context-profile cfg))
  (define profile (or cli-profile settings-profile))
  (define final-hash-with-profile (hash-set final-hash 'context-assembly-profile profile))
  ;; v0.97.4 GAP-E / v0.97.6 F4: Dynamic conclusion budget from model registry
  ;; Resolve context window from model config when available, fall back to
  ;; CLI config, then default 128000.
  (define model-cw
    (and model-reg
         effective-model-name
         (model-registry-context-window model-reg effective-model-name)))
  (define max-ctx-tokens (or model-cw (hash-ref base-config 'max-context-tokens 128000)))
  (log-info "context-assembly: model=~a context-window=~a (source: ~a)"
            (or effective-model-name "(default)")
            max-ctx-tokens
            (if model-cw "model-registry" "config-default"))
  ;; H3a: Wire all runtime parameters via shared function
  (wire-runtime-parameters! settings profile max-ctx-tokens)

  ;; v0.99.6 C1/H2: Wire verifier provider for gate calls.
  ;; Must be here (not in wire-runtime-parameters!) because prov is local.
  (current-verifier-provider prov)

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
  (define final-hash-with-auto-extract
    (hash-set* final-hash-with-memory
               'memory-auto-extraction-enabled
               auto-extract-enabled?
               'memory-auto-extraction-min-confidence
               auto-extract-min-conf))

  ;; LF1 (GAP-1/7): Wire LLM distill function and reflection LLM function
  ;; from provider. These enable LLM-powered conclusion distillation and
  ;; reflection merging when a provider is available. Both default to #f
  ;; H3b: Wire LLM callbacks via factory functions
  (when (and prov effective-model-name)
    (current-llm-distill-fn (make-distill-callback prov effective-model-name))
    (current-reflection-llm-fn (make-reflection-callback prov effective-model-name)))

  ;; v0.99.23 §5.1: Wire session-wide agent pool limit from --agent-pool flag.
  ;; Default 3 (matching current-agent-pool-limit's own default).
  (when (cli-config-agent-pool cfg)
    (current-agent-pool-limit (cli-config-agent-pool cfg)))

  (hash->session-config final-hash-with-auto-extract))
