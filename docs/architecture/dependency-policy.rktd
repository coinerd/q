;; docs/architecture/dependency-policy.rktd
;;
;; Machine-readable dependency policy for q architecture layers.
;; Tests in test-arch-boundaries.rkt and test-arch-fitness.rkt
;; load this file instead of hardcoding exception lists.
;;
;; Schema: ((section . data) ...)
;; Sections: layers, known-exceptions, module-size, complexity-budgets,
;;           deep-module-conventions

;; Layer definitions with max boundary exceptions
((layers
  (runtime
   (max-exceptions . 12)
   (forbidden-from . (llm tools extensions))
   (rationale
    . "Runtime should not import upward into tools/extensions except via documented exceptions (includes iteration/ submodules and layer-adapters)"))
  (agent (max-exceptions . 2)
       (forbidden-from . (llm))
       (rationale . "Agent layer types; minimal cross-boundary imports"))
  (tui (max-exceptions . 0)
       (forbidden-from . (llm tools))
       (allowed-from . (runtime agent extensions util)))
  (extensions (max-exceptions . 5)
              (forbidden-from . (tui))
              (rationale . "Extensions may import from runtime/core but never TUI"))
  (llm (max-exceptions . 0)
       (forbidden-from . (runtime tools extensions))
       (rationale . "LLM providers are leaf modules with no upward imports"))
  (browser (max-exceptions . 0)
           (forbidden-from . (runtime tui extensions))
           (allowed-from . (agent util))
           (rationale . "Browser subsystem peer of tools — imports only agent/event and util. Never runtime/tui.")))
 ;; Known boundary exceptions (files that violate layer rules for documented reasons)
 ;; Format: ((filename (rationale . "...") (owner . "...") (revisit-by . "YYYY-MM-DD")) ...)
 ;; Owner: responsible module/component. Revisit-by: date to reassess necessity.
 ;; Gate test: undocumented exceptions (missing owner/revisit-by) will FAIL.
 (known-exceptions
  (runtime
   . ((layer-adapters.rkt
       (rationale . "explicit adapter facade routing tool/extension deps behind contained boundary")
       (owner . "runtime")
       (revisit-by . "2026-07-01"))
      (agent-session.rkt (rationale . "list-extensions via adapter (re-exports extension listing)")
                         (owner . "runtime")
                         (revisit-by . "2026-07-01"))
      (session/session-lifecycle.rkt
       (rationale . "injection-event-topic import (extracted from agent-session/iteration)")
       (owner . "runtime")
       (revisit-by . "2026-07-01"))
      (session/session-lifecycle-transitions.rkt
       (rationale . "pure FSM transitions extracted from session-lifecycle")
       (owner . "runtime")
       (revisit-by . "2026-09-01"))
      (runtime-helpers.rkt (rationale . "hook dispatch for session events (via adapter)")
                           (owner . "runtime")
                           (revisit-by . "2026-07-01"))
      (tool-coordinator.rkt (rationale . "tool execution orchestration (all imports via adapter)")
                            (owner . "tools")
                            (revisit-by . "2026-07-01"))
      (turn-orchestrator.rkt (rationale . "context assembly + provider turn (imports via adapter)")
                             (owner . "runtime")
                             (revisit-by . "2026-07-01"))
      (session/session-config.rkt (rationale . "central typed config (imports via adapter)")
                          (owner . "runtime")
                          (revisit-by . "2026-07-01"))
      (package.rkt (rationale . "package audit reads extension manifests")
                   (owner . "extensions")
                   (revisit-by . "2026-08-01"))
      (extension-catalog.rkt (rationale . "extension loading/discovery")
                             (owner . "extensions")
                             (revisit-by . "2026-08-01"))
      (session/session-switch.rkt
       (rationale . "dynamic-require to extensions for lazy loading (avoids circular dependency)")
       (owner . "runtime")
       (revisit-by . "2026-07-01"))
      (goal/goal-checks.rkt
       (rationale . "imports shell-risk from tools/ for command safety validation")
       (owner . "runtime")
       (revisit-by . "2026-08-01"))))
  (agent
   . ((iteration/loop-state.rkt
      (rationale . "typed require from extensions/api.rkt for opaque ExtRegistry")
      (owner . "agent")
      (revisit-by . "2026-08-01"))))
  (extensions
   .
   ((dialog-api.rkt (rationale . "TUI dialog interface") (owner . "tui") (revisit-by . "2026-07-01"))
    (ui-surface.rkt (rationale . "TUI UI surface interface")
                    (owner . "tui")
                    (revisit-by . "2026-07-01"))
    (widget-lifecycle.rkt
     (rationale . "Imports tui/component.rkt for q-component? type and make-q-component bridge")
     (owner . "extensions")
     (revisit-by . "2026-07-01"))
    (context.rkt
     (rationale . "imports runtime/session-types.rkt for context assembly (bidirectional — fragile)")
     (owner . "extensions")
     (revisit-by . "2026-07-01"))
    (ext-package-manager.rkt
     (rationale . "imports runtime/ for package lifecycle management (bidirectional — fragile)")
     (owner . "extensions")
     (revisit-by . "2026-07-01")))))
 ;; Module size configuration
 (module-size (max-lines . 900) (known-large . ()))
 ;; Complexity budgets (informational in v0.22.8, enforced in v0.23.0)
 (complexity-budgets (max-lines-per-module . 900)
                     (max-function-length . 80)
                     (max-require-fan-in . 14))
 ;; Deep module conventions (v0.27.0)
 ;; Documents the sub-module directories created by the Deep Module Refactoring.
 ;; Each convention specifies the parent module, sub-directory, and budget.
 (deep-module-conventions
  (tui/input (parent . "tui/input.rkt")
             (sub-modules . (completion-ops editing-ops history-ops state-types))
             (budget . 300)
             (convention . "Stateless operation functions. Parent re-exports."))
  (tui/keybindings (parent . "tui/tui-keybindings.rkt")
                   (sub-modules . (binding-resolver default-map mode-map))
                   (budget . 150)
                   (convention . "Keymap resolution and default maps."))
  (agent/iteration
   (parent . "agent/iteration.rkt")
   (sub-modules . (loop-state counters step-interpreter main-loop tool-turn-bridge loop-config loop-phases))
   (budget . 400)
   (convention . "Agent iteration loop. Migrated from runtime/ in v0.73.4. TR module in loop-state."))
  (runtime/iteration
   (parent . "runtime/iteration.rkt")
   (sub-modules
    .
    (fsm-types directive retry-policy tool-turn-bridge counters decision step-interpreter main-loop internal))
   (budget . 450)
   (convention
    . "Iteration loop decomposition. Pure logic in counters/decision, effectful in step-interpreter, orchestration in main-loop."))
  (runtime/session-persistence
   (parent . "runtime/session-lifecycle.rkt")
   (budget . 150)
   (convention . "Session persistence and crash recovery. Extracted from session-lifecycle for testability."))
  (runtime/session-index
   (parent . "runtime/session-index.rkt")
   (sub-modules . (schema query mutations))
   (budget . 400)
   (convention . "Session tree operations. Parent kept for consumer compatibility."))
  (runtime/context-assembly (parent . "runtime/context-assembly.rkt")
                            (sub-modules . (budgeting selection serialization config))
                            (budget . 250)
                            (convention . "Context budget, message selection, serialization, config."))
  ;; v0.80.4: Logical runtime sub-packages (not yet physical sub-dirs)
  ;; These document the intended grouping. Physical move deferred to v0.82+.
  (runtime/session-logical
   (convention . "Session lifecycle, persistence, migration, types, config, compaction, FSM, controls, events, mutations, index")
   (files . "session-store*, session-index*, session-persistence, session-migration, session-types, session-config, session-compaction, session-controls, session-events, session-mutation, session-fsm, session-lifecycle*, session-switch")
   (status . "logical-only"))
  (runtime/goal-logical
   (convention . "Goal system: runner, state, types, checks, evidence, evaluator, codec, agent-evaluator")
   (files . "goal-runner, goal-state, goal-types, goal-checks, goal-evidence, goal-evaluator, goal-codec, goal-agent-evaluator")
   (status . "logical-only"))
  (runtime/compaction-logical
   (convention . "Context compaction: compactor, prompts, hooks, LLM bridge")
   (files . "compactor, compactor-llm-bridge, compaction-hooks, context-summary, incremental-summarizer, branch-summarizer")
   (status . "logical-only"))
  (runtime/credential-logical
   (convention . "Credential backends: auth-store, credential-backend, oauth, credentials/ sub-dir")
   (files . "auth-store, credential-backend, oauth, oauth-callback, credentials/*")
   (status . "partial — credentials/ sub-dir exists; top-level facades remain"))
  (extensions/gsd-planning (parent . "extensions/gsd-planning.rkt")
                           (sub-modules . (command-normalization execution-policy plan-diff))
                           (budget . 130)
                           (convention . "Pure logic extracted from GSD planning."))
  (extensions/gsd (parent . "extensions/gsd-planning.rkt")
                  (sub-modules . (command-handlers tool-handlers))
                  (budget . 350)
                  (convention . "GSD command/tool dispatch extracted from gsd-planning facade."))
  (extensions/github/handlers (parent . "extensions/github/tool-handlers.rkt")
                              (sub-modules . (issue-ops pr-ops milestone-ops))
                              (budget . 425)
                              (convention . "GitHub tool handler decomposition."))
  (extensions/racket-tooling (parent . "extensions/racket-tooling-handlers.rkt")
                             (sub-modules . (analysis formatting rewrite))
                             (budget . 420)
                             (convention . "Racket tooling: check, edit, codemod handlers."))
  (agent/event-structs
   (parent . "agent/event-structs.rkt")
   (sub-modules . (base message-events provider-events session-events tool-events turn-events))
   (budget . 500)
   (convention . "Event struct definitions by domain.")))
 ;; Composition roots — modules with high fan-out (>25) that wire dependencies
 ;; from multiple layers. These are expected to have large require lists.
 ;; Adding new composition roots requires justification (e.g., new subsystem wiring).
 (composition-roots
  . (("runtime/agent-session.rkt"
      (fan-out . 33)
      (rationale . "Central session orchestration — wires runtime, tools, extensions, LLM, events"))
      ("tui/tui-render-loop.rkt"
      (fan-out . 29)
      (rationale . "TUI render loop — wires terminal, VDOM, state, layout, streaming"))
      ("agent/loop.rkt"
      (fan-out . 27)
      (rationale . "Agent main loop — wires iteration, streaming, dispatch, events"))
      ("runtime/session/session-lifecycle.rkt"
      (fan-out . 26)
      (rationale . "Session lifecycle — wires persistence, config, events, compaction"))
      ("agent/iteration/main-loop.rkt"
      (fan-out . 25)
      (rationale . "Iteration loop — wires step interpreter, FSM, tool bridge, streaming"))))
 ;; R-18: Pure modules — must not gain I/O imports
 ;; Format: ((module-path . allowed-current-impurities) ...)
 (pure-modules (decision . ("runtime/iteration/decision.rkt" . ()))
               (event-payloads . ("util/event-payloads.rkt" . ()))
               (event-codec . ("util/event-codec.rkt" . ())))
 ;; R-19: Parser modules — must not require I/O modules
 ;; Format: (module-path ...)
 (parser-modules ("extensions/gsd/command-parser.rkt" "tui/command-parse.rkt" "cli/args.rkt"))
 ;; R-20: Provide surface budget
 ;; Alert when public API of a single module exceeds this count.
 (provide-surface-budget (max-per-module . 150) (alert-threshold . 100))
 ;; R-21: Hotspot budget policy
 ;; Files with hotspot score > warn-threshold should have a risk-note entry.
 ;; Files with score > block-threshold without a risk-note will fail CI.
 ;; Format of risk-notes: ((file-path (risk . "description") (owner . "team")) ...)
 (hotspot-budget
  (warn-threshold . 15000)
  (block-threshold . 20000)
  (risk-notes
   . (("runtime/agent-session.rkt"
       (risk . "Central session orchestration; high fan-in, hard to decompose further")
       (owner . "runtime"))
      ("runtime/settings.rkt"
       (risk . "Central settings with 221 provides — provider schemas, credential helpers, model defaults; high surface by design")
       (owner . "runtime"))
      ("runtime/session/session-lifecycle.rkt" (risk . "Session lifecycle FSM; high state complexity")
                                       (owner . "runtime"))
      ("scripts/run-tests.rkt"
       (risk
        . "Parallel test runner with subprocess orchestration, output parsing, and suite management")
       (owner . "tools"))
      ("sandbox/subprocess.rkt"
       (risk
        . "Shell subprocess execution with resource limits, timeout handling, and output capture; security-sensitive boundary")
       (owner . "hardening"))
      ("tui/tui-render-loop.rkt"
       (risk . "TUI render loop with streaming state, VDOM diffing, and layout; high visual-coupling")
       (owner . "tui"))
      ("runtime/session/session-store-tree.rkt"
       (cycle-resolved . "v0.74.1")
       (risk . "Session store tree operations; formerly circular with session-store.rkt via lazy-require; now uses parameter injection")
       (owner . "runtime"))
      ("extensions/gsd/core.rkt"
       (risk . "GSD planning core with wave execution, state machine dispatch, and high co-change coupling")
       (owner . "extensions"))
      ("tui/commands.rkt"
       (risk . "TUI command dispatch with slash command routing and mode-specific handling")
       (owner . "tui"))
      ("tui/state-events.rkt"
       (risk . "TUI state event dispatch; large enum of UI event types with high change frequency")
       (owner . "tui"))
      ("tui/terminal-input.rkt"
       (risk . "Terminal input handling with mouse, key, and paste event parsing")
       (owner . "tui"))
      ("interfaces/sessions.rkt"
       (risk . "Session interface with SDK bindings, large provide surface")
       (owner . "interfaces"))
      ("llm/gemini.rkt"
       (risk . "Gemini provider with streaming, tool calling, and multi-modal support")
       (owner . "provider"))
      ("tools/scheduler.rkt"
       (risk . "Tool scheduler with parallel execution, timeout management, and coordination")
       (owner . "tools"))
      ("runtime/turn-orchestrator.rkt"
       (risk . "Turn orchestration with streaming integration; high complexity score 25051")
       (owner . "runtime"))
      ("llm/stream.rkt"
       (risk . "LLM streaming abstraction with SSE parsing, chunk accumulation, and provider-specific handling")
       (owner . "provider"))
      ("llm/anthropic.rkt"
       (risk . "Anthropic Claude provider with streaming, tool use, and extended thinking")
       (owner . "provider"))
      ("extensions/image-pipeline.rkt"
       (risk . "Image pipeline with format detection, resizing, and validation")
       (owner . "extensions"))
      ("tui/terminal.rkt"
       (risk . "Terminal abstraction layer with platform-specific rendering and CSI sequences")
       (owner . "tui"))
      ("tui/state-ui.rkt"
       (risk . "TUI UI state management with overlay, selection, and widget state")
       (owner . "tui"))
))))
