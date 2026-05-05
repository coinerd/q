;; docs/architecture/dependency-policy.rktd
;;
;; Machine-readable dependency policy for q architecture layers.
;; Tests in test-arch-boundaries.rkt and test-arch-fitness.rkt
;; load this file instead of hardcoding exception lists.
;;
;; Schema: ((section . data) ...)
;; Sections: layers, known-exceptions, module-size, complexity-budgets,
;;           deep-module-conventions

(
 ;; Layer definitions with max boundary exceptions
 (layers
  (runtime
   (max-exceptions . 9)
   (forbidden-from . (llm tools extensions))
   (rationale . "Runtime should not import upward into tools/extensions except via turn-orchestrator.rkt"))
  (tui
   (max-exceptions . 0)
   (forbidden-from . (llm tools))
   (allowed-from . (runtime agent extensions util)))
  (extensions
   (max-exceptions . 2)
   (forbidden-from . (tui))
   (rationale . "Extensions may import from runtime/core but never TUI"))
  (llm
   (max-exceptions . 0)
   (forbidden-from . (runtime tools extensions))
   (rationale . "LLM providers are leaf modules with no upward imports")))

 ;; Known boundary exceptions (files that violate layer rules for documented reasons)
 ;; Format: ((filename . rationale) ...)
 (known-exceptions
  (runtime . ((agent-session.rkt . "extension registry + DI parameter setup")
              (iteration.rkt . "injection-event-topic value import (cannot lazy-require)")
              (session-lifecycle.rkt . "injection-event-topic import (extracted from agent-session/iteration)")
              (runtime-helpers.rkt . "hook dispatch for session events")
              (tool-coordinator.rkt . "tool execution orchestration")
              (turn-orchestrator.rkt . "context assembly + provider turn (sole boundary module)")
              (package.rkt . "package audit reads extension manifests")
              (extension-catalog.rkt . "extension loading/discovery")
              (session-switch.rkt . "dynamic-require to extensions for lazy loading (avoids circular dependency)")))
  (extensions . ((dialog-api.rkt . "TUI dialog interface")
                 (ui-surface.rkt . "TUI UI surface interface")
                 (context.rkt . "imports runtime/session-types.rkt for context assembly (bidirectional — fragile)")
                 (ext-package-manager.rkt . "imports runtime/ for package lifecycle management (bidirectional — fragile)"))))

 ;; Module size configuration
 (module-size
  (max-lines . 900)
  (known-large . ()))

 ;; Complexity budgets (informational in v0.22.8, enforced in v0.23.0)
 (complexity-budgets
  (max-lines-per-module . 900)
  (max-function-length . 80)
  (max-require-fan-in . 25))

 ;; Deep module conventions (v0.27.0)
 ;; Documents the sub-module directories created by the Deep Module Refactoring.
 ;; Each convention specifies the parent module, sub-directory, and budget.
 (deep-module-conventions
  (tui/input
   (parent . "tui/input.rkt")
   (sub-modules . (completion-ops editing-ops history-ops state-types))
   (budget . 300)
   (convention . "Stateless operation functions. Parent re-exports."))
  (tui/keybindings
   (parent . "tui/tui-keybindings.rkt")
   (sub-modules . (binding-resolver default-map mode-map))
   (budget . 150)
   (convention . "Keymap resolution and default maps."))
  (runtime/iteration
   (parent . "runtime/iteration.rkt")
   (sub-modules . (loop-state retry-policy tool-turn-bridge transition-logic))
   (budget . 350)
   (convention . "Iteration loop decomposition. Recursive core stays in parent."))
  (runtime/session-index
   (parent . "runtime/session-index.rkt")
   (sub-modules . (schema query mutations))
   (budget . 400)
   (convention . "Session tree operations. Parent kept for consumer compatibility."))
  (runtime/context-assembly
   (parent . "runtime/context-assembly.rkt")
   (sub-modules . (budgeting selection serialization))
   (budget . 250)
   (convention . "Context budget, message selection, serialization."))
  (extensions/gsd-planning
   (parent . "extensions/gsd-planning.rkt")
   (sub-modules . (command-normalization execution-policy plan-diff))
   (budget . 130)
   (convention . "Pure logic extracted from GSD planning."))
  (extensions/github/handlers
   (parent . "extensions/github/tool-handlers.rkt")
   (sub-modules . (issue-ops pr-ops milestone-ops))
   (budget . 425)
   (convention . "GitHub tool handler decomposition."))
  (extensions/racket-tooling
   (parent . "extensions/racket-tooling-handlers.rkt")
   (sub-modules . (analysis formatting rewrite))
   (budget . 420)
   (convention . "Racket tooling: check, edit, codemod handlers."))
  (agent/event-structs
   (parent . "agent/event-structs.rkt")
   (sub-modules . (base message-events provider-events session-events tool-events turn-events))
   (budget . 500)
   (convention . "Event struct definitions by domain."))))
