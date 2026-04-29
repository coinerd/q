;; docs/architecture/dependency-policy.rktd
;;
;; Machine-readable dependency policy for q architecture layers.
;; Tests in test-arch-boundaries.rkt and test-arch-fitness.rkt
;; load this file instead of hardcoding exception lists.
;;
;; Schema: ((section . data) ...)
;; Sections: layers, known-exceptions, module-size, complexity-budgets

(
 ;; Layer definitions with max boundary exceptions
 (layers
  (runtime
   (max-exceptions . 7)
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
              (runtime-helpers.rkt . "hook dispatch for session events")
              (tool-coordinator.rkt . "tool execution orchestration")
              (turn-orchestrator.rkt . "context assembly + provider turn (sole boundary module)")
              (package.rkt . "package audit reads extension manifests")
              (extension-catalog.rkt . "extension loading/discovery")))
  (extensions . ((dialog-api.rkt . "TUI dialog interface")
                 (ui-surface.rkt . "TUI UI surface interface"))))

 ;; Module size configuration
 (module-size
  (max-lines . 900)
  (known-large . ("tui/state.rkt" "extensions/racket-tooling.rkt")))

 ;; Complexity budgets (informational in v0.22.8, enforced in v0.23.0)
 (complexity-budgets
  (max-lines-per-module . 900)
  (max-function-length . 80)
  (max-require-fan-in . 20))
)
