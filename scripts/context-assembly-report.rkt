#lang racket/base

;; scripts/context-assembly-report.rkt — Context assembly activation summary
;;
;; Generates a text report of all context assembly feature flags and their status.
;; Usage: racket scripts/context-assembly-report.rkt

(require racket/format
         (only-in "../runtime/session/session-config.rkt"
                  current-task-state-aware-rollout-rate
                  current-context-assembly-profile
                  context-assembly-profile?)
         (only-in "../runtime/context-assembly/state-aware-builder.rkt"
                  current-task-state-aware-assembly?
                  current-graph-conclusion-selection?
                  current-conclusion-token-budget)
         (only-in "../runtime/context-assembly/auto-distillation.rkt"
                  current-auto-distillation-enabled?
                  current-llm-distill-fn)
         (only-in "../runtime/context-assembly/rollback-actions.rkt"
                  current-rollback-action-execution?))

(define (report)
  (displayln "═══════════════════════════════════════════════════════")
  (displayln "  Context Assembly Activation Report")
  (displayln "═══════════════════════════════════════════════════════")
  (displayln "")
  (printf "  Profile:                  ~a\n" (current-context-assembly-profile))
  (printf "  Rollout rate:             ~a\n" (current-task-state-aware-rollout-rate))
  (printf "  State-aware assembly:     ~a\n" (current-task-state-aware-assembly?))
  (printf "  Graph conclusion select:  ~a\n" (current-graph-conclusion-selection?))
  (printf "  Conclusion token budget:  ~a\n" (current-conclusion-token-budget))
  (printf "  Auto-distillation:        ~a\n" (current-auto-distillation-enabled?))
  (printf "  LLM distill function:     ~a\n" (if (current-llm-distill-fn) "configured" "none"))
  (printf "  Rollback action exec:     ~a\n" (current-rollback-action-execution?))
  (displayln "")
  (displayln "═══════════════════════════════════════════════════════"))

(report)
