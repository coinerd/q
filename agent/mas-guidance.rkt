#lang racket/base

;; agent/mas-guidance.rkt — MAS delegation guidance for system prompt
;;
;; v0.99.21 §4.1: Makes the primary agent aware of spawn-subagent capabilities.
;; When the blackboard is enabled, this module produces a guidance string that
;; is injected into the system prompt, teaching the agent when and how to
;; delegate to subagents.

(require (only-in "../runtime/settings-query.rkt" hot-swap-enabled? verifier-enabled?))

(provide build-mas-delegation-guidance)

;; build-mas-delegation-guidance : q-settings? -> (or/c string? #f)
;;
;; Returns a MAS delegation guidance string suitable for system prompt inclusion.
;; Returns #f when the blackboard is NOT enabled (caller should skip injection).
;;
;; The guidance is approximately 500 tokens and covers:
;;   - When to use spawn-subagent (4 delegation patterns)
;;   - Practical guidelines (context passing, aggregation, rate limits)
;;   - Feature-specific notes (hot-swap, verifier) when those features are enabled
(define (build-mas-delegation-guidance settings)
  (string-append
   "\n\n## Multi-Agent Delegation\n"
   "You have access to `spawn-subagent` for delegating isolated subtasks "
   "and `spawn-subagents` for parallel batch execution. Use them when:\n\n"
   "1. **Parallelizable work**: A task can be split into 3+ independent subtasks "
   "(e.g., analyze multiple files, review different modules). "
   "Use `spawn-subagents` with separate jobs.\n\n"
   "2. **Isolated exploration**: A subtask needs read-only investigation without "
   "cluttering your context. Use `spawn-subagent` with role 'analyst'.\n\n"
   "3. **Second opinion**: You want an independent review of a plan or code change. "
   "Use `spawn-subagent` with role 'reviewer'.\n\n"
   "4. **High-risk isolation**: A subtask involves dangerous operations that should "
   "run in isolation. Use `spawn-subagent` with role 'executor'.\n\n"
   "### Guidelines\n"
   "- Pass relevant context in the task description — subagents start with a fresh context window.\n"
   "- Subagents return their final text result. Aggregate and synthesize.\n"
   "- Rate limit: 30 spawns/minute. Max 12 parallel jobs.\n"
   "- Use `spawn-subagent` for single tasks, `spawn-subagents` for parallel batches.\n"
   (if (hot-swap-enabled? settings)
       "- Agent roles can be hot-swapped at runtime — new role versions are loaded automatically.\n"
       "")
   (if (verifier-enabled? settings)
       "- A verifier agent may review your wave output. Plan for potential rework.\n"
       "")))
