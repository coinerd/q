#lang racket/base

;; extensions/gsd/policy.rkt — Unified GSD Policy Engine
;; STABILITY: evolving
;;
;; F4 fix: Single decision function for all guard checks.
;; Consolidates BLOCKED-TOOLS from state-machine.rkt and
;; write guards from core.rkt into one policy module.

(require racket/match
         racket/string)

(provide policy-decision
         policy-decision?
         policy-allowed?
         policy-blocked?
         policy-reason
         policy-tags
         gsd-decide-action
         blocked-tools-for
         current-gsd-wave-timeout-seconds
         current-gsd-wave-timeout-retries
         current-gsd-wave-no-change-retries
         current-gsd-wave-failure-context
         current-gsd-campaign-infra-retries
         current-gsd-campaign-infra-retry-delay
         current-gsd-wave-max-iterations
         current-gsd-max-consecutive-tool-calls
         gsd-session-iteration-budget)

;; ============================================================
;; Policy decision struct
;; ============================================================

(struct policy-decision (allowed? reason tags) #:transparent)

(define (policy-allowed? d)
  (policy-decision-allowed? d))

(define (policy-blocked? d)
  (not (policy-decision-allowed? d)))

(define (policy-reason d)
  (policy-decision-reason d))

(define (policy-tags d)
  (policy-decision-tags d))

;; ============================================================
;; Bounded campaign budgets
;; ============================================================

(define (positive-real-guard who)
  (lambda (value)
    (if (and (real? value) (positive? value))
        value
        (raise-argument-error who "positive-real?" value))))

(define (positive-integer-guard who)
  (lambda (value)
    (if (exact-positive-integer? value)
        value
        (raise-argument-error who "exact-positive-integer?" value))))

(define (nonnegative-integer-guard who)
  (lambda (value)
    (if (exact-nonnegative-integer? value)
        value
        (raise-argument-error who "exact-nonnegative-integer?" value))))

;; One wave owns one fresh runtime session and cannot run indefinitely. These
;; parameters make the production policy explicit while keeping focused tests
;; deterministic. Tool calls are bounded by the runtime's existing
;; consecutive-tool circuit breaker; no command or shell parsing is involved.
;; v1.00.03: default raised 1800 → 3600 s (per-wave budget). A live W5 wave
;; with a scoped verify command legitimately needs up to ~40 min of tool-turn
;; work; the old 30-minute cap policy-cancelled real implementation waves
;; that were mid-edit on a large TUI file. The budget is now also overridable
;; per-campaign via ~/.q/config.json (wave-timeout-seconds) and via
;; /go --wave-timeout=SECONDS.
;; 2026-08-18 (BUG-0017 follow-up): default raised 3600 → 7200 s. A live W3
;; wave performing area-by-area metadata migration + a grouped-runner audit
;; consumed the full 3600 s budget while making steady, verifiable progress
;; and was killed mid-fix on a real runner defect (ZERO_PARSED/exit-guard).
(define current-gsd-wave-timeout-seconds
  (make-parameter 7200 (positive-real-guard 'current-gsd-wave-timeout-seconds)))

;; BUG-0017 follow-up (2026-08-18): a wave whose run exceeds the per-wave
;; budget (timed-out) is retried with a fresh session up to this many times,
;; mirroring the campaign's LLM provider-retry ceiling
;; (current-provider-retry-max-retries = 5 in go-orchestrator). The attempt is
;; NOT consumed by retries — only final exhaustion persists interrupted.
(define current-gsd-wave-timeout-retries
  (make-parameter 5 (nonnegative-integer-guard 'current-gsd-wave-timeout-retries)))

;; v1.00.17 W3 (#9515): a wave whose verifier outcome is "no wave target
;; files changed" (the executor returned without editing any declared target
;; — typically transient exploration paralysis, e.g. v1.00.16 W3 attempt-2)
;; is retried this many times with the verifier's message appended to the
;; executor prompt as failure context. Bounded (default 1): exactly one
;; context-carrying retry, then permanent failure. The retry consumes a
;; fresh fenced attempt exactly like a timeout retry, so at-least-once
;; semantics are preserved.
(define current-gsd-wave-no-change-retries
  (make-parameter 1 (nonnegative-integer-guard 'current-gsd-wave-no-change-retries)))

;; v1.00.18 W? (BUG-0024): campaign-level infra-failure retry ceiling. When a
;; wave run classifies as 'infra-failed (D8 #9357: provider/network/SSE
;; transient failure) the coordinator re-attempts the SAME wave automatically
;; up to this many times with exponential backoff instead of returning
;; wave-cancelled ("re-run /go") and stopping the whole campaign. Observed 5x
;; during the v1.00.17 campaign; each stop required a manual /retry. The
;; attempt is NOT consumed by an automatic retry (D8 semantics preserved —
;; the attempt-count rollback in the infra branch keeps it so). Only bound
;; exhaustion stops the campaign, with an aggregated message listing all
;; failure timestamps. Settings key: gsd.campaign-infra-retries.
(define current-gsd-campaign-infra-retries
  (make-parameter 3 (nonnegative-integer-guard 'current-gsd-campaign-infra-retries)))

;; Backoff delay (seconds) for the Nth automatic infra retry (1-based).
;; Default: 30s → 60s → 120s → flat 120s. Parameterized as a function so
;; tests pin it to 0 and keep the retry loop deterministic.
(define (infra-retry-delay-guard who)
  (lambda (value)
    (if (and (procedure? value) (procedure-arity-includes? value 1))
        value
        (raise-argument-error who "(-> exact-positive-integer? real?)" value))))

(define (default-infra-retry-delay attempt)
  (min 120 (* 30 (expt 2 (max 0 (sub1 attempt))))))

(define current-gsd-campaign-infra-retry-delay
  (make-parameter default-infra-retry-delay
                  (infra-retry-delay-guard 'current-gsd-campaign-infra-retry-delay)))

;; v1.00.17 W3 (#9515): rendered failure-context block (string) that the
;; prompt layer suffixes to the wave executor prompt while the orchestrator
;; runs a no-change retry; #f (default) outside a retry. Carried as a
;; parameter because the runner port callback executes in the campaign
;; thread's dynamic extent.
(define current-gsd-wave-failure-context (make-parameter #f))
;; v1.00.03 user finding: the old 50-iteration wave budget made the derived
;; hard limit only 80 (resolve-max-iterations-hard = max(iter*8/5, 80)), so a
;; real implementation wave was policy-cancelled at iteration 80 mid-work
;; ("[SYS] [executing... iteration 79, 1 remaining before hard stop]" then
;; wave-cancelled). A wave is a fresh session doing a bounded chunk of work
;; within the 3600s timeout; the iteration ceiling is a runaway guard, not a
;; completion cap. Raise it so implementation waves are never iteration-killed
;; while the timeout and the 100-consecutive-tool breaker still bound runaway
;; loops. The derived hard limit scales with the budget.
(define current-gsd-wave-max-iterations
  (make-parameter 2000 (positive-integer-guard 'current-gsd-wave-max-iterations)))
;; D3 (#9351): raised from 30 — incident 81f9be4b W2 died at exactly 30
;; consecutive tool-only turns (attempt-3) while the identical wave completed
;; in the main session with 24-consecutive bursts. Implementation waves
;; legitimately exceed the old default; 100 kept the runaway-loop guard
;; without policy-killing real work. v1.00.03 live finding (2026-08-17): a W3
;; executor spent 100 consecutive tool-only turns on a productive edit-retry
;; loop (read -> grep -> re-edit after a leading-whitespace mismatch) and was
;; policy-killed at the ceiling. The edit-tool whitespace auto-fallback
;; (#9366) removes the root-cause trigger, but recovery loops that re-read and
;; re-grep before retrying should not be killed either.
;; BUG-0016 (2026-08-18): the breaker is now progress-aware (compute-next-
;; counters resets on distinct-file edits), and the campaign ceiling is 600
;; so a bulk metadata migration that touches hundreds of distinct files is
;; never mistaken for a runaway loop.
(define current-gsd-max-consecutive-tool-calls
  (make-parameter 600 (positive-integer-guard 'current-gsd-max-consecutive-tool-calls)))

(define (gsd-session-iteration-budget configured-max)
  (unless (exact-positive-integer? configured-max)
    (raise-argument-error 'gsd-session-iteration-budget "exact-positive-integer?" configured-max))
  (min configured-max (current-gsd-wave-max-iterations)))

;; ============================================================
;; Consolidated tool blocklist (from state-machine.rkt BLOCKED-TOOLS)
;; ============================================================

(define (blocked-tools-for mode)
  (case mode
    [(plan-written) '("edit" "write" "bash")]
    [(executing) '("planning-write")]
    [(verifying) '("edit" "write" "bash" "planning-write")]
    [else '()]))

;; ============================================================
;; Unified decision function
;; ============================================================

;; (gsd-decide-action ctx action) → policy-decision
;; ctx: hasheq with 'mode, 'tool, 'target-path, 'pinned-dir
;; action: symbol — 'tool-call, 'write-file, 'edit-plan, 'transition
(define (gsd-decide-action ctx action)
  (define mode (hash-ref ctx 'mode 'idle))
  (case action
    [(tool-call)
     (define tool (hash-ref ctx 'tool ""))
     (define blocked (blocked-tools-for mode))
     (cond
       [(not (and (string? tool) (non-empty-string? tool)))
        (policy-decision #f "Malformed tool call: non-empty tool name required" '(tool-malformed))]
       [(member tool blocked)
        (policy-decision #f
                         (format "Tool '~a' blocked in ~a mode" tool mode)
                         (list 'tool-blocked mode tool))]
       [else (policy-decision #t #f (list 'tool-allowed mode tool))])]
    [(write-file)
     (define target (hash-ref ctx 'target-path ""))
     (define pinned (hash-ref ctx 'pinned-dir ""))
     (if (and (eq? mode 'executing) (in-planning-dir? target pinned))
         (policy-decision #f
                          (format "Cannot write to ~a during execution" target)
                          (list 'write-blocked 'executing target))
         (policy-decision #t #f (list 'write-allowed mode target)))]
    [(edit-plan)
     (if (eq? mode 'executing)
         (policy-decision #f "Cannot edit plan during execution" '(edit-plan-blocked))
         (policy-decision #t #f '(edit-plan-allowed)))]
    [(transition)
     (define target-mode (hash-ref ctx 'target-mode #f))
     (policy-decision #t #f (list 'transition mode target-mode))]
    [else
     (policy-decision #f
                      (format "Unknown GSD policy action: ~a" action)
                      (list 'unknown-action 'blocked action))]))

;; ============================================================
;; Internal helpers
;; ============================================================

;; Path normalization for security (prevents .. traversal)
(require racket/path)

(define (in-planning-dir? target pinned)
  (and (string? target)
       (non-empty-string? target)
       (string? pinned)
       (non-empty-string? pinned)
       (let ([ct (with-handlers ([exn:fail? (λ (_) target)])
                   (path->string (simple-form-path (string->path target))))]
             [cp (with-handlers ([exn:fail? (λ (_) pinned)])
                   (path->string (simple-form-path (string->path pinned))))])
         (or (string=? ct cp) (string-prefix? ct (string-append cp "/"))))))
