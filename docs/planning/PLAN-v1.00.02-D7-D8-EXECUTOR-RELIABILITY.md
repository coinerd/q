# PLAN: D7 planning-read executor contract violation + D8 provider-stall→wave-failed (campaign 81f9be4b W3)

- **Created:** 2026-08-16 23:40 CEST
- **Status:** ANALYSIS COMPLETE — remediation waves proposed (not yet authorized)
- **Campaign context:** 81f9be4b… (v1.00.02 UX milestone #883, issues #9340–#9345). W3 ("Live
  user preferences") has now **failed 5 attempts** (attempt-4 22:28–22:39, attempt-5
  23:22–23:30, both in the user's live session). Each failure churns an attempt without
  advancing the wave — the campaign will keep retrying on the next `/go`, burning wall-clock
  and tokens on the same two defects.

---

## Part 1 — D7: `planning-read` extension-ctx/exec-context contract violation (confirmed defect)

### Symptom (observed live, attempts 1 & 5)

```
[FAIL] planning-read: tool 'planning-read' raised: extension-ctx-working-directory: contract violation
  expected: extension-ctx?
  given: (exec-context #<path:/home/user/src/q-agent/> (cancellation-token …) …)
```

Fails in **3 ms** (instant) every call. In attempt-5 the agent repeatedly retried it —
`iteration.exploration-loop` fired at iteration 5 ("pair repeated 8 times") and iteration 12
("pair repeated 9 times") with `planning-read` in the repeated pair. This burns iteration
budget and was a direct contributor to attempt-5's failure.

### Root cause chain (code-verified)

| Step | Code | Fact |
|---|---|---|
| 1 | `tools/scheduler-execution.rkt` (execute-committed-invocation, in-process branch) | scheduler calls `((tool-execute t) final-args exec-ctx)` — passes an **`exec-context`** struct |
| 2 | `extensions/dynamic-tools.rkt` `ext-register-tool!` → `wrapped-handler` | handler arity 2 → `(handler args exec-ctx)`; the second argument is the scheduler's exec-context |
| 3 | `extensions/gsd/tool-handlers.rkt` `handle-planning-read` → `get-base-dir args exec-ctx` | passes exec-context straight through |
| 4 | `extensions/gsd/tool-handlers.rkt:87` | `(and exec-ctx (ctx-cwd exec-ctx))` — **calls `ctx-cwd` on an exec-context** |
| 5 | `extensions/context.rkt:152` | `(define ctx-cwd extension-ctx-working-directory)` — contract requires **`extension-ctx?`** |
| 6 | — | `exec-context` ≠ `extension-ctx` → **contract violation** |

Note: `get-base-dir`'s own contract (tool-handlers.rkt:42) is
`(->* (hash?) ((or/c exec-context? #f)) path?)` — it **expects** an exec-context and then
applies the wrong accessor. The fix is unambiguous.

### Why interactive works but the executor doesn't

`get-base-dir` resolution order:

```racket
(or (hash-ref args 'base_dir #f)
    (current-pinned-dir)                    ;; ← interactive: SET (after /plan)
    (and exec-ctx (ctx-cwd exec-ctx))        ;; ← executor: reached (pinned-dir is #f) → crash
    (current-directory))
```

- Interactive TUI after `/plan`: `current-pinned-dir` is set → returns early → `ctx-cwd` never
  called → works.
- Campaign executor launch runs `reset-all-gsd-state!` (`extensions/gsd/core.rkt:77`), which
  sets `(set-pinned-dir! #f)` → falls through to `(ctx-cwd exec-ctx)` → contract violation.

### Impact

- `planning-read` is **100% broken inside campaign executors** (planning-write shares the same
  `get-base-dir` path — also broken).
- The agent keeps trying it (the EXECUTE prompt explicitly says "planning-read is allowed to
  check STATE or VALIDATION"), triggering exploration-loop detection and wasting iterations.
- Since W3 is an implementation wave that would legitimately use planning-read/planning-write,
  this materially degrades executor viability.

### Proposed fix (W0)

`get-base-dir` must dispatch on the actual context type:

```racket
(define (get-base-dir args [ctx #f])
  (or (hash-ref args 'base_dir #f)
      (current-pinned-dir)
      (and ctx
           (cond [(exec-context? ctx) (exec-context-working-directory ctx)]
                 [(extension-ctx? ctx) (ctx-cwd ctx)]
                 [else #f]))
      (current-directory)))
```

Requires importing `exec-context-working-directory` from `tools/exec-context.rkt` into
`extensions/gsd/tool-handlers.rkt`. Regression test: invoke `handle-planning-read` with a
real exec-context (and pinned-dir #f) → success; with extension-ctx → success.

---

## Part 2 — D8: one transient provider stall converts a viable wave to `wave-failed`

### Symptom (observed attempt-4, 22:28–22:39)

- 30/30 tool calls executed to completion (read ×17, grep ×12, set-task-state ×1) — D1
  watchdog held; ~11 minutes of real implementation progress.
- Then a provider stall mid-thinking:

```
20:37:08-09Z  model.stream.thinking  … (large thinking burst)
20:39:09Z     stream.turn.completed  reason=provider-stream-error
20:39:09Z     circuit-break.tripped  reason=progressive-stall
20:39:09Z     auto-retry.start       HTTP read timeout (120 seconds) waiting for SSE chunk
20:39:09Z     turn.completed         reason=error
```

- `prompt-run-result->outcome` (`extensions/gsd/go-orchestrator.rkt:112`) maps a non-completed
  loop result to `(wave-execution-outcome 'failed …)` → `run-campaign!` persists
  `wave-failed` → attempt consumed.

### The retry policy is tuned for interactive turns, not 30-minute waves

| Knob | Default | Source |
|---|---|---|
| SSE read timeout | **120 s** | `llm/stream.rkt` `http-read-timeout-default` |
| Provider retries | **2** | `runtime/provider-retry.rkt:162` `#:max-retries 2` |
| Base delay | 1000 ms | same |
| Progressive-stall breaker | **2 consecutive** minimal-output stalls | `runtime/auto-retry.rkt` `default-stall-max-consecutive = 2` |
| Cumulative ceiling | **300 s** | `runtime/auto-retry.rkt` `default-cumulative-ceiling-secs` |
| Wave budget | 1800 s | `extensions/gsd/policy.rkt` `current-gsd-wave-timeout-seconds` |

A single ~4-minute provider hiccup (2 × 120 s read timeouts) trips the progressive-stall
breaker → wave-failed. For a 30-minute implementation wave this is **far too shallow** — the
same policy makes sense for a short interactive turn (bail fast, ask user) but not for a
durable, attempt-tracked campaign wave.

### Root causes (contributing)

1. **Policy not campaign-aware.** The executor inherits the interactive provider-retry policy
   (`call-with-provider-retry` is hard-coded `#:max-retries 2`); nothing scales retries/stall
   tolerance to the wave budget.
2. **No infra-vs-code distinction at the wave boundary.** `wave-execution-outcome 'failed` is
   produced for provider stalls just the same as for genuine agent failure. The campaign has
   no "infrastructure degraded — retry same attempt" path.
3. **`meta-fix-predicate` is inert by default.** `run-campaign!` accepts
   `#:meta-fix-predicate` (default `(lambda (_) #f)`, go-orchestrator.rkt:240) — it exists to
   reset a wave to pending without consuming an attempt, but **nothing wires it** to
   provider-error outcomes. This is the intended hook for D8.

### Proposed fix (W1)

Two complementary changes (pick per acceptance criteria):

- **(A) Campaign-aware provider retry:** thread a wave-scale retry policy into the executor
  (higher `max-retries` / `stall-max-consecutive` / ceiling derived from the wave budget), or
  parameterize via `extensions/gsd/policy.rkt` (e.g. `current-gsd-wave-provider-retry-mult`).
- **(B) Infra-failure meta-fix:** classify the wave-failed outcome; when the failure is a
  provider/network error (stream-error, read timeout, circuit-breaker), reset the wave to
  `pending` WITHOUT consuming an attempt (use the existing `meta-fix` path), so a transient
  provider stall doesn't churn attempts. Consume an attempt only on genuine code/agent failure.

Acceptance criteria: a synthetic provider stall mid-wave (after N tool calls) leaves the wave
`pending` with the same attempt count (or retries inside the wave without failing it); a real
agent failure still consumes an attempt.

---

## Part 3 — Secondary findings (same audit)

### S2a — Lock file corruption (D4 follow-up)

`.planning/campaigns/81f9be4b….lock` now reads:

```
#hasheq((acquired . 1786915322) (owner . "") (pid . 1194981))VGGEKK0") (pid . 1194981))
```

- **No truncation before rewrite.** `acquire-lease` does `(file-position port 0)` then writes,
  but never truncates — a shorter new write leaves a stale tail from the previous owner
  (`…VGGEKK0") (pid…` is the tail of the attempt-4 owner `01M0645J64E772Q0ZFNVGGEKK0`).
- **Empty owner on re-dispatch.** The attempt-5 write has `(owner . "")` — the durable
  re-dispatch / auto-resume path did not carry the session id (D4 partially applied; the
  campaign-request path passes it, but the resume path reaches `run-campaign!` with the
  default). Fix: truncate (`#:exists 'truncate` or `file-truncate`) and audit every
  `run-campaign!`/`execute-campaign-request!` call site for a real session id.
- Impact: OS advisory lock still works (file content is cosmetic for locking), but the owner
  becomes unreadable → defeats D4's diagnostic purpose.

### S2b — D5 cwd duality recurred in attempt-5

Attempt-5 hit `File not found: /home/user/src/q-agent/q/tui/key-dispatch.rkt` — the wave plan
lists `q/tui/keybindings/key-dispatch.rkt`, but the executor resolved `q/tui/key-dispatch.rkt`
(repo-root vs `q/` base confusion again, the same D5 as attempt-3). Contributing factor in the
failed attempt; separate remediation (executor base-dir normalization or plan-path validation).

---

## Part 4 — Proposed remediation waves

| Wave | Scope | Acceptance |
|---|---|---|
| W0 | D7 fix: context-type dispatch in `get-base-dir` (+ regression tests) | planning-read/planning-write work under exec-context AND extension-ctx; executor no longer loops |
| W1 | D8: campaign-aware provider retry AND/OR infra-meta-fix (no attempt consumption on provider stalls) | synthetic provider stall does not consume an attempt; genuine failure still does |
| W2 | Lock hygiene: truncate before write + real session-id on all lease owners | lock file always a single well-formed hash with non-empty owner |
| W3 | D5: executor cwd/base-dir normalization (or plan-path validation) | executor resolves wave-plan file targets consistently (repo root / q/) |

Each wave: local branch, TDD, focused/Fast gates, PR CI, merge, planning sync. This plan is
canonical in `.planning/` and mirrors to `q/docs/planning/` per protocol.

## Open questions for the user

1. Authorize the D7 (W0) fix now? It is low-risk and directly unblocks executor quality
   (planning-read/planning-write).
2. For D8 (W1): prefer (A) scale the provider retry inside the wave, (B) meta-fix reset
   without attempt consumption, or both?
3. Campaign 81f9be4b: with W3 at 5 failed attempts, do you want to keep retrying via `/go`
   (it will keep churning while deepseek-v4-flash stalls), or pause the campaign until a
   stable provider/model is selected?
