# W0: Deferred Fixes + Stability Annotations + ADR

**Version:** 0.22.9
**Dependencies:** None (first wave)
**Effort:** M (local, ~90 min)
**Delivery:** Local commit (no remote pi)
**Findings:** INT-01, INT-02, BOTH-01, BOTH-02

## Tasks

### T1: INT-01 — Re-apply assert-payload wrappers to iteration.rkt

**File:** `runtime/iteration.rkt`
**Action:** Add event contract assertions at 11 high-value `emit-session-event!` call sites using racket_codemod one-at-a-time with format+compile check after each.

**Steps:**
1. Add import for event-contracts.rkt (9 contract names)
2. Add `assert-payload` helper function before shared helpers section
3. Wrap each emission point individually:
   - `"budget.warning"` → `budget-payload/c`
   - `"context.compacted"` (×2) → `compact-result-payload/c`
   - `"agent.blocked"` → `reason-payload/c`
   - `"message.injected.drain"` → `injection-count-payload/c`
   - `"turn.cancelled"` (×3) → `turn-cancelled-payload/c`
   - `"iteration.decision"` → `iteration-decision-payload/c`
   - `"runtime.error"` → `error-detail-payload/c`

**Verify:**
```bash
raco fmt -i runtime/iteration.rkt && raco make runtime/iteration.rkt
raco test tests/test-iteration.rkt
raco test tests/test-event-payload-contracts.rkt
```

### T2: INT-02 — Complete stability tier annotations

**Scope:** ~291 untagged source modules
**Action:** Add `;; STABILITY:` as second line of each module following the tier assignment table in the plan.

**Tier rules:**
- `stable`: event-bus, llm/*, settings, model-registry, safe-mode, extension-catalog, tools/builtins/*, interfaces/cli, interfaces/json-mode, util/*, cli/*, sandbox/*, wiring/*, main.rkt
- `evolving`: agent/loop, agent/state, agent/queue, runtime/session-store, runtime/compactor, runtime/session-index, runtime/context-manager, tui/*, interfaces/tui, interfaces/rpc-mode, extensions/github-integration, extensions/message-inject, extensions/compact-context
- `internal`: agent/state, agent/queue, runtime/context-builder, runtime/session-switch, runtime/session-migration, runtime/auto-retry, runtime/trace-logger, tools/permission-gate, tools/file-mutation-queue, extensions/quarantine, extensions/package-audit, skills/*, scripts/*

**Verify:** `grep -rl 'STABILITY:' --include='*.rkt' . | grep -v compiled | grep -v tests | wc -l` should equal source module count

### T3: BOTH-02 — Fix .gitignore for .rktd files

**File:** `q/.gitignore`
**Action:** Add after `*.rktd` line:
```
!docs/architecture/*.rktd
```

**Verify:** `git status docs/architecture/dependency-policy.rktd` shows tracked

### T4: BOTH-01 — Write ADR-0013

**File:** `q/docs/adr/0013-typed-racket-optional-args.md`
**Action:** Document rest-arg workaround for TR struct constructors (see plan for full content)

### T5: Update dependency-policy.rktd

**File:** `docs/architecture/dependency-policy.rktd`
**Action:** Add new modules from v0.22.8 (plan-types-parser, event-contracts, session-events, session-types, session-controls, session-compaction)

## Verification

```bash
racket scripts/run-tests.rkt --suite fast
racket scripts/check-deps.rkt
```

**Expected:** 409+ files pass, 0 failures.
