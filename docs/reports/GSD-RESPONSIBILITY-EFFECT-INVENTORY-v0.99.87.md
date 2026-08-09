# GSD Responsibility & Effect Inventory — v0.99.87

**Wave:** v0.99.87 W2 — GSD Responsibility & Effect Inventory  
**Issue:** #9214 (milestone 874)  
**Date:** 2026-08-09  
**Baseline revision:** `274de500` (co-change/effect data) — current HEAD `7b62932a` (W1 merged)  
**Scope:** all 26 modules under `extensions/gsd/` (6,197 LOC total at MA-06 measurement)

---

## 1. Domain classification

Every GSD module classified into one primary domain. A module may carry
secondary *effects* (Section 3) but belongs to exactly one domain.

| Module | Lines | Domain | Category (roadmap) |
|---|---|---|---|
| `shared.rkt` | 67 | Pure helpers (title extraction, slugify) | pure planning |
| `wave-status.rkt` | 78 | Canonical wave status constants | pure planning |
| `command-types.rkt` | 48 | `gsd-command-result` / `gsd-ok` / `gsd-err` | compatibility facade |
| `command-parser.rkt` | 109 | `/plan`, `/go`, ... tokenizer + args | command parsing |
| `plan-types-parser.rkt` | 129 | Markdown → raw wave hash parsing | pure planning |
| `plan-types.rkt` | 453 | Typed plan/wave/task structs + normalized IR | pure planning |
| `plan-validator.rkt` | 142 | Plan structure validation | pure planning |
| `runtime-state-types.rkt` | 42 | `gsd-runtime-state` struct | campaign state (types) |
| `session-state.rkt` | 323 | Per-session ctx struct, parameters, transactions | campaign state |
| `events.rkt` | 179 | Stable event telemetry + correlation | event projection |
| `event-structs.rkt` | 122 | Event struct definitions | event projection |
| `policy.rkt` | 102 | `gsd-decide-action` guard decisions | transition logic |
| `transition-logic.rkt` | 221 | Pure transition computation (no I/O) | transition logic |
| `state-machine.rkt` | 447 | FSM API, event emission, wave-gate tracking | transition logic |
| `wave-executor.rkt` | 280 | Wave execution engine + error recovery | transition logic |
| `context-bundle.rkt` | 149 | Role-specific context assembly | pure planning |
| `prompts.rkt` | 218 | Prompt templates per phase | pure planning |
| `plan-context-builder.rkt` | 211 | Plan context enrichment (git root, capabilities) | pure planning |
| `core.rkt` | 359 | Command dispatch core, write guard, transactions | UI/extension glue |
| `archive.rkt` | 306 | Plan archival + wave-doc persistence | persistence |
| `wave-docs.rkt` | 297 | PLAN.md / wave-doc read-modify-write | persistence |
| `tool-handlers.rkt` | 270 | planning-read / planning-write tool handlers | UI/extension glue |
| `command-handlers.rkt` | 531 | Command handler implementations | UI/extension glue |
| `campaign-state.rkt` | 493 | Durable campaign record, identity, reconstruction | campaign state |
| `go-orchestrator.rkt` | 426 | Single-wave campaign coordinator loop | campaign state |
| `wave-completion.rkt` | 195 | Verifier-first completion + durable outbox | campaign state |

**Domain counts:** pure planning 8 · campaign state 5 · transition logic 4 ·
UI/extension glue 3 · persistence 2 · event projection 2 · command parsing 1 ·
compatibility facade 1 = **26 modules, 0 unclassified**.

---

## 2. Consumer/producer matrix (internal `extensions/gsd/*` requires)

Producers (provides API consumed elsewhere) → consumers. Only intra-GSD edges shown.

| Producer | Consumers |
|---|---|
| `runtime-state-types.rkt` | session-state, state-machine, transition-logic, core |
| `session-state.rkt` | state-machine, core, events, archive, command-handlers, tool-handlers |
| `plan-types.rkt` | plan-validator, context-bundle, prompts, wave-executor, core, command-handlers, plan-context-builder |
| `plan-types-parser.rkt` | plan-types |
| `wave-docs.rkt` | archive, campaign-state, wave-executor, wave-completion, go-orchestrator, tool-handlers, command-handlers, core |
| `wave-status.rkt` | archive, wave-docs, wave-completion, go-orchestrator |
| `state-machine.rkt` | archive, core, wave-executor, command-handlers, tool-handlers |
| `events.rkt` | state-machine, core, command-handlers, tool-handlers |
| `event-structs.rkt` | state-machine, core, command-handlers, tool-handlers |
| `policy.rkt` | state-machine, core, tool-handlers |
| `transition-logic.rkt` | state-machine |
| `command-types.rkt` | archive, core, command-handlers |
| `shared.rkt` | archive, wave-docs, wave-executor |
| `campaign-state.rkt` | wave-executor, wave-completion, go-orchestrator, command-handlers |
| `wave-completion.rkt` | go-orchestrator |
| `plan-context-builder.rkt` | command-handlers, go-orchestrator |
| `context-bundle.rkt` | core, command-handlers |
| `prompts.rkt` | command-handlers |
| `go-orchestrator.rkt` | command-handlers |
| `archive.rkt` | core, command-handlers, tool-handlers |
| `tool-handlers.rkt` | command-handlers |

**Leaf producers (most reused):** `wave-docs.rkt` (8 consumers), `session-state.rkt` (7),
`plan-types.rkt` (7), `state-machine.rkt` (5), `events.rkt`/`event-structs.rkt` (4).

**Leaf consumers (no internal consumers):** `command-parser.rkt`, `command-types.rkt`,
`event-structs.rkt`, `plan-types-parser.rkt`, `policy.rkt`, `runtime-state-types.rkt`,
`shared.rkt`, `wave-status.rkt` — pure leaves, no cyclic risk.

**Dependency direction:** pure planning/types → transition logic → campaign state →
UI/extension glue. No cycles among the 26 modules (verified by require graph scan).

---

## 3. Effect inventory

Effects detected by scanning non-comment code (counts = occurrences).

| Module | fs-write | fs-rename | fs-delete | mkdir | dir-list | sha256 | git (quoted) | subprocess | parameterize | make-param | path-ops |
|---|---|---|---|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|
| `archive.rkt` | 2 | 2 | 1 | 3 | 3 | — | — | — | — | — | 3 |
| `campaign-state.rkt` | 1 | 1 | — | 1 | — | 5 | — | — | — | — | — |
| `command-handlers.rkt` | — | — | 1 | 1 | — | — | — | — | — | — | — |
| `go-orchestrator.rkt` | — | — | — | 1 | — | — | — | — | — | — | 1 |
| `tool-handlers.rkt` | 2 | — | — | 1 | — | — | — | — | — | — | 3 |
| `wave-completion.rkt` | 2 | 1 | — | 1 | — | — | — | — | — | — | — |
| `wave-docs.rkt` | 2 | — | — | 1 | — | — | — | — | — | — | 1 |
| `core.rkt` | 2 | — | — | — | — | — | — | — | 1 | — | 1 |
| `plan-context-builder.rkt` | — | — | — | — | — | — | 1 | 3 | — | 1 | 2 |
| `events.rkt` | — | — | — | — | — | — | — | — | — | 1 | — |
| `session-state.rkt` | — | — | — | — | — | — | — | — | — | 2 | — |
| `state-machine.rkt` | — | — | — | — | — | — | — | — | — | 3 | — |
| `wave-executor.rkt` | — | — | — | — | — | — | — | — | — | — | — |

\* `git (quoted)` counts the `"git"` string literal (subprocess invocation via
`find-executable-path "git"`). Only `plan-context-builder.rkt` invokes git.
`go-orchestrator.rkt`/`command-handlers.rkt`/`tool-handlers.rkt` `git`
identifier tokens are `.git` marker detection or capability checks — not
subprocess git — matching the machine inventory (no declared `git` effect).

**dynamic-require:** none found in any of the 26 GSD modules.

**Network/GitHub effects:** no `net/url`, `net/http`, `curl`, or raw `gh` CLI
invocation in any GSD module. Git/GitHub interaction happens through filesystem
markers (`.git` detection) and the tool/bridge layer (`gateway-bridge`, `q-sync`,
`github-integration`), which are outside the GSD module set.

**Subprocess:** only `plan-context-builder.rkt` (3× tokens: 1 launch site
`subprocess` + `subprocess-wait`/`subprocess-status` handles — `git diff`
excerpt via `find-executable-path "git"` for the verifier; `racket/system`
import). `command-handlers.rkt` has no subprocess call (its `subprocess`
token is a comment: "replaced with subprocess that captures stderr"); the
verification gate runs in-process.

**Pure modules (zero effects):** `shared`, `wave-status`, `command-types`,
`command-parser`, `plan-types-parser`, `plan-types`, `plan-validator`,
`runtime-state-types`, `event-structs`, `policy`, `transition-logic`,
`context-bundle`, `prompts` — 13/26 = 50% effect-free.

---

## 4. State-ownership map

| State aggregate | Owning module | Mutated by |
|---|---|---|
| `gsd-runtime-state` (struct) | `runtime-state-types.rkt` | session-state (set-state!), state-machine (transitions) |
| `gsd-session-ctx` (per-session boxes) | `session-state.rkt` | state-machine, core, command-handlers, tool-handlers (via accessors) |
| `current-gsd-ctx` parameter | `session-state.rkt` | runtime init, tests |
| `gsd-default-ctx` global | `session-state.rkt` | legacy accessors (deprecated) |
| `current-gsd-session-id` | `session-state.rkt` | runtime init |
| `current-gsd-correlation-id` | `events.rkt` | events |
| Plan IR (`gsd-plan`/`gsd-normalized-plan`) | `plan-types.rkt` | plan-validator (produces validated), core/command-handlers (consume) |
| Campaign record (durable, sha256 identity) | `campaign-state.rkt` | wave-executor, wave-completion, go-orchestrator |
| Wave docs / PLAN.md index (on-disk) | `wave-docs.rkt` | archive, wave-executor, wave-completion, go-orchestrator, tool-handlers, command-handlers |
| Event bus | `events.rkt` | session-state (storage), runtime |

**Single-owner invariant:** each mutable aggregate has exactly one module
that owns its definition/storage; all other mutations flow through that module's
accessors. `wave-docs.rkt` is the sole owner of on-disk wave-doc mutations —
consistent with MA-07 (tracking projections) being addressed in v0.99.90.

---

## 5. Top-5 co-change cluster verification

From `architecture-baseline-v0.99.87.rktd` (history limit 200, non-merge commits,
threshold ≥3). The five highest-count non-test clusters:

| Rank | Cluster | Count | Semantic necessity | Verdict |
|---|---|---|---|---|
| 1 | `agent/iteration/loop-config.rkt` ↔ `agent/iteration/main-loop.rkt` | 6 | iteration loop consumes loop-config injection; co-evolves by design | ✅ necessary |
| 2 | `agent/loop-dispatch.rkt` ↔ `agent/loop.rkt` | 6 | dispatch forwards to loop; tightly coupled by design | ✅ necessary |
| 3 | `agent/iteration/main-loop.rkt` ↔ `runtime/session/session-lifecycle.rkt` | 5 | iteration loop drives session lifecycle events; boundary interplay | ✅ necessary |
| 4 | `llm/openai-compatible.rkt` ↔ `llm/stream.rkt` | 5 | SSE stream normalization shared between provider and stream layer | ✅ necessary |
| 5 | `runtime/context-assembly/state-aware-builder.rkt` ↔ `runtime/context-assembly/turn-context.rkt` | 5 | builder consumes turn-context shape | ✅ necessary |

Note: the five highest-count non-test clusters; both count-6 pairs listed.
Three test-pair clusters tie at count 5 (`main-loop ↔ test-agent-iteration-di`,
`rollback-actions ↔ test-rollback-isolation`, `state-aware-builder ↔
test-rollback-isolation`) — test-prod pairings, also semantically necessary.

**GSD-involved cluster:** `extensions/gsd/go-orchestrator.rkt` ↔
`tests/test-gsd-go-orchestrator.rkt` (count 3) — test-prod pairing from v0.99.80
W2 campaign-coordinator work; expected and semantically necessary.

All top-5 clusters reflect genuine coupling (shared shape, boundary interplay, or
test-prod pairing) — no accidental co-change clusters identified.

---

## 6. ADR-0011 findings re-verification

ADR-0011 (v0.25.3, `docs/adr/0011-gsd-state-machine-rewrite.md`) and its
implementation audit (`.planning/AUDIT-v0.24.5-IMPLEMENTATION.md`, findings F1–F8)
re-verified against current code (v0.99.87):

| Finding | Fix (per ADR) | Current state | Status |
|---|---|---|---|
| F1 ARCH-TEST | Replace `hash-ref result 'key` with struct accessors | `gsd-command-result` accessors used; remaining `hash-ref` in `test-gsd-plan-types.rkt` operates on raw parse hashes (correct usage) | ✅ RESOLVED |
| F2 DUAL-EVENT | Single delegation path for `emit-gsd-event!` | `events.rkt` owns `emit-gsd-event!`/`ctx-emit-gsd-event!`; command-handlers imports via `events:` aliases — single path | ✅ RESOLVED |
| F3 GO-NO-TXN | Wrap `handle-go-command` mutations in transaction | `command-handlers.rkt` go mutations run under `with-gsd-transaction "go"` in `launch-wave-executor` (line 301), reached via `handle-go-command` (line 456) → `prepare-go-campaign` | ✅ RESOLVED |
| F4 GO-DUPE | Document `cmd-go` deprecation | `core.rkt` line 217: "DEPRECATED (v0.29.13): Removed dead cmd-go handler" | ✅ RESOLVED |
| F5 SHIM-SNAP | `gsd-snapshot` delegates to `gsm-snapshot`; fix SDK consumer | `extensions/gsd-planning-state.rkt` **removed**; `sdk-compat.rkt` uses `gsd-runtime-state-mode` struct accessor (line 30) | ✅ RESOLVED + shim removed |
| F6 DOC-FITNESS | Fitness test asserts architecture doc mentions key modules | `tests/test-gsd-v024-fitness.rkt` still present | ✅ RESOLVED |
| F7 TR-VERSION | Broaden version regex | Typed Racket pilot test remains (v0.22.x+); plan-types/plan-validator still `#lang typed/racket` with TR boundary | ✅ RESOLVED |
| F8 doc mention gate | Architecture doc contains state machine/event/policy/archive/transaction | Covered by F6 fitness test | ✅ RESOLVED |

**All 8 ADR-0011 findings remain RESOLVED.** No regression detected.
Additional hardening since ADR: `transition-logic.rkt` extracted (pure),
per-session `gsd-session-ctx` with semaphore (thread safety),
durable campaign record with sha256 identity (v0.99.80).

---

## 7. Implications for v0.99.89/90 (extraction candidates)

Modules with a clean domain + ≥2 consumers/testability evidence qualify as
extraction slices for the GSD Pure Domain Decomposition (v0.99.89) and
Execution/Persistence/Campaign Reliability Isolation (v0.99.90):

1. **`transition-logic.rkt`** — already pure; smallest slice (state predicates,
   transition table) with tests. Extraction: none needed (already isolated).
2. **`wave-status.rkt` / `shared.rkt` / `event-structs.rkt`** — pure leaves,
   zero effects; ready for neutral ownership moves.
3. **`plan-context-builder.rkt`** — pure enrichment + 2 subprocess effects
   (git diff); effect-port candidate for v0.99.90.
4. **`wave-docs.rkt`** — sole owner of tracking-file mutations (MA-07); port
   candidate for atomic projection + crash recovery.
5. **`campaign-state.rkt`** — durable state with sha256 identity; port candidate
   for idempotency + fencing (MA-08).

**No module requires monolithic redesign** — the domain separation is already
clean enough that v0.99.89/90 can proceed slice-by-slice.

---

## 8. Data source

- Module sizes/requires: static scan of `extensions/gsd/*.rkt` at HEAD `7b62932a`.
- Effect counts: non-comment code scan (patterns: fs write/rename/delete/mkdir/
  dir-list, sha256, git, subprocess, parameterize, make-parameter, dynamic-require).
- Co-change clusters: `docs/reports/architecture-baseline-v0.99.87.rktd`
  (generated at `274de500`, history limit 200).
- ADR-0011: `docs/adr/0011-gsd-state-machine-rewrite.md` +
  `.planning/AUDIT-v0.24.5-IMPLEMENTATION.md`.
