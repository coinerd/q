# Wave Report — v0.99.90 W0: Port Inventory and Composition Root

**Issue:** #9231 · **Branch:** `feature/v09990-w0-port-inventory-composition-root` · **Baseline:** `93619627`

## Goal (frozen roadmap)

Inventory GSD filesystem, git, GitHub, clock, process, and event effects; define small cohesive ports and one composition root. Gate: DI/contracts + Fast + review. Acceptance: at most one port per coherent external domain, tested with real deterministic fakes.

## Delivered

### 1. Effect inventory (roadmap acceptance)

Read-only consumer grep across all `extensions/gsd/*.rkt` (29 pre-W0 modules):

| Domain | Current direct owners (pre-W0) |
|---|---|
| filesystem | archive, campaign-state, command-handlers, core, go-orchestrator, plan-context-builder, projection-effects, tool-handlers, wave-completion, wave-docs, wave-executor |
| git | plan-context-builder (`git show --stat --oneline HEAD -- <files>`) |
| GitHub | **none** — no GSD production module performs network/GitHub effects |
| clock | archive, campaign-state, events, go-orchestrator, state-machine, wave-executor |
| process | plan-context-builder (subprocess), go-orchestrator (`shutdown-worker!`) |
| event | events.rkt owns external publication; callers include state-machine, core, command-handlers, tool-handlers, facade bridge |

### 2. Cohesive contracted ports (`extensions/gsd/effect-ports.rkt`, NEW)

Neutral contracts, `#lang racket/base` + `racket/contract` only. No universal effect map — one value per coherent external domain:

- `gsd-filesystem-port` — kind / read-bytes / write-bytes! / rename! / delete! / mkdir! / list / acquire-lock / release-lock!
- `gsd-git-port` — find-root / head-summary (read-only git inspection)
- `gsd-clock-port` — seconds / milliseconds (both existing units preserved)
- `gsd-process-port` — run / stop-worker!
- `gsd-effect-ports` aggregate — filesystem, git, clock, process, event-sink
- Event uses the existing `(symbol? hash? -> void?)` session-sink shape (no duplicate event abstraction)
- `gsd-external-domains` = `(filesystem git github clock process event)`; `gsd-port-domain-counts` pins **github ⇒ 0** until W4's correlated command adapter

Every operation field carries a contract (`contract-out`), so domain violations blame callers and range violations blame adapters (same blame model as `util/extension/host-services.rkt`).

### 3. Production adapters (`extensions/gsd/system-adapters.rkt`, NEW)

Concrete implementations: filesystem ops, advisory lock via `port-try-file-lock?`, git subprocess (`git show --stat --oneline HEAD -- files`, 2000-char truncation, `""` on failure — byte-identical to the previous `plan-context-builder` behavior), clock, process. `shutdown-worker!` wired from `sandbox/gateway-bridge` (already imported by go-orchestrator today).

### 4. Composition root (`extensions/gsd/composition-root.rkt`, NEW)

- `system-gsd-effect-ports` — the one production dependency value
- `current-gsd-effect-ports` — contracted DI parameter (`parameter/c gsd-effect-ports?`), declared in `docs/architecture/parameter-inventory.rktd` as SERVICE_HANDLE
- Stable facades unchanged: `extensions/gsd-planning.rkt` 32-name surface and `gsd/core.rkt` 22-name surface are NOT expanded (pinned negative test); loader convention (`the-extension`) untouched; Runtime/`extension-ctx` untouched

### 5. Representative production wiring

`plan-context-builder.rkt::get-diff-excerpt` now delegates to the injected git port (`(current-gsd-effect-ports)`); `current-git-root` remains the public override seam. The system adapter preserves the exact command/trim/truncation → `tests/test-plan-context-enrichment.rkt` 26/26 unchanged.

### 6. Tests (failing-first)

- `tests/helpers/gsd-port-fakes.rkt` (NEW) — stateful in-memory fakes with chronological call logs: FS, git, clock, process, event; no mock framework, no temp FS, no subprocess, no wall clock.
- `tests/test-gsd-effect-ports.rkt` (NEW, 6 tests, `@speed fast`, `@suite extensions`) — exact domain vocabulary, aggregate one-port-per-domain, wrong-domain rejection, deterministic fake behavior, malformed-request + broken-adapter contract rejections.
- `tests/test-gsd-composition-root.rkt` (NEW, 5 tests, `@speed fast`, `@suite extensions`) — default composition validity, dynamic DI + restore, fake-state isolation between roots, stable-facade non-expansion, neutral-contracts no-concrete-deps.
- `tests/test-gsd-responsibility-inventory.rkt` — domain vocabulary gains `external-ports`; effect vocabulary unchanged + `file->bytes` scanner; module count 29→32 (3 new port modules); suite now also runs under `module+ test` (raco test) with runtime-path fixes.
- `tests/test-arch-parameters.rkt` — audited parameter count 176→177.

### 7. Abstraction gate (mandatory report)

- **What:** 4 new modules (ports, adapters, composition root, fakes) + 2 test files.
- **Gate criteria met:** ports name real q GSD external domains (filesystem/git/clock/process/event — each with 1+ direct owners found in the inventory grep); reduces boundary errors (contracted operation fields blame callers vs adapters); simplifies tests (deterministic fakes replace ad-hoc lambdas and temp files); narrows the wiring point (single `current-gsd-effect-ports` instead of scattered globals).
- **Alternatives considered:** universal `run-effect` hash (rejected — roadmap forbids generic effect maps); per-domain filesystem ports for campaign/projection/archive (rejected — one FS port is enough, W1/W2 will build repository/transaction semantics ON it); GitHub port (rejected in W0 — zero production owners; W4 owns correlated/idempotent commands); `extension-ctx` field injection (rejected — would pollute the public versioned ctx for all extensions).

## Gates

| Gate | Result |
|---|---|
| Focused batch (ports, composition, inventory, facade-compat, events, enrichment, go-orchestrator, contracts, golden) | ✅ 124 tests |
| Golden traces | ✅ 16/16 UNCHANGED |
| Arch | ✅ 22 files / 22 passed |
| Extensions | ✅ 98 files / VERDICT PASS |
| **Fast (W0 gate)** | ✅ 1063 files / 15498 tests |
| lint-format | ✅ 2096 files 0/0 |

## Scope stops honored

W1 (`.rktd` repository boundary), W2 (atomic projection transaction), W3 (structured executor outcomes), W4 (GitHub/release adapter) — none pulled forward.

## Reports / docs

- `docs/reports/PORT-INVENTORY-COMPOSITION-ROOT-v0.99.90.md` (this file)
- `.planning/` STATE/VALIDATION/SUMMARY/HANDOFF W0-DONE updates; tracked copies in `docs/planning/` ride this PR
