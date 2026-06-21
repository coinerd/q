# RED Module Design Documentation — v0.99.37

**Date:** 2026-06-23
**Wave:** W10 (#8450)
**Scope:** Design-it-twice migration packages for 4 high-risk RED modules
**Discipline:** Design documents only — NO source code changes

---

## Table of Contents

1. [Executive Summary](#1-executive-summary)
2. [Risk Taxonomy](#2-risk-taxonomy)
3. [cli/args.rkt — CLI Argument Parser](#3-cliargsrkt)
4. [wiring/run-modes.rkt — Runtime Construction Facade](#4-wiringrun-modesrkt)
5. [scripts/run-tests.rkt — Test Runner Core](#5-scriptsrun-testsrkt)
6. [agent/event-structs.rkt — Event Struct Facade](#6-agentevent-structsrkt)
7. [Migration Priority & Sequencing](#7-migration-priority--sequencing)
8. [Test Strategy for Migration](#8-test-strategy-for-migration)

---

## 1. Executive Summary

Four modules are classified RED (risk ≥ 12) and excluded from direct
modification in v0.99.37. This document provides design-it-twice
migration packages for each: current-state analysis, risk factors,
proposed decomposition strategy, and a safe incremental migration path.

| Module | Lines | Risk | Primary Concern |
|--------|-------|------|-----------------|
| `cli/args.rkt` | 634 | 16 | 19-field struct, 22 flags, data-table parser |
| `wiring/run-modes.rkt` | 621 | 17 | 307L `build-runtime-from-cli`, 29 parameter mutations |
| `scripts/run-tests.rkt` | 592 | 14+ | Gate-critical, process management, parallel runner |
| `agent/event-structs.rkt` | 562 | 14+ | Protocol-critical, 30+ event types, macro-generated |

**Recommendation:** None of these modules need decomposition in
v0.99.37. Their current architecture is adequate. The design docs below
document the migration path for future milestones if decomposition
becomes necessary.

---

## 2. Risk Taxonomy

### Why these modules are RED

Each module is RED for a different reason:

- **cli/args.rkt**: Sheer surface area. The `cli-config` struct has 19
  fields and the flag table has 22 entries. Any structural change risks
  breaking the CLI contract that users depend on. The struct is
  `#:transparent`, so field ordering matters for serialization.

- **wiring/run-modes.rkt**: God function. `build-runtime-from-cli` is
  307 lines long and mutates 29 parameters. It touches nearly every
  subsystem (provider, tools, extensions, memory, sandbox, GSD,
  blackboard, verifier). Breaking it breaks the entire application
  startup.

- **scripts/run-tests.rkt**: Gate-critical. This IS the test runner.
  A regression here can cause false release blockers (the #8435 pattern)
  or mask real failures. Any change must be validated against the full
  test suite, which takes 1.5-2 hours on the VPS.

- **agent/event-structs.rkt**: Protocol-critical. 30+ typed event
  structs are consumed by extensions, TUI, GUI, and serialization.
  The facade re-exports from 9 sub-modules + browser events. Changing
  the provide list or struct definitions can break JSON serialization,
  event bus routing, and extension APIs.

### Risk Factors Explained

| Factor | Weight | Applies To |
|--------|--------|------------|
| **Gate-critical** | +5 | run-tests.rkt |
| **Protocol-critical** | +5 | event-structs.rkt |
| **User-facing contract** | +4 | cli/args.rkt |
| **God function (>200L)** | +4 | run-modes.rkt |
| **Many dependents** | +3 | event-structs.rkt |
| **Parameter mutation count >20** | +3 | run-modes.rkt |
| **Macro-generated code** | +3 | event-structs.rkt |
| **Parallel/threading** | +3 | run-tests.rkt |
| **Test coverage thin** | +2 | All |

---

## 3. cli/args.rkt

### 3.1 Current Architecture (634 lines)

```
cli/args.rkt
├── cli-config struct (19 fields, #:transparent)
├── flag-def struct (7 fields, data-driven flag table)
├── FLAG-DEFINITIONS (22 flag definitions)
├── Pure functions:
│   ├── make-help-config
│   ├── acc-ref / acc-set / acc-cons (alist operations)
│   ├── make-initial-acc
│   ├── acc->cli-config (accumulator → struct conversion)
│   └── cli-config->runtime-config (struct → hash conversion)
├── Parser:
│   ├── parse-cli-args (main entry point)
│   ├── apply-flag (dispatch by flag type)
│   └── handle-positional (subcommands + prompt)
└── I/O:
    ├── print-usage (generated from flag table)
    └── print-version
```

### 3.2 Strengths

- **Data-driven flag table**: The `FLAG-DEFINITIONS` list is already
  the right abstraction. Adding a flag means adding a `flag-def` entry,
  not touching parser logic. This is the core design pattern.
- **Pure accumulator**: The parser uses a functional alist accumulator,
  not mutable state. This is clean and testable.
- **Explicit contracts**: `parse-cli-args`, `cli-config->runtime-config`,
  `print-usage`, and `print-version` all have `contract-out` wrappers.
- **Good test coverage**: `test-cli-args.rkt` (11KB), `test-cli-flags.rkt`
  (2.4KB), `test-cli.rkt` (23KB) exercise parsing thoroughly.

### 3.3 Risk Factors (Risk = 16)

| Factor | Detail |
|--------|--------|
| 19-field struct | Adding/removing/reordering fields breaks all 22 flag `apply-fn` closures that construct `cli-config` directly |
| User-facing CLI contract | Flag names (`--model`, `--session`, `--tui`, etc.) are user-facing; cannot change without migration |
| `#:transparent` serialization | Field order matters for any code that treats the struct as a list |
| Magic number `10` for max-turns | Appears in `make-initial-acc`, `make-help-config`, and inline constructor calls |
| Sessions subcommand handling | Embedded in `handle-positional` rather than in the flag table |

### 3.4 Proposed Migration Path (If Needed)

**Phase 1: Extract Defaults Module**

Create `cli/defaults.rkt` with:
```racket
(define DEFAULT-MAX-TURNS 10)
(define DEFAULT-MODE 'interactive)
(define DEFAULT-COMMAND 'chat)
(provide (all-defined-out))
```

Replace all inline `10` references with `DEFAULT-MAX-TURNS`.

**Phase 2: Extract Flag Definitions**

Create `cli/flag-definitions.rkt` containing:
- `flag-def` struct
- `FLAG-DEFINITIONS` list
- `make-initial-acc`

This separates data (flag table) from logic (parser).

**Phase 3: Extract Config Conversion**

Create `cli/config-conversion.rkt` with:
- `cli-config->runtime-config`
- Any future `runtime-config->cli-config` (for round-trip)

**Phase 4: Extract Usage Printer**

Create `cli/usage.rkt` with `print-usage` and `print-version`.

After all 4 phases, `cli/args.rkt` becomes a thin re-export facade:
```racket
(require "flag-definitions.rkt"
         "config-conversion.rkt"
         "usage.rkt")
(provide (all-from-out ...))
```

### 3.5 Why NOT to Migrate in v0.99.37

The flag table pattern is already the right abstraction. The 634 lines
are mostly data (flag definitions) and straightforward parsing logic.
The only real code smell is the magic `10` for max-turns and the
sessions subcommand handling embedded in `handle-positional`. These are
minor and don't warrant the risk of structural change.

---

## 4. wiring/run-modes.rkt

### 4.1 Current Architecture (621 lines)

```
wiring/run-modes.rkt
├── Imports (60+ symbols from 25+ modules)
├── MCP governed tool execution:
│   ├── make-mcp-event-publisher
│   └── make-mcp-governed-execute-fn
├── build-runtime-from-cli (307 lines — THE god function):
│   ├── Config conversion (cli-config → base-config hash)
│   ├── Event bus creation
│   ├── Tool registry creation + default registration
│   ├── Provider/model creation
│   ├── Extension registry creation + loading
│   ├── Settings loading (global + project)
│   ├── 29 parameter mutations (wire-runtime-parameters!)
│   ├── Memory backend setup
│   ├── Sandbox/execution plane wiring
│   ├── MCP config wiring
│   ├── GSD session wiring
│   ├── Blackboard setup
│   ├── MAS guidance wiring
│   └── Session config assembly
├── mode-for-config (pure: cli-config → symbol)
├── reload-config! (re-read settings, rebuild provider)
└── Re-exports: run-interactive, run-single-shot, etc.
```

### 4.2 Strengths

- **Clear entry point**: `build-runtime-from-cli` is the single function
  that wires everything. Callers don't need to know about the 29
  parameters.
- **Mode dispatch is clean**: `mode-for-config` is a pure function that
  maps cli-config mode to a symbol.
- **Already has sub-modules**: `run-interactive.rkt` and `run-json-rpc.rkt`
  are already extracted.

### 4.3 Risk Factors (Risk = 17)

| Factor | Detail |
|--------|--------|
| 307-line function | `build-runtime-from-cli` is a god function touching every subsystem |
| 29 parameter mutations | A single `wire-runtime-parameters!` call mutates 29 parameters; getting the order wrong is catastrophic |
| 60+ imported symbols | Circular dependency risk if any import changes its provides |
| Implicit ordering | Parameters must be wired in the right order (e.g., settings before provider, provider before tools) |
| Side effects at module load | Some `require`d modules have module-level side effects |
| `reload-config!` re-mutates | Configuration reload re-runs parameter mutations, which can race with active threads |

### 4.4 Proposed Migration Path (If Needed)

**Phase 1: Extract Phase Functions**

Break `build-runtime-from-cli` into phases:
```racket
(define (build-runtime-from-cli cfg)
  (define ctx (make-build-context cfg))
  (define ctx1 (phase:config ctx))
  (define ctx2 (phase:event-bus ctx1))
  (define ctx3 (phase:tool-registry ctx2))
  (define ctx4 (phase:provider ctx3))
  (define ctx5 (phase:extensions ctx4))
  (define ctx6 (phase:settings ctx5))
  (define ctx7 (phase:wire-parameters ctx6))
  (define ctx8 (phase:sandbox ctx7))
  (define ctx9 (phase:session ctx8))
  ctx9)
```

Each phase takes a `build-context` struct and returns an updated one.
This makes the ordering explicit and testable.

**Phase 2: Extract build-context Struct**

```racket
(struct build-context
  (cfg base-config bus registry provider settings session-config ...)
  #:transparent)
```

**Phase 3: Extract wire-parameters Module**

Move the 29-parameter wiring into `wiring/wire-parameters.rkt`:
```racket
(provide wire-runtime-parameters!)
(define (wire-runtime-parameters! ctx)
  (parameterize-params ...)
  ...)
```

**Phase 4: Extract MCP Module**

Move `make-mcp-governed-execute-fn` to `wiring/mcp-wiring.rkt`.

After all 4 phases, `run-modes.rkt` becomes a composition facade.

### 4.5 Why NOT to Migrate in v0.99.37

The god function works. It's ugly but correct. Breaking it into phases
requires introducing a `build-context` struct that threads through all
phases — a significant refactor with high regression risk. The function
has been stable for many versions; the cost/risk of decomposition exceeds
the benefit unless we need to add new subsystems that require modifying
the wiring order.

The one change that WOULD be worth doing: extract `wire-runtime-parameters!`
into its own module so the 29 mutations can be tested in isolation. But
even this is risky because the parameters have implicit ordering
dependencies.

---

## 5. scripts/run-tests.rkt

### 5.1 Current Architecture (592 lines)

```
scripts/run-tests.rkt
├── Thin facade re-exports from sub-modules:
│   ├── run-tests/classify.rkt (metadata, file discovery, classification)
│   ├── run-tests/parse.rkt (output parsing, result struct)
│   ├── run-tests/reporting.rkt (summary, failure logs, formatting)
│   ├── run-tests/cli.rkt (argument parsing)
│   ├── run-tests/gate-evidence.rkt (CI gate evidence)
│   ├── run-tests/inventory.rkt (inventory report)
│   └── run-tests/ledger.rkt (known-failure ledger)
├── Core functions:
│   ├── build-result-from-process (subprocess result assembly)
│   ├── resolve-test-path (path normalization)
│   ├── file-timeout-ms (metadata-aware timeout)
│   ├── parse-result-bytes (output parsing helper)
│   ├── run-single-file/subprocess (subprocess execution)
│   ├── run-single-file/in-process (in-process execution)
│   ├── run-single-file (dispatch by mode)
│   ├── split-list (batch partitioning)
│   ├── run-all-files (parallel batch runner)
│   ├── effective-mode (suite-aware mode selection)
│   ├── run-suite-once (single suite execution with strict mode)
│   └── main (entry point)
```

### 5.2 Strengths

- **Already decomposed**: The heaviest logic is in 7 sub-modules.
  `run-tests.rkt` itself is mostly orchestration.
- **Dual execution modes**: `subprocess` (isolated) and `in-process`
  (fast). The `auto` mode picks based on suite.
- **Strict mode**: Catches zero-parsed false greens.
- **Parallel batching**: Mutation-sensitive files serialized, rest parallel.
- **Extensive test coverage**: 14 test files covering metadata,
  classification, parsing, profiles, timeouts, ledger, and zero-parsed
  elimination.

### 5.3 Risk Factors (Risk = 14+)

| Factor | Detail |
|--------|--------|
| Gate-critical | This module IS the CI gate. A regression causes false pass/fail. |
| Process management | Subprocess + thread management with timeout/custodian cleanup |
| Parallel execution | `run-all-files` spawns threads per batch; race conditions possible |
| GC timing | Manual `(collect-garbage 'major)` every 5 batches — OOM risk if removed |
| Strict mode exit codes | Exit 4 for zero-parsed; changing exit codes breaks CI scripts |
| Mode dispatch | `effective-mode` has subtle logic: `auto` → `grouped` for unit-fast, `subprocess` otherwise |

### 5.4 Proposed Migration Path (If Needed)

**Phase 1: Extract Process Runner**

Create `run-tests/execution.rkt` with:
- `run-single-file/subprocess`
- `run-single-file/in-process`
- `run-single-file`
- `build-result-from-process`
- `resolve-test-path`
- `file-timeout-ms`
- `parse-result-bytes`

**Phase 2: Extract Parallel Runner**

Create `run-tests/parallel-runner.rkt` with:
- `run-all-files`
- `split-list`
- Thread management + GC

**Phase 3: Extract Suite Runner**

Create `run-tests/suite-runner.rkt` with:
- `run-suite-once`
- `effective-mode`
- Strict mode logic

After all 3 phases, `run-tests.rkt` retains only `main` (the entry point).

### 5.5 Why NOT to Migrate in v0.99.37

The decomposition into 7 sub-modules is already excellent. The remaining
592 lines in `run-tests.rkt` are the core execution engine that is
difficult to decompose further without introducing inter-module state
threading. The process management and parallel execution logic is tightly
coupled and works correctly.

The main risk is that any change to this module requires a full broad
gate run (1.5-2 hours) to validate. This cost is prohibitive for a
non-functional refactor.

**If a change IS needed:** The safest extraction is Phase 1 (process
runner), because `run-single-file` has clean inputs/outputs and is already
well-tested.

---

## 6. agent/event-structs.rkt

### 6.1 Current Architecture (562 lines)

```
agent/event-structs.rkt (facade)
├── Requires from 9 sub-modules + browser/events.rkt
├── Explicit re-exports (ADR-0028: no wildcard re-exports):
│   ├── event-structs/base.rkt → typed-event base
│   ├── event-structs/turn-events.rkt → turn start/end
│   ├── event-structs/message-events.rkt → message lifecycle
│   ├── event-structs/tool-events.rkt → tool execution + per-tool
│   ├── event-structs/provider-events.rkt → provider/LLM
│   ├── event-structs/session-events.rkt → session/input/model/agent
│   ├── event-structs/iteration-events.rkt → auto-retry/compaction
│   ├── event-structs/hook-events.rkt → hook interception
│   ├── event-structs/stream-events.rkt → streaming
│   ├── event-structs/memory-events.rkt → memory operations
│   ├── event-structs/context-pressure-events.rkt → context pressure
│   ├── event-structs/mas-events.rkt → multi-agent
│   ├── event-structs/verification-events.rkt → verification
│   └── browser/events.rkt → browser events
├── Deserializer registration (in-memory at module load)
└── Provide list (200+ identifiers)
```

### 6.2 Strengths

- **Already decomposed**: 14 sub-modules, each focused on a domain.
- **Explicit re-exports**: ADR-0028 mandates explicit identifier lists,
  preventing accidental export leakage.
- **API classification**: v0.96.1 added PUBLIC vs INTERNAL classification.
- **Extensive test coverage**: 26 event-related test files.
- **Macro-generated serializers**: `define-typed-event` generates
  serializer/deserializer registration automatically.

### 6.3 Risk Factors (Risk = 14+)

| Factor | Detail |
|--------|--------|
| Protocol-critical | Event types are the serialization protocol; changes break JSON round-trip |
| 200+ exported identifiers | The provide list is huge; missing one breaks consumers |
| Macro-generated code | `define-typed-event` generates serializers; changing the macro changes all events |
| PUBLIC API stability | 4 event types have stability commitments for extensions |
| Serializer registration | Registration happens at module load; order matters |
| Event type uniqueness | Types must be globally unique strings |

### 6.4 Proposed Migration Path (If Needed)

**Phase 1: Extract Domain Facades**

Instead of one mega-facade, create domain-specific facades:
```racket
;; agent/event-structs/core.rkt — base + turn + message
;; agent/event-structs/tools.rkt — tool execution events
;; agent/event-structs/infrastructure.rkt — session, iteration, hook
;; agent/event-structs/streaming.rkt — stream, context-pressure, memory
```

Each domain facade re-exports from its sub-modules. Consumers import
the domain facade they need.

**Phase 2: Deprecate Mega-Facade**

Mark `agent/event-structs.rkt` as deprecated. New code should import
from domain facades. Old code continues to work via re-exports.

**Phase 3: Split Serializer Registration**

Move serializer/deserializer registration into a dedicated module
that each domain facade requires. This makes registration order explicit.

### 6.5 Why NOT to Migrate in v0.99.37

The current architecture is already well-decomposed. The mega-facade
is ugly but serves backward compatibility. Creating domain facades
would be a cosmetic improvement that risks breaking the 200+ exported
identifier contract.

The real risk in this module is the `define-typed-event` macro, which
generates serializer registration code. Any change to the macro (even
in a sub-module) changes all event types simultaneously. This is the
true RED area, but it's in `util/event/event-macro.rkt`, not the facade.

**If a change IS needed:** The safest change is adding a new event
type by creating a new sub-module and adding its re-exports to the
facade's provide list. This is additive and doesn't risk existing types.

---

## 7. Migration Priority & Sequencing

If decomposition of any RED module becomes necessary, the recommended
priority order (lowest risk first):

| Priority | Module | First Step | Estimated Effort | Regression Risk |
|----------|--------|------------|------------------|-----------------|
| 1 | scripts/run-tests.rkt | Extract process runner | Medium | Low (well-tested, clean I/O) |
| 2 | cli/args.rkt | Extract defaults module | Low | Low (pure data, well-tested) |
| 3 | agent/event-structs.rkt | Add domain facades (additive) | Medium | Medium (200+ identifiers) |
| 4 | wiring/run-modes.rkt | Extract wire-parameters | High | High (29 mutations, implicit order) |

**None of these migrations are recommended for v0.99.37.** They are
documented here for future milestones when the cost of NOT refactoring
exceeds the risk of refactoring.

---

## 8. Test Strategy for Migration

### 8.1 Characterization Tests First

Before any decomposition, create characterization tests that document
current behavior. These tests should be "golden master" style — they
capture what the code DOES, not what it SHOULD do.

For each module:
- **cli/args.rkt**: Test every flag combination produces the expected
  `cli-config` struct fields
- **wiring/run-modes.rkt**: Test that `build-runtime-from-cli` produces
  a `session-config?` with expected keys for various CLI configs
- **scripts/run-tests.rkt**: Test that `run-single-file` produces
  expected `test-file-result` for known-good and known-failing test files
- **agent/event-structs.rkt**: Test that all 200+ exported identifiers
  resolve and that serializer round-trip works for each event type

### 8.2 Incremental Validation

After each phase of decomposition:
1. Run the module's dedicated tests (fast)
2. Run the smoke gate (1 min)
3. Run the fast suite (~15 min)
4. Only run the broad gate (1.5-2 hours) before merge

### 8.3 Rollback Plan

Each phase should be a separate commit. If any test suite fails, revert
the commit. The modular extraction pattern (extract → re-export) ensures
backward compatibility at every step — the facade always works even if
internal modules are refactored.

---

## 9. Conclusion

The four RED modules are stable and adequately architected. Their risk
comes from their criticality (gate, protocol, user contract, startup),
not from code quality issues. The decomposition paths documented above
provide safe, incremental migration strategies for future milestones.

For v0.99.37, these modules remain untouched. The design documentation
serves as a reference for the team and as a starting point if any of
these modules need modification in the future.
