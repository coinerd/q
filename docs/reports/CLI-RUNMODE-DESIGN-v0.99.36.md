# CLI/Run-Mode Design-It-Twice Audit — v0.99.36 W7

**Date:** 2026-06-22
**Wave:** W7 (#8420)
**Risk Assessment:** RED (risk 16-17) — design document only, no code changes
**Modules analyzed:** `cli/args.rkt` (634L), `wiring/run-modes.rkt` (621L)

---

## 1. Current State Analysis

### 1.1. cli/args.rkt — CLI Argument Parser

**634 lines.** Single module handling:

| Responsibility | Lines | Complexity |
|---|---|---|
| `cli-config` struct (19 fields) | ~30 | Adding a field requires touching 4 locations |
| `flag-def` struct + flag table (21 flags) | ~200 | Data-driven but apply-fns contain ad-hoc logic |
| `parse-cli-args` (recursive descent) | ~70 | Uses accumulator alist pattern |
| `handle-positional` (command/subcommand dispatch) | ~60 | Nested cond with inline cli-config construction |
| `acc->cli-config` (accumulator → struct) | ~40 | Derives final command/mode from accumulator state |
| `cli-config->runtime-config` | ~25 | Builds hash from struct for session creation |
| `print-usage` | ~50 | Generated from flag table |
| `print-version` | ~3 | Trivial |

**Complexity drivers:**

1. **19-field struct with implicit coupling.** Every new flag requires changes in 4 places: the struct definition, `make-initial-acc`, `acc->cli-config`, and a new `flag-def`. The struct fields serve double duty as both parsing targets and runtime configuration.

2. **Dual command/mode derivation.** The actual `command` and `mode` are derived post-parse by `acc->cli-config` using nested `match` forms that inspect the accumulator state. This makes it impossible to know the final command until parsing is complete — there's no early exit for `help`/`version` in the accumulator path (they return sentinel symbols from apply-fns that are checked inline).

3. **Sessions subcommand mini-parser.** The `sessions` and `verify-session` subcommands construct `cli-config` directly in `handle-positional`, bypassing the accumulator entirely. This means two parallel construction paths.

4. **Mixed concerns.** Parsing, validation, default resolution, usage generation, and runtime-config conversion all live in one module. The module is both a pure parser and an I/O module (print-usage, print-version).

### 1.2. wiring/run-modes.rkt — Runtime Construction Facade

**621 lines.** Central wiring hub for the entire runtime:

| Responsibility | Lines | Complexity |
|---|---|---|
| Imports (37 `only-in` from 25+ modules) | ~90 | God-module coupling |
| `build-runtime-from-cli` | **307** | God function — 8 when-blocks, 29 parameter mutations |
| `wire-runtime-parameters!` | ~65 | Shared wiring extracted in H3a |
| `reload-config!` | ~30 | Hot-reload path |
| `make-mcp-governed-execute-fn` | ~30 | MCP tool execution bridge |
| `mode-for-config` | ~12 | Simple dispatcher |
| `filter-workflow-skills` | ~10 | Skill filtering helper |

**Complexity drivers:**

1. **God function: `build-runtime-from-cli` (307 lines).** This function creates the event bus, tool registry, extension registry, model registry, provider, session directory, trace logger, blackboard, agent registry, MCP server, verifier, and execution plane. It's a single linear function with 8 conditional `when` blocks, each wiring a different subsystem.

2. **29 direct parameter mutations.** The function sets 29 module-level parameters (`current-*`) via direct calls. These mutations affect global state, making the function impossible to test in isolation and causing subtle ordering dependencies (e.g., `current-verifier-provider` must be set after `prov` is created).

3. **Feature-flag-driven branching.** Each feature (MCP, blackboard, hot-swap, auto-reload, broker, verifier, execution plane) has its own `when` block with inline wiring logic. Adding a new feature means editing this function. There's no registry of features — the wiring is hard-coded.

4. **System prompt assembly mixed with infrastructure.** The function assembles system instructions (skill summaries, workflow guidance, MAS delegation guidance, parallel mode guidance, project tree) inline alongside infrastructure wiring. This conflates "what to tell the agent" with "how to build the runtime."

5. **Two callers share partial logic.** `build-runtime-from-cli` and `reload-config!` both need to wire runtime parameters. They share `wire-runtime-parameters!`, but each has its own duplicated timeout/security/profile wiring logic.

### 1.3. Risk Assessment

Both modules are classified **RED** (do not edit in v0.99.36) because:

- **High blast radius:** `cli/args.rkt` is imported by `interfaces/cli.rkt`, `wiring/run-modes.rkt`, and every test that constructs CLI configs. `wiring/run-modes.rkt` is imported by `main.rkt` and is the single entry point for runtime construction.
- **No existing tests for build-runtime-from-cli:** The function's complexity makes it untestable without a full integration test harness.
- **Protocol-critical:** Changes to either module affect every entry path (interactive, TUI, JSON, RPC, GUI).

---

## 2. Design A: Builder Pipeline (Staged Composition)

### 2.1. Concept

Decompose `build-runtime-from-cli` into a **pipeline of staged builders**, each producing an intermediate record that feeds the next stage. Replace the single 307-line function with 6-8 focused functions, each 20-40 lines.

### 2.2. Structure

```
wiring/
  run-modes.rkt          ← thin facade (re-exports, mode-for-config)
  runtime-builder.rkt    ← pipeline orchestration
  stages/
    load-settings.rkt    ← stage 1: settings + resources
    build-provider.rkt   ← stage 2: provider + model registry
    build-registry.rkt   ← stage 3: tool registry + extensions
    build-session.rkt    ← stage 4: session config assembly
    wire-features.rkt    ← stage 5: feature wiring (MCP, BB, hot-swap...)
    wire-parameters.rkt  ← stage 6: runtime parameter mutation
    assemble-prompt.rkt  ← stage 7: system prompt assembly
```

### 2.3. Intermediate Record

```racket
(struct runtime-context
  (cli-config        ; cli-config?
   settings          ; settings?
   base-hash         ; hash? (from cli-config->runtime-config)
   event-bus         ; event-bus?
   tool-registry     ; tool-registry?
   provider          ; provider?
   model-registry    ; model-registry?
   extension-registry ; extension-registry?
   session-dir       ; path-string?
   system-instructions ; (listof string?)
   trace-logger      ; trace-logger? or #f
   blackboard        ; blackboard? or #f
   features-wired    ; (listof symbol) — which features were activated)
  #:transparent)
```

Each stage takes a `runtime-context` and returns an updated one:

```racket
(define (stage-load-settings ctx)
  (define settings (load-settings ...))
  (struct-copy runtime-context ctx [settings settings]))

(define (stage-build-provider ctx)
  (define prov (build-provider ...))
  (struct-copy runtime-context ctx [provider prov]))
```

### 2.4. Pipeline Orchestration

```racket
(define (build-runtime-from-cli cfg)
  (~> (make-initial-runtime-context cfg)
      stage-load-settings
      stage-build-provider
      stage-build-registry
      stage-build-session-dir
      stage-assemble-prompt
      stage-wire-features
      stage-wire-parameters
      finalize-session-config))
```

### 2.5. Feature Wiring as Stage

Feature wiring (currently 8 inline when-blocks) becomes a stage that iterates
over a feature table:

```racket
(define FEATURE-WIRERS
  (list (feature-def 'mcp-server wire-mcp-server!)
        (feature-def 'blackboard wire-blackboard!)
        (feature-def 'hot-swap wire-hot-swap!)
        (feature-def 'auto-reload wire-auto-reload!)
        (feature-def 'broker wire-broker!)
        (feature-def 'verifier wire-verifier!)
        (feature-def 'execution-plane wire-execution-plane!)))

(define (stage-wire-features ctx)
  (for/fold ([c ctx]) ([fw (in-list FEATURE-WIRERS)])
    (if ((feature-def-predicate fw) (runtime-context-settings c))
        ((feature-def-wirer fw) c)
        c)))
```

### 2.6. Trade-offs

**Pros:**
- Each stage is independently testable (pure-ish: takes context, returns context)
- Adding a new feature = add a `feature-def` entry + a wirer function
- `reload-config!` can reuse specific stages without re-running the full pipeline
- Pipeline is self-documenting: the `~>` chain shows the build order
- `runtime-context` struct makes intermediate state explicit

**Cons:**
- Large refactoring surface — must split 307 lines into 6-8 new files
- `struct-copy` everywhere adds verbosity
- The `runtime-context` struct is a "god struct" — it carries everything
- Parameter mutations are deferred but still happen (just in a later stage)
- Risk of introducing ordering bugs if stages are reordered incorrectly
- High diff churn for a RED module

---

## 3. Design B: Wiring Protocol + Feature Modules (Declarative)

### 3.1. Concept

Replace the imperative `when`-block chain with a **declarative feature-wiring protocol**. Each feature becomes a self-contained module that exports a `wire!` function. The wiring facade discovers registered features and invokes them in dependency order. Parameter mutation is centralized behind a single `wire-parameters!` protocol.

### 3.2. Structure

```
wiring/
  run-modes.rkt           ← thin facade (unchanged interface)
  feature-registry.rkt    ← feature registration + ordered invocation
  features/
    mcp-server.rkt        ← wire-mcp-server!
    blackboard.rkt        ← wire-blackboard!
    hot-swap.rkt          ← wire-hot-swap!
    auto-reload.rkt       ← wire-auto-reload!
    broker.rkt            ← wire-broker!
    verifier.rkt          ← wire-verifier!
    execution-plane.rkt   ← wire-execution-plane!
  parameter-wiring.rkt    ← centralized parameter mutation protocol
  prompt-assembly.rkt     ← system prompt construction
```

### 3.3. Feature Protocol

Each feature module exports a feature descriptor:

```racket
;; wiring/features/blackboard.rkt
(provide blackboard-feature)

(define blackboard-feature
  (make-feature
   #:name 'blackboard
   #:enabled? (lambda (settings) (blackboard-enabled? settings))
   #:dependencies '()  ; no ordering dependencies
   #:wire! (lambda (settings ctx)
             (define bb (make-blackboard))
             (current-blackboard bb)
             (current-blackboard-injection-enabled #t)
             (start-blackboard-subscriber! (runtime-context-event-bus ctx) bb)
             (struct-copy runtime-context ctx [blackboard bb]))))
```

### 3.4. Feature Registry

```racket
;; wiring/feature-registry.rkt
(define registered-features '())

(define (register-feature! fd)
  (set! registered-features (cons fd registered-features)))

(define (wire-all-features! settings ctx)
  ;; Topologically sort by dependencies, then invoke
  (define sorted (topo-sort registered-features))
  (for/fold ([c ctx]) ([fd (in-list sorted)])
    (if ((feature-enabled? fd) settings)
        ((feature-wire! fd) settings c)
        c)))
```

### 3.5. Parameter Wiring Protocol

Instead of 29 scattered `current-*` calls, centralize all parameter mutation:

```racket
;; wiring/parameter-wiring.rkt
(define (wire-parameters! settings profile ctx)
  (wire-context-assembly! profile (runtime-context-max-ctx-tokens ctx))
  (wire-security! settings)
  (wire-memory! settings)
  (wire-verifier! settings)
  (wire-execution! settings)
  (wire-broker! settings)
  (wire-misc! settings profile))
```

Each sub-function groups related parameters. This is essentially what
`wire-runtime-parameters!` already does, but with cleaner grouping.

### 3.6. Prompt Assembly Extraction

```racket
;; wiring/prompt-assembly.rkt
(provide assemble-system-instructions)

(define (assemble-system-instructions settings cfg resources project-dir)
  (append (resource-set-instructions resources)
          (maybe-skill-section resources)
          (maybe-workflow-section resources)
          (maybe-project-tree-section project-dir)
          (maybe-mas-guidance-section settings)
          (maybe-parallel-guidance-section cfg)))
```

### 3.7. Simplified build-runtime-from-cli

```racket
(define (build-runtime-from-cli cfg)
  (define settings (load-settings-from-cli cfg))
  (define resources (load-all-resources cfg settings))
  (define base-hash (cli-config->runtime-config cfg))
  (define ctx (make-runtime-context cfg settings base-hash resources))
  
  (define ctx-with-infra (build-infrastructure ctx))   ; bus, registry, provider
  (define ctx-with-features (wire-all-features! settings ctx-with-infra))
  
  (wire-parameters! settings (resolve-profile cfg settings) ctx-with-features)
  
  (finalize-session-config ctx-with-features
                           (assemble-system-instructions settings cfg resources ...)))
```

~20 lines instead of 307.

### 3.8. Trade-offs

**Pros:**
- Maximum decoupling — each feature is a self-contained module
- Adding a feature = create module + call `register-feature!`
- Features are independently testable in isolation
- Dependency ordering is explicit (via `#:dependencies`)
- Prompt assembly is cleanly separated from infrastructure
- `reload-config!` can selectively re-wire specific features
- New features can be registered by extensions (pluggable)

**Cons:**
- Most aggressive refactoring — 7+ new files + new protocol type
- The feature registry adds runtime indirection (harder to trace execution)
- `make-feature` protocol type must be designed carefully to avoid over-engineering
- Topological sort adds complexity for what is currently a flat feature list
- Extension-registered features could introduce ordering bugs
- Highest risk of introducing subtle behavioral changes in a RED module

---

## 4. Design Comparison

| Dimension | Design A (Pipeline) | Design B (Feature Protocol) |
|---|---|---|
| **New files** | 6-8 | 10+ |
| **Lines of new code** | ~400 | ~600 |
| **Lines removed from run-modes.rkt** | ~250 | ~300 |
| **Testability** | Each stage testable individually | Each feature testable individually |
| **Extensibility** | Add stage to pipeline | Register new feature module |
| **Execution traceability** | High (linear pipeline, `~>` chain) | Medium (registry dispatch) |
| **Parameter mutation** | Deferred to late stage, still happens | Centralized in parameter-wiring module |
| **Prompt assembly** | Separate stage | Separate module |
| **Risk of behavioral change** | Medium (stages are mechanical splits) | High (new protocol, registry, topo-sort) |
| **Effort** | Medium (~2-3 days) | High (~4-5 days) |
| **Reversibility** | Easy (stages can be re-inlined) | Hard (protocol type is viral) |

---

## 5. Recommendation

**Neither design should be implemented in v0.99.36.** Both modules are RED,
and this is a design-it-twice audit — the goal is to produce the analysis and
design, not to execute it. The design should be picked up in a future milestone
when the modules can be safely refactored with comprehensive integration tests
in place first.

**If forced to choose for a future milestone, Design A (Builder Pipeline) is
recommended** because:

1. **Lower risk.** Stages are mechanical extractions from existing code — each
   stage is a named function wrapping an existing code block. The refactoring
   can be done incrementally, one stage at a time, with tests between each
   extraction.

2. **Higher traceability.** A pipeline `~>` chain shows the build order at a
   glance. Debugging is straightforward — insert a stage that logs the
   intermediate `runtime-context`.

3. **No new protocol type.** Design B introduces `make-feature`,
   `feature-enabled?`, `feature-wire!`, and a topological sort. These are
   abstractions that add power but also add indirection that makes the wiring
   harder to follow.

4. **Design B's extensibility advantage is not yet needed.** The feature list
   has been stable for 15+ releases. The registry pattern is over-engineering
   for a 7-feature system that changes infrequently.

5. **Design A's stages can evolve into Design B later.** If the feature count
   grows or extensions need to register wiring logic, the `stage-wire-features`
   stage can be upgraded to use a registry without changing the pipeline
   structure.

---

## 6. Pre-Conditions for Future Implementation

Before either design is implemented, the following must be in place:

1. **Integration test harness for `build-runtime-from-cli`.** Currently there
   are zero tests for this function. Any refactoring without tests is reckless.
   The test should construct a `cli-config`, call `build-runtime-from-cli`,
   and verify the resulting `session-config` has expected fields.

2. **Mock provider support.** `build-provider` currently requires real API
   credentials. Tests need a mock provider that doesn't make network calls.

3. **Parameter snapshot/restore.** The 29 parameter mutations need a way to
   snapshot state before wiring and restore it after, so tests don't pollute
   global state.

4. **Feature flag inventory.** A formal inventory of all feature flags and
   their interactions, to ensure the design covers all cases.

---

## 7. What This Wave Delivered

This wave delivered a design document only. No code was changed in either
`cli/args.rkt` or `wiring/run-modes.rkt`. The analysis and designs serve as
input for a future refactoring milestone.

**Deliverables:**
- Complexity analysis of both RED modules (§1)
- Design A: Builder Pipeline with staged composition (§2)
- Design B: Wiring Protocol with feature modules (§3)
- Side-by-side comparison (§4)
- Recommendation with rationale (§5)
- Pre-conditions for future implementation (§6)
