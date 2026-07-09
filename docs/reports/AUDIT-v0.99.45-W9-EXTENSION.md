# Audit: Extension Subsystem — v0.99.45 W9

**Date:** 2026-07-04
**Auditor:** Automated test suite
**Provider:** Mock/synthetic (no real API keys)
**Test file:** `tests/test-audit-v09945-w9-extension.rkt`
**Tests:** 72 assertions, 0 failures

## Configuration

- Provider: Mock (no real LLM)
- Modules tested:
  - `util/extension/extensions.rkt` — Extension struct (name, version, api-version, hooks) with guard validation
  - `util/extension/extension-types.rkt` — Extension context struct (16 fields)
  - `extensions/api.rkt` — Extension registry (thread-safe, insertion-ordered)
  - `util/hook-types.rkt` — Hook result types, per-hook action validation, schema versioning
  - `extensions/tiers.rkt` — 5-tier capability system with cumulative capabilities
  - `extensions/hooks.rkt` — Hook dispatch with pass/amend/block semantics
  - `extensions/combinators.rkt` — with-timeout, with-error-policy, with-hook-validation
  - `extensions/manifest.rkt` — QPM manifest (struct, validation, serialization, checksum)
  - `extensions/manifest-audit.rkt` — Package checksum verification and file auditing
  - `extensions/quarantine.rkt` — Extension disable/quarantine/restore lifecycle
  - `extensions/context.rkt` — Rich extension context (16 fields with accessors)
  - `extensions/events.rkt` — Per-extension subscription tracking and auto-cleanup
  - `extensions/ext-commands.rkt` — Slash command registration from extensions
  - `extensions/dynamic-tools.rkt` — Dynamic tool registration from extensions
  - `extensions/message-inject.rkt` — Message injection API (system/user/assistant)
  - `extensions/compact-context.rkt` — Planning state preservation for compaction
  - `runtime/extension-catalog.rkt` — Extension discovery and activation
  - `util/command-types.rkt` — Command entry struct and shared command AST types

## Test Matrix

| Area | Tests | Status |
|------|-------|--------|
| Extension Struct (construction, hooks, guard validation, transparent) | 4 | PASS |
| Extension Registry (empty, register/lookup, unregister, replace, insertion order, handlers-for-point) | 7 | PASS |
| Hook Result Types (pass, amend, block, schema version, valid actions, validation, valid-name) | 7 | PASS |
| Tier System (predicates, cumulative capabilities, hook-point mapping, API version, validate extension) | 5 | PASS |
| Hook Dispatch (pass-through, amend, block, multiple exts, block stops chain, amend chains payload, no handlers) | 7 | PASS |
| Combinators (timeout success, timeout fires, on-error, error-policy critical/advisory, hook validation valid/invalid) | 6 | PASS |
| Manifest (construction, validate valid/invalid, unsafe paths, safe-path predicate, jsexpr round-trip, type predicate, compare) | 10 | PASS |
| Extension Context (minimal construction, optional defaults, model, messages empty/with-data, token count) | 6 | PASS |
| Extension Events (subscribe and publish, unsubscribe-all cleanup) | 2 | PASS |
| Command Types (construction, register and lookup) | 2 | PASS |
| Quarantine (unknown state, disable, idempotent disable, format status, list empty) | 5 | PASS |
| Extension Catalog (valid name, ext-info struct) | 2 | PASS |
| Manifest Audit (missing dir, valid package, missing file) | 3 | PASS |
| Compact Context (summary no-dir, summary with-dir) | 2 | PASS |
| with-hook-block-guard (pass case, block case) | 2 | PASS |
| Shared Command Types (construction) | 1 | PASS |
| **Total** | **72** | **ALL PASS** |

## Findings

### FINDING-001 (info): 5-tier cumulative capability system with compile-time hook validation

**Severity:** Info (positive finding)
**Category:** Architecture / Security
**Description:** The extension tier system provides a well-designed privilege escalation model:

1. **5 ordered tiers** (cumulative — each tier includes all lower capabilities):
   - `hooks` — hook dispatch only (lowest privilege)
   - `commands` — hook dispatch + command registration
   - `session` — + session lifecycle + compaction hooks
   - `providers` — + provider registration
   - `tui` — + TUI panels, keybindings, rendering (highest privilege)

2. **Hook point → tier mapping**: Each hook point maps to a minimum required tier. Unknown hook points default to `hooks` (lowest).

3. **Compile-time validation**: The `define-q-extension` macro validates hook point names at expansion time using `valid-hook-name?` from the hook schema. Unknown hook points cause a syntax error immediately.

4. **Runtime validation**: `validate-extension-tier` checks that all hooks in an extension are within the declared tier. Violations return descriptive error messages.

**Impact:** Positive — defense-in-depth security model with both compile-time and runtime validation.

### FINDING-002 (info): Thread-safe extension registry with insertion-ordered dispatch

**Severity:** Info (positive finding)
**Category:** Concurrency
**Description:** The extension registry uses a semaphore-protected box containing a struct with both an ordered list and a hash index:
- **Insertion order preserved**: Extensions are dispatched in registration order (list-based)
- **O(1) lookup**: Hash index for name-based lookup
- **Thread-safe**: All operations use `call-with-semaphore`
- **Replace on re-register**: Registering an extension with the same name replaces the old one and appends to end of list (changes dispatch order)

**Impact:** Positive — correct concurrent access with deterministic dispatch ordering.

### FINDING-003 (info): Per-hook-point action schema with 60+ defined hook points

**Severity:** Info (positive finding)
**Category:** Architecture
**Description:** The hook action schema (`hook-action-schemas`) defines valid actions for each of 60+ hook points. Each hook point specifies which actions are valid:
- Most hooks allow `'(pass amend block)`
- Some are restricted: `message-update` → `'(amend)` only, `message.stream.delta` → `'(pass)` only
- GUI hooks have appropriate restrictions: `gui.window.opened` → `'(pass)`, `gui.theme.changed` → `'(pass amend)`
- `validate-hook-result` checks that returned actions are valid for the specific hook point

The `with-hook-validation` combinator provides defense-in-depth: it validates both that the result is a `hook-result?` struct AND that the action is valid for the specific hook point.

**Impact:** Positive — fine-grained hook contract enforcement prevents extensions from returning unexpected actions.

### FINDING-004 (info): Hook dispatch with amend chaining and block short-circuit

**Severity:** Info (positive finding)
**Category:** Architecture
**Description:** The `dispatch-hooks` function implements a clean pipeline:
- **Pass**: Continue with unchanged payload
- **Amend**: Replace payload for next handler (payload flows through chain)
- **Block**: Immediately stop dispatch, return block result
- **Error handling**: Critical hooks default to `block` on error (safety-first), advisory hooks default to `pass` (liveness-first)
- **Timeout**: Each handler has a per-call timeout (default 500ms) with thread cleanup

The `with-hook-block-guard` helper extracts the common pattern of dispatching a hook and branching on block vs pass, reducing boilerplate at 3+ call sites.

**Impact:** Positive — well-designed hook pipeline with safety-first defaults.

### FINDING-005 (info): Comprehensive manifest validation with path traversal protection

**Severity:** Info (positive finding)
**Category:** Security
**Description:** The `safe-manifest-file-path?` predicate prevents path traversal attacks:
- Rejects paths containing `..` (directory traversal)
- Rejects absolute paths starting with `/`
- Rejects Windows drive letters (`C:/...`)
- Rejects empty strings

The `validate-manifest` function checks:
- Name and description are non-empty strings
- Version looks like semver (x.y.z)
- API version is a digit string
- Type is one of `extension`, `skill`, `bundle`
- All file paths pass `safe-manifest-file-path?`
- Compat string looks like a semver range
- Compatibility hash has required `min-q-version`

**Impact:** Positive — strong input validation prevents path traversal and malformed manifests.

### FINDING-006 (info): Quarantine system with file locking and atomic state writes

**Severity:** Info (positive finding)
**Category:** Reliability / Security
**Description:** The quarantine system provides extension lifecycle management:
- **File locking**: Uses atomic lock file creation (`#:exists 'error`) to prevent TOCTOU race conditions
- **Atomic state writes**: State file written to temp file, then renamed (POSIX atomic rename)
- **Permissions**: State file set to `#o600` (owner read/write only)
- **State corruption recovery**: If state.json is corrupted, resets to empty state (no crash)
- **Three states**: `active`, `disabled`, `quarantined`
- **Quarantine moves directory**: Physically moves extension directory to quarantine root
- **Restore moves back**: Restores from quarantine to original path

**Impact:** Positive — robust lifecycle management with crash-safe state persistence.

### FINDING-007 (info): Extension events with per-extension subscription tracking

**Severity:** Info (positive finding)
**Category:** Architecture
**Description:** The extension event system wraps the event bus with per-extension subscription tracking:
- **Track subscriptions**: Each `ext-subscribe!` call records the subscription ID under the extension name
- **Bulk cleanup**: `ext-unsubscribe-all!` removes all subscriptions for an extension (called on unload)
- **Thread-safe**: Subscription tracking uses semaphore-protected hash
- **Error isolation**: Cleanup errors are logged but don't propagate (already-unsubscribed IDs are safe)

This design prevents subscription leaks when extensions are unloaded.

**Impact:** Positive — clean resource lifecycle management for event subscriptions.

### FINDING-008 (info): Extension context provides 16-field rich API surface

**Severity:** Info (positive finding)
**Category:** API Design
**Description:** The extension context (`extension-ctx`) bundles all session/runtime state that extensions need:
- **Core**: session-id, session-dir, event-bus, extension-registry
- **Model**: model-name, provider-registry
- **Session state**: session-messages (read-only history), session-token-usage
- **Registries**: tool-registry, command-registry
- **Interaction**: ui-channel, cancellation-token, working-directory
- **GSD**: gsd-ctx (per-session GSD state)
- **Schema version**: ctx-version (for future migration)

All fields are read-only after construction. Optional fields default to `#f`. The context provides convenience methods: `ctx-register-provider!`, `ctx-list-providers`, `ctx-session-messages`, `ctx-session-token-count`.

**Impact:** Positive — comprehensive but read-only context prevents unauthorized state mutation.

## Architecture Summary

The Extension subsystem is organized in layers:

1. **Foundation** (`util/extension/extensions.rkt`, `util/extension/extension-types.rkt`):
   - Extension struct with guard validation
   - Extension context struct (16 fields, transparent)
   - No runtime dependencies (pure types)

2. **Core API** (`extensions/api.rkt`):
   - Thread-safe registry (semaphore + box + list + hash)
   - Registration, lookup, listing, handlers-for-point

3. **Hook System** (`util/hook-types.rkt`, `extensions/hooks.rkt`, `extensions/combinators.rkt`):
   - Hook result types (pass/amend/block) — Typed Racket module
   - Per-hook-point action schema (60+ defined points)
   - Dispatch with amend chaining and block short-circuit
   - Combinators: with-timeout, with-error-policy, with-hook-validation

4. **Tier System** (`extensions/tiers.rkt`):
   - 5-tier cumulative capability model
   - Hook point → tier mapping
   - Compile-time and runtime validation

5. **Extension APIs** (`extensions/context.rkt`, `dynamic-tools.rkt`, `ext-commands.rkt`, `message-inject.rkt`, `events.rkt`):
   - Rich context with 16 fields
   - Dynamic tool registration
   - Slash command registration
   - Message injection (system/user/assistant)
   - Event subscription tracking

6. **Package Management** (`extensions/manifest.rkt`, `manifest-audit.rkt`):
   - QPM manifest (struct, validation, serialization, checksum)
   - Package auditing (missing files, extra files, checksum verification)

7. **Lifecycle** (`extensions/quarantine.rkt`, `extensions/loader.rkt`, `runtime/extension-catalog.rkt`):
   - Quarantine with file locking and atomic state
   - Dynamic loading with manifest validation and integrity checking
   - Discovery and activation (symlink-based)

8. **Macro DSL** (`extensions/define-extension.rkt`):
   - `define-q-extension` macro with compile-time hook validation
