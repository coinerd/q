<!-- verified-against: 0.20.1 -->
# API Stability Tiers

<!-- verified-against: 0.20.1 -->

This document defines the stability tiers for q's public interfaces. Each module
is assigned a tier that determines the backwards-compatibility guarantees and
breaking-change policy.

## Tier Definitions

| Tier | Stability | Breaking Changes | Version Impact |
|------|-----------|-----------------|----------------|
| **stable** | Production-ready. API is frozen. | Only on MAJOR version bumps. Requires migration guide. | `MAJOR.0.0` |
| **evolving** | Functional but may change. | Allowed on MINOR bumps with changelog entry. | `x.MINOR.0` |
| **experimental** | Under active development. | May break on any bump. Use at your own risk. | Any |
| **internal** | Not for external use. | No guarantees. May change without notice. | Any |

## Module Stability Assignments

### Stable

These modules form the public API contract. Breaking changes require:
1. A MAJOR version bump (e.g., 0.x → 1.0)
2. A migration guide in `docs/migration/`
3. At least one minor version of deprecation warnings

| Module | Interface | Since |
|--------|-----------|-------|
| `util/version.rkt` | `q-version` | v0.10.0 |
| `interfaces/sdk.rkt` | `make-runtime`, `runtime-config` | v0.10.0 |
| `wiring/rpc-methods.rkt` | `make-core-rpc-handlers` | v0.10.0 |
| `extensions/api.rkt` | `make-extension`, `extension?` | v0.10.0 |
| `util/protocol-types.rkt` | Core message/event types (formerly `agent/types.rkt`) | v0.10.0 |
| `tools/tool.rkt` | `make-tool`, `tool-registry?` | v0.10.0 |

### Evolving

These modules are functional but may receive backwards-compatible additions or
minor API adjustments between MINOR versions. Changes are documented in CHANGELOG.md.

| Module | Notes |
|--------|-------|
| `extensions/hooks.rkt` | New hook types may be added |
| `extensions/context.rkt` | Context fields may expand |
| `interfaces/rpc-mode.rkt` | Server interface may gain options |
| `runtime/session-store.rkt` | Storage API may gain methods |
| `runtime/model-registry.rkt` | Registry API under refinement |
| `llm/provider.rkt` | Provider protocol may extend |
| `tui/tui-init.rkt` | TUI entry point may gain config |

### Experimental

These modules are under active development. Use them, but expect changes.

| Module | Notes |
|--------|-------|
| `extensions/widget-api.rkt` | Widget system in design phase |
| `extensions/dialog-api.rkt` | Dialog API may change |
| `extensions/custom-ui-api.rkt` | Custom UI integration evolving |
| `extensions/message-inject.rkt` | Message injection API |
| `interfaces/sessions.rkt` | Session management API |

### Internal

These modules are implementation details. Do not depend on them externally.

| Module | Notes |
|--------|-------|
| `runtime/compactor.rkt` | Internal compaction logic |
| `runtime/context-builder.rkt` | Context assembly internals |
| `runtime/iteration.rkt` | Agent loop iteration |
| `runtime/auto-retry.rkt` | Auto-retry logic |
| `sandbox/*` | Sandbox internals |
| `tui/render.rkt` | TUI rendering internals |
| `wiring/run-modes.rkt` | Mode dispatch |

## Deprecation Policy

1. **Stable APIs**: Deprecated features must emit warnings for at least one MINOR
   version before removal. The warning message must point to the replacement API.

2. **Evolving APIs**: Best-effort deprecation notices. Check CHANGELOG.md for changes.

3. **Experimental/Internal**: No deprecation guarantees.

## Checking Stability

Run `racket scripts/lint-docs.rkt` to verify doc coherence. The
`docs/sdk-rpc-catalog.md` lists all SDK exports with their stability tier.

## Adding New Modules

When adding a new public module:
1. Start as **experimental** unless the API is well-defined
2. Promote to **evolving** after one MINOR version with no breaking changes
3. Promote to **stable** after two MINOR versions with no breaking changes
4. Update this document and `docs/sdk-rpc-catalog.md`
