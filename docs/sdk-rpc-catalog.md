# q SDK & RPC Catalog

<!-- verified-against: 0.20.2 -->

This document catalogs all public SDK and RPC interfaces provided by q.

## SDK Interface (`interfaces/sdk.rkt`)

The SDK is the primary programmatic interface for embedding q in other applications.

### Exported Types

| Type | Description |
|------|-------------|
| `runtime-config` | Configuration struct for the q runtime |
| `runtime` | Opaque runtime handle returned by `make-runtime` |

### Exported Procedures

| Procedure | Contract | Description |
|-----------|----------|-------------|
| `make-runtime` | `(->* (#:provider any/c) ... runtime?)` | Initialize a q runtime with the given LLM provider |

### Runtime Config Fields

| Field | Type | Description |
|-------|------|-------------|
| `provider` | `any/c` | LLM provider instance |
| `tool-registry` | `tool-registry?` | Registered tools |
| `event-bus` | `event-bus?` | Event bus for notifications |
| `session-store` | `session-store?` | Session persistence backend |

## RPC Interface (`wiring/rpc-methods.rkt`)

The RPC interface exposes q operations over JSON-RPC for external integrations.

### Exported Procedures

| Procedure | Contract | Description |
|-----------|----------|-------------|
| `make-core-rpc-handlers` | `(-> hash? (hash/c symbol? procedure?))` | Create RPC method handlers from dependencies |

## RPC Mode (`interfaces/rpc-mode.rkt`)

RPC mode provides a JSON-RPC server interface for programmatic control.

### Exported Procedures

| Procedure | Description |
|-----------|-------------|
| `run-rpc-mode` | Start the RPC server loop |

## Extension Points

Extensions interact with the agent through the extension context (`extensions/context.rkt`) and hook system (`extensions/hooks.rkt`). See the [Extension API](../extensions/api.rkt) for details.

## Stability Tiers

| Module | Tier | Notes |
|--------|------|-------|
| `interfaces/sdk.rkt` | **stable** | Public SDK, breaking changes require major version bump |
| `wiring/rpc-methods.rkt` | **stable** | RPC protocol, breaking changes require major version bump |
| `interfaces/rpc-mode.rkt` | **evolving** | May change between minor versions |
| `extensions/api.rkt` | **stable** | Extension API contract |
| `extensions/hooks.rkt` | **evolving** | New hook types may be added |

---

*This catalog is auto-generated from source `provide` forms. Run `racket scripts/gen-sdk-catalog.rkt` to regenerate.*
