# Provider-Registry-Service Isolation — v0.99.88 W2

**Wave:** v0.99.88 W2 · **Issue:** #9223 · **Findings:** MA-02/MA-03 (closure)
**Scope:** remove the direct `runtime/provider/provider-registry.rkt` import
from `extensions/context.rkt`; delegate ctx-* provider operations behind the
injected neutral host service; remove the policy exception; negative probe
rejects reintroduction. Behavior must be equivalent (dual-run).

## 1. Result

`extensions/context.rkt` no longer imports any Runtime module. Its provider
registry field now carries the neutral `provider-host-service` value, and the
four ctx-* provider operations delegate to that service. The concrete registry
import exists in exactly one Runtime module: `runtime/extension-host-adapter.rkt`
(from W1). The pair-precise policy exception for `context.rkt` is removed.

Resolved import closure of `extensions/context.rkt` (verified with
`module->imports`):

- `racket/base`
- `racket/contract`
- `../util/extension/host-services.rkt` (neutral protocol)
- `../llm/provider.rkt` (`provider?` used by the unchanged public contract)
- `../util/extension/extension-types.rkt` (pure types)

## 2. What changed

| File | Change |
|---|---|
| `extensions/context.rkt` | Removed `(only-in "../runtime/provider/provider-registry.rkt" ...)` (4 used + 2 already-unused identifiers). Added neutral import; ctx-* wrappers dispatch on `provider-host-service?`; absent/non-service value behaves exactly like the historical null-registry path (error hash / void no-op / `'()` / `#f`). Public signatures and the `#:provider-registry` kwarg (contract `(or/c any/c #f)`) are preserved. |
| `tests/test-provider-registry-service-isolation.rkt` | NEW dual-run characterization D1–D7 (below). |
| `tests/test-arch-fitness.rkt` | H3 negative probe: `extensions/context.rkt` imports no `runtime/` module. |
| `tests/test-extension-exception-fitness.rkt` | Exception-count/membership gates updated: extensions exceptions 5 → 4; runtime-boundary list now `("ext-package-manager.rkt")` only. |
| `tests/test-extension-host-service-protocol.rkt` | P7 now injects a host service (facade behavior assertions unchanged). |
| `tests/test-extension-context-characterization.rkt` | CH3/CH4b/CH5/CH5b wrap the concrete registry once via `make-provider-host-service` (dual-run wiring pattern). |
| `tests/test-wave4-sdk-expansion.rkt` | 4 ctx-* sites wrap the registry via the adapter; direct `lookup-provider` assertions unchanged. |
| `docs/architecture/dependency-policy.rktd` | `context.rkt` exception row removed; closure note added. |

## 3. Dual-run characterization (roadmap risk control)

D1–D7 in `tests/test-provider-registry-service-isolation.rkt` prove the OLD
direct registry path and the NEW injected-service ctx path are behaviorally
identical:

- **D1** register → `'registered`, provider visible on both paths;
- **D2** re-register → `'updated`, single entry;
- **D3** lookup found/missing identical;
- **D4** list order: ctx facade returns exactly the registry's list order
  (registry order is `hash-values` order, not insertion order — pinned);
- **D5** unregister + unknown-unregister no-op (void) identical;
- **D6** invalid-provider and non-string-name contract errors identical;
- **D7** facade surface: injected service exposed via `ctx-provider-registry`,
  absent-service degradation unchanged, kwarg remains an `any/c` passthrough
  slot (non-service values stored/returned verbatim).

## 4. Compatibility facade

- Public signatures of `ctx-register-provider!` / `ctx-unregister-provider!` /
  `ctx-list-providers` / `ctx-lookup-provider` are unchanged.
- `#:provider-registry` kwarg contract remains `(or/c any/c #f)` — no new
  rejection for legacy values (a raw registry stored in the slot behaves as
  the historical absent-registry path; callers that need the registry wrap it
  once with `make-provider-host-service`, the documented W2 wiring pattern).
- Rollback during the wave: revert the internal delegation; facade and
  signatures need no changes.

## 5. Policy closure and negative probe

- `docs/architecture/dependency-policy.rktd`: the `context.rkt` exception row
  is deleted; the extensions section now has 4 exceptions
  (`dialog-api`, `ui-surface`, `widget-lifecycle`, `ext-package-manager`).
- **H3** (arch suite): `extensions/context.rkt` must not import any `runtime/`
  module — a reintroduced runtime import fails the gate immediately.
- `test-extension-exception-fitness.rkt` exception-count and membership gates
  updated to the W2 baseline; stale-destination and boundary probes unchanged.

## 6. Gates

| Gate | Result |
|---|---|
| Focused (6 files: isolation + protocol + characterization + context + wave4 + arch-fitness) | ✅ 149/149 |
| Arch | ✅ 21 files / 232 tests |
| Broad | pending |
| Fast | pending (after Broad) |
| lint-format | ✅ 2080 files, 0/0 |

## 7. W3 handoff

`ext-package-manager.rkt` is now the only remaining Runtime-boundary extension
exception. W3 will apply the same pattern: isolate the package lifecycle behind
a host/wiring adapter with no install logic in `util/`, package + Security +
Arch + Fast gates.
