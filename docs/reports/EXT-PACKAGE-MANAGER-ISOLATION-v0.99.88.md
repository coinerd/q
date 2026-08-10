# Ext-Package-Manager Isolation — v0.99.88 W3

**Wave:** v0.99.88 W3 · **Issue:** #9224 · **Finding:** MA-04 (closure)
**Scope:** isolate package lifecycle management behind a host/wiring adapter;
no install logic in util; `ext-package-manager.rkt` is the ONLY remaining
Runtime-boundary extension exception (now removed). Behavior equivalent
(dual-run E1–E7).

## 1. Result

`extensions/ext-package-manager.rkt` no longer imports any Runtime module.
Its package lifecycle operations are consumed through the injected neutral
`package-host-service` capability (`util/extension/host-services.rkt`), which
is constructed by the Runtime adapter
(`runtime/extension-host-adapter.rkt` → `make-package-host-service`) wrapping
the concrete `runtime/package.rkt`. The pair-precise policy exception for
`ext-package-manager.rkt` is removed — **zero runtime-boundary extension
exceptions remain** (MA-04 closed).

Resolved import closure of `extensions/ext-package-manager.rkt` (verified with
`extract-requires` + H4 probe):

- `racket/contract`, `racket/string`
- `extensions/define-extension.rkt`, `dynamic-tools.rkt`, `context.rkt`,
  `hooks.rkt`, `tool-api.rkt`
- `util/extension/host-services.rkt` (neutral protocol only)

## 2. What changed

| File | Change |
|---|---|
| `util/extension/host-services.rkt` | Added the neutral `package-host-service` protocol struct (contracted op fields: package-list, package-installed?, package-install, package-remove) and the pure `package-summary` (name, version) type plus `package-host-service-capabilities` metadata. Zero new imports. |
| `runtime/extension-host-adapter.rkt` | Added `make-package-host-service` — wraps concrete `runtime/package.rkt` ops, converts `qpm-package` → `package-summary`. Single Runtime touchpoint. |
| `runtime/layer-adapters.rkt` | Re-export `qpm-manifest-version` (one identifier added to the existing manifest facade; no new import). |
| `extensions/context.rkt` | Added `#:package-service` kwarg (`(or/c any/c #f)`) + `ctx-package-service` accessor; struct field added in `util/extension/extension-types.rkt`. No runtime import. |
| `extensions/ext-package-manager.rkt` | Removed `../runtime/package.rkt` and `../extensions/manifest.rkt` imports; handler dispatches on the injected service; direct calls without a service degrade to the safe null service (empty list / not-installed / error results). Public API (`handle-ext-pkg`, `register-ext-pkg-tools`, extension) preserved. |
| `runtime/extension-setup.rkt` | Inject `#:package-service (make-package-host-service)` at ctx construction (wiring adapter). |
| `docs/architecture/dependency-policy.rktd` | `ext-package-manager.rkt` exception removed (extensions exceptions 4 → 3; runtime-boundary list now empty). |
| `tests/test-package-host-service-isolation.rkt` (new) | E1–E7 dual-run: neutral shape, list/installed?/install/remove parity (direct vs adapter), error-string pass-through, null-service degradation, ctx-injected tool path. |
| `tests/test-arch-fitness.rkt` | H4 (no runtime import in ext-package-manager), H5 (adapter owns package.rkt + host-services), H6 (neutral protocol exports the capability). |
| `tests/test-extension-exception-fitness.rkt` | Count 4 → 3, membership updated, runtime-boundary list `'()`. |
| `tests/test-extension-context-characterization.rkt`, `tests/test-hook-expansion.rkt` | Field-count/arity updates for the new optional `package-service` field. |

## 3. Design notes

- **No install logic in util**: `runtime/package.rkt` is untouched; all
  install/remove/list/filesystem logic stays in Runtime. The neutral protocol
  carries only pure `package-summary` data (name/version) across the boundary.
- **Injection, not import**: the extension reads the service from the ctx at
  registration time (`register-ext-pkg-tools`) and closes it over in the tool
  handler; direct `handle-ext-pkg` calls without a service use the null
  service (safe defaults, matching the W2 absent-registry precedent).
- **Null service semantics**: list → `'()` ("No packages installed."),
  installed? → `#f`, install → error string, remove → `#f`.
- **Blame model**: op-field contracts in `package-host-service` blame the
  extension caller for domain violations and the Runtime adapter for range
  violations (same model as `provider-host-service`, W1).
- **Capability metadata**: `package-host-service-capabilities` mirrors the
  four ops (name/contract/lifetime/owner), per the W1 risk-control rule.

## 4. Dual-run equivalence (E1–E7)

- E2: service list == direct `list-packages` (sorted, summarized).
- E3: `installed?` parity before/after install.
- E4: install success summary name/version == manifest of direct result;
  missing-qpm.json error STRING passed through verbatim.
- E5: remove parity (unknown → #f both; installed → #t both).
- E6/E6b: null-service degradation of `handle-ext-pkg` (no service / `#f`).
- E7: ctx-injected service exposed via `ctx-package-service`; the registered
  `ext-package` tool performs real install/list against `current-packages-dir`.

## 5. Boundary & policy verification

- H4 negative probe: `extensions/ext-package-manager.rkt` imports no
  `runtime/` module (and `extract-requires` confirms it).
- H5: `runtime/extension-host-adapter.rkt` owns the `package.rkt` import and
  implements the neutral protocol.
- Exception-fitness gate reads the policy file (cannot drift): extensions
  exceptions = dialog-api, ui-surface, widget-lifecycle (3); runtime-boundary
  = `'()`.
- `runtime/package.rkt` unchanged; `current-packages-dir` parameter still
  honored (adapter reads it at call time — E-tests parameterize it).

## 6. Gates

| Gate | Result |
|---|---|
| Package (test-package + isolation + manifest) | ✅ 51/51 |
| Focused (13 files) | ✅ 243/243 |
| Security | ✅ 63 files / 702 tests |
| Arch | ✅ 21 files / 235 tests |
| Fast | ✅ 1056 files / 15403 tests |
| lint-format | ✅ 2081 files, 0/0 |

## 7. W4 handoff

TUI bridge exceptions (`dialog-api.rkt`, `ui-surface.rkt`, `widget-lifecycle.rkt`)
are now the ONLY remaining extension exceptions (3). W4 decides each
individually and releases v0.99.88 (Broad + Arch + Security + Smoke +
Release gates; all five old exceptions terminal and none expired).
