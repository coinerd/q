# Extension Context Characterization — v0.99.88 W0

**Issue:** #9221 · **Milestone:** #875
**Baseline:** `0e78c51f` (v0.99.87 release)
**Tests:** `tests/test-extension-context-characterization.rkt` (CH1–CH8)
**Authority:** roadmap v0.99.88 W0; freeze contract `docs/architecture/maintainability-roadmap-freeze-v0.99.87.rktd`

## 1. Session-type boundary — verified closed (MA-03 evidence)

`util/extension/extension-types.rkt` is the pure type module for the extension
context. Source scan (CH1) confirms it contains **no `runtime/` import**. Its
only cross-layer require is `extensions/api.rkt` (for `extension-registry?`
predicate re-export). The previous session-type coupling (extension context
carrying concrete Runtime session types) is closed; the remaining concrete
Runtime coupling is the provider registry only.

## 2. Provider registry coupling — the remaining Runtime import

`extensions/context.rkt` imports from `runtime/provider/provider-registry.rkt`:

| Import | Used by |
|---|---|
| `register-provider!` | `ctx-register-provider!` |
| `unregister-provider!` | `ctx-unregister-provider!` |
| `list-providers` | `ctx-list-providers` |
| `lookup-provider` | `ctx-lookup-provider` |
| `provider-info?`, `provider-info-provider` | contract/return types |

Policy exception `docs/architecture/dependency-policy.rktd` entry for
`context.rkt` is already pair-precise: boundary `runtime`, destination
`runtime/provider/provider-registry.rkt`, owner `extensions`, revisit
2026-10-01, rationale names the v0.99.88 service isolation. **No stale
session-type rationale remains** (the pair-precise wording was introduced in
#9218 / v0.99.87 W1 and is verified present here); no policy edit required in
W0.

## 3. Consumer inventory of the ctx-* provider wrapper API

| Wrapper | Production consumers | Test consumers |
|---|---|---|
| `ctx-register-provider!` | **0** | `test-wave4-sdk-expansion.rkt`, `test-extension-context-characterization.rkt` (CH3/CH4b/CH5) |
| `ctx-unregister-provider!` | **0** | `test-wave4-sdk-expansion.rkt`, `test-extension-context-characterization.rkt` (CH3/CH4) |
| `ctx-list-providers` | **0** | `test-wave4-sdk-expansion.rkt`, `test-extension-context-characterization.rkt` (CH3/CH4/CH4b/CH5) |
| `ctx-lookup-provider` | **0** | `test-wave4-sdk-expansion.rkt`, `test-extension-context-characterization.rkt` (CH3/CH4/CH4b/CH5) |
| `ctx-provider-registry` (accessor) | **0** production (only example comments + tests) | `test-extension-context.rkt` |
| `extension-ctx-provider-registry` (struct accessor) | re-export only in `extension-types.rkt`/`context.rkt` | none (no test references the raw accessor) |

CH8 pins this: scanning `extensions/`, `runtime/`, `wiring/`, `agent/`,
`tools/`, `tui/`, `interfaces/` finds zero production call sites outside the
defining module. **Implication for W1/W2:** the compatibility shim can be thin;
no production extension depends on the current direct delegation today.

## 4. extension-ctx struct surface (CH2)

- 16 read-only fields (session-id … ctx-version), transparent, no mutators.
- `struct->vector` = tag + 16 fields.
- Optional fields default to `#f`; `ctx-version` defaults to 1.

## 5. Construction roots

| Root | Location | Notes |
|---|---|---|
| Session setup | `runtime/extension-setup.rkt` `register-session-extensions!` | builds ctx with `#:gsd-ctx (current-gsd-ctx)`, dispatches `register-tools` |
| Session switch | `runtime/session/session-switch.rkt` | injectable `#:make-ctx` factory |
| Adapter factory | `runtime/layer-adapters.rkt` | re-exports `make-extension-ctx` |
| SDK/tests | `interfaces/sdk`, `scripts/sdk-gsd-integration-test.rkt` | direct construction |

CH6 pins the null path (`register-session-extensions!` with `#f` registry
returns `'()`) and the gsd-ctx wiring contract used by the session setup root.

## 6. Error-case characterization (CH4, CH5)

- **Null registry** (ctx built without `#:provider-registry`): `ctx-register-provider!`
  returns `(hasheq 'error #t 'message ...)`; `ctx-list-providers` → `'()`;
  `ctx-lookup-provider` → `#f`; `ctx-unregister-provider!` is a no-op. No
  exception is raised.
- **Closed/null session** (CH4b): a ctx built without `session-dir`/`session-store`
  (closed-session state) is registry-safe — register/list/lookup work with a
  real registry and degrade safely without one; `ctx-session-store` stays `#f`
  (no hidden state, CH4c).
- **Idempotency**: duplicate `register` returns `'updated`; unregister of an
  unknown name is a no-op.
- **Shared registry / concurrency**: two ctx values sharing one registry observe
  each other's registrations; registration order is preserved by the registry.

## 7. GSD integration

`ctx-gsd-ctx` carries the per-session GSD state (C-01); `register-session-extensions!`
passes `(current-gsd-ctx)`. CH6b pins the wiring expression. GSD command
handlers consume `current-gsd-ctx` directly (extensions/gsd), not via the
extension ctx — no new coupling introduced.

## 8. Dynamic loading

`extensions/loader.rkt` `load-extension!` / `try-load-extension` /
`discover-extensions` cover dynamic module loading; existing coverage in
`tests/test-extension-loader.rkt`. Extension handlers receive the ctx through
hook dispatch (`dispatch-hooks` with `#:ctx`), which is already characterized
in `tests/test-extension-context.rkt`.

## 9. W0 acceptance

- [x] Session-type finding reverified (no Runtime session-type import; CH1)
- [x] Registry import + all ctx-* consumers inventoried (§2, §3; CH8)
- [x] extension-ctx field/construction characterization pinned (CH2, CH6)
- [x] session-switch/resume construction-root contract pinned (CH7)
- [x] Null/closed/concurrent registry error cases characterized (CH4, CH4b,
      CH4c, CH5)
- [x] GSD wiring pinned (CH6b); GSD command-handler ctx delivery is covered by
      pre-existing tests (`tests/test-gsd-planning.rkt`, `test-gsd-go-orchestrator.rkt`)
- [x] Dynamic loading characterized via pre-existing `tests/test-extension-loader.rkt`
      (loader API listed in §8; no new loader tests needed in W0)
- [x] Stale policy rationale verified absent (#9218 introduced pair-precise
      wording; §2)
- [x] No production change
- [x] Focused Extension/Registry/GSD gate (8 files / 237 tests) + Fast
      (1053 files / 15366 tests) green

§5–§8 below document the session-switch/resume/GSD/dynamic-load surface;
the new test file pins the fields (CH2), construction roots (CH6), and the
session-switch rebind/resume construction contract (CH7). Session-switch and
resume lifecycle behavior itself is covered by pre-existing
tests/test-session-switch.rkt; dynamic loading by tests/test-extension-loader.rkt.

## 10. Scope freeze input for W1–W3

- W1 defines the neutral host service protocol for the four registry
  operations (register/unregister/list/lookup) — no struct accessors.
- W2 removes the direct `runtime/provider/provider-registry.rkt` import from
  `extensions/context.rkt`; the `ctx-*` facade delegates through the injected
  host service. Zero production consumers → compatibility shim stays thin;
  CH8 keeps the inventory honest.
- W3 isolates `ext-package-manager.rkt` (unchanged in W0).
- W4 decides the three TUI bridge exceptions (unchanged in W0).
