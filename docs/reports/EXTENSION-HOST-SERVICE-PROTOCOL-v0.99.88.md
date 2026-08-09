# Extension Host Service Protocol — v0.99.88 W1

**Wave:** v0.99.88 W1 · **Issue:** #9222 · **Findings:** MA-02/MA-03

**Scope:** define the neutral protocol and Runtime adapter; do **not** migrate
`extensions/context.rkt` yet (that is W2).

## 1. Result

W1 introduces a named, contracted provider-registry host capability in
`util/extension/host-services.rkt` and a Runtime-owned adapter in
`runtime/extension-host-adapter.rkt`.

The neutral module imports only `racket/contract`. It does not know the
concrete registry, provider structs, Runtime, LLM, tools, TUI, or extension
implementation modules. The adapter is the only new module that imports the
concrete `runtime/provider/provider-registry.rkt` implementation.

The old `ctx-register-provider!` / unregister / list / lookup facade remains
unchanged in W1. W2 will delegate that facade through this injected service and
remove the direct Runtime import from `extensions/context.rkt`.

## 2. Normative capability table

| Capability name | Contract | Lifetime | Owner | Semantics |
|---|---|---|---|---|
| `provider-registry.register!` | `(->* (string? any/c) (#:config hash?) (or/c 'registered 'updated))` | session | Runtime | Registry validates the opaque provider instance; new vs replacement result preserved. |
| `provider-registry.unregister!` | `(-> string? void?)` | session | Runtime | Removes provider and models; unknown name is a no-op. |
| `provider-registry.list` | `(-> list?)` | session | Runtime | Returns Runtime-owned opaque descriptors in registry order. |
| `provider-registry.lookup` | `(-> string? (or/c any/c #f))` | session | Runtime | Returns an opaque descriptor or `#f`. |

The same names/lifetimes/owners are machine-auditable via
`provider-registry-capabilities` (`host-capability-descriptor` values).
Operation contracts are attached to the `provider-host-service` struct fields
with `contract-out`.

## 3. Design controls

### No service locator

The protocol is a named four-operation struct, not a symbol-keyed map. A caller
cannot discover arbitrary services or bypass capability contracts.

### No untyped callback bundle

Each operation field has an explicit higher-order contract. Domain violations
(non-string names, invalid config shape) blame the extension caller; range
violations blame the Runtime adapter. The concrete registry additionally
validates `provider?`, preserving the existing `exn:fail:contract` behavior.

### Opaque Runtime values

Provider instances and descriptors cross the neutral boundary as opaque
`any/c` values. Extensions receive no concrete registry/provider-info struct
accessors and must not construct or destructure Runtime-owned descriptors.
This preserves behavior without moving Runtime implementation or LLM types
into `util/`.

### Lifetime and owner

`make-provider-host-service` closes over one concrete provider registry. The
service therefore has the same session lifetime as that registry; Runtime owns
construction, validation, synchronization, and teardown.

## 4. Boundary evidence

`tests/test-extension-host-service-protocol.rkt` pins:

- **P1:** protocol surface and complete capability metadata;
- **P2:** neutral module has no Runtime/LLM/tools/TUI/extensions import;
- **P3:** adapter constructor enforces `provider-registry?`;
- **P4/P4b:** dual-run equivalence to direct registry calls, including
  registration/update, order, lookup, unregister, and invalid-provider errors;
- **P5:** operation contracts reject domain violations;
- **P6/P6b:** fresh-namespace `dynamic-require` and concrete-import placement;
- **P6c:** a temporary Typed Racket client compiles and imports the protocol via
  `require/typed` as an opaque type;
- **P7:** the old ctx-* compatibility facade is unchanged in W1.

`tests/test-arch-fitness.rkt` adds permanent invariants:

1. `util/extension/host-services.rkt` cannot import a concrete host layer;
2. `runtime/extension-host-adapter.rkt` must own the concrete registry import
   and implement the neutral protocol.

## 5. W2 handoff

W2 must:

1. inject `provider-host-service` at every extension-context construction root;
2. preserve the public `#:provider-registry` keyword and ctx-* signatures as a
   temporary compatibility facade where required;
3. delegate the four ctx-* operations through the injected service;
4. remove all six concrete provider-registry imports from
   `extensions/context.rkt` (two are already unused);
5. dual-run old/direct and new/adapter paths for result, error, and ordering
   parity;
6. remove the pair-precise policy exception only after the negative import
   probe rejects reintroduction.

W1 intentionally performs none of these migration steps, keeping the protocol
and adapter reviewable independently from the high-risk construction-root
cutover.

## 6. W1 acceptance and gates

- [x] Four minimal capability-oriented registry operations defined; no concrete
      registry struct accessor exported.
- [x] Runtime adapter encapsulates the concrete provider registry.
- [x] Every capability has a name, contract, session lifetime, and Runtime owner.
- [x] No service-locator map and no untyped callback bundle.
- [x] Existing ctx-* facade remains behaviorally unchanged (P7).
- [x] Contract, registry parity, fresh-namespace dynamic-require, and Typed
      Racket boundaries pinned (P1–P6c).
- [x] Contract blame pinned to the extension caller (P5b).
- [x] Focused: 4 files / 76 tests, all green.
- [x] Arch: 21 files / 231 tests, all green.
- [x] Fast: 1054 files / 15382 tests, all green.
- [x] `racket scripts/lint-format.rkt`: 2079 files, 0 errors, 0 warnings.

## 7. Reviewer remediation (W1)

Independent review: **APPROVED** (one pass; no REQUEST_CHANGES round). Three
non-blocking MINOR findings were folded in as hardening:

1. `#:transparent` removed from `provider-host-service` and
   `host-capability-descriptor` so the neutral structs are opaque by default,
   matching the opaque-value boundary narrative (no test depended on
   transparency).
2. Capability metadata drift risk consciously accepted: descriptors carry
   name/lifetime/owner/summary per spec; operation contracts live on the
   struct fields. P1 pins names; P5 pins contract enforcement; P5b pins blame.
3. P5b added: contract violations must name `blaming:` + the caller module +
   the violated operation (`register-provider!`), guarding blame regression.
