# Semantic developer navigator (CL2, #9979)

Developer-only tooling for finding **current** ownership, static bindings, dependency/caller
evidence and a bounded test bundle for a q task. It lives in `scripts/semantic-navigator.rkt`;
it is not part of the trusted runtime, it does not execute tests, and it requires no LLM.

## What this is — and what it is not

- **A navigational fact is not a verified behavioral contract.** The navigator can say where a
  decision owner, a durable-state writer, a consumer, a `@covers` claim or a test lives. It
  cannot say that the component is safe to modify or verify in isolation. Behavioral oracles
  remain in the code/tests and in the authoritative CI lanes.
- The CL1 manifest's subsystems are **top-level directory groupings**, not component
  boundaries. This tool adds static module- and symbol-level resolution on top; capability
  boundaries are still recorded in the curated maps and remain an explicit modelling step.
- Freshness has three separate fields: **metadata availability** (including coverage
  availability), **source identity** (a digest of all tracked source fingerprints) and
  **regeneration-verified**, which is always `#f` because the byte-identical regeneration
  check (`tests/test-subsystem-maps.rkt`) is referenced but **not executed** here. `stale? #f`
  means only "no missing metadata detected", never "regeneration was validated".
- `context` reuses **qt's own** `changed-selection` over admitted owners. It is labelled a
  **reproduction aid**, never a PR verification claim; all required CI contexts remain
  unchanged and authoritative. No `./qt` gate or policy was modified.

## Commands

```sh
racket scripts/semantic-navigator.rkt symbol decide-campaign-action
racket scripts/semantic-navigator.rkt module agent/state.rkt
racket scripts/semantic-navigator.rkt context agent/state.rkt llm/provider-errors.rkt
racket scripts/semantic-navigator.rkt context --changed main..HEAD
racket scripts/semantic-navigator.rkt benchmark
```

Add `--machine` to any query for a canonical Racket datum (sorted keys, fixed ordering) instead
of the human rendering. All queries print the git revision, SHA-1 content fingerprints of the
inputs, the provenance paths and an `uncertainty` list. The SHA-1 values are freshness
fingerprints, not a security property.

### `symbol NAME`

Static definitions and exports with module + line, probable importers (a module that both
imports the defining module and textually contains the identifier), map excerpts and covering
tests. Probable importers are labelled `textual-reference-not-call-graph`; macros, aliases and
dynamic bindings can make that set wrong, so it is evidence to inspect, not a call graph.
Multiple defining modules produce `ambiguous-symbol`; an unmatched name produces
`no-static-definition` rather than an invented answer.

### `module PATH`

Subsystem, definitions, exports, direct static `require` edges, direct static callers, resolved
`dynamic-require` targets (`literal-resolved` or `runtime-path-resolved`, cross-checked against
the CL1 manifest marker) and `unresolved` targets with a reason. Owner tests from `@covers` are
distinguished from `coverage-unavailable`: missing/corrupt metadata is never reported as absent
ownership. Reader failures keep the reliable prefix and add `source-parse-incomplete`; generated
`struct-out` accessors are represented where statically established. Uncertainty names
unresolved targets, unresolved provide forms, missing owner tests, absent curated maps and
stale/missing metadata.

### `context PATH... | --changed RANGE`

Runs the same pure `changed-selection` function used by `./qt quick --changed`, over the same
admitted owners. The bundle contains the selected owners, their `@covers` sources, relevant
map paths, subsystems, provenance/fingerprints, escalation reasons and the reproduction label.
Selection may widen (fail open); it never silently narrows. A zero-test or empty selection is
not represented as a pass.

### `benchmark`

Mechanically checks that the frozen CL0 navigation tasks T1 (campaign decision), T3
(`agent/state.rkt` impact fan-out) and T4 (provider retry ownership) have their frozen facts
**contained** in navigator bundles. It reports only containment, bundle size and a limitation
string. It explicitly does **not** report agent trial measurements: tokens, file reads,
wall-clock/owner-location time and first-edit correctness remain **PENDING** under the frozen
protocol in `docs/reports/CL0-AGENT-TASK-BENCHMARK-003e425d.md`, which requires fresh contexts,
a fixed model/host and n ≥ 3 per task. Mechanical containment is not a correctness oracle and
must never be presented as measured cognitive-load reduction.

## Data sources and authority

Derived/current only:

- tracked `.rkt` sources (`git ls-files`) — static definitions, `provide` specs, `require`
  edges and literal `dynamic-require` targets, read with the Racket reader (no evaluation);
- `docs/architecture/subsystem-manifest.rktd` and `docs/architecture/subsystem-maps/*.md` —
  derived ownership/grouping facts from CL1;
- `tests/.coverage-manifest.json`, `tests/.quick-admission.rktd`,
  `reports/test-ownership-map.json` (consumed by the manifest) — existing ownership metadata;
- `scripts/testing/full-plan.rkt` and `scripts/testing/quick-audit.rkt` — qt's real selection
  and admission functions, required directly rather than reimplemented.

No new manually maintained inventory is introduced. Every output path is either derived from
these inputs or comes from a curated map that is itself pinned by `tests/test-subsystem-maps.rkt`.

## Verification and limitations

- Tests: `tests/test-semantic-navigator.rkt` pins ambiguous/missing symbols, dynamic edges,
  ownership gaps, stale/missing maps, reuse of qt selection and CL0 containment behaviour.
- The index caches per repository root, but reuse is gated by a digest of every tracked source's
  content (`source-identity`), so edits invalidate it automatically. `invalidate-index-cache!`
  remains available for explicit invalidation. Content fingerprints make the observed input set
  explicit.
- Static parsing does not expand macros or resolve arbitrary computed `provide`/`require`
  forms; those are reported as unresolved uncertainty, never guessed. `dynamic-require` targets
  bound by `define-runtime-path` are resolved statically; computed targets stay `unresolved`.
- Textual importer candidates are not proof of a call, correct binding or behavioral
  dependency. Dynamic requires, reflection and runtime-generated dependencies remain
  unresolved and require broader verification.
- The selective-verification defect corpus and the frozen CL0 task corpus are for later
  shadow-mode/safety evaluation (CL4/CL5). This tool is not an authority for PR verification
  and does not change the required CI contexts.

## Quantitative benchmark deferral

The CL2 acceptance criterion to compare context consumption, file reads, development time and
correctness against CL0 is **not satisfied by this implementation**: no fresh-context agent
trials were executed, and mechanical fact containment does not measure cognitive load. The
implementation therefore supports only the claim that relevant facts are *available* in a
bounded bundle. Quantitative comparison is proposed for explicit deferral to CL5 (#9982)
under the frozen CL0 protocol; that deferral requires maintainer approval before CL2 is
closed as fully measured.
