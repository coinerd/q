# Abstraction Governance — v0.99.42

**Date:** 2026-06-24
**Milestone:** v0.99.42 (#829)
**Predecessor:** v0.99.41 (Release Smoke CI Actions Truth Remediation)

## Executive Summary

v0.99.42 was an abstraction-governance milestone focused on making
abstraction quality durable without destabilizing the now-green
release/CI pipeline. The work targeted high-risk boundary code
in release tooling, CI classification, and milestone gating.

The milestone delivered 10 waves of targeted improvements with zero
release/CI regressions. Key outcomes: scanner v5 with 3 new signals,
verdict predicate abstraction, 4 structured result types, a pure/effect
split for manifest construction, an explicit lifecycle state machine,
cwd-independent project root detection, common-case API ergonomics,
and design-fact comments at critical invariants.

## Governance Principles Applied

### §54: Practical Decision Table

Every wave was pre-scored on risk/reward before implementation.
Only green-rated candidates (reward ≥ 7, risk ≤ 6) were implemented
directly. Yellow-rated candidates (W7, W8) were piloted with
restricted scope.

### §55: Final Checklist

Each wave was accepted only when:
- Smoke gate passed (19 files, 286 tests)
- No public API rename without migration plan
- No cosmetic churn (comments must carry design weight)
- Metrics synced and prose linted

### Design-Fact Discipline (§44)

Four invariants now carry DESIGN FACT comments with explicit
"DO NOT" misuse-prevention directives:

1. **cancelled_superseded exclusion** — excluded from blocking
   verdicts despite `'blocking #t` in result hash. Misuse: adding
   it would make cancelled runs unconditionally block milestones.
2. **Validation tolerance** — manifest validation ignores unknown
   SHAs. Misuse: failing on missing data produces false positives.
3. **Pure/effect boundary** — `build-manifest` has no I/O. Misuse:
   adding I/O breaks unit testability without subprocess mocking.
4. **Lifecycle linearity** — milestone lifecycle is intentionally
   linear. Misuse: skip transitions break `can-close-milestone?`.

## Structural Patterns Introduced

### Structured Result Types

Replaced ad-hoc `(cons 'pass/'fail message)` pairs with transparent
structs carrying predicates:

| Type | File | Replaces |
|------|------|----------|
| `status-result` | scripts/status-result.rkt | `(cons 'ok/'fail)` |
| `dry-run-result` | scripts/release-dry-run.rkt | `(cons 'pass/'fail)` |
| `manifest` | scripts/gen-release-manifest.rkt | Unstructured JSON |
| `manifest-trace` | scripts/gen-release-manifest.rkt | Nested hash |
| `milestone-lifecycle-transition-result` | scripts/milestone-gate.rkt | Implicit transitions |

### Pure-Core / Effect-Shell

`build-manifest` is pure (no I/O), `collect-release-inputs` is the
effect shell (git, files, environment). This makes the manifest
construction unit-testable without subprocess mocking.

### Verdict Predicate Abstraction

Replaced bare string comparisons with named predicates:
`release-verdict-success?`, `ci-verdict-blocking?`,
`blocking-verdict?`, `superseded-verdict?`.

### Lifecycle State Machine

Explicit 6-state lifecycle: `planned → in_progress → release_ready
→ release_published → ci_green → closed`. Linear progression with
`can-close-milestone?` guard requiring both release and CI success.

## Metrics Summary

```text
Source modules: 701 (unchanged)
Source lines: ~112,375 (+575 from W0)
Test assertions: ~28,152 (+270 from W0)
Scanner signals: 11 (v5, +3 from v4)
Structured result types: 4 (+3)
DESIGN FACT comments: 10 (+4)
```

## Future Direction

1. **Migrate cwd-dependent scripts** — 23 of 26 sites remain on
   `(build-path (current-directory) ...)` pattern. `find-project-root`
   is ready; migration is safe but deferred to avoid churn.
2. **Scanner CI integration** — scanner v5 is ready to add as a
   lint job in `.github/workflows/ci.yml`.
3. **Broader pure/effect split** — extend the release-manifest pilot
   pattern to other I/O-heavy scripts.
4. **RED module first-slice** — begin thinning the largest RED modules
   (spawn-subagent.rkt 741L, anthropic.rkt 639L) by extracting pure
   helpers.
