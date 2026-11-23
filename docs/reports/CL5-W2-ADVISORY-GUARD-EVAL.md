# CL5 W2 — advisory guard evaluation over the frozen corpus

Issue #9996. **Advisory only**: nothing here runs `./qt quick`, executes tests,
writes the admission record, or changes selection/scheduling/CI policy. Tool:
`scripts/cl5/guard-evaluation.rkt`; tests: `tests/test-cl5-guard-evaluation.rkt`.
Base: `abc6928a3babea7f47e8e1ef6ab7744dfbc07f92`.

## What was added

1. **Contradictory shipped evidence is rejected.** `guard-decision` now escalates
   `shipped-evidence-inconsistent` when the shipped selection reports `'ok` but carries no
   selected set. This closes the nonblocking finding from the PR #9994 review; the previous code
   treated `'ok` with `#f` as "nothing to check".
2. **Explicit closure-validity argument.** `ownership-repair-eval.rkt`'s `classify-candidate` no
   longer defaults `closure-ok?` to `#t`; every caller must pass inspected evidence, so the
   fail-closed branch cannot be skipped by omission. The repair-eval test vocabulary now also
   accepts `evidence-unavailable`.
3. **Advisory evaluation runner.** It feeds the frozen corpus changed-file sets (from
   `scripts/cl5/selection-cases.rkt`) and a deterministic set of low-fan-in covered modules (the
   audit's `covered` class, path-sorted) through `guard-decide-repo`, and records verdict, lane,
   escalation codes, named tests, selected set and the evidence summary.

## Result at `abc6928a`

| Case | Kind | Verdict | Lane | Escalations |
|---|---|---|---|---|
| D5 (`extensions/gsd/wave-status.rkt`) | corpus | escalate | comprehensive | `unadmitted-cover` (names `tests/test-gsd-wave-status.rkt`, `tests/test-audit-v09945-w3-gsd.rkt`) |
| F-PE-1 (`runtime/auto-retry.rkt`) | corpus | escalate | comprehensive | `shipped-escalation-preserved`, `uncovered-module`, `zero-selection` |
| `agent/blackboard-reducer.rkt` | control | selected | quick | none |
| `extensions/gsd/delivery-outcome.rkt` | control | selected | quick | none |
| `llm/anthropic-helpers.rkt` | control | selected | quick | none |

Interpretation:

- Every frozen corpus change escalates with at least one named reason; none reports a green.
- The D5 advisory names the documented detector — the same gap the W2 measurement observed.
- F-PE-1 has no declared detector for `runtime/auto-retry.rkt` (`uncovered-module`) and the shipped
  selector already needs a conservative fallback (`shipped-escalation-preserved`); the guard
  preserves that instead of downgrading it.
- Clean controls with audited, freshly admitted detectors are selected **without** escalation, so
  the guard is not blanket-escalating.

## Claims and non-claims

The guard claims `attribution-and-admission-only`: a green means a selectable detector existed and
the shipped selector ran it — **not** that the detector detects a given mutation. The advisory
cannot close the high-risk gap set; it converts silences into named escalations.

## Reproduction

```
racket -e '(require (file "scripts/cl5/guard-evaluation.rkt")) (run-guard-evaluation (current-directory))'
raco test tests/test-cl5-guard-evaluation.rkt
```

4 tests: corpus list identity, every corpus change escalates and never goes green, the D5 advisory
names its detector, and clean controls are selected without escalation. The contradictory-evidence
rule has its own test in `tests/test-cl5-ownership-audit.rkt`.

## Bounds

Offline and read-only. No production selector, CI, admission, scheduler or corpus change; W3 and the
40 % performance claim remain on hold.
