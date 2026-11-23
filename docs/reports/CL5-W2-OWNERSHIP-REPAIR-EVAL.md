# CL5 W2 — ownership-repair evaluation for the 21 high-risk confirmed gaps

Bounded evaluation for issue #9995 (CL5 W2 follow-up). Read-only: **no owner is admitted, no
admission record is regenerated, no selector/CI change**. Tool:
`scripts/cl5/ownership-repair-eval.rkt`; tests: `tests/test-cl5-ownership-repair-eval.rkt`.
Base: `cc58f9e7de1db9b69f92810b497a8730b77be80e` (PR #9994 merge).

## Method

For every confirmed gap at `risk-rank >= 5` from `scripts/cl5/ownership-audit.rkt`, each declared
detector is evaluated against the **existing** admission contract — not a new one:

| Input | Source |
|---|---|
| `@boundary`, `@speed` | `get-file-metadata` (`scripts/run-tests/classify.rkt`) |
| reviewed closure | `closure-of` (`scripts/testing/quick-audit.rkt`) |
| capability hits | `scan-file` over that closure, denylist v1 |
| admission state | `owner-admission-state` (the shipped freshness check) |

Verdicts: `eligible` (unit + fast + clean scan + closure ≤ 12), `needs-review` (integration
boundary, slow tier, any capability hit, or oversized closure), `ineligible` (no usable metadata),
`already-admitted`.

An `eligible` verdict is an admission *prerequisite*, not proof of detection: a reviewer must still
confirm the test exercises the module's decision surface.

## Result: 21 gaps, 34 declared detectors, 2 modules eligible

| Verdict | Modules |
|---|---|
| **eligible** | **2** — `runtime/iteration/retry-policy.rkt` (`tests/test-iteration-retry-policy.rkt`, closure 4, clean), `runtime/memory/policy.rkt` (`tests/memory/policy-boundary-test.rkt`, closure 6, clean) |
| needs-review | **19** |
| ineligible | 0 |

Why the 19 need review:

- **integration boundary** (corpus-level behaviour, not a pure decision test) —
  `extensions/gsd/author-session-recovery.rkt`, `author-execution.rkt`, `evidence-author-journal.rkt`,
  `review-audit-promotion.rkt`, `delivery-recovery.rkt`, `supplemental-evidence-recovery.rkt`,
  `tracker-resume.rkt`, `runtime/credentials/protocol.rkt`, and others;
- **slow tier** — e.g. `tests/test-gsd-supplemental-preserved-integration.rkt`;
- **capability hits in the reviewed closure** — e.g. `sandbox/subprocess.rkt`'s detectors are
  subprocess-owning tests by construction (`tests/test-subprocess-ownership.rkt`, 2 hits), and the
  GSD authority tests pull filesystem/process capabilities (up to 68 hits, closures of 280+ files);
- **oversized closure** — 46–285 files for the GSD author/delivery suites.

Representative promising-but-blocked rows:

| Module | Detector | Blocking reason |
|---|---|---|
| `extensions/gsd-planning/execution-policy.rkt` | `tests/test-gsd-execution-policy.rkt` | unit + fast, closure 47, **1 capability hit** (reviewable disposition) |
| `extensions/gsd/policy.rkt` | `tests/test-gsd-policy.rkt` | unit + fast, closure 109, 8 hits |
| `runtime/context-selection/authority.rkt` | `tests/test-context-selection-authority.rkt` | unit + fast, closure 12, **1 hit** |
| `runtime/memory/policy.rkt` | `tests/memory/policy-boundary-test.rkt` | unit + fast, closure 6, clean → **eligible** |
| `runtime/iteration/retry-policy.rkt` | `tests/test-iteration-retry-policy.rkt` | unit + fast, closure 4, clean → **eligible** |

## What this means for the remediation plan

1. **Option A cannot close the high-risk set.** The pure-unit admission contract repairs 2 of 21
   modules. The remaining 19 declare mostly *integration-level* detectors, which the contract
   deliberately excludes. Bulk admission is not available, and adding those detectors to the table
   would break the "quick = pure decision code" property that makes it fast and reviewable.
2. **Option C is the mechanism for the other 19.** A change to any of them must escalate to the
   comprehensive lane (the guard already does this: `unadmitted-cover`). That is a *safety*
   answer, not a coverage repair.
3. **Option B remains the long-term coverage answer** for those contracts: map each shared contract
   to its consumer/boundary tests and run them, or escalate.
4. Two reviewable capability hits (`execution-policy`, `context-selection/authority`) could be
   admitted with explicit `reviewed-hits` dispositions *if* the reviewer confirms the capability is
   off the exercised path — that is a human decision this evaluation does not make.

## Verification

```
racket -e '(require (file "scripts/cl5/ownership-repair-eval.rkt")) (run-evaluation (current-directory))'
raco test tests/test-cl5-ownership-repair-eval.rkt
```

3 tests: contract truth table (unit/fast/clean → eligible; integration, slow, capability hit,
oversized closure → needs-review; already-admitted reported; missing metadata → ineligible), and a
repository run asserting every high-risk gap and candidate evaluates to a declared verdict.

## Bounds

Eligibility is metadata + static capability scanning; it does not execute tests, does not verify that
a detector detects anything, and does not admit owners. The risk ranking comes from the audit's path
heuristic and is a prioritisation aid, not an audited security classification.
