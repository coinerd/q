# CL5 W2 — contract-aware escalation design (#9997)

Design + isolated prototype. **Nothing is wired into `./qt`, CI, admission or scheduling; no test is
executed.** Tool: `scripts/cl5/contract-escalation.rkt`; tests:
`tests/test-cl5-contract-escalation.rkt`. Base: `dc54219f654cb43efc476707cfca2e62fe1e9bcf`.

## Why file-level ownership is not enough

The advisory guard (Option C) answers "can the quick lane honestly claim a green?" and escalates when
it cannot. That is a safety net, not coverage. The audit shows the structural reason a net is needed:
the `@covers` manifest is **per-test**, so it cannot express "this shared contract is consumed by
these modules, whose behaviour must be re-verified when the contract changes".

Two concrete symptoms at `dc54219f`:

- D5 (`extensions/gsd/wave-status.rkt`, a vocabulary contract) selected three files, none of them a
  detector for the changed module or for its consumers (W2 measurement: `missed`).
- The audit lists **17 potential gaps**: modules that *do* have an audited detector, ≥ 3 production
  dependents, and no consumer escalation.

The corpus documents the expected behaviour for D5 directly: "consumer-contract escalation
(vocabulary fan-in)". The selector has no such rule (evidence: `changed-selection.rkt` escalation
codes are runner-helper/config/fixture/generated/graph-parse/unmapped-source/source-scan).

## Model: a contract is data, not code

```
module     repo-relative shared contract module
kind       vocabulary | policy | protocol | shared-state
consumers  modules whose behaviour depends on the contract
tests      owner/boundary tests that must run for a contract change
```

Semantics: a change to `module` requires `tests ∪ declared-detectors(consumers)`. The prototype
**computes** that set and reports it; executing it belongs to a lane decision that this issue does not
make.

## Fail-closed rules

| Code | When | Why |
|---|---|---|
| `unmapped-contract` | a changed module has ≥ `fan-in-threshold` dependents and no declaration | a shared contract without a consumer set is exactly the D5 blind spot |
| **`partial-consumers`** | the declaration omits a **graph-known direct dependent** | an omitted consumer is untested surface; the set must escalate, never claim `covered` |
| `stale-consumer` | a declared consumer does **not** require the module (checked against the forward requires graph) | declarations rot; a dead consumer silently shrinks the set |
| **`unknown-test`** | **any** test in the computed required set is absent — contract tests **and** consumer-inherited detectors | a missing consumer detector must not read as covered |
| `no-coverage` | a declaration yields no contract test and no consumer detector | an empty declaration is not coverage |
| **`attribution-unavailable`** | the coverage manifest is missing or corrupt | the declared-detector half of the set cannot be trusted |

All four are validated mechanically from the repository's own graph and file system; none requires
executing tests. The fan-in threshold (default 3) is a design parameter derived from the audit's
fan-in distribution, **not** a policy.

## Repository behaviour at `dc54219f`

Seeded registry (two entries, both evidence-backed):

| Contract | Kind | Consumers (from the impact graph) | Contract tests |
|---|---|---|---|
| `extensions/gsd/wave-status.rkt` | vocabulary | archive, campaign-state, go-orchestrator, projection-kernel, wave-completion | `tests/test-gsd-wave-status.rkt` |
| `util/error/errors.rkt` | shared-state | `agent/loop.rkt`, `runtime/agent-session.rkt` | `tests/test-browser-errors.rkt` |

Results (after the completion corrections):

- **D5** → `covered`, **23 required tests**: the contract test plus the detectors of all **eight**
  graph-known direct dependents (`archive`, `campaign-state`, `command-handlers`, `go-orchestrator`,
  `planning-artifact-validation`, `projection-kernel`, `wave-completion`, `wave-docs`). The seed now
  declares every dependent; omitting one raises `partial-consumers`.
- `util/error/errors.rkt` → **`escalate` / `partial-consumers`**: the audit shows 44 direct
  dependents and the seed names two, so the prototype explicitly refuses a `covered` claim instead of
  quietly shrinking the set. This is the honest state until that contract is fully reviewed.
- A high-fan-in module with no declaration → `unmapped-contract`, naming the fan-in.
- A missing consumer detector, a missing/corrupt manifest, a dead consumer, an empty declaration and
  an omitted dependent each have a negative test.

Note the deliberate difference from the guard: B says "these tests must run" (the comprehensive lane
does run them); C says "the quick lane may not claim a green". B is coverage repair, C is the safety
contract. They are complementary, not alternatives.

## Design comparison (as required by the issue)

| Option | Correctness for shared contracts | Cost | Maintainability | Verdict |
|---|---|---|---|---|
| A — ownership repair | only pure-unit detectors; 2 of 21 high-risk gaps | cheap per gap | table grows, drift silent | bounded optimisation |
| B — contract escalation (this design) | directly targets fan-in semantics | one lane run per contract change | needs graph validation (built here) | **the coverage answer** |
| C — fail-closed guard | converts silence into escalation | escalates uncovered changes | one rule, self-validating | **the safety contract** |

Recommended composition: C always; B for the contracts where the consumer set is known and validated;
A only where a pure-unit detector exists.

## Seeding plan (bounded, separate work)

1. Seal the two seed entries above with a review of each consumer edge (they are graph-verified, not
   guessed).
2. Add the 17 potential-gap modules one at a time, each with graph-validated consumers; `stale-consumer`
   fires automatically when a consumer stops requiring the contract.
3. Only then consider feeding `required` into a lane — a separate issue, not this prototype.

## Limitations (explicit)

- A contract set proves *attribution*, not detection: the required tests may still not detect a given
  mutation.
- The registry is a hand-maintained surface. The validator now catches dead consumers, omitted
  graph-known dependents, missing tests anywhere in the required set, and invalid attribution
  metadata; it still cannot prove that a *complete* declared set is behaviourally right.
- Fan-in is computed from static `require` edges; dynamic requires are invisible (the graph
  parse-failure signal from the guard covers the unparseable case, not dynamic edges).
- No execution, no timing, no cost claims.

## Non-goals

No production selector, CI, admission, scheduler or corpus change. W3 and the 40 % performance claim
remain on hold.
