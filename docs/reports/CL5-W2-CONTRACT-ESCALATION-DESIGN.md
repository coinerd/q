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
| **`no-consumer-detector`** | a declared consumer has no `@covers` detector | an untested consumer cannot be counted as covered |
| **`graph-unavailable`** | the contract or one of its consumers cannot be parsed | dependency evidence is unknown, so completeness cannot be claimed |
| **`no-required-tests`** | the change yields an empty required set | zero declared tests is unknown coverage, not coverage |
| **`duplicate-contract`** | the registry declares a module twice | last-wins silently shrinks the required set |
| **`invalid-contract`** | unknown `kind` / malformed entry | a typo must not read as a valid contract |
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

Results (after the completion and bypass corrections):

- **D5** computes **23 required tests** (owner test + the detectors of its eight graph-known direct
  dependents) but now **escalates** with **`no-consumer-detector`**: `extensions/gsd/projection-kernel.rkt`
  is a real consumer of the vocabulary and has **no** `@covers` detector. That is a new finding from
  the fail-closed rule, not a regression — the contract is not claimed as covered while a consumer is
  undetected.
- `util/error/errors.rkt` → **`escalate`**: `partial-consumers` (44 audit dependents, two declared)
  plus two `no-consumer-detector` rows for the two declared consumers with no detector.
- `runtime/working-set.rkt` (12 dependents, no declaration) → `unmapped-contract` + `no-required-tests`.
- A missing consumer detector, a missing/corrupt manifest, a dead consumer, an empty declaration, an
  omitted dependent, an unparseable graph node and an empty required set each have a negative test.

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

## Decision → selection (why escalation cannot shrink the set)

`contract-selection` is the only bridge from a decision to a test instruction, and it is pure:

| Verdict | Instruction | Tests |
|---|---|---|
| `covered` | `run-required` | exactly the computed required set |
| `escalate` | `run-comprehensive` | the comprehensive set (strictly more) |

The invariant is tested directly: every escalation code yields `run-comprehensive` with the
comprehensive set, never a subset of the required set. This is the property that must survive
integration with the production selector — an unknown contract has to mean *more* tests, not fewer.

## Limitations (explicit)

- A contract set proves *structural* completeness at best — consumers and tests accounted for — and
  never *behavioural* completeness: the required tests may still not detect a given mutation. A
  complete list of inadequate tests is still inadequate protection.
- The registry is a hand-maintained surface. The validator now catches dead consumers, omitted
  graph-known dependents, missing tests anywhere in the required set, and invalid attribution
  metadata; it still cannot prove that a *complete* declared set is behaviourally right.
- Fan-in is computed from static `require` edges; dynamic requires are invisible (the graph
  parse-failure signal from the guard covers the unparseable case, not dynamic edges).
- No execution, no timing, no cost claims.

## Non-goals

No production selector, CI, admission, scheduler or corpus change. W3 and the 40 % performance claim
remain on hold.
