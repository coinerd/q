# Provider Hardening Reassessment — v0.99.91 W4-B

**Issue:** #9241 · **Milestone:** #878 · **Approved path:** B (immutable)

**Baseline:** v0.99.90 `b006ff08`

**W4 implementation evidence:** `e390433d` · **pre-review release candidate:** `1aaf2da6`

**Machine decision:** `docs/architecture/provider-hardening-terminal-v0.99.91.rktd`

**Status:** RELEASED — merge `a4b85569`, annotated tag `v0.99.91`, workflow `31520743425`

## 1. Decision summary

**MA-09 is CLOSED by Path B, conditional only on the W4-B release gates.** The
shared-production-abstraction proposal is **rejected for v0.99.91**. The
campaign adds no provider base, shared request/event parser, protocol template
flags/methods, or artificial equality. Reopening requires a separate dated
amendment with two new co-change proofs for semantically identical primitives.

This is not a ban on neutral helpers. Existing C1–C8 helpers remain shared and
W3-B permits future neutral code that contains no provider protocol marker.

## 2. Campaign evidence

| Wave | Evidence | Result |
|---|---|---|
| W0 | 10 scenarios × 4 real parsers | 40 explicit supported/unsupported cells |
| W1-B | typed unsupported records + exact provider-specific cases | Anthropic reasoning produces no chunk; Gemini thought text remains plain text |
| W2-B | v1 differential corpus | 4 providers × 5 kinds = 20 versioned, digested, redacted byte/JSON fixtures |
| W3-B | ownership/locality policy and mutation probes | adapter marker ownership, generic transport neutrality, frozen C1–C8 registry |
| W4-B | terminal ledger + targeted closure probes | exact C1–C23/G1–G3 bijection; G1/G2/G3, C11/C13/C18/C22 pinned |

The W0 matrix means “the real parser normalizes this scenario”; it is not a
claim that every provider API supports every feature. Unsupported behavior and
provider-specific usage/tool/timeout/error asymmetries remain explicit.

## 3. C1–C23 terminal disposition

The machine ledger contains exactly one entry per ID and executable evidence
for each entry. Summary:

- **C1–C8 — SHARED_PRIMITIVE:** already-neutral HTTP, SSE framing, port
  finalization, error/status classification, stop reasons, tool-delta
  accumulation, and timeout primitives. W3-B freezes their approved ownership.
- **C9–C15 — RETAINED_LOCAL:** no new qualifying evidence supports extraction.
  C9 remains blocked by G1; C10–C15 lack two repeated, semantically identical
  primitive-level co-change proofs. C11 Azure URL parsing and C13 path joins are
  explicitly pinned rather than silently abstracted.
- **C16–C21 — PROVIDER_PROTOCOL:** request bodies, event parsers, auth headers,
  OpenAI tool schema repair, Gemini generated tool IDs, and provider-specific
  content conversion encode wire-protocol identity.
- **C22–C23 — INTENTIONAL_DUPLICATION:** Kimi eager streaming closes the response
  before generator consumption; OpenAI-family null coercion defends endpoint
  quirks. Generalizing either would weaken truthful contracts.

## 4. Corrected G1–G3 source of truth

| Gap | Terminal contract |
|---|---|
| G1 status pre-check | Anthropic/Gemini reject `>=400`; OpenAI-compatible rejects `>=300`; Azure rejects every status except `200`. This semantic divergence blocks C9 sharing. |
| G2 per-model timeout | **Correction to the v0.99.87 prose:** OpenAI-compatible uses `effective-request-timeout-for` in both non-streaming and streaming calls. Anthropic, Gemini, and Azure use global defaults. |
| G3 stream error wrapping | OpenAI-compatible wraps ordinary stream-phase failures as network provider errors while preserving provider/timeouts; the other adapters clean up and rethrow their existing failures. |

These are documented asymmetries, not capability regressions. W4-B tests pin
the exact thresholds, two OpenAI timeout call sites, and ownership of stream
error wrapping.

## 5. Capability and ownership closure

C18 now has an explicit four-protocol authentication pin:

- Anthropic: `x-api-key` plus `anthropic-version`
- Gemini: `x-goog-api-key`
- OpenAI-compatible: `Authorization: Bearer`
- Azure OpenAI: `api-key` (with API version in the endpoint)

C22 is pinned to the Anthropic-compatible Kimi path and the isolated generic
`eager-stream` adapter. W3-B remains a high-precision syntactic fitness gate,
not data-flow or security analysis; computed marker indirection is a documented
scope boundary. W2-B fixture containment, digest, schema, symlink and secret
checks remain the security evidence.

## 6. Defect localization and change amplification

Canonical method:

```bash
racket scripts/architecture-baseline.rkt --revision 1aaf2da6 --last 200 \
  --raw /tmp/v09991-release-candidate.rktd \
  --markdown /tmp/v09991-release-candidate.md
```

Release-only commits and exact moves are excluded. The explicit revision makes
this snapshot reproducible after later review and merge commits move `HEAD`.

| Module | LOC | changes (last 200) | hotspot |
|---|---:|---:|---:|
| `llm/stream.rkt` | 537 | 10 | 5370 |
| `llm/openai-compatible.rkt` | 513 | 9 | 4617 |
| `llm/gemini.rkt` | 537 | 2 | 1074 |
| `llm/azure-openai.rkt` | 162 | 4 | 648 |
| `llm/anthropic/format.rkt` | 431 | 1 | 431 |

Provider-scope co-change remains led by
`openai-compatible.rkt` ↔ `stream.rkt` at **5**. No second provider primitive
meets the roadmap’s repeated-co-change criterion. The sliding 200-commit window
therefore does not justify Path A.

From v0.99.90 through pre-review release candidate `1aaf2da6`, the campaign
changed **58 files, +2575/−55**, with **zero production `llm/` changes**. The
larger release-candidate count includes synchronized version documentation and
release notes; the W4 implementation itself first appears at `e390433d`.
Changes are contracts, fixtures, tests, policy, reports, metrics, and release
surfaces. This demonstrates low release
amplification for hardening but does not claim empirical post-W3 production
locality: no provider production defect was introduced to manufacture such
evidence. Synthetic W3 mutation probes establish the guard behavior instead.

## 7. No-regression proof and limitations

At W4 implementation commit `e390433d`, the cumulative provider suite (golden
matrix, adapter completion, differential fixtures, parity, locality, terminal
decision) is **41/41**. The release also requires Provider Smoke, Broad, Arch, Security,
release-smoke, release readiness, independent review, CI, and public artifact
verification.

Limitations are explicit:

1. Provider Smoke is compilation/export smoke, not a credentialed live API run.
2. W2 timeout recipes validate deterministic shared machinery and recorded
   provider contracts; they do not force every live endpoint failure mode.
3. W3 is syntactic and high precision, not whole-program data-flow analysis.
4. History metrics are evidence at the pinned SHA, not evergreen test thresholds.

## 8. Terminal MA-09 wording

> **MA-09 — CLOSED (Path B; shared production abstraction rejected for
> v0.99.91).** Provider contracts are explicit across supported and unsupported
> scenarios, differential fixtures are versioned and redacted, and protocol
> ownership is guarded by positive and negative locality probes. Reassessment
> found no new qualifying abstraction evidence: C9 remains the sole plausible
> shared candidate and remains G1-divergent; C10–C15 lack repeated
> primitive-level co-change; C16–C21 are provider protocol and C22–C23 are
> intentional duplication. G1–G3 remain documented, tested asymmetries rather
> than capability regressions. Reopening requires a separate dated amendment
> with two new semantically identical primitive co-change proofs.

## 9. Release gate record

Gate evidence is pinned to the candidate on which each command ran:

| Gate | Evidence SHA | Result |
|---|---|---|
| Provider cumulative + smoke (post-remediation) | `7c6b723f` | 48/48 PASS |
| Broad | `e390433d` | 1252/1260 files, 17882 tests PASS; 8 explicit local-profile skips |
| Arch (post-remediation) | `7c6b723f` | 24 files / 254 tests PASS |
| Security | `e390433d` | 64 files / 710 tests PASS |
| release-smoke (post-remediation) | `7c6b723f` | 15 files / 180 tests PASS |
| Fast (post-remediation) | `7c6b723f` | 1074 files / 15615 tests PASS |
| lint-all / lint-format / contract changes | `1aaf2da6` | 23 PASS + 1 non-blocking pre-release warning / PASS / PASS |
| pre-release truth / dry-run | `1aaf2da6` | 4/4 PASS / 5/5 PASS on synchronized release surfaces |
| strict readiness | `a4b85569` exact main | 7/7 PASS; Fast repeat-3 PASS |
| independent review | `b4aee7f6` | APPROVED — 0 MAJOR / 0 MINOR |
| PR CI / required policy | PR #9271 | 17/17 PASS / NONE unmet |
| annotated tag / public bundle | `a4b85569`, run `31520743425` | PASS; public `q-0.99.91.tar.gz` + manifest verified, tag object `f7c59e31` |
