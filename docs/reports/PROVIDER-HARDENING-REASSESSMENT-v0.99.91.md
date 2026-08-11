# Provider Hardening Reassessment — v0.99.91 W4-B

**Issue:** #9241 · **Milestone:** #878 · **Approved path:** B (immutable)

**Baseline:** v0.99.90 `b006ff08` · **W4 candidate:** `e2e65ada`

**Machine decision:** `docs/architecture/provider-hardening-terminal-v0.99.91.rktd`

**Status:** RELEASE CANDIDATE — publication gates pending

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

Canonical method: `scripts/architecture-baseline.rkt --revision HEAD --last 200`;
release-only commits and exact moves excluded.

| Module | LOC | changes (last 200) | hotspot |
|---|---:|---:|---:|
| `llm/stream.rkt` | 537 | 10 | 5370 |
| `llm/openai-compatible.rkt` | 513 | 10 | 5130 |
| `llm/gemini.rkt` | 537 | 2 | 1074 |
| `llm/azure-openai.rkt` | 162 | 4 | 648 |
| `llm/anthropic/format.rkt` | 431 | 1 | 431 |

Provider-scope co-change remains led by
`openai-compatible.rkt` ↔ `stream.rkt` at **5**. No second provider primitive
meets the roadmap’s repeated-co-change criterion. The sliding 200-commit window
therefore does not justify Path A.

From v0.99.90 through the W4 candidate, the campaign changed **33 files,
+2217/−21**, with **zero production `llm/` changes**. Changes are contracts,
fixtures, tests, policy, reports, and metrics. This demonstrates low release
amplification for hardening but does not claim empirical post-W3 production
locality: no provider production defect was introduced to manufacture such
evidence. Synthetic W3 mutation probes establish the guard behavior instead.

## 7. No-regression proof and limitations

At the W4 candidate, the cumulative provider suite (golden matrix, adapter
completion, differential fixtures, parity, locality, terminal decision) is
**41/41**. The release also requires Provider Smoke, Broad, Arch, Security,
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

To be finalized on the reviewed release candidate:

| Gate | Result |
|---|---|
| Provider cumulative focused | 41/41 PASS |
| Provider Smoke | PENDING |
| Broad | PENDING |
| Arch | PENDING |
| Security | PENDING |
| release-smoke / dry-run / readiness | PENDING |
| independent review | PENDING |
| PR CI / required policy | PENDING |
| annotated tag / public bundle | PENDING |
