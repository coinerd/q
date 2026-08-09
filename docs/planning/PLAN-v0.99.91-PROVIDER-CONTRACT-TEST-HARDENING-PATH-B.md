# Plan: v0.99.91 — Provider Contract & Test Hardening (PATH B)

**Status:** FROZEN — NOT STARTED
**Approved path:** **PATH_B** (immutable before Plan-ID/hash)
**Decision evidence:** W3 matrix C1–C23/G1–G3: fewer than two semantically identical primitives have repeated co-change; C9 alone has one notable pair and is G1-blocked
**Plan-ID / Hash:** generated at campaign start from this Path-B-only manifest
**Dependency:** v0.99.90 released
**GitHub:** milestone #878; waves #9237–#9241
**Finding:** MA-09

## Goal

Complete provider-specific contracts and differential evidence without inventing a shared production abstraction or hiding unsupported capabilities.

## Immutable wave map

| Wave | Title | Scope | Required gate / acceptance |
|---|---|---|---|
| W0 | Provider Contract Golden Matrix | Equivalent scenarios across Anthropic/Gemini/OpenAI/Azure; supported/unsupported explicit | provider tests + Fast; complete normalized contract matrix |
| W1-B | Adapter Contract Completion | Fill provider-specific cases; no shared production abstraction | provider contracts + Fast; every W0 scenario explicit per provider |
| W2-B | Differential Stream/Error Fixtures | Versioned redacted bytes/JSON for framing/tools/usage/malformed/timeouts | Broad + Security; complete deterministic fixture matrix |
| W3-B | Provider Change-Locality Guards | Preserve protocol parser ownership and transport neutrality | positive/negative probes + Arch + Fast |
| W4-B | Hardening Reassessment and Release | Reassess coverage/localization/amplification; terminal no-abstraction decision | Broad + Arch + Security + Provider Smoke + Release + review |

**Forbidden:** Path-A W1–W4, `provider-base.rkt`, protocol flags/template methods, or shared request/event parsers. Reopening requires a later separate amendment with two new qualifying co-change proofs; it cannot mutate this campaign.
