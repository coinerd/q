# SERIES-COMPLETION — v1.00.28 → v1.00.29 (Test Design/CI series closure)

Status: **CLOSED** (2026-09-13, v1.00.29 released)
Scope: the v1.00.28 "Test Workload Reduction" milestone (#893, verdict PARTIAL
WORKLOAD REDUCTION; FINAL TARGET NOT ACHIEVED) and the v1.00.29 "Proof-Graph
Reduction" campaign (milestone #894, verdict PARTIAL — SAFE REDUCTION DELIVERED).

## Verdict accounting

| Milestone | Released | Final verdict | Decision record |
|---|---|---|---|
| v1.00.28 | 2026-09-09 | PARTIAL WORKLOAD REDUCTION; FINAL TARGET NOT ACHIEVED | `artifacts/ci-baseline/v1.00.28-final/decision.md` |
| v1.00.29 | 2026-09-13 | PARTIAL — SAFE REDUCTION DELIVERED | `artifacts/ci-baseline/v1.00.29-final/decision.md` |

Both milestones chose the honest PARTIAL verdict over target revision: every
safety/integrity gate holds, real measured reductions were delivered, and the
missed performance goals are recorded with attribution — not silently widened.

## What the series delivered

1. **Measurement before action**: per-file test census with drift guard
   (v1.00.28 W0), proof-graph census with a metric contract and per-pair
   duplicate classification (v1.00.29 W0), duration-aware shard-plan tooling.
2. **Work cut where provably safe**: fixture amplification removed, wait audit
   banked, prepared-env verified restore with identity manifests across six ci
   consumers (restore wall ~0.5 s vs multi-minute install+compile), one
   exact-duplicate proof removed through fail-closed same-SHA reuse
   (dup-01: nightly fast-suite consumes the ci bundle).
3. **Integrity raised, honestly priced**: the W4 purge+identity expansion
   RAISED PR CI wall by ~1623 s mean (+171.4 %) — recorded as a known,
   attributed regression against the performance goals, accepted in exchange
   for the elimination of the stale-bytecode class (BUG-0065 systemic).
4. **Governance proven by exercise**: the selector amendment was proposed,
   never self-approved; the W8 pilot was CLOSED-SKIPPED with the gate state
   recorded verbatim; the dup-04 W0 misclassification was caught by the
   campaign's own review gates and reclassified distinct_environment with both
   instances preserved (the environment-compatibility protection held).
5. **Fail-closed reuse machinery**: q.proof-bundle/1 (canonical writer,
   15-step fail-closed validator, consumer gate with exit codes 0/3/4,
   corruption canaries force fallback — never a green skip). Its first
   production run refused a non-clean aggregate and exposed a masked CI
   failure (BUG-0073) — the machinery works.

## What the series did NOT achieve

- Required PR CI p50/p95 goals (≤ 360 s / ≤ 480 s): post-W4 measured
  2558.0 s / 3023.0 s — not achieved, attribution recorded.
- Main/release staged north-stars: evidence-pending at W10 close; the
  v1.00.29 release run provides the first post-W9 main/release production
  observations for the next series.
- Fleet-wide flake tax and verified-restore window ratio: unknown at release
  time (single-run evidence only; window measurement left to the coordinator
  post-merge), never coerced to zero.

## Handoff to the next series

- Performance levers named by the v1.00.29 decision remain available and
  unchanged: shard fan-out tuning, tail-shard rebalancing, moving slow
  required gates off the mergeable critical path, prepared-env cache reuse
  beyond the six activated consumers.
- The selector amendment (W7 §8) remains PROPOSED; a future campaign may run
  the W8-equivalent pilot only after independent review and merge.
- BUG-0073 (CI tee-masking, pipefail sweep) is filed and open — the first
  candidate for the next integrity pass.
