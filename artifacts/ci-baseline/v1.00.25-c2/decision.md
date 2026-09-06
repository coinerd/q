# Cohort C2 Post-Promotion Decision — v1.00.25 W6

- cohort-id: `v1.00.25-c2`
- decision-version: `w6-decision-v1`
- cohort-mode: `post-promotion` (promoted defaults, required CI lane itself; no shadow duplication)
- cohort-status at decision time: `closed`
- decision rule: the post-promotion activation fast target is evaluated exactly as written;
  **a missed gate records `target unachieved` and names the next lever, never a revised
  target, and a timing miss alone implies no queue rollback** (roadmap v1.00.25 §6, W6).
- decision date: 2026-09-06

## Overall verdict

**`target unachieved`** — the out-of-sample fast execution target on the promoted
defaults was missed. On 20 new eligible PR head SHAs (no overlap with C1), the
required CI lane itself produced 20 successful timing samples; the observed
distribution is p50 = **253.5 s** (target ≤ 115 s) and p95 = **276.0 s**
(target ≤ 135 s). The promoted defaults are therefore not confirmed to meet the
fast execution target out of sample, and the next lever is named below for a
separate reviewed decision.

## Gate text (verbatim, from the roadmap and encoded in the report tooling)

"Out-of-sample fast execution target on promoted defaults: p50 ≤ 115 s and
p95 ≤ 135 s (roadmap v1.00.25 §6, W6). Targets are never revised inside this
wave or milestone."

## Numbers behind the verdict

### Post-promotion timing — promoted defaults on the required lane

| metric | value |
|---|---|
| cohort size (eligible SHAs) | 20 (20 measured, 0 excluded) |
| samples (successful timing) | 20 |
| attempts recorded | 20 |
| failed attempts | 0 |
| cancelled attempts | 0 |
| rerun attempts | 0 |
| timing basis | max wall-clock duration among successful `test (N)` shard jobs of the final successful run per SHA |
| p50 (linear interpolation) | 253.5 s |
| p95 (linear interpolation) | 276.0 s |
| min / max shard-max | 214.0 s / 277.0 s |

Every sample was produced by the required CI lane itself (GitHub Actions run on
the PR head SHA), not by a shadow or duplicated workflow: the evidence linkage is
`pr-number → head SHA → Actions run id → successful `test (N)` shard jobs`,
recorded per attempt in `cohort.json` and checked by the cohort-report tooling's
post-promotion gate.

### Gate verdict (verbatim from `report.json` `post-promotion-gate`)

- `verdict`: `target unachieved`
- `achieved`: `false`
- `p50-seconds`: `253.5` vs `p50-max-seconds`: `115.0` → miss
- `p95-seconds`: `276.0` vs `p95-max-seconds`: `135.0` → miss
- `mode`: `post-promotion`, `samples`: `20`

## Named next lever (verbatim from `report.json` `post-promotion-gate`)

"Next lever (separate reviewed decision; a timing miss alone implies no queue
rollback): reduce the fast-lane critical path by trimming batch shard fan-out
and reusing the prepared environment cache; re-run this cohort on new SHAs
before the next promotion decision."

This lever is recorded for a separate reviewed decision. No queue rollback is
implied by this timing miss alone, and no target in the roadmap is revised here.

## Provenance summary

- 20 new eligible PR head SHAs, first eligible SHA after the W2–W4 activations
  were stable (`5739881dc53a` — run `33867684782`, created 2026-09-04),
  closing at `879d6321a47b` — run `34001252123`, created 2026-09-06);
  SHAs never dropped.
- Measurement window: 2026-09-04 … 2026-09-06, entirely after the W2–W4
  activation range (hash-joining shards and fast-lane queue promotion, unified
  static-fast and corpus-cached-rules plans, planned-light path activation) on
  the promoted defaults.
- Reliability ingestion: `report.json` `reliability` records
  `total-attempts: 20, successes: 20, failures: 0, cancelled: 0, reruns: 0`.
  A further 8 ineligible SHAs observed during selection are recorded in
  `cohort.json` `exclusions` with named reasons (e.g. `lane-run-failed` with
  the failing run link); failed, cancelled, and rerun attempts are never
  dropped from the record.
- Artifacts: `cohort.json`, `report.json`, `report.md`, `decision.md`,
  `SHA256SUMS` (checksums bound in `SHA256SUMS`; wave evidence bound to the
  non-artifact content digest in
  `docs/reports/gsd-wave-evidence/v1.00.25-w6.rktd`).

## Reviewer

- Reviewer: W6 delivery verification lane (automated pre-commit checks plus the
  W6 reviewer role recorded in `docs/reports/gsd-wave-reviews/v1.00.25-w6.rktd`).
- Review outcome: numbers in this record are reproduced directly from
  `report.json`'s `post-promotion-gate` section (`w6-decision-v1`); no target
  was revised, no queue was rolled back, and the verdict `target unachieved`
  with the named next lever is recorded as the truthful W6 outcome.
