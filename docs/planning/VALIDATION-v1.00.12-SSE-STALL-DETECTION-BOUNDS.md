# VALIDATION v1.00.12 — SSE Stall Detection Bounds

**Milestone:** v1.00.12 · **Released:** 2026-08-22 · **Tag:** `v1.00.12`

## Verify Mapping

| Claim | Evidence | Command / Source |
|-------|----------|------------------|
| Resolver semantics locked | 13 tests PASS | `raco test tests/test-sse-phase-timeout-bounds.rkt` — `(phase 900 600)=(120 300 60)`, `(phase 900 #f)=(120 120 60)`, `(phase 900 90)=(120 90 60)`, `(phase 100 600)=(100 100 60)` + sweep invariants |
| Message suffix on all three raise sites | SS-5 regex tests PASS (initial hold, content stall) | same file; regex `#rx"\\[phase=(initial|thinking|content) data-received=(yes|no) chars=[0-9]+\\]$"` |
| Old-message fixtures unaffected | focused suites green | test-gsd-d8-provider-retry-scaling, test-streaming-text-preservation, test-stream, test-openai-compatible, test-midstream-stall, … all PASS |
| Local gates @1.00.12 | `.planning/v1.00.12-w3/gates/` | fast 1110 PASS · tui 88 PASS · arch 30 PASS · workflows 29 PASS · security 64 PASS · lint-all 22/0 · lint-release-readiness 5/5 |
| CI on release PRs | GitHub checks | #9449 CLEAN; #9450/#9451 CLEAN |
| Release published | `gh release view v1.00.12` | published 2026-08-22T15:19:06Z, assets q-1.00.12.tar.gz + release-manifest.json |
| Version consistency | lint-version / readiness | version sync 1.00.12 across util/version.rkt, info.rkt, README badge, CHANGELOG |

## Scope Notes

- SS-6 (adapter parity) intentionally deferred to v1.00.13 Request Lifecycle
  Policy Unification; handoff documented in `docs/provider-retry.md`
  ("Scope and handoff") and revised plan §5.1.
- No local full regression per TEST-GATE-POLICY (CI covers PR shards;
  release workflow ran the full bundle verification).

## Regression Watch

- Models with legitimate >300 s silent reasoning will now cap at 300 s
  (documented in CHANGELOG Operational/Release).
