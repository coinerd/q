# Fast-tier architecture review — v1.00.28 W3

Wave: `v1.00.28-w3` · Branch: `campaign/v1.00.28-w3` · Base: `origin/main`
Artifacts: `artifacts/tier-ownership/v1.00.28-w3/` (checksummed in `SHA256SUMS`)

## Scope and hard rule

Every test carrying `@speed fast` **and** `@boundary integration` was enumerated from the
W0 census, ranked by W0 work-mass contribution (non-test LOC attributable to the test's
run), and classified per row. Hard rule honored: **retiering is not deletion** — a
behavior may only leave `fast` if a named destination gate and its owner sign off in the
same commit.

## Inventory result

- **254 `fast`+`integration` rows** classified (see `fast-integration-review.json`,
  `rows = 254`, `rows_selected = 254`).
- Owner-area distribution: core 176, runtime 41, extensions 29, workflows 3,
  test-design 1, agent-session 1, gsd-delivery 1, test-runtime 1.
- Per-row classification fields: business behavior, real boundary, fake adapter,
  minimal real contract, destination lane/owning required gates.
- Top work-mass contributor: `tests/test-gsd-go-orchestrator.rkt` (rank 1,
  1172 work-mass LOC), classified `retained — boundary-real by contract; W1
  instrumented evidence pending`.

## Candidate-family review (§6 splits — all landed IN PLACE)

The four named families were reviewed and split along the unit/integration seam
**without** changing `@boundary`/`@speed` metadata: business behavior now runs on
in-memory/injectable layers under `unit-fast`, while the minimal real-boundary contract
cases stay in the same file under the required `fast` gate.

| Family | Split | unit-fast side | required real contract (gate `fast`) |
|---|---|---|---|
| `tests/test-cwd-independence.rkt` | split-in-place | cheap CWD/unit assertions on deterministic layers | one real executable/CWD probe canary |
| `tests/test-agent-session-basic.rkt` | split-in-place | lifecycle behavior against in-memory session storage | filesystem persistence: create/resume/recreate on a real store root |
| `tests/test-golden-flows.rkt` | split-in-place | golden behavior against deterministic layers (shared session-store root, one per run) | one small real golden path |
| `tests/test-gsd-delivery-verifier.rkt` | split-in-place | verifier decision logic on synthetic Git facts via injectable adapter | minimum real-Git contract set |

All four keep a single real session-store/Git root per run, removed once per run at
module tail, so the real-boundary contract stays real but amortized.

## Why no cross-tier retier landed in W3

The full 254-row pass found every row's real boundary is the one declared by its
`@boundary integration` marker and no cheaper faithful adapter is identified yet; the
candidate families' cost was already reduced in place by the §6 splits above. Any
cross-tier move would need destination-gate signoff without W1's instrumented
per-test cost evidence, so all 254 rows are classified **retained** with destination
`unchanged — remains owned by its current required gates`. This is a review verdict,
not a deferral-by-omission: each row names its business behavior, real boundary,
minimal real contract, and owning lane.

## Reconciliation

`inventory-reconciliation.json`: per behavior ID `before == after` exactly
(**0 moves, 0 drops, 4 in-place reviews**). No silent drops. The ownership matrix
(`tests/tier-ownership-matrix.json`, 972 rows) was regenerated after the review and the
drift check is green — no metadata changed, so no stale rows.

## Focused results at review time

- `racket tests/test-cwd-independence.rkt` → exit 0
- `racket tests/test-agent-session-basic.rkt` → 19 success(es) 0 failure(s) 0 error(s), exit 0
- `racket tests/test-golden-flows.rkt` → exit 0
- `racket tests/test-gsd-delivery-verifier.rkt` → 24 success(es) 0 failure(s) 0 error(s), exit 0
- `racket tests/test-milestone-gate.rkt` → exit 0 (includes inventory-reconciliation red cases:
  moved-without-destination and moved-without-named-gate fail closed)

Evidence trio bound to this report:
`docs/reports/gsd-wave-{evidence,reviews,validation}/v1.00.28-w3.rktd`.
