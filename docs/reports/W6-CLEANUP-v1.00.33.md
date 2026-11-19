# W6 Cleanup (v1.00.33)

Milestone #900 wave W6 (#9954): deletion-type cleanup of the Phase F final-sweep
candidates from `docs/reports/TEST-RETIER-FINAL-SWEEP-v1.00.32.md`, applied
under the deletion-evidence tiers of review amendment 7002476681. One stale doc
passage corrected; three orphan helpers deleted after the orphan tier was fully
re-proven on current main (`524c93a4`). No manufactured quota: one candidate
class (doc) was fixed, one class (orphans) was deleted, zero behavior-owning
tests or live fixtures were touched, and every number below is a recorded run.

## Candidate table

| Candidate | Tier claimed | Tier met? | Decision |
|---|---|---|---|
| `docs/automatic-review.md` stale passage (ex-Python controller claim) | stale doc | yes — verified against current code | corrected in place (minimal, adjacent claims included) |
| `tests/helpers/contract-blame.rkt` (42 LOC) | orphan helper | yes | **deleted** |
| `tests/helpers/test-events.rkt` (75 LOC) | orphan helper | yes | **deleted** |
| `tests/helpers/ws-test-helpers.rkt` (54 LOC) | orphan helper (superseded W0 scaffold) | yes | **deleted** |

## Stale doc verification (`docs/automatic-review.md`)

Verified against current code before editing:

- `git show 707bf3bf --stat` (#9913): Python control plane eliminated
  pre-milestone; current base `524c93a4` descends from it.
- `extensions/gsd/review-dispatch.rkt` lines 49/195/202/288: the controller is
  native `auto-review-seam-run!`; `default-review-python` survives only as the
  exported port contract name ("The name is kept because it is the exported
  port contract the coordinator and the dispatch tests inject/override" —
  comment in the same file).
- `tests/test-gsd-auto-review-production-contract.rkt` header (fast, @suite
  extensions): "B2 fast contract owner for the default production coordinator…
  THIS file is its required-PR counterpart… the REAL native review controller
  (default-review-python -> auto-review-seam-run!)".
- `tests/test-gsd-auto-review-production.rkt` header (slow): the deep
  two-generation owned-Verify owner in full regression.
- `tests/test-gsd-auto-review-loop.rkt` (slow) is still the actual isolated
  `/go` loop; `tests/test-gsd-auto-review-loop-contract.rkt` (fast) is its B1
  required-PR counterpart — both headers read and quoted in the correction.
- `tests/test-gsd-delivery-auto-review-native.rkt` header confirms the kept
  claim: strict-controller ownership + "runs in the slow/all suites rather than
  the PR fast gate" after Phase B deleted the Python oracle and its shim.

Edit scope: replaced the two false statements (focused-proof subject + test
file attribution; missing B1/B2 counterparts), kept the honest slow/all-suite
controller-coverage statements and the historical Phase-B sentence verbatim.
Only other `Python` mention in the file is that historical sentence (checked
via `rg -n "Python|python" docs/automatic-review.md`).

## Deletion evidence per type (orphan helper tier)

Tier: "no remaining consumers AND no required standalone/debug/build purpose".

### 1. `tests/helpers/contract-blame.rkt` — provides `check-contract-blame`, `check-contract-accepts`

- Current-tree consumer grep:
  `rg -n "require.*contract-blame" --glob '*.rkt' .` → **0 hits**;
  identifier grep `check-contract-blame|check-contract-accepts` outside the
  helper itself → only `CHANGELOG-ARCHIVE.md`, historical
  `docs/reports/*`, `docs/reports/architecture-baseline-v0.99.87.rktd`,
  `tests/tier-ownership-matrix.json` (regenerated below), and immutable
  `artifacts/ci-recovery/.../raw/*.json` PR-history blobs. No code consumer.
- Full-history consumer search: for every commit ever matching
  `git log --all -S 'helpers/contract-blame'` (17 commits), `git grep -l
  'helpers/contract-blame' <sha> -- '*.rkt'` → **only the helper itself, in
  every commit**. It never had a single `require` consumer since creation
  (v0.55.0 W0, `9843eb26`).
- Standalone/debug/build purpose: test-runner suite selection walks
  `tests/**/test-*.rkt` (`scripts/run-tests/inventory.rkt`), so the helper is
  never executed; `rg` over `scripts/`, `qt`, `bin` → no reference; no
  `tests/.coverage-manifest.json` row. Purpose stated at creation ("blame test
  helpers" for the contract-metrics tooling wave) never materialized into a
  consumer.

### 2. `tests/helpers/test-events.rkt` — provides `make-test-event` + 7 typed event builders

- Current-tree grep:
  `rg -n "require.*helpers/test-events|require.*test-events" --glob '*.rkt' .`
  → **0 hits**. The ~20 test files matching `make-test-event` each define
  their own local builder (verified by reading the `define (make-test-event …)`
  sites, e.g. `tests/test-tui-exploration-events.rkt:17`,
  `tests/test-blackboard-subscriber.rkt:29`,
  `tests/test-transcript-on2.rkt:24`) — name collisions, not consumers.
- Full-history search: for every commit ever matching
  `git log --all -S 'helpers/test-events'` (17 commits), `git grep -l
  'helpers/test-events' <sha> -- '*.rkt'` excluding the helper → **0 hits in
  every commit**. Created in v0.47.0-W1 F6 (`0ce6ae04`) promising to "replace
  duplicated ad-hoc event construction"; the duplication it targeted was
  instead solved by local per-file builders. Later commits only maintained the
  file itself (facade path fix `92fd7af4`, metadata lint `e3a76daf`) — proof
  of maintenance without use.
- Standalone/debug/build purpose: never selected by the runner, no script
  reference, no covers row.

### 3. `tests/helpers/ws-test-helpers.rkt` — provides `make-ws-context` (W0 scaffold)

- Precise supersession point in history: created as W0 scaffolding for
  v0.29.4 (`62d164b1`) with the in-file note "In W2, the real implementation
  will be in runtime/working-set.rkt"; W2 (`baaaa818`, "Updated
  test-working-set-factory.rkt to use real factory (19 tests pass)") migrated
  its exactly-one consumer `tests/test-working-set-factory.rkt` from
  `(require (only-in "helpers/ws-test-helpers.rkt" …))` to the production
  `runtime/working-set.rkt` factory. Consumer count has been 0 ever since.
- Current-tree grep:
  `rg -n "require.*ws-test-helpers" --glob '*.rkt' .` → **0 hits**; the only
  `make-ws-context` definitions on disk are the production one
  (`runtime/working-set.rkt:291`, richer: returns `ws-entry` structs) and the
  deleted helper's weaker hash-returning duplicate.
- Full-history search: `git grep -l 'ws-test-helpers' <sha> -- '*.rkt'` over
  all 17 pickaxe commits → only `tests/test-working-set-factory.rkt` at the
  two pre-W2 commits (`62d164b1`, `6c918391`); nothing after W2.
- Standalone/debug/build purpose: none — it is strictly the superseded W0
  stand-in for a production function that has existed since v0.29.4.

### Post-deletion sweep

`rg -n "helpers/contract-blame|helpers/test-events|ws-test-helpers" --glob
'*.rkt' --glob '*.json' --glob '*.rktl' .` (excluding `artifacts/`) → 0 hits:
no dangling reference in code, metadata, or generated artifacts.

## Regenerated artifacts (after `git add -A`)

| Artifact | Command | Result |
|---|---|---|
| Tier matrix | `racket scripts/tier-ownership-matrix.rkt generate` | 1172 → **1169 rows** (exactly the 3 helper rows removed; python set-diff of rows confirms no other change); `check` green |
| Covers manifest | `racket scripts/run-tests.rkt --generate-covers-manifest` | entries=223, **byte-identical** (sha256 before/after match) |
| Ownership reports | `racket scripts/run-tests/inventory.rkt --ownership-map` | 7 areas, 0 gaps; `reports/test-ownership-map.{md,json}` **byte-identical** |
| Metrics | `racket scripts/metrics.rkt --sync-all` | README: test files 1681 → **1678**; test lines 366592 → **366421** (−171 = exactly the deleted helpers); test assertions 52707 → **52702** |
| Milestone snapshot | `run-tier-ownership-check artifacts/tier-ownership/v1.00.32-w0/ownership-matrix.json` | **0 errors** on the post-deletion tree (snapshot tracks test families, not helpers); its `SHA256SUMS` verifies OK |

`docs/TEST_OWNERSHIP{,_MAP}.{json,md}` are legacy snapshots (no generator
writes them; 0 helper mentions; area-based over test files) — intentionally
untouched.

### `sha256sum -c` results

- `artifacts/tier-ownership/v1.00.32-w0/`: **OK** (current-release snapshot,
  unmodified, verifies from its dir).
- Dir-relative manifests (25 dirs incl. all `v1.00.29-*`, `v1.00.31-*`,
  proof-bundles): **OK**.
- Pre-existing anomalies, untouched and unmasked by this wave (empty
  `git diff HEAD -- artifacts/`): `test-runtime/v1.00.28-w1/SHA256SUMS` is
  root-relative and its `wait-audit.json` hash no longer matches even the HEAD
  blob (a pre-W6 wave edited the file without re-baking); `v1.00.28-w2` and
  `v1.00.28-w5` manifests are root-relative (verify OK from repo root);
  `v1.00.24-hotspots/SHA256SUMS` references renamed legacy files. None of
  these concern W6 files; fixing past-milestone evidence records is outside
  this wave's scope and would falsify historical snapshots.

## Final sweep gates

| Gate | Command | Result |
|---|---|---|
| Full lint suite | `racket scripts/lint-all.rkt` | **23 passed, 0 failed**, 2 non-blocking warnings (`arch`, `release-readiness` — same as baseline) |
| Format | `racket scripts/lint-format.rkt` | 0 errors, 0 warnings (2651 files) |
| Test lint | `racket scripts/lint-tests.rkt` | PASSED (0 errors; 386 warnings / 110 infos, pre-existing) |
| Docs lint | `racket scripts/lint-docs.rkt` | PASSED (0 warnings) |
| Metrics lint | `racket scripts/metrics.rkt --lint` + `--lint-prose` | both PASS after sync |
| Quick lane | `./qt quick` | passed: 28 files, 325 tests |
| Changed impact | `./qt quick --changed HEAD` | passed: 8 files, 41 tests |
| Offline PR lane | `./qt default --explain` | exit 0 (honest not-verified list unchanged) |
| Deterministic clock | `racket scripts/run-tests.rkt tests/test-deterministic-clock.rkt` | PASS (wait-audit untouched — no `(sleep …)` literal moved — so no wait-audit lint due) |
| Milestone gate | `racket scripts/run-tests.rkt tests/test-milestone-gate.rkt` | PASS (46 s) — incl. the `v1.00.32-w0` ownership-matrix drift-enforcement cases |
| Tier matrix | `racket scripts/tier-ownership-matrix.rkt check` | green (1169 rows) |
| Protected paths | `git diff --exit-code -- scripts/required-pr-checks.policy .github .planning extensions runtime agent llm tools cli tui` | empty (worktree and staged) |

## Net LOC / effectful work

| File | Before | After |
|---|---|---|
| `tests/helpers/contract-blame.rkt` | 42 | deleted |
| `tests/helpers/test-events.rkt` | 75 | deleted |
| `tests/helpers/ws-test-helpers.rkt` | 54 | deleted |
| `docs/automatic-review.md` | — | +17 / −8 (correction) |
| `README.md` | — | generated metrics only |
| `tests/tier-ownership-matrix.json` | 1172 rows | 1169 rows (regenerated) |
| `docs/reports/W6-CLEANUP-v1.00.33.md` | — | new (this report) |

Net test/fixture LOC: **−171** (pure deletion; no helper consolidation, no new
fixtures). Effectful work: one doc correction; zero production modules, zero
test behaviors, zero runner/lint/policy files touched. All three deletions
remove dead weight only — no test lost a claim, because no test could
require them.

## Residual risk

- `tests/test-gsd-auto-review-loop.rkt`'s own header comment still says
  "Python controller" (legacy phrasing in a test file outside this wave's
  allowed file scope); its execution path is native, and the corrected doc now
  states that. A later docs-scoped wave may align the comment.
- The three pre-existing artifact-manifest anomalies listed above remain
  (historical evidence records; fixing them is not a W6 deletion-tier item).
- Legacy `docs/TEST_OWNERSHIP*` snapshots predate the current generator
  layout and were not refreshed (no live consumer found).
- Name collisions: any future test defining a local `make-test-event` remains
  unaffected; grep discipline (`require.*` form, not bare identifier) is what
  distinguished consumers from collisions here.
