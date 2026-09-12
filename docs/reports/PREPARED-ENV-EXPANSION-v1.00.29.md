# Prepared-Environment Expansion — v1.00.29 W6

**Wave:** `v1.00.29-w6` — *Prepared-environment expansion* (spec `PLAN-v1.00.29-PROOF-GRAPH-REDUCTION.md` §6 W4)
**Branch:** `campaign/v1.00.29-w6` (base `520805da`, W5 merge)
**Status:** DELIVERED (this report) · Machine-readable companions:
`artifacts/proof-graph/v1.00.29-w6/consumers.json` (schema `prepared-env-consumers@1`) and
`artifacts/proof-graph/v1.00.29-w6/setup-savings.json` (schema `prepared-env-setup-savings@1`)

---

## 1. What this wave delivered

1. **Immutable prepared-environment identity, computed and compared.**
   `scripts/ci/prepared-env-report.rkt` gained the identity-manifest modes
   (`--identity-emit`, `--identity-compare`, `--identity-fallback-record`).
   The identity is the nine-dimension tuple of spec §6 W4: OS image, architecture,
   Racket version **+ executable digest**, package lock/**resolved set**,
   precompile-recipe revision, policy knobs, and the artifact digest. The artifact
   identity *name* is derived from all nine dimensions (`prepared-env-identity@1`),
   so a difference in **any** dimension yields a different artifact identity —
   cross-Racket/platform/policy reuse of an artifact name is structurally excluded.
2. **The shared action computes the manifest at save time and compares at restore.**
   `.github/actions/setup-racket/action.yml` (re-stamped, §5 below) now runs:
   - *compare at restore*: after a verified restore, derive the expected identity
     (job tuple + this checkout) and the observed identity (this runner image, the
     job-local Racket executable digest, the materialized store) and compare **every
     dimension** through `--identity-compare`;
   - *save-side manifest*: whenever the full install+compile path runs (primary cold
     build, flagged fallback, or identity-mismatch fallback), stamp the identity of
     the built environment to `RUNNER_TEMP` + step summary + counted `::notice`.
3. **ANY mismatch = loud, counted cold fallback.** A mismatch emits `::warning::`,
   writes the counted fallback record (`prepared-env-fallback-record@1`) and a
   `RUNNER_TEMP` stamp (`prepared-env-identity-fallback-stamp.json`), flips the
   step output `identity-result=mismatch`, and routes the job to the full cold
   path — never silent acceptance. A failure of the gate itself is a hard failure.
   The W4 BUG-0065 purge step and the guarded restore step are byte-identical.
4. **The verified restore is expanded to every provably-identical consumer**
   (§2), each with a per-consumer one-command rollback switch (§5).
5. **Honest savings ledger** (§4): measured where retained evidence exists,
   projected-with-formula everywhere else.

## 2. Consumer matrix — activation decisions

Derived from the W0 workflow inventory (`workflow-inventory.json` + `graph.json`,
41 jobs) cross-checked against the live workflows. Producer: `ci.yml:fast-env`
(`prepared-env-fast`, Linux/x64/Racket 8.10/CS/full, lock
`28571bb312c728efdb8c81ccf93a79063ebcb0ff290570d68f0359de1040e7bd`, 24 h retention,
same-run consumption only).

| # | Consumer | Profile | Decision | One-command rollback (§11.6) |
|---|----------|---------|----------|------------------------------|
| 1 | `ci:test` (3 shards) | identical | **activated** (W3; W6 adds identity compare) | `gh variable set RACKET_PREPARED_ARTIFACT --body off` |
| 2 | `ci:smoke` | identical | **activated (W6)** | `gh variable set RACKET_PREPARED_SMOKE --body off` |
| 3 | `ci:workflows` (2 shards) | identical | **activated (W6)** | `gh variable set RACKET_PREPARED_WORKFLOWS --body off` |
| 4 | `ci:release-dry-run` | identical | **activated (W6)** | `gh variable set RACKET_PREPARED_RELEASE_DRY_RUN --body off` |
| 5 | `ci:gsd-governance` | identical | **activated (W6)** | `gh variable set RACKET_PREPARED_GSD_GOVERNANCE --body off` |
| 6 | `ci:abstraction-audit` | identical | **activated (W6)** | `gh variable set RACKET_PREPARED_ABSTRACTION_AUDIT --body off` |
| 7 | `ci:prepared-env-report` | identical | **activated (W6)** | `gh variable set RACKET_PREPARED_ENV_REPORT --body off` |
| 8 | `ci:lint-quality` | identical env | **deferred** — W2 checkpoint pins "no ordering edge in either direction" with fast-env (`test-w9`: lint-quality must have no `needs:` line) | none needed (deferred) |
| 9 | `ci:test-platform` | identical env (ubuntu) | **deferred** — W2 checkpoint records + w9 pins its needs edge as exactly `[lint]` | none needed (deferred) |
| 10 | `ci:test-cross-version` | Racket **8.11** | **deferred** — distinct runtime → separate artifact identity `prepared-env-racket-8.11@…`; lock rejects 8.11 (W0 known-red) | none needed (deferred) |
| 11 | `ci:security` | strict queue | **deferred** — spec gate: *no strict-security consumer receives a weaker prepared environment*; reserved identity `prepared-env-strict@…` | none needed (deferred) |
| 12 | `ci:release-readiness` | identical env | **deferred** — tag-conditioned lane, no same-run producer | none needed (deferred) |
| 13–17 | `fullreg:test` (×6), `workflows-suite`, `test-platform` (macos-14), `summarize`, `mutation-pilot` | identical / **Darwin-arm64** | **deferred** — separate workflow runs: the artifact is same-run scoped; macos lane is a distinct platform identity `prepared-env-platform-macos-arm64@…` | none needed (deferred) |
| 18 | `nightly:test` | identical env | **deferred** — run isolation (no same-run producer) | none needed (deferred) |
| 19–21 | `release:preflight/test/prepare` | identical env | **deferred** — release lanes build cold by design (reproducibility) | none needed (deferred) |
| 22–27 | `rc:build/smoke/draft/verify-draft/publish/verify-public` | identical env | **deferred** — release reproducibility + verifier independence | none needed (deferred) |
| 28–29 | `repair:diagnose/apply` | identical env | **deferred** — repair lane pins its own toolchain, stays independent | none needed (deferred) |
| 30 | `benchmark:benchmark` | identical env | **deferred** — benchmarks must measure the cold environment | none needed (deferred) |
| 31 | `pilot:pilot` | identical env | **deferred** — report-only direct-restore instrument (BUG-0065 PURGE-EXEMPT), never an activated production consumer | none needed (deferred) |

Excluded (no Racket setup, per live workflow text): `ci:lint`, `ci:test-aggregate`,
`ci:workflows-aggregate`, `fullreg:report`, `shadow:shadow-run`, `cohort-c1:plan`,
`cohort-c1:shadow`, `shard-telemetry:report`, `project-automation:update-status`.

**Tally: 7 activated consumer records (10 job instances), 24 deferred, 9
non-Racket exclusions, 1 producer — all 41 W0 jobs accounted for.**

## 3. Fallback observations (loud + counted)

- **Producer-skipped / restore-failure** (W3 semantics, unchanged): the shard
  evaluates `PREPARED_ENV` to `off`/falls back to the full path; the fallback is
  flagged with `::warning` + REBUILT step summary — never silent.
- **Identity mismatch (new in W6)**: `--identity-compare` exits 1 with a named
  field list; the action emits `::warning::prepared-environment identity mismatch …`,
  writes the counted record + `RUNNER_TEMP` stamp, appends a superseding step-summary
  section, and routes to the full cold path. Test-proven fail-closed cases:
  wrong `racket-executable-digest`, wrong `os`, wrong `os-image`, wrong
  `lock-digest`, multi-dimension mismatch, and an incomplete manifest
  (missing dimension ⇒ hard failure, never silent pass).
- **Cold fallback observed**: every full-path build now stamps a save-side identity
  manifest (`prepared-env-identity-save-stamp.json`), so a cold fallback is always
  observable and countable per lane. No post-activation retained CI run exists in
  this wave's evidence set (sandbox; no live runner) — the committed observation
  window (`v1.00.26-prepared-env`, 129 records) predates activation and honestly
  carries `restore-ms: "unknown"`; the W5 emit-restore-record wiring reopens the
  window post-merge, now covering the six newly activated lanes.
- **Verified-restore gate**: threshold stays ≥95 % (tool-enforced in the
  aggregator); this wave records no fabricated rate.

## 4. Setup savings vs the W0 baseline

Per-consumer rows live in `setup-savings.json`; derivation from the retained W0
evidence (`evidence/jobs-34450964386.json`, run at `293b6a271aa1`):

- **W0 full-path class baseline**: mean **259.3 s** over the 11 full-path Racket
  lanes of run 34450964386 (lint-quality 273, workflows 245/246, release-dry-run
  225, security 295, test-platform 261, cross-version 292, smoke 243,
  gsd-governance 217, abstraction-audit 306, prepared-env-report 249).
- **Restored-path measurement**: the same run's test shards measured
  **23 / 20 / 40 s** (W3-activated lane) — used as the conservative restored-path
  estimate (it still contains the job-local runtime install).
- **`ci:test` (measured)**: saved ≈ **11.6 runner-minutes per main-CI run**
  (3.94 + 3.99 + 3.65), banked at the W3 activation; evidence cited in-file.
- **Six W6 lanes (projected-from-w0-baseline, formula in each row)**:
  smoke 3.67 + workflows 7.42 (2 shards) + release-dry-run 3.37 +
  gsd-governance 3.23 + abstraction-audit 4.72 + prepared-env-report 3.77
  ≈ **26.2 runner-minutes per main-CI run**. Each row marks
  `measurement: "projected-from-w0-baseline"` with the formula
  `saved_seconds = own-lane w0 setup step − 23 s restored-path estimate`;
  the projection is an upper bound (restore-path overheads unmeasured). **No
  measured post-activation numbers exist in this wave's evidence set; none are
  claimed.**

## 5. Checksum re-stamps (checksum-pin discipline, W4 precedent)

Changing `.github/actions/setup-racket/action.yml` invalidated the pins; all were
re-stamped:

| Artifact / pin | Old | New |
|---|---|---|
| `setup_action_sha256` in `artifacts/ci-topology/v1.00.26-w2/dag-checkpoint.json` | `4d93721474c14f9e396ce1ea4fd7e4c08732c8de9870d7cf6ac4662f2d6c396c` | `a8532d7cfa683fe3e9ed3426dfcf11dd43531acec166ee14f7e802e016062784` |
| `artifacts/ci-topology/v1.00.26-w2/SHA256SUMS` (checkpoint file hash) | `3cd242bf89934566a49327f26947b201c04da4baabbfc73ca3373505b8da9d22` | `2ee4c26a41beb38d53ffe78aa8988c0286efe50dc1877b77fc119a09c17249ff` |
| literal pin `tests/test-w9-ci-workflow-verification.rkt` (`setup_action_sha256`) | `4d937214…6396c` | `a8532d7c…2784` |
| literal pin `tests/test-ci-runtime-contract.rkt` (checkpoint file hash) | `3cd242bf…49d22` | `2ee4c26a…749ff` |

The checkpoint's `prepare_action_sha256` (producer, untouched) is unchanged. A
`w6_restamp` provenance note was added to the checkpoint recording the old and new
values and the reason. The pin messages' surrounding strings were kept intact.

**Ownership matrix**: regenerated with
`racket scripts/run-tests/inventory.rkt --ownership-map --tier-matrix artifacts/tier-ownership/v1.00.28-w0/ownership-matrix.json`
(1385 families, 7 areas, 0 gaps) — **byte-identical** to the committed artifact
(no new test files; the two extended test files keep their families/metadata), so
the existing `SHA256SUMS` entry (`e9472899…6e6af`) remains valid and was NOT
re-stamped; `--ownership-map --check` ⇒ **PASS, no drift**.

## 6. §11.6 Rollback — one command per consumer, exercised

Every activated consumer evaluates a `PREPARED_ENV` expression of the form
`(global switch && consumer switch && not workflow_dispatch && producer success) && 'auto' || 'off'`.
Setting the consumer's repository variable to `off` — or
`RACKET_PREPARED_ARTIFACT=off` for all consumers at once — pins exactly that lane
to the legacy full cold path; the producer stays available for the others. The
rollback decision logic is executable and drill-tested via
`scripts/ci/prepared-env-report.rkt --rollback-drill`.

**Rollback drill (exercised for `ci:smoke`; commands and observed outputs):**

```
$ racket scripts/ci/prepared-env-report.rkt --rollback-drill \
    --consumers artifacts/proof-graph/v1.00.29-w6/consumers.json \
    --consumer ci:smoke
{"schema":"prepared-env-rollback-drill@1","consumer":"ci:smoke",
 "rollback-variable":"RACKET_PREPARED_SMOKE",
 "rollback-command":"gh variable set RACKET_PREPARED_SMOKE --body off",
 "event":"push","producer-result":"success",
 "prepared-path-in-play":true,
 "effective-setup-path":"verified prepared-env restore"}

$ racket scripts/ci/prepared-env-report.rkt --rollback-drill \
    --consumers artifacts/proof-graph/v1.00.29-w6/consumers.json \
    --consumer ci:smoke --vars RACKET_PREPARED_SMOKE=off
{"…","prepared-path-in-play":false,
 "effective-setup-path":"legacy full cold setup"}
```

After the drill: `ci:smoke` is pinned to the legacy cold path while
`--consumer ci:workflows` still reports `prepared-path-in-play: true` with
`RACKET_PREPARED_SMOKE=off` (drill-asserted in
`tests/test-prepared-env-report.rkt`). The live CI-side command for the operator
is exactly the `rollback-command` above; the drill exercises the same decision
expression the workflow evaluates, and
`tests/test-workflow-purge-contract.rkt` pins that every activated consumer's
workflow body wires its rollback variable.

## 7. Honesty notes / spec ambiguities resolved

- **Producer half is W3-frozen this wave** (only `setup-racket/action.yml` was in
  scope): the artifact manifest already carries repository/git-sha/os/arch/
  racket-version/lock-digest (enforced at restore). The OS-image,
  executable-digest, resolved-set and recipe dimensions are computed and recorded
  consumer-side and at save time; the compare tooling fail-closes on them the
  moment a producer manifest records them. Declared, not hidden.
- **`policy` axis = environment-policy class** (store schema, compile recipe,
  purge invariant), not the consumer payload's `STRICT_TEST_RUNNER` execution
  flag — the latter cannot change the produced bytes. The strict *security
  queue* is nevertheless kept on a reserved separate identity class, per the
  spec gate on strict-security consumers.
- **`ci:test-platform` runs on `ubuntu-latest`** (identical profile) despite the
  W0 inventory's `macos-arm64` note — ground truth is the workflow file; the lane
  stays deferred because of the pinned W2 needs edge, not its profile.
- `fullreg:report`, aggregates, lint, shadow/cohort/shard-telemetry jobs run no
  Racket setup at all (verified against the live workflow text) and are excluded
  from the matrix with reasons.

## 8. Verification (wave contract)

- `racket tests/test-prepared-env-report.rkt` — **30/30 GREEN**
- `racket tests/test-workflow-purge-contract.rkt` — **GREEN** (incl. new W6
  activated-consumer scan + bypass negative fixture)
- `racket tests/test-ci-runtime-contract.rkt` — **38/38 GREEN** (re-stamped)
- `racket tests/test-w9-ci-workflow-verification.rkt` — **18/18 GREEN**
  (checkpoint SHA256SUMS verifies: `…/dag-checkpoint.json: OK`)
- `racket tests/test-release-workflow-contract.rkt` — **GREEN**
- `scripts/metrics.rkt --lint` — **PASS (5/5)**; `scripts/check-deps.rkt` — **PASS**
- Fast suite (`--suite fast`, CI's native sharding, 16×): **1185 files, 1184 pass,
  1 pre-existing failure** (`tests/test-interfaces-tui.rkt`, selection-text P1,
  TTY-sensitive — same signature W5 recorded; this wave touches no TUI file),
  0 timeouts.
