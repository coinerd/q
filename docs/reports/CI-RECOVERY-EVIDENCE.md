# CI-RECOVERY-EVIDENCE — W1 strict trial assessment contract (v1.00.32)

Status: UNMERGED CONTRACT/IMPLEMENTATION CANDIDATE — measurements NOT taken.
Owner: W1 evidence implementer (scripts/ci/prepared-env-report.rkt,
tests/test-prepared-env-report.rkt, this document). Consumers: the W1/W2
workflow owner (.github/workflows/ci.yml, ci-recovery-benchmark.yml wiring) and
the W2 report owner (scripts/run-tests/ci-recovery-benchmark-report.rkt).
Plan: `.planning/PLAN-v1.00.32-MEASURED-PR-CI-RECOVERY.md`, wave
`waves/W1-baseline-and-activation-evidence-readiness.md`.

**No number in this document is a measurement.** Every JSON fragment below is a
SYNTHETIC fixture that illustrates the contract. Actual baseline collection is a
separately authorized coordinator gate; this wave launches no CI and claims no
activation, savings or recovery.

## 1. Purpose and non-goals

One strict, complete all-shard treatment assessment binds ONE trial attempt to
identities and to ACTUAL execution telemetry:

- run / head / attempt / trial / treatment / expected shard inventory binding,
- environment identity (source sha, executable, platform, lock, recipe, policy)
  plus the producer compiled-root manifest digest,
- refusal (never silent acceptance) of duplicate, missing, mixed or malformed
  shard records,
- actual launcher telemetry (`root-hits > 0`, `fallbacks == 0`,
  `mode == "verified-root-hit"`) as the ONLY root-on activation evidence for
  treatment C — a resolution step alone never certifies,
- an honest, distinct `ready-root-off` classification for treatments A/B whose
  launcher telemetry shows `global-off` everywhere.

Non-goals: no new telemetry framework (existing `--emit-restore-record` shards,
the `ci/prepared-environment/compiled-root.rkt` launcher datum and the existing
canonical identity validators (`--identity-emit` / `--identity-compare`) are
reused verbatim); no change to ordinary PR CI behavior, required checks or
execution switches; no performance verdict.

## 2. Envelope schema: `ci-recovery-trial-evidence@1`

One JSON file per trial attempt. Field names are exact; `"unknown"` is the only
honest placeholder for unobserved data (never a fabricated zero).

```json
{
  "schema": "ci-recovery-trial-evidence@1",
  "trial": {
    "trial-id": "trial-2026-10-30-c1",   // non-empty string, coordinator-chosen
    "attempt": 1,                        // positive integer
    "treatment": "C",                    // "A" | "B" | "C" (see §4)
    "run-id": 7100,                      // number; must equal every record's run-id
    "head-sha": "aaaa...40 hex chars",   // EXACT 40-char lowercase hex; must equal every record's head-sha
    "created-at-utc": "2026-10-30T10:00:00Z"
  },
  "expected": {
    "shard-inventory": [0, 1],           // non-empty list of UNIQUE integers: the exact expected shards
    "producer-manifest-digest": "dddd...64 hex chars",
      // SHA-256 of prepared manifest.json (A/B), compiled manifest.rktd (C);
      // caller/producer-bound, never read back from a shard
    "identity": {
      // the nine prepared-env identity dimensions + artifact digest,
      // exactly the fields of a prepared-env-identity@1 manifest:
      "os": "Linux", "os-image": "ubuntu-24.04:...", "arch": "x64",
      "racket-version": "8.10",
      "racket-executable-digest": "<64hex>",
      "lock-digest": "<64hex>",
      "resolved-set-digest": "<64hex>",
      "precompile-recipe-revision": "trial-addon-relative-r1",
      "policy-fingerprint": "addon-store-v2-no-workspace-bytecode",
      "artifact-digest": "<64hex>"
    }
  },
  "shards": [
    {
      "shard": 0,                        // integer; member of expected.shard-inventory
      "record": { /* one prepared-env-restore-record as emitted by
                     `--emit-restore-record`, PLUS the workflow adapter's strict
                     binding metadata (the emitter itself never fabricates it):
                       "trial-id":  == trial.trial-id,
                       "attempt":   == trial.attempt,
                       "treatment": == trial.treatment,
                       "identity":  the shard's ACTUAL observed environment
                                    identity — the ten fields of a
                                    prepared-env-identity@1 manifest as
                                    measured on this shard — compared field by
                                    field against expected.identity.
                     run-id/head-sha/shard must match the trial; the outcome must
                     be exactly verified|rebuilt|fallback — "unknown" refuses and
                     so does ANY other value outside the enum. */ },
      "launcher": {
        // ACTUAL launcher telemetry for this shard, converted 1:1 from the
        // Racket datum ci/prepared-environment/compiled-root.rkt writes to
        // --telemetry-file (tag `compiled-root-launch-telemetry`):
        "producer-manifest-digest": "<64hex>",  // OBSERVED binding at launch; must equal expected
        "mode": "verified-root-hit",     // verified-root-hit | global-off | no-flags-current-source | unmapped-child
        "root-hits": 3,                  // integer >= 0 (datum field root-hits)
        "fallbacks": 0,                  // integer >= 0 (datum field fallbacks)
        "exit-code": 0,                  // REQUIRED integer (the launcher CLI always emits it); must be 0
        "wall-ms": 90000, "loads": 12, "bytes-served": 1048576,
        "mechanism": "external-compiled-root(map-dir symlinks)",
        "raw-datum": "(optional verbatim datum string, retained only)"
      }
    }
  ]
}
```

Binding rules (all enforced; see §5):

- `trial.head-sha` must be exactly 40 lowercase hex characters (exact source
  binding). Every `record.head-sha` and `record.run-id` must equal the trial's.
- Every record must carry the adapter-attached `attempt`, `trial-id`,
  `treatment` and `identity` keys. The first three must equal the trial block's
  values (`record-attempt-mismatch` / `record-trial-id-mismatch` /
  `record-treatment-mismatch` — a missing key refuses with the same reason,
  since the key is required). `identity` must be a well-formed ten-field
  identity set (`record-identity-missing` / `record-identity-malformed`) whose
  nine canonical dimensions AND `artifact-digest` each equal
  `expected.identity` (`record-identity-mismatch-<field>`).
- `expected.identity` is validated by the SAME rules as the canonical
  prepared-env identity manifest (`--identity-emit` / `--identity-compare`):
  digest dimensions must be well-formed 64-hex, the others non-empty. It is
  CALLER-provided; a shard that carries `expected-identity` or
  `expected-manifest-digest` keys is refused (self-attestation ban).
- Every `launcher.producer-manifest-digest` must equal
  `expected.producer-manifest-digest` — this is the binding between the shards
  that ran and the producer manifest whose bytes the caller can hash with
  `--producer-manifest F`.
- Every `launcher.exit-code` must be an integer (`launcher-exit-code-invalid`)
  and 0 (`launcher-exit-code-nonzero`). The actual launcher CLI always emits
  `exit-code`, so it is a REQUIRED field and missing telemetry fails closed; a
  FAILED launcher process never certifies a trial, not even with
  `mode == "verified-root-hit"`.

Trust model: the expected context is CALLER/PRODUCER-bound, and that binding is
not a mere hash claim. The caller supplies it from the dispatch context and the
producer side — never from the shards under assessment — so the assessor
compares observed telemetry AGAINST it; shard self-attestation of expected data
is refused. `--producer-manifest F` strengthens the digest expectation into a
byte-level check: the manifest file's SHA-256 must equal
`expected.producer-manifest-digest`, and `--expected-identity F` must be a real
`prepared-env-identity@1` manifest artifact. Trusting the caller/producer for
the expected side is exactly what makes the comparison meaningful: it is the
one side of the equation the shards cannot forge.

Datum → JSON mapping for `launcher` (workflow owner): the telemetry datum is
`(compiled-root-launch-telemetry (mode . "<mode>") (exit-code . n) (wall-ms . n)
(root-hits . n) (fallbacks . n) (loads . n) (bytes-served . n) (mechanism . "<m>"))`
written by `write-launch-telemetry!`; map each assoc field to the identically
named JSON key shown above. The five verdict-relevant fields are `mode`,
`root-hits`, `fallbacks`, `exit-code` (all required — the CLI always emits
`exit-code`) and `producer-manifest-digest`; the rest are retained for the
report when present.

## 3. CLI: `--trial-assess`

```
racket scripts/ci/prepared-env-report.rkt --trial-assess --envelope F \
  [--out VERDICT.json] [--expected-identity MANIFEST] [--producer-manifest MANIFEST]
```

- `--envelope F` (required): the `ci-recovery-trial-evidence@1` JSON file.
- `--out VERDICT.json`: write the verdict (§6) as canonical, byte-deterministic
  JSON. Identical envelopes produce byte-identical verdicts.
- `--expected-identity F`: supply the producer-bound expected identity as a real
  `prepared-env-identity@1` manifest (the exact artifact `--identity-emit`
  produces). The existing validator runs — a structurally invalid manifest exits
  nonzero without a verdict. Conflicts with an inline `expected.identity` exit 2.
- `--producer-manifest F`: the REAL `manifest.rktd` artifact; its SHA-256 must
  equal `expected.producer-manifest-digest`, else the refusal reason
  `producer-manifest-digest-mismatch-with-file` is added.

Exit codes: `0` = ready (`ready?` true); `1` = assessment completed but refused
(verdict with named reasons IS written); `2` = structural misuse (missing
`--envelope`, unparseable JSON, invalid identity manifest, unreadable
`--producer-manifest`, identity conflict — no verdict written).

Existing modes (`--emit-restore-record`, `--aggregate`, `--manifest --check`,
`--identity-*`, `--consumers-check`, `--savings-check`, `--rollback-drill`,
`--containment-check`) are UNCHANGED; report-only users keep their behavior and
exit codes.

## 4. Classification semantics

| treatment | meaning | ready requires (per shard, ALL shards) |
|---|---|---|
| C (root-on) | verified compiled root + prepared aggregate | launcher `mode == "verified-root-hit"`, `root-hits > 0`, `fallbacks == 0`, `exit-code == 0`, launcher digest == expected digest, record identity == expected identity |
| A (root-off, cold aggregate) | today's bounded-eager path | launcher `mode == "global-off"`, `exit-code == 0` (root activity is contamination here) |
| B (root-off, prepared aggregate) | prepared aggregate, root untouched | same as A |

- classification `ready-root-on`: every expected shard present exactly once,
  all bindings intact, and the C column satisfied. Root-off is NEVER mixed into
  a root-on verdict: a C shard with `mode == "global-off"` (or fallback > 0, or
  zero hits) refuses the trial.
- classification `ready-root-off`: A/B with all-`global-off` telemetry — an
  honest DISTINCT classification, not a failed or successful root-on trial.
- classification `refused`: any refusal reason applies (§5); `ready?` is false.
- `ready?` is true exactly when the classification is not `refused`.
- A failed launcher (`exit-code != 0`) or launcher telemetry without the
  mandatory `exit-code` field refuses under EVERY treatment, even when the
  mode and counters look like a hit: process success is part of activation
  evidence, not an optional extra.

Resolution-only claims (a record or a resolve-step success WITHOUT launcher
telemetry, or with `root-hits == 0`) never certify activation: the trial is
refused with `missing-launcher-telemetry` / `zero-root-hits`.

## 5. Refusal reason catalogue (exact strings; deduplicated, sorted)

Envelope / trial block: `envelope-schema-invalid`, `missing-trial-block`,
`trial-id-invalid`, `attempt-invalid`, `treatment-invalid`,
`trial-run-id-invalid`, `trial-head-sha-malformed`, `trial-created-at-invalid`.

Expected context: `expected-block-invalid`, `expected-shard-inventory-invalid`,
`expected-producer-manifest-digest-malformed`, `expected-identity-missing`,
`expected-identity-invalid-<dimension>` (one per bad dimension, e.g.
`expected-identity-invalid-lock-digest`).

Shards / records: `shards-invalid`, `shards-empty`, `shard-entry-invalid`,
`duplicate-shard-record`, `unexpected-shard-record`, `missing-shard-record`,
`record-run-id-mismatch`, `record-head-sha-mismatch`, `record-shard-mismatch`,
`record-attempt-mismatch`, `record-trial-id-mismatch`,
`record-treatment-mismatch`, `record-identity-missing`,
`record-identity-malformed`, `record-identity-mismatch-<field>` (one per
differing field across the nine canonical dimensions plus `artifact-digest`,
e.g. `record-identity-mismatch-lock-digest`), `record-outcome-unknown`,
`record-outcome-invalid` (ANY outcome outside verified|rebuilt|fallback, e.g.
raw CI vocabulary `restored`/`success`),
`self-attested-expected-context-in-record`.

Launcher telemetry: `missing-launcher-telemetry`,
`self-attested-expected-context-in-launcher`,
`launcher-manifest-digest-malformed`, `launcher-manifest-digest-mismatch`,
`launcher-mode-invalid`, `launcher-root-hits-invalid`,
`launcher-fallbacks-invalid`, `launcher-exit-code-invalid`,
`launcher-exit-code-nonzero`, `launcher-mode-<mode>` (C: mode was a legal
launcher mode but not `verified-root-hit`, e.g. `launcher-mode-global-off`),
`zero-root-hits`, `launcher-fallbacks-nonzero`,
`launcher-mode-not-global-off` (A/B: any launcher mode other than
`global-off`).

CLI-only: `producer-manifest-digest-mismatch-with-file`.

Reason notes: a single-fault envelope yields exactly the single named reason
(e.g. dropping one shard yields only `missing-shard-record`); when the trial
head-sha itself is malformed, per-record head checks are suppressed so the
malformed-identity reason stays the named cause.

## 6. Verdict schema: `ci-recovery-trial-assessment@1`

```json
{
  "schema": "ci-recovery-trial-assessment@1",
  "ready?": true,
  "classification": "ready-root-on",   // ready-root-on | ready-root-off | refused
  "treatment": "C",
  "reasons": [],                        // sorted, deduplicated refusal reasons
  "shards-assessed": 2,
  "expected-shard-count": 2,
  "raw": {
    "trial": { /* echo of the envelope trial block */ },
    "expected": { /* echo of the envelope expected block (raw evidence retention) */ },
    "shards": [ /* echo of every shard entry: records + launcher telemetry */ ]
  }
}
```

Raw data is retained verbatim so the committed report can always show the
records and launcher telemetry behind a verdict.

## 7. Required function for the report owner

The report owner (`scripts/run-tests/ci-recovery-benchmark-report.rkt`) requires
the pure API directly — no subprocess, no parsing:

```racket
(require "../ci/prepared-env-report.rkt")
;; trial-evidence-schema   ; "ci-recovery-trial-evidence@1"
;; trial-assessment-schema ; "ci-recovery-trial-assessment@1"
;; trial-treatments        ; '("A" "B" "C")
;; launcher-modes          ; the four launcher modes
;; assess-trial-envelope   ; (assess-trial-envelope envelope) -> immutable hash
```

`(assess-trial-envelope env)` is PURE and DETERMINISTIC: no I/O, no environment
reads; identical input yields an `equal?` result and byte-identical JSON via
`json-value->string`. Result keys: `schema`, `ready?`, `classification`,
`treatment`, `reasons`, `shards-assessed`, `expected-shard-count`, `raw`
(same semantics as §6; use `hash-ref`). This is a plain concrete function +
data schema — no macros, no DSL.

## 8. Workflow owner wiring notes

1. Per shard, keep emitting `prepared-env-restore-record` with the existing
   `--emit-restore-record` mode (unchanged; it never emits trial metadata).
2. When the compiled-root lane resolves for a shard, capture the launcher
   telemetry file from `ci/prepared-environment/compiled-root.rkt launch
   --telemetry-file ...` and map the datum into the `launcher` object (§2) —
   including `exit-code`, which the CLI always emits. The launcher's observed
   manifest digest comes from the resolve step's flags binding; if the lane
   never ran, omit `launcher` entirely — for treatment C that is a refusal
   (`missing-launcher-telemetry`), never a silent pass.
3. Attach the strict binding metadata to each record (the adapter's job, never
   the emitter's): `attempt`, `trial-id`, `treatment` copied from the trial
   block, and `identity` = the shard's ACTUAL observed identity, taken from
   the identity manifest the shard itself emitted via `--identity-emit`.
4. Build the envelope: trial block from the dispatch context (run-id, head-sha,
   attempt, treatment), expected context from the PRODUCER side (shard
   inventory from the shard plan; identity manifest from `--identity-emit`;
   producer manifest digest = SHA-256 of the published `manifest.rktd`).
5. Run `--trial-assess` per trial; retain the verdict next to the raw records.
   Never mark a trial ready from the workflow's own claims — the assessor is
   the only ready-oracle.

## 9. Baseline collection protocol (coordinator gate, NOT this wave)

Baseline evidence uses CURRENT Actions timestamps and artifacts only. Historical
timings (e.g. the v1.00.30 820.5 s reference) are context, never the baseline.

1. Required critical path: `max(required checks' completed_at) -
   run.run_started_at`. It is ONE window from the run's start to the completion
   of the LAST required check — never a sum of job durations (parallel jobs
   would double-count) and never a chain of per-job intervals with inter-job
   dependency gaps subtracted. Because the window ends at the last required
   completion, required-DAG producer costs (e.g. a required compiled-root
   producer job) are naturally included; independent OPTIONAL pilot jobs are
   excluded from it (item 2). Queue time is reported separately, per job
   (`job.started_at - run.run_started_at`); it is never merged into execution
   time, and inter-job dependency gaps are never subtracted from the window.
2. Optional pilot jobs (compiled-root producer, prepared-aggregate producer
   canaries) are listed separately with their own timestamps; they are NEVER
   added into the required critical path.
3. Per-shard components are taken from the emitted records and launcher
   telemetry: slowest-shard `wall-clock-seconds`, `restore-ms`,
   `compiled-root-producer-seconds`, `compiled-root-fallback-seconds`,
   proof/verification steps. Components are reported individually; a sum is
   computed only when EVERY component is observed, never partially zero-filled.
4. Missing data stays `"unknown"`. Unknown is not zero, not skipped, not
   interpolated, and never backfilled from history.
5. Every retained number cites its run id, attempt, head sha and artifact, so
   the strict assessor can re-verify the binding end to end.

## 10. What this wave does NOT claim

No compiled-root or prepared-aggregate activation in normal PR CI; no measured
savings, recovery or promotion claim; no live experiment was dispatched; no
fixture above is a real outcome. Ready/`ready?` describes EVIDENCE QUALITY for
one synthetic-or-real trial envelope — it is never a performance verdict.

## 11. Trial adapter CLI and aggregate restore binding

`scripts/ci/ci-recovery-trial.rkt` accepts these modes (all are offline; none
launches Actions):

- `validate-dispatch --trial-id T --attempt N --treatment A|B|C --head-sha SHA`.
  Attempts remain positive; shard IDs in the stamp command are **nonnegative**.
  Empty invocation exits 2 with usage, rather than crashing on vector/list use.
- `bind-producer --stage DIR --out F`: hashes the actual staged `manifest.json`
  and, if present, `q-compiled/trusted-root/manifest.rktd` bytes.
- `emit-identity --stage DIR --out F --lock-file F --racket-version V --os OS
  --os-image IMG --arch ARCH --recipe-revision R --policy-fingerprint P`.
  A consumer replaces `--stage DIR` with `--addon-store INSTALLED_VERSION_DIR`.
  Both use `trial-addon-relative-r1`: sort files by path, concatenate each
  `<sha256 of file bytes>  <path relative to addon-store>\n`, then SHA-256 that
  byte stream. Both `resolved-set-digest` and `artifact-digest` use this digest.
  The other eight fields are observed from the actual executable, lock and
  runner inputs. Relocation alone cannot change the digest; changed relative
  names or contents do. The producer hashes staged bytes, each shard and the
  aggregate hash their **installed** store independently. No producer/expected
  identity is copied into observed evidence. The shared setup action's normal
  PR identity recipe and verification semantics are unchanged.
- `stamp-producer-manifest --shard N --out F --prepared-download DIR
  [--compiled-root DIR]`: observes the restored compiled manifest if present,
  otherwise the restored prepared manifest. Missing bytes cause exit 1 and no
  stamp, never a synthesized binding. A/B bind prepared bytes; C binds compiled
  bytes, including after artifact extraction under a different absolute root.
- `attest-aggregate --out F [--identity F --stamp F]`: serializes JSON using the
  independently emitted observed identity and stamp. Reads `RUN_ID`,
  `RUN_ATTEMPT`, `HEAD_SHA`, `AGG_REQUESTED`, `AGG_RESULT`, `AGG_STATE`,
  `AGG_IDENTITY_RESULT`, `AGG_RESTORE_MS` from the workflow environment.
  Missing numeric outputs become the JSON **string** `"unknown"`; no bare
  `unknown` token or zero-fill is emitted.

The collector embeds the **raw** attestation at top-level
`actions.json["aggregate-attestation"]` (also in `actions.jsonl`), not merely
`config.prepared-aggregate-restore`. Its schema is
`ci-recovery-aggregate-restore-attestation@1`, with keys:

```json
{
  "schema": "ci-recovery-aggregate-restore-attestation@1",
  "run-id": 7100, "run-attempt": 1,
  "head-sha": "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa",
  "prepared-requested": "auto", "result": "success", "state": "restored",
  "identity-result": "verified", "restore-ms": 15000,
  "identity": { "comment": "SYNTHETIC placeholder: actual canonical ten fields required" },
  "producer-manifest-digest": "dddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddd"
}
```

This illustrative placeholder identity is deliberately **not eligible**. B/C
require the actual canonical ten-field observed identity equal to envelope
`expected.identity`, the restored manifest digest equal to envelope
`expected.producer-manifest-digest`, matching run ID/attempt/head SHA, valid
schema, `prepared-requested=auto`, `result=success`, nonempty/nonunknown raw
state, and `identity-result=verified`. A requires `prepared-requested=off`.
The adapter refuses mismatches **after retaining** envelope, actions, raw
attestation, run/jobs and producer files. The report independently validates
these raw bindings; a shard-ready W1 verdict alone cannot establish B/C
eligibility. `components.prepared-aggregate-seconds` divides observed
milliseconds by 1000; unknown remains unknown.

## 12. Cancellation retention and offline reconciliation

The wrapper uploads `ci-recovery-context-RUN_ID-RUN_ATTEMPT/trial-context.json`
after validating the trusted dispatch and **before** running called code.
It contains trial ID/attempt/treatment, run ID/attempt/head SHA, workflow revision,
shard total/plan, inventory digest and context creation time. Collection uses
`always()`, not `!cancelled()`. This is best effort: GitHub may never schedule a
pending collector after cancellation. The coordinator must reconcile **every**
authorized attempt against its dispatch ledger, including attempts with no
final trial artifact. No reconciliation dispatches/retries a trial.

Read-only metadata endpoints (no source execution with write credentials):

- `GET /repos/OWNER/REPO/actions/runs/RUN_ID/attempts/RUN_ATTEMPT`: preserve the
  complete JSON; adapter reads `id`, `run_attempt`, `head_sha`, `path`, `event`,
  `run_started_at`, `head_branch`, `status`, `conclusion`.
- `GET /repos/OWNER/REPO/actions/runs/RUN_ID/attempts/RUN_ATTEMPT/jobs?per_page=100`:
  paginate all pages, combine `.jobs`; preserve job `id`, `name`, `status`,
  `conclusion`, `started_at`, `completed_at` (JSON null stays null in raw files).
- Artifact downloads are scoped to that run. Keep the early context and every
  available small evidence artifact; never download the multi-GB prepared store
  for reconciliation. Do not use the latest-attempt jobs endpoint for an older
  attempt, and do not substitute another run's context.

Exact offline reconciliation commands below assume a coordinator-authorized
read-only GitHub client (`gh` shown to match the workflow), `REPO`, `RUN_ID`,
`RUN_ATTEMPT`, and an empty per-attempt working directory. Run the adapter and
policy from a separately trusted checkout (`TRUSTED`), not downloaded source.
Artifact download misses are retained in logs and are NOT successful restores.

```sh
mkdir -p raw context evidence/records evidence/maps evidence/producer evidence/aggregate trial
# Retain metadata before executing any adapter code.
gh api "repos/$REPO/actions/runs/$RUN_ID/attempts/$RUN_ATTEMPT" > raw/run.json
gh api --paginate --slurp "repos/$REPO/actions/runs/$RUN_ID/attempts/$RUN_ATTEMPT/jobs?per_page=100" \
  | jq '{jobs: [.[].jobs[]]}' > raw/jobs.json
gh run download "$RUN_ID" --repo "$REPO" --name "ci-recovery-context-$RUN_ID-$RUN_ATTEMPT" --dir context
cp context/trial-context.json trial/trial-context.json
gh run download "$RUN_ID" --repo "$REPO" --pattern "prepared-env-records-$RUN_ID-*" --dir evidence/records
gh run download "$RUN_ID" --repo "$REPO" --pattern "compiled-root-map-$RUN_ID-*" --dir evidence/maps
gh run download "$RUN_ID" --repo "$REPO" --name "prepared-env-producer-manifests-$RUN_ID" --dir evidence/producer
gh run download "$RUN_ID" --repo "$REPO" --name "prepared-aggregate-restore-$RUN_ID" --dir evidence/aggregate
# Missing artifacts are allowed here only as missing evidence, never eligibility.
# Bind these values to the retained context; do not hand-edit labels or attempts.
CTX=trial/trial-context.json
test "$(jq -r '."run-id"' "$CTX")" = "$RUN_ID"
test "$(jq -r '."run-attempt"' "$CTX")" = "$RUN_ATTEMPT"
test "$(jq -r '."head-sha"' "$CTX")" = "$(jq -r '.head_sha' raw/run.json)"
racket "$TRUSTED/scripts/ci/ci-recovery-trial.rkt" collect \
  --run-json raw/run.json --jobs-json raw/jobs.json \
  --required-policy "$TRUSTED/scripts/required-pr-checks.policy" \
  --records-dir evidence/records --maps-dir evidence/maps --producer-dir evidence/producer \
  --aggregate-attestation evidence/aggregate --out-dir trial \
  --trial-id "$(jq -r '."trial-id"' "$CTX")" --attempt "$(jq -r '.attempt' "$CTX")" \
  --treatment "$(jq -r '.treatment' "$CTX")" --run-id "$RUN_ID" --run-attempt "$RUN_ATTEMPT" \
  --head-sha "$(jq -r '."head-sha"' "$CTX")" \
  --workflow-revision-sha "$(jq -r '."workflow-revision-sha"' "$CTX")" \
  --shard-total "$(jq -r '."shard-total"' "$CTX")" --shard-plan "$(jq -r '."shard-plan"' "$CTX")" \
  --test-inventory-digest "$(jq -r '."test-inventory-digest"' "$CTX")" \
  --created-at-utc "$(jq -r '."created-at-utc"' "$CTX")"
# Keep nonzero collect/assess exits in the coordinator log; continue retention.
racket "$TRUSTED/scripts/ci/prepared-env-report.rkt" --trial-assess \
  --envelope trial/envelope.json --out trial/verdict.json
```

Cancelled API runs cause collector exit 1 and `collection.json.result=refused`,
while preserving all files, even if some required jobs completed successfully.
`actions.run.status/conclusion` retain the raw run classification for independent
report validation. With absent records the W1 assessor also emits a refused
verdict. Never relabel a cancelled attempt as successful based on W1 shard
readiness alone. If cancellation happened before the context upload, retain
raw metadata plus the coordinator's **pre-existing** authorized dispatch ledger
context; if that context cannot be recovered, keep an explicitly incomplete,
refused attempt rather than inventing inputs or dropping the attempt.

These are candidate source changes and offline synthetic regression checks,
not delivered/merged behavior or proof that GitHub has run a cancelled collector.
