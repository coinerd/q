# CI Baseline & Cohort Snapshots — Retention / Schema Contract

This directory retains the **deterministic, reviewable inputs** that feed the
W4 cohort evidence tooling (`scripts/run-tests/cohort-report.rkt`) and the
W0 baseline reporter (`scripts/run-tests/baseline-report.rkt`).

Both tools are pure functions of on-disk inputs — **no network, no database,
no external service** is contacted at report-generation time. Everything an
activation reviewer needs to reproduce a report byte-for-byte is retained here
and named by milestone/cohort.

## Why retention matters

GitHub workflow artifacts expire after **seven days**. After expiry the raw
per-file runner JSON is gone, yet an activation reviewer must still be able to
reproduce a cohort report byte-identically. The contract below guarantees that:
the manifest (this directory) plus the cohort-report `--check` mode is a
closed, deterministic regeneration path.

## Layout

| Path | Produced by | Purpose | Retention |
|---|---|---|---|
| `runs-<run-id>.json` | maintainer `curl` of the GitHub runs REST endpoint (anonymous, retained) | Run-level summary + per-job wall clock | indefinite (named by run ID) |
| `jobs/<run-id>.json` | maintainer `curl` of the jobs REST endpoint (anonymous, retained) | Per-job/per-step timing | indefinite (named by run ID) |
| `selected-l3.txt` | `run-tests.rkt --impact-dry-run` | L3 impact-selected file list | per-baseline |
| `cohort-<milestone>-<n>.json` | cohort manifest (see schema below) | The 20-PR cohort activation record | indefinite (named by milestone/cohort) |
| `workflows.json` | retained workflow metadata | Scheduler/config snapshot reference | indefinite |

## Cohort manifest schema (schema-version 1)

A cohort is **20 consecutive eligible unique PR head SHAs**. Each SHA has
exactly one final successful **timing sample** (the timing datum) plus zero or
more failed/cancelled/rerun **reliability attempts**. The manifest is a JSON
object:

```json
{
  "cohort-id": "v1.00.23-cohort-1",
  "milestone": "v1.00.23",
  "schema-version": 1,
  "expected-count": 20,
  "shas": [
    {
      "sha": "<40-hex>",
      "pr": 9550,
      "scheduler": "batch",
      "ordering": "fifo",
      "attempts": [
        {"run-id": "...", "result": "failure",  "elapsed-seconds": 400.0, "timing-sample": false},
        {"run-id": "...", "result": "success",  "elapsed-seconds": 300.0, "timing-sample": true}
      ],
      "inventory-digest": "sha256:...",
      "file-count": 1162,
      "test-count": 16808,
      "pass": 1162, "fail": 0, "timeout": 0, "skip": 0,
      "zero-test": false,
      "flakes": 0,
      "parallel-only-failures": 0,
      "prepared-env": "match",
      "queue-wait-seconds": 12,
      "queue-depth": 0,
      "runner-minutes": 7.5
    }
  ],
  "exclusions": [
    {"sha": "<40-hex>", "reason": "missing-lane-artifact", "detail": "..."}
  ]
}
```

### Field contract

| Field | Requirement |
|---|---|
| `cohort-id`, `milestone`, `schema-version`, `shas` | mandatory top-level keys |
| `expected-count` | canonical 20; cohort size + exclusions must sum to this |
| `sha` | unique within the cohort (duplicate SHAs are rejected) |
| `attempts` | ≥1 attempt; exactly one must carry `"timing-sample": true` (the final success) |
| `result` | one of `success`/`failure`/`cancelled`/`rerun` (reliability evidence) |
| `timing-sample` | exactly `true` on the single final successful attempt per SHA |
| `inventory-digest` | non-empty per-SHA file/test inventory digest |
| `scheduler` | one of `batch`, `serial` (incompatible schedulers rejected) |
| `prepared-env` | one of `match`, `rebuild`, `cached` |
| `zero-test` | must be `true` whenever `test-count` is 0 |
| `exclusions[].reason` | one of the **named mechanical** reasons below |

### Named mechanical exclusion reasons

No free-text rejection is accepted. Every exclusion MUST use one of:

- `missing-lane-artifact`
- `incompatible-scheduler`
- `incompatible-config`
- `inventory-mismatch`
- `artifact-corrupt`
- `artifact-expired`
- `non-unique-sha`

## Regeneration (`--check`)

After GitHub's seven-day artifact retention expires, a reviewer reproduces a
cohort report byte-for-byte from the manifest alone:

```bash
# manifest retained here; report JSON retained alongside
racket scripts/run-tests/cohort-report.rkt \
  --manifest artifacts/ci-baseline/cohort-<milestone>-<n>.json \
  --out-json <path-to-stored-report.json> \
  --check
# exit 0 ⇒ byte-identical regeneration confirmed
```

Determinism: identical manifest inputs produce byte-identical report outputs.
All ordering is by explicit keys; nothing host- or time-dependent is ever
embedded. The manifest digest (`report → manifest-digest`) is a change-
detection checksum over the canonical JSON form; per-SHA `inventory-digest`
fields are the authoritative per-SHA digests.

## Bounded raw inputs

Retained raw inputs are bounded and named by milestone/cohort. No external
database or service is queried at report time. The maintainer-run retention
commands (documented in `baseline-report.rkt`) fetch GitHub run/job summaries
**before** report generation; the cohort manifest is then authored from those
retained inputs and checked in here.
