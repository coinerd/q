# Test Regression Triage Protocol

Operational protocol for responding to full-regression results from the
scheduled/manual workflow (`.github/workflows/full-regression.yml`). This doc
covers the four regression events that require an explicit, bounded response.
This is operational documentation, not a test-reporting contract.

Governance: the canonical test-strategy governance document is
[`docs/TDD-TEST-STRATEGY-PLAN.md`](../TDD-TEST-STRATEGY-PLAN.md) (adopted in v1.00.04;
supersedes PR #9348) — see its "Adoption status" section for how
the full-regression workflow is wired into the L4 contract.

**Release linkage (binding rule):** release readiness depends on the
`full-regression` workflow's evidence — a scheduled run's summary or a fresh
manual `workflow_dispatch` run — **never** on a successful `fast` gate run
alone. A green `fast` gate proves nothing about the slow/full/profile-aware
suites; those are only proven by this workflow's retained artifacts
(`run-summary.json`, `matrix-summary.json`, per-shard JSON, failure logs).

## Event 1 — Green PR gate, red full regression

A PR merged with a green fast gate, then the full regression fails.

**Required within one working day:**

1. Open (or update) a regression issue referencing the failing run URL, with:
   - the test file(s) that failed,
   - the failing shard's evidence (artifact `results-shard-<N>`:
     `shard-<N>.json` + `test-output.log`),
   - the execution profile (from `run-summary.json` → `profile`),
   - the runner mode (from `run-summary.json` → `runner_mode`),
   - whether an isolation rerun (re-running the failing test alone) changed
      the result.
2. Explicitly assess whether impact selection (the fast gate's impact
   analysis) missed a relevant test: was the failing test logically dependent
   on the changed code but not selected? Record the assessment in the issue
   either way (miss or no-miss) — a miss is an impact-selection bug that gets
   its own follow-up issue.

## Event 2 — Timeout

A shard (or test) hits its timeout. A timed-out run has status `timeout` in
`run-summary.json` — it is never folded into `passed`, and relabeling a
timeout as success is prohibited.

1. **Preserve evidence first.** The workflow uploads the timed-out shard's
   partial JSON (`upload-artifact` runs with `if: always()`), so test
   progress before the timeout is retained. Do not rerun-and-overwrite the
   evidence before triage.
2. **Classify the bottleneck** in the regression issue: which tests were
   in-flight at the timeout, per-test durations up to that point, and the
   category — slow-but-passing suite growth, a hang (no output progress), or
   a genuine timeout-under-load regression.
3. **Define a bounded remediation experiment:** a specific, time-boxed change
   (e.g., splitting the slowest test file, fixing the hang, or adjusting the
   shard budget) with a measurable success criterion, then a fresh
   `workflow_dispatch` run to verify. "Raise the timeout until it passes" is
   not remediation.

## Event 3 — Recurring flake

The same test fails intermittently across runs.

1. **Minimal reproduction:** reduce the flake to the smallest
   command/sequence that reproduces it (including environment factors —
   parallel mode, load, platform).
2. **Isolation/environment fix:** fix the root cause (shared state, port
   collision, timing assumption), not the symptom.
3. **Repeated-execution verification:** before removing a flake marker, the
   fix must be verified by repeated executions of the affected suite (via
   `scripts/run-tests/run.rkt` — its repeated-execution mode) showing stable
   green across enough repetitions to cover the observed flake rate.
4. **Quarantine with expiry:** while the flake is being fixed, its
   known-failure ledger entry carries an `expires_on` date. After that date
   the entry **escalates**: the failing test is reported as a failure with
   `escalate: true` / `quarantine_expired: true` in the shard JSON instead of
   being tolerated. Quarantine is a temporary state, not a backlog you can
   park a failure in.

## Event 4 — Unavailable scheduled run

The nightly schedule did not produce a run (no `run-summary.json` evidence
for the latest commit).

1. Release readiness escalates to a **fresh manual dispatch** of
   `full-regression` on the release commit — with the profile used for the
   release recorded in the dispatch inputs.
2. Never silently proceed on stale evidence: if the freshest full-regression
   summary predates the release candidate's merge commit, the release is
   blocked until a fresh run (scheduled or manual) covers it.

## Quick reference

| Event | First action | Deadline | Prohibited |
|---|---|---|---|
| Green PR → red full | Regression issue w/ artifacts | 1 working day | Ignoring because the PR gate was green |
| Timeout | Preserve shard JSON evidence | — | Relabeling as success |
| Recurring flake | Minimal reproduction | Expiry date on ledger entry | Unexpiring quarantine |
| Missing scheduled run | Fresh manual dispatch | Before release | Proceeding on stale evidence |
