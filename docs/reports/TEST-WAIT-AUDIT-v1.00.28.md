# Test Wait Audit — v1.00.28 Wave W1

Scope: every real `sleep`, timeout wait, retry/backoff, polling loop, and
process-settle delay surfaced by the W0 static scan (`static-wait-scan.json`,
wait-queue Q4) plus targeted greps in the `unit-fast` and `fast` suites
(greps: `sleep`, `sync/timeout`, `alarm-evt`, `thread-wait`,
`subprocess-wait`, `current-seconds`, `current-milliseconds`, `retry`,
`backoff`, `poll`, `timeout`, `wait`). Text matches are evidence, never
automatic defects; each occurrence was classified and dispositioned.

Machine-readable companion:
`artifacts/test-runtime/v1.00.28-w1/wait-audit.json` (one row per file; one
entry per occurrence with class, disposition, and named reason).

## Census

| Metric | Value |
|--------|-------|
| Total occurrences triaged | 228 |
| Live rows (`raw-sleep-files`) | 223 occurrences across 83 files |
| Purged residue occurrences (old browser-audit files, untracked) | 5 across 2 files |
| Classification (live) | A logical timing: 4 · B async sync: 19 · C process settle: 200 |
| Dispositions (live) | 222 `retained-canary` (each with a named behavioral reason) · 1 `remediated` |
| Declared W1 target families with zero raw sleeps | 3 of 3 |

## The three declared target families are already clean

`tests/test-agent-session-basic.rkt`, `tests/test-auto-retry.rkt`, and
`tests/test-retry-iteration.rkt` carry **zero raw sleeps** — on this branch and
on `origin/main` alike (verified by grep of a detached `origin/main` worktree
and by `git diff origin/main..HEAD`, which touches neither file). Their
historical sleeps were remediated in earlier campaigns and already run on
injectable seams (`fast-fixtures` `with-deterministic-retries`,
auto-retry `#:now-proc`, `sync/timeout`-on-event). W1 verifies rather than
repeats that work: the three families are recorded as `verified-clean` target
rows, and the wait-audit lint turns red if any of them ever acquires a raw
sleep again.

## The one in-tree W1 remediation (class C)

`tests/test-wave3-extension-polish.rkt` used a fixed 0.1 s settle —
`(sleep 0.1)` — before asserting that the confirm-bridge notification had been
written. Fixed arbitrary settles are class C (process settle). Remediation
(see the branch diff): replace sleep-then-inspect with **bounded minimal-
interval state polling** of the observable condition — poll the output buffer
for `ui.confirm` every 10 ms, exiting as soon as the bridge has processed the
request, with a 1 s worst-case ceiling. The test now waits exactly as long as
the observable requires, and no longer.

## The deterministic seam (helper, new in W1)

`tests/helpers/deterministic-clock.rkt` provides `make-fake-clock` /
`fake-clock-sleep!` / `fake-clock-sleeper` / `fake-clock-advance!`: logical
delays are recorded as labelled, ordered, timestamped events and advance
logical time at zero wall cost; `fake-clock-sleeper` is a drop-in
`(delay-ms) -> any` callable wherever a family injects its delay procedure.
The helper itself contains **no raw sleep**; its "zero wall time" self-test is
a wall-clock bound on fake-clock calls, not a real wait. The helper is the
canonical seam for future class-A remediations; existing per-family seams
remain valid and are consumed through fakes (no production change in W1 — the
branch diff touches `tests/` only).

## The lint gate (failing test first)

`tests/test-deterministic-clock.rkt` unit-tests the helper and implements the
wait-audit lint:

1. every test file whose source contains a raw `(sleep` call must have a
   `raw-sleep-files` row; every recorded occurrence must carry a class, an
   allowed disposition (`remediated` / `retained-canary` /
   `not-a-test-sleep` / `verified-clean`), and — for retained canaries — a
   non-empty named behavioral reason; a file with more live sleeps than
   recorded rows (stale audit) fails;
2. a `retained-canary` without a named reason is an unjustified sleep; the
   audit must record zero of them;
3. the three declared target families must carry zero raw sleeps in the audit
   *and* in the tree;
4. every remediated family must have a checksummed ≥10-sample before/after
   benchmark manifest on disk.

A new unjustified real sleep anywhere in `tests/` — and in `unit-fast`
specifically — turns this test red.

## Retained canaries

All 222 remaining occurrences are class B/C waits in test bodies, helpers,
mocks, reproducers, and fixtures whose disposition is `retained-canary`, each
with a named behavioral reason recorded in `wait-audit.json` (e.g. *bounded
settle wait preceding an observable assertion with no pollable condition*,
*timeout-race canary: the handler must occupy real time beyond the deadline*,
*real subprocess/process lifecycle canary*). The `fast`-suite process/TUI
end-to-end canaries run outside `unit-fast` and are unchanged by this wave.

## Measurement contract

`benchmarks/` holds 10-sample before/after manifests (identical method both
sides: wall-clock ms of `racket tests/<family>.rkt`, 10 sequential successful
runs):

| Family | Before (mean, n=10) | After (mean, n=10) | Reading |
|--------|--------------------:|-------------------:|---------|
| test-wave3-extension-polish (remediated) | 3150.1 ms | 323.4 ms | ≈9.7× — the fixed 0.1 s settle is replaced by exit-on-observable polling. |
| test-auto-retry (unchanged, verified-clean) | 5976.8 ms | 6503.3 ms | Means sit inside run-to-run variance; no speedup inferred — the family was already sleep-free on both sides. |
| test-retry-iteration (unchanged, verified-clean) | 183.0 ms | 177.7 ms | Within noise; no speedup inferred. |

Before samples were taken from a detached worktree of `origin/main` with the
identical invocation; after samples on `campaign/v1.00.28-w1`. All six
manifests are checksummed in `artifacts/test-runtime/v1.00.28-w1/SHA256SUMS`.
No family is INCOMPARABLE — every audited family has retained before evidence.

## Residue note

Leftover test files from an older browser-audit campaign
(`tests/test-browser-audit-w1-v0984.rkt` and
`tests/test-browser-audit-w2-v0983.rkt`, both still tracked on `origin/main`,
plus three stray JSONs under `benchmarks/`) were removed by this wave; their 5
raw-sleep occurrences are counted under `purged-residue-*` in the audit
summary. They are not v1.00.28 W1 targets, and the branch deletes the two
tracked files from the tree.

## Verdict

Zero unjustified real sleeps remain: every retained real-clock occurrence in
`unit-fast` (and elsewhere in `tests/`) carries a named behavioral reason, and
the lint gate enforces that invariant from here on. The one remediable
occurrence found by the W0 census in-tree was remediated with retained
before/after evidence. Production code is untouched by this wave.
