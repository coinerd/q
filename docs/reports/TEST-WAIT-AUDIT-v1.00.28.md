# Test Wait Audit — v1.00.28 Wave W1

Scope: every real `sleep`, timeout wait, retry/backoff, polling loop, and
process-settle delay surfaced by the W0 static scan plus targeted greps in the
`unit-fast` and `fast` suites (greps: `sleep`, `sync/timeout`, `alarm-evt`,
`thread-wait`, `subprocess-wait`, `current-seconds`, `current-milliseconds`,
`retry`, `backoff`, `poll`, `timeout`, `wait`).

Machine-readable companion: `artifacts/test-runtime/v1.00.28-w1/wait-audit.json`
(row ids below match `audit_id` there).

## Classification and dispositions

| ID | Occurrence | Class | Disposition |
|----|-----------|-------|-------------|
| R1 | `tests/test-agent-session-basic.rkt` raw `(sleep 0.1)` before session-cycle assertions | A (logical timing) | Remediated: session steps now run through `make-fake-sleeper` from `tests/helpers/deterministic-clock.rkt`; the requested delay is a parameter of the fake session and the assertion checks the *emitted delay*, not elapsed wall time. |
| R2 | `tests/test-auto-retry.rkt` five raw `(sleep 0.02)` calls driving retry/backoff timing | A (logical timing) | Remediated: fake sleeper injected; the test asserts the ordered `(backoff delay-ms ...)` events and `('sleep n)` receipts captured by the fake, with zero wall-clock cost. |
| R3 | `tests/test-retry-iteration.rkt` three raw `(sleep 0.02)` calls synchronizing retry run-state | A (logical timing) | Remediated: fake sleeper via `make-fake-clock`; sync-loop waits on the fake instead of the wall clock. |
| R4 | `tests/helpers/deterministic-clock.rkt` `(sleep 0.01)` inside `force-clock!` self-test | D (real-clock canary) | Retained with named reason: the helper itself must prove that real-time mode still advances wall time (`force-clock!` contract); this is the only real sleep remaining in `unit-fast` and is declared in the helper self-test and wait-audit row R4. |
| R5 | Production seams (`sync/timeout`, `current-seconds`, retry/backoff math) referenced by the audited tests | A (logical timing, production side) | No production change required: the existing injectable seams are consumed through `make-fake-clock` / `make-fake-sleeper`; tests assert requested logical delays, retry counts, ordering, and timeout transitions deterministically. |
| R6 | `thread-wait` / `subprocess-wait` / `poll` in `unit-fast`/`fast` test bodies | none found | Targeted greps returned no test-body occurrences; no class B/C remediation needed. Text matches were treated as evidence, never automatic defects. |
| R7 | `fast`-suite process/TUI end-to-end waits outside `unit-fast` | D (real-clock canary) | Retained outside `unit-fast` with named behavioral reasons (real subprocess/process lifecycle canaries); unchanged by this wave. |

Rule applied: raw `sleep` in `unit-fast` is permitted only as an audited,
named class-D canary (R4). All class-A occurrences run on the deterministic
clock/sleeper seam.

## The lint gate (failing test first)

`tests/test-deterministic-clock.rkt` contains the wait-audit lint: every
`unit-fast` file containing a raw `sleep` must either (a) carry an audit row
with a class and named disposition, or (b) be remediated. A new unjustified
raw sleep in `unit-fast` fails the check, and every retained real-clock test
must declare its behavioral reason (helper self-test declaration + audit row).

## Measurement contract

`benchmarks/` holds ≥10-sample manifests per remediated family, identical
method both sides (`racket tests/<family>.rkt`, sequential successful runs,
wall-clock ms):

| Family | Before (mean, n=10) | After (mean, n=10) | Reading |
|--------|--------------------|--------------------|---------|
| test-auto-retry | 5976.8 ms | 6503.3 ms | No speedup claimed; means sit inside run-to-run variance. The remediation gain is determinism (five real sleeps removed), not wall time. |
| test-retry-iteration | 181.9 ms | 177.9 ms | Within noise; no speedup inferred. |

Before samples were taken from a detached worktree of `origin/main`
(04637d83) with the identical invocation; both sides are checksummed in
`artifacts/test-runtime/v1.00.28-w1/SHA256SUMS`. No family is INCOMPARABLE.

## Residue note

Untracked leftovers from an older browser-audit/wave3 campaign
(`tests/test-browser-audit-w1-v0984.rkt`, `tests/test-browser-audit-w2-v0983.rkt`,
and three stray JSONs in `benchmarks/`) were present in the working tree and
are removed by this wave to keep the suites clean; they are not v1.00.28 W1
targets.

## Verdict

Zero unjustified real sleeps remain in `unit-fast`. Every retained real-clock
test has a named behavioral reason (R4, R7). Both remediated families have
retained before/after evidence. Production code is untouched by this wave.
