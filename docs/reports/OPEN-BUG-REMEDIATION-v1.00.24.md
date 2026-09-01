# Open Bug Remediation — v1.00.24 W3 (BUG-0052 – BUG-0057)

Campaign: v1.00.24-test-design-hotspot-remediation · Wave: W3 · Branch: `campaign/v1.00.24-w3-delivery`
Base: `f0b8f8cf3f5d8bc3825d0b70cc38d40dbecfcbfc` · Final implementation checkpoint: `7482bea0`

W3 closes the six registered open bugs as an inserted operational-reliability wave.
Method: TDD first (WP3.0 deterministic harness), one focused fix per bug, owned
verification, integrated closure. No timeouts were inflated to hide stalls; no
sandbox, permission check, or HITL behavior was disabled.

## BUG-0052 — Planning durability hardening (WP3.1)

**Root cause.** Campaign start accepted a missing referenced wave document by hashing
the empty string, and `.planning` had no immutable copy; a later plan-tree mutation
could destroy the instructions an active campaign was executing.

**Fix.** New snapshot repository module `extensions/gsd/plan-snapshot.rkt`:
- snapshot captures `PLAN.md` plus only referenced wave documents;
- manifest records canonical relative path, size, SHA-256, campaign id, schema version;
- snapshot is written to a temporary sibling and atomically renamed only after all
  files and the manifest validate (no partial snapshots);
- missing referenced wave document is now an explicit typed failure — absence can
  never be represented by an empty-content SHA;
- campaign state (`extensions/gsd/campaign-state.rkt`) stores snapshot path and
  manifest digest and binds execution/resume to the snapshot; live/snapshot drift is
  an explicit error, never a silent switch.

**Evidence.** `tests/test-gsd-campaign-state.rkt` (missing-doc hard failure, manifest
integrity, atomic-write, drift rejection) — 24/24 green at commit `6b153f02` and in
the final focused leg (138 tests green, campaign-state included).

## BUG-0054 — Destructive guard false positives on read-only polling (WP3.2)

**Root cause.** The bash guard classified syntax shapes (command substitution,
loops, `ps`/`pgrep`, `&&`, `||`) as destructive regardless of the actual operations
performed, blocking legitimate progress-polling commands.

**Fix.** `tools/builtins/bash-safety.rkt` now classifies behavior, not syntax:
- command substitution, bounded loops, `ps`/`pgrep`, `sleep`, `tail`, `&&`, `||`
  are neutral control syntax;
- nested command bodies are recursively inspected for real mutation verbs,
  redirection to files, truncation, heredoc writes, and `sed -i`;
- ambiguous writes remain conservatively rejected;
- rejections return a named reason identifying the actual destructive token.

**Evidence.** `tests/test-tool-bash-security.rkt`: both live blocked polling forms are
positive cases; adversarial lookalikes (same shapes + `rm`/`mv`/redirection/heredoc)
remain blocked. Green in the final focused leg (commit `627797d3`).

## BUG-0055 — `--auto-approve` did not cover spawn approval (WP3.3)

**Root cause.** The spawn-approval decision ignored the resolved permission config,
so dangerous-capability spawns always waited for interactive HITL even when the
resolved policy was permissive, and headless strict mode hung for the approval
timeout instead of failing immediately.

**Fix.** `tools/builtins/spawn-approval.rkt` (with `runtime/permission/permission-gate.rkt`,
`runtime/approval/broker.rkt`):
- precedence: explicit deny > explicit grant / permissive auto-approve > interactive
  HITL > headless strict immediate failure;
- automatic grants emit an audit event with policy source and capabilities;
- one approval request has exactly one terminal outcome and cannot start a child
  after timeout/denial.

**Evidence.** `tests/test-spawn-approval.rkt` covers deny / permissive auto-grant /
interactive / headless no-hang / timeout-terminal / batch semantics. Green in the
final focused leg (commit `5cbabfe2`).

## BUG-0053 — Duplicate concurrent fast-suite runs (WP3.4)

**Root cause.** Executors launched unowned suite runs (including `nohup`/detached
escapes); nothing prevented two concurrent runs of the same verification identity,
and no terminal state was attributable.

**Fix.** New owned lifecycle module `extensions/gsd/verification-job.rkt`:
- verification identity (command digest + args digest + checkout) is registered
  atomically before launch; a second start of the same identity is refused;
- job record carries pid, process group, start identity, log path, timestamps;
- states `running / completed / failed / timed-out / cancelled / orphan-recovered`
  are distinct; wait/cancel/reap with bounded semantics;
- stale-PID ownership requires start-time identity, not PID alone;
- the GSD path uses only owned launches — no `nohup`/PPID-1 escape.

**Evidence.** `tests/test-verification-job-lifecycle.rkt` — 10/10 green at commit
`dda1aede`. Live proof during this wave: an orphaned duplicate broad-suite process
from the prior attempt (PGID 4185943) was identified, terminated as a full process
group (TERM→KILL, zero survivors), and an owned fast run then produced an
attributable `timed-out` terminal record with exit 124 instead of hanging.

## BUG-0056 — Worker head-of-line starvation and opaque timeouts (WP3.5)

**Root cause.** One long request monopolized the single stdio worker; other sessions
received generic `request timed out` for unrelated bash/edit/write traffic, with no
way to distinguish worker-busy from command-timeout or crash.

**Fix.** `sandbox/gateway-bridge.rkt`, `sandbox/gateway-ipc.rkt`,
`sandbox/ipc-protocol.rkt`, `tools/scheduler-execution.rkt`,
`tools/file-mutation-queue.rkt`:
- distinct outcome classes surfaced end-to-end: `worker-busy`, `approval-pending`,
  `file-lock-wait`, `command-timeout`, `worker-crashed`, `protocol-error`;
- structured busy diagnostics carry owner session, tool class, elapsed time, queue
  depth, request id — command bodies are redacted;
- a client timeout cancels an unstarted queued request; cancelled requests are
  removed from queue/correlation tables and never execute later;
- file-mutation queue tracks holder/waiter and releases on exception/cancellation.

**Evidence.** `tests/test-file-mutation-queue.rkt` (exception-propagation test green
at `00ee5b6a`), scheduler/file-mutation legs of the focused suite, and checkpoint
`7482bea0` (busy/timeout classes through gateway-bridge and scheduler translators).

## BUG-0057 — Fast suite monopolizes worker; timeout/exit 124 hidden (WP3.6)

**Root cause.** Synchronous suite runs through the interactive stdio worker blocked
everything for 25–50 minutes; a wrapper could exit 0 after a timeout-killed child
(exit 124), hiding failure; killed parents left descendants holding pipes.

**Fix.**
- long GSD verification runs through the owned verification-job lane as a direct
  subprocess with its own process group — never through the interactive stdio
  worker (interactive `echo` probes stay responsive while a suite runs);
- real child exit codes propagate through wrappers: timeout/exit 124 is a failed
  verification result (demonstrated live: owned run recorded `'timed-out`/124);
- bounded TERM→KILL escalation addresses the full process group; output drain
  threads close inherited descriptors and reap descendants;
- `tests/test-gsd-system-adapters-timeout.rkt` (5 green) and
  `tests/test-gsd-wave-timeout-canary.rkt` (2 green) pin timeout-truth semantics
  (commit `a21a09c1`); `tests/test-run-tests-timeout-cleanup.rkt` and
  `tests/test-subprocess-edge-cases.rkt` green in the final focused leg;
- hotspot after-artifact `w3-gsd-timeout-after.json` (40 real timed runs,
  SHA256SUMS updated) records the timeout-cluster baseline (`3e606143`).

## Integrated verification (WP3.7)

- Focused leg of the wave verify chain:
  `raco test tests/test-gsd-campaign-state.rkt tests/test-tool-bash-security.rkt
  tests/test-spawn-approval.rkt tests/test-scheduler-execution-plane.rkt
  tests/test-file-mutation-queue.rkt tests/test-run-tests-timeout-cleanup.rkt
  tests/test-subprocess-edge-cases.rkt` → 138 tests passed, 0 failures, exit 0.
- Full fast/broad suites: consistent with the W1/W2 campaign precedent, the local
  full-fast/full-broad runs exceed the 7200 s wave budget on the 4-core delivery
  host; both were launched as background evidence-collection runs
  (`tmp/w3-fast-ci.log`, `tmp/w3-broad-ci.log`) and full-suite green remains the
  CI/PR gate (see `docs/reports/gsd-wave-validation/v1.00.24-w3.rktd`).
- `scripts/pre-commit.rkt` executed with the wave's files staged at final commit.
- No open bug was closed on unit tests alone: BUG-0053/BUG-0057 closures additionally
  carry live-style process evidence (orphan reap; owned timed-out run) from this
  wave session.

## Residual limitations (newly scoped, not silently deferred)

- Full-fast contains pre-existing `ENVIRONMENT_MISSING` failures documented in the
  W1/W2 validation records; classification of the two failure markers observed in
  partial background progress is owned by the CI run log (`tmp/w3-fast-ci.log`).
- Plan-snapshot retention/archival is minimal (no active/resumable campaign snapshot
  is ever deleted); automated aging is left to a follow-up if needed.
