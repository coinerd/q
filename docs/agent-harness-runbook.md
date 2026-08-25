<!-- verified-against: 1.00.17 -->
# Agent Harness Runbook

**Scope:** Operating manual for the q-agent self-hosting harness — how to run
gates, interpret process exits, and avoid the failure modes found in the
W0-W2 waves of the Agent Harness Reliability milestone.
**Established:** v0.99.77 W3 (F-20 exit-137 misattribution, background-gate
pattern)
**Applies to:** anyone running the q test suite, CI-local gates, or long
scripts on the harness (VPS or workstation).

---

## 1. Gates and Long Commands MUST Run in the Background

A gate (fast suite, broad suite, full suite, `lint-all`, release dry-run) or any
command that plausibly exceeds a few minutes **MUST** run in the background.

### The sanctioned pattern

```bash
cd /path/to/q
nohup racket scripts/run-tests.rkt --suite fast --jobs 4 > /tmp/gate-<name>.log 2>&1 &
echo "pid $!"
# then POLL the log, never block on the foreground command:
tail -f /tmp/gate-<name>.log        # interactive follow
grep -c '\.' /tmp/gate-<name>.log   # progress dot count
ps -p <pid> -o pid,stat,etime       # is it still alive?
```

### Why

- A foreground long-running command ties up the agent loop; a provider or
  network hiccup then kills the session instead of the gate.
- The harness's own `timeout N` wrapper converts long runs into "user break"
  aborts that leave no log evidence. Background + poll keeps the evidence.
- Background gates survive context compaction, machine switches, and
  mid-run interruptions; the log is the single source of truth when resumed.

### Polling protocol

1. Start with `nohup ... > log 2>&1 &`; record the PID.
2. Poll at reasonable intervals (not tighter than every few minutes for a
   full fast gate, which is ~1h40m).
3. Wait for the `VERDICT:` line in the log (grep for it) — do not rely on
   the process being alive (a zombie or orphaned child can linger).
4. Only then interpret the result (see §2 for exit-code interpretation).

---

## 2. Exit 137 Means SIGKILL — Not OOM

Exit code 137 = 128 + 9 = **SIGKILL**. It means the process was killed by
`kill -9`, a timeout watchdog, or an OOM-killer — and on the harness it is
**usually a timeout/kill**, not memory pressure.

### Interpretation rules

- **Do NOT assume OOM** when a command exits 137. First check for a
  surviving `T`-state (stopped) child:
  ```bash
  ps -eo pid,stat,comm | grep -E '^ *[0-9]+ T '     # stopped processes
  ps -eo pid,stat,ppid,etime,comm | grep -i racket   # racket children
  ```
- A `T`-state (SIGSTOP'd) child is the classic harness hang: the parent
  waits forever, the timeout fires SIGKILL at the parent (137), and the
  stopped child survives as a zombie/orphan. See §3 for the W1 fix.
- Only treat 137 as OOM when you have explicit evidence: `dmesg | grep -i oom`
  or `/var/log/kern.log` entries naming the process, or the log ends with a
  memory-related abort.
- 137 is distinct from 124 (`timeout` utility's own kill), 143 (SIGTERM,
  128+15), and 130 (SIGINT, 128+2). Record the exact code in the gate log.

### Checklist on any 137

```text
1. Read the tail of the log for the last successful test line.
2. ps -eo pid,stat,ppid,etime,comm | grep racket  → any T-state children?
3. If yes: kill them (kill -9 <pid>) and note the file that hung.
4. dmesg / kern.log only if a T-state child was NOT found.
5. Record verdict: timeout-kill (most common) vs OOM (rare, evidenced).
```

---

## 3. Post-W1 Timeout Behavior

W1 (issue #9110, PR #9115) fixed the run-subprocess timeout path:

- **Before W1:** a timeout invoked `(subprocess-kill sp)` with the wrong
  arity, the error was swallowed by `with-handlers`, **no signal was
  delivered**, and SIGTERM was not delivered to SIGSTOP'd (T-state) children
  anyway. The harness froze until the external wrapper SIGKILLed it (137).
- **After W1:** the timeout path runs a two-phase kill:
  1. Phase 1 — SIGTERM to the direct child **and** its process group
     (`#:process-group? #t` launches under `setsid` when available).
  2. 2-second grace period.
  3. Phase 2 — SIGKILL to the direct child **and** its process group.

### Consequences

- **A foreground timeout now returns a result** (a timed-out outcome) instead
  of hanging. It is safe to run a single subprocess invocation in the
  foreground for debugging.
- **But background remains the sanctioned pattern for gates.** A gate is many
  subprocesses; even with correct per-subprocess kill, a multi-minute gate in
  the foreground still ties up the session and is one network hiccup away from
  losing the run. The W1 fix makes foreground *safe*, not *preferred*.
- Process-group semantics: a `setsid`-launched child is its own group leader
  (PGID == PID), so the group kill reaches all grandchildren. On platforms
  without `setsid` (e.g. macOS), the fallback is a direct-child
  SIGTERM → SIGKILL; grandchildren may survive — check with `ps` and clean up.

### Rules of thumb

| Situation | Sanctioned pattern |
|-----------|--------------------|
| Gate (fast/broad/full), lint-all, release dry-run | **Background** `nohup ... &` + poll |
| Single subprocess debug invocation | Foreground OK (post-W1) |
| Any run that must survive compaction/switch | **Background** |
| Interpreting a result | Wait for `VERDICT:` line, then §2 checklist |

---

## 4. The Background-Gate Rule for Resumed Sessions

A session that resumes after compaction or a machine switch **MUST** assume
any gate it started was running in the background and may still be in flight:

1. Check the gate log for a `VERDICT:` line before relying on its result.
   A log without a verdict is in-flight or dead — do not treat it as green.
2. Check for surviving racket processes (`ps -eo pid,stat,etime,comm |
   grep racket`) before starting a duplicate gate. Two gates on the same
   suite in parallel produce misleading load-dependent flakes.
3. If a stale background gate has no verdict and no live process, discard
   the log and restart fresh.
4. Never run a broad gate in the foreground just because it "should be
   quick" — the fast suite alone is ~1h40m.

This rule is mirrored in the `.planning/RESUME-PROMPT-*.md` files so that
resumed sessions receive it as part of their prompt.

---

## 5. Related Artifacts

| Artifact | Purpose |
|----------|---------|
| `scripts/run-tests.rkt` | Parallel test runner; gate entry point |
| `sandbox/subprocess.rkt` | Timeout kill path (W1 fix: SIGTERM→SIGKILL, process-group) |
| `scripts/run-tests/classify-filters.rkt` | Serialization of mutation-sensitive / subprocess-isolation files (W2 fix) |
| `docs/gsd-process-governance.md` | Process rules for milestones/waves/releases |
| `.planning/RESUME-PROMPT-*.md` | Per-session resume prompts incl. background-gate rule |
