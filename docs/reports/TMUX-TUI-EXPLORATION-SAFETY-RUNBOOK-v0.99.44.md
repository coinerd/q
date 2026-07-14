# tmux TUI Exploration Safety Runbook — v0.99.44 (v0.99.50 correction)

**Original date:** 2026-07-04

**Operational contract corrected:** 2026-07-13

## Overview

This runbook defines the executable safety contract for mock and explicit
real-provider tmux TUI exploration. The v0.99.50 correction supersedes obsolete
environment-variable, suite, redaction, and helper-field claims from the
original v0.99.44 text.

The governing rule is: **protocol evidence over plausible prose**. Prompt
echoes, generic model text, stale events, and mock-provider output cannot make a
real scenario PASS.

## 1. Authorization and prerequisites

Real mode requires all three acknowledgement gates with these exact values:

```text
Q_TMUX_TUI_TESTS=1
Q_TMUX_TUI_REAL_PROVIDER=1
Q_TMUX_TUI_REAL_PROVIDER_CONFIRM=I_UNDERSTAND_COSTS
```

It also requires:

- `tmux` and `racket` on `PATH`;
- an explicit source HOME in `Q_TMUX_TUI_REAL_PROVIDER_HOME`;
- at least one recognized q file under that HOME's `.q/` directory:
  `config.json`, `credentials.json`, or `config.rkt`.

The three acknowledgement variables authorize cost-bearing execution. They do
**not** validate credentials or provider availability. The executor validates
the source HOME and recognized q files before starting tmux.

No API key value belongs in a command argument, report, pane summary, or shell
history. Legacy non-TUI opt-in/key variable names are not part of the
executable contract.

## 2. Isolation and credential handling

Every real execution creates a fresh workspace containing:

```text
q-tmux-real-<unique>/
  home/                 # temporary HOME
    .q/                 # recognized config copied without displaying contents
  project/              # isolated fixture project
  launch.sh             # contains paths and command only, never credential contents
```

The production executor in `scripts/tmux-explore/executor.rkt`:

1. creates the destination HOME and project;
2. copies only recognized files from the explicit source HOME;
3. starts one uniquely named `q-explore-*` tmux session;
4. sends the registered scenario prompt;
5. waits for structured `turn.completed` or `stream.turn.completed` evidence;
6. writes only redacted capture/trace artifacts to the requested output root;
7. uses `dynamic-wind` to stop the named session and delete the
   copied-credential HOME on success, timeout, exception, and interruption.

The older test helper `make-real-provider-tmux-env` returns a `tmux-env`; its
actual HOME field accessor is `tmux-env-home`. Its companion
`copy-q-config-to-temp-home!` is an explicit second step and may copy
`config.json`, `credentials.json`, and `config.rkt`.

## 3. Narrow tmux cleanup

`tmux kill-server` is forbidden. Cleanup uses only:

```text
tmux kill-session -t <generated-q-explore-session>
```

The executor never enumerates and kills unrelated sessions. The generated
session name is retained as non-secret metadata so cleanup and artifact review
can be correlated.

## 4. Redaction and retained artifacts

`util/credential-redaction.rkt` is the canonical policy used by the explorer,
harness, and report scanner. It avoids known false positives while retaining
leak detection:

- `set-task-state` and `risk-score` are clean;
- `Bearer authentication` is clean;
- existing `<REDACTED>` placeholders are clean and idempotent;
- realistic long `sk-` API keys, Bearer/JWT tokens, and key assignments are
  redacted and detected;
- a mixture of placeholders and a real secret still fails leak scanning.

Real executor artifacts are written outside the credential workspace:

```text
<tag>-capture.txt       # redacted bounded pane capture
<tag>-trace.jsonl       # redacted structured events
<tag>-execution.txt     # session name, terminal status, event count
<tag>-execution-error.txt  # redacted error text, when startup/execution fails
```

The copied-credential HOME is deleted after artifacts are emitted. Historical
`write-failure-artifacts!` output from the test harness is a different API: its
raw capture is diagnostic and must not be assumed universally redacted. Never
publish a retained artifact without running the report scanner.

## 5. Registered scenarios and semantic requirements

The exact registered tags are:

| Tag | Required structured evidence |
|---|---|
| `memory` | correlated retrieval event with result presence and terminal completion |
| `gsd` | ordered attempted/succeeded transition with matching transition ID |
| `mas` | matching approval, tool-call, child, result, turn, and terminal completion |
| `tools` | ordered started/completed tool events with matching tool/call IDs and result |
| `release-audit` | structured authorization-refused event and terminal completion |
| `durable-memory` | ordered matching store, resume, retrieval, and completion lifecycle |
| `resume` | matching resumed session/previous-session IDs and completion |
| `compact` | terminal persisted compaction event with truthful before/after counts |

Every scenario verifier rejects missing traces, timeout, crash, mock-provider
fallback, unrelated IDs, missing results, and absent terminal completion.

Mock mode is deterministic and no-network. Its evidence is explicitly labeled
mock and is never eligible for a real PASS.

## 6. Running the explorer

### Mock mode

```bash
racket scripts/tmux-tui-explore.rkt --mode mock
racket scripts/tmux-tui-explore.rkt --mode mock --filter tools
racket scripts/tmux-tui-explore.rkt --list
```

### Real mode: one cost-bounded scenario

```bash
Q_TMUX_TUI_TESTS=1 \
Q_TMUX_TUI_REAL_PROVIDER=1 \
Q_TMUX_TUI_REAL_PROVIDER_CONFIRM=I_UNDERSTAND_COSTS \
Q_TMUX_TUI_REAL_PROVIDER_HOME="$HOME" \
racket scripts/tmux-tui-explore.rkt --mode real --filter tools
```

If any selected real scenario is not PASS, the command exits nonzero. For a
manual diagnostic run that should retain reports without acting as a gate, add
the explicit `--non-gating` option:

```bash
racket scripts/tmux-tui-explore.rkt --mode real --filter tools --non-gating
```

Authorization variables are still required with `--non-gating`.

## 7. Test suite policy

There is no selectable `tui-tmux` or `tui-tmux-real` suite in
`scripts/run-tests.rkt`. Relevant executable suite commands are:

```bash
racket scripts/run-tests.rkt --suite fast
racket scripts/run-tests.rkt --suite smoke --profile local
racket scripts/run-tests.rkt --suite tui
```

`Q_TMUX_TUI_TESTS=1` controls whether opt-in tmux scenarios execute; it is not a
suite selector. Slow/e2e metadata controls normal test inventory selection.
Real-provider exploration remains manual and is not a default CI blocker.

Focused W3 gates:

```bash
raco test tests/test-secret-redaction.rkt
raco test tests/test-tmux-tui-explore.rkt tests/test-tmux-explore-verifiers.rkt
raco test tests/test-tmux-explore-executor.rkt tests/test-tmux-q-harness.rkt
raco test tests/test-tmux-tui-report.rkt tests/test-tmux-real-provider-docs.rkt
Q_TMUX_TUI_TESTS=1 racket scripts/tmux-tui-smoke.rkt
```

## 8. Evidence and failure interpretation

The visible `q>` prompt is readiness evidence, not turn-completion evidence.
The executor waits for structured completion in `trace.jsonl`. Pane text is
retained only as supporting, redacted evidence.

A real result is non-PASS when any of the following occurs:

- startup or provider failure;
- timeout or session crash;
- missing/malformed trace;
- mock-provider fallback;
- no correlated scenario lifecycle;
- mismatched turn, sequence, approval, tool-call, child, session, or result ID;
- no terminal completion.

Use the report scanner for retained harness bundles:

```bash
racket scripts/tmux-tui-report.rkt
racket scripts/tmux-tui-report.rkt --dir /path/to/artifacts
```

Explorer `--append-log` appends a Markdown ledger. Harness centralized logging
is a separate JSONL protocol; the two formats must not be conflated.

## 9. CI policy

Mock/focused tests may run in CI. Real-provider exploration should remain an
explicit manual operation because it is cost-bearing, credential-bearing, and
provider-variable. Never store copied credentials, raw credential-bearing pane
captures, or a temporary real-provider HOME as CI artifacts.
