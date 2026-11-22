# CL5 experiment configuration (#9982)

Owner-approved fixed configuration for the CL5 Wave 3 fresh-context agent trials.
Authority for `scripts/cl5/experiment-config.rkt`; the module carries the same values and
`tests/test-cl5-experiment-config.rkt` fails if this document and the module drift.

Status: **configuration fixed; no Wave 3 trial has been executed with it.** This document records
the configuration and the verification of its *effective* use. It is not a measurement report.

## 1. Fixed configuration

| Field | Value |
|---|---|
| provider | `deepseek` |
| model | `deepseek-flash` |
| full id | `deepseek/deepseek-flash` |
| thinking level | `high` |
| tool allowlist | `read,bash,edit,write` |
| pi CLI | `1.6.0` (recorded per series; must match across both arms) |

This supersedes the earlier candidate `deepseek/deepseek-v4-pro`. No environment default is
consulted: the provider, model and thinking level are explicit fields of one config hash passed to
every trial, and a series cannot start without them.

## 2. Arms

| Arm | Revision | Purpose |
|---|---|---|
| baseline | `003e425d2f1206436eb9c0b84353604ce85bb082` | frozen CL0 corpus revision (v1.00.32), no CL1/CL2/CL4 changes |
| intervention | `ec88d4638483a8fb4710ed20872d790d7457a8c6` | post-CL1..CL4 tree; the CL5 base revision |

Both arms run with the identical provider, model, thinking level, tool allowlist and host class;
only the checked-out revision differs. Both revisions are full 40-character SHAs, and the config
rejects a floating ref, a short SHA, or equal arm revisions.

## 3. Comparability rules

- A matched comparison means *same model configuration in both arms*. Earlier CL5 attempts ran on
  `kimi-coding/k3-256k`; those runs are not a matched experiment with this configuration and must
  never be reported as one, merged into one series, or used to compute a delta.
- One pi CLI version and one host class per series; both are stamped into the series metadata and
  a mismatch refuses to extend an existing series.
- The frozen CL0 task corpus, prompts, oracles and the pre-registered decision thresholds in
  `.planning/PLAN-CL5-9982.md` are unchanged by this configuration change.

## 4. Trial preflight (before pi, before any cleanup)

Every trial calls `worktree-preflight` **before** launching a session and **before** the
destructive `git reset --hard` / `git clean -fdx` cleanup:

| Check | Behavior on failure |
|---|---|
| the path exists and is a git checkout | raises; nothing is touched |
| it is a disposable linked worktree (`.git` is a file) | raises for a primary checkout unless `#:allow-primary? #t` is passed explicitly |
| `git rev-parse HEAD` equals the configured arm revision | raises, naming both SHAs — the checkout is **not** reset |
| `git status --porcelain` is empty | raises, listing the changes — the checkout is **not** reset |
| session **and** series-output directories are outside the tree (checked at the `run-series` entrypoint with the same resolved paths used for writing) | raises before any session; nothing is written |

Cleanup repeats the disposable-checkout guard, so a primary checkout is never reset even if a
caller bypassed preflight. Negative cases covered by tests: wrong HEAD, dirty worktree, primary
checkout, output inside the tree, and cleanup on a primary checkout.

## 5. Fail-closed behavior

| Condition | Behavior |
|---|---|
| thinking level not in `off minimal low medium high xhigh max` | raises before any session |
| `full-id` not equal to `<provider>/<model>` | raises |
| arm revision not a full SHA, or arms equal | raises |
| configured model absent from `pi --list-models <provider>` | raises; **no fallback** to another model or provider |
| provider quota / rate limit | reported as a blocker; never worked around by switching provider or model |
| session transcript records a different provider/model/thinking **at any point** (including an intermediate switch that is later reverted) | raises; the trial is not recorded |
| session transcript has no model or thinking event | raises; absence is not assumed to mean "default was fine" |
| series metadata differs on arm, revision, provider, model, full id, thinking, tool allowlist, pi version, host class, trial count or rotation | refuses to extend the series |
| the running `pi --version` differs from the version stamped in the config (or no version was stamped) | raises before the first session |

## 6. Verification evidence (effective configuration)

`pi` recognizes the configuration:

```
$ pi --version
1.6.0
$ pi --list-models deepseek | head -2
provider      model            context  max-out  thinking  images
deepseek      deepseek-flash   1M       384K     yes       yes
```

The *effective* configuration — not just the command line — was verified from a configuration
probe session (a trivial non-task prompt in a throwaway directory, not a benchmark trial):

```
$ pi -p --provider deepseek --model deepseek-flash --thinking high \
      --no-extensions --no-mcp --no-skills --no-prompt-templates \
      --session-dir /tmp/cl5-cfg-smoke/sessions --tools read "Reply with exactly: CONFIG-OK"
CONFIG-OK
```

Session transcript (`2026-11-21T22-29-03-368Z_01a2003a-3107-770f-a94f-06599d0d6e61.jsonl`):

```
session       {"version":3,"cwd":"/tmp/cl5-cfg-smoke"}
model_change  {"provider":"deepseek","modelId":"deepseek-flash"}
thinking_level_change {"thinkingLevel":"high"}
```

`verify-effective-config` in `scripts/cl5/experiment-config.rkt` re-derives those three fields
from the transcript of every real trial and raises on any difference, so a silent substitution
cannot enter a series.

## 7. Wave ordering and current boundary

- Wave 2 (deterministic shadow-selection experiment over the CL0 defect corpus) runs before
  Wave 3 and is unaffected by this model change: it executes no model.
- Wave 3 stays blocked behind the Wave 1 harness review findings.
- No Wave 3 measured session has been started. The probe in §5 is a configuration check with a
  non-task prompt and is excluded from every series by construction (series writers only accept
  trial records produced by `run-trial`, which requires a frozen task id and grades an oracle).
- No change to required CI, test selection, admission, scheduling or security policy.
