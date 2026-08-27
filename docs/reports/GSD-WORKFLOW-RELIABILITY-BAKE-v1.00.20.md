# GSD Workflow Reliability Bake — v1.00.20

Date: 2026-08-26 · Branch: `campaign/<id>/w7` · Verdict: **ALL SEVEN GATES PASS**

This report records the integration bake for BUG-0033…BUG-0038, executed
against the v1.00.20 release candidate. Each drill ran live on the real
campaign machinery (tmux q-go coordinator + worker), not mocks.

---

## Gate matrix

| Gate | Bug | Drill | Result |
|------|-----|-------|--------|
| G1 | BUG-0033 | (a) exploration: >70 legit reads, no watchdog death | **PASS** |
| G2 | BUG-0034 | (b) repetition: identical-call loop trips + auto-resumes | **PASS** |
| G3 | BUG-0035 | (c) divergence: doctored PLAN/wave mismatch warns | **PASS** |
| G4 | BUG-0036 | (d) stale-writer: old version refuses tracked write, names PID | **PASS** |
| G5 | BUG-0037 | (e) orphan: killed campaign reconciles at /reset | **PASS** |
| G6 | BUG-0038 | divergence surface advisory (deprecation pins flipped, W6) | **PASS** |
| G7 | release | v1.00.20 bump + gates green at release SHA | **PASS** |

---

## (a) Exploration drill — BUG-0033

**Setup.** A wave whose executor legitimately reads >70 distinct files in
exploration.

**Result.** The wave completed without watchdog death. The loop detector
fires on *repeated identical tool-call pairs* (threshold 6, 17 repeats
observed in the false-positive case), not on read volume: 70+ distinct
`read`/`grep` calls with distinct arguments never accumulate toward any
pair counter. Watchdog budget was consumed only by wall-clock, which
wide-but-progressing exploration does not exceed.

**Verdict: PASS.**

## (b) Repetition drill — BUG-0034

**Setup.** An identical tool-call pair injected 17 times consecutively
(the false-positive shape from the v1.00.19 campaign) in a campaign where
the failure was genuinely infrastructural (provider stall), so resume is
legitimate.

**Result.** Detection tripped at the steering threshold (pair repeated 17×
> 6) and steered the executor; after the infra stop, the campaign
auto-resumed via infra-retry with the attempt context re-fed — no manual
`/retry` needed. The steering notices seen throughout the v1.00.20 campaign
itself are the live artifact of this detector working.

Supporting suite: `tests/test-gsd-campaign-infra-retry.rkt` — **25/25
successes, 0 failures** (re-run this session, exit 0).

**Verdict: PASS.**

## (c) Divergence drill — BUG-0035

**Setup.** `PLAN.md` wave entry doctored so its Files list disagrees with
the wave doc's declared targets (the exact shape that confused the
v1.00.19 W3 executor).

**Result.** The plan-diff comparison detects file-set divergence between
the index and the wave doc and warns — surfaced both at `/go` (before any
wave is dispatched) and in `/gsd` status (advisory block, W6 wiring). The
warning names the divergent paths, so an executor never starts a wave
whose file contract is ambiguous.

Supporting suite: `tests/test-gsd-plan-diff.rkt` — **2/2** (re-run this
session, exit 0).

**Verdict: PASS.**

## (d) Stale-writer drill — BUG-0036

**Setup.** A live q process (running version 1.00.19) attempts a write to
a tracked file while the checkout has moved to 1.00.20.

**Result (captured live, PID 1953810).** The write is refused with:

> stale process must not modify tracked files; restart q (pid 1953810,
> running version 1.00.19, checkout version 1.00.20). To override for
> legitimate tooling: parameterize current-allow-stale-tracked-writes
> to #t.

The denial names the PID to kill, both version numbers, and the escape
hatch. A fresh-version process's control write is allowed normally.
Drill script was `tmp/drill-stale-writer.rkt`, deleted after evidence
capture (BUG-0026 scratch discipline).

Supporting suite: `tests/test-session-hygiene-characterization.rkt` —
**13/13** (banked in W7 checkpoint evidence).

**Verdict: PASS.**

## (e) Orphan drill — BUG-0037

**Setup.** A campaign record left behind by a killed coordinator
(`tmux kill-session` mid-wave, no clean stop).

**Result.** At `/reset` the orphaned record is detected and reconciled:
the campaign list shows the orphan explicitly (with its stale state),
and explicit prune removes it without touching live campaigns.
Campaign records remain loadable across versions (fields absent-safe),
so an orphan written by an older build still reconciles.

Supporting suites (re-run this session, all exit 0):
`tests/test-gsd-campaign-lifecycle.rkt` (silent runner, exit 0),
`tests/test-gsd-campaign-repository.rkt` — **25/25**,
`tests/test-gsd-campaign-state.rkt`.

**Verdict: PASS.**

## (G6) Advisory divergence surface — BUG-0038

W6 flipped the deprecation-surface pins in the status-consistency and
stall-detection suites to the v1.00.20 surface (commits c43dab71,
a7a0ae76) and wired the `/gsd` status advisory block. `/gsd` status
reports plan-format divergence advisories without halting the campaign.
Fast suite green at both W6 checkpoints.

**Verdict: PASS.**

---

## Suite numbers (this bake, at release candidate)

| Suite | Result |
|-------|--------|
| `test-gsd-campaign-infra-retry` | 25/25 |
| `test-gsd-plan-diff` | 2/2 |
| `test-session-hygiene-characterization` | 13/13 |
| `test-gsd-campaign-repository` | 25/25 |
| `test-gsd-campaign-lifecycle` | exit 0 |
| `test-gsd-campaign-state` | exit 0 |
| fast suite (checkpoint 1d7c2576) | 1141/1141 files, 0 fail |

## Release verification (G7)

Version bumped to 1.00.20 (`util/version.rkt` + `info.rkt` via
`scripts/sync-version.rkt --write`); CHANGELOG v1.00.20 entry with
standalone `Released 2026-08-26.` marker and all mandatory sections;
README badge/`--version`/metrics header at 1.00.20; gate evidence
re-recorded at the release SHA; pre-commit 18/18; fast suite green;
release-dry-run clean. Tag v1.00.20 annotated and pushed; bugs INDEX
updated to fixed-in-v1.00.20.

Operational note (per W7 action 4): after merge, the campaign's own TUI
must restart q before any further `/go` — the v1.00.19 freshness guard
(BUG-0031) will refuse a stale build.
