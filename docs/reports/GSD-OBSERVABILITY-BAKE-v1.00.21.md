# GSD Observability Bake — v1.00.21

- **Date:** 2026-08-28
- **Campaign:** BUG-0039 … BUG-0045 (GSD observability + release-race hardening)
- **Delivery branch:** `fix/bug-0039-campaign-cost-tracking`
- **Bake base:** `55fa4367` (post-W7 checkpoint: fixture re-record after
  raco-fmt, go-orchestrator 1460 lines)
- **Release:** v1.00.21

This report is the campaign-level acceptance evidence for the seven
campaign gates (plus the release-gate check), in the spirit of the
v1.00.20 bake (`GSD-WORKFLOW-RELIABILITY-BAKE-v1.00.20.md`). Each drill
below names the suite that proves it, the observed numbers, and the
verdict. Where a drill is deliberately contract-level (a live duplicate
run is unsafe or would require tearing down the running release
pipeline), that is stated explicitly per the wave plan.

## Gate summary

| Gate | Bug | Drill | Suite | Result | Verdict |
|------|-----|-------|-------|--------|---------|
| a | BUG-0044 | threshold-config | `test-stall-threshold-config.rkt` | 12/12 | PASS |
| b | BUG-0043 | error-surface | `test-outcome-error-surface.rkt` | 7/7 | PASS |
| c | BUG-0045 | release race / idempotent publish | `test-release-workflow-contract.rkt` | silent, exit 0 | PASS (contract-level) |
| d | BUG-0041 | wave-doc lint | `test-wave-doc-lint.rkt` | 9/9 | PASS |
| e | BUG-0039 | budget pause/resume | `test-campaign-cost-tracking.rkt` | 4/4 | PASS |
| f | BUG-0040 | notifications | `test-campaign-notifier.rkt` | 10/10 | PASS |
| g | BUG-0042 | decomposition metrics | `go-orchestrator-baseline.rktd` + `test-release-workflow-contract.rkt` | 1460/23 vs target ≤1500 | PASS |

## (a) Threshold-config drill — BUG-0044

`racket tests/test-stall-threshold-config.rkt` → **12 success(es), 0
failure(s), 12 tests run.**

Key drill steps proven:

- `STALL-*-DEFAULT constants are 8/15/30/300` and
  `default-constructed watchdog is 8/15/30/300` — compile-time
  constants are now *defaults*, not the only path.
- `synthetic settings file with soft-limit 3 changes trip behavior
  without source edits` — a settings file alone (no source edit) re-tunes
  the watchdog: the drill's three-warning window trips at soft-limit 3
  where the default would not have fired.
- `all four keys from a settings file override the defaults` —
  `gsd.watchdog.soft-limit`, `hard-limit`, `window`, `abs-backstop`
  all reach the watchdog.
- `startup log line exposes the EFFECTIVE thresholds once per wave` —
  the effective-values banner (which values, from where) is emitted at
  wave start, satisfying the observability half of the gate.
- `invalid (non-positive/non-integer) values fall back to defaults —
  never crash` — hostile settings degrade safely.

Verdict: **PASS** — thresholds are runtime-configurable through the
settings file and the effective values are observable at startup.

## (b) Error-surface drill — BUG-0043

`racket tests/test-outcome-error-surface.rkt` → **7 success(es), 0
failure(s), 7 tests run.**

Key drill steps proven:

- `injected stall-kill outcome → one 'system-error transcript entry, no
  message-surface copy` — an injected stall kill renders exactly one
  `[SYS] [ERROR]` entry in the transcript and nothing leaks into the
  message (prompt) surface. This is the core acceptance shape of the
  gate.
- `terminal failure emits an error-classified event with kind + message
  verbatim` and `typed error event kind exists in the GSD event
  taxonomy` — outcome errors ride a typed event, not string matching.
- `TUI render source has the [SYS] [ERROR] event variant` — the render
  side pins the visual surface.
- `done-class outcomes are unaffected` — no regression for success.

Verdict: **PASS** — kills and failures are visually distinguishable from
assistant output; the message surface stays clean.

## (c) Release race drill — BUG-0045 (contract-level by design)

`racket tests/test-release-workflow-contract.rkt` → **silent runner,
exit 0** (the suite is contract-shaped: it parses and asserts the
workflow YAML and publish script, and prints nothing on success).

Per the wave plan, the *live* duplicate-run drill (pushing the same tag
twice against production) is **not safe**, so the race is proven at the
contract level, exactly as W3 built it:

- `.github/workflows/release.yml` carries a `concurrency` group keyed on
  the tag ref, so duplicate tag-push triggers serialize instead of
  racing.
- The publish path **re-verifies assets before declaring success** and
  no-ops on an already-published tag (idempotent publish): a duplicate
  run re-checks the release/assets and exits verified-success instead of
  422-ing and leaving untagged draft residue.
- The contract suite asserts both properties against the checked-in
  workflow/script text, so drift breaks the suite.

Verdict: **PASS (contract-level, live duplicate-run deliberately not
exercised against production — noted per wave plan).** If a real
duplicate run ever races in the wild, the no-op path is the designed
outcome and this v1.00.21 release pipeline itself is its first
beneficiary.

## (d) Wave-doc lint drill — BUG-0041

`racket tests/test-wave-doc-lint.rkt` → **9 success(es), 0 failure(s),
9 tests run.**

Key drill steps proven:

- `doc missing Files/Verify/Done yields three named violations` — the
  doctored wave doc (sections removed) is rejected **by name** at `/go`
  entry.
- `lint verdict is stored as durable campaign evidence at creation` —
  the verdict lands in the campaign record (the campaign record, not
  just console noise).
- `one named warning per doc at /go entry; warnings never block` — lint
  is advisory at entry, so execution is not held hostage.
- `executor prompt carries the lint verdict (/go wiring)` and
  `non-canonical status header is its own named violation` — the whole
  path from doc → verdict → record → executor prompt is pinned.

Verdict: **PASS** — malformed wave docs can no longer degrade execution
silently; the gap is named and recorded.

## (e) Budget drill — BUG-0039

`racket tests/test-campaign-cost-tracking.rkt` → **4 success(es), 0
failure(s), 4 tests run.**

Key drill steps proven:

- `synthetic usage metadata lands in attempt/wave/campaign fields` —
  usage flows into the durable records at all three granularities.
- `absent metadata records 'usage-missing — never zeros` — missing data
  is explicit, not silently free.
- `max-cost crossing → durable pause with named reason, resumable` — a
  tiny `gsd.campaign.max-cost` pauses the campaign durably with a named
  reason, and **raising the ceiling resumes cleanly**.
- `gsd.campaign.max-cost is wired through settings and the orchestrator`
  — the knob is a settings key end to end.

Verdict: **PASS** — spend is tracked, aggregable, budgetable, and the
pause/resume transition is durable and named.

## (f) Notification drill — BUG-0040

`racket tests/test-campaign-notifier.rkt` → **10 success(es), 0
failure(s), 10 tests run.**

Key drill steps proven:

- `recording sink observes done / failed / stall transitions` — the
  fake (recording) sink captures terminal transitions including
  wave-done and budget-pause emissions.
- `message format: campaign id, wave idx, kind, reason, spend` — every
  emission carries the required context on one line.
- `raising sink never fails the transition` — a sink that raises harms
  nothing; the campaign proceeds.
- `silent default: no sinks outside tmux with no opt-ins` and
  `opt-in sinks resolve from settings` / `misconfigured webhook URL:
  warn once, sink skipped` — notification is strictly opt-in and
  misconfiguration degrades to a single warning.

Verdict: **PASS** — detached campaigns can now notify instead of being
visible only by polling tmux panes, and a broken sink is inert.

## (g) Decomposition metrics drill — BUG-0042

Fixture: `tests/fixtures/go-orchestrator-baseline.rktd`, asserted live
by `tests/test-release-workflow-contract.rkt` (silent, exit 0):

| Metric | W0 campaign baseline | Post-W7 (today) | W7 target | Verdict |
|--------|---------------------|-----------------|-----------|---------|
| `extensions/gsd/go-orchestrator.rkt` lines | 2566 | **1460** | ≤ 1500 | PASS |
| top-level defines | 91 | **23** | (shrink) | PASS |

- Five concerns were extracted (stall-policy, infra-retry-policy,
  freshness, attempt-artifacts, campaign-budgets); the file is 1106
  lines / 68 defines below its campaign-start size and 40 lines under
  the W7 target even after the post-W7 raco-fmt re-record.
- Characterization is identical: the pre-extraction characterization
  suites (`test-gsd-stall-policy.rkt` 10, `test-gsd-infra-retry-policy.rkt`
  20) and the responsibility inventory pin
  (`test-gsd-responsibility-inventory.rkt`) stayed green across the
  extraction; the fast suite at the bake base was fully green (W6
  checkpoint: 1147 files / 16615 tests).
- The fixture's maintenance contract re-records on legitimate growth
  while keeping the file under target — enforced by the contract suite,
  not by convention.

Verdict: **PASS** — decomposition is real, characterized, and pinned
below target.

## Release gate (v1.00.21)

Release per `docs/releasing.md`: bump-version → sync-version
(`scripts/sync-version.rkt --write`) → CHANGELOG entry with the four
mandatory sections and bug-number mapping checked against
`.planning/bugs/INDEX.md` (registry authoritative; all seven campaign
bugs verified against the registry rows) → version literals purged from
tests → `metrics --sync-all` + README status → pre-commit 18/18 → fast
suite green → PR (squash) → gate evidence at FINAL SHA in a clean
worktree → preflight invariants → annotated tag `v1.00.21` → push → release
pipeline (concurrency-grouped, idempotent publish) → publication
verified. Bugs INDEX updated to `fixed-in v1.00.21`.

Final bake evidence at the delivery SHA:

- `racket scripts/run-tests.rkt --suite fast` → **PASS, 1149 files /
  1149 pass / 0 fail** (RUN-SUMMARY runner-version=1.00.21).
- `racket scripts/pre-commit.rkt` → **18/18 checks passed**.
- `racket scripts/release-dry-run.rkt` → **6/6 checks passed**
  (version-match, tag-format, changelog-entry, release-notes,
  manifest, arch-integrity) at canonical version 1.00.21.
- `racket scripts/release-preflight.rkt v1.00.21` → **all invariants
  hold** (tag-exists, tag-object-type, tag-version-consistency,
  manifest dry-run); with `--readiness` the registry/milestone
  readiness stage also holds (no fixes gated for v1.00.21 — the INDEX
  rows are `fixed v1.00.21`, not pending targets).
- Annotated tag `v1.00.21` created at the delivery SHA and pushed to
  `origin` together with the delivery branch.
- `.planning/bugs/INDEX.md` — BUG-0039…BUG-0045 rows now read
  `fixed v1.00.21`; header counts updated (Open 5, Fixed/validated 23).

### Operational reminder (this campaign's own TUI)

After merge, **restart q before any further `/go`** — the v1.00.19
freshness guard will otherwise refuse campaign start against the
freshly bumped checkout.
