# Testing-acceleration milestone #899 — Phase F final sweep (v1.00.32)

Wave: #9938 (Phase F). Head audited: `039a97ba` (`feat/9938-phase-f-final-sweep`,
= main at audit time; contains merged WA #9939, WB1 #9941, WB2 #9940, WB3 #9942,
WB4 #9943, WC #9945, WD #9944). **WE (#9937) was not yet merged** at audit time;
its retiering moves required-PR context membership and does not delete files —
a one-line consumer re-grep is the only WF evidence WE could invalidate, and no
WF decision below depends on a file WE would move.

Mandate: delete superseded test/infrastructure code **only** where the
replacement owner is verified authoritative and the orphaning happened **by the
B1–B4/WE retiers**. Nothing qualifies below; this sweep makes **zero deletions**
and documents why. No cleanup was manufactured.

## Method

Every candidate from the #9938 checklist was checked with the same four steps:
(1) name the authoritative replacement owner, (2) repo-wide consumer grep
(`.rkt`/`.yml`/`.rktd`/live JSON, excluding frozen versioned baseline artifacts
and provenance comments), (3) ownership-matrix + covers-manifest membership
check, (4) decision. Greps were run at head `039a97ba` in a clean worktree.

## Candidate table

| # | Candidate | Category | Authoritative owner (verified) | Consumer-grep evidence | Decision |
|---|---|---|---|---|---|
| C1 | Multi-round E2E cases in the six retiered deep owners (`tests/test-gsd-auto-review-loop.rkt`, `tests/test-gsd-auto-review-production.rkt`, `tests/test-gsd-generated-metadata-correction.rkt`, `tests/test-gsd-evidence-probe.rkt`, `tests/test-gsd-evidence-repair-production.rkt`, `tests/test-gsd-supplemental-evidence-probe.rkt`) vs their new B1–B4 fast contract files | Duplicate multi-round E2E rendered redundant by splits? | The deep owner IS the required slow/full deep-regression owner per binding amendment 5 ("replacement required owners, moved slow/full owner … demonstrated deep-regression execution in the same PR"); the contract file owns a NEW focused slice (single genuine Verify stage + controller seam + refusal/recovery), not the same cases | `comm -12` of sorted `test-case` label sets for all six contract/deep pairs: **0 shared labels in every pair**; the one look-alike ("squares input" in `test-gsd-auto-review-loop.rkt:152`) is fixture-heredoc content, not a case | **Retained.** Split was clean — contract files added new guarantees; deep owners keep unique whole-coordinator/multi-generation regression coverage. Deleting either side breaks amendment 5/6. |
| C2 | `tests/test-tui-atomic-state.rkt`, `tests/test-tui-overlay-state.rkt`, `tests/test-tui-selection-state.rkt` (deleted by WC #9945) | Orphan-consumer verification of an already-made milestone deletion | `tests/test-c-tui-state-consolidated.rkt` (require union of the three originals; every assertion/case label preserved verbatim per its header) | Live-tree grep: only hits are the consolidated owner's provenance header and the frozen `docs/reports/architecture-baseline-v0.99.87.rktd` (versioned historical snapshot, not a consumer). `tests/tier-ownership-matrix.json`: 0 rows; `tests/.coverage-manifest.json`: 0 entries; `tests/tui/event-simulator.rkt` keeps 14 live consumers incl. the new owner | **Verified clean.** No WF action needed; WC's own deletion was already docs/matrix-consistent. |
| C3 | `tests/helpers/contract-blame.rkt` (provides `check-contract-blame`, 0 requirers), `tests/helpers/test-events.rkt` (provides `make-test-event`, 0 requirers), `tests/helpers/ws-test-helpers.rkt` (provides `make-ws-context`, 0 requirers) | Orphaned helper builders | None reachable — the last consumers were removed in **pre-milestone** final-plan history (helper `git log -S` trails end before the milestone base `22851554`; the qt-facade wave `707bf3bf` era removed the surrounding suites); none of the milestone commits `22851554..039a97ba` touches these files (`git log` on the four paths is empty) | `grep -rn --include='*.rkt' "helpers/contract-blame.rkt"` (and the other two) over `tests/ tui/ agent/ util/ runtime/ scripts/`: only self-reference; each is still a `tier-ownership-matrix.json` row (admitted standalone module) | **Retained — outside WF deletion authority.** These are genuine pre-existing orphans (real cleanup debt), but WF may only delete what B1–B4/WE orphaned. Flagged to the owner for a separate authorized housekeeping pass with its own matrix/SHA256SUMS regeneration. |
| C4 | `tests/helpers/metadata-cache-repro.rkt` | Orphaned helper? | N/A — not orphaned: standalone `#!/usr/bin/env racket` BUG-0090 cold/warm cache reproduction supervisor; its target `tests/test-gsd-generated-metadata-correction.rkt` still exists (B3 retiered it, did not delete it) | Not a library: zero requirers is by design; named in `scripts/testing/prepare-exclusions.rktd` only as a *deliberately-not-excluded* compile-verified file; target-file grep confirms target present | **Retained.** Working manual reproduction tooling for BUG-0090; its purpose survived B3. |
| C5 | Nested `./qt`/`raco test` spawns inside tests (Phase C checklist item) | Unnecessary nested full-runner invocations | The remaining spawns are (a) the authoritative interface owners themselves (`tests/test-qt-interface.rkt` slow since WB4, `tests/test-qt-lanes.rkt`), (b) fixture-internal **genuine-Verify probes** over synthetic mini-worlds (e.g. `test-gsd-evidence-probe.rkt:241` runs `raco test` on a generated probe file; `test-gsd-auto-review-loop.rkt`'s fixture runner/lint heredocs run inside the simulated delivery checkout) — the amendment-6 design, (c) hermetic self-re-exec wrappers (e.g. `test-gsd-review-runtime.rkt:267` re-runs its own module under an empty `PLTUSERHOME`) | Grep `(system\|(subprocess\|process\*` × `raco\|/qt` over `tests/`: every hit falls in (a)–(c); **no test spawns the q test suite recursively** | **Retained (nothing to delete).** Phase C (final-plan §2, pre-milestone) already removed the redundant paths; what remains is owned genuine-verify design. |
| C6 | Stale docs — former Python controllers / legacy CLI claims | Stale docs | Python control plane was eliminated pre-milestone (`707bf3bf`, #9913); the native controller behind the historical seam name `default-review-python` is `auto-review-seam-run!` (`extensions/gsd/review-dispatch.rkt:195`, comment: "The name is kept because it is the exported port contract") | `docs/testing.md`: fresh from WD #9944; "legacy CLI/legacy runner adapter" wording is deliberate, honest naming of the still-supported adapter — **not stale**. **Found stale:** `docs/automatic-review.md:498-504` still says "real Python controller" and presents `tests/test-gsd-auto-review-production.rkt` as the focused proof — post-WB2 the focused required-PR proof is `tests/test-gsd-auto-review-production-contract.rkt` and the controller is native | **Retained — file outside WF scope.** WF may edit only `docs/testing.md` (no stale claim found there); `docs/automatic-review.md` needs an owner-authorized correction. Flagged below. |
| C7 | Historic bench/tooling (`benchmarks/`, `tests/reproducers/`) | Historic bench consuming gate time (Phase A item) | WA (`docs/reports/TEST-WORK-MASS-v1.00.32.md`) measured the current workload; reproducers are standalone live-debug tools with existing consumers (`tests/test-sse-phase-timeout-bounds.rkt` etc.) | Not orphaned by B1–B4/WE; not in the required-gate critical path per WA artifact | **Retained — outside WF scope/authority.** |

## No-deletion statement

Zero files were deleted or modified in this sweep. Every checklist category was
audited with named replacement owners and consumer greps; no file was orphaned
*by the B1–B4/WE retiers*, and the one factually stale docs claim
(`docs/automatic-review.md`) lives outside this wave's authorized file scope.
This is the honest outcome, not a skipped audit.

## Flagged for owner follow-up (not done here)

1. `docs/automatic-review.md` lines 498–504: "real Python controller" → native
   `auto-review-seam-run!`; focused-proof pointer → `tests/test-gsd-auto-review-production-contract.rkt`.
   Same-commit docs-accuracy pin update if the claim is pinned later.
2. Pre-existing orphan helpers C3 (`contract-blame`, `test-events`,
   `ws-test-helpers`): deletable in a future authorized housekeeping pass;
   requires matrix row removal + SHA256SUMS + covers-manifest regeneration.

## Final gate results (executed at head `039a97ba`, clean tree before report commit)

| Gate | Result |
|---|---|
| `racket scripts/run-tests/inventory.rkt --ownership-map --tier-matrix artifacts/tier-ownership/v1.00.32-w0/ownership-matrix.json` | green — 1571 families, 7 areas, 0 gaps; **byte-identical to committed artifact** (tree stayed clean) |
| `racket scripts/tier-ownership-matrix.rkt generate` + `check` | wrote 1167 rows; **check green (1167 rows)**; byte-identical |
| `racket scripts/run-tests.rkt --generate-covers-manifest` | entries=203, manual-review=0, runner-version=1.00.32; **byte-identical** |
| `sha256sum artifacts/tier-ownership/v1.00.32-w0/ownership-matrix.json > …/SHA256SUMS` | regenerated deterministically; byte-identical |
| `racket scripts/lint-all.rkt` | **23 passed, 2 warnings** (pre-existing non-blocking `arch`, `release-readiness`) |
| `racket scripts/lint-format.rkt` | 2647 files scanned, 0 errors/0 warnings — PASSED |
| `racket scripts/lint-tests.rkt` | 0 errors, 386 warnings, 110 infos — PASSED |
| `racket scripts/lint-docs.rkt` | 0 warnings — PASSED |
| `./qt quick` | **quick passed: 28 files, 325 tests** (not PR/full verification) |
| `./qt default --explain` | selection 1378 files (union 1379); suites fast=1329, security=64, workflows=36, smoke=19, platform=38; 1 unavailable-here (`tests/test-run-tests-exit-five.rkt`, `@requires browser`); selection unchanged — no WF deletions, so no intentional-owner delta |
| `racket scripts/run-tests.rkt tests/test-deterministic-clock.rkt` | VERDICT PASS, 1 file / 1 pass / 0 fail / 0 skip, 2.724 s (wait-audit lint clean) |
| `git diff --exit-code -- scripts extensions .github .planning` | empty (exit 0) |
| `git diff --exit-code -- scripts/required-pr-checks.policy` | empty (exit 0); 13 contexts pinned (lint, lint-quality, security, release-dry-run, workflows (0), workflows (1), workflows-aggregate, smoke (ubuntu-latest), test (0), test (1), test (2), test-aggregate, test-platform) |

Final file accounting: fast suite = **1329 files** as required; only tracked-file
change in this wave is this report (added before the metrics resync, per the
`git add -A` → `metrics.rkt --sync-all` ordering rule).
