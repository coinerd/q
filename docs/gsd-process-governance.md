# GSD Process Governance

**Status:** Active — enforced via `scripts/milestone-close-gate.rkt`  
**Scope:** All q-agent milestones, waves, and releases  
**Established:** v0.99.47 (2026-07-09)

This document codifies the process rules discovered through GSD audit failures
(v0.99.47 premature close, v0.99.47 claim inflation). Each rule exists because
a prior milestone violated it and required a remediation cycle.

---

## 1. Milestone Naming Convention (GAP-2)

Milestone titles MUST accurately describe the actual scope of work delivered.

### Rules

| Title contains | Requirement |
|----------------|-------------|
| "Real-World" / "Real-Life" | At least one test must use a real LLM provider (not mock/synthetic) |
| "Audit" | Findings must be documented and tracked as GitHub issues |
| "Hardening" | Must include new tests for the hardened behavior |
| "Remediation" | Must reference the source audit/issue being remediated |

### Prohibited patterns

- **"Real-World X"** if all tests use mock or synthetic data — use
  "Subsystem API Test Coverage" or "Unit Test Coverage for X" instead.
- **"X Audit"** if no findings are documented — use "X Investigation" or
  "X Characterization" instead.
- **"X Complete"** if scope was only partially delivered — use
  "X Phase 1" or "X (partial)" instead.

### Enforcement

Milestone naming remains a required human planning/review check. The current
`claim-verifier.rkt` verifies quantitative claims, while
`milestone-close-gate.rkt` fails closed on missing claims and release evidence;
neither script infers semantic scope from a milestone title. A future naming
checker must not be claimed until executable fixtures prove it.

---

## 2. Required PR Checks and Wave Evidence

Every wave is implemented in one feature branch and one PR. `main` branch
protection requires the complete non-tag CI workflow, including the
`gsd-governance` check. Missing, pending, skipped, cancelled, failed, stale, or
malformed required checks block merge.

Each wave PR adds exactly one record under
`docs/reports/gsd-wave-evidence/*.rktd`, plus retained review and validation
snapshots under the adjacent `gsd-wave-reviews/` and `gsd-wave-validation/`
directories. The evidence record contains:

- milestone, wave, and issue identity;
- `ready-for-merge` status and reviewed implementation SHA;
- SHA-256 of the PR content diff excluding the three attestation directories;
- paths to the retained review and validation snapshots;
- the exact required-check inventory from `scripts/required-pr-checks.policy`.

CI computes the actual changed-content digest from the PR base/head or the main
push before/head pair. `gsd-wave-gate.rkt` verifies that digest, reads both
retained artifacts, requires `APPROVED`, cross-checks implementation SHA and
content digest, and validates required red-first/test/lint/fast/planning fields.
A fabricated path or stale record therefore fails. The external merge helper
binds required checks to the exact current PR head SHA and verifies GitHub's
merge result before explicitly closing issues or setting board state to Done.

Because GitHub does not permit a sole maintainer to approve their own PR, the
independent review is a retained external reviewer artifact rather than a
self-review. This is explicit evidence, not a silent waiver.

---

## 3. Per-Wave Issue Filing Requirement (GAP-3)

Every wave that discovers findings (bugs, gaps, design issues, tech debt)
MUST file a GitHub issue for each finding BEFORE the wave's issue is closed.

### Wave closure checklist

```text
1. Implementation complete
2. Tests pass (focused + smoke suite)
3. PR merged to main
4. Findings (if any) have GitHub issues filed     ← REQUIRED
5. Wave issue closed
6. Board status set to Done
```

### What counts as a "finding"

A finding is any of:
- A bug discovered during implementation
- A design gap or architectural concern
- A tech debt item identified during testing
- A limitation or edge case that couldn't be resolved in-scope
- A recommendation from a post-completion audit

### Enforcement

When closing a wave issue, verify that any findings documented in the
wave's STATE or PR body have corresponding GitHub issues. If a wave
documented findings but no issues exist, the wave must NOT be closed
until issues are filed.

Example of the v0.99.47 failure: the wave documented 1 medium + 6 low
findings but filed zero issues. This should have blocked closure.

---

## 4. Pre-Closure Release-Truth Gate (GAP-4)

Before closing a milestone, run the pre-closure gate:

```bash
racket scripts/milestone-close-gate.rkt <milestone-number>
```

### Gates checked (all must pass)

| # | Gate | Source |
|---|------|--------|
| 1 | Claim verification — test counts match actual | `claim-verifier.rkt` |
| 2 | CI green on main — all required jobs passed | GitHub Actions API |
| 3 | Release assets present — tarball + manifest | GitHub Releases API |
| 4 | Manifest traceability — version/tag/commit match | `milestone-gate.rkt` |
| 5 | All milestone issues closed | GitHub Issues API |
| 6 | CHANGELOG entry exists for version | `CHANGELOG.md` |
| 7 | Metrics synced — README matches codebase | `lint-widened-ledger.rkt` |

### Enforcement

This is a **HARD GATE** — no exceptions, no manual overrides. The script
exits with code 1 if any gate fails. The milestone must NOT be closed
until the script exits 0.

Example of the v0.99.47 failure: W11 was closed with CI RED and no
release assets. The close gate would have caught both.

---

## 5. HANDOFF.json Maintenance Protocol (GAP-5)

`HANDOFF.json` MUST be kept current. It is the machine-readable handoff
state for context recovery and machine switches.

### Required fields

| Field | Meaning | Updated when |
|-------|---------|-------------|
| `version` | Current canonical version | After each release |
| `git_head` | Current main HEAD SHA | After each merge to main |
| `phase` | Current milestone title | At milestone start |
| `status` | PLANNED / IN PROGRESS / COMPLETED | At each phase transition |
| `plan` | Path to current PLAN.md | At milestone start |
| `state` | Path to current STATE.md | At milestone start |
| `sync.last_machine` | Which machine last updated | On every update |
| `sync.last_sync` | ISO timestamp of last sync | On every update |

### Maintenance protocol

```text
1. At milestone START: update phase, status, plan, state paths
2. After each wave merge: update git_head
3. At milestone COMPLETION: update version, status, git_head
4. Before machine switch: run `q_sync handoff`
```

### Enforcement

A stale HANDOFF.json (version field more than 1 release behind current) is a
process violation. HANDOFF freshness is currently enforced by per-wave review
and the machine-checkable wave evidence record; `milestone-close-gate.rkt` does
not yet parse external `.planning/HANDOFF.json`, so this document does not claim
that it does.

Example of the v0.99.47 failure: HANDOFF.json referenced a release 47 versions older than the project version.

---

## 6. Temp Directory Cleanup (GAP-6)

tmux test runs create temp directories under `/var/tmp/q-tmux-*`. These
MUST be cleaned up after successful runs.

### Protocol

- The tmux harness (`tests/helpers/tmux-q-harness.rkt`) calls
  `cleanup-tmux-env!` after successful test completion.
- Failure artifacts (when `write-failure-artifacts!` was called) are
  preserved for debugging.
- The report script (`scripts/tmux-tui-report.rkt`) distinguishes:
  - "no failure artifacts (expected on success)" — empty dir, test passed
  - "incomplete failure bundle" — artifacts exist but missing expected files
  - "complete" — all expected artifact files present

### Enforcement

After a tmux test run, `ls /var/tmp/q-tmux-*` should show only directories
for failed tests. Successful test directories should be cleaned up.

---

## 7. Architecture Boundary Debt Tracking (GAP-7)

Architecture boundary violations documented in
`docs/architecture/dependency-policy.rktd` MUST have a resolution milestone
scheduled before their `revisit-by` date expires.

### Protocol

- Each boundary exception has exactly one lifecycle: a `revisit-by` date or an
  explicit `permanent-waiver` with a non-empty `waiver-justification`.
- A dedicated milestone must exist to resolve dated violations before
  the earliest `revisit-by` date.
- Permanent waivers are reserved for intentional composition boundaries; they
  remain subject to exception-count and boundary-drift gates.

### Enforcement

The CI gate test (`tests/test-arch-boundaries.rkt`) validates lifecycle
metadata for every exception layer and fails when dated exceptions expire. Extending dates without scheduling resolution
work defers the problem but does not solve it.

Example of the v0.99.47/46 failure: 13 boundary exceptions had
`revisit-by 2026-07-01` which expired, causing CI RED. The dates were
extended to 2026-10-01, but no resolution milestone was scheduled.

---

## Summary: Gap → Rule → Enforcement

| Gap | Rule | Enforcement tool |
|-----|------|------------------|
| GAP-1 | Claims must match reality; empty evidence fails | `claim-verifier.rkt`, `milestone-close-gate.rkt` |
| GAP-2 | Titles must match scope | Planning + independent review (no automated semantic checker yet) |
| GAP-3 | Findings must have issues | Wave closure checklist + GitHub sub-issues |
| F-10/F-13 | Current checks and review/validation evidence before merge | branch protection, `gsd-wave-gate.rkt`, external merge helper |
| GAP-4 | Release truth before close | `milestone-close-gate.rkt` |
| GAP-5 | HANDOFF.json must be current | Per-wave review/evidence (external planning is not parsed by close gate) |
| GAP-6 | Temp dirs cleaned on success | `cleanup-tmux-env!` |
| GAP-7 | Boundary debt must be tracked | Dedicated resolution milestone |
