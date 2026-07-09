# GSD Process Governance

**Status:** Active — enforced via `scripts/milestone-close-gate.rkt`  
**Scope:** All q-agent milestones, waves, and releases  
**Established:** v0.99.46 (2026-07-09)

This document codifies the process rules discovered through GSD audit failures
(v0.99.46 premature close, v0.99.46 claim inflation). Each rule exists because
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

The claim-verifier (`scripts/claim-verifier.rkt`) and milestone-close-gate
(`scripts/milestone-close-gate.rkt`) check for naming/scope mismatches by
comparing milestone titles against test evidence. A milestone titled
"Real-World" with zero real-provider test references will fail the gate.

---

## 2. Per-Wave Issue Filing Requirement (GAP-3)

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

Example of the v0.99.46 failure: the wave documented 1 medium + 6 low
findings but filed zero issues. This should have blocked closure.

---

## 3. Pre-Closure Release-Truth Gate (GAP-4)

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

Example of the v0.99.46 failure: W11 was closed with CI RED and no
release assets. The close gate would have caught both.

---

## 4. HANDOFF.json Maintenance Protocol (GAP-5)

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

A stale HANDOFF.json (version field more than 1 release behind current)
is a process violation. The milestone-close-gate checks that the
HANDOFF.json version matches the current canonical version before
allowing milestone closure.

Example of the v0.99.46 failure: HANDOFF.json referenced v0.99.46 while
the project was at v0.99.46 — 47 versions stale.

---

## 5. Temp Directory Cleanup (GAP-6)

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

## 6. Architecture Boundary Debt Tracking (GAP-7)

Architecture boundary violations documented in
`docs/architecture/dependency-policy.rktd` MUST have a resolution milestone
scheduled before their `revisit-by` date expires.

### Protocol

- Each boundary exception has a `revisit-by` date.
- A dedicated milestone must exist to resolve the violations before
  the earliest `revisit-by` date.
- If violations are not resolved, they must either be fixed or given
  a permanent waiver (no expiry date) with justification.

### Enforcement

The CI gate test (`tests/test-arch-fitness.rkt`) fails when boundary
exception dates expire. Extending dates without scheduling resolution
work defers the problem but does not solve it.

Example of the v0.99.46/46 failure: 13 boundary exceptions had
`revisit-by 2026-07-01` which expired, causing CI RED. The dates were
extended to 2026-10-01, but no resolution milestone was scheduled.

---

## Summary: Gap → Rule → Enforcement

| Gap | Rule | Enforcement tool |
|-----|------|------------------|
| GAP-1 | Claims must match reality | `claim-verifier.rkt` |
| GAP-2 | Titles must match scope | `claim-verifier.rkt` (naming check) |
| GAP-3 | Findings must have issues | Wave closure checklist |
| GAP-4 | Release truth before close | `milestone-close-gate.rkt` |
| GAP-5 | HANDOFF.json must be current | `milestone-close-gate.rkt` |
| GAP-6 | Temp dirs cleaned on success | `cleanup-tmux-env!` |
| GAP-7 | Boundary debt must be tracked | Dedicated resolution milestone |
