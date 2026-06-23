# AUDIT: v0.99.44 — Real-LLM tmux TUI Exploration Hardening (FINAL)

**Date:** 2026-07-04
**Milestone:** #831
**Parent Issue:** #8613
**Status:** ✅ COMPLETE

---

## 1. Executive Summary

The v0.99.44 milestone converted ad-hoc real-provider tmux TUI exploration
into official product testing infrastructure. Over 11 waves (W0–W10), the
project delivered:

- A structured evidence-collection harness (149 unit tests)
- An official exploratory runner with 6 scenarios
- Safety runbook, suite policy, and centralized reporting protocol
- Full fast suite green: **994/994 files PASS**

All deliverables are complete. This audit confirms release readiness.

---

## 2. Wave-by-Wave Summary

| Wave | Issue | PR | Commit | Deliverable |
|------|-------|----|--------|-------------|
| W0 | #8614 | #8626 | `0b4e30dd` | Baseline + fixture stabilization |
| W1 | #8615 | #8627 | `81b4f77b` | Official `scripts/tmux-tui-explore.rkt` |
| W2 | #8616 | #8628 | `e4c85cab` | Structured turn-completion trace events |
| W3 | #8617 | #8629 | `dde4ff3f` | Real-provider safe harness helpers |
| W4 | #8618 | #8630 | `28bcf6e8` | Approval-prompt-specific automation |
| W5 | #8619 | #8631 | `d397b435` | Tool-execution trace + artifact truth |
| W6 | #8620 | #8632 | `3989b437` | MAS/subagent lifecycle evidence |
| W7 | #8621 | #8633 | `8af9eb4e` | Durable memory restart round-trip |
| W8 | #8622 | #8634 | `8b9fd703` | GSD lifecycle + release/audit truthfulness |
| W9 | #8623 | #8635 | `ea190d55` | Flake classification + centralized reporting |
| W10 | #8624 | #8636 | `e7f3da74` | Documentation, safety runbook, suite policy |

---

## 3. Deliverable Inventory

### 3.1 Harness API Surface (W0–W9)

**W2 — Structured Turn Completion (trace.jsonl-based):**
`find-trace-jsonl-paths`, `read-trace-events`, `trace-entry-phase`,
`trace-entry-turn-id`, `turn-completion-trace-entry?`,
`latest-turn-completion-event`, `wait-for-turn-completion-event`

**W3 — Real-Provider Safe Helpers:**
`real-provider-authorized?`, `require-real-provider-authorization!`,
`make-real-provider-tmux-env`, `copy-q-config-to-temp-home!`,
`send-prompt-and-wait!`, `detect-queued-prompts`,
`assert-no-queued-prompts!`, `write-exploration-artifacts!`

**W4 — Approval-Prompt Automation:**
`detect-approval-prompt`, `parse-approval-prompt`,
`classify-approval-safety`, `safe-capabilities`,
`dangerous-capabilities`, `dangerous-command-patterns`,
`approve-approval!`, `deny-approval!`, `handle-approval-if-present!`,
`assert-no-approval-pending!`

**W5 — Tool-Execution Trace Verification:**
`tool-execution-phases`, `trace-entry-data`, `trace-entry-tool-name`,
`tool-execution-trace-entry?`, `find-tool-execution-events`,
`parse-tool-events`, `compute-file-fingerprint`,
`verify-file-unchanged!`, `detect-sensitive-leak`,
`verify-artifact-redacted!`

**W6 — MAS/Subagent Lifecycle Evidence:**
`mas-lifecycle-phases`, `mas-spawn-event`, `mas-lifecycle-info`,
`mas-trace-entry?`, `find-mas-events`, `parse-spawn-approval-event`,
`parse-mas-lifecycle`, `verify-subagent-spawn-lifecycle`,
`detect-mas-coordination-events`

**W7 — Durable Memory Restart Round-Trip:**
`memory-event-phases`, `session-lifecycle-phases`,
`memory-store-record`, `memory-retrieval-record`,
`session-lifecycle-record`, `durable-memory-roundtrip`,
`memory-trace-entry?`, `session-lifecycle-trace-entry?`,
`find-memory-events`, `find-session-lifecycle-events`,
`parse-memory-store-event`, `parse-memory-retrieval-event`,
`parse-session-start-event`, `parse-durable-memory-roundtrip`,
`verify-durable-memory-roundtrip`

**W8 — GSD Lifecycle + Release/Audit Truthfulness:**
`gsd-event-phases`, `gsd-transition-record`, `gsd-wave-record`,
`gsd-plan-record`, `gsd-lifecycle-info`, `gsd-trace-entry?`,
`find-gsd-events`, `parse-gsd-transition-event`,
`parse-gsd-wave-event`, `parse-gsd-plan-event`,
`parse-gsd-lifecycle`, `verify-gsd-transition-succeeded`,
`release-refusal-patterns`, `release-evidence`,
`detect-release-authorization-refusal`, `find-release-manifest`,
`verify-release-manifest-present!`,
`verify-release-authorization-refused!`

**W9 — Flake Classification + Centralized Reporting:**
`flake-classifications`, `flake-record`, `classify-result-status`,
`flake-indicator-patterns`, `detect-flake-indicators`,
`flaky-run?`, `classify-exploration-run`, `central-log-entry`,
`make-central-log-entry`, `central-log-entry->hash`,
`write-central-log-entry!`, `parse-central-log`,
`exploration-bundle`, `make-exploration-bundle`,
`bundle-pass-rate`, `bundle-has-flakes?`,
`render-bundle-index-markdown`

### 3.2 Explorer Scenarios

| Tag | Mock Status | Classification |
|-----|-------------|----------------|
| memory | pass | pass |
| gsd | pass | pass |
| mas | partial | partial |
| tools | pass | pass |
| release-audit | pass | pass |
| durable-memory | partial | partial |

### 3.3 Documentation

- `docs/reports/TMUX-TUI-EXPLORATION-SAFETY-RUNBOOK-v0.99.44.md` —
  Comprehensive safety runbook (376 lines)
- `docs/reports/TMUX-TUI-TESTING-v0.99.43.md` — Updated with W2–W9
  cross-reference and test count (149 unit tests)

---

## 4. Test Evidence

### 4.1 Fast Suite
```
racket scripts/run-tests.rkt --suite fast
=> PASS: 994/994 files
```

### 4.2 Harness Unit Tests
```
raco test tests/test-tmux-q-harness.rkt
=> 149 tests passed
```

### 4.3 Explorer Tests
```
raco test tests/test-tmux-tui-explore.rkt
=> 7 tests passed
```

### 4.4 Lint
```
racket scripts/lint-all.rkt
=> 22 passed, 1 non-blocking warning
```

---

## 5. CI Verification

### 5.1 Latest Main CI
- **Run:** `28057815505` (triggered by W10 merge `e7f3da74`)
- **Ubuntu jobs:** ✅ All pass (lint, test 8.10, test 8.11, smoke, security, workflows, inter-wave-gate, release-dry-run)
- **macOS jobs:** Queued (runner availability — not code failures)
- **Conclusion:** All Linux jobs green. macOS queued due to GitHub Actions runner availability, not code issues.

### 5.2 Release Workflow
- Release workflow triggers on tag push (`v0.99.44`)
- Release-dry-run CI job: ✅ success
- `release-manifest.json` generation: verified in workflow

---

## 6. Safety Policy Compliance

| Policy | Status | Enforcement |
|--------|--------|-------------|
| Temp HOME isolation | ✅ | `make-real-provider-tmux-env` |
| Credential redaction | ✅ | `redact-sensitive` + `redact-explore-text` |
| Real-provider opt-in gate | ✅ | Three-env-var check |
| No `tmux kill-server` | ✅ | `stop-session!` named sessions only |
| Mock/no-network default | ✅ | `--mode mock` default |
| Real tests not CI blockers | ✅ | `Q_TMUX_TUI_TESTS=1` opt-in |
| `release-manifest.json` mandatory | ✅ | Release workflow gate |

---

## 7. Release Readiness

### 7.1 Version Bump
- `util/version.rkt`: `0.99.43` → `0.99.44`
- All 23 version surfaces synced via `scripts/bump-version.rkt`
- `scripts/lint-version.rkt`: ✅ PASSED

### 7.2 CHANGELOG
- v0.99.44 entry added with full wave-by-wave summary
- Test counts and file inventory documented

### 7.3 Gate Checklist
- [x] Latest main CI: Ubuntu jobs green
- [x] Release-dry-run: ✅ success
- [x] Version lint: ✅ passed
- [x] Fast suite: ✅ 994/994 files
- [x] Harness tests: ✅ 149 tests
- [x] Explorer tests: ✅ 7 tests
- [x] Lint suite: ✅ 22 passed, 1 non-blocking warning
- [x] CHANGELOG updated
- [x] Safety runbook published

### 7.4 Known Limitations
- macOS CI jobs queued due to runner availability (not code failures)
- Real-provider exploration scenarios marked `pending-real-run`
  (infrastructure is in place; actual runs require opt-in)
- `q>` prompt not reliable as completion signal (mitigated by trace events)

---

## 8. Conclusion

All v0.99.44 scope items are complete. The milestone successfully:
1. Delivered structured evidence-collection infrastructure
2. Established safety policies and documentation
3. Created centralized reporting with flake classification
4. Maintained 994/994 fast suite green throughout

**Verdict: ✅ RELEASE READY**
