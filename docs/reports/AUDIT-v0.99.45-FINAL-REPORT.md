# Real-World Subsystem Audit — Final Report

**Milestone:** v0.99.45
**Date:** 2026-07-04
**Auditor:** Automated test suite + manual analysis
**Provider:** Mock/synthetic (no real API keys)

## Executive Summary

A comprehensive real-world subsystem audit of the Pi-Q agent was conducted across 11 waves (W0–W10), covering all major subsystems. The corrected audit inventory is **663 tests** across **11 test files**, containing **1,626 check assertions**. The smoke gate was green (19/19 files, 286/286 tests), but it did **not** include the v0.99.45 audit tests; the audit tests are part of the fast/default suite and require fast-suite verification.

The audit found **1 medium-severity issue**, **6 low-severity issues**, and **40+ informational findings** (overwhelmingly positive). No critical or high-severity issues were discovered. The codebase demonstrates strong architectural patterns, comprehensive input validation, thread-safe design, and defense-in-depth security.

**Release status:** v0.99.45 was audit-only. There was no v0.99.45 tag or GitHub Release. Remediation issues were filed in the follow-up v0.99.46 milestone as #8666–#8671.

## Wave Summary

| Wave | Subsystem | Issue | Tests | PR | Findings |
|------|-----------|-------|-------|----|----------|
| W0 | Audit Protocol | #8639 | Protocol doc | #8651 | — |
| W1 | Memory | #8640 | 29 | #8652 | 0 critical, 0 findings |
| W2 | Context Assembly | #8641 | 32 | #8653 | 2 low |
| W3 | GSD | #8642 | 74 | #8654 | 1 low, 2 info |
| W4 | MAS | #8643 | 68 | #8655 | 4 info |
| W5 | Tools | #8644 | 66 | #8656 | 1 medium, 1 low, 3 info |
| W6 | Session | #8645 | 66 | #8657 | 1 low, 4 info |
| W7 | Credentials | #8646 | 77 | #8658 | 1 low, 5 info |
| W8 | TUI | #8647 | 101 | #8659 | 1 low, 7 info |
| W9 | Extension | #8648 | 72 | #8663 | 8 info |
| W10 | Integration | #8649 | 74 | #8664 | 7 info |
| **Total** | | | **663** | **11 PRs** | **1 med, 6 low, 40+ info** |

## All Findings Summary

### Medium Severity (1)

| ID | Wave | Subsystem | Description |
|----|------|-----------|-------------|
| W5-FINDING-001 | W5 | Tools | `risk-severity?`, `token-type?`, `risk-type?` predicates use `member` which returns a list (truthy), not a boolean. Contract is `(-> any/c boolean?)` but returns list. Calling these predicates from outside the module raises a contract violation. |

### Low Severity (6)

| ID | Wave | Subsystem | Description |
|----|------|-----------|-------------|
| W2-FINDING-001 | W2 | Context Assembly | `build-tiered-context` uses windowed selection that drops middle messages by design. |
| W3-FINDING-001 | W3 | GSD | `find-transition-path` returns `'()` (empty list) instead of `#f` when from==to state. |
| W5-FINDING-002 | W5 | Tools | `make-default-pipeline` includes validation middleware with default `lookup-fn` `(lambda (_) #f)`, causing ALL tool calls to be rejected as "unknown tool". |
| W6-FINDING-001 | W6 | Session | Setter functions exposed in internal submodule rather than being truly private. |
| W7-FINDING-001 | W7 | Credentials | `valid-oauth-config?` only checks `client-id` non-empty and `redirect-port` positive — does NOT validate that `authorize-url` or `token-url` are non-empty strings. |
| W8-FINDING-001 | W8 | TUI | `tokenize` returns 2 values (`values cmd args`) but its contract promises 1 value (`-> string? any/c`). Works internally but breaks when called from outside the module. |

## Architecture Strengths Identified

### 1. Thread Safety Throughout
- Extension registry: semaphore-guarded boxes with list + hash index
- Provider registry: semaphore-guarded boxes with config merging
- Process tracking: semaphore-guarded counter with limit enforcement
- Event subscription tracking: semaphore-guarded hash
- Quarantine state: file locking + atomic writes

### 2. Defense-in-Depth Security
- Path traversal protection in manifests (`safe-manifest-file-path?`)
- Extension tier system (5 cumulative privilege levels)
- Sandbox resource limits with stricter-wins merge
- Credential masking, atomic writes, #o600 permissions
- Shell risk classification with structured + regex modes
- Policy-aware credential backends (keychain-required, env-only)

### 3. Comprehensive Input Validation
- Manifest validation (semver, API version, type, path safety)
- Hook action schema (60+ hook points with per-point validation)
- Extension guard struct validation
- IPC protocol deserialization with graceful failure
- Settings type coercion (string→boolean, string→number, string→symbol)

### 4. Clean Architecture Layers
- Pure types in foundation layer (`util/`)
- Runtime types in separate modules (no upward dependencies)
- Extension context is read-only
- 3-layer GSD design (pure logic → stateful → interface)
- Typed Racket boundary at hook-types

### 5. Robust Error Handling
- Retry system with per-type budgets and structured error wrapping
- Hook dispatch with criticality-based defaults (safety vs liveness)
- Graceful degradation for missing config, corrupted state, timeout
- `retry-exhausted` struct with full error history

### 6. Unicode and Internationalization
- Char-width follows UAX #11 (East Asian Width)
- Grapheme cluster utilities follow UAX #29
- Correct handling of CJK, combining marks, zero-width, emoji

## Test Infrastructure

- **Smoke gate**: 19 files, 286 tests, ~1 min — green during v0.99.45, but did not include the v0.99.45 audit tests.
- **Audit tests**: 11 v0.99.45 audit test files, 663 test cases, and 1,626 check assertions. These are fast/default suite tests and must be verified with the project test runner rather than inferred from the smoke gate.
- **Fast/default suite**: authoritative gate for v0.99.45 audit test inclusion.
- **Broad gate**: available but not the evidence used for the v0.99.45 audit acceptance claim.
- **Mock provider**: Used for all audit tests (no real API keys on VPS)
- **Test pattern**: Each wave produces test file + audit report, committed to repo

## Remediation Status

v0.99.45 documented findings but did not file remediation during the v0.99.45 milestone. Follow-up remediation was filed in v0.99.46 as #8666–#8671.

| Finding | v0.99.46 remediation |
|---------|----------------------|
| W5-FINDING-001 | #8666 — fixed in W1; member-based predicates now return exact booleans. |
| W8-FINDING-001 | #8667 — fixed in W2; `tokenize` now has a stable two-value contract. |
| W7-FINDING-001 | #8668 — fixed in W2; OAuth config validation rejects empty authorize/token URLs. |
| W3-FINDING-001 | #8669 — clarified in W2; same-state transition path is explicit zero-hop `()`, distinct from `#f` no-path. |
| CI/doc/report drift | #8670/#8671 — fixed in W0/W1; README metrics and report/test truth restored. |

## Conclusion

The Pi-Q agent v0.99.45 audit found no critical or high-severity issues and documented strong architectural patterns throughout the codebase. However, v0.99.45 was audit-only, not a released version, and the final report must not use the smoke gate as proof that the audit tests ran. The corrected evidence is 663 test cases and 1,626 check assertions in the v0.99.45 audit tests, with follow-up remediation tracked and completed through the v0.99.46 remediation milestone.
