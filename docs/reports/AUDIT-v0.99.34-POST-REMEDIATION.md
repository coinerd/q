# v0.99.34 Post-Remediation Audit Report

**Date**: 2026-06-20  
**Milestone**: #820 (v0.99.34 — Remaining Test Debt Closeout)  
**Parent issue**: #8377  
**Final verification wave**: W3 (#8381)  
**Verified commit**: `main@9c1cfbc1` before this report  
**Working dir**: `/home/user/src/q-agent/q/`  
**Profile**: `local`  

## Scope

v0.99.34 closed the remaining post-v0.99.33 test/release-gate debt:

1. direct/focused `tests/test-tool-edit-builtin.rkt` flake;
2. subprocess stdout/stderr pipe-drain deadlock;
3. stale ledger/report truth claims;
4. final clean local gates from fresh bytecode.

## Waves completed

| Wave | Issue | PR | Commit | Result |
|------|-------|----|--------|--------|
| W0 | #8378 | #8382 | `996b14e8` | Fixed edit-builtin no-match/direct flake while preserving existing `not found` wording and explicit `0 times` count. |
| W1 | #8379 | #8383 | `d2565bc8` | Fixed subprocess pipe-buffer deadlock by draining stdout/stderr concurrently with bounded reader threads; preserved background-child inherited-pipe behavior. |
| W2 | #8380 | #8384 | `9c1cfbc1` | Corrected stale ledger and v0.99.33 audit-report truth claims; marked ledger markdown historical and current JSON ledger empty. |
| W3 | #8381 | #8385 | `b709f7ee` | Ran final gates and recorded exact local evidence. |

## Final gate results from W3

All commands were run from `feature/v09934-w3-final-audit` branched from clean `main@9c1cfbc1`.

### 1. Fresh bytecode build

```bash
find . -name compiled -type d -prune -exec rm -rf {} +
raco make main.rkt
```

- Exit code: 0
- Result: **PASS**

### 2. Unit-fast gate

```bash
racket scripts/run-tests.rkt --suite unit-fast --mode in-process --profile local
```

- Exit code: 0
- Files: `10 total`, `10 passed`, `0 failed`, `0 timeouts`
- Tests: `102 total`, `102 passed`, `0 failed`
- Category: `PASS=10`
- Verdict: **PASS**

### 3. Smoke gate

```bash
racket scripts/run-tests.rkt --suite smoke --profile local
```

- Exit code: 0
- Files: `19 total`, `19 passed`, `0 failed`, `0 timeouts`
- Tests: `286 total`, `286 passed`, `0 failed`
- Category: `PASS=19`
- Verdict: **PASS**

### 4. Edit-builtin repeat check

```bash
for i in $(seq 1 20); do raco test tests/test-tool-edit-builtin.rkt || break; done
```

- Exit code: 0
- Iterations: `20/20`
- Result: **PASS**

### 5. Focused 9-file target set

```bash
racket scripts/run-tests.rkt --profile local \
  tests/test-doctor.rkt \
  tests/test-tool-edit-builtin.rkt \
  tests/test-gui-state-sync-w0.rkt \
  tests/test-llm-error-visibility.rkt \
  tests/test-loop-edge-cases.rkt \
  tests/test-session-config-helpers.rkt \
  tests/test-struct-mutability.rkt \
  tests/test-ci-local.rkt \
  tests/test-spawn-subagent-serialization.rkt
```

- Exit code: 0
- Files: `9 total`, `9 passed`, `0 failed`, `0 timeouts`
- Tests: `63 total`, `63 passed`, `0 failed`
- Category: `PASS=9`
- Verdict: **PASS**

### 6. Required fast gate

```bash
racket scripts/run-tests.rkt --suite fast --profile local
```

- Exit code: 0
- Files: `948 total`, `948 passed`, `0 failed`, `0 timeouts`
- Tests: `12873 total`, `12873 passed`, `0 failed`
- Category: `PASS=948`
- Verdict: **PASS**
- Post-report rerun: also **PASS** (`948/948 files`, `12873/12873 tests`) after this audit file was written.

### 7. Broad local ledger gate

```bash
timeout 900 racket scripts/run-tests.rkt --suite broad --profile local --ledger tests/test-suite-ledger.json
```

- Exit code: 0
- Files: `1040 total`, `1040 passed`, `0 failed`, `0 timeouts`
- Tests: `13801 total`, `13801 passed`, `0 failed`
- Category: `PASS=1040`
- Verdict: **PASS**
- Known failures: `0`
- New failures: `0`
- Unclassified failures: `0`
- Resolved known failures: `0`
- Release-blocking known failures: `0`

## Ledger status

`tests/test-suite-ledger.json` currently has no active entries. The markdown ledger report is now explicitly historical and points to the JSON ledger as the current source of truth.

## Audit verdict

**APPROVED for the local v0.99.34 gate profile.**

The direct/focused flake is fixed, the subprocess pipe-drain production debt is fixed, stale report/ledger claims have been corrected, and final local fast + broad-ledger gates are clean with no known, new, unclassified, or release-blocking failures.
