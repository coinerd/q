# W4: CI Architecture Gate + Version Bump

**Version:** 0.22.9
**Dependencies:** W3 complete
**Effort:** S (~30 min)
**Delivery:** Either
**Findings:** EXT-04 (P1-4)

## Tasks

### T1: Create scripts/arch-report.rkt

**File:** `scripts/arch-report.rkt` (new)
**Action:** Architecture fitness report script.

**Features:**
1. Scan all source `.rkt` files, report line counts
2. Flag modules exceeding 600-line threshold
3. Report stability annotation coverage
4. List known-large exceptions with rationale
5. `--ci` flag: exit 1 if any violations found

**Threshold:** 600 lines (lowered from 900 after W1/W2/W3 decompositions)

**Known-large exceptions** (expected after W1/W2/W3):
- `agent/event-types.rkt` (~852 lines) — data-only module, pure struct/constant definitions
- `agent/loop.rkt` (~853 lines) — core agent loop, tightly coupled state machine

### T2: Add arch-report step to CI

**File:** `q/.github/workflows/ci.yml`
**Action:** Add after "Dependency completeness" step:

```yaml
- name: Architecture fitness
  run: racket scripts/arch-report.rkt --ci
```

### T3: Update CHANGELOG.md

**Action:** Add v0.22.9 entry (see plan §W4 T3 for full text)

### T4: Version bump

**Action:** Run `racket scripts/sync-version.rkt --write --all`
- Updates `util/version.rkt`: 0.22.8 → 0.22.9
- Updates `info.rkt`, `README.md`

### T5: Final verification

```bash
racket scripts/run-tests.rkt          # full suite
racket scripts/arch-report.rkt --ci   # fitness gate
racket scripts/check-deps.rkt         # dependency check
racket scripts/sync-version.rkt --check --all  # version sync
```

**Expected:** All checks pass. 409+ files, 6500+ tests, 0 failures.

## Module Size Targets (post W1/W2/W3)

| Module | Before | After | Status |
|--------|--------|-------|--------|
| agent-session.rkt | 795 | ~120 | ✅ Under 600 |
| session-store.rkt | 894 | ~150 | ✅ Under 600 |
| sdk.rkt | 763 | ~100 | ✅ Under 600 |
| event-types.rkt | 852 | 852 | ⚠️ Known-large (data-only) |
| agent/loop.rkt | 853 | 853 | ⚠️ Known-large (core loop) |

All other modules already under 600 lines.
