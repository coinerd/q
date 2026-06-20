# TEST-SUITE LEDGER — Current Status

**Date:** 2026-06-20  
**Status source:** `tests/test-suite-ledger.json`  
**Audited commit:** `main@b709f7ee`  
**Profile:** `local`

## Current status

The active known-failure ledger is empty:

```json
{"entries": []}
```

Fresh v0.99.34 in-depth audit verification confirmed:

```text
Fast local:          948/948 files, 12873/12873 tests, PASS
Broad local ledger:  1040/1040 files, 13801/13801 tests, PASS
Known failures:      0
New failures:        0
Unclassified:        0
Resolved known:      0
Release-blocking:    0
```

## Source of truth

- Current machine-readable ledger: `tests/test-suite-ledger.json`
- Historical creation/retirement report: `docs/reports/TEST-SUITE-LEDGER.md`
- v0.99.34 final audit: `docs/reports/AUDIT-v0.99.34-POST-REMEDIATION.md`
- In-depth planning audit: `.planning/AUDIT-v0.99.34-IN-DEPTH.md`

## Update policy

If a broad-suite failure reappears:

1. Do not mark the run green if failures are known or ledgered; known failures remain visible.
2. Classify failures in `tests/test-suite-ledger.json` only after triage.
3. Report exact command evidence, profile/environment, and ledger summary.
4. Keep `New=0`, `Unclassified=0`, and `Release-blocking=0` as the local release-gate requirement.
