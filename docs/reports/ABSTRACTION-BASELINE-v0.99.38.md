# Abstraction Baseline — v0.99.38

**Date:** 2026-06-23
**Head:** `main@ce34c68f`
**Predecessor:** v0.99.37 at `main@802aa786` (remediation) + `#8466` GUI hotfix

## Gate Baseline

All gates verified from clean worktree:

```
raco make main.rkt                    → PASS
racket scripts/lint-version.rkt       → PASS, Errors=0
racket scripts/metrics.rkt --lint     → PASS, all 5 static metrics match
racket scripts/metrics.rkt --lint-prose README.md → PASS
racket scripts/ci-local.rkt --quick   → PASS, 5/5
racket scripts/run-tests.rkt --suite smoke --profile local → PASS
  Files: 19, Tests: 286, 0 failures
```

Previous trusted broad evidence (from v0.99.37 remediation #8464):
```
fast local: 966/966 files, 13303/13303 tests
broad local: 1058/1058 files, 14232/14232 tests
Known=0 New=0 Unclassified=0 Release-blocking=0
```

## Module Count

Total source modules: 695
Total test files: 1094

## Top 10 Largest Modules

1. 741L tools/builtins/spawn-subagent.rkt
2. 642L scripts/abstraction-audit.rkt
3. 639L llm/anthropic.rkt
4. 634L cli/args.rkt (RED)
5. 621L wiring/run-modes.rkt (RED)
6. 592L scripts/run-tests.rkt (RED)
7. 562L agent/event-structs.rkt (RED)
8. 549L tui/state-types.rkt
9. 549L tui/terminal.rkt
10. 548L runtime/session/session-store.rkt

## Metrics

| Metric | Value |
|--------|-------|
| Version | 0.99.37 |
| Source modules | 695 |
| Test files | 1094 |
| Source lines | 109,851 |
| Test lines | 174,582 |
| Test assertions | 27,027 |

## Key Differences from v0.99.37

- `scripts/metrics.rkt` now uses `git ls-files` for enumeration (systemic fix)
- GUI adapter module loading fixed (#8466)
- README metrics correct from clean tree
