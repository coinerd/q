# AUDIT v0.99.44 W0 — Baseline and Real-LLM tmux TUI Readiness

**Date:** 2026-06-23  
**Issue:** #8614  
**Milestone:** v0.99.44 — Real-LLM tmux TUI Exploration Hardening  
**Baseline commit:** `91f2a829c8befa6ce11e157962716ba96b1aee13`

## Summary

W0 confirms the repository is ready to begin the v0.99.44 implementation waves.

```text
Mock tmux suite: PASS
Real-provider isolated smoke: PASS
tmux hygiene: PASS
```

## Mock tmux evidence

```bash
Q_TMUX_TUI_TESTS=1 racket scripts/tmux-tui-smoke.rkt
```

Result:

```text
PASS: 7  FAIL: 0  SKIP: 0  TOTAL: 7
```

Scenario files passed:

```text
smoke, artifacts, commands, resize, context, gsd, tools
```

## Real-provider isolated smoke

A temp HOME was created under `/var/tmp/q-v09944-w0-real-home-*`, local q config/credentials were copied there without printing contents, and a single real-provider prompt returned:

```text
V09944_REAL_PROVIDER_READY
```

## Hygiene

```text
tmux kill-server: not used
lingering tmux sessions before/after: none observed
```

## Caveats to resolve in v0.99.44

```text
- structured turn completion is needed; q> is insufficient
- queued prompt detection is needed
- approval automation must be typed, not blind y
- MAS/subagent tests need lifecycle events
- durable memory needs restart/round-trip evidence
```
