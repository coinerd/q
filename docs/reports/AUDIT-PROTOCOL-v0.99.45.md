# Audit Protocol — v0.99.45 Real-World Subsystem Audit

**Date:** 2026-07-04
**Version:** v0.99.45
**Milestone:** #832

## Purpose

This document defines the standardized audit procedure for each subsystem wave
(W1–W10) in the v0.99.45 milestone. Every wave must follow this protocol to
ensure consistent, reproducible, and evidence-backed findings.

## User Personas

| Persona | Use Case | Key Subsystems |
|---------|----------|----------------|
| **Solo Developer** | Code, debug, refactor a small project | Tools, context, memory, TUI |
| **Technical Lead** | Plan and execute multi-wave project work | GSD, MAS, session, tools |
| **Power User** | Custom extensions, memory backends, credential management | Memory, auth, extensions, compaction |

## Audit Procedure

### Step 1: Documentation Review
- Read the user docs for the subsystem being audited
- Identify documented behaviors, commands, and configuration options
- Note any stated limitations or known issues

### Step 2: Configuration Matrix
- Define the configuration combinations to test
- For each combination, specify expected behavior

### Step 3: Workflow Execution
- Exercise the subsystem with realistic user task workflows
- Use the tmux-q-harness to capture structured evidence
- Document exact prompts, inputs, and responses

### Step 4: Trace Evidence Collection
- Collect `trace.jsonl`, `session.jsonl`, and other artifacts
- Verify structured events match expected patterns
- Note any missing or unexpected events

### Step 5: Behavioral Verification
- Compare actual behavior against documented behavior
- Classify any discrepancy as a finding

### Step 6: Edge Case Probing
- Test boundaries (empty input, very long input, special characters)
- Test error conditions (missing files, invalid config, network failure)
- Test unusual but realistic scenarios

### Step 7: Findings Documentation
- Write audit report using the template below

## Audit Report Template

```markdown
# Audit: [Subsystem Name]

## Configuration
- Provider: [real/mock]
- Model: [model name]
- Memory backend: [backend]
- Session dir: [temp dir]
- Special flags: [any]

## Test Matrix
| Test | Expected | Actual | Status | Evidence |
|------|----------|--------|--------|----------|
| ... | ... | ... | pass/fail | trace ref |

## Findings
- [FINDING-001]: [description]
  - Severity: critical/high/medium/low/info
  - Category: bug/friction/gap/docs
  - Reproducible: yes/no
  - Evidence: [trace artifact path]
  - Recommendation: [action]

## Trace Evidence Summary
- Events collected: [count]
- Key phases: [list]
- Anomalies: [list]

## Remediation Items
- Issue #[N]: [title]
```

## Finding Severity Levels

| Severity | Definition |
|----------|-----------|
| Critical | Data loss, crash, security issue |
| High | Incorrect results, broken feature |
| Medium | Usability friction, missing convenience |
| Low | Minor cosmetic or documentation issue |
| Info | Observation, no action needed |

## Finding Categories

| Category | Definition |
|----------|-----------|
| Bug | Behavior contradicts documented or expected behavior |
| Friction | Feature works but is hard or confusing to use |
| Gap | Documented feature is missing or incomplete |
| Docs | Documentation is wrong or misleading |

## Provider Mode

- **Real provider** (`Q_TMUX_REAL_PROVIDER=1`): Use when the subsystem requires
  actual LLM reasoning to exercise (memory retrieval quality, GSD planning).
- **Mock provider**: Use when the subsystem can be fully tested with synthetic
  responses (tool execution, session lifecycle, TUI rendering).

## Gate Requirements

Every wave must:
1. Run smoke gate (`racket scripts/run-tests.rkt --suite smoke --profile local`)
2. Keep it green (19/19 files, 286/286 tests)
3. Write audit report to `docs/reports/AUDIT-v0.99.45-W<N>-<subsystem>.md`
4. File issues for any critical/high findings

## Artifact Policy

- All test sessions use temp HOME/project directories
- No real user data is accessed
- All artifacts are checked for credential leakage before publishing
- Credential patterns: `sk-ant-`, `Bearer`, `api_key`, `API_KEY`, `ANTHROPIC_API_KEY`
