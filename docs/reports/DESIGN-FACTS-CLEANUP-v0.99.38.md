# Design-Facts Cleanup — v0.99.38 W9

**Date:** 2026-06-23
**Issue:** #8483
**Base:** `main@a2ce9691`

## Purpose

Add design-fact comments to source files where v0.99.38 W0–W8 audits found
hidden assumptions, known bugs, or non-obvious invariant provenance. Each
comment ties to a concrete audit finding — no cosmetic churn.

## What Was Done

### Design-Fact Comments Added

6 source files received targeted `DESIGN FACT` comments:

| File | Audit Finding | Comment Purpose |
|------|--------------|-----------------|
| `scripts/run-tests/parse.rkt` | W3 F1 | FAILURE-END regex `#rx"^-{20,}$"` bug: `{20,}` is literal in `#rx` mode |
| `util/config-paths.rkt` | W6 | `global-config-dir` consolidation rationale: ~19 modules still inline |
| `scripts/abstraction-audit.rkt` | W8 | Signal provenance: each regex maps to a concrete audit wave |
| `scripts/metrics-helpers.rkt` | W5 | Pure/shell boundary pattern: establishes the extraction template |
| `sandbox/subprocess.rkt` | W6 | `sanitize-env` mutation boundary: pure predicate in helpers |
| `gui/main.rkt` | W6 | `gui-available?` environment gate: DISPLAY/WAYLAND_DISPLAY assumption |

### Design Principles Followed

1. **Every comment ties to a concrete finding** — each references a specific
   audit wave (W1, W3, W5, W6, W8) and the invariant/bug it documents.
2. **No cosmetic churn** — no file headers rewritten, no broad renaming,
   no comment blocks that repeat what code already says.
3. **No RED module edits** — cli/args.rkt, wiring/run-modes.rkt,
   agent/event-structs.rkt, scripts/run-tests.rkt were NOT modified.
4. **Comments document WHY, not WHAT** — the focus is on non-obvious
   decisions, known gaps, and audit provenance.

### New Tests

**10 new tests** in `tests/test-design-fact-comments.rkt`:

- `design-fact-comment-tests` (6 tests): Verify each DESIGN FACT comment exists
- `design-fact-content-tests` (4 tests): Verify comment content references
  the right audit findings (#px fix, provider-factory, --check-only, all 4 signal origins)

### Pre-Existing Well-Documented Modules

The following modules already had excellent design-fact comments and were
NOT modified (to avoid cosmetic churn):

- `extensions/gsd/transition-logic.rkt` — L-09 transition table design note,
  MAS integration point, enriched table explanation
- `runtime/provider/provider-factory.rkt` — RFC 1918 ranges, F2 host matching,
  F12 architecture note
- `scripts/metrics.rkt` — pure/shell split, --check-only flag, write-or-check

## RED Module Gap Documentation (W7 E6/E7)

The W7 audit found that `cli-config->runtime-config` in `cli/args.rkt`
(RED module) does NOT propagate `context-profile`, `agent-pool`,
`parallel?`, or `keybindings-path` to the runtime config hash. These
fields are parsed by the CLI but dropped during the hash conversion.

This gap is documented here rather than in the source code because
`cli/args.rkt` is a RED module (read-only). Future remediation should
add these fields to the `let*` chain in `cli-config->runtime-config`.

## Acceptance Criteria Met

- [x] Small report documenting design-fact additions
- [x] All source comments tie to concrete audit findings
- [x] Tests verify comment presence and content
- [x] No RED module source modified
- [x] No broad renaming or file header churn
- [x] Fast local passes (smoke gate)
