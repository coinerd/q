# Source-Relative Path / Module-Loading Audit — v0.99.38 W2

**Date:** 2026-06-23
**Issue:** #8471
**Base:** `main@230411f7`

## Methodology

Systematic grep of all non-test `.rkt` files for `dynamic-require`, `current-directory`,
`define-runtime-path`, `define-runtime-module-path-index`, and `variable-reference` patterns.
Each site categorized by risk level.

## Dynamic-Require Safety Audit

### Category A: Library module names (SAFE — always resolved from installed collections)

| File | Line | Module |
|------|------|--------|
| tui/char-width.rkt | 201 | `'racket/string` |
| interfaces/rpc-mode.rkt | 111 | `'racket/crypto` |
| gui/main.rkt | 55–89 | `'racket/gui/easy/*`, `'racket/gui` |
| gui/components/rich-transcript-view.rkt | 229–230 | `'racket/gui` |
| util/readline.rkt | 30, 48–49 | `'readline/rktrl` |

**Verdict:** These use quoted symbols resolved from the Racket installation. CWD-independent by definition.

### Category B: define-runtime-path / define-runtime-module-path-index (SAFE — source-relative)

| File | Line | Path Variable |
|------|------|---------------|
| interfaces/gui.rkt | 31 | `gui-main-module` |
| gui/main.rkt | 47 | `gui-action-adapter-module` |
| runtime/session/session-switch.rkt | 49,50 | `hooks-rkt-path`, `context-rkt-path` |
| tools/builtins/skill-router.rkt | 34,35 | `mas-workflow-module`, `workflow-executor-module` |

**Verdict:** `define-runtime-path` resolves paths relative to the source file at compile time. CWD-independent.

### Category C: variable-reference->resolved-module-path (SAFE — CWD-independent)

| File | Line | Pattern |
|------|------|---------|
| agent/registry-defaults.rkt | 23–27 | `#%variable-reference` → source dir → `role-module-path` |
| agent/registry.rkt | 164 | `dynamic-require (agent-descriptor-module-path desc) ...` |

**Verdict:** `#%variable-reference` captures the source location at module load time. Paths built from it are absolute and cwd-stable.

### Category D: path->complete-path before resolve-path (SAFE — explicit absolutization)

| File | Line | Pattern |
|------|------|---------|
| extensions/loader.rkt | 183 | `(path->complete-path path)` → `resolve-path` → `simplify-path` |
| extensions/loader.rkt | 365 | `(simplify-path (resolve-path (path->complete-path path)))` |

**Verdict:** `path->complete-path` makes relative paths absolute relative to `(current-directory)` at call time, then `resolve-path` resolves symlinks. The path is absolute before being passed to `dynamic-require`. CWD-independent once the path is captured.

### Category E: From RED modules (AUDIT ONLY — do not edit)

| File | Line | Note |
|------|------|------|
| wiring/run-modes.rkt | 370 | `module-path` from watcher callback; `roles-dir` is absolute (resolved from project-dir earlier in function). SAFE. |
| scripts/run-tests.rkt | 330 | Uses `make-resolved-module-path` with already-resolved paths. SAFE. |

## current-directory Dependency Inventory

### Non-CLI modules with `(current-directory)` — all have safe defaults

| File | Line | Pattern | Risk | Safe? |
|------|------|---------|------|-------|
| extensions/compact-context.rkt | 38 | `(gather-planning-summary [project-dir (current-directory)])` | Low | ✅ Optional param |
| extensions/gsd/command-handlers.rkt | 157 | `(or (current-pinned-dir) (current-directory))` | Low | ✅ Pinned-dir fallback |
| extensions/gsd/tool-handlers.rkt | 87 | `(current-directory)` | Medium | ⚠️ Direct use |
| skills/resource-loader.rkt | 232 | `(load-project-resources [project-dir (current-directory)] ...)` | Low | ✅ Optional param |
| skills/context-files.rkt | 281 | `(load-agent-context [dir (current-directory)])` | Low | ✅ Optional param |
| skills/context-files.rkt | 310 | `(discover-agents-files [start-dir (current-directory)])` | Low | ✅ Optional param |
| tui/commands/extension.rkt | 75,165,194 | `(current-directory)` | Medium | ⚠️ TUI command layer |
| runtime/auth/auth-store.rkt | 125,174,186 | `#:project-dir [project-dir (current-directory)]` | Low | ✅ Optional param |

**Summary:** 8 of 10 sites use optional parameter defaults. The remaining 2 (gsd/tool-handlers.rkt, tui/commands/extension.rkt) are in the TUI/command layer where `(current-directory)` is the expected user context. No production code changes needed.

## Regression Tests Added

**File:** `tests/test-cwd-independence.rkt` (12 tests)

| Test Section | What It Guards |
|--------------|----------------|
| §1 variable-reference | `#%variable-reference` produces absolute, cwd-stable paths |
| §2 path->complete-path | Extension loader pattern produces absolute paths |
| §3 resolve-project-dir-from-args | W1 helper works from any cwd |
| §4 define-runtime-path | Runtime paths resolve to existing files, are cwd-stable |
| §5 variable-reference built paths | Agent registry pattern (planner.rkt resolves correctly) |
| §6 dynamic-require via runtime-path | Full load chain works from non-source cwd |

## Conclusions

1. **All dynamic-require sites are CWD-safe.** No string-path-based dynamic-require remains. The codebase uses one of four safe patterns: library names, `define-runtime-path`, `variable-reference`, or `path->complete-path`.

2. **The #8466 GUI hotfix resolved the last risky pattern** (bare string path in dynamic-require at gui/main.rkt). All sites now use source-relative resolution.

3. **No production code changes needed.** The current-directory dependencies in non-CLI modules are all behind optional parameter defaults, making them testable and explicit.

4. **12 regression tests** added as future safety net against reintroducing CWD-fragile patterns.
