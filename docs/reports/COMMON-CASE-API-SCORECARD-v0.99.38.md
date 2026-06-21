# Common-Case API and Repeated-Call-Pattern Scorecard — v0.99.38 W1

**Date:** 2026-06-23
**Issue:** #8470
**Base:** `main@4b12b536`

## Methodology

Systematic `grep` survey of non-test `.rkt` files for repeated patterns that
indicate missing common-case APIs. Each pattern is scored:

- **Reward**: How much call-site complexity / duplication the helper eliminates
- **Risk**: Likelihood of breaking existing behavior
- **Class**: Green (reward ≥ 11, risk ≤ 7), Yellow (reward ≥ 15, risk 8–11), Red (risk ≥ 12)

## Pattern Inventory

### P1. Project-dir resolution from args/config hash

**Occurrences:** 7+ call sites

| File | Line | Pattern |
|------|------|---------|
| extensions/q-sync.rkt | 52-53 | `(hash-ref args 'project_dir (path->string (current-directory)))` |
| extensions/compact-context.rkt | 65 | `(hash-ref args 'project_dir (path->string (current-directory)))` |
| runtime/provider/provider-factory.rkt | 112 | `(or (hash-ref config 'project-dir #f) (current-directory))` |
| wiring/run-modes.rkt | 207 | `(or (hash-ref base-config 'project-dir #f) (current-directory))` |
| extensions/gsd/command-handlers.rkt | 157 | `(or (current-pinned-dir) (current-directory))` |
| extensions/gsd/tool-handlers.rkt | 87 | `(current-directory)` |
| tui/commands/extension.rkt | 75,165,194 | `(current-directory)` |

**Key inconsistency:** Some sites use `'project_dir` (underscore, from JSON args),
others use `'project-dir` (hyphen, from config hash). Values may be path or string.

**Existing helpers:**
- `default-project-dir` in `runtime/settings-query.rkt` — returns `(current-directory)`
- `project-dir-from-settings` in `runtime/settings-query.rkt` — for settings structs only
- `get-project-dir` in `extensions/q-sync.rkt` — local to that module only

**Recommendation:** Add `resolve-project-dir-from-args` to `util/config-paths.rkt`.
Score: **Green** (reward 14, risk 5)

---

### P2. Settings defaulting with symbol/string key ambiguity

**Occurrences:** 6+ call sites

The `(or (hash-ref cfg key-sym #f) (hash-ref cfg key-str default))` pattern appears
because JSON keys arrive as strings but config keys are often symbols.

| File | Line | Pattern |
|------|------|---------|
| runtime/auth/auth-store.rkt | 111 | `(or (hash-ref cfg key-sym #f) (hash-ref cfg key-str default))` |
| runtime/goal/goal-evaluator.rkt | 116 | `(or (hash-ref part 'text #f) (hash-ref part 'content #f) (~a part))` |
| runtime/memory/backends/external-protocol.rkt | 65 | `(hash-ref v 'value (hash-ref v 'data default-value))` |

**Existing helpers:**
- `setting-ref` in `settings-query.rkt` already handles merged settings access
- No generic "hash-ref with symbol/string fallback" helper exists

**Recommendation:** Document but defer. The pattern is scattered and the right
abstraction depends on per-module context. Score: **Yellow** (reward 15, risk 8)

---

### P3. Load-settings with default current-directory

**Occurrences:** 5+ call sites

| File | Line | Pattern |
|------|------|---------|
| interfaces/doctor.rkt | 166 | `(load-settings (current-directory))` |
| interfaces/sessions.rkt | 405 | `(load-settings)` |
| wiring/run-modes.rkt | 599 | `(load-settings (or project-dir (current-directory)))` |
| scripts/benchmark/executor.rkt | 73 | `(load-settings (current-directory))` |

`load-settings` already defaults to `(current-directory)`. The explicit pass-through
is unnecessary. These are callers that don't know the default exists.

**Recommendation:** Document as common-case awareness gap. No wrapper needed —
callers should just call `(load-settings)`. Score: **Green** (reward 12, risk 2)
but implementation is just caller cleanup.

---

### P4. Content part extraction (`'text` / `'content` / fallback)

**Occurrences:** 4+ call sites

```racket
(or (hash-ref part 'text #f) (hash-ref part 'content #f) (~a part))
```

| File | Line |
|------|------|
| runtime/goal/goal-evaluator.rkt | 116 |
| runtime/goal/goal-agent-evaluator.rkt | 87 |
| runtime/tool-coordinator.rkt | 310 |

**Recommendation:** Extract `part->text` helper. Score: **Yellow** (reward 15, risk 9)

---

### P5. Nested hash-ref chains

**Occurrences:** 8+ call sites

```racket
(hash-ref (hash-ref config 'timeouts #f) 'models #f)
```

| File | Line |
|------|------|
| runtime/settings-query.rkt | 153 |
| runtime/goal/goal-codec.rkt | 40 |
| runtime/context-assembly/blackboard-context.rkt | 68, 88 |

**Recommendation:** Document. `setting-ref*` already exists for settings structs.
A generic `hash-ref*` helper is tempting but risky. Score: **Yellow** (reward 16, risk 10)

## Green Implementations for W1

### G1. `resolve-project-dir-from-args` (util/config-paths.rkt)

Consolidates the repeated `(hash-ref args 'project_dir (path->string (current-directory)))`
pattern. Handles both `'project_dir` and `"project_dir"` keys, and both path and string values.

**API:**
```racket
(define (resolve-project-dir-from-args args)
  ...)
```

**Tests:** Validated against all known call-site variants.

## Scorecard Summary

| Pattern | ID | Reward | Risk | Class | W1 Action |
|---------|----|--------|------|-------|-----------|
| Project-dir from args/config | P1 | 14 | 5 | **Green** | Implement G1 |
| Settings symbol/string key | P2 | 15 | 8 | Yellow | Document |
| Load-settings default | P3 | 12 | 2 | **Green** | Document |
| Content part extraction | P4 | 15 | 9 | Yellow | Document |
| Nested hash-ref chains | P5 | 16 | 10 | Yellow | Document |
