# Session/TUI/Settings Representation Boundary Audit — v0.99.36 W8

**Date:** 2026-06-22
**Wave:** W8 (#8421)
**Risk Assessment:** GREEN (reward 11, risk 4)
**Modules audited:** `runtime/session/session-config.rkt`, `tui/state-types.rkt`,
  `runtime/settings-core.rkt`, `runtime/settings-query.rkt`, `tui/input/state-types.rkt`,
  `tui/state-events/helpers.rkt`

---

## 1. Session-Config Representation (runtime/session/session-config.rkt)

**Lines:** ~310
**Struct:** `session-config` wrapping immutable hash via `gen:dict`

### Assessment: WELL-DESIGNED

The `session-config` struct implements `gen:dict` with full iteration protocol
support (`dict-iterate-first/next/key/value`), enabling transparent `dict-ref`
and `for/hash` access from all consumers. This is the correct boundary pattern:

- **Consumers use `dict-ref`** (not `hash-ref`) for typed, default-aware access.
- **27 smart accessors** (`config-*`) provide typed defaults without leaking the
  hash structure.
- **`hash->session-config` normalizes** keys (validates known keys, coerces
  `thinking-level` strings to symbols, logs warnings for unknown keys).
- **`session-config->hash`** enables round-trip conversion for serialization.

### Findings

| Finding | Severity | Notes |
|---|---|---|
| 27 accessors all follow same `hash-ref` pattern | LOW | Repetitive but uniform — risk of typo is low |
| `normalize-session-config-hash` warns but preserves unknown keys | LOW | Intentional — forward compat |
| Profile activation matrix (`apply-context-assembly-profile!`) has 5 cases × 6 flags = 30 lines | LOW | Data-driven table would be cleaner but current form is clear |

### Verdict: No changes needed. The gen:dict pattern is exemplary.

---

## 2. TUI State Types (tui/state-types.rkt)

**Lines:** ~410
**Structs:** `ui-state` (24 fields), `streaming-state` (8 fields), `transcript-entry` (5 fields),
  `overlay-state` (8 fields), `selection-state` (2 fields), `cache-state` (2 fields),
  `tree-browser-state` (4 fields), `branch-info` (5 fields), `goal-display-info` (4 fields)

### Assessment: WELL-STRUCTURED with one documentation defect

#### Finding 2.1: ui-state field comment mismatch (BUG)

**Severity:** LOW (documentation only, no runtime impact)

The `ui-state` struct has 24 fields. The last 4 fields before `active-goal` are:

```racket
         context-tokens             ; (or/c #f integer?) — ... (v0.19.12 W1)
         cost-tracker               ; ← NO COMMENT
         context-pressure-level     ; ← NO COMMENT
         context-pressure-percent   ; (or/c #f cost-tracker?) — mutable cost accumulator (G8.4)
```

The comment `; (or/c #f cost-tracker?) — mutable cost accumulator (G8.4)` is attached
to `context-pressure-percent`, but it belongs on `cost-tracker`. The fields
`context-pressure-level` and `context-pressure-percent` have no documentation.

**Fix:** Move the comment to `cost-tracker`, add proper comments for the
two pressure fields.

#### Finding 2.2: Explicit struct exports (GOOD pattern)

`ui-state`, `streaming-state`, `transcript-entry`, `overlay-state`, `branch-info`
use explicit field-by-field exports instead of `struct-out`. This preserves
`struct-copy` transparency while avoiding accidental exposing of the constructor
pattern. The `selection-state` and `cache-state` structs use `contract-out`
because they are NOT used with `struct-copy` externally — correct distinction.

#### Finding 2.3: Render cache embedded in state (ARCHITECTURE NOTE)

The architecture note at the top correctly documents WHY render cache functions
can't be extracted: `struct-copy ui-state` requires the struct definition at
compile time. Any extraction would create a circular dependency. This is a known
constraint, not a defect.

### Verdict: Fix Finding 2.1 (comment mismatch).

---

## 3. Settings Representation (runtime/settings-core.rkt + settings-query.rkt)

**Lines:** settings-core.rkt ~200, settings-query.rkt ~550
**Struct:** `q-settings` (3 fields: global, project, merged)

### Assessment: WELL-STRUCTURED with one duplication issue

#### Finding 3.1: Boolean coercion duplication (12 instances)

**Severity:** MEDIUM (maintainability — every new boolean setting repeats the pattern)

12 settings query functions repeat the exact same boolean coercion:

```racket
(define (setting-X-enabled? settings)
  (define raw (setting-ref* settings '(path) default))
  (cond
    [(boolean? raw) raw]
    [(string? raw) (and (member (string-downcase raw) '("true" "1" "yes")) #t)]
    [else default]))
```

The functions affected:
`setting-memory-auto-extraction-enabled?`, `setting-memory-user-scope-enabled?`,
`setting-memory-auto-reflection-enabled?`, `setting-reflection-prompt-enabled?`,
`blackboard-enabled?`, `hot-swap-enabled?`, `auto-reload-enabled?`, `mcp-enabled?`,
`mcp-server-enabled?`, `broker-enabled?`, `verifier-enabled?` (variant with #t default),
`setting-memory-auto-extraction-enabled?` (variant).

**Fix:** Extract a `coerce-config-boolean` helper and replace all 12 instances.

#### Finding 3.2: q-settings struct design (GOOD)

The `q-settings` struct with `(global project merged)` is clean:
- `global` and `project` are `#:INTERNAL` — access via `merged` only.
- `merged` is the deep-merged result.
- `deep-merge-hash` handles nested hash merging correctly.
- Settings cache memoizes by `(project-dir, home-dir)` + mtime check.

#### Finding 3.3: settings-query.rkt is large but uniform

~550 lines, 40+ query functions. Each function follows a consistent pattern:
`(setting-ref* settings path default)` + optional coercion. The uniformity
makes it easy to navigate, and the functions are individually small (5-10 lines).

---

## 4. TUI Input State (tui/input/state-types.rkt)

**Lines:** ~260
**Struct:** `input-state` (9 fields)

### Assessment: WELL-STRUCTURED

Clean separation: pure data definitions (structs) separated from logic
(editing-ops, history-ops). The `input-state` struct uses `#:transparent`
and explicit field exports. Mouse event parsing, IME cursor markers, and
horizontal scroll are well-documented with protocol references.

### Verdict: No changes needed.

---

## 5. TUI State Event Helpers (tui/state-events/helpers.rkt)

**Lines:** ~100
**Functions:** 6 pure helpers

### Assessment: WELL-STRUCTURED

Extracted helpers are pure functions: `classify-error-type`, `format-error-hint`,
`append-entry`, `recent-tool-start?`/`recent-tool-end?`. Clean separation of
concerns.

### Verdict: No changes needed.

---

## 6. Summary of Changes

| Change | Module | Risk | Effort |
|---|---|---|---|
| Extract `coerce-config-boolean` helper | settings-query.rkt | LOW (identical behavior) | SMALL |
| Fix ui-state field comment mismatch | state-types.rkt | ZERO (docs only) | TRIVIAL |

Both changes are narrow, low-risk, and improve maintainability without
altering runtime behavior.

---

## 7. Patterns Observed

### GOOD patterns (retain)
1. **gen:dict wrapping** (session-config): Provides transparent dict protocol
   while hiding the hash structure.
2. **Explicit struct exports** (tui/state-types): Field-by-field exports instead
   of struct-out for structs used with struct-copy.
3. **Settings query pattern** (settings-query): Each setting is a single
   function with `setting-ref*` + default + optional coercion.
4. **Layered settings** (settings-core): global → project → merged with
   deep-merge and mtime-based caching.
5. **Forward declarations** (state-types): `styled-line?` and `q-component?`
   declared as compatible predicates to break circular dependencies.

### Issues found (fixed)
1. **Boolean coercion duplication** → extracted to `coerce-config-boolean`.
2. **Field comment mismatch** → corrected.
