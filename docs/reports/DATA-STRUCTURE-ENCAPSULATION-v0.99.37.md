# Data Structure / Cache / Index Encapsulation Audit — v0.99.37

**Date:** 2026-06-23
**Wave:** W9 (#8449)
**Scope:** Manual §38, §47 — algorithms depend on operation-level APIs, not raw caches
**Risk Assessment:** GREEN (reward 12, risk 5)

## 1. Executive Summary

This report inventories mutable data structures across the codebase and
assesses their encapsulation against Manual §38 ("algorithms depend on
operation-level APIs, not raw caches") and §47 ("aggregate roots guard
consistency; components don't mutate root internals directly").

The architecture is **healthy**. All mutable state is encapsulated behind
operation-level APIs. No algorithm directly accesses raw cache internals.
Five distinct encapsulation patterns are identified, each with clear
ownership semantics and appropriate thread-safety guards.

## 2. Mutable Data Structure Inventory

### 2.1 Global Mutable Hashes (12 sites)

| Module | Variable | Pattern | Thread-Safe | Clear API |
|--------|----------|---------|-------------|-----------|
| `util/token-estimate-cache.rkt` | `token-cache` | Cache + `clear!` | Single-writer | ✅ `clear-token-estimate-cache!` |
| `runtime/provider/model-registry.rkt` | `model-resolution-cache` | Cache + invalidate | Single-writer | ✅ `invalidate-model-resolution-cache!` |
| `runtime/settings-core.rkt` | `settings-cache` | Memoize + mtime | Single-writer | ❌ No explicit clear |
| `runtime/session/session-migration.rkt` | `migration-registry` | Registry | N/A (init-time) | ❌ No explicit clear |
| `tools/file-mutation-queue.rkt` | `path-locks` | Lock table | ✅ `mutex` | N/A (self-cleaning) |
| `tools/file-mutation-queue.rkt` | `path-semaphores` | Lock table | ✅ `mutex` | N/A (self-cleaning) |
| `tui/state-events/registry.rkt` | `event-reducers` | Registry | ✅ parameter-scoped | ✅ `call-with-test-registry` |
| `util/event/event-macro.rkt` | 4 registries | Registry | ✅ parameter-scoped | ✅ `parameterize` |
| `util/event/event-migration.rkt` | migration registry | Registry | ✅ parameter-scoped | ✅ `parameterize` |

### 2.2 Global Box State (49 sites)

| Category | Count | Pattern | Thread-Safe |
|----------|-------|---------|-------------|
| TUI render-loop state | 12 | Single-thread render | N/A (render thread only) |
| Agent registry | 3 | Semaphore-guarded swap | ✅ |
| Registry watcher | 3 | Custodian-scoped | ✅ |
| Session/TUI channels | 5 | Single-owner | N/A |
| Rate limiters | 2 | Counter state | Documented |
| ID generation | 2 | Semaphore-guarded | ✅ |
| Token cache counters | 2 | Single-writer | Documented |
| Other (one-shot flags, etc.) | 20 | Various | Per-module |

### 2.3 Hash Mutation Sites

| Operation | Count |
|-----------|-------|
| `hash-set!` / `hash-update!` | 60 |
| `set-box!` | 316 |
| `call-with-semaphore` guards | 48 |

## 3. Encapsulation Pattern Classification

### Pattern 1: Semaphore-Guarded Box Swap (HEALTHY)

**Used by:** `agent/registry.rkt`, `util/ids.rkt`

```racket
(define registry (box (hasheq)))
(define registry-semaphore (make-semaphore 1))

(define (register-agent! ...)
  (call-with-semaphore registry-semaphore
    (lambda () ... (set-box! registry new-hash) ...)))
```

**Properties:**
- Immutable hash inside box — updates create new hash, swap atomically
- Semaphore serializes all mutations
- Readers get consistent snapshots
- Prior snapshots remain valid after swap

**Tested by:** `test-data-structure-boundaries.rkt` — box swap preserves snapshot

### Pattern 2: Parameterized Registry (HEALTHY)

**Used by:** `tui/state-events/registry.rkt`, `util/event/event-macro.rkt`

```racket
(define current-event-field-registry (make-parameter (make-hasheq)))
```

**Properties:**
- Thread-local by default (parameters are thread-inherited at creation)
- Test isolation via `parameterize` / `call-with-test-registry`
- No global mutation — each scope gets its own hash
- Cleanest pattern for testability

**Tested by:** W8 `test-tui-event-boundaries.rkt` — registry isolation tests

### Pattern 3: Global Cache with Explicit Clear (HEALTHY)

**Used by:** `util/token-estimate-cache.rkt`, `runtime/provider/model-registry.rkt`

```racket
(define cache (make-hash))
(define (invalidate-cache!) (hash-clear! cache))
```

**Properties:**
- Global mutable hash, accessed via functions only
- Explicit clear/invalidate API
- Single-writer assumption (documented)
- Local cache alternative available (`make-token-estimate-cache`)

**Tested by:** `test-data-structure-boundaries.rkt` — cache isolation + invalidation tests

### Pattern 4: Self-Cleaning Lock Table (HEALTHY)

**Used by:** `tools/file-mutation-queue.rkt`

```racket
(define path-locks (make-hash))
(define path-locks-mutex (make-semaphore 1))
;; Entries auto-removed when count drops to 0
```

**Properties:**
- Mutex-guarded mutations
- Self-cleaning (entries removed when no longer needed)
- No external clear API needed
- Entries are reference-counted

### Pattern 5: One-Shot Flag Box (ACCEPTABLE)

**Used by:** `runtime/safe-mode.rkt`, various TUI render-loop state

```racket
(define safe-mode-lock-one-shot (box #f))
```

**Properties:**
- Single-write-once semantics
- No concurrent mutation risk
- Documented per-module

## 4. Encapsulation Violation Assessment

### 4.1 No Direct Cache Access Found

All caches are accessed through function APIs:
- `cached-estimate-text-tokens` (not `token-cache` directly)
- `resolve-model` (not `model-resolution-cache` directly)
- `load-settings` (not `settings-cache` directly)

**Verdict:** ✅ Manual §38 satisfied — no algorithm pokes raw caches.

### 4.2 No Aggregate Root Violation Found

All registry mutations go through register/unregister functions:
- `register-agent!` / `reset-registry!`
- `register-event-reducer!` / via parameter scope
- `register-tool!` / `unregister-tool!`

**Verdict:** ✅ Manual §47 satisfied — components don't mutate root internals.

### 4.3 Thread-Safety Coverage

Of 12 global mutable hashes:
- 6 are parameter-scoped (thread-local) ✅
- 3 have explicit semaphore/mutex guards ✅
- 3 are single-writer (documented) ⚠️

The 3 single-writer caches (`settings-cache`, `model-resolution-cache`,
`migration-registry`) are accessed only from the main thread during
initialization or model resolution. This is documented but not enforced
by contracts.

## 5. Test Coverage

### 5.1 New Boundary Tests (W9)

`tests/test-data-structure-boundaries.rkt` — 20 tests across 6 suites:

| Suite | Tests | Focus |
|-------|-------|-------|
| model-cache-boundary | 4 | Cache invalidation, resolution, unknown model |
| agent-registry-snapshot | 2 | Registry list, register/reset lifecycle |
| token-cache-isolation | 3 | Local vs global cache, multiple instances |
| id-generator-boundary | 3 | Monotonicity, uniqueness, format |
| settings-cache-boundary | 4 | Minimal settings, merge semantics, deep merge |
| encapsulation-property | 4 | Counter closure, box snapshot, hash immutability |

### 5.2 Existing Test Coverage

- `test-token-estimate-cache.rkt` — 6 tests for hit/miss/clear
- `test-model-registry.rkt` — 429 lines of resolution tests (no cache-specific tests)
- `test-agent-registry.rkt` — registry lifecycle tests
- `test-settings.rkt` — 629 lines (no cache-specific tests)
- `test-ids.rkt` — ID generation tests

### 5.3 Coverage Gap Identified

The model-resolution-cache and settings-cache have no dedicated cache
invalidation tests prior to W9. The W9 tests close this gap for the
model-resolution cache. Settings cache invalidation (mtime-based) remains
untested because it requires filesystem mocking.

## 6. Recommendations

### 6.1 Add `clear-settings-cache!` API (Low Priority)

`settings-cache` in `runtime/settings-core.rkt` has no explicit clear
function. While the mtime-based invalidation handles config file changes,
adding a `clear-settings-cache!` function would enable clean test
isolation and hot-reload scenarios.

### 6.2 Consider `hash` over `hasheq` for String-Keyed Configs (Medium Priority)

Several modules use `hasheq` for config hashes with string keys. Since
`hasheq` uses `eq?` comparison and strings are not guaranteed to be
interned across module boundaries, this can cause silent lookup failures.
Using `hash` (which uses `equal?`) for any hash with string keys is safer.

This was discovered during W9 testing — the model-registry test config
had to use `hash` instead of `hasheq` for the inner provider config
because `flex-ref` creates new strings that are not `eq?` to the
config keys.

### 6.3 Document Single-Writer Assumptions (Low Priority)

Three global caches rely on single-thread access without semaphore guards.
While this is correct for the current single-threaded initialization model,
adding explicit documentation comments (already present for some) ensures
future maintainers don't introduce concurrent access without adding guards.

## 7. Conclusion

The mutable data structure landscape is well-encapsulated. All five
patterns provide appropriate isolation. No algorithm directly accesses
raw cache internals. Thread-safety is handled correctly for all
concurrently-accessed state.

The new boundary tests (20 tests) validate cache invalidation semantics,
registry lifecycle, local cache isolation, ID monotonicity, and
encapsulation properties.
