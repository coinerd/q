# W2: session-store.rkt Decomposition

**Version:** 0.22.9
**Dependencies:** W1 complete
**Effort:** L (~2-3 hours)
**Delivery:** Remote pi + local review
**Findings:** EXT-02 (P0-2)

## Context

`runtime/session-store.rkt` is 894 lines — the largest module in the codebase. It combines append-only write logic, hash-chain integrity verification, tree operations, in-memory session management, forking, import, migration, naming, and versioning. The external audit (P0-2) identified this as the second-highest-priority decomposition target.

## Current Module Map

```
runtime/session-store.rkt (894 lines)
├── Write-ahead markers (lines 76–108): pending-marker-path, write/remove/has-pending-marker
├── Hash chain (lines 111–163): genesis hash, compute-event-hash, read-last-hash, recompute
├── Core append (lines 164–205): append-entry!, append-entries!
├── Session loading (lines 206–228): load-session-log, replay-session
├── Integrity (lines 229–423): verify-session-integrity, repair-session-log!
├── Hash verification (lines 424–486): verify-hash-chain
├── Versioning (lines 487–700): CURRENT-SESSION-VERSION, header write/read, migration
├── Session ops (lines 501–558): import-session!, write-session-name!
├── Fork (lines 558–600): fork-session!
├── In-memory manager (lines 701–765): make-in-memory-session-manager + 6 operations
├── Custom entries (lines 748–768): append-custom-entry!, load-custom-entries
├── Tree store (lines 769–894): append-tree-entry!, load-tree, get-tree-branch, etc.
```

## Target Architecture

```
runtime/session-store.rkt              → façade (~150 lines)
runtime/session-store-append.rkt       → new (~200 lines)
runtime/session-store-integrity.rkt    → new (~250 lines)
runtime/session-store-tree.rkt         → new (~200 lines)
```

## Tasks

### T1: Create runtime/session-store-integrity.rkt

**Extract from session-store.rkt:**
- `GENESIS-HASH` constant (line 111)
- `canonical-jsexpr->string` (line 118)
- `compute-event-hash` (line 134)
- `read-last-hash` (line 141)
- `recompute-hash-chain` (line 153)
- `verify-session-integrity` (lines 229–345)
- `repair-session-log!` (lines 346–423)
- `verify-hash-chain` (lines 424–486)

**Imports:** Only `racket/base`, `racket/port`, `racket/file`, `util/protocol-types` for message?. No imports from other session-store submodules — completely self-contained.

**Provides:** All 8 functions + `GENESIS-HASH` constant.

### T2: Create runtime/session-store-tree.rkt

**Extract from session-store.rkt:**
- `append-tree-entry!` (line 769) — tree append with before/after hooks
- `load-tree` (line 792)
- `get-tree-branch` (line 812)
- `get-children` (line 835)
- `resolve-active-branch` (line 839)
- `tree-info` (line 862)

**Imports:** `racket/base`, `racket/file`, `util/protocol-types`. Clean seam — no dependency on append or integrity.

**Provides:** All 6 tree functions.

### T3: Create runtime/session-store-append.rkt

**Extract from session-store.rkt:**
- Write-ahead markers: `pending-marker-path`, `write-pending-marker!`, `remove-pending-marker!`, `has-pending-marker?` (lines 76–108)
- Core: `append-entry!`, `append-entries!` (lines 164–205)
- Loading: `load-session-log`, `replay-session` (lines 206–228)
- Versioning: `CURRENT-SESSION-VERSION`, `write-session-version-header!`, `read-first-log-entry`, `ensure-session-version-header!`, `migrate-session-log!` (lines 487–700)
- Session ops: `import-session!`, `write-session-name!`, `fork-session!` (lines 501–600)
- Custom entries: `append-custom-entry!`, `load-custom-entries` (lines 748–768)
- In-memory manager: `make-in-memory-session-manager` + 6 operations (lines 701–765)

**Imports from submodules:** `compute-event-hash`, `read-last-hash` from integrity module.

**Provides:** All 22 symbols.

### T4: Convert session-store.rkt to façade

```racket
#lang racket/base
;; runtime/session-store.rkt — session storage (façade)
;; STABILITY: evolving

(require "session-store-append.rkt"
         "session-store-integrity.rkt"
         "session-store-tree.rkt")

(provide (all-from-out "session-store-append.rkt")
         (all-from-out "session-store-integrity.rkt")
         (all-from-out "session-store-tree.rkt"))
```

**Critical:** Verify ALL 35 original exported symbols are available via `(contract-out)` preserved.

### T5: Verify

```bash
raco make runtime/session-store.rkt
raco test tests/test-session-index.rkt
raco test tests/test-agent-session.rkt
raco test tests/test-integration.rkt
racket scripts/run-tests.rkt --suite fast
```

## Expected Module Sizes

| Module | Lines | Notes |
|--------|-------|-------|
| session-store.rkt (façade) | ~150 | Re-exports + in-memory manager |
| session-store-integrity.rkt | ~250 | Hash chain + verification |
| session-store-tree.rkt | ~200 | Tree operations |
| session-store-append.rkt | ~200 | Append, loading, versioning |
