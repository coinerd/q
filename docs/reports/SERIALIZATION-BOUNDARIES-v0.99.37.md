# Serialization Boundaries — v0.99.37

**Date:** 2026-06-22
**Wave:** W3 (#8443)

---

## §28 Principle: Serialized representation separate from in-memory

Each serialization domain must have:
1. A single owner module (serialize + deserialize)
2. Round-trip property tests (serialize → deserialize → equality)
3. Invalid-input handling (malformed JSON degrades gracefully, no crash)

---

## Domain Map

### 1. Event JSON Codec
- **Owner:** `agent/event-json.rkt`, `agent/event-json-helpers.rkt`
- **Functions:** `typed-event->jsexpr`, `jsexpr->typed-event`
- **Round-trip tests:** ✅ `tests/test-event-json-round-trip.rkt`
- **Invalid-input tests:** ❌ MISSING (added in this wave)
- **Risk:** `jsexpr->typed-event` called `lookup-event-deserializer` with `#f` when type key was missing, causing contract violation crash. **FIXED in this wave** — added guard `(and type (lookup-event-deserializer type))`.

### 2. Scrollback (Transcript) Codec
- **Owner:** `tui/scrollback.rkt`
- **Functions:** `transcript-entry->jsexpr`, `jsexpr->transcript-entry`
- **Round-trip tests:** ✅ `tests/test-tui-scrollback-roundtrip.rkt`
- **Invalid-input tests:** ❌ MISSING (added in this wave)
- **Risk:** `jsexpr->transcript-entry` calls `string->symbol` on `kind` without validation. Non-string `kind` crashes. Missing keys have defaults (safe).

### 3. GSD Event Codec
- **Owner:** `extensions/gsd/` event modules
- **Functions:** GSD-specific serializers/deserializers
- **Round-trip tests:** ✅ `tests/test-gsd-event-codec-roundtrip.rkt`
- **Invalid-input tests:** Partial (tests transition-rejection, not input-format rejection)

### 4. Typed Event Codec
- **Owner:** `agent/event-structs/` + `agent/event-json.rkt`
- **Round-trip tests:** ✅ `tests/test-typed-event-codec-roundtrip.rkt`
- **Invalid-input tests:** ❌ MISSING

### 5. Spawn Subagent Serialization
- **Owner:** `tools/builtins/spawn-subagent.rkt`, `spawn-subagent-helpers.rkt`
- **Round-trip tests:** ✅ `tests/test-spawn-subagent-serialization.rkt`
- **Invalid-input tests:** ❌ MISSING

### 6. Provider Vision Serialization
- **Owner:** `llm/` provider modules
- **Round-trip tests:** ✅ `tests/test-provider-vision-serialization.rkt`
- **Invalid-input tests:** ❌ MISSING

### 7. Context Assembly Serialization
- **Owner:** `runtime/context-assembly/`
- **Round-trip tests:** ✅ `tests/test-context-assembly-serialization.rkt`
- **Invalid-input tests:** ❌ MISSING

### 8. Session Store Integrity
- **Owner:** `runtime/session/session-store-integrity.rkt`
- **Functions:** Hash chain verification, event hashing, write-ahead markers
- **Round-trip tests:** N/A (integrity verification, not serialization)
- **Invalid-input tests:** Partial (corruption repair exists)

### 9. RPC Mode JSON
- **Owner:** `interfaces/rpc-mode.rkt`
- **Functions:** `read-json` + `write-json` for RPC protocol
- **Round-trip tests:** Indirect (via `test-json-mode.rkt`)
- **Invalid-input tests:** Partial (has `with-safe-fallback` wrapper)

### 10. Extension Manifest
- **Owner:** `extensions/loader.rkt`, `extensions/manifest.rkt`
- **Functions:** Manifest JSON read/write
- **Round-trip tests:** Indirect (via `test-manifest.rkt`)
- **Invalid-input tests:** ❌ MISSING

---

## Summary

| Domain | Owner Module | Round-trip | Invalid-input |
|--------|-------------|------------|---------------|
| Event JSON | agent/event-json.rkt | ✅ | ✅ Fixed |
| Scrollback | tui/scrollback.rkt | ✅ | ✅ Fixed |
| GSD Events | extensions/gsd/ | ✅ | Partial |
| Typed Events | agent/event-structs/ | ✅ | ❌ |
| Spawn Subagent | tools/builtins/ | ✅ | ❌ |
| Provider Vision | llm/ | ✅ | ❌ |
| Context Assembly | runtime/context-assembly/ | ✅ | ❌ |
| Session Integrity | runtime/session/ | N/A | Partial |
| RPC Mode | interfaces/rpc-mode.rkt | Indirect | Partial |
| Extension Manifest | extensions/loader.rkt | Indirect | ❌ |

**Gap:** 6 domains lack invalid-input tests. This wave adds them for the two
highest-risk domains: Event JSON and Scrollback.

---

## Invalid-Input Patterns Tested

1. **Empty hash:** `(hash)` → should produce a valid default event/entry, not crash
2. **Missing required key:** should use default or return #f, not crash
3. **Wrong type for key:** non-string `kind`, non-number `timestamp` → should degrade gracefully
4. **Extra unknown keys:** should be ignored (forward compatibility)
5. **Non-hash input:** should return #f or a default, not crash

---

## Recommended Future Work

- Harden `jsexpr->typed-event` to validate input is a hash before `hash-ref`
- Harden `jsexpr->transcript-entry` to validate `kind` is a string
- Add invalid-input tests for remaining 4 domains (W9 data structures wave)
