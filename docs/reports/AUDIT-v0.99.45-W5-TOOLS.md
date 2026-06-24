# Audit: Tools Subsystem — v0.99.45 W5

**Date:** 2026-07-04
**Auditor:** Automated test suite
**Provider:** Mock/synthetic (no real API keys)
**Test file:** `tests/test-audit-v09945-w5-tools.rkt`
**Tests:** 66 assertions, 0 failures

## Configuration

- Provider: Mock (no real LLM)
- Modules tested:
  - `tools/tool-struct.rkt` — Tool struct definition (12 fields)
  - `tools/tool.rkt` — Tool facade (make-tool, results, re-exports)
  - `tools/schema-helpers.rkt` — Schema validation and hint formatting
  - `tools/shell-risk.rkt` — Shell command risk classifier
  - `tools/registry.rkt` — Thread-safe tool registry
  - `tools/registry-table/spec.rkt` — Tool spec struct
  - `tools/middleware.rkt` — Composable middleware pipeline
- RED module skipped: `tools/builtins/spawn-subagent.rkt` (741L)
- Special flags: None

## Test Matrix

| # | Test | Expected | Actual | Status |
|---|------|----------|--------|--------|
| 1 | Tool struct fields | 12 fields accessible | ✅ Correct | PASS |
| 2 | Tool with options | All optional fields settable | ✅ Correct | PASS |
| 3 | Tool predicate | Non-tool values rejected | ✅ Correct | PASS |
| 4 | Schema valid | Valid schema accepted | ✅ Correct | PASS |
| 5 | Schema invalid no type | Rejected | ✅ Correct | PASS |
| 6 | Schema invalid wrong type | Rejected | ✅ Correct | PASS |
| 7 | Schema invalid no properties | Rejected | ✅ Correct | PASS |
| 8 | Schema invalid not hash | Rejected | ✅ Correct | PASS |
| 9 | Args valid | Correct args pass | ✅ Correct | PASS |
| 10 | Args missing required | Raises error | ✅ Correct | PASS |
| 11 | Args type mismatch | Raises error | ✅ Correct | PASS |
| 12 | Args non-hash | Raises error | ✅ Correct | PASS |
| 13 | Args no required | Empty args OK | ✅ Correct | PASS |
| 14 | Hint format | Required vs optional formatted | ✅ Correct | PASS |
| 15 | Hint all required | No ? marks | ✅ Correct | PASS |
| 16 | Tokenize simple | Word tokens extracted | ✅ Correct | PASS |
| 17 | Tokenize pipe | Separator tokens | ✅ Correct | PASS |
| 18 | Tokenize redirect | Redirect tokens | ✅ Correct | PASS |
| 19 | Tokenize quotes | Quote tokens | ✅ Correct | PASS |
| 20 | Tokenize substitution | Substitution tokens | ✅ Correct | PASS |
| 21 | Classify rm -rf | Critical destructive | ✅ Correct | PASS |
| 22 | Classify safe command | No findings | ✅ Correct | PASS |
| 23 | Classify mkfs | Critical destructive | ✅ Correct | PASS |
| 24 | Classify git --force | High severity | ✅ Correct | PASS |
| 25 | Classify pipe to bash | Network pipe flagged | ✅ Correct | PASS |
| 26 | Classify substitution | Command substitution flagged | ✅ Correct | PASS |
| 27 | Summary empty | count=0, max=info | ✅ Correct | PASS |
| 28 | Summary with findings | Critical detection | ✅ Correct | PASS |
| 29 | Risk predicates | Contract violation bug documented | ✅ Documented | PASS |
| 30 | Registry make | Empty registry created | ✅ Correct | PASS |
| 31 | Register and lookup | Tool found by name | ✅ Correct | PASS |
| 32 | Lookup missing | Returns #f | ✅ Correct | PASS |
| 33 | Lookup nil | Returns #f | ✅ Correct | PASS |
| 34 | Unregister | Tool removed | ✅ Correct | PASS |
| 35 | Register non-tool error | Raises error | ✅ Correct | PASS |
| 36 | Active management | Filter works correctly | ✅ Correct | PASS |
| 37 | List active tools | Returns filtered list | ✅ Correct | PASS |
| 38 | JSON serialization | tool→jsexpr format correct | ✅ Correct | PASS |
| 39 | Registry snapshot | Snapshot access works | ✅ Correct | PASS |
| 40 | Capability filter | Filters by required-capability | ✅ Correct | PASS |
| 41 | Capability filter any | 'any tools in all filters | ✅ Correct | PASS |
| 42 | Capability filter invalid | Invalid cap raises error | ✅ Correct | PASS |
| 43 | Spec create | Default cap='any | ✅ Correct | PASS |
| 44 | Spec with capability | Custom cap set | ✅ Correct | PASS |
| 45 | Spec handler | Procedure stored | ✅ Correct | PASS |
| 46 | MW compose empty | Pass-through works | ✅ Correct | PASS |
| 47 | MW compose single | Single middleware works | ✅ Correct | PASS |
| 48 | MW safe-mode block | Blocked by safe-mode | ✅ Correct | PASS |
| 49 | MW safe-mode allow | Allowed in safe-mode | ✅ Correct | PASS |
| 50 | MW safe-mode inactive | All pass when off | ✅ Correct | PASS |
| 51 | MW permission allowed | Passes | ✅ Correct | PASS |
| 52 | MW permission blocked | Blocked | ✅ Correct | PASS |
| 53 | MW validation error | Error caught | ✅ Correct | PASS |
| 54 | MW validation pass | Passes | ✅ Correct | PASS |
| 55 | MW validation unknown | Unknown tool blocked | ✅ Correct | PASS |
| 56 | MW default pipeline | Blocks without lookup-fn (documented) | ✅ Documented | PASS |
| 57 | MW hook block | Blocked by hook | ✅ Correct | PASS |
| 58 | MW hook amend | Amended call used | ✅ Correct | PASS |
| 59 | MW hook pass-through | Passes with #f hook | ✅ Correct | PASS |
| 60 | MW hook nil dispatcher | Passes with #f dispatcher | ✅ Correct | PASS |
| 61 | Merge disjoint | Both lists appended | ✅ Correct | PASS |
| 62 | Merge override | Extension appended | ✅ Correct | PASS |
| 63 | Merge dedup extension | Duplicates removed | ✅ Correct | PASS |
| 64 | Success result | Content accessible via result-text | ✅ Correct | PASS |
| 65 | Error result | Error text accessible | ✅ Correct | PASS |
| 66 | Result with details | Details hash preserved | ✅ Correct | PASS |

## Findings

### FINDING-001 (medium): risk-severity?, token-type?, risk-type? violate their own contracts

**Severity:** Medium
**Category:** Bug
**Description:** The functions `risk-severity?`, `token-type?`, and `risk-type?` in `tools/shell-risk.rkt` use `member` to check set membership. `member` returns a list (the tail starting at the found element) when the item is found, not `#t`. However, all three functions have `contract-out` declarations promising `(-> symbol? boolean?)`.

When called with a valid symbol (e.g., `(risk-severity? 'critical)`), `member` returns `'(critical)`, which violates the `boolean?` contract and raises `exn:fail:contract:blame`.

When called with an invalid symbol, `member` returns `#f`, which satisfies `boolean?`.

**Impact:** Medium — any code that calls these predicate functions with valid symbols will crash with a contract violation. The functions are exported and available to external consumers.

**Recommendation:** Change `(member v '(...))` to `(and (member v '(...)) #t)` in all three functions, or use `(memq v '(...))` wrapped in a boolean coercion.

### FINDING-002 (low): Default middleware pipeline blocks all tools without lookup-fn

**Severity:** Low
**Category:** Design
**Description:** `make-default-pipeline` includes a validation middleware with a default `lookup-fn` of `(lambda (_) #f)`. Since the validation middleware checks if the tool exists via `lookup-fn` before allowing execution, the default pipeline rejects ALL tool calls as "unknown tool".

Callers MUST provide a `#:lookup-fn` argument that actually resolves tool names to tool structs.

**Impact:** Low — this is likely intentional (defensive default), but the function name "default-pipeline" implies it should work out of the box. It could confuse developers who try to use it without reading the source.

**Recommendation:** Document this requirement prominently, or change the default `lookup-fn` to always return a dummy tool that passes validation.

### FINDING-003 (info): Shell risk classifier is comprehensive and well-structured

**Severity:** Info (positive finding)
**Category:** Security
**Description:** The `shell-risk.rkt` module provides a robust shell command risk assessment:
- **Tokenizer**: Handles quotes, substitutions, redirects, separators, parens
- **Classifier**: Detects destructive commands (rm -rf, mkfs, dd, shutdown), Windows destructive (format, del), eval/exec indirection, git --force, command substitution, pipe-to-shell
- **Summary**: Provides severity counting with max-severity and critical? flag
- **Risk levels**: 5-tier (info, low, medium, high, critical)
- **Risk types**: 10 categories (destructive, high-risk, network-pipe, substitution, etc.)

**Impact:** Positive — provides comprehensive defense against dangerous shell commands.

### FINDING-004 (info): Tool registry has robust thread-safe design

**Severity:** Info (positive finding)
**Category:** Reliability
**Description:** The tool registry:
- **Thread-safe**: All mutations protected by semaphore
- **Snapshot-based reads**: `with-registry-snapshot` takes shallow copy under lock, preventing mutation during iteration
- **Active tool management**: Supports filtering tools by name set, with `#f` meaning "all active"
- **Capability filtering**: `tools-for-capability` filters by required-capability, with 'any as wildcard
- **JSON serialization**: `tool->jsexpr` converts tools to OpenAI normalized format

**Impact:** Positive — prevents race conditions and enables safe concurrent tool management.

### FINDING-005 (info): Middleware pipeline is composable and well-designed

**Severity:** Info (positive finding)
**Category:** Architecture
**Description:** The middleware system provides:
- **Onion model**: `compose-middleware` creates nested wrappers (first MW's pre-check runs first, post-handle runs last)
- **Hook middleware**: Supports 'block (short-circuit), 'amend (replace call), and pass-through
- **Safe-mode middleware**: Checks safe-mode restrictions before execution
- **Validation middleware**: Schema-based argument validation with tool lookup
- **Permission middleware**: Simple allowed/blocked check
- **Mutation queue middleware**: Queues mutations for sequential execution

**Impact:** Positive — enables flexible, composable tool execution policies.

## Remediation Items

1. **FINDING-001**: Fix `risk-severity?`, `token-type?`, `risk-type?` to return actual booleans. Change `(member v '(...))` to `(and (member v '(...)) #t)`.

## Architecture Summary

The Tools subsystem is organized in five layers:

1. **Definition layer** (`tools/tool-struct.rkt`, `tools/tool.rkt`):
   - 12-field tool struct with transparent printing
   - `make-tool` constructor with validation (name, schema, capability, arity)
   - Default: `externalizable?=#f`, `dangerous?=#f`, `required-capability='any`

2. **Validation layer** (`tools/schema-helpers.rkt`):
   - JSON Schema validation (type=object, properties required)
   - Argument validation (required keys, type matching)
   - Hint formatting for prompts

3. **Risk layer** (`tools/shell-risk.rkt`):
   - Tokenizer for shell commands
   - Risk classifier with 10 risk types and 5 severity levels
   - Summary aggregation

4. **Registry layer** (`tools/registry.rkt`):
   - Thread-safe tool registration with semaphore
   - Active tool management with set-based filtering
   - Capability-based filtering
   - JSON serialization for external API

5. **Execution layer** (`tools/middleware.rkt`, `tools/scheduler.rkt`):
   - Composable middleware pipeline (onion model)
   - Hook, safe-mode, validation, permission, mutation-queue middleware
   - Scheduler for batch execution with ordering preservation
