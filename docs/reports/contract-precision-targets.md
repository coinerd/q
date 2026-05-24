# Contract Precision Target Inventory

Generated: W21 analysis from exact metrics (850 any/c across 145 files)
Baseline: 850 any/c (post-W13 exact baseline was 846, +4 drift)

## Priority Targets (util/* and tools/*)

### Tier 1 — High impact, low risk (struct-heavy modules)

| Module | any/c | Category | Est. Reduction |
|--------|-------|----------|----------------|
| `tools/model-bridge.rkt` | 19 | Request/response struct accessors | 12-15 |
| `util/tree-entries.rkt` | 15 | Branch/tree/summary struct predicates+accessors | 10-12 |
| `tools/scheduler-strategy.rkt` | 15 | Tool result structs (success/failure/denied) | 10-12 |
| `util/event-classes.rkt` | 14 | Event struct predicates | 8-10 |
| `util/entry-predicates.rkt` | 10 | Entry type predicates | 6-8 |

### Tier 2 — Medium impact

| Module | any/c | Category | Est. Reduction |
|--------|-------|----------|----------------|
| `util/sandbox-config.rkt` | 9 | Config struct accessors | 5-7 |
| `util/custom-entries.rkt` | 9 | Custom entry constructors | 5-7 |
| `util/tool-registry-struct.rkt` | 7 | Registry struct | 4-6 |
| `util/loop-result.rkt` | 7 | Loop result struct | 4-6 |
| `util/json-helpers.rkt` | 5 | JSON conversion | 2-3 |

### Pattern Analysis

Most `any/c` in these modules fall into 3 patterns:

1. **Struct predicate contracts** `(-> any/c boolean?)` — These can't be tightened further
   (predicate must accept any input). These are correct uses. ~30% of total.

2. **Struct accessor contracts** `(-> struct-type? any/c)` — Return type is `any/c`
   because struct fields are untyped. Could narrow to actual field types
   (string?, hash?, list?, etc.). ~40% of total. **Primary reduction target.**

3. **Constructor contracts** `(-> any/c any/c ... struct-type?)` — Input `any/c`
   could narrow to actual argument types. ~20% of total. **Secondary target.**

4. **Genuinely polymorphic** — Functions like `hash-ref` wrappers that return
   different types. ~10%. Leave as `any/c`.

## W22 Scope Recommendation

Focus on Tier 1 modules for maximum impact:
- `tools/model-bridge.rkt`: Tighten request/response types
- `util/tree-entries.rkt`: Tighten struct field types
- `tools/scheduler-strategy.rkt`: Tighten tool result accessors
- `util/loop-result.rkt`: Tighten loop result types

**Estimated any/c reduction**: 30-45 (from 850 → 805-820)

## W23 Typed Boundary Candidate

Per ADR 0014, candidate for Typed Racket pilot:
- `util/loop-result.rkt` (7 any/c, stable, few callers, pure data types)
- Low churn, well-defined struct, clear import boundary
