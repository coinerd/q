# Abstraction Review Checklist — v0.99.37

**Date:** 2026-06-22
**Scanner version:** v3 (v0.99.37 W1)

---

## How to Use

Run the advisory scanner:
```bash
racket scripts/abstraction-audit.rkt --out report.md
```

Then review the report sections against this checklist.

---

## Checklist Items

### Boundary Shape

- [ ] **all-defined-out**: Must not appear in production modules.
      Scanner flag: `all-defined-out-modules`. Current: 1 (browser/events.rkt).
- [ ] **struct-out**: Prefer explicit exports for `#:transparent` structs.
      Scanner flag: `struct-out-exports`. Current: 40 modules.
- [ ] **contract-out**: Preferred for public boundaries. Current: 347 modules.

### Serialization Ownership (§28)

- [ ] **Serialization hotspots**: Each domain has a single owner module.
      Scanner flag: `serialization-hotspots`.
- [ ] **Round-trip tests**: Every serialization format has round-trip property tests.
- [ ] **Invalid input handling**: Serialization boundaries reject malformed input gracefully.

### Expected-Failure Visibility (§25–§26)

- [ ] **Error density**: High `error`/`raise` density may indicate unstructured failure paths.
      Scanner flag: `error-density`.
- [ ] **Handler density**: High `with-handlers` density may indicate catch-all exception handling.
      Scanner flag: `handler-density`.
- [ ] **Result-type boundaries**: Expected failures should be visible in the return type,
      not hidden behind exceptions.

### Effect-Shell Discipline (§16, §27)

- [ ] **I/O mixed with logic**: Core logic should be pure; I/O at adapter boundaries.
      Scanner flag: `io-mixed-with-logic`.
- [ ] **Port abstraction**: Writers accept ports, not file paths.

### Mutable Cache Encapsulation (§38, §47)

- [ ] **Mutable cache usage**: Caches should be hidden behind operation-level APIs.
      Scanner flag: `mutable-cache-modules`. Current: 22+ modules.
- [ ] **No raw hash exposure**: Mutable hashes should not be exported directly.

### Benchmark Semantics (W2)

- [ ] **Timing assertions**: Wall-clock assertions should not gate correctness.
      Scanner flag: `bench-timing-modules`.
- [ ] **Benchmark isolation**: Performance tests separate from correctness suites.

### Handler/Model Separation (§41)

- [ ] **Handler thinness**: Event handlers translate input to domain operations.
      Scanner flag: `event-handler-modules`.
- [ ] **Pure model**: Domain rules in pure functions, not in handlers.

### Parser Ownership (§47)

- [ ] **Ad-hoc parsing**: High parsing density suggests unowned parsing logic.
      Scanner flag: `ad-hoc-parse-modules`.
- [ ] **Single parser**: Each input format has one parser owner.

---

## Strict Mode

```bash
racket scripts/abstraction-audit.rkt --strict
```

Thresholds:
- Max lines: 800
- Max exports: 40
- Max requires: 30
- Max handlers: 12
- Max errors: 20
