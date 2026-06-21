# Expected-Failure / Result-Boundary Pilot — v0.99.37

**Date:** 2026-06-22
**Wave:** W4 (#8444)

---

## §25–§26 Principle: Expected failure visible in normal interface

Failures should be **data**, not side effects. The caller should be able to
dispatch on the type of failure without parsing strings or interpreting exit
codes.

---

## Error/Exception Density Map

| Module | `raise`/`error` | `with-handlers` | Pattern |
|--------|----------------|-----------------|---------|
| util/error/errors.rkt | 26 | 0 | Central error definitions |
| llm/stream.rkt | 15 | 0 | Inline raise on stream errors |
| tools/builtins/firecrawl.rkt | 13 | 5 | Mix of raise + handler |
| browser/adapters/playwright-sidecar.rkt | 13 | 9 | Heavy exception handling |
| tools/builtins/browser-tools.rkt | 0 | 10 | All caught, no structured result |
| scripts/metrics.rkt | 0 | 4 | printf + exit 1 |
| sandbox/subprocess.rkt | 0 | 7 | Swallowed exceptions → #f |

**Problem areas:**
1. Scripts that use `printf + exit 1` to signal failure (20+ instances)
2. `with-handlers` that swallow exceptions and return ad-hoc defaults (#f, 0, '())
3. No structured way for callers to know *what* failed

---

## Target: scripts/metrics.rkt lint-metrics

The `lint-metrics` function compares computed metrics against README.md values.
It currently:
- Returns an exit code (0 = ok, 1 = any failure)
- Prints error messages via `printf` (not parseable)
- Cannot tell callers *which* metrics drifted or *by how much*

The `lint-prose-metrics` function has a latent bug: the `unless (file-exists?)`
guard prints an error and returns 1, but execution falls through to the body.

---

## Design-It-Twice: Two Approaches

### Approach A: Structured Result Types (Winner)

Create `scripts/metrics-lint-result.rkt` with `#:transparent` struct variants:

```racket
(struct lint-ok (checked-count) #:transparent)
(struct lint-mismatch (metric-name expected found) #:transparent)
(struct lint-metric-not-found (metric-name) #:transparent)
(struct lint-file-error (path message) #:transparent)
```

The pure check function returns a `(listof lint-result)` where each element is
either `lint-ok` (per-metric) or a specific failure variant. The CLI layer
formats results for display and computes exit code.

**Pros:**
- Each failure mode is a distinct type — exhaustiveness checking in match
- Fields carry structured data (expected vs found, metric name)
- `#:transparent` enables equal?-based testing
- Consistent with established pattern from v0.99.36 W5 (status-result.rkt)
- Pure check function (no I/O) — testable without temp files

**Cons:**
- More boilerplate per failure mode
- Requires new module

### Approach B: Tagged List Results

Return `(list 'ok message)` or `(list 'error error-list)`:

```racket
(define (lint-metrics-data readme-values computed-metrics)
  (define errors
    (for/list (...)
      (cond
        [(not readme-val) (list 'not-found name)]
        [(not (equal? value readme-val)) (list 'mismatch name value readme-val)]
        [else (list 'ok name))])))
  (if (andmap (λ (r) (eq? (car r) 'ok)) errors)
      (list 'ok "All metrics match")
      (list 'error errors)))
```

**Pros:**
- Simpler, less code
- No new module needed

**Cons:**
- No type safety — any list could be a "result"
- No exhaustiveness checking
- Harder to extend with new failure modes
- Loses field-level documentation

### Decision: Approach A

Approach A wins because:
1. Type safety prevents silent bugs (e.g., the `lint-prose-metrics` fall-through bug)
2. Consistent with the established pattern (status-result.rkt)
3. Enables programmatic callers (CI tools, other scripts) to dispatch on failure type

---

## Implementation

### New module: `scripts/metrics-lint-result.rkt`

- `lint-ok`, `lint-mismatch`, `lint-metric-not-found`, `lint-file-error`
- `lint-result?` — predicate for any result variant
- `format-lint-result` — human-readable string for display
- `lint-result-exit-code` — 0 for ok, 1 for any failure
- `check-metrics` — pure function taking (readme-values computed-metrics) → (listof lint-result)

### Refactored `scripts/metrics.rkt`

- `lint-metrics` calls `check-metrics`, formats results, computes exit code
- Backward compatible: CLI output and exit codes unchanged

### Tests: `tests/test-metrics-lint-result.rkt`

- Pure-function tests for `check-metrics` (no file I/O needed)
- Result-type tests: verify struct fields, predicates, formatting
- Edge cases: empty hash, all-ok, all-mismatch, missing metrics

---

## Future Applicability

This pattern should be applied to other `printf + exit 1` scripts:
- `scripts/pre-commit.rkt` (3 exit-1 paths)
- `scripts/lint-release-notes.rkt`
- `scripts/ci-local.rkt` (2 exit-1 paths)
- `scripts/audit-project.rkt`
