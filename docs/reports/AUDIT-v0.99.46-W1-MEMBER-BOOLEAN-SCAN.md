# v0.99.46 W1 Member→Boolean Contract Scan

Date: 2026-07-09
Issues: #8666, #8671

## Scope

This scan followed up the v0.99.45 audit finding that Racket `member` returns a truthy list rather than the exact boolean `#t`, which can violate `boolean?` contracts when used directly in exported predicates.

Production roots scanned:

- `agent/`
- `cli/`
- `extensions/`
- `gui/`
- `interfaces/`
- `llm/`
- `runtime/`
- `sandbox/`
- `scripts/`
- `skills/`
- `tools/`
- `tui/`
- `util/`
- `wiring/`

## Findings

### Fixed in W1

`tools/shell-risk.rkt` exported these predicates with `(-> symbol? boolean?)` contracts but returned the raw result of `member` for valid values:

- `risk-severity?`
- `token-type?`
- `risk-type?`

They now coerce truthy membership results to exact booleans using `(and (member ...) #t)`.

### Remaining member→boolean suspects

A production scan for predicate definitions using bare `member`/`memq`/`memv` found no remaining suspect production definitions after the W1 fix.

Result:

```text
BOOLEAN_MEMBER_SUSPECTS
TOTAL 0
```

Test-suite uses of `member` under `check-not-false`, `check-false`, or explicit `(and (member ...) #t)` were intentionally not treated as production contract violations.

### Single-value contract / multi-value scan

A broader scan for contracted functions whose bodies contain `(values ...)` produced many syntactic overmatches because it is intentionally conservative and does not fully parse top-level Racket forms. The actionable known instance remains:

- `tui/command-parse.rkt`: `tokenize` has a single-value contract but returns two values (#8667; planned for W2).

No additional W1 fixes were made for multi-value findings; #8667 remains the owning remediation issue.

## Verification

Focused regression tests:

```text
raco test tests/test-shell-risk.rkt tests/test-audit-v09945-w5-tools.rkt
```

Result:

```text
87 tests passed
```

## Conclusion

W1 remediates the member→boolean contract class for the known shell-risk predicates and documents a codebase scan showing no remaining production predicate suspects of that class.
