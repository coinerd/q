<!-- verified-against: 0.83.9 --># q Source Style Guide

This document defines formatting and naming conventions for the q Racket codebase.
All changes to `q/` source files (excluding `tests/`) should follow these rules.

## 1. File Layout

```racket
#lang racket/base                          ; libraries — minimal requires
;; OR
#lang racket                               ; entry points (main.rkt, CLI modules)

(require ...)

(provide
 (struct-out ...)
 function-name
 ...)

;; --- implementation ---
```

- `#lang racket/base` for library modules.
- `#lang racket` for entry points and CLI modules.
- `provide` before `define`.
- Group `require` by: Racket standard → third-party → internal (`q/...`).

## 2. Naming

| Construct          | Convention                          | Example                    |
|--------------------|-------------------------------------|----------------------------|
| Functions          | hyphenated-lowercase                | `build-context`            |
| Predicates         | suffix `?`                          | `session-active?`          |
| Mutators           | suffix `!`                          | `mark-dirty!`              |
| Constants          | `UPPER-HYPHEN` or `+earmuffed+`     | `+default-timeout+`        |
| Module names       | hyphenated, one concept per file    | `context-files.rkt`        |
| Struct types       | hyphenated-lowercase                | `agent-context`            |
| Struct fields      | hyphenated-lowercase                | `#:tier-b-count`           |

## 3. Formatting

| Rule                          | Value                                             |
|-------------------------------|---------------------------------------------------|
| Indent                        | 2 spaces                                          |
| Soft line limit               | 100 characters                                    |
| Hard line limit               | 150 characters                                    |
| Tabs                          | **None** — spaces only                            |
| Trailing whitespace           | **None**                                          |
| Final newline                 | Yes (one `\n` at end of file)                     |
| Blank lines                   | Exactly one between top-level forms               |

### Line-length exceptions

Long string literals (tool descriptions, help text, API messages) and deeply
nested data literals (`hasheq` tables) may exceed 100 characters but must stay
under 150 characters where practical. If a string is unavoidably long, prefer
`string-append` or `~a` formatting over a single oversized line.

## 4. Contracts

- Use `contract-out` at **module boundaries only** (i.e. in `provide` forms).
- Do **not** add contracts to internal/private functions; rely on type discipline
  and tests instead.
- Keep contracts flat where possible; avoid expensive higher-order contracts in
  hot paths.

```racket
(provide
 (contract-out
  [run-session (-> session? provider? event-bus? any)]))
```

## 5. Structs

- Prefer `struct` over plain lists or hash tables for typed data.
- Always use `#:transparent` for debuggability (unless intentional opacity is
  needed for a contract boundary).

```racket
(struct agent-context
  (name description instructions examples tool-preferences)
  #:transparent)
```

## 6. Testing

- Use `rackunit` with `test-case` blocks.
- One test file per source module: `foo.rkt` → `tests/test-foo.rkt`.
- Test files use `#lang racket` with `(require rackunit ...)`.
- Run all tests: `raco test tests/`.

```racket
#lang racket
(require rackunit
         "../path/to/module-under-test.rkt")

(test-case "descriptive name"
  (check-equal? (my-fn input) expected))
```

### 6.1 Testing Portability

Tests run on CI (clean Ubuntu runner) and must not depend on the developer's
local environment. Follow these rules:

| Rule | Do | Don't |
|------|----|-------|
| **No absolute paths** | `define-runtime-path` or `/tmp/` test data | `/home/user/...`, `/Users/...` |
| **No CWD assumptions** | Anchor paths from test file via `define-runtime-path` | `(current-directory)`, relative paths from assumed cwd |
| **No hash ordering** | Explicit ordered list, or `sort` after `hash-keys` | Assume `hash-keys` returns insertion order |
| **Clean bytecode** | `find . -name '*.zo' -delete` before testing | Trust cached `.zo` after module changes |
| **Lint before push** | `racket scripts/lint-tests.rkt` | Wait for CI to catch these |
| **Version consistency** | `racket scripts/lint-version.rkt` after bumping `info.rkt` | Manually update version strings across docs |
| **Metric consistency** | `racket scripts/metrics.rkt --lint` after changing source files | Manually update README metrics table |

Run all linters locally:
```bash
cd q/ && racket scripts/lint-tests.rkt && racket scripts/lint-version.rkt && racket scripts/metrics.rkt --lint
```

## 7. Comments

- `;;` for normal comments.
- `;;;` (three semicolons) for section headers.
- `; ` (one semicolon + space) for inline comments on the same line as code.

```racket
;;; Context Assembly

;; Build the tiered context from message history.
(define (build-context messages) ...)
```

## 8. Whitespace Checklist (pre-commit)

Before committing, verify:

```bash
# No tabs in source (excluding tests/)
cd q && find . -name '*.rkt' -not -path './tests/*' -exec grep -l $'\t' {} +

# No trailing whitespace in source
cd q && find . -name '*.rkt' -not -path './tests/*' -exec grep -n ' $' {} +

# No lines over 150 chars (informational)
cd q && find . -name '*.rkt' -not -path './tests/*' -exec awk 'length > 150 {print FILENAME":"NR":"length}' {} + | sort -t: -k3 -rn | head -20
```

## 9. Abstraction Selection Guide

When introducing a new abstraction (module, macro, DSL, struct, HOF wrapper,
contract boundary), verify at least one criterion applies:

- Pattern appears in **3+ places**
- Names a **real q domain concept**
- **Reduces boundary errors** (type safety, contracts)
- **Simplifies tests** (easier mocking, pure functions)
- **Narrows a public API** (hides unstable internals)

### Decision Tree (weakest sufficient abstraction)

Use the **weakest sufficient abstraction** at each level:

| Situation | Preferred | Escalate to |
|-----------|-----------|-------------|
| Single use | Inline code | Function |
| Reused logic | Function | HOF |
| Parameterized behavior | HOF | Macro |
| Named structured data | Struct | Class / Opaque |
| Lookup table | Hash | Struct |
| Module boundary | Module | Contract |
| Composable logic | Data + interpreter | DSL syntax |
| Full language extension | DSL combinators | `#lang` |

### q-specific examples

- **Prefer `hash` over `struct`** for simple lookup tables (tool registry,
  provider settings) until the shape stabilizes.
- **Prefer `struct` over `hash`** for event payloads, message parts, and any
  data that crosses module boundaries.
- **Prefer function over HOF** for one-off transformers; use HOF only when the
  combinator is tested independently.
- **Prefer `syntax-parse` macros** over raw `syntax-rules`; always add an
  expansion test.
- **DSL prerequisite chain**: structs → functions → combinators → macros
  → `#lang`. Never skip steps.

